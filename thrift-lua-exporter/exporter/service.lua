local parser = require "lua-parser.parser"
local scope = require "lua-parser.scope"
local lpeg = require "lpeglabel"
local lib = require "lib"

local kDiscard = "--@Warr[[Discard]]"
local kDiscardPatt = "--@Warr%[%[Discard%]%]"

local p_longStr = lpeg.P {
    lpeg.V"string",
    equals = lpeg.P"="^0,
    open = "[" * lpeg.Cg(lpeg.V"equals", "init") * "[" * lpeg.P"\n"^-1,
    close = "]" * lpeg.C(lpeg.V"equals") * "]",
    closeeq = lpeg.Cmt(lpeg.V"close" * lpeg.Cb("init"), function (s, i, a, b) return a == b end),
    string = lpeg.V"open" * lpeg.C((lpeg.P(1) - lpeg.V"closeeq")^0) * lpeg.V"close" / 1,
}

local p_comment = lpeg.P"--" * (p_longStr / 0) + lpeg.P"--" * (lpeg.P(1) - lpeg.P"\n")^0
local p_desc = lpeg.P"\n" * lpeg.S" \r\t"^0 * p_comment * (lpeg.S" \r\t\n" + p_comment + p_longStr)^0 * lpeg.P(-1)

local _parseAstName
function _parseAstName(smt, nl)
    if smt.tag == "Id" or smt.tag == "String" then
        table.insert(nl, smt[1])
    elseif smt.tag == "Index" then
        _parseAstName(smt[1], nl)
        _parseAstName(smt[2], nl)
    else
        lib.Log(smt)
        assert(false)
    end
end

local function parseAstName(smt)
    local ret = {tag = smt.tag, is_method = smt.is_method, pos = smt.pos}
    _parseAstName(smt, ret)
    return ret
end

local function _parseFunctionArgs(smt)
    local ret = {}
    for idx = 1, #smt do
        table.insert(ret, smt[idx][1])
    end
    return ret
end

local function parseAstValue(smt)
    if not smt then
        return {tag = "Nil"}
    end

    local ret = {tag = smt.tag, pos = smt.pos}
    if smt.tag == "Function" then
        ret.args = _parseFunctionArgs(smt[1])
        ret.body = {pos = smt[2].pos}
    end
    return ret
end

local function parseAstSet(smt)
    local pairs = {}
    for i, n in ipairs(smt[1]) do
        local pair = { name = parseAstName(n), value = parseAstValue(smt[2][i]) }
        table.insert(pairs, pair)
    end
    return {tag = smt.tag, pos = smt.pos, pairs = pairs}
end

-- 解析源文件的ast数据
local function parseServiceAst(source, handlerName, ast)
    local member = {}
    local blocks = {}
    local startpoint
    local isError
    for _, smt in ipairs(ast) do
        local block
        if smt.tag == "Local" or smt.tag == "Set" then
            block = parseAstSet(smt)
            table.insert(blocks, block)

            for _, p in ipairs(block.pairs) do
                if p.name[1] == handlerName then
                    local n = #p.name
                    if n == 1 then
                        assert(startpoint == nil)
                        startpoint = block
                    elseif n == 2 and p.value.tag == "Function" then
                        -- 只支持1对1的赋值
                        if #block.pairs ~= 1 then
                            local line = scope.lineno(source, block.pos)
                            print(string.format("%s.%s, line:%s not allow", handlerName, p.name[2], line))
                            isError = true
                            break
                        end
                        local func = p.value
                        table.insert(member, {id = p.name[2], block = block, pos = block.pos, args = func.args, body = func.body})
                    end
                end
            end
        else
            block = {tag = smt.tag, pos = smt.pos}
            table.insert(blocks, block)
        end
    end

    -- 解析错误
    if isError or not startpoint then
        return nil
    end

    -- 设置区块结束位置
    local block = blocks[1]
    for i = 2, #blocks do
        block.epos = blocks[i].pos
        block = blocks[i]
    end
    block.epos = #source

    return {startpoint = startpoint, member = member}
end

-- 预处理service
-- 构建输出顺序, 检测已删除成员
local function preProcessService(handler, service)
    local delMap = {}
    local hmMap = {}
    local smMap = {}
    for _, m in ipairs(service.member) do
        smMap[m.id] = true
    end
    for _, m in ipairs(handler.member) do
        hmMap[m.id] = m
        if not smMap[m.id] then
            delMap[m.id] = true -- mark deleted
        end
    end

    local prevId
    local aheadList = {}
    local linkMap = {}
    for _, sm in ipairs(service.member) do
        local hm = hmMap[sm.id]
        if not hm then
            if not prevId then
                table.insert(aheadList, sm)
            else
                local link = linkMap[prevId]
                if not link then
                    linkMap[prevId] = {after = {sm}}
                else
                    table.insert(link.after, sm)
                end
            end
        else
            linkMap[sm.id] = {hm = hm, sm = sm, after = {}}
            prevId = sm.id
        end
    end

    local expQue = {}
    if #aheadList > 0 then
        table.insert(expQue, {hm = handler.startpoint, after = {aheadList}})
    end
    for _, hm in ipairs(handler.member) do
        local link = linkMap[hm.id]
        if link then
            table.insert(expQue, link)
        else
            table.insert(expQue, {hm = hm, sms = linkMap[hm.id]})
        end
    end
    return expQue, delMap
end

-- 找到注释起始位置
local function parseDescPos(source)
    local pos = 1
    local len = #source
    local ret = len
    while pos < len do
        local p = lpeg.match(p_desc, source, pos)
        if p then
            ret = pos
            break
        else
            p = lpeg.match(p_comment, source, pos)
            pos = p or (pos + 1)
        end
    end
    return ret
end

local function addServiceMember(f, m, handlerName)
    if m.desc and m.desc ~= "" then
        local s = lib.Trim(m.desc)
        s = string.gsub(s, "(\n)", ' ')
        f:write(string.format("--@Node[[%s]]\n", s))
    end
    if #m.args > 0 then
        f:write("--@Args[[")
        for _, a in ipairs(m.args) do
            f:write(string.format("%s %s, ", a.type, a.id))
        end
        f:seek("cur", -2)
        f:write("]]\n")
    end

    f:write(string.format("function %s:%s(", handlerName, m.id))
    if #m.args > 0 then
        for _, a in ipairs(m.args) do
            f:write(string.format("%s, ",a.id))
        end
        f:seek("cur", -2)
    end
    f:write(")\n")
    f:write("    --@TODO:\n")
    f:write("end\n")
end

local function newServiceHandler(service, handlerName, handlerFile)
    local f = io.open(handlerFile, 'w+b')
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write(string.format("-- %s service handler\n", service.name))
    f:write(string.format("%s = __TObject.new(tcligs.%sIface, {__type= \"%s\"})\n", handlerName, service.name, service.name))
    f:write("\n")

    for _, m in ipairs(service.member) do
        addServiceMember(f, m, handlerName)
        f:write("\n")
    end

    f:write(string.format("return %s\n", handlerName))
    f:close()
end

local function checkMemberArgsChanged(hm, sm)
    if #hm.args - 1 ~= #sm.args then
        return true
    end

    for idx, arg in ipairs(sm.args) do
        if hm.args[idx + 1] ~= arg.id then
            return true
        end
    end
    return false
end

local function updateServiceMember(f, hm, sm, code, desc)
    -- 去掉废弃标记
    local b, e = string.find(desc, kDiscardPatt)
    if b then
        desc = string.format("%s%s", string.sub(desc, 1, b - 1), string.sub(desc, e))
    end
    if not checkMemberArgsChanged(hm, sm) then
        f:write(desc)
        f:write(code)
        return
    end

    -- 更新参数描述列表
    b, e = string.find(desc, "--@Args%[%[.-%]%]")
    if b then
        f:write(string.sub(desc, 1, b-1))
    else
        f:write(desc)
    end
    if #sm.args > 0 then
        f:write("--@Args[[")
        for _, a in ipairs(sm.args) do
            f:write(string.format("%s %s, ", a.type, a.id))
        end
        f:seek("cur", -2)
        f:write("]]")
    end
    if b then
        f:write(string.sub(desc, e+1))
    end

    -- 更新参数列表
    f:write(string.format("function %s:%s(", handlerName, hm.id))
    if #sm.args > 0 then
        for _, arg in ipairs(sm.args) do
            f:write(string.format("%s, ", arg.id))
        end
        f:seek("cur", -2)
    end
    f:write(")\n")

    -- 更新提示
    f:write("    --@TODO: arguments changed")
    b = string.find(code, "--@TODO:", hm.body.pos - hm.pos)
    if b then
        local p = string.find(code, '\n', b)
        f:write(string.sub(code, p + 1))
    else
        f:write(string.sub(code, hm.body.pos - hm.pos))
    end
end

local function updateServiceHandler(handlerName, handlerFile, source, handler, service)
    local expQue, delMap = preProcessService(handler, service)
    local f = io.open(handlerFile, 'w+b')
    f:write(string.char(0xef, 0xbb, 0xbf))  -- utf8

    local lastPos = 1
    local lastDesc = ""
    for _, exp in ipairs(expQue) do
        local pos, epos = exp.hm.block.pos, exp.hm.block.epos
        f:write(string.sub(source, lastPos, pos - 1))
        lastPos = epos

        local desc = lastDesc
        local sub = string.sub(source, pos, epos - 1)
        local dpos = parseDescPos(sub)
        local code = string.sub(sub, 1, dpos - 1)
        lastDesc = string.sub(sub, dpos)

        if delMap[exp.hm.id] then
            f:write(desc)
            if not string.find(desc, kDiscardPatt) then
                f:write(kDiscard, "\n")
            end
            f:write(code)
        elseif exp.sm then
            updateServiceMember(f, exp.hm, exp.sm, code, desc)
        else
            f:write(desc)
            f:write(code)
        end

        if exp.after then
            for _, sm in ipairs(exp.after) do
                f:write("\n")
                addServiceMember(f, sm, handlerName)
            end
        end
    end

    f:write(string.sub(source, lastPos - 1))
    f:close()
end

local function exportService(service, path)
    local handlerName = string.format("%sHandler", service.name)
    local handlerFile = string.format("%s/%s.lua", path, handlerName)
    local source = lib.LoadFile(handlerFile)
    if not source then
        newServiceHandler(service, handlerName, handlerFile)
        return true
    end

    local ast, err = parser.parse(source, handlerFile)
    if not ast then
        print(string.format("can not export service:[%s]", service.name))
        print(string.format("parsr handler file failed, file:[%s]", handlerFile))
        lib.Log(err)
        return false
    end

    local handler = parseServiceAst(source, handlerName, ast)
    if not handler or not handler.startpoint then
        print(string.format("can not export service:[%s]", service.name))
        print(string.format("can not parse handler soruce file, file:[%s]", handlerFile))
        return false
    end

    updateServiceHandler(handlerName, handlerFile .. ".txt", source, handler, service)
    return
end

return {export = exportService}
