local parser = require "lua-parser.parser"
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
local p_desc = lpeg.P"\n" * lpeg.S" \r\t"^0 * comment * (lpeg.S" \r\t\n" + comment)^0 * lpeg.P(-1)

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
    local ret = {tag = smt.tag, is_method = smt.is_method, pos = smt.pos, epos = smt.epos}
    _parseAstName(smt, ret)
    return ret
end

local function _parseFunctionArgs(smt)
    local ret = {pos = smt.pos, epos = smt.epos}
    for _, arg in ipairs(smt) do
        table.insert(ret, arg[1])
    end
    return ret
end

local function parseAstValue(smt)
    if not smt then
        return {tag = "Nil"}
    end

    local ret = {tag = smt.tag, pos = smt.pos, epos = smt.epos}
    if smt.tag == "Function" then
        ret.args = _parseFunctionArgs(smt[1])
        ret.body = {pos = smt[2].pos, epos = smt[2].epos}
    end
    return ret
end

local function parseAstSet(smt)
    local pairs = {}
    for i, n in ipairs(smt[1]) do
        local pair = { name = parseAstName(n), value = parseAstValue(smt[2][i]) }
        table.insert(pairs, pair)
    end
    return {tag = smt.tag, pos = smt.pos, epos = smt.epos, pairs = pairs}
end

local function parseServiceAst(fd, ast, s)
    local ret = {blocks = {}, member = {}}
    local blocks = {}
    for _, smt in ipairs(ast) do
        local block
        if smt.tag == "Local" or smt.tag == "Set" then
            block = parseAstSet(smt)
            table.insert(ret.blocks, block)

            for _, p in ipairs(block.pairs) do
                if p.name[1] == s then
                    local n = #p.name
                    if n == 1 then
                        ret.startpoint = block
                    elseif n == 2 then
                        table.insert(ret.member, {id = p.name[2], block = block})
                    end
                end
            end
        else
            block = {tag = smt.tag, pos = smt.pos, epos = smt.epos}
            table.insert(ret.blocks, block)
            if smt.tag == "Return" then
                ret.endpoint = block
            end
        end
    end
    --[[
    for i, block in ipairs(ret.blocks) do
        --print("block", i)
        --print(string.sub(fd, block.pos, block.epos - 1))
    end

    print("22222222")
    lib.Log(ret.blocks)
    print("33333333")
    lib.Log(ret.startpoint)
    print("44444444")
    lib.Log(ret.member)
    print("55555555")
    lib.Log(ret.endpoint)
    ]]
end

-- 预处理service
-- 构建输出顺序, 检测已删除成员
local function preProcessService(handler, service)
    local del = {}
    local sdMap = {}
    local allMap = {}
    for _, m in ipairs(service.member) do
        allMap[m.id] = true
    end
    for _, m in ipairs(handler.member) do
        sdMap[m.id] = m
        if not allMap[m.id] then
            del[m.id] = true
        end
    end

    local prev
    local ahead = {}
    local link = {}
    for _, m in ipairs(service.member) do
        local hm = sdMap[m.id]
        if not hm then
            if not prev then
                table.insert(ahead, m)
            else
                local sms = link[prev]
                if not sms then
                    link[prev] = {after = {m}}
                else
                    table.insert(sms.after, m)
                end
            end
        else
            prev = m.id
            link[prev] = {update = m, after = {}}
        end
    end

    local sort = {}
    if ahead then
        table.insert(sort, {after = {ahead}})
    end
    for _, hm in ipairs(handler) do
        table.insert(sort, {hm = hm, sms = link[hm.id]})
    end
    return sort, del
end

-- 找到注释起始位置
local function parseDescPos(source)
    local pos = 1
    local len = #source
    local ret = len
    while pos < len do
        local p = lpeg.match(p_desc, source, pos)
        if p then
            ret = p + 1
            break
        end
            p = lpeg.match(p_comment, source, pos)
            pos = p or (pos + 1)
        end
    end
    return ret
end

local function addServiceMember(f, m)
    if m.desc and m.desc ~= "" then
        f:write(string.format("--@Node[[%s]]\n", lib.Trim(m.desc)))
    end
    if #m.args > 0 then
        f:write("--@Args[[")
        for _, a in ipairs(m.args) do
            f:write(string.format("%s %s, ", a.type, a.id))
        end
        f:seek("cur", -2)
        f:write("]]\n")
    end

    f:write(string.format("function %s:%s(", serviceName, m.id))
    if #m.args > 0 then
        for _, a in ipairs(m.args) do
            f:write(string.format("%s, ",a.id))
        end
        f:seek("cur", -2)
    end
    f:write(")\n")
    f:write("    --@TODO:\n")
    f:write("end\n")
    f:write("\n")
end

local function newServiceHandler(file, service)
    local f = io.open(file, 'w+b')
    f:write(string.char(0xef, 0xbb, 0xbf))

    f:write(string.format("%s = __TObject.new(tcligs.%sIface, {__type= \"%s\"})\n", serviceName, s.name, serviceName))
    f:write("\n")

    for _, m in ipairs(s.member) do
        addServiceMember(f, m)
        --[=[
        if m.desc and m.desc ~= "" then
            f:write(string.format("--@[[%s]]\n", lib.Trim(m.desc)))
        end
        if #m.args > 0 then
            f:write("--@[[")
            for _, a in ipairs(m.args) do
                f:write(string.format("%s %s, ", a.type, a.id))
            end
            f:seek("cur", -2)
            f:write("]]\n")
        end

        f:write(string.format("function %s:%s(", serviceName, m.id))
        if #m.args > 0 then
            for _, a in ipairs(m.args) do
                f:write(string.format("%s, ",a.id))
            end
            f:seek("cur", -2)
        end
        f:write(")\n")
        f:write("    --TODO:\n")
        f:write("end\n")
        f:write("\n")
        ]=]
    end

    f:write(string.format("return %s\n", serviceName))
    f:close()
end

local function checkMemberArgsChanged(hm, sm)
    if #hm.args ~= #sm.args then
        return true
    end

    for idx, name in ipairs(hm.args) do
        if sm.args[idx] ~= name then
            return true
        end
    end
    return false
end

local function updateServiceMember(f, hm, sm, code, desc)
    -- 去掉废弃标记
    local b, e = string.find(desc, kDiscardPatt)
    if b then
        desc = string.format("%s%s", string.sub(desc, 1, b -1), string.sub(desc, e))
    end
    if checkMemberArgsChanged(hm, sm) then
        f:write(desc)
        f:write(code)
        return
    end

    -- 更新参数描述列表
    b, e = string.find(desc, "--@Args%[%[.-%]%]")
    if b then
        f:write(string.sub(desc, 1, b))
    else
        f:write(desc)
    end
    if #sm.args > 0 then
        f:write("--@Args[[")
        for _, a in ipairs(sm.args) do
            f:write(string.format("%s %s, ", a.type, a.id))
        end
        f:seek("cur", -2)
        f:write("]]\n")
    end
    if b then
        f:write(string.sub(desc, e+1))
    ends

    -- 更新参数列表
    f:write(string.sub(code, hm.args.pos))
    if #sm.args > 0 then
        for _, a in ipairs(sm.args) do
            f:write(string.format("%s, ", a.id))
        end
        f:seek("cur", -2)
    end

    local argEndPos = hm.args.pos
    while code[argEndPos] ~= ')' do
        argEndPos = argEndPos + 1
    end
    argEndPos = argEndPos

    -- 更新提示
    local b = string.find(code, argEndPos, "--@TODO:")
    if b then
        f:write(string.sub(code, argEndPos, p))
        f:write("--@TODO: arguments chagned")
        local p = string.find(code, b, '\n')
        f:write(string.sub(code, p + 1))
    else
        f:write(string.sub(code, argEndPos, hm.body.pos))
        f:write("--@TODO: arguments chagned")
        f:write(string.sub(code, hm.body.pos))
    end
end

local function updateServiceHandler(file, source, handler, service)
    local sort, del = preProcessService(handler, service)
    local f = io.open(file, 'w+b')
    f:write(string.char(0xef, 0xbb, 0xbf))  -- utf8

    local pos = 1
    local lastDesc = ""
    for _, block in ipairs(sort) do
        f:write(string.sub(source, pos, block.hm.pos - 1))
        pos = block.hm.epos - 1

        local desc = lastDesc
        local sub = string.sub(source, block.hm.pos, pos)
        local dpos = parseDescPos(sub)
        local code = string.sub(sub, 1, dpos)
        lastDesc = string.sub(sub, dpos)

        if del[block.hm.id] then
            f:write(desc)
            if not string.find(desc, kDiscardPatt) then
                f:write("\n", kDiscard, "\n")
            end
            f:write(code)
        elseif block.sms.update then
            updateServiceMember(f, block.hm, bloc.sms.sm, code, desc)
        else
            f:write(desc)
            f:write(code)
        end

        if block.sms.after then
            for _, sm in ipairs(block.sms.after) do
                addServiceMember(f, sm)
            end
        end
    end

    f:close()
end

local function exportService(path, service)
    local handlerName = string.format("%sHandler", service.name)
    local handlerFile = string.format("%s/%s", path, handlerName)
    local fd = lib.LoadFile(handlerFile)
    if not fd then
        exportNewService(service, handlerFile)
        return true
    end

    local ast, err = parser.parse(fd, handlerFile)
    if not ast then
        print(string.format("can not export service:[%s]", service.name))
        print(string.format("parsr handler file failed, file:[%s]", handlerFile))
        lib.Log(err)
        return false
    end

    local handler = parseServiceAst(ast)
    if not handler.startpoint then
        print(string.format("can not export service:[%s]", service.name))
        print(string.format("can not parse handler soruce file, file:[%s]", handlerFile))
        return false
    end

    updateServiceHandler(handlerFile .. ".txt", fd, handler, service)
    return
end

--[[
local tmp_file = "loader.lua"
local f = io.open(tmp_file, "r")
local fd = f:read("*a")
f:close()
local ast, err = parser.parse(fd, "loader.lua")
--lib.Log(ast)
parseServiceAst(fd, ast, "InitServiceS2CHandler")
--lib.Log(parseServiceAst(fd, ast, ""))
]]
--@d[[]]
--@p[[]]
--@TODO: 

return {export = exportService}

