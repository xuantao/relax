local parser = require "lua-parser.parser"
local lib = require "lib"

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
        if smt.tag == "Local" or smt.tag == "Set" then
            local block = parseAstSet(smt)
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
            local block = {tag = smt.tag, pos = smt.pos, epos = smt.epos}
            table.insert(ret.blocks, block)
            if smt.tag == "Return" then
                ret.endpoint = block
            end
        end
    end

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
                    link[prev] = {m}
                else
                    table.insert(sms, m)
                end
            end
        else
            prev = m.id
        end
    end

    local sort = {}
    if ahead then
        table.insert(sort, {sms = {ahead}})
    end
    for _, hm in ipairs(handler) do
        table.insert(sort, {hm = hm, sms = link[hm.id]})
    end
    return sort, del
end

local function newServiceHandler(file, service)
    local f = io.open(file, 'w+b')
    f:write(string.char(0xef, 0xbb, 0xbf))

    f:write(string.format("%s = __TObject.new(tcligs.%sIface, {__type= \"%s\"})\n", serviceName, s.name, serviceName))
    f:write("\n")

    for _, m in ipairs(s.member) do
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
    end

    f:write(string.format("return %s\n", serviceName))
    f:close()
end

local function updateServiceHandler(file, source, handler, service)
    local sort, del = preProcessService(handler, service)
    local f = io.open(file, 'w+b')
    f:write(string.char(0xef, 0xbb, 0xbf))  -- utf8



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
        lib.Log(err)
        return false
    end

    local handler = parseServiceAst(ast)
    if not handler.startpoint then
        print(string.format("can not export service:[%s]", service.name))
        print(string.format("can not parse handler soruce file, file:[%s]", handlerFile))
        return false
    end

    updateServiceHandler(handlerFile, fd, handler, service)
    return
end

local tmp_file = "loader.lua"
local f = io.open(tmp_file, "r")
local fd = f:read("*a")
f:close()
local ast, err = parser.parse(fd, "loader.lua")
--lib.Log(ast)
parseServiceAst(fd, ast, "InitServiceS2CHandler")
--lib.Log(parseServiceAst(fd, ast, ""))

--@d[[]]
--@p[[]]
--@TODO: 

return {export = exportService}

