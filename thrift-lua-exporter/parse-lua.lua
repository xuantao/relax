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
    local ret = {tag = smt.tag, isMethod = smt.is_method, pos = smt.pos, epos = smt.epos}
    _parseAstName(smt, ret)
    return ret
end

local function _parseFunctionArgs(smt)
    local ret = {}
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

local function parseService(fd, ast, s)
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
                        table.insert(ret.member, {name = p.name[2], block = block})
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


local tmp_file = "loader.lua"
local f = io.open(tmp_file, "r")
local fd = f:read("*a")
f:close()
local ast, err = parser.parse(fd, "loader.lua")
--lib.Log(ast)
parseService(fd, ast, "InitServiceS2CHandler")
--lib.Log(parseService(fd, ast, ""))

--@d[[]]
--@p[[]]
--@TODO: 

