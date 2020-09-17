local parser = require "lua-parser.parser"
local pp = require "lua-parser.pp"
local scope = require "lua-parser.scope"
local lib = require "lib"


--print(parser)

local tmp_file = "loader.lua"
local f = io.open(tmp_file, "r")
local fd = f:read("*a")
f:close()


local ast, err = parser.parse(fd, "loader.lua")

local getIdnex, getVal
function getIdnex(ast, ret)
    if ast.tag ~= "Index" or ast[1].tag ~= "Id" then
        ret = {}
        return
    end

    local id = ast[1][1]
    table.insert(ret, id)

    if ast[2].tag == "String" then
        table.insert(ret, ast[2][1])
    elseif ast[2].tag == "Index" then
        return getIdnex(ast[2], ret)
    end
end

function getVal(smt)
    if smt.tag ~= "Function" or smt[1].tag ~= "NameList" or smt[2].tag ~= "Block" then
        return
    end

    local args = {}
    local lstPos = 0
    for _, a in ipairs(smt[1]) do
        assert(a.tag == "Id")
        table.insert(args, a[1])
        print("arg", a[1])
        lstPos = a.pos
    end

    local l, n = scope.lineno(fd, lstPos)
    print("arg_pos", lstPos, l, n)

    --if 
    local blockPos = smt[2].pos
    l, n = scope.lineno(fd, blockPos)
    print("blk_pos", blockPos, l, n)
end

local function parsrSetSmt(set)
    local var = {}
    for _, v in ipairs(set[1]) do
        local name = {}
        getIdnex(v, name)
        --name.pos = 
        local pos = v.pos
        local l, n = scope.lineno(fd, pos)
        print("var", pos, l, n)
        table.insert(var, name)
    end

    for _, v in ipairs(set[2]) do
        getVal(v)
    end

    lib.Log(var)
end



--lib.Log(ast)
--print("\n\n")
--lib.Log(err)

--pp.print(ast)
print("111111111111111111111111")
--pp.dump(ast)
--lib.Log(ast)

local function _parseAstName(smt, ret)
    if smt.tag == "NameList" then
        for _, node in ipairs(smt) do
            assert(node.tag == "Id")
            table.insert(ret, node[1])
        end
    elseif smt.tag == "Index" then
        local id = smt[1][1]
        table.insert(ret, id)

        if smt[2].tag == "String" then
            table.insert(ret, smt[2][1])
        elseif smt[2].tag == "Index" then
            return getIdnex(smt[2], ret)
        end
    else
        lib.Log(smt)
        assert(false)
    end
end

local function parseAstName__(smt)
    local nl = {}
    if smt.tag == "NameList" then
        for _, node in ipairs(smt) do
            assert(node.tag == "Id")
            table.insert(nl, node[1])
        end
    elseif smt.tag == "Index" then
        assert(false)
        local ns = {}
        _parseAstName(smt, ns)
        table.insert(nl, table.concat(ns, [[.]]))
    elseif smt.tag == "VarList" then
        for _, node in ipairs(smt) do
            local ns = {}
            _parseAstName(node, ns)
            table.insert(nl, table.concat(ns))
        end
    else
        --print("unknown tag", smt.tag)
        --lib.Log(smt)
        for _, node in ipairs(smt) do
            local ns = {}
            _parseAstName(node, ns)
            table.insert(nl, table.concat(ns, [[.]]))
        end
        --assert(false, smt.tag)
    end
    return nl
end

local function _parseAstValue(smt)
    local ret = {tag = smt.tag, pos = smt.pos, epos = smt.epos}
    if smt.tag ~= "Function" then
        ret.args = parseAstName(smt[1])
        ret.body = {pos = smt[2].pos, epos = smt[2].epos}
    end
    return ret
end

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
    local name = {}
    _parseAstName(smt, name)
    return {tag = smt.tag, isMethod = smt.is_method, pos = smt.pos, epos = smt.epos, name = name}
end

local function _parseFunctionArgs(smt)
    local ret = {}
    for _, arg in ipairs(smt) do
        table.insert(ret, arg[1])
    end
    return ret
end

local function parseAstValue(smt)
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

local function parseAstAny(smt)
    return {tag = smt.tag, pos = smt.pos, epos = smt.epos}
end

local blockProcs = {
    Set = parseAstSet,
    Local = parseAstSet,
}

local function parseService(fd, ast, s)
    local blocks = {}
    for _, smt in ipairs(ast) do
        local proc = blockProcs[smt.tag]
        if proc then
            table.insert(blocks, proc(smt))
        else
            table.insert(blocks, parseAstAny(smt))
        end
    end

    for i, block in ipairs(blocks) do
        print("block", i)
        print(string.sub(fd, block.pos, block.epos - 1))
    end
    print("2222222222")
    lib.Log(blocks)
end

--lib.Log(ast)
parseService(fd, ast, "")
--lib.Log(parseService(fd, ast, ""))

--@d[[]]
--@p[[]]
--@TODO: 

