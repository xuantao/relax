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

for _, smt in ipairs(ast) do
    if smt.tag == "Set" then
        parsrSetSmt(smt)
    end
end

--lib.Log(ast)
--print("\n\n")
--lib.Log(err)

--pp.print(ast)
--pp.dump(ast)




