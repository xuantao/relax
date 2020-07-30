--local lib = require "lib"
local clang = require "luaclang-parser"

local logTable
function logTable(t)
    print'{'
    for k, v in pairs(t) do
        if type(v) == "table" then
            print(k, '= {')
            logTable(v)
            print('}')
        else
            print('', k, v)
        end
    end
    print'}'
end

local function visitTable(t, f)
    if not t then return end

    for k, v in pairs(t) do
        f(v)
    end
end

local test_file = "test-clang.cpp"
local visitCursor
local tab = 1
function visitCursor(c)
    local f, lb, cb, le, ce = c:location()
    if f == test_file then
        print(tab, c:kind(), c:name(), c:displayName(), c:usr(), table.concat({f, lb, cb, le, ce}, '-'))

        if c:kind() == "ParmDecl" then
            local type = c:type()
            print(tab, "type", type:name())
            local can = type:canonical()
            if can then
                print(tab, "canonical", can:name())
            end

            local pointee = type:pointee()
            if pointee then
                print(tab, "pointee", pointee:name())
            end
        end
    end

    local children = c:children() or {}
    for _, child in ipairs(children) do
        local f = child:location()
        if f == test_file then
            tab = tab + 1
            visitCursor(child)
            tab = tab - 1
        end
    end
    visitTable(c:children(), visitCursor)
end


local test_args = {
    "-std=c++14",
    --"-ast-dump"
    "-Xclang",
    "-DCLANG_PARSER",
    "-fsyntax-only",
    "-I",
    "G:/jx3m-code/Include",
    "-I",
    "G:/jx3m-code/Include/Base",
--[[
    "-I",
    "G:/jx3m-code/Include/DevEnv",
    "-I",
    "G:/jx3m-code/Include/NavMesh",
    "-I",
    "G:/jx3m-code/Include/SO3Represent",
    "-I",
    "G:/jx3m-code/Include/SO3UI",
    "-I",
    "G:/jx3m-code/Include/SO3World"
]]
}

local index = clang.createIndex(true, true)
local tu = index:parse(test_file, test_args)
local cursor = tu:cursor()

logTable(tu:diagnostics())
visitCursor(cursor)


