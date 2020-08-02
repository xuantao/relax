local lib = require "lib"
local clang = require "clang"

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
local ret = {}

local function getTag(cursor)
    local tag
    for _, child in ipairs(cursor:children()) do
        if child:kind() == clang.CursorKind.AnnotateAttr then
            return child:name()
        end
    end
end

local function onVar(cursor)
    local type = cursor:type()
    return {"var", cursor:name(), {type = type:name(), tag = getTag(cursor)}}
end

local function onField(cursor)
    local type = cursor:type()
    return {"field", cursor:name(), {type = type:name(), access = cursor:access(), tag = getTag(cursor)}}
end

local function onArg(cursor)
    local type = cursor:type()
    return {"arg", cursor:name(), {type = type:name()}}
end

local function onFunction(cursor)
    local ret_type = cursor:resultType()
    local args = {}
    for _, arg in ipairs(cursor:arguments()) do
        table.insert(args, onArg(arg))
    end
    return {"function", cursor:name(), {ret = ret_type:name(), args = args, tag = getTag(cursor)}}
end

local function onCunstructor(cursor)
    local args = {}
    for _, arg in ipairs(cursor:arguments()) do
        table.insert(args, onArg(arg))
    end
    return {"constructor", cursor:name(), args = args, tag = getTag(cursor)}
end

local function onEnum(cursor)
    local tag
    local vars = {}

    for _, child in ipairs(cursor:children()) do
        local kind = child:kind()
        if kind == clang.CursorKind.EnumConstantDecl then
            table.insert(vars, {name = child:name(), value = 0})
        elseif kind == clang.CursorKind.AnnotateAttr then
            tag = child:name()
        else
            print("enum unknown element", cursor:name(), kind, child:spelling(), child:name())
        end
    end
    return {"enum", cursor:name(), {tag = tag, vars = vars}}
end

local onClass
function onClass(cursor)
    local baseClass = {}
    local inner = {}
    local member = {}
    local tag

    for _, child in ipairs(cursor:children()) do
        local kind = child:kind()
        if kind == clang.CursorKind.CXXBaseSpecifier then
            table.insert(baseClass, {name = child:name(), access = child:access()})
        elseif kind == clang.CursorKind.Constructor then
            table.insert(member, onCunstructor(child))
        elseif kind == clang.CursorKind.CXXMethod then
            table.insert(member, onFunction(child))
        elseif kind == clang.CursorKind.FieldDecl then
            table.insert(member, onField(child))
        elseif kind == clang.CursorKind.VarDecl then
            table.insert(member, onVar(child))
        elseif kind == clang.ClassDecl or kind == clang.CursorKind.StructDecl then
            table.insert(inner, onClass(child))
        elseif kind == clang.EnumDecl then
            table.insert(inner, onEnum(child))
        elseif kind == clang.CursorKind.AnnotateAttr then
            tag = child:name()
        else
            print("class unknown element", cursor:name(), kind, child:spelling(), child:name())
        end
    end

    return {"class", cursor:name(), {tag = tag, members = member, inner = inner, base = baseClass}}
end

local onNamespace
function onNamespace(cursor)
    local eles = {}
    for _, child in ipairs(cursor:children()) do
        local kind = child:kind()
        if kind == clang.CursorKind.Namespace then
            table.insert(eles, onNamespace(child))
        elseif kind == clang.CursorKind.FunctionDecl then
            table.insert(eles, onFunction(child))
        elseif kind == clang.CursorKind.StructDecl or kind == clang.CursorKind.ClassDecl then
            table.insert(eles, onClass(child))
        elseif kind == clang.CursorKind.EnumDecl then
            table.insert(eles, onEnum(child))
        elseif kind == clang.CursorKind.VarDecl then
            table.insert(eles, onVar(child))
        elseif kind == clang.CursorKind.FunctionTemplate then
            --TODO:
        elseif kind == clang.CursorKind.ClassTemplate then
            --TODO:
        else
            print("namespace unknown element", cursor:name(), kind, child:spelling(), child:name())
        end
    end

    return {"namespace", cursor:name(), elements = eles}
end

function visitCursor(c)
    local f, lb, cb, le, ce = c:location()
    if f == test_file then
        local kind = c:kind()
        print(tab, c:kind(), c:spelling(), c:name(), c:displayName(), c:usr(), table.concat({f, lb, cb, le, ce}, '-'))

        if kind == clang.CursorKind.FunctionDecl then
            onFunction(c)
        elseif kind == clang.CursorKind.StructDecl or kind == clang.CursorKind.ClassDecl then
            onClass(c)
        elseif kind == clang.CursorKind.EnumDecl then
            onEnum(c)
        elseif kind == clang.CursorKind.FunctionTemplate then
            --TODO:
        elseif kind == clang.CursorKind.ClassTemplate then
            --TODO:
        end

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
    "-ast-dump",
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
-- 命令行
-- clang -DCLANG_PARSER -Xclang -ast-dump -fsyntax-only test-clang.cpp > out.log
-- 编译命令
-- gcc –c –I C:/MinGW/lua5.3/include -I C:/MinGW/LLVM/include luaclang-parser.cpp –o luaclang-parser.o
-- g++ -shared -L C:/MinGW/lua5.3/bin -llua53 -L C:/MinGW/LLVM/bin -llibclang -o luaclang-parser.dll luaclang-parser.o 




--logTable(tu:diagnostics())
--visitCursor(cursor)

local function doTest()
    local index = clang.createIndex(true, true)
    local tu = index:parse(test_file, test_args)
    local cursor = tu:cursor()
    local ret = {}

    for _, child in ipairs(cursor:children()) do
        local f, lb, cb, le, ce = child:location()
        if f == test_file then
            local kind = child:kind()
            if kind == clang.CursorKind.Namespace then
                table.insert(ret, onNamespace(child))
            elseif kind == clang.CursorKind.FunctionDecl then
                table.insert(ret, onFunction(child))
            elseif kind == clang.CursorKind.StructDecl or kind == clang.CursorKind.ClassDecl then
                table.insert(ret, onClass(child))
            elseif kind == clang.CursorKind.EnumDecl then
                table.insert(ret, onEnum(child))
            elseif kind == clang.CursorKind.VarDecl then
                table.insert(ret, onVar(child))
            elseif kind == clang.CursorKind.FunctionTemplate then
                --TODO:
            elseif kind == clang.CursorKind.ClassTemplate then
                --TODO:
            end
        end
    end
    return ret
end

lib.Log(doTest())
