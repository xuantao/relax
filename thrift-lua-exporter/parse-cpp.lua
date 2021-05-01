local lib = require "lib"
local clang = require "clang"
local test_file = "test-clang.cpp"
local ret = {}

local TypeTag = {
    None = 1,
    Builtin = 2,
    Enum = 3,
    Complex = 4,
    Pointer = 5,            -- 指针
    Array = 6,              -- 数组
    FunctionPointer = 7,    --
    MemberPointer = 8,      --
    LValueReference = 9,    -- 左值引用
    RValueReference = 10,   -- 右值引用
}

local function getKey(t, value)
    for k, v in pairs(t) do
        if v == value then
            return k
        end
    end
end

local function getTag(cursor)
    local tag
    for _, child in ipairs(cursor:children()) do
        if child:kind() == clang.CursorKind.AnnotateAttr then
            return child:name()
        end
    end
end

local parseType
parseType = function(type)
    local kind = type:kind()

    local tag = TypeTag.None
    local element
    if kind == clang.TypeKind.Pointer then
        --return {surfex = TypeSurfex.Pointer, spelling = type:spelling(), element = parseType(type:element())}
        tag = TypeTag.Pointer
        element = parseType(type:pointee())
    elseif kind == clang.TypeKind.LValueReference then
        tag = TypeTag.LValueReference
        element = parseType(type:pointee())
        --local p = type:pointee()
        --print(getKey(clang.TypeKind, p:kind()), p:spelling())
    elseif kind == clang.TypeKind.RValueReference then

    elseif kind == clang.TypeKind.Elaborated then
        parseType(type:namedType())
    elseif kind == clang.TypeKind.Typedef then
        parseType(type:canonical())
    elseif kind == clang.TypeKind.Unexposed then
        --local cur = type:declaration()
        --local tmp = cur:specializedTemplate()
        --print("clang.TypeKind.Unexposed", getKey(clang.CursorKind, cur:kind()), cur:spelling(), tmp:spelling(), getKey(clang.CursorKind,tmp:kind()))
        --local tmpType = tmp:type()
        --print("xxxxxxxx", tmpType:kind(), tmpType:spelling())
        
        local tmpArgs = type:templateArguments()
        for _, v in ipairs(tmpArgs) do
            print(getKey(clang.TypeKind, v:kind()), v:spelling());
        end

    elseif kind == clang.TypeKind.FunctionProto then
        local cur = type:declaration()
        local ret = type:resultType()
        local args = type:arguments()
        print("clang.TypeKind.FunctionProto", type:spelling())
        print("ret")
        parseType(ret)
        for _, a in ipairs(args) do
            print("arg", _)
            parseType(a)
        end
    end

    local canonical = type:canonical()
    print(getKey(clang.TypeKind, kind), type:spelling(), canonical and canonical:spelling() or "none", getKey(clang.TypeKind, canonical and canonical:kind()))

    if kind == clang.TypeKind.Typedef or
        kind == clang.TypeKind.Pointer or
        kind == clang.TypeKind.Complex or
        kind == clang.TypeKind.LValueReference or
        kind == clang.TypeKind.RValueReference or
        kind == clang.TypeKind.Record or
        kind == clang.TypeKind.ConstantArray or
        kind == clang.TypeKind.IncompleteArray or
        kind == clang.TypeKind.Vector or
        kind == clang.TypeKind.VariableArray or
        kind == clang.TypeKind.DependentSizedArray then
        local elemnet = type:element()
        print("element", elemnet and elemnet:spelling() or "unknown")
    end
    return type:name()
end

local function onVar(cursor)
    local type = cursor:type()
    return {"var", cursor:name(), {type = parseType(type), tag = getTag(cursor)}}
end

local function onField(cursor)
    local type = cursor:type()
    return {"field", cursor:name(), {type = parseType(type), access = cursor:access(), tag = getTag(cursor)}}
end

local function onArg(cursor)
    local type = cursor:type()
    local children = cursor:children()

    print("onArg", getKey(clang.CursorKind, cursor:kind()), cursor:spelling(), type:spelling())

    if children then
        print("11111", getKey(clang.CursorKind, cursor:kind()), cursor:spelling())
        for _, child in ipairs(children) do
            print("222222", getKey(clang.CursorKind, child:kind()), child:name(), child:spelling())
            if child:kind() == clang.CursorKind.UnexposedExpr or
                child:kind() == clang.CursorKind.IntegerLiteral then
                local cs = child:children()
                print("2222---", cs, #cs)
                for _, c in ipairs(cs)do
                    print("3333", getKey(clang.CursorKind, c:kind()), c:spelling(), c:name())
                end
            end

        end
    end

    return {"arg", cursor:name(), {type = parseType(type)}}
end

local function onFunction(cursor)
    local ret_type = cursor:resultType()
    local args = {}

    --print("onFunction 1", cursor:displayName())

    local type = cursor:type()
    if type then
        local args = type:arguments()
        for _, a in pairs(args) do
            print("arg", getKey(clang.TypeKind, a:kind()), a:spelling())
        end
    end

    for _, c in ipairs(cursor:children()) do
        print("onFunction", getKey(clang.CursorKind, c:kind()), c:spelling(), c:displayName())
    end

    for _, arg in ipairs(cursor:arguments()) do
        table.insert(args, onArg(arg))
    end
    --return {"function", cursor:name(), {ret = parseType(ret_type), args = args, tag = getTag(cursor)}}
    return {}
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
    print("onClass", cursor:spelling())
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
    print("onNamespace", cursor:spelling())
    local eles = {}
    for _, child in ipairs(cursor:children()) do
        local kind = child:kind()
        if kind == clang.CursorKind.Namespace then
            table.insert(eles, onNamespace(child))
        elseif kind == clang.CursorKind.FunctionDecl then
            print("clang.CursorKind.FunctionDecl", child:spelling(), child:displayName())
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

local function onVisitFunction(g, cursor)
end

local onVisit
function onVisit(g, cursor)
    for _, child in ipairs(cursor:children()) do
        --local f, lb, cb, le, ce = child:location()
        local kind = child:kind()
        if kind == clang.CursorKind.Namespace then
            --table.insert(ret, onNamespace(child))
            onVisit(g, child)
        elseif kind == clang.CursorKind.FunctionDecl then
            print("clang.CursorKind.FunctionDecl", child:spelling(), child:displayName())
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

local function parse(cursor)

end

local test_args = {
    "-std=c++11",
    --"-ast-dump"
    "-DCLANG_PARSER",
    "-Xclang",
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
    local tu = index:parse(test_file, test_args,
        --2 +
        clang.TranslationUnit_Flags.SingleFileParse + clang.TranslationUnit_Flags.SkipFunctionBodies)
    local cursor = tu:cursor()
    local ret = {}

    lib.Log(tu:diagnostics())

    for _, child in ipairs(cursor:children()) do
        local f, lb, cb, le, ce = child:location()
        print("file", f)
        if f == test_file then
            local kind = child:kind()
            if kind == clang.CursorKind.Namespace then
                table.insert(ret, onNamespace(child))
            elseif kind == clang.CursorKind.FunctionDecl then
                print("clang.CursorKind.FunctionDecl", child:spelling(), child:displayName())
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

doTest()
--lib.Log(doTest())
