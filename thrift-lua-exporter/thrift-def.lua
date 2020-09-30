-- thrift-def
-- 解析thrift协议数据结构使用的常量定义

return {
    kRefNormal    = "normal",
    kRefList      = "list",
    kRefMap       = "map",
    -- 类型标签
    kTagBuiltin   = "builtin",
    kTagEnum      = "enum",
    kTagStruct    = "struct",
    -- 内建类型
    kBuiltinTypes = {
        bool   = {tag = builtin, name = "bool",   realName = "bool",   csName = "bool",   csConv = "ToBoolean"},
        byte   = {tag = builtin, name = "byte",   realName = "byte",   csName = "sbyte",  csConv = "ToSByte"},
        i16    = {tag = builtin, name = "i16",    realName = "i16",    csName = "short",  csConv = "ToInt16"},
        i32    = {tag = builtin, name = "i32",    realName = "i32",    csName = "int",    csConv = "ToInt32"},
        i64    = {tag = builtin, name = "i64",    realName = "i64",    csName = "long",   csConv = "ToInt64"},
        double = {tag = builtin, name = "double", realName = "double", csName = "double", csConv = "ToDouble"},
        string = {tag = builtin, name = "string", realName = "string", csName = "string", csConv = "ToString"},
        binary = {tag = builtin, name = "binary", realName = "binary", csName = "",       csConv = ""}, -- csharp not support
    },
}