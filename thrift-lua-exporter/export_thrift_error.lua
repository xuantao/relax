-- export thrift error code
local lib = require "lib"
local lpeg = require "lpeg"
local locale = lpeg.locale()
local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C, Cb, Cf, Cg, Cp, Ct, Cmt = lpeg.C, lpeg.Cb, lpeg.Cf, lpeg.Cg, lpeg.Cp, lpeg.Ct, lpeg.Cmt
local I = Cp()
local source = [[
enum /*<@ErrorCode>*/ LSError {
}
enum /*<@EnumDef>*/ KickReason {
}
]]

local space = lpeg.S ' \t\n'^0
local comm = space * lpeg.P'//' * -lpeg.P'\n'^0 * lpeg.P'\n'^-1
local m_comm = space * lpeg.P'/*' * (1 - lpeg.P'*/')^0 * lpeg.P'*/'
local letter = (lpeg.R'az' + lpeg.R'AZ' + lpeg.R'09' + lpeg.S'_')
local c_name = lpeg.C(letter^1)
local enum_label = lpeg.P'/*<@' * c_name * lpeg.P'>*/'

--local c_enum = lpeg.P'enum' * space * enum_label * space * c_name
--local c_value = lpeg.S'=' * space * lpeg.C(lpeg.R'09'^0)
--local c_note = lpeg.P'//' * space * lpeg.C((1-lpeg.P'\n')^0) * lpeg.P'\n'^0
--local c_element = space * c_name * space * c_value^-1 * space * lpeg.S',;' * space * c_note^-1

--print("000", table.concat({c_element:match(" xuantao = 123,  // nihao  ")}, '-')..'&')
--print("000", c_element:match(" xuantao = 246 ,"))

local text_com = [[
 // xuangag //
    /* */
    /****xagag*/
    /**/
enum test {
}

struct {

}

include ;
enumstruct
]]
--/*<@ErrorCode>*/
local textEnum = [[
enum LSError {
    // common
    Success = 0,
    InvalidArgs = 1,
    Unknown = 2,

    // special
    AuthNotPass = 101,
    CannotRecover = 102,
    AlreadyIn = 103,
    ProtoVerMismatch = 104,     // 协议版本号不匹配
    CannotCreateUser = 105,     // 废弃
    Banned = 106,               // 账号被封
    WhiteListOnly=107,          // 只允许白名单用户进入
    NeedQueue = 108,            // 需要排队
    Timeout = 109,              // 操作超时
    TooManyClients = 110,       // 已经达到同时在线客户端个数上限
    AreaPlatfromMismatch = 111, // 大区或平台的类型与服务器的配置不一致
    NeedInitUser = 112,         // 是一个新用户，需要走InitUser流程
    NoSceneServer = 113,        // 场景服务器不存在
    MaxUserCount = 114,         // 达到账号数上限当前值，无法创建新账号
    MaxUserCountHard = 115,     // 达到账号数上限最终值，无法创建新账号
    BannedByJiaZhang = 116,     // 账号被家长封禁。客户端提示语：您的账号已被家长设定为暂时无法登陆游戏。如有疑问，请拨打服务热线0755-86013799进行咨询
    BannedBySafeIdip = 117,     // 账号被安全idip侧封禁，客户端的提示语为传入的参数
    BannedBySafeIdipGM = 118,   // 账号被GM封禁，客户端的提示语为传入的参数

    QueueListFull = 201,        // 排队队列已达上限，拒绝连接。（此错误码仅在客户端使用，用于错误提示）
    QueueNetworkErr = 301,      //< 连接不到网络，请再试试。（此错误码仅用于客户端使用，用于错误提示）
}
]]

function anywhere(...)
    local i = lpeg.Cp()
    local p = nil
    for _, key in ipairs({...}) do
        if not p then
            p = lpeg.P(key)
        else
            p = p + lpeg.P(key)
        end
    end
    return lpeg.P{ i * p * i + 1 * lpeg.V(1) }
end

local function testScan()
    local intr = anywhere("enum", "struct", "include")
    local pos = 1
    while true do
        local l
        pos, l = intr:match(text_com, pos)
        if not pos then
            break
        end

        print("intr", pos, l, string.sub(text_com, pos, pos + 10))
        pos = pos + 1
    end
end

local function genScanner(...)
    local i = lpeg.Cp()
    local p = nil
    for _, key in ipairs({...}) do
        if not p then
            p = lpeg.C(key)
        else
            p = p + lpeg.C(key)
        end
    end
    return lpeg.P{ i * p * i + 1 * lpeg.V(1) } / function (b, k, e) return k, b, e end
end

local p_space = lpeg.S' \t\n'^0                                                 -- 空白字符
local p_comment = lpeg.P'//' * (1 - lpeg.P'\n')^0 * (lpeg.P'\n' + -lpeg.P(1))   -- 单行注释
local p_multi_line_comment = lpeg.P'/*' * (1 - lpeg.P'*/')^0 * lpeg.P'*/'       -- 多行注释
local p_empty = (lpeg.S' \t\n' + p_comment + p_multi_line_comment)^0            -- 空白内容
local p_sign = lpeg.S'+-'^-1                                                    -- 符号(+-)
local p_digit = lpeg.R('09')                                                    -- 数字
local p_decimal = p_digit^1                                                     -- 十进制
local p_hexadecimal = lpeg.P'0' * lpeg.S'xX' * lpeg.R('09', 'af', "AF")^1       -- 十六进制
local p_float = (p_digit^1 * lpeg.P'.' * p_digit^0 + lpeg.P'.' * p_digit^1) *
    (lpeg.S'eE' * p_sign * p_digit^1)^-1                                        -- 浮点数值
local p_idsafe = lpeg.R('AZ', 'az', '\127\255') + lpeg.P'_'                     -- id safe begin
local p_identity = p_idsafe * (p_idsafe + p_digit)^0                            -- identity
local p_reference = p_identity * (p_empty * P'.' * p_empty * p_identity)^0      -- reference
local c_annotation = (P'//' * P'/'^0 * C((1 - P'\n')^0) * (P'\n' + P(-1)) +
    P'/*' * (P'*' - P'*/')^0 * C((1 - P'*/')^0) * P'*/')^-1/1                   -- 提取注释
local c_tag = (P'/*<@' * p_space * C(p_reference) * p_space * P'>*/')^-1/1      --提取标记

-- 枚举
-- 提取数据结构 {
--  name = "name",
--  tag = "tag",
--  vals = {
--      {"name", "value", "desc"},
--      {"name", "value", "desc"},
--  }
--}
--[[local c_enum = P {
    V'enum',
    enum = (c_annotation^-1/1) * p_space^0 * P'enum' * (c_tag^-1/1) * p_space * C(p_identity) * p_empty * V'body' /
        function (desc, tag, id, v) print("ss", string.format("desc:%s, tag:%s, id:%s", desc, tag, id)) return {"enum", id, {tag = tag, desc = desc, vars = v}} end,
    body = P'{' * p_empty * V'vars' * p_empty * P'}',
    vars = Ct(V'ele_begin'^0 * V'ele_end'^-1),
    ele_begin = p_empty * C(p_identity) * (V'value'^-1/1) * p_empty * P',' * S' \t'^0 * (c_annotation^-1/1) /
        function(id, v, desc) return {id = id, value = v, desc = desc} end,
    ele_end = p_empty * C(p_identity) * (V'value'^-1/1) * S' \t'^0 * (c_annotation^-1/1) /
        function(id, v, desc) return {id = id, value = v, desc = desc} end,
    value = p_empty * P'=' * p_empty * C(p_decimal + p_hexadecimal),
}
]]
local commentProc = function(t, str, pos)
    return p_comment:match(str, pos)
end
local multiLineCommentProc = function(t, str, pos)
    return p_multi_line_comment:match(str, pos)
end
local enumProc = function(t, str, pos)
    local tmp = c_enum * Cp()

    --lib.Log(str, p_enum_2:match(str, pos))
    lib.Log(str, c_enum:match(str, pos))
end

local function testEnumProc()
    --enumProc(nil, [[enum test_declare;]], 1)
    --enumProc(nil, [[enum test{}]], 1)
    --enumProc(nil, [[enum/*<@error>*/test{}]], 1)
    --enumProc(nil, [[enum test {}]], 1)
    --enumProc(nil, [[enum test { }]], 1)
    --enumProc(nil, [[enum test { xuantao, zouhui, }]], 1)
    --enumProc(nil, [[enum test { xuantao=1, zouhui }]], 1)
    --enumProc(nil, [[enum test { xuantao=1, zouhui = 2 }]], 1)
    --enumProc(nil, [[enum test { xuantao=1, zouhui = 2, }]], 1)
    --enumProc(nil, [[enum test { xuantao = 1, zouhui }]], 1)
    --enumProc(nil, [[enum test { xuantao = 1, zouhui = 2, }]], 1)
    --enumProc(nil, [[enum test { xuantao/* xuantao */ = 1, zouhui/*zouhui*/  }]], 1)
    enumProc(nil, textEnum, 1)
end
--testEnumProc()

local function testCommentProc()
    local f = function (str)
        assert(commentProc(nil, str, 1) == #str + 1)
    end
    print("---test comment proc begin---")
    f("//")
    f("//    ")
    f("//xuantao")
    f("////")
    f("//xuantao//")
    f("//xuantao// ")
    print("---test comment proc succed--")
end

local function testMultiLineCommentProc()
    local f = function (str)
        assert(multiLineCommentProc(nil, str, 1) == #str + 1, str)
    end

    local s1 = [[/**/]]
    local s2 = [[/*****/]]
    local s3 = [[/* */]]
    local s4 = [[/*//*/]]
    local s5 = [[/*
    */]]
    local s6 = [[/*/*********
    ******/]]

    print("---test multi line comment proc begin---")
    f(s1)
    f(s2)
    f(s3)
    f(s4)
    f(s5)
    f(s6)
    print("---test multi line comment proc succed--")

end

local function testCapture()
    print("testCapture")

    local scanner = genScanner("//", "/*", "enum", "include", "struct")
    local p = 1
    while true do
        local k, b, e = scanner:match(text_com, p)
        if not k then
            break
        end

        p = e
        print(k, b, e)
    end
end
--testCapture()


local function test()
    local I = lpeg.Cp()
    local locale = lpeg.locale()
    local keyword = function (k)
        return lpeg.P(k) * -(locale.alnum + locale.alpha + lpeg.P("_"));
    end

    local anywhere = function (p)
        return lpeg.P{I * p * I + 1 * lpeg.V(1)}
    end

    local atwordboundary = function (p)
        return lpeg.P {I * p * -(locale.alnum + locale.alpha + lpeg.P("_")) * I +
            locale.alpha^0 * (1 - locale.alpha)^1 * lpeg.V(1)}
    end

    local I = lpeg.Cp()
    local P = comm * I

    local t1 =
[[
enumstruct
enum struct xuantao
]]

    local word = "enum"
    local e = atwordboundary(word)
    print("string length", #t1)
    print("atwordboundary1", lpeg.match(e, t1))

    local e1 = atwordboundary(keyword(word))
    print("atwordboundary2", lpeg.match(e1, t1))

    print("anywhere1", lpeg.match(anywhere(word), t1))
    local e2 = anywhere(keyword(word))
    print("anywhere2", lpeg.match(e1, t1))
    local e3 = anywhere(atwordboundary(word))
    print("anywhere3", lpeg.match(e1, t1))


    --print(P:match("////// xuagakdgka"))
    --print(P:match(t1))

    --local P2 = comm / ""
    --print(P2:match("////// xuagakdgka") .. '$')
    --print(P2:match(t1) .. '$')
end
--test()


--testCommentProc()
--testMultiLineCommentProc()

local function testExtractTag()
    local I = lpeg.Cp()
    local c1 = lpeg.P'//' * (1 - lpeg.P'\n')^0 * (lpeg.P'\n' + -lpeg.P(1))   -- 单行注释
    local c2 = lpeg.P'/*' * (1 - lpeg.P'*/')^0 * lpeg.P'*/'       -- 多行注释
    local comment = (c1 + c2)
    local tag = lpeg.P'/*<@' * c_name * lpeg.P'>*/'
    local tag2 = #tag
    --local only = (-tag * comment + tag)
    local _tag = P'/*<@' * p_space^0 * C(p_reference) * p_space^0 * P'>*/'
    local c_tag = p_space^0 * _tag^-1 * p_empty / 1
    local only = c_tag

    --print(tag:match("/*<@error>*/"))
    --print(comment:match("/*<@error>*/"))
    print("only1", only:match("/*<@error>*/"), "$")
    print("only2", only:match("/*<-error>*/"), "$")
    --print("test1", test:match("/*<@error>*/"), "$")
    --print("test2", test:match("/*<-error>*/"), "$")
end
--testExtractTag()

local function testCb()
    local equals = lpeg.P"="^0
    local open = P"[" * lpeg.Cg(equals, "init") * P"[" * lpeg.P"\n"^-1
    local close = P"]" * lpeg.C(equals) * P"]"
    local closeeq = lpeg.Cmt(close * lpeg.Cb("init"), function (s, i, a, b)
        print('close', string.format("s:%s, i:%s, a:%s, b:%s", s, i, a, b))
        return a == b
    end)
    local long_str = open * lpeg.C((lpeg.P(1) - closeeq)^0) * close / 1

    local str = [==[[=[xuantao]=]]==]
    local str1 = [==[[=[xuantao]=]]==]
    print("testCb1", long_str:match(str), "$")

    local longstring = P { -- from Roberto Ierusalimschy's lpeg examples
        V "open" * C((P(1) - V "closeeq")^0) * V "close" / function (a, b, c, d) return 
            print("l2", string.format("a:%s, b:%s, c:%s, d:%s", a, b, c, d))
        end;
        open = "[" * Cg((P "=")^0, "init") * P "[" * (P "\n")^-1;
        close = "]" * C((P "=")^0) * "]";
        closeeq = Cmt(V "close" * Cb "init", function (s, i, a, b) return a == b end)
    };
    print("testCb2", longstring:match(str), "$")

    local longstring3 = #(P '[[' + (P '[' * P '=' ^ 0 * P '['))
    local longstring4 = longstring3 * P(function(input, index)
        print("4", input, index)
        local level = input:match('^%[(=*)%[', index)
        if level then
            local _, stop = input:find(']' .. level .. ']', index, true)
            if stop then return stop + 1 end
        end
    end)
    print("testCb", longstring4:match(str), "$")
    --print("testCb", longstring4:match(str1), "$")
end
--testCb()

local function testCt()
    local ct1 = lpeg.Ct(C(p_decimal) * (p_space^0 * P',' * p_space^0 * C(p_decimal))^0)
    lib.Log("ct1", lpeg.match(ct1, "1"))
    lib.Log("ct1", lpeg.match(ct1, "1,2,3,4"))
    lib.Log("ct1", lpeg.match(ct1, "1, 2, 3, 4"))

    local ct2 = lpeg.Ct(C(p_decimal) * ((P'=' * C(p_decimal))^0 / 1) * ',' * Cg(p_identity, "name") * ((P'=' * C(p_decimal))^0 / 1))
    lib.Log("ct2", lpeg.match(ct2, "1,xuantao=102"))
    lib.Log("ct2", lpeg.match(ct2, "1,xuantao"))
    lib.Log("ct2", lpeg.match(ct2, "1=101,xuantao=102"))
end
--testCt()

local function testExtractComment()
    --local ec = P'//' * P'/'^0 * p_space^0 * C((1 - P'\n')^0) * (P'\n' + P(-1))
    local ec1 = P'//' * P'/'^0 * C((1 - P'\n')^0) * (P'\n' + P(-1))
    local ec2 = P'/*' * (P'*' - P'*/')^0 * C((1 - P'*/')^0) * P'*/'
    local ec = ec1 + ec2
    local str1 = "//"
    local str2 = "//xuantao"
    local str3 = "//  xuantao"
    local str4 = "//  xuantao  "
    local str5 = [[//  xuantao  
    //zouhui
]]
    local str6 = "//////"
    local str7 = "//////xuantao"
    local str8 = "/////  xuantao"
    local str9 = "////  xuantao  "
    local str10 = [[/////  xuantao  
//zouhui
]]
    local str11 = "/**/"
    local str12 = "/******/"
    local str13 = "/*xuantao*/"
    local str14 = "/*  xuantao  */"
    local str15 = [[/*  xuantao  
//zouhui
*/
]]
    print("ec", lpeg.match(ec, str1), "$")
    print("ec", lpeg.match(ec, str2), "$")
    print("ec", lpeg.match(ec, str3), "$")
    print("ec", lpeg.match(ec, str4), "$")
    print("ec", lpeg.match(ec, str5), "$")
    print("ec", lpeg.match(ec, str6), "$")
    print("ec", lpeg.match(ec, str7), "$")
    print("ec", lpeg.match(ec, str8), "$")
    print("ec", lpeg.match(ec, str9), "$")
    print("ec", lpeg.match(ec, str10), "$")
    print("ec", lpeg.match(ec, str11), "$")
    print("ec", lpeg.match(ec, str12), "$")
    print("ec", lpeg.match(ec, str13), "$")
    print("ec", lpeg.match(ec, str14), "$")
    print("ec", lpeg.match(ec, str15), "$")

    local ec2 = P'/*' * (P'*' - P'*/')^0 * (1 - P'*/')^0 * P'*/'
    print("ec", lpeg.match(ec2, str11), "$")
    print("ec", lpeg.match(ec2, str12), "$")
    print("ec", lpeg.match(ec2, str13), "$")
    print("ec", lpeg.match(ec2, str14), "$")
    print("ec", lpeg.match(ec2, str15), "$")
end
--testExtractComment()

local function testScanner()
    local s1 = (p_comment + p_multi_line_comment)^-1 * p_space^0 * P'enum' / 'enum'
    local s2 = (p_comment + p_multi_line_comment)^-1 * p_space^0 * P'struct' / 'struct'
    local s = P{ I * (s1 + s2) * I + (p_comment + p_multi_line_comment + 1) * V(1) } /
        function (b, k, e) return k, b, e end
 
    local str1 = "enum struct"
    local str2 = "/* desc */enum /* desc */struct"
    local str3 = [[
// test desc
enum
// test desc
struct
]]
    local str4 = [[
/* test desc
*/
enum
/* test desc
*/
struct
]]

    local str5 = [[
    // enum 
    // struct
]]
    local str6 = [[
/* test desc
    enum
*/
/* test desc
    struct
*/
]]

    local visit = function (str)
        local pos = 1
        print(str)
        while true do
            local k, b, e = lpeg.match(s, str, pos)
            if not k then
                break
            end
            pos = e
            print(k, b, e)
        end
    end

    visit(str1)
    visit(str2)
    visit(str3)
    visit(str4)
    visit(str5)
    visit(str6)
    -- lib.Log("s", lib.ToStr(lpeg.match(s, str1)), '$')
    -- lib.Log("s", lib.ToStr(lpeg.match(s, str2)), '$')
    -- lib.Log("s", lib.ToStr(lpeg.match(s, str3)), '$')
    -- lib.Log("s", lib.ToStr(lpeg.match(s, str4)), '$')
    -- lib.Log("s", lib.ToStr(lpeg.match(s, str5)), '$')
    -- lib.Log("s", lib.ToStr(lpeg.match(s, str6)), '$')
end
--testScanner()

local function testConst()
    local pc = (c_annotation^-1 / 1 * p_space^0 * P'const' * p_empty * C(p_reference) * p_empty * C(p_identity) * p_space * P'=' *
        p_space* C(p_decimal + p_hexadecimal + p_reference) * S',;'^-1 * S' \t'^0 * c_annotation^-1) /
        function (pre_desc, type, id, value, suf_desc)
            return {"const", id, {t = type, v = value, d = suf_desc or pre_desc}}
        end

    local str1 = "/*xuantao*/const int v1 = 10"
    local str2 = "const int v2 = 11, // zouhui"
    local str3 = "/* xuantao */const int v3 = 12; // zouhui"
    lib.Log(lpeg.match(pc, str1))
    lib.Log(lpeg.match(pc, str2))
    lib.Log(lpeg.match(pc, str3))
end
--testConst()

local function testTypedef()
    local td = (c_annotation^-1 / 1 * p_space^0 * P'typedef' * p_empty * C(p_reference) * p_empty * C(p_identity) *
        S',;'^-1 * S' \t'^0 * c_annotation^-1) /
        function (pre_desc, type, id, suf_desc)
            return {"typedef", id, {t = type, d = suf_desc or pre_desc}}
        end
    local str1 = "/*xuantao*/ typedef i32 code"
    local str2 = "typedef i32 code, // zouhui"
    local str3 = "/*xuantao*/ typedef i32 code ; // zouhui"
    lib.Log(lpeg.match(td, str1))
    lib.Log(lpeg.match(td, str2))
    lib.Log(lpeg.match(td, str3))
end
--testTypedef()

local function loadFile(f)
    do return "" end
    local file = io.open(f, 'r')
    if not file then
        return
    end

    local s = file:read('a')
    if #s >= 3 and
        s:byte(1, 1) == 0xef and s:byte(2, 1) == 0xbb and s:byte(3, 1) == 0xbf then
        return s:sub(4)
    end
    return s
end

local function testInclude()
    local i = P'include' * p_empty * (P"'" * C(p_reference) * P"'" +  P'"' * C(p_reference) * P'"') /
        function (f)
            local p = string.find(f, "%.")
            local s = p and string.sub(f, 1, p - 1) or f
            local t = loadFile(f)
            --local v = t and lpeg.match(V(1), t)
            return {"include", s, { f = f, v = v}}
        end

    local str1 = [[include 'shared.thrift']]
    local str2 = [[include "shared.thrift"]]
    local str3 = [[include " shared.thrift  "]]
    lib.Log(lpeg.match(i, str1))
    lib.Log(lpeg.match(i, str2))
    lib.Log(lpeg.match(i, str3))
end
--testInclude()

local TS
local c_include = P'include' * p_empty * (P"'" * C(p_reference) * P"'" +  P'"' * C(p_reference) * P'"') /
    function (f)
        local p = string.find(f, "%.")
        local s = p and string.sub(f, 1, p - 1) or f
        local t = loadFile(f)
        --local v = t and lpeg.match(V(1), t)
        return {"include", s, { f = f, v = v}}
    end
local c_typedef = (c_annotation * p_space * P'typedef' * p_empty * C(p_reference) * p_empty * C(p_identity) *
    S',;'^-1 * S' \t'^0 * c_annotation) / function (pre_desc, type, id, suf_desc)
        return {"typedef", id, {type = type, desc = suf_desc or pre_desc}}
    end
local c_const = c_annotation * p_space * P'const' * p_empty * C(p_reference) * p_empty * C(p_identity) * p_space * P'=' *
    p_space* C(p_decimal + p_hexadecimal + p_reference) * S',;'^-1 * S' \t'^0 * c_annotation /
    function (pre_desc, type, id, value, suf_desc)
        return {"const", id, {t = type, v = value, d = suf_desc or pre_desc}}
    end
local c_enum = P{
    V'enum',
    enum = c_annotation * p_space * P'enum' * p_space * c_tag * p_empty * C(p_identity) * p_empty * V'body' /
        function (desc, tag, id, v) return {"enum", id, {tag = tag, desc = desc, vars = v}} end,
    body = P'{' * p_empty * V'vars' * p_empty * P'}',
    vars = Ct(V'ele_begin'^0 * V'ele_end'^-1),
    ele_begin = p_empty * C(p_identity) * (V'value'^-1/1) * p_empty * P',' * S' \t'^0 * c_annotation /
        function(id, v, desc) return {id = id, value = v, desc = desc} end,
    ele_end = p_empty * C(p_identity) * (V'value'^-1/1) * S' \t'^0 * c_annotation /
        function(id, v, desc) return {id = id, value = v, desc = desc} end,
    value = p_empty * P'=' * p_empty * C(p_decimal + p_hexadecimal),
}

TS = P{
    (V'include' + V'typedef' + V'const' + V'enum') + (p_comment + p_multi_line_comment + 1) * V(1),
    include = P'include' * p_empty * (P"'" * C(p_reference) * P"'" +  P'"' * C(p_reference) * P'"') /
        function (f)
            local p = string.find(f, "%.")
            local s = p and string.sub(f, 1, p - 1) or f
            local t = loadFile(f)
            --local v = t and lpeg.match(V(1), t)
            return {"include", s, { f = f, v = v}}
        end,
    typedef = c_annotation * p_space * P'typedef' * p_empty * C(p_reference) * p_empty * C(p_identity) *
        S',;'^-1 * S' \t'^0 * c_annotation / function (pre_desc, type, id, suf_desc)
            return {"typedef", id, {type = type, desc = suf_desc or pre_desc}}
        end,
    const = c_annotation * p_space * P'const' * p_empty * C(p_reference) * p_empty * C(p_identity) * p_space * P'=' *
        p_space* C(p_decimal + p_hexadecimal + p_reference) * S',;'^-1 * S' \t'^0 * c_annotation / 
            function (pre_desc, type, id, value, suf_desc) return {"const", id, {t = type, v = value, d = suf_desc or pre_desc}} end,
    enum = P{
        V'enum',
        enum = c_annotation * p_space * P'enum' * p_space * c_tag * p_empty * C(p_identity) * p_empty * V'body' /
            function (desc, tag, id, v) return {"enum", id, {tag = tag, desc = desc, vars = v}} end,
        body = P'{' * p_empty * V'vars' * p_empty * P'}',
        vars = Ct(V'ele_begin'^0 * V'ele_end'^-1),
        ele_begin = p_empty * C(p_identity) * (V'value'^-1/1) * p_empty * P',' * S' \t'^0 * c_annotation /
            function(id, v, desc) return {id = id, value = v, desc = desc} end,
        ele_end = p_empty * C(p_identity) * (V'value'^-1/1) * S' \t'^0 * c_annotation /
            function(id, v, desc) return {id = id, value = v, desc = desc} end,
        value = p_empty * P'=' * p_empty * C(p_decimal + p_hexadecimal),
    }
}

--print("test")
--lib.Log(Ct(TS):match(textEnum))
--lib.Log(c_enum:match(textEnum))

local function getFileName(f)
    local p_slash = f:find("[/\\][^/\\]*$") or 1
    local p_dot = f:find("%.") or #f + 1
    print(p_slash, p_dot)
    return f:sub(p_slash + 1, p_dot - 1)
end

--print(getFileName("d:/a/b/c/d/xuantao.lua"))
--print(getFileName("d:\\a\\b\\c\\d\\xuantao.lua"))
--print(getFileName("d:\\a/b\\c\\d/xuantao.lua"))

local function getFilePath(f)
    local p_slash = f:find("[/\\][^/\\]*$")
    return p_slash and f:sub(1, p_slash) or ""
end
--print(getFilePath("d:/a/b/c/d/xuantao.lua"))
--print(getFilePath("d:\\a\\b\\c\\d\\xuantao.lua"))
--print(getFilePath("d:\\a/b\\c\\d/xuantao.lua"))


local function testSkip()
    --local s = P'struct' * p_empty * p_identity * p_empty * P'{';
    local s = P {
        "struct",
        struct = P'struct' * p_empty * p_identity * p_empty * P'{' * p_empty * V'body'* p_empty * P'}',
        body = "",
    }
end
