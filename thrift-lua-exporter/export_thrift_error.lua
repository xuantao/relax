-- export thrift error code
local lib = require "lib"
local lpeg = require "lpeg"
local locale = lpeg.locale()
local P, R, S, C, V, Cg = lpeg.P, lpeg.R, lpeg.S, lpeg.C, lpeg.V, lpeg.Cg
local Cmt, Cb = lpeg.Cmt, lpeg.Cb
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

local c_enum = lpeg.P'enum' * space * enum_label * space * c_name
local c_value = lpeg.S'=' * space * lpeg.C(lpeg.R'09'^0)
local c_note = lpeg.P'//' * space * lpeg.C((1-lpeg.P'\n')^0) * lpeg.P'\n'^0
local c_element = space * c_name * space * c_value^-1 * space * lpeg.S',;' * space * c_note^-1

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

local p_space = lpeg.S' \t\n'
local p_comment = lpeg.P'//' * (1 - lpeg.P'\n')^0 * (lpeg.P'\n' + -lpeg.P(1))   -- 单行注释
local p_multi_line_comment = lpeg.P'/*' * (1 - lpeg.P'*/')^0 * lpeg.P'*/'       -- 多行注释
local p_empty = (p_space + p_comment + p_multi_line_comment)^0
local p_sign = lpeg.S'+-'^-1
local p_digit = lpeg.R('09')
local p_decimal = p_digit^1
local p_hexadecimal = lpeg.P'0' * lpeg.S'xX' * lpeg.R('09', 'af', "AF")^1
local p_float = (p_digit^1 * lpeg.P'.' * p_digit^0 + lpeg.P'.' * p_digit^1) *(lpeg.S'eE' * p_sign * p_digit^1)^-1
local p_idsafe = lpeg.R('AZ', 'az', '\127\255') + lpeg.P'_'
--local p_ident = p_idsafe * (p_idsafe + p_digit + lpeg.P'.')^0
local p_ident = p_idsafe * (p_idsafe + p_digit)^0
local p_tag = p_space^0 * lpeg.P'/*<@' * c_name * lpeg.P'>*/'* p_space^0

local commentProc = function(t, str, pos)
    return p_comment:match(str, pos)
end
local multiLineCommentProc = function(t, str, pos)
    return p_multi_line_comment:match(str, pos)
end
local enumProc = function(t, str, pos)
    local element = lpeg.C(p_ident) * p_empty * (lpeg.P'=' * p_empty * lpeg.C(p_decimal + p_hexadecimal) * p_empty)^-1 * lpeg.P','^-1
    local p = lpeg.P'enum' * p_empty * lpeg.C(p_ident) * p_empty * lpeg.P'{' * (p_empty * element * p_empty)^0 * p_empty * lpeg.P'}'
    local p_enum = lpeg.P{
        lpeg.P'enum' * lpeg.V'empty' * p_ident * lpeg.V'empty' * lpeg.P'{' * lpeg.V'empty' * lpeg.V'body' * lpeg.V'empty' * lpeg.P'}';
        empty = (p_space + p_comment + p_multi_line_comment)^0;
        --body = lpeg.V'empty' + lpeg.V'stat';
        body = lpeg.V'stat'^0;
        stat = p_ident * lpeg.V'empty' * lpeg.V'assign' * lpeg.P','^-1;
        assign = (lpeg.P'=' * lpeg.V'empty' * lpeg.V'value' * lpeg.V'empty')^-1;
        value = p_decimal + p_hexadecimal;
    }
    print("1", p:match(str, pos))
    --print("2", p_enum:match(str, pos))


    local p_enum2 = P{
        V'enum',
        enum = P'enum' * lpeg.C(p_tag + V'space'/"") * C(p_ident) * V'space' * V'body',
        space = (p_space + p_comment + p_multi_line_comment)^0,
        body = P'{' * V'space' * V'element'^0 * V'space' * P'}',
        element = V'space' * C(p_ident) * (V'space' * P'=' * V'space' * C(p_decimal + p_hexadecimal))^-1 * V'space'*P','^-1,
    }
    print("3", p_enum2:match(str, pos))
end

local function testEnumProc()
    enumProc(nil, [[enum test{}]], 1)
    enumProc(nil, [[enum/*<@error>*/test{}]], 1)
    enumProc(nil, [[enum test {}]], 1)
    enumProc(nil, [[enum test { }]], 1)
    enumProc(nil, [[enum test { xuantao, zouhui }]], 1)
    enumProc(nil, [[enum test { xuantao=1, zouhui }]], 1)
    enumProc(nil, [[enum test { xuantao = 1, zouhui }]], 1)
    enumProc(nil, [[enum test { xuantao = 1, zouhui = 2, }]], 1)
    enumProc(nil, [[enum test { xuantao/* xuantao */ = 1, zouhui/*zouhui*/  }]], 1)
end

local function testDigit()

end

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
    local only = (-tag * comment + tag)

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

    local ct2 = lpeg.Ct(C(p_decimal) * (P'=' * C(p_decimal))^0 * ',' * C(p_ident) * (P'=' * C(p_decimal))^0)
    lib.Log("ct2", lpeg.match(ct2, "1,xuantao=102"))
    lib.Log("ct2", lpeg.match(ct2, "1,xuantao"))
    lib.Log("ct2", lpeg.match(ct2, "1=102,xuantao=102"))
end
testCt()



