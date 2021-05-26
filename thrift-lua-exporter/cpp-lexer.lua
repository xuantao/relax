-- lexer 解析文本，预处理
-- 由于只是单独
local lib = require "lib"
local lpeg = require "lpeglabel"

lpeg.locale(lpeg)
local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C, Carg, Cb, Cc = lpeg.C, lpeg.Carg, lpeg.Cb, lpeg.Cc
local Cf, Cg, Cmt, Cp, Cs, Ct = lpeg.Cf, lpeg.Cg, lpeg.Cmt, lpeg.Cp, lpeg.Cs, lpeg.Ct
local Lc, T = lpeg.Lc, lpeg.T
--local alpha, digit, alnum = lpeg.alpha, lpeg.digit, lpeg.alnum
--local xdigit = lpeg.xdigit
--local space = lpeg.space

-- 匹配模式
local p_space = lpeg.S' \t\n'                                               -- 空白字符
local p_comment = P'//' * ((P'\\\n' + P(1)) - P'\n')^0 * (P'\n' + -P(1))         -- 单行注释
local p_multi_line_comment = P'/*' * (1 - P'*/')^0 * P'*/'                  -- 多行注释
local p_empty = p_space + p_comment + p_multi_line_comment                  -- 空白内容
local p_empty_list = p_empty^0
local p_sign = S'+-'^-1                                                     -- 符号(+-)
local p_digit = R('09')                                                     -- 数字
local p_idsafe = R('AZ', 'az', '\127\255') + P'_'                           -- id safe begin
local p_identity = p_idsafe * (p_idsafe + p_digit)^0                        -- identity
local p_rest = lpeg.alnum + P'_'
local p_char = P"'" * (P(1) - P"'")^0 * P"'"

-- 注释串提取器
local c_annotation = (P'//' * P'/'^0 * C((1 - P'\n')^0) * (P'\n' + P(-1)) +
    P'/*' * (P'*' - P'*/')^0 * C((1 - P'*/')^0) * P'*/')^-1/1
-- 标识符提取器
local c_indentity = C(p_idsafe * (p_idsafe + p_digit)^0)
-- 字符常量提取器
local c_char = P"'" * C((P(1) - P"'")^0) * P"'"
-- bool常量提起器
--local c_boolean = (C"true" + C"false" + C"TRUE" / "true" + C"FALSE" / "false") - (p_digit + p_idsafe)
-- 数值常量提取器
local c_numeric = P{
    C(V"float" + (V"bin" + V"oct" + V"dec" + V"hex") * V"intSuf"),
    bin = P"0" * S"bB" * S"01"^1 * -R"29",
    oct = P"0" * p_digit^0 * -S"bBxX",
    dec = R"19" * p_digit^0,
    hex = P'0' * S'xX' * R('09', 'af', "AF")^1,
    float = (p_digit^1 * P'.' * p_digit^0 + P'.' * p_digit^1) * (S'eE' * p_sign * p_digit^1)^-1 *(S"lL" + S"fF")^-1,
    intSuf = (S"uU" * S"lL" + S"lL" * S"uU" + S"uU" + S"lL")^-1,
}
-- 字符串常量提取器
local c_string = P{
    V"main" / function (s) return string.format("\"%s\"", s) end,
    main = Ct((V"string" * ((p_empty + P"\\\n")^0 * V"string")^0)^1) / table.concat,
    string = V"shortStr" + V"longStr",
    shortStr = P'"' * Cs((P'\\"' + (P(1) - S'"\n'))^0) * P'"',
    longStr = V"open" * Cs((P(1) - V"closeEq")^0) * V"close" / 1,
    open = P'R"' * Cg(Cs((P(1) - P'(')^0), "init") * P'(',
    close = P')' * Cs((P(1) - P'"')^0) * P'"',
    closeEq = Cmt(V"close" * Cb("init"), function (s, i, a, b) return a == b end),
}
-- 提取一行文本内容
local c_line = P{
    Cs((V"exp" - (P"//" + '\n'))^0) * (p_comment + P'\n' + -P(1)),
    exp = (p_multi_line_comment + S" \t" + P'\\\n')^1/' ' + c_char + c_string + C(P(1)),
}

local function buildSkipPatt(lcode, rcode)
    local l = P(lcode)
    local r = P(rcode)
    local w = #rcode
    return P{
        Ct(V"exp") / table.concat,
        exp = C(l) * (p_empty + c_char + c_string + V"exp" + (C(P(w)) - r))^0 * C(r),
    }
end

local function calcline (s, i)
    if i == 1 then return 1, 1 end
    local rest, line = s:sub(1,i):gsub("[^\n]*\n", "")
    local col = #rest
    return 1 + line, col ~= 0 and col or 1
end

local function testCapture()
    local M = lpeg.match
    assert(M(c_char, [[none]]) == nil)
    assert(M(c_char, [['none]]) == nil)
    assert(M(c_char, [[''none]]) == '')
    assert(M(c_char, [['1'none]]) == '1')
    assert(M(c_char, [['a'none]]) == 'a')
--[=[
    assert(M(c_boolean, [[none]]) == nil)
    assert(M(c_boolean, [[true1]]) == nil)
    assert(M(c_boolean, [[TRUEa]]) == nil)
    assert(M(c_boolean, [[false_]]) == nil)
    assert(M(c_boolean, [[FALSE0]]) == nil)
    print(M(c_boolean, [[true,]]))
    assert(M(c_boolean, [[true,]]) == "true")
    assert(M(c_boolean, [[TRUE]]) == "true")
    assert(M(c_boolean, [[false]]) == "false")
    assert(M(c_boolean, [[FALSE]]) == "false")
]=]
    assert(M(c_numeric, "a") == nil)
    assert(M(c_numeric, "4") == "4")
    assert(M(c_numeric, "1u") == "1u")
    assert(M(c_numeric, "1l") == "1l")
    assert(M(c_numeric, "1ul") == "1ul")
    assert(M(c_numeric, "1lu") == "1lu")
    assert(M(c_numeric, "0b10") == "0b10")
    assert(M(c_numeric, "0B10") == "0B10")
    assert(M(c_numeric, "0B10u") == "0B10u")
    assert(M(c_numeric, "0B12") == nil)
    assert(M(c_numeric, "0x1afu") == "0x1afu")
    assert(M(c_numeric, "1l") == "1l")
    assert(M(c_numeric, "1ul") == "1ul")
    assert(M(c_numeric, "1lu") == "1lu")

end

testCapture()

local p_skip_brace = P{
    "exp",
    exp = P'{' * (p_empty + c_char + c_string + V"exp" + (P(1) - S"{}"))^0 * P'}',
}

local test_brace1 = [[{}]]
local test_brace2 = [[{{}}]]
local test_brace3 = [[{{xuantao}}]]
local test_brace4 = [[{{xuantao/*}*/}}]]
local test_brace5 = [[{{"}"'}'xuantao/*}*/}}]]
local test_brace6 = [[{{xuantao/*}*/
// }
}}]]
--[[
print(lpeg.match(p_skip_brace, test_brace1))
print(lpeg.match(p_skip_brace, test_brace2))
print(lpeg.match(p_skip_brace, test_brace3))
print(lpeg.match(p_skip_brace, test_brace4))
print(lpeg.match(p_skip_brace, test_brace5))
print(lpeg.match(p_skip_brace, test_brace6))
]]

local function buildToken(type)
    return function (s) return {kind = type, value = s} end
end

--local kWeyWords

local TokenKind = {
    kSymbol = 1,  -- '#' '\\\n'
    kKeyword = 2,
    kConst = 3,
    kIdentify = 4,

    -- 以下为扩展类型（通过combine解析合并后的类型）
    kRaw = 11,          -- 原生类型(包含void、auto)
    kRefer = 12,        -- 引用类型(A::B::C)
    kMemberPtr = 13,    -- 成员指针(A::B::*)
    kOperator = 14,     -- 重载操作(operator symbol/literal/new/delete/type_cast)
}

local SkipType = {
    kAttr = 1,      -- [[attr]]
    kRound = 2,     -- (...)
    kSquare = 3,    -- [...]
    kAngle = 4,     -- <...>
    kBrace = 5,     -- {...}
}

local c_round_bracket = buildSkipPatt('(', ')')
local c_angle_bracket = P{
    Ct(V"exp") / table.concat,
    --exp = C(P'<') * (V"content" + (C(P(1)) - P'>'))^0 * C(P'>'),
    exp = C(P'<') * (V"content" - P'>')^0 * C(P'>'),
    content = p_empty + c_round_bracket + c_char + c_string + V"special" + V"exp" + C(P(1)),
    special = C(P">=" + "<=" + ">>" + "<<"),
}

-- 捕获有效字符并跳过
local c_skip_patts = {
    [SkipType.kAttr] = buildSkipPatt('[[', ']]') * Cp(),
    [SkipType.kRound] = buildSkipPatt('(', ')') * Cp(),
    [SkipType.kSquare] = buildSkipPatt('[', ']') * Cp(),
    [SkipType.kAngle] = c_angle_bracket * Cp(),
    [SkipType.kBrace] = buildSkipPatt('{', '}') * Cp(),
}

-- 跳跃
local p_jump_patt = p_empty + c_char / 0 + c_string / 0
for _, skip in ipairs(c_skip_patts) do
    p_jump_patt = p_jump_patt + skip / 0
end

local c_token_group = P{
    Ct(V"group") / table.concat,
    group = V"t1" + V"t2" + V"t3" + V"t4",
    t1 = C(p_identity) * p_empty_list * c_angle_bracket,
    t2 = C(p_identity) * p_empty_list * V"t3",
    t3 = C(P"::") * p_empty_list * V"group",
    t4 = C(p_identity),
}

local c_token = P{
    "token",
    token = (V"keywords" + V"symbols" + V"identity" + V"const") * Cp(),
    identity = C(p_idsafe * (p_idsafe + p_digit)^0) / buildToken(TokenKind.kIdentify),
    keywords = C((
        P"class" + "struct" + "union" + "enum" + "template" + "virtual" +
        "typename" + "decltype" + "final" + "constexpr" + "const" + "volatile" + "mutable" +
        "namespace" + "auto" + "operator" + "sizeof" + "typedef" + "static_assert" + "static" + "inline" + "explicit" +
        "public" + "protected" + "private" + "extern" + "using"
        ) * -p_rest) / buildToken(TokenKind.kKeyword),
    symbols = C(P"..." + "&&" + "||" + "->" + "+=" + "-=" + "|=" + "&=" + "/=" + "*=" + "==" + ">=" + "<="
        + "::" + "<<" + ">>" + ':' + '#'
        + '!' + '<' + '>' + "." + '(' + ')' + '[' + ']'
        + '{' + '}' + '\\' + ',' + ';' + '+' + '-' + '*' + '/' + '&' + '~'
        + '=') / buildToken(TokenKind.kSymbol),
    const = (c_numeric + c_string) / buildToken(TokenKind.kConst),
}

local c_split_refer = P {
    Ct(p_empty_list * (V"s" * p_empty_list * V"p" + V"p") * (p_empty_list * V"s" * p_empty_list * V"p")^0),
    s = P"::",
    p = Ct((C(p_identity) * p_empty_list * c_angle_bracket) + C(p_identity)) / table.concat,
}

-- skip empty text
local function adjustLexerCursor(lexer, p)
    p = lpeg.match(p_empty_list, lexer.source, p) or p
    lexer.cursor = p
end

local function splitRefer(s)
    return lpeg.match(c_split_refer, s)
end

local lexerMeta = {}

function lexerMeta:Location(range)
    --TODO: 需要修正宏引入的位置偏差
    local range = range or {self.cursor, self.cursor}
    local l, c = calcline(self.source, range[1])
    return {file = self.file, line = l, column = c, range = range}
end

function lexerMeta:PeekChar()
    return string.sub(self.source, self.cursor, 1)
end

function lexerMeta:Token()
    adjustLexerCursor(self, self.cursor)
    if self.cursor >= self.length then
        print("nothing")
        return
    end

    local t, p = lpeg.match(c_token, self.source, self.cursor)
    if not t then
        print("unkown code in pos", self.cursor, string.sub(self.source, self.cursor))
        return
    end

    t.range = {self.cursor, p}
    --print("lexer", t.kind, t.value, calcline(self.source, self.cursor))

    adjustLexerCursor(self, p)
    return t
end

function lexerMeta:Capture(type)
    local patt = c_skip_patts[type]
    if not patt then return end

    local s, p = lpeg.match(patt, self.source, self.cursor)
    if not s then return end

    local token = {
        type = TokenKind.kNone,
        value = s,
        range = {self.cursor, p},
    }
    self.cursor = p
    --print(debug.traceback())
    print("lexer", "capture", s)
    return token
end

function lexerMeta:Skip(type)
    local patt = c_skip_patts[type]
    if not patt then return false end

    local _, p = lpeg.match(patt, self.source, self.cursor)
    if not p then return false end

    self.cursor = p
    return true
end

-- 跳跃到一个控制字符，如',' ';' '{'
-- 如果是多个控制字符，需要每组控制字符的长度必须是一致的！
-- 返回对应的控制字符
function lexerMeta:JumpCtrl(...)
    local ctrl
    local len
    for _, c in ipairs({...}) do
        if ctrl then
            assert(len == #c)
            ctrl = ctrl + P(c)
        else
            len = #c
            ctrl = P(c)
        end
    end
    if not ctrl then return end

    local patt = ((-ctrl)*(p_jump_patt + P(len)))^0 * C(ctrl) * Cp()
    local code, p = lpeg.match(patt, self.source, self.cursor)
    if not code then return end

    self.cursor = p
    return code
end

-- 设置光标
function lexerMeta:SetCursor(cursor)
    self.cursor = cursor
    assert(self.cursor > 0)
end

-- 移动光标
function lexerMeta:MoveCursor(delta)
    self:SetCursor(self.cursor + delta)
end

-- 匹配指定模式
function lexerMeta:Match(patt)
    return lpeg.match(patt, self.source, self.cursor)
end

local c_qualifier = Ct(((p_empty_list / 0) * (
    (P"&&" / "rightValue") + (P'&' / "reference") + (P'*' / "pointer") + (P"const" / 'const') + (P"volatile" / 'volatile')))^0) * Cp()

function lexerMeta:Qualifiers()
    --print("lexerMeta:Qualifiers()")
    local q, p = lpeg.match(c_qualifier, self.source, self.cursor)
    --print("lexerMeta:Qualifiers()", q, p, string.sub(self.source, p))
    if q then self.cursor = p end
    return q
end

function lexerMeta:processDefine()
    local arg
    local t = self:Token()
    local n = self:Token()
    assert(t.kind == TokenKind.kIdentify)

    if n.value == '(' then
        arg = {}

        local a = self:Token()
        while a.value ~= ')' do
            table.insert(arg, a.value)
        end
        a = self:Token()
    else
        self:SetCursor(n.range[1])
    end

    local s, p = lpeg.match(c_line * Cp(), self.source, self.cursor)
    self:SetCursor(p)
    return t.value, {arg = arg, value = s}
end

-- 预处理
function lexerMeta:preprocess()
    local t = self:Token()
    while t do
        if t.value == "#" then
            local n = self:Token()
            if n.value == "define" then
                
            elseif n.vlaue == "undef" then

            end
        end

    end


end

function CreateLexer(source)
    return setmetatable({
        source = source,
        cursor = lpeg.match(p_empty_list, source) or 1,
        length = string.len(source),
    }, {__index = lexerMeta})
end

local p_find_jin = (p_empty + c_char / 0 + c_string / 0 + "##" + (P(1) - '#'))^0 * '#'

local source = [[1#xuan
#include
#define
#undef
]]

local p = 1
while p do
    local p1 = lpeg.match(p_find_jin, source, p)
    --print(p1)
    if not p1 then break end
    print(p1, string.sub(source, p1))
    p = p1-- and p1 + 1
end

--print(lpeg.match(p_find_jin, source))

