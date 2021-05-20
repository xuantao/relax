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
        "public" + "protected" + "private" + "extern"
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
    print("lexerMeta:Qualifiers()")
    local q, p = lpeg.match(c_qualifier, self.source, self.cursor)
    print("lexerMeta:Qualifiers()", q, p, string.sub(self.source, p))
    if q then self.cursor = p end
    return q
end

function CreateLexer(source)
    return setmetatable({
        source = source,
        cursor = lpeg.match(p_empty_list, source) or 1,
        length = string.len(source),
    }, {__index = lexerMeta})
end

local classParser = {}
--[=[
-- 测试括弧
local p =  P{
    Ct(V"exp") / table.concat,
    exp = C(P'<') * (V"content" + (C(P(1)) - P'>'))^0 * C(P'>'),
    content = p_empty + c_round_bracket + c_char + c_string + C(V"special") + V"exp",
    special = P">=" + P"<=" + P">>" + P"<<",
}

--print(lpeg.match(c_angle_bracket, [[<x, "111",(1>2),(1<2),y /*xxx*/  z,1>>2,2<<3,4>=4,5<=6>]]))
]=]

function log(t)
    if not t then return end
    if type(t) == "table" then
        for _, v in ipairs(t) do
            print(v)
        end
    else
        print(t)
    end
end
--[=[
log(lpeg.match(c_angle_bracket, [[<sfag>]]))
log(lpeg.match(c_type, [[:: function<sfag> safa]]))
]=]

local c_class_identifyer = P{
    V"identifyer" / function (p, c) return tonumber(c), p end,
    identifyer = P"final"^0 * p_empty_list * Cp() * (V"variate" + V"inherit" + V"body" + V"stop"),
    variate = (P"const" + "volatile" + S"&*" + p_identity) / '1',
    inherit = P':' / '2',
    body = P'{' / '3',
    stop = P';' / '4',
}

--[=[
print(lpeg.match(c_class_identifyer, [[ : public]]) == 2)
print(lpeg.match(c_class_identifyer, [[ final : public]]))
print(lpeg.match(c_class_identifyer, [[xuantao]]))
print(lpeg.match(c_class_identifyer, [[*v]]))
print(lpeg.match(c_class_identifyer, [[const v]]))
print(lpeg.match(c_class_identifyer, [[&& v]]))
print(lpeg.match(c_class_identifyer, [[*& s]]))
print(lpeg.match(c_class_identifyer, [[ { }]]))
print(lpeg.match(c_class_identifyer, [[ ; ]]))
print(lpeg.match(c_class_identifyer, [[ = ]]))
log(lpeg.match(c_qualifier, [[const *&]]))
log(lpeg.match(c_qualifier, [[&&]]))
local function testJump(text, ...)
    local ctrl
    for _, c in ipairs({...}) do
        if ctrl then ctrl = ctrl + P(c)
        else ctrl = P(c) end
    end
    if not ctrl then return end

    local c, p = lpeg.match(((-ctrl)*(p_jump_patt + P(1)))^0 * C(ctrl) * Cp(), text)
    return c
end

print(testJump([[xuantao {} ; zouhui]], '{'))
print(testJump([[xuantao {} ; zouhui]], ';'))
]=]

local AccessType = {
    kPublic = 1,
    kProcteced = 2,
    kPrivate = 3,
}

local ObjectType = {
    --kFile = 1,
    kGlobal = 1,
    kNamespace = 2,     -- 名字空间
    kClass = 3,         -- 自定义类型
    kEnum = 4,          -- 枚举
    kVariate = 5,       -- 变量
    kFunction = 6,      -- 函数
    kOverload = 7,      -- 重载函数
    kAlias = 8,         -- 引用
}

local SentanceState = {
    kSpecifierSeq = 1,  -- 声明
    kDeclcrator = 2,    -- 
    kSurffix = 3,       -- 后续扩展
}

local TypeKind = {
    kRaw = 1,           -- 原生类型
    kRefer = 2,         -- 引用自定义类型
    kFunction = 3,      -- 函数
    kMemberPtr = 4,     -- 成员指针
    kMemberFuncPtr = 5, -- 成员函数指针
}

local OperatorKind = {
    kSymbol = 1,        -- 符号
    kTypeCast = 2,      -- 类型转换
    kLiteral = 3,       -- 字符串连接
    kNewDelete = 4,     -- new/delete 操作
}

local function toAccessType(name)
    if name == "public" then return AccessType.kPublic
    elseif name == "proctected" then return AccessType.kProcteced
    elseif name == "private" then return AccessType.kPrivate
    end
end

local function toMap(ary)
    local map = {}
    if ary then
        for _, code in ipairs(ary) do
            map[code] = true
        end
    end
    return map
end

local RawTypes = toMap{
    "char", "wchar_t", "bool", "signed", "unsigned",
    "short", "int", "long", "float", "double"
}

local BaseTypes = toMap{
    "char", "wchar_t", "bool", "signed", "unsigned",
    "short", "int", "long", "float", "double",
    "BOOL", "DWORD", --TODO: 追加一些常用基础类型定义
}

local env = {
    domain = { type = 1, parent = nil, children = {}, enums = {}, vars = {}, funcs = {}, classes = {}, typedefs = {} },
    domainMap = {},
    marks = { isStatic = false, qualifiers = {}, },
}

local function makeSentance()
    return {
        specifier,
        qualifier = {},
        isDecl = false,
        declarator = {type = {qualifier = {}}, attr = {},},
        attr = {},
    }
end

local domainMeta = {}

local function lookupImpl(domain, path, idx, preferType)
    local kind = domain.kind
    if kind ~= ObjectType.kGlobal and kind ~= ObjectType.kNamespace and
        kind ~= ObjectType.kClass and king ~= ObjectType.kEnum then
        return nil
    end

    domain = domain:Get(path[idx], preferType)
    idx = idx + 1
    if not domain or not path[idx] then return domain end

    return lookupImpl(domain, path, idx, preferType)
end

function domainMeta:Lookup(name, preferType)
    name = lib.Trim(name)
    local p = splitRefer(name)
    local n = #p
    if n == 0 then return nil end

    if name[1] == ':' and name[2] == ':' then
        local d = self.domain
        while d.domain do d = d.domain end
        return lookupImpl(d, p, 1, preferType)
    else
        local d = self.domain
        local prefer = n > 1 and preferType or true
        while d do
            local t = d:Get(p[1], prefer)
            if t then
                return n == 1 and t or lookupImpl(t, p, 2, preferType)
            else
                d = d.domain
            end
        end
    end
end


function domainMeta:Get(id, preferType)
    if self.kind == ObjectType.kClass and self.id == id then
        return self
    end

    --TODO: 查找优先级，默认优先查找变量？
    for _, obj in ipairs(self.objs) do
        if obj.id == id and (not preferType or obj.kind ~= ObjectType.kVariate) then
            return obj
        end
    end
end

local parserMeta = {}



--[[ 创建解析器
  cfg = { -- 环境配置
    paths,      -- 文件搜索路径
    ignores,    -- 忽略表
    tags,       -- 标记表
  }
]]
function CreateParser(cfg)
    local cfg = cfg or {}
    local parser = setmetatable({
            paths = cfg.paths or {},
            tags = toMap(cfg.tags),
            ignores = toMap(cfg.ignores),
            defines = {},
            lexer = nil,            -- 词法解析
            lexerStack = {},        -- 文件堆栈
            domain = setmetatable({kind = ObjectType.kGlobal, id = "", objs = {},}, { __index = domainMeta }),
            sentance = makeSentance(),
            block = {kind = 1, closeSymbol = ""},
            stack = {},
            anonymousIdx = 0,
        }, {
            __index = parserMeta
        }
    )
    parser.block.objs = parser.domain.objs
    return parser
end

function parserMeta:Parse(file)
    --self.lexer = CreateLexer(source)
end

function parserMeta:ParseSource(source)
    self.lexer = CreateLexer(source)
    self:doParse()
    --lib.Log(self.block.objs)
    --[[
    self.sentance = makeSentance()
    local ok = self:tryParseDeclImpl()
    print(source)
    lib.Log(ok)
    ]]
end

function parserMeta:setSource(source)
    self.lexer = CreateLexer(source)
end

function parserMeta:resetSentance()
    self.sentance = makeSentance()
end

function parserMeta:getToken(isExpect)
    local cursor = self.lexer.cursor
    local token = self.lexer:Token()
    -- 标记
    if token and self.tags[token.value] then
        token = processTag(token)
    end
    -- 忽律
    if token and self.ignores[token.value] then
        token = processIngore(token)
    end
    -- 宏
    if token and self.defines[token.value] then
        token = processIngore(token)
    end

    if isExpect and not token then
        self:error("")
    end
    return token
end

function parserMeta:peekToken(skipEmpty)
    local cursor = self.lexer.cursor
    local token = self:getToken(true)
    self.lexer:SetCursor(cursor)
    if skipEmpty then
        self:rollback(token)    --跳过空白内容
    else
        self.lexer:SetCursor(cursor)
    end
    return token
end

function parserMeta:rollback(token)
    self.lexer:SetCursor(token.range[1])
end

function parserMeta:expect(code)
    local cursor = self.lexer.cursor
    local token = self:getToken()
    if not token or token.value ~= code then
        self:error("expect code is not exist")
    end
    return token
end

function parserMeta:processIngore(token)
    local after = self:getToken()
    if not after then return end

    if after.value == '(' then
        if not self.lexer:JumpCtrl(')') then
            error("111")
        end

        after = self:getToken()
    end
    return after
end

function parserMeta:processDefine(token)
    --TODO:

    return token
end

function parserMeta:processTag(token)
    --TODO:
end

function parserMeta:preProcess()
    --self.lexer
    local s, p = lpeg.match(c_line * Cp(), self.lexer.source, self.lexer.cursor)
    if s then
        print("skip", s)
        self.lexer.cursor = p
    else
        self:error()
    end
end

function parserMeta:error(desc, range)
    local loc = self.lexer:Location(range)
    lib.Log(self.sentance)
    print(string.format("error %s at:%s:%s-%s, src:%s", desc, loc.file, loc.line, loc.column, string.sub(self.lexer.source, self.lexer.cursor, self.lexer.cursor + 20)))
    error(desc)
end

function parserMeta:warning(desc)
    --lib.Log(self.sentance)
    print("at:" .. string.sub(self.lexer.source, self.lexer.cursor, self.lexer.cursor + 20))
    print(debug.traceback())
end

function parserMeta:doParse()
    while true do
        local token = self:getToken()
        if not token then
            if self.block.closeSymbol ~= "" then
                self:error("unexpect end")
            end
            break
        end

        local value = token.value
        if token.kind == TokenKind.kKeyword then
            if value == "public" or value == "proctected" or value == "private" then
                if self.domain.kind ~= ObjectType.kClass then
                    self:error("only class/struct accept the keyword " .. value)
                end
                self:expect(':')
                self.block.access = toAccessType(value)
            elseif value == "const" or value == "volatile" or value == "mutable" then
                self:appendQualifier(token)
            elseif value == "using" then
                self:parseUsing()
            elseif value == "typedef" then
                self.sentance.isTypedefing = true
            elseif value == "typename" then
                self:setSpecifier(self:tryCombine())
            elseif value == "decltype" then
                self:setSpecifier(self:tryCombine(token))
            elseif value == "operator" then
                self:appendDeclSeq(self:combineOperator(token))
            elseif value == "auto" then
                self:setSpecifier({kind = TokenKind.kRefer, value = value, range = token.range})
            elseif value == "void" then
                self:setSpecifier({kind = TokenKind.kRaw, value = value, range = token.range})
            elseif value == "namespace" then
                self:parseNamespace()
            elseif value == "struct" or value == "class" or value == "union" then
                self.sentance.isStruct = true
                self:parseStruct(value == "struct")
            elseif value == "enum" then
                self:processEnum()
            elseif value == "virtual" then
                self.sentance.attr.isVirtual = true
            elseif value == "constexpr" then
                self.sentance.attr.isConstExpr = true
            elseif value == "static" then
                self.sentance.attr.isStatic = true
            elseif value == "inline" or value == "override" or value == "final" or value == "explicit" then
                -- ignore
            elseif value == "extern" then
            elseif value == "friend" or value == "template" or value == "static_assert" then
                local c = self.lexer:JumpCtrl(';', '{')
                if not c then
                    self:error("xxxxxxxxxxx")
                elseif c == '{' then
                    if not self.lexer:JumpCtrl('}') then
                        self:error("yyyyyyyyyy")
                    end
                end
            else
                self:error("")
            end
        elseif token.kind == TokenKind.kSymbol then
            if value == '{' then  -- jump out unamed scope
                self:openBrace()
            elseif value == '}' then
                self:closeBrace()
            elseif value == '#' then
                self:preProcess()
            elseif value == ':' then
                self:parseBitField()
            elseif value == "::" then
                self:appendDeclSeq(self:tryCombine(token))
            elseif value == '~' then
                self.sentance.declarator.attr.isDestructor = true
            elseif value == ',' then
                self:meetComma()
            elseif value == ";" then
                self:meetSemicolon()
            elseif value == '(' then
                self:openBracket(token)
            elseif value == ')' then
                self:closeBracket(token)
            elseif value == '=' then
                self:parseAssign()
            elseif value == '[' then
                self:parseArray()
            elseif value == "[[" then
                if not self.lexer:JumpCtrl("]]") then
                    self:error("can not process attribute")
                end
            elseif value == '...' then
                if self.sentance.declarator.seq then
                    self:error("")
                end
                self.sentance.declarator.seq = {kind = TokenKind.kIdentify, value = value, range = token.range}
            elseif value == '&&' or value == '&' or value == '*' then
                self:appendQualifier(token)
            else
                lib.Log(token)
                self:error("");
            end
        elseif token.kind == TokenKind.kIdentify then
            self:appendDeclSeq(self:tryCombine(token))
        else
            self:error("unknown")
        end

        if token.value == self.block.closeSymbol then
            print("1 bbbbbbbbbbbbbbbbbbbb")
            break
        end
    end
end

function parserMeta:safeParse()
    return pcall(self.doParse, self)
end

function parserMeta:getDomain(name)
    local domain = self.domain
    while domain do
        if domain.name == name then return domain end

        local d = domain.children[name]
        if d then return d end

        domain = domain.parent
    end
end

function parserMeta:pushBlock(domain, closeSymbol)
    table.insert(self.stack, {
        domain = self.domain,
        sentance = self.sentance,
        block = self.block,
    })

    self.domain = domain
    self.sentance = makeSentance()
    self.block = {closeSymbol = closeSymbol, objs = {}}
    return self.block
end

function parserMeta:popBlock()
    local pos = #self.stack
    local backup = self.stack[pos]
    table.remove(self.stack, pos)
    --print("parserMeta:popBlock", pos)
    self.domain = backup.domain
    self.sentance = backup.sentance
    self.block = backup.block
end

local function mergeQualifier(l, r)
    --TODO: 合并const、volaite
    --print("merge qualifier")
    if not l then return r end
    if not r then return l end
    for _, v in ipairs(r) do
        table.insert(l, v)
    end
    --print("merge qualifier")
    --lib.Log(l)
    return l
end

function parserMeta:parseInherits(defaultAccess)
    local inherits = {}
    local element = {access = defaultAccess}

    while true do
        local token = self:getToken(true)
        local value = token.value
        if value == "decltype" or value == "::" or token.kind == TokenKind.kIdentify then
            local com = self:tryCombine(token)
            if com.kind ~= TokenKind.kIdentify and com.kind ~= TokenKind.kRefer then
                self:error("expect a type name")
            end

            table.insert(inherits, element)
            element.value = com.value
            element = {access = defaultAccess}

            local symbol = self:peekToken(true).value
            if symbol == '{' then
                break
            elseif symbol == ',' then
                self:getToken() -- skip ','
            else
                print("symbol", symbol)
                lib.Log(token)
                self:error("unexpect token")
            end
        elseif value == "virtual" then
            element.isVirtual = true
        elseif value == "public" or value == "protected" or value == "private" then
            element.access = toAccessType(value)
        elseif value ~= "typename" and value ~= "struct" and value ~= "class" then
            self:error("unexpect token in class inherit list")
        end
    end

    if #inherits == 0 then
        self:error("not get any inherit type")
    end
    return inherits
end

function parserMeta:combineDecltype(token)
    if self:peekToken(true).value ~= '(' then
        self:error("11")
    end

    local cap = self.lexer:Capture(SkipType.kRound)
    if not cap then
        self:error("")
    end

    local ret = {
        kind = TokenKind.kRefer,
        value = string.format("%s%s", token.value, cap.value),
        range = {token.range[1], cap.range[2]},
    }

    if self:peekToken(true).value == "::" then
        local com = self:tryCombine()
        if not com then
            self.error("")
        end

        ret.kind = com.kind
        ret.value = ret.value .. com.value
        ret.range[2] = com.range[2]
    end
    return ret
end

function parserMeta:tryParseType()
    local cursor = self.lexer.cursor
    local q = self:tryParseCV()
    local c = self:tryCombine(token)
    if not c then
        self:error("") 
        self.lexer:SetCursor(cursor)
        return
    end

    self:tryParseQualifier(q)
    local type = {
        qualifier = q,
        name = c.value
    }
    if c.kind == TokenKind.kRaw then
        type.kind = TypeKind.kRaw
    elseif c.kind == TokenKind.kIdentify or c.kind == TokenKind.kRefer then
        type.kind = TypeKind.kRefer
    elseif c.value == "auto" then
        type.kind = TypeKind.kRefer
    else
        self:error(string.format("unexpect token %s", c.value))
        self.lexer:SetCursor(cursor)
        return
    end

    while true do
        c = self:tryCombine()
        if not c then
            break
        end

        if c.kind ~= TokenKind.kMemberPtr then
            self:error("")
        end

        local ret = type
        type = {
            kind = TypeKind.kMemberPtr,
            name = "",
            class = c.value,
            ret = ret,
        }
        type.qualifier = self:tryParseQualifier()
    end

    if self:peekToken().value == '(' then
        local t = self:getToken()
        local d, m = self:tryParseDecl()
        --if not d or d.type.kind ~= TypeKind.kMemberPtr or 
        print("try parse type")
        lib.Log(d)
        if d then
            mergeQualifier(type.qualifier, m.qualifier)
            lib.ShallowCopy(type, m)
            type = d.type

            --TODO: 处理函数属性
            self:tryParseFuncAttr()
        else
            self:rollback(t)
        end
    end

    return type
end

function parserMeta:combineOperator(opToken)
    local com = {kind = TokenKind.kOperator, value = opToken.value, range = opToken.range}
    local qualifier = self:tryParseQualifier()
    local token = self:getToken()
    local value = token.value
    if token.kind == TokenKind.kSymbol then -- 重载操作符
        local op = token.value
        com.range = {token.range[1], token.range[2]}
        if value == '(' then
            op = '()'
            com.range[2] = self:expcet(')').range[2]
        elseif value == '[' then
            op = "[]"
            com.range[2] = self:expcet(']')
        end
        com.value = com.value .. " " .. op
        com.operator = {kind = OperatorKind.kSymbol, value = op}
    elseif value == "\"\"" then       -- 重载字面量连接
        local next = self:getToken(true)
        if next.kind ~= TokenKind.kIdentify then
            self:error()
        end

        com.value = com.value .. ' "" ' .. next.value
        com.range = next.range
        com.operator = {kind = OperatorKind.kLiteral, value = next.value}
    elseif value == "new" or value == "delete" then
        local op = value
        com.range = token.range
        if self.peekToken(true).value == '[' then
            self:getToken()
            op = op .. '[]'
            com.range[2] = self:expect(']').range[2]
        end

        com.value = com.value .. ' ' .. op
        com.operator = {kind = OperatorKind.kNewDelete, value = op}
    else -- 重载类型转换
        self:rollback(token)
        com.range[1] = self.lexer.cursor
        local type = self:tryParseType()
        if not type then
            self:error("operator type cast failed")
        end

        com.operator = {kind = OperatorKind.kTypeCast, value = type}
        com.range[2] = self.lexer.cursor
    --else
    --    self:error("")
    end

    if self:peekToken().value ~= '(' then
        self:error()
    end
    return com
end

function parserMeta:combineIdentify(token)
    local value = token.value
    local pos = token.range[2]
    if self:peekToken(true).value == '<' then
        local cap = self.lexer:Capture(SkipType.kAngle)
        if not cap then
            self:error("\"<>\" is not pair")
        end

        value = value .. cap.value
        pos = cap.range[2]
    end
    return value, pos
end

function parserMeta:tryCombine(token)
    token = token or self:getToken()
    if not token then return end

    if RawTypes[token.value] then   -- 原生类型可以连接
        local ret = {value = "", range = {token.range[1], 0}}
        local next = self:getToken()
        ret.kind = TokenKind.kRaw
        ret.value = token.value
        while RawTypes[next.value] do
            ret.value = ret.value .. ' ' .. next.value
            ret.range[2] = next.range[2]
            next = self:getToken(true)
        end
        self:rollback(next)
        return ret
    elseif token.value == "decltype" then
        return self:combineDecltype(token)
    elseif token.value == "operator" then
        return self:combineOperator(token)
    elseif token.value == "void" or token.value == "auto" then
        return token
    elseif token.kind ~= TokenKind.kIdentify and token.value ~= "::" then
        self:rollback(token)
        return
    end

    local ret = {value = "", range = {token.range[1], 0}}
    if token.kind == TokenKind.kIdentify then
        ret.value, ret.range[2] = self:combineIdentify(token)
        ret.kind = token.value == ret.value and TokenKind.kIdentify or TokenKind.kRefer
    elseif token.value == "::" then
        local next = self:getToken(true)
        if next.kind ~= TokenKind.kIdentify then
            self:error("\"::\" need flow a qualified identity")
        end

        local value, pos = self:combineIdentify(next)
        ret.kind = TokenKind.kRefer
        ret.value = ret.value .. value
        ret.range[2] = pos
    end

    token = self:getToken(true)
    if token.value ~= "::" then
        self:rollback(token)
        return ret
    end

    ret.kind = TokenKind.kRefer
    while true do
        ret.value = ret.value .. token.value
        ret.range[2] = token.range[2]

        local next = self:getToken(true)
        if next.value == "*" then
            ret.kind = TokenKind.kMemberPtr
            ret.value = ret.value .. next.value
            ret.range[2] = next.range[2]
            break
        elseif next.value == '~' then               -- 析构函数
            local com = self:tryCombine()
            if not com then
                self:error("expect a type identify")
            end

            ret.value = ret.value .. next.value .. com.value
            ret.range[2] = com.range[2]
            break
        elseif next.value == "operator" then        -- 重载操作符
            self:rollback(next)
            local com = self:combineOperator()
            if not com then
                self:error("1")
            end

            ret.kind = com.kind
            ret.value = ret.value .. com.value
            ret.range[2] = com.range[2]
            ret.operator = com.operator
            break
        elseif next.value == "template" then    -- 内部模板类型
            local com = self:tryCombine()
            if not com then
                self:error()
            end

            ret.kind = com.kind
            ret.value = string.format("%stemplate %s", ret.value, com.value)
            ret.range[2] = com.range[2]
            break
        elseif next.kind == TokenKind.kIdentify then
            local value, pos = self:combineIdentify(next)
            ret.value = ret.value .. value
            ret.range[2] = pos

            token = self:getToken(true)
            if token.value ~= "::" then
                self:rollback(token)
                break
            end
        else
            self:error()
        end
    end

    return ret
end

function parserMeta:parseUsing()
    --TODO:
end

function parserMeta:parseNamespace()
    local token = self:getToken()
    local symbol
    if token.kind == TokenKind.kIdentify then
        symbol = self:getToken()
    else
        symbol = token
        token = nil
    end

    if symbol.value == ';' then
        return
    elseif symbol.value ~= '{' then
        self:error("unexpect token")
    end

    local isAnonymous
    if not token then
        isAnonymous = true
        token = {value == self:genAnonymousName(), range = symbol.range}
    end

    local ns = self:makeNamespace(token)
    ns.isAnonymous = isAnonymous

    local block = self:pushBlock(ns, '}')
    block.objs = ns.objs
    self:doParse()
    self:popBlock()
end

function parserMeta:processEnum()
    local name = ''
    local token = self:getToken()
    if token.value == 'class' then
        token = self:getToken()
    end

    if token.kind ==TokenKind.kIdentify then
        name = token.value
        token = self:getToken()
    end

    if token.value == ';' then
        self:resetSentance()
        return
    end

    if token.value == ':' then  -- underlying type
        local combined = self:tryCombine()
        print("ccc", combined.id)

        token = self:getToken()
    end
    if token.value ~= '{' then
        self:error("11111111")
    end

    --TODO: parse enum
    if not self.lexer:JumpCtrl('}') then
        self:error("")
    end
end

-- 生成唯一名称
function parserMeta:genAnonymousName()
    self.anonymousIdx = self.anonymousIdx + 1
    return string.format("_anonymous_%s_", self.anonymousIdx)
end

function parserMeta:makeNamespace(seq)
    local ns = self.domain:Get(seq.value, true)
    if ns then
        return ns
    end

    ns = setmetatable({
            kind = ObjectType.kNamespace,
            id = seq.name,
            loc = self.lexer:Location(seq.range),
            domain = self.domain,
            attr = {},
            objs = {},
        }, {
            __index = domainMeta
        }
    )

    table.insert(self.domain.objs, ns)
    return ns
end

function parserMeta:makeClass(seq, isStruct, isPreDef)
    if not seq or seq.kind ~= TokenKind.kIdentify then
        lib.Log(seq)
        self:error("declare class/struct name is invalid", seq.range)
        return
    end

    if self.domain.kind ~= ObjectType.kGlobal and
        self.domain.kind ~= ObjectType.kNamespace and
        self.domain.kind ~= ObjectType.kClass then
        print("111111111 domain.kind", self.domain.kind)
        self:error("current domain not allow declare class/struct type", seq.range)
    end

    local cls = self.domain:Lookup(seq.value)
    if not cls then
        cls = setmetatable({
                kind = ObjectType.kClass,
                id = seq.value,
                isStatic = isStatic,
                loc = self.lexer:Location(seq.range),
                attr = {},
                domain = self.domain,
                isPreDef = isPreDef,
                objs = {},
            }, {
                __index = domainMeta
            }
        )
        cls.isStruct = isStruct
        cls.isPreDef = isPreDef
        cls.loc = self.lexer:Location(seq.range)
        table.insert(self.domain.objs, cls)
    elseif not isPreDef then
        if not cls.isPreDef then
            self:error("rename")
        end
    end

    if not isPreDef then
        cls.isPreDef = nil
        cls.loc = self.lexer:Location(seq.range)
    end
    return cls
end

function parserMeta:parseStruct(isStruct)
    local combined = self:tryCombine()
    local symbol = self:getToken()

    if symbol.value == ';' then
        self:makeClass(combined, isStruct, true)
        self:resetSentance()
        return  -- 前置声明
    end

    if symbol.value ~= "final" and symbol.value ~= ':' and symbol.value ~= '{' then
        self:rollback(symbol)

        if not combined or
            (combined.kind ~= TokenKind.kIdentify and combined.kind ~= TokenKind.kRefer) then
            self:error("class/struct expect and identify name")
        end
        self:setSpecifier(combined)
        return  -- 定义变量
    end

    -- 匿名数据结构
    local isAnonymous
    if symbol.value == '{' and not combined then
        isAnonymous = true
        combined = {
            kind = TokenKind.kIdentify,
            value = self:genAnonymousName(),
            range = symbol.range,
        }
    end

    local cls = self:makeClass(combined, isStruct, nil)
    cls.isAnonymous = isAnonymous
    self:setSpecifier(combined)

    if symbol.value == "final" then
        cls.isFinal = true
        symbol = self:getToken()
    end

    if symbol.value == ':' then
        cls.inherits = self:parseInherits(isStruct and AccessType.kPublic or AccessType.kPrivate)
        symbol = self:getToken()
    end

    if symbol.value ~= '{' then
        self:error("except class body")
    end

    local block = self:pushBlock(cls, '}')
    block.objs = cls.objs
    self:doParse()
    self:popBlock()
    --TODO: 是否要校验结果
end

function parserMeta:appendQualifier(token)
    local s = self.sentance
    if token.value == '*' or token.value == '&' or token.value == "&&" then
        s.isDecl = true
    end

    table.insert(s.declarator.type.qualifier, token.value)
    if not s.isDecl then
        table.insert(s.qualifier, token.value)
    end
end

function parserMeta:setSpecifier(com)
    assert(com.value ~= "constexpr")

    local s = self.sentance
    if not s.isDecl and not s.specifier then
        s.specifier = com
        s.declarator.type.name = com.value
        if com.kind == TokenKind.kRaw then
            s.declarator.type.kind = TypeKind.kRaw
        else
            s.declarator.type.kind = TypeKind.kRefer
        end
    else
        self:error(string.format("current sentence not allow set specifier with \"\"", com.value))
    end
end

function parserMeta:appendDeclSeq(com)
    local s = self.sentance
    local kind = com.kind
    if kind == TokenKind.kMemberPtr then
        s.isDecl = true
        s.declarator.type = {
            kind = TypeKind.kMemberPtr,
            ret = s.type,
            class = com.value,
            qualifier = {},
        }
    elseif kind == TokenKind.kOperator then
        s.isDecl = true
        s.declarator.seq = com
    elseif not s.isDecl and not s.specifier then
        self:setSpecifier(com)
    elseif not s.declarator.seq then
        s.isDecl = true
        s.declarator.seq = com
    else
        lib.Log(com)
        lib.Log(s)
        self:error("")
    end
end

function parserMeta:parseBitField()
    local decl = self.sentance.declarator
    if self.domain.kind ~= ObjectType.kClass or not decl.seq then
        self:error("unexpect code \":\"")
    end

    local token = self:getToken(true)
    if token.kind == TokenKind.kConst then
        decl.bitFeild = {kind = TokenKind.kConst, value = token.value, range = token.range}
    elseif token.kind == TokenKind.kIdentify or token.value == '::' then
        decl.bitFeild = self:tryCombine(token)
    else
        self:error("unexpect code \":\"")
    end
end

-- 逗号
function parserMeta:meetComma()
    print("parserMeta:meetComma", self.block.isArgsBlock)
    self:produce()

    if not self.block.isArgsBlock then   -- 参数列表声明中
        local s = self.sentance
        s.declarator = {
            type = {qualifier = lib.ShallowCopy(s.qualifier),},
            attr = {},
        }

        if s.specifier then
            s.declarator.type.name = s.specifier.value
            if s.specifier.kind == TokenKind.kRaw then
                s.declarator.type.kind = TypeKind.kRaw
            else
                s.declarator.type.kind = TypeKind.kRefer
            end
        end
    else
        self.sentance = makeSentance()
    end
end

-- 遇到分号
function parserMeta:meetSemicolon()
    self:produce()
    self.sentance = makeSentance()
end

-- 开启花括号
function parserMeta:openBrace()
    local domainKind = self.domain.kind
    local decl = self.sentance.declarator

    do
        if not self.lexer:JumpCtrl('}') then
            self:error("")
        end
        if decl.kind == ObjectType.kFunction then
            self:produce()
            self:resetSentance()
        end
        return
    end
    --TODO:
    if (domain.kind == ObjectType.kGlobal or domain.kind == ObjectType.kClass) and 
        domain.temporary.type and domain.temporary.identify then
        self:rollback(token)
        self:parseAssign()    -- 变量赋初始值
    else
        if domain.kind == ObjectType.kGlobal or domain.kind == ObjectType.kFunction then
            if not self.lexer:JumpCtrl('}') then
                error("")
            end
        end
        if domain.kind == ObjectType.kFunction then
            self:popDomain()
            self:addFunction(domain)
        end
    end
end

-- 关闭花括号
function parserMeta:closeBrace()
    local kind = self.domain.kind
    if kind ~= ObjectType.kNamespace and
        kind ~= ObjectType.kClass then
            print("1111111111", kind)
        self:error("unknown brace closed")
    end

    --TODO: 这里可以检查是否非正常结束
    --self:resetSentance()
    --self.domain.temporary = nil
    --self:popDomain()
end

function parserMeta:parseBracket()
    local sen = makeSentance()
end

function parserMeta:tryParseFuncAttr()
    local attr = {}
    local type
    while true do
        local token = self:getToken(true)
        local value = token.value
        if value == "const" then
            attr.isConst = true
        elseif value == "noexcept" then
            attr.isNoexcept = true
        elseif value == "->" then
            --TODO: 
            --type = {}
            --self:tryCombine()
            self:tryParseType()
        elseif value == '&' or value == "&&" then
            print("what is this")
        elseif value == "override" or value == "final" then
            attr.isVirtual = true
        elseif value == '=' then
            local next = self:getToken(true)
            if next.value == "delete" then
                attr.isDelete = true
            elseif next.value == "default" then
                attr.isDefault = true
            else
                self:error("unexpect token here")
            end
        else
            self:rollback(token)
            break
        end
    end

    return attr, type
end

function parserMeta:tryParseDeclFuncAttr(attr)
    while true do
        local token = self:getToken(true)
        local value = token.value
        if value == "const" then
            attr.isConst = true
        elseif value == "noexcept" then
            attr.isNoexcept = true
        else
            self:rollback(token)
            break
        end
    end
end

function parserMeta:tryParseTailType()
    if self:peekToken().vlaue ~= "->" then
        return
    end

    self:getToken()
    return self:tryParseType()
end

function parserMeta:tryParseCV(q)
    q = q or {}
    while true do
        local token = self:getToken(true)
        local value = token.value
        if value == "const" or value == "volatile" or value == "mutable" then
            table.insert(q, value)
        else
            self:rollback(token)
            break
        end
    end
    return q
end

function parserMeta:tryParseQualifier(q)
    q = q or {}
    while true do
        local token = self:getToken(true)
        local value = token.value
        if value == "const" or value == "volatile" or value == "mutable" or
            value == "*" or value == "&" or value == "&&" then
            table.insert(q, value)
        else
            self:rollback(token)
            break
        end
    end
    return q
end

function parserMeta:tryParseArgs(isDecl)
    local cursor = self.lexer.cursor
    --print("parserMeta:tryParseArgs", string.sub(self.lexer.source, cursor))
    local block = self:pushBlock(self.domain, ')')
    block.isArgsBlock = true
    block.isDecling = isDecl
    local ok, err = self:safeParse()
    self:popBlock()
    if not ok then
        print("parse args error", err)
        self.lexer:SetCursor(cursor)
        return
    end

    --TODO: 解析参数, 检查参数列表
    --lib.Log(block.objs)
    --print("parserMeta:tryParseArgs end")
    return block.objs, {}
end

-- 检查是否有效声明ID，用来区分(S)是否函数声明
function parserMeta:isValidDeclID(s)
    return true
end

-- 识别定义的函数变量的参数修饰等
-- 如：auto (*f)(int a, int b) noexcept -> int
-- 的：[(int a, int b) noexcept -> int]这块内容
function parserMeta:parseDeclFunc(decl, modifier)
    if modifier.args then
        self:error("unexpect \"()\"")
    end

    modifier.args = self:tryParseArgs()
    if not modifier.args then
        self:error("parse declare function arguments failed")
    end

    self:tryParseDeclFuncAttr(decl.attr)
    local tailType = self:tryParseTailType()

    if modifier.kind == TypeKind.kMemberPtr then
        modifier.kind = TypeKind.kMemberFuncPtr
    else
        modifier.kind = TypeKind.kFunction
        modifier.ret = {qualifier = {}}
    end

    return modifier.ret, tailType
end

--[[ 尝试解析括号
    括号可能是函数、变量声明
]]
function parserMeta:tryParseDeclImpl(deep)
    --print("parserMeta:tryParseDeclImpl begin")
    if self:peekToken().value == ')' then
        return
    end

    local decl
    local modifier
    local tailRet

    local cv = self:tryParseCV()     -- drop invalid cv
    local qualifier = self:tryParseQualifier()

    local token = self:getToken(true)
    --[[if token.value == '*' or token.value == '&' or token.value == '&&' then
        table.insert(qualifier, token.value)
        self:tryParseQualifier(qualifier)

        local next = self:getToken(true)
        if next.value == '(' then
            decl, modifier = self:tryParseDeclImpl(deep + 1)
            if not decl then
                self:error("")
                return
            end
        else
            decl = {type = {qualifier = {}}}
            modifier = decl.type
            if next.kind == TokenKind.kIdentify then
                decl.seq = {kind = TokenKind.kIdentify, value = next.value, range = next.range}
            else
                decl.seq = {kind = TokenKind.kIdentify, value = "", range = next.range}
                self:rollback(next)
            end
        end
    else]]if token.value == '(' then
        decl, modifier = self:tryParseDeclImpl(deep + 1)
        if not decl then
            self:error("")  -- 无法识别内层括号
            return
        end
    elseif token.value == "::" or token.kind == TokenKind.kIdentify then
        local combine = self:tryCombine(token)
        if not combine.isMemberPtr and not self:isValidDeclID(combine.value) then
            self:error("")
            return
        end

        if combine.kind == TokenKind.kMemberPtr then
            self:tryParseQualifier(qualifier)
            local next = self:getToken(true)
            --print("zzzzzzz", combine.value)
            if next.value == '(' then
                decl, modifier = self:tryParseDeclImpl(deep + 1)
                --print("cccccccc", string.sub(self.lexer.source, self.lexer.cursor))
                if not decl then
                    self:error("")
                    return
                end

                modifier.kind = TypeKind.kMemberPtr
                modifier.class = combine.value
                modifier.ret = {qualifier = {}}
            else
                decl = {
                    type = {
                        kind = TypeKind.kMemberPtr,
                        class = combine.value,
                        qualifier = {},
                        ret = {qualifier = {}}
                    }
                }
                modifier = decl.type
                if next.kind == TokenKind.kIdentify then
                    decl.seq = {kind = TokenKind.kIdentify, value = next.value, range = next.range}
                    --decl.id = next.value
                    --decl.range = next.range
                else
                    decl.seq = {kind = TokenKind.kIdentify, value = "", range = next.range}
                    --decl.id = ""
                    --decl.range = next.range
                    self:rollback(next)
                end
            end
        else
            decl = {
                seq = combine,
                type = {qualifier = {}}
            }
            modifier = decl.type
        end
    else
        --print("xxxxxxxccccccccccccccc 1",  "-" .. token.value..'-')
        return
    end

    local next = self:getToken(true)
    if next.value == '(' then
        modifier, tailRet = self:parseDeclFunc(decl, modifier)
        if tailRet then
            self:error("this place not declare tail return type")
        end

        next = self:getToken(true)
    end

    if next.value ~= ')' then
        self:error("expect a \")\"")
        return
    end

    mergeQualifier(modifier.qualifier, qualifier)

    -- 函数与成员指针需要区分类型修饰符的位置（变量、返回值）
    if self:peekToken().value == '(' then
        self:getToken()
        modifier, tailRet = self:parseDeclFunc(decl, modifier)
        if tailRet then
            self:error("this place not declare tail return type")
        end
    end

    --print("xxxxxxxxxxxxxxxx", string.sub(self.lexer.source, self.lexer.cursor))
    --lib.Log(decl)
    return decl, modifier, tailRet
end

function parserMeta:tryParseDecl()
    local cursor = self.lexer.cursor
    local decl, modifier = self:tryParseDeclImpl(1)
    if not decl then
        print("111111111 unknown xxxxxxxx")
        self.lexer:SetCursor(cursor)
        return
    end
    return decl, modifier
    --[[
    local s = self.sentance
    local type = s.declarator.type
    mergeQualifier(type.qualifier, modifier.qualifier)
    lib.ShallowCopy(type, modifier)

    -- 设置语句
    s.declarator.kind = ObjectType.kVariate
    s.declarator.type = decl.type
    s.declarator.seq = decl.seq
    --print("parserMeta:tryParseDecl()")
    --lib.Log(decl)
    return true
    ]]
end

-- 判断是否构造函数实例
-- eg. TypeName::TypeName(...)
function parserMeta:isConstructImpl(seq)
    local t = splitRefer(seq.value)
    local n = #t
    return t[n] and t[n] == t[n-1]
end

function parserMeta:isMemberImpl(seq)
    local p1 = string.find(seq.value, "::")
    local p2 = string.find(seq.value, "operator")
    if p1 then
        if p2 then return p2 > p1
        else return true end
    else
        return false
    end
end

function parserMeta:skipBracketBody()
    local c = self.lexer:JumpCtrl(')')
    if not c then
        self:error("")
    end
end

function parserMeta:skipFunctionBody()
    local token = self:getToken(true)
    -- 跳过初始化列表
    if token.value == ':' then
        while true do
            local c = self:tryCombine()
            if not c or (c.kind ~= TokenKind.kIdentify and c.kind ~= TokenKind.kRefer) then
                self:error("unexpect token")
            end

            local symbol = self:getToken(true).value
            local closeSymbol
            if symbol == '(' then
                closeSymbol = ')'
            elseif symbol == '{' then
                closeSymbol = '}'
            else
                self:error("unexpect symbol")
            end

            if not self.lexer:JumpCtrl(closeSymbol) then
                self:error("")
            end

            token = self:getToken()
            if token.value == '{' then
                break
            elseif token.value == ',' then
                self:error("unexpect")
            end
        end
    end

    -- 跳过函数体
    if token.value == '{' then      -- 函数体
        if not self.lexer:JumpCtrl('}') then
            self:error("")
        end
    elseif token.value ~= ';' then  -- 函数声明
        self:error("unexpect token")
    end
end

function parserMeta:checkIsValueImpl()
    local qualifier = self:tryParseCV()
    if #qualifier > 0 then
        return false
    end

    local t = self:getToken()
    if t.value == "::" or t.kind == TokenKind.kIdentify then
        local com = self:tryCombine(t)
        if not com then
            return true
        end
        if com.kind ~= TokenKind.kIdentify then
            return false
        end

        self:tryParseQualifier(qualifier)
        if #qualifier > 0 then
            return false
        end

        local iden = self:tryCombine()
        if iden then
            return false
        end

        local symbol = self:peekToken().value
        if symbol ~= ',' and symbol ~= ')' then
            return true
        end

        local obj = self.domain:Lookup(com.value)
        return obj and obj.kind == ObjectType.kVariate
    elseif t.kind == TokenKind.kKeyword then
        if t.value == "sizeof" then
            return true
        else
            return false
        end
    elseif t.value == "..." or t.value == ')' then
        return false
    else
        return true
    end
end

function parserMeta:checkIsValue()
    local cursor = self.lexer.cursor
    local isValue = self:checkIsValueImpl()
    self.lexer:SetCursor(cursor)
    return isValue
end

-- 打开圆括号
function parserMeta:openBracket(token)
    --print("parserMeta:openBracket", string.sub(self.lexer.source, self.lexer.cursor))
    local s = self.sentance
    local decl = s.declarator
    if not s.specifier and not decl.seq then
        if not self.lexer:JumpCtrl(')') then
            self:error("")
        else
            self:warning("unknown \"()\"")
        end

        self.sentance = makeSentance()
        return
    end

    --TODO: 函数声明或变量构造？ struct Obj g_Obj(1); // 声明全局变量
    if decl.seq then
        if self:isMemberImpl(decl.seq) then
            -- 跳过函数实现
            self:skipBracketBody()
            self:tryParseFuncAttr()
            self:skipFunctionBody()
            self:resetSentance()
            return
        end

        -- 检查参数是否变量用于区分是否是使用括号初始化
        local args = (not self:checkIsValue()) and self:tryParseArgs()
        if not args then
            print("vvvvvvvvvvvvvv")
            decl.kind = ObjectType.kVariate
            self.lexer:JumpCtrl(')')
            return
        end

        decl.kind = ObjectType.kFunction
        decl.type = {
            kind = TypeKind.kFunction,
            qualifier = {},
            ret = decl.type,
            args = args
        }
        self:tryParseFuncAttr()
        return
    end

    if s.specifier.value == "BaseObj" then
        print("domain.kind", self.domain.kind, self.domain.id)
    end

    -- 构造函数
    if self.domain.kind == ObjectType.kClass and self.domain.id == s.specifier.value then
        decl.attr.isConstruct = true
        decl.kind = ObjectType.kFunction
        decl.seq = s.specifier
        decl.type = nil
        decl.args = self:tryParseArgs()
        if not decl.args then
            self:error("can not parse function arguments")
        end

        local attr = self:tryParseFuncAttr()
        lib.ShallowCopy(attr, decl.attr)
        
        self:skipFunctionBody()
        self:produce()
        self:resetSentance()
        print("xxxxxxxxxccccccccccccccccc")
        return
    end

    -- 跳过构造函数实现
    if self:isConstructImpl(s.specifier) then
        self:skipBracketBody()
        self:tryParseFuncAttr()
        self:skipFunctionBody()
        self:resetSentance()
        return
    end

    local d, m = self:tryParseDecl()
    -- 声明变量
    if d then
        mergeQualifier(decl.type.qualifier, m.qualifier)
        lib.ShallowCopy(decl.type, m)
        
        -- 设置语句
        decl.kind = ObjectType.kVariate
        decl.type = d.type
        decl.seq = d.seq

        --TODO: 处理函数属性
        --self:tryParseFuncAttr()
        self:tryParseDeclFuncAttr({})
        return
    end

    -- 参数表、函数类型
    if self.block.isArgsBlock or s.isUsing then
        local args = self:tryParseArgs()
        if args then
            decl.kind = ObjectType.kFunction
            decl.seq = {kind = TokenKind.kIdentify, value = "", range = token.range}
            decl.type = {
                ret = decl.type,
                qualifier = {},
                args = args,
            }
            self:tryParseFuncAttr()
            return
        end
    else
        if not self.lexer:JumpCtrl(')') then
            self:error("")
        else
            self:warning("unknown \"()\"")
        end

        self:resetSentance()
        return
    end

    self:error("can not parse \"()\"")
end

-- 关闭圆括号
function parserMeta:closeBracket()
    self:produce()
end

-- 不支持对函数声明操作 [= 0, = delete, = default]
function parserMeta:parseAssign()
    local cursor = self.lexer.cursor
    local c = self.lexer:JumpCtrl(',', ';', ')')
    if not c then
        self:error("expect some ctrl code")
    else
        assert(self.lexer.cursor - cursor < 100)
        self.lexer:MoveCursor(-1)
        local assign = string.sub(self.lexer.source, cursor, self.lexer.cursor - 1)
        self.sentance.declarator.assign = lib.Trim(assign)
        print("assign", self.sentance.declarator.assign)
    end
end

function parserMeta:parseArray()
    local cursor = self.lexer.cursor
    local c = self.lexer:JumpCtrl(']')
    if not c then
        self:error("expect ']'")
    else
        local type = self.sentance.declarator.type
        local len = string.sub(self.lexer.source, cursor, self.lexer.cursor - 2)
        len = lib.Trim(len)
        if not type.array then
            type.array = {len}
        else
            table.insert(type.array, len)
        end
    end
end

-- 当前语句产生的对象类型
function parserMeta:produce()
    --print("parserMeta:produce", string.sub(self.lexer.source, self.lexer.cursor))
    local s = self.sentance
    local decl = s.declarator
    local block = self.block

    if not s.specifier or not decl.seq then
        return  -- empty statement
    elseif not decl.seq then
        if s.isStruct then
            return
        elseif not block.isArgsBlock then
            --print("xxxxxxxxxxxxxxxxxxxxxxxxxx")
            --lib.Log(decl)
            self:error("declarator need a name")
        end

        table.insert(block.objs, {
            kind = ObjectType.kVariate,
            id = "",
            loc = self.lexer:Location(),
            type = decl.type,
            attr = lib.ShallowCopy(s.attr, decl.attr),
        })
    elseif s.isTypedefing then
        if not decl.seq or decl.seq.kind ~= TokenKind.kIdentify or decl.seq.value == "" then
            self:error("typedef need a identify name")
        end
        if decl.kind and decl.kind ~= ObjectType.kVariate and decl.kind ~= ObjectType.kFunction then
            self:error("typedef unexpect syntax")
        end

        table.insert(block.objs, {
            kind = ObjectType.kAlias,
            id = decl.seq.value,
            loc = self.lexer:Location(decl.seq.range),
            type = decl.type,
            attr = lib.ShallowCopy(s.attr, decl.attr),
        })
    else
        if self:isMemberImpl(decl.seq) then
            return -- 成员函数实例化、成员静态变量初始化
        end

        table.insert(block.objs, {
            kind = decl.kind or ObjectType.kVariate,
            id = decl.seq.value,
            loc = self.lexer:Location(decl.seq.range),
            type = decl.type,
            attr = lib.ShallowCopy(s.attr, decl.attr),
        })
    end
end

--local parser = CreateParser()
--[=[
print("1")
parser:ParseSource([[struct const typename Type::template InnerType<int>::type value;]])
print("2")
parser:ParseSource([[decltype(v)::type value;]])
print("3")
parser:ParseSource([[const char* value;]])
print("4")
parser:ParseSource([[   ]])
parser:ParseSource([[ :: std::vector< int   >:: type]])
]=]

local source = [[
    struct a final : decltype(s)::type {

    };

    sssssssssssssss(1, 2)

    namespace b {
        struct Obj {
            int a[10];
            int b = 0;
            int c{1};
        };
    }

    enum xxxfadga1;
    enum xxxfadga2 : int {

    };
    enum xxxfadga3 {

    };

    void test1(int a, int b, ...) {}
    auto test2() -> int {}
    auto test3(int c = 1) = delete {}
]]

--print(string.sub("1234567", 2, 4))

--parser:ParseSource(source)
--parser:setSource(source)
--parser:getToken()
--parser:parseStruct()

--parser:ParseSource([[*foo_6)(void) ]])
--parser:ParseSource([[(*(foo_6))(void)) ]])
--parser:ParseSource[[*(*(* const foo_4)(void))) ]]    -- 这里为什么需要一个空格？

--parser:ParseSource([[BaseObj::* const ((BaseObj::* const p_4)())) ]])
--parser:ParseSource([[BaseObj::* (*p_get_mem_ptr_4)()) ]])
--parser:ParseSource([[BaseObj::* (*p_get_mem_ptr_5))() ]])

--parser:ParseSource [[typedef int int_t, *intp_t, *(&fp)(int, ulong), arr_t[10]; ]]


--[=[
local s = lpeg.match(c_split_refer, "/*xx*/::A::/*vv??*/B < int > :: c")
print(s)
lib.Log(s)

s = lpeg.match(c_split_refer, "A::B<int>::c")
--lib.Log(s)

s = lpeg.match(c_split_refer, " A ::") -- has some problem
lib.Log(s)


local c_line = P{
    Cs((V"exp" - (P"//" + '\n'))^0) * (p_comment + P'\n' + -P(1)),
    exp = (p_multi_line_comment + S" \t" + P'\\\n')^1/' ' + c_char + c_string + C(P(1)),
}

local s = [[
    a '\n' "zouhui" "\\n" \
    "d" x/*multi line comment*/ //comment \
    b
    c
    d
]]

print(lpeg.match(c_line, s))
print(lpeg.match(c_line, [[1]]))
print(lpeg.match(c_line, [[]]))

local s1 = [[//xuantao]]

print(lpeg.match(p_comment, s1))
]=]

--print(lpeg.match(c_numeric, "4"))

local source_func_decl = [[
    int *p_int = nullptr, BaseObj::*p_md_int = nullptr, (BaseObj::* p_mf_int)() = nullptr;
    int (&p_int_2) = a;
    int(*BaseObj::*(*p_get_mem_ptr_1)) = nullptr;
    int(*BaseObj::*(*p_get_mem_ptr_2)()) = nullptr;

    //(int ((BaseObj::*)())) (*p_get_mem_ptr_3)();
    //int(void) (*int_value_1)() = nullptr;
    //int(*(BaseObj::*)(p_get_mem_ptr_3)()) = nullptr;

    int BaseObj::* (*p_get_mem_ptr_3)() = nullptr;
    int (BaseObj::* (*p_get_mem_ptr_4)()) = nullptr;
    int (BaseObj::* (*p_get_mem_ptr_5))() = nullptr;
]]

local p = CreateParser()
p:ParseSource(source_func_decl)
lib.Log(p.domain)


return {
    CreateParser = CreateParser,
}
