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
local p_comment = P'//' * (1 - P'\n')^0 * (P'\n' + -P(1))                   -- 单行注释
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
    "num",
    num = C(V"float" + (V"bin" + V"oct" + V"dec" + V"hex") * V"intSuf"),
    bin = P"0" * S"bB" * S"01"^1 * -S"29",
    oct = P"0" * p_digit^0 * -S"bBxX",
    dec = S"19" * p_digit^0,
    hex = P'0' * S'xX' * R('09', 'af', "AF")^1,
    float = (p_digit^1 * P'.' * p_digit^0 + P'.' * p_digit^1) * (S'eE' * p_sign * p_digit^1)^-1 *(S"lL" + S"fF")^-1,
    intSuf = (S"uU" * S"lL" + S"lL" * S"uU" + S"uU" + S"lL")^-1,
}
-- 字符串常量提取器
local c_string = P{
    "main",
    main = Ct((V"string" * (p_empty^0 * V"string")^0)^1) / table.concat,
    string = V"shortStr" + V"longStr",
    shortStr = P'"' * Cs((P'\\"' + (P(1) - S'"\n'))^0) * P'"',
    longStr = V"open" * Cs((P(1) - V"closeEq")^0) * V"close" / 1,
    open = P'R"' * Cg(Cs((P(1) - P'(')^0), "init") * P'(',
    close = P')' * Cs((P(1) - P'"')^0) * P'"',
    closeEq = Cmt(V"close" * Cb("init"), function (s, i, a, b) return a == b end),
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
    assert(M(c_numeric, "1") == "1")
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
    return function (s) return {type = type, value = s} end
end

--local kWeyWords

local TokenType = {
    kSymbol = 1,  -- '#' '\\\n'
    kKeyword = 2,
    kConst = 3,
    kIdentifier = 4,
}

local SkipType = {
    kAttr = 1,      -- [[attr]]
    kRound = 2,     -- (...)
    kSquare = 3,    -- [...]
    kAngle = 4,     -- <...>
    kBrace = 5,     -- {...}
}

local TypeKind = {
    kRaw = 1,       -- 原生
    kRef = 2,       -- 引用
    kAnonymous = 3, -- 匿名
}

local c_round_bracket = buildSkipPatt('(', ')')
local c_angle_bracket = P{
    Ct(V"exp") / table.concat,
    exp = C(P'<') * (V"content" + (C(P(1)) - P'>'))^0 * C(P'>'),
    content = p_empty + c_round_bracket + c_char + c_string + V"special" + V"exp",
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
    identity = C(p_idsafe * (p_idsafe + p_digit)^0) / buildToken(TokenType.kIdentifier),
    keywords = C((
        P"class" + "struct" + "enum" + "template" + "virtual" +
        "typename" + "decltype" + "final" + "const" + "constexpr" + "volatile" + "mutable" +
        "namespace" + "auto" + "operator" + "sizeof"
        ) * -p_rest) / buildToken(TokenType.kKeyword),
    symbols = C(P"..." + "&&" + "||" + "->" + "+=" + "-=" + "|=" + "&=" + "/=" + "*=" + "==" + ">=" + "<="
        + "::" + "<<" + ">>" + ':' + '#'
        + '!' + '<' + '>' + "." + '(' + ')' + '[' + ']'
        + '{' + '}' + '\\' + ',' + ';' + '+' + '-' + '*' + '/' + '&' + '~'
        + '=') / buildToken(TokenType.kSymbol),
    const = (c_numeric + c_string) / buildToken(TokenType.kConst),
}

-- skip empty text
local function adjustLexerCursor(lexer, p)
    p = lpeg.match(p_empty_list, lexer.source, p) or p
    lexer.cursor = p
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
    --print("lexer", t.type, t.value, calcline(self.source, self.cursor))

    adjustLexerCursor(self, p)
    return t
end

-- 一组token，如std::vector<int>可合并成一组
--[[
function lexerMeta:TokenGroup()
    local s, p = lpeg.match(c_token_group, self.source, self.cursor.pos)
    if not s then
        return
    end

    local tokens = {}
    local prevP, prevL, prevC = self.cursor.pos, self.cursor.line, self.column
    while true do
        local t = self:Token()
        if not t or t.endCur.pos > p then
            self.cursor.pos, self.cursor.line, self.column = prevP, prevL, prevC
            return
        end

        table.insert(tokens, t)
        adjustLexerCursor(self, t.endCur.pos)
        if t.endCur.pos == p then
            break
        end
    end

    return {
        value = s,
        tokens = tokens,
        startCur = tokens[1].startCur,
        endCur = tokens[#tokens].endCur
    }
end

function lexerMeta:Rollback(token)
    self.cursor = token.startCur.pos
end
]]

function lexerMeta:Capture(type)
    local patt = c_skip_patts[type]
    if not patt then return end

    local s, p = lpeg.match(patt, self.source, self.cursor)
    if not s then return end

    local token = {
        type = TokenType.kNone,
        value = s,
        range = {self.cursor, p},
    }
    self.cursor = p
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

local text = [[
class KBaseObject
{
public:
    KBaseObject();
    virtual ~KBaseObject();

    DWORD m_dwID;

public:
    DECLARE_LUA_CLASS(KBaseObject)
    DECLARE_LUA_DWORD_READONLY(ID)
};
]]


--[[
local parser = CreateLexer(text)
while true do
    local s = parser:GetToken()
    if not s then
        break
    end
end
]]
local classParser = {}

-- 测试括弧
local p =  P{
    Ct(V"exp") / table.concat,
    exp = C(P'<') * (V"content" + (C(P(1)) - P'>'))^0 * C(P'>'),
    content = p_empty + c_round_bracket + c_char + c_string + C(V"special") + V"exp",
    special = P">=" + P"<=" + P">>" + P"<<",
}

--print(lpeg.match(c_angle_bracket, [[<x, "111",(1>2),(1<2),y /*xxx*/  z,1>>2,2<<3,4>=4,5<=6>]]))

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

local DomainType = {
    kGlobal = 1,
    kNamespace = 2,
    kClass = 3,
    kFunction = 4,  -- 此处特指函数声明表达式，函数体将会被忽律
}

local AccessType = {
    kPublic = 1,
    kProcteced = 2,
    kPrivate = 3,
}

local ObjectType = {
    kFile = 1,
    kNamespace = 2,
    kClass = 3,
    kEnum = 4,
    kVariate = 5,
    kFunction = 6,
    kOverload = 7,
}

local SentanceState = {
    kSpecifierSeq = 1,  -- 声明
    kDeclcrator = 2,    -- 
    kSurffix = 3,       -- 后续扩展
}

-- 名称组合类型
local CombinedKind = {
    kRaw = 1,               -- 原生类型(包含void、auto)
    kIdentify = 2,          -- 名字
    kRefer = 3,             -- 引用类型
    kMemberPtr = 4,         -- 成员指针
    kOperator = 5,          -- 重载
    kOperatorSymbol = 5,    -- 重载操作符
    kOperatorTypeCast = 6,  -- 重载类型转换
    kOperatorLiteral = 7,   -- 重载字面量拼接
    kOperatorNewDelete = 8,
}

local OperatorKind = {
    kSymbol = 1,        -- 符号
    kTypeCast = 2,      -- 类型转换
    kLiteral = 3,       -- 字符串连接
    kNewDelete = 4,     -- new/delete 操作
}

-- 代码块类型，表示当前处理的token所在范围
local BlockKind = {
    kBracket = 1,   -- 圆括号
    kBrace = 2,     -- 花括号
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

local env = {
    domain = { type = 1, parent = nil, children = {}, enums = {}, vars = {}, funcs = {}, classes = {}, typedefs = {} },
    domainMap = {},
    marks = { isStatic = false, qualifiers = {}, },
}


local function createMark()
    return {
        tags = {},          -- 标记
        qualifier = {},     -- 修饰符
    }
end

local function createDomain(name, type, access)
    return {
        name = name,
        type = type,
        category = type,
        access = access or AccessType.kPublic,
        parent = nil,
        children = {},  -- namesapce, class
        enums = {},
        vars = {},
        funcs = {},
        classes = {},
        typedefs = {},
        tags = {},      -- 标记
        qualifier = {},
        temporary = createMark(),
    }
end

local function makeSentance()
    return {
        --state = SentanceState.kSpecifierSeq,
        --specifier = {info = nil, qualifiers = {}},
        attr = {},
        declarator = {qualifier = {}},
        specifier,
        --declarator,
        qualifier = {},
    }
end

local function makeBlock(domain, closeSymbol)
    return {
        domain = domain, 
        closeSymbol = closeSymbol,
        sentance = {
            state = SentanceState.kSpecifierSeq,
            specifierSeq = {id ="", tokens = {}},
            declarator = {id = "", tokens = {}, qualifiers = {}},
        }
    }
end

local function isSpecifierSeqEmpty(seq)
    return seq.id == "" and
        not seq.isConst and
        not seq.isConstExpr and
        not seq.isVoliate and
        not seq.isMutable
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
            mark = createMark(),    -- 局部解析标记
            domain = createDomain("", DomainType.kGlobal, AccessType.kPublic),
            sentance = makeSentance(),
            block = {kind = 1, closeSymbol = ""},
            stack = {},
        }, {
            __index = parserMeta
        }
    )
    return parser
end

function parserMeta:Parse(file)
    --self.lexer = CreateLexer(source)
end

function parserMeta:ParseSource(source)
    self.lexer = CreateLexer(source)
    --self:doParse()
    self.sentance = makeSentance()
    --self.sentance.specifier = {}
    local decl = {typeQualifier = {}, qualifier = {}}
    local ok = self:tryParseDecl2(decl)
    print(source, ok)
    lib.Log(decl)
end

function parserMeta:setSource(source)
    self.lexer = CreateLexer(source)
end

function parserMeta:resetMark()
    self.domain.temporary = createMark()
    --self.mark = createMark()
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
        if not self.lexer.JumpCtrl(')') then
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

end

function parserMeta:error(desc)
    print("at:" .. string.sub(self.lexer.source, self.lexer.cursor, self.lexer.cursor + 20))
    error(desc)
end

function parserMeta:doParse()
    while true do
        local token = self:getToken()
        if not token then
            if self.block.closeSymbol ~= "" then
                self:error("unexpect end")
            end
            break
        elseif token.value == self.block.closeSymbol then
            break
        end

        local value = token.value
        if token.type == TokenType.kKeyword then
            if value == "public" or value == "proctected" or value == "private" then
                if self.domain.type ~= DomainType.kClass then
                    self:error("only class/struct accept it")
                end
                self:expect(':')
                self.domain.access = toAccessType(value)
            elseif value == "const" or value == "volatile" or value == "mutable" then
                self:appendQualifier(token)
            elseif value == "using" then
                --TODO:
            elseif value == "typename" then
                self:setSpecifier(self:tryCombine())
            elseif value == "decltype" then
                self:setSpecifier(self:tryCombine(token))
            elseif value == "operator" then
                local com = self:tryCombine(token)
                self:appendDeclSeq(com)
            elseif value == "auto" then
                self:setSpecifier({kind = CombinedKind.kRefer, value = value, range = token.range})
            elseif value == "void" then
                self:setSpecifier({kind = CombinedKind.kRaw, value = value, range = token.range})
            elseif value == "namespace" then
                self:processNamespace()
            elseif value == "struct" or value == "class" then
                self:processClass(value == "struct")
            elseif value == "enum" then
                self:processEnum()
            elseif value == "virtual" then
                self.sentance.attr.isVirtual = true
            elseif value == "inline" or value == "override" or value == "final" then
                -- ignore
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
        elseif token.type == TokenType.kSymbol then
            if value == '{' then  -- jump out unamed scope
                local domain = self.domain
                if (domain.type == DomainType.kGlobal or domain.type == DomainType.kClass) and 
                    domain.temporary.type and domain.temporary.identify then
                    self:rollback(token)
                    self:processAssign()    -- 变量赋初始值
                else
                    if domain.type == DomainType.kGlobal or domain.type == DomainType.kFunction then
                        if not self.lexer:JumpCtrl('}') then
                            error("")
                        end
                    end
                    if domain.type == DomainType.kFunction then
                        self:popDomain()
                        self:addFunction(domain)
                    end
                end
            elseif value == '}' then
                self:closeBrace()
            elseif value == '#' then
                self:preProcess()
            elseif value == ':' then
                self:parseBitField()
            elseif value == "::" then
                self:appendDeclSeq(self:tryCombine(token))
            elseif value == ',' then
                self:meetComma()
            elseif value == ";" then
                self:meetSemicolon()
            elseif value == '(' then
                self:openBracket()
            elseif value == ')' then
                self:closeBracket()
            elseif value == '=' then
                self:processAssign()
            elseif value == '[' then
                self:processArray()
            elseif value == "[[" then
                if not self.lexer:JumpCtrl("]]") then
                    self:error("can not process attribute")
                end
            elseif value == '...' then
                if self.sentance.declarator.seq then
                    self:error("")
                end
                self.sentance.declarator.seq = {kind = CombinedKind.kIdentify, value = value, range = token.range}
            elseif value == '&&' or value == '&' or value == '*' then
                self:appendQualifier(token)
            else
                error("");
            end
        elseif token.type == TokenType.kIdentifier then
            self:appendDeclSeq(self:tryCombine(token))
        else
            self:error("unknown")
        end
    end
end

function parserMeta:safeParse()
    self:doParse()
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

function parserMeta:newDomain(name, type, access)
    if name ~= '' and self.domain.name == name or self.domain.children[name] then
        self:error("can not create same domain with name ".. name)
    end

    local domain = createDomain(name, type, access)
    self.domain.children[name] = domain

    domain.parent = self.domain
    if type == DomainType.kClass then
        domain.inherit = {}
    end
    return domain
end

function parserMeta:pushDomain(domain)
    assert(self.domain == domain.parent)
    self.domain = domain
    self.domain.temporary = {}
end

function parserMeta:popDomain()
    assert(self.domain.parent)
    self.domain = self.domain.parent
end

function parserMeta:pushBlock(domain, kind, closeSymbol)
    local backup = {
        domain = self.domain,
        sentance = self.sentance,
        block = self.block,
    }

    table.insert(self.stack, backup)
    self.domain = domain
    self.sentance = makeSentance()
    self.block = {kind = kind, closeSymbol = closeSymbol, objs = {}}
    return self.block
end

function parserMeta:popBlock()
    local pos = #self.stack
    local backup = self.stack[pos]
    table.remove(self.stack, pos)

    self.domain = backup.domain
    self.sentance = backup.sentance
    self.block = backup.block
end

local function mergeQualifier(l, r)
    --TODO: 合并const、volaite
    if not l then return r end
    if not r then return l end
    for _, v in ipairs(r) do
        table.insert(l, v)
    end
    return l
end

function parserMeta:setType(combined)
    print("parserMeta:setType", self.mark, self.mark.type, combined.id)
    --lib.Log(combined)
    --log(self.mark)
    --log(combined)
    --lib.Log(self.domain.temporary.type)
    assert(self.domain.temporary.type == nil)--, self.mark.type.name)

    if not combined then
        self.domain.temporary.type = {
            id = '', 
            qualifier = self.domain.temporary.qualifier,
        }
    else
        self.domain.temporary.type = {
            id = combined.id,
            --tokens = combined.tokens,
            qualifiers = self.domain.temporary.qualifier,
        }
    end
    self.domain.temporary.qualifier = {}
end

function parserMeta:setIdentify(combined)
    assert(self.domain.temporary.identify == nil)
    self.domain.temporary.identify = {
        id = combined.id,
        --tokens = combined.tokens,
    }
end

function parserMeta:parseInherits(defaultAccess)
    local inherits = {}
    local element = {access = defaultAccess}

    while true do
        local token = self:getToken(true)
        local value = token.value
        if value == "decltype" or value == "::" or token.type == TokenType.kIdentifier then
            local com = self:combineDecltype(token)
            if com.kind ~= CombinedKind.kIdentifier and com.kind ~= CombinedKind.kRefer then
                self:error("")
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
        kind = CombinedKind.kRefer,
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

function parserMeta:combineOperator()
    local com = {}
    local qualifier = self:tryParseQualifier()
    local token = self:getToken()
    local value = token.value
    if token.type == TokenType.kSymbol then -- 重载操作符
        com.kind = CombinedKind.kOperatorSymbol
        if value == '(' then
            local next = self:expcet(')')
            com.value = '()'
            com.range = {token.range[1], next.range[2]}
        elseif value == '[' then
            local next = self:expcet(']')
            com.value = '[]'
            com.range = {token.range[1], next.range[2]}
        else
            com.value = value
            com.range = token.range
        end
    elseif value == "\"\"" then       -- 重载字面量连接
        local next = self:getToken(true)
        if next.type ~= TokenType.kIdentifier then
            self:error()
        end

        com.kind = CombinedKind.kOperatorLiteral
        com.value = next.value
        com.range = next.range
    elseif value == "new" or value == "delete" then
        com.kind = CombinedKind.kNewDelete
        com.value = value
        com.range = token.range
        if self.peekToken(true).value == '[' then
            self:getToken()
            com.value = com.value .. ' []'
            com.range[2] = self:expect(']').range[2]
        end
    elseif token.type == TokenType.kIdentifier or value == "::" then  -- 重载类型转换
        local type = self:trycombine(token)
        if type.kind ~= CombinedKind.kIdentify or type.kind ~= CombinedKind.kRefer then
            self:error()
        end
        --TODO:
        com.qualifier = self:tryParseQualifier(qualifier)
        com.kind = CombinedKind.kOperatorTypeCast
        com.value = type.value
        com.range = type.range
    else
        self:error("")
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

    local ret = {range = {token.range[1], 0}}
    if RawTypes[token.value] then   -- 原生类型可以连接
        local next = self:getToken()
        ret.kind = CombinedKind.kRaw
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
    elseif token.type ~= TokenType.kIdentifier and token.value ~= "::" then
        self:rollback(token)
        return
    end

    if token.type == TokenType.kIdentifier then
        ret.value, ret.range[2] = self:combineIdentify(token)
        ret.kind = token.value == ret.value and CombinedKind.kIdentify or CombinedKind.kRefer
    elseif token.value == "::" then
        local next = self:getToken(true)
        if next.type ~= TokenType.kIdentifier then
            self:error("\"::\" need flow a qualified identity")
        end

        local value, pos = self:combineIdentify(next)
        ret.kind = CombinedKind.kRefer
        ret.value = ret.value .. value
        ret.range[2] = pos
    end

    token = self:getToken(true)
    if token.value ~= "::" then
        self:rollback(token)
        return ret
    end

    ret.kind = CombinedKind.kRefer
    while true do
        ret.value = ret.value .. token.value
        ret.range[2] = token.range[2]

        local next = self:getToken(true)
        if next.value == "*" then
            ret.kind = CombinedKind.kMemberPtr
            ret.value = ret.value .. next.value
            ret.range[2] = next.range[2]
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
        elseif next.type == TokenType.kIdentifier then
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

    ret.value = ret.value
    return ret
end

-- 组合名字，如将 std::vector<int>::template allocator<short>::type 合并成一组
function parserMeta:combineNames()
    local isOperator
    local hasTemplate
    local afterCombined
    local tokenList = {}
    local token = self:getToken()
    local cursor = self.lexer.cursor
    local qualifier = {}
    if not token then return end    -- at tail

    -- 读取类型修饰符
    while token.value == "const" or token.value == "constexpr" or token.value == "volatile" or token.value == "mutable" do
        table.insert(qualifier, token.value)
        token = self:getToken(true)
    end
    -- 跳过不必要关键字
    if token.value == "typename" or token.value == "class" or token.value == "struct" then
        token = self:getToken(true)
    end

    -- 检测是否接受起始token
    if token.value == "decltype" then
        table.insert(tokenList, token)
        if self:peekToken().value ~= '(' then
            self:error("11")
        end
        table.insert(tokenList, self.lexer:Capture(SkipType.kRound))
    elseif token.type == TokenType.kIdentifier then
        table.insert(tokenList, token)
    else
        self.lexer:SetCursor(cursor)    -- 不是符合要求，回退到起始位置
        print("1111111111111")
        return
    end

    -- 连结后续名字
    token = self:getToken(true)
    while token.value == "::" or token.value == '<' do
        if token.value == "::" then                 -- 类型连结
            local next = self:getToken(true)
            table.insert(tokenList, token)
            if next.value == "operator" then        -- 重载操作符
                isOperator = true
                table.insert(tokenList, next)
                next = self:getToken(true)
                if next.type == TokenType.kSymbol then
                    table.insert(tokenList, next)
                else
                    self:rollback(next)
                    afterCombined = self:combineNames()
                    if not afterCombined then
                        self:error("11")
                    end
                end
                break
            elseif next.value == "template" then    -- 内部模板类型
                hasTemplate = true
                table.insert(tokenList, next)
                next = self:getToken()
                if next.type ~= TokenType.kIdentifier then
                    self:error("need id")
                end
                table.insert(tokenList, next)
            end
        else
            if token.value == '<' then              -- 模板实例类型
                self:rollback(token)
                local next = self.lexer:Capture(SkipType.kAngle)
                table.insert(tokenList, next)
            end
        end

        token = self:getToken(true)
    end

    if not afterCombined then   -- 存在递归调用
        self:rollback(token)
    end

    lib.Log(tokenList)

    -- 构建名称
    local id = ""
    for _, node in ipairs(tokenList) do
        local value = node.value
        id = id .. value
        if value == "template" or value == "operator" then
            id = id .. ' '
        end
    end

    print("combined", id)
    if afterCombined then
        return {
            id = id .. afterCombined.id,
            hasTemplate = hasTemplate or afterCombined.hasTemplate,
            isOperator = isOperator or afterCombined.isOperator,
            qualifier = mergeQualifier(qualifier, afterCombined.qualifier),
            location = self.lexer:Location({tokenList[1].range[1], afterCombined.location.range[2]})
        }
    else
        return {
            id = id,
            qualifier = qualifier,
            hasTemplate = hasTemplate,
            isOperator = isOperator,
            location = self.lexer:Location({tokenList[1].range[1], tokenList[#tokenList].range[2]})
        }
    end
end

function parserMeta:combinedQualifier()
    local list = {}
    while true do
        local token = self:getToken()
        local k = token.value
        if k == "&&" or k == '&' or k == '*' or
            k == "const" or k == "constexpr" or k == "volatile" or k == "mutable" then
            table.insert(list, toke)
        else
            self:rollback(token)
            break
        end
    end
    return list
end

function parserMeta:processNamespace()
    local token = self:getToken()
    local id = token.value
    if token.type == TokenType.kIdentifier then
        token = self:getToken()
    else
        id = self:genAnonymousName()
    end

    if token.value ~= '{' then
        self:error("namespace expect symbol '{'")
    end

    local domain = self:getDomain(id) or self:newDomain(id, DomainType.kNamespace)
    local block = self:pushBlock(domain, 1, '}')
    self:doParse()
    self:popBlock()
end

function parserMeta:processEnum()
    local name = ''
    local token = self:getToken()
    if token.value == 'class' then
        token = self:getToken()
    end

    if token.type ==TokenType.kIdentifier then
        name = token.value
        token = self:getToken()
    end

    if token.value == ';' then
        self:resetMark()
        return
    end

    if token.value == ':' then  -- underlying type
        local combined = self:combineNames()
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

local _idx = 0
-- 生成唯一名称
function parserMeta:genAnonymousName()
    _idx = _idx + 1
    return string.froamt("_anonymous_%s_", _idx)
end

local _defaultLoc = {
    file 
}

function parserMeta:makeType(combined)
    local qualifier = self.domain.temporary.qualifier
    self.domain.temporary.qualifier = {}
    if not combined then
        return {
            id = self:genAnonymousName(),
            isAnonymous = true,
            location = self.lexer:Location(),
            qualifier = self.domain.temporary.qualifier,
        }
    else
        return {
            id = combined.id,
            location = combined.location,
            qualifier = mergeQualifier(qualifier, combined.qualifier),
        }
    end
end

function parserMeta:newClass(id, location, isStruct)
    if self.domain.category ~= DomainType.kGlobal or
        self.domain.category ~= DomainType.kNamespace or
        self.domain.category ~= DomainType.kClass then
        self:error("current domain not allow declare class/struct type", location)
    end

    local domain = self:newDomain(id, DomainType.kClass)
    domain.isStruct = isStruct
    return domain
end

function parserMeta:processClass(isStruct)
    local combined = self:combineNames()
    local type = self:makeType(combined)
    self:setType(combined)

    local symbol = self:getToken()
    local value = symbol.value
    if symbol.value ~= "final" and symbol.value ~= ':' and symbol.value ~= '{' then
        self:rollback(symbol)
        return  -- 定义变量
    end
    lib.Log(type)

    local cls = self:newDomain(type.id, DomainType.kClass)
    self:pushDomain(cls)
    if symbol.value == "final" then
        cls.isFinale = true
        symbol = self:getToken()
    end

    if symbol.value == ':' then
        cls.inherits = self:parseInherits(isStruct and AccessType.kPublic or AccessType.kPrivate)
        symbol = self:getToken()
    end

    if symbol.value ~= '{' then
        self:error("except class body")
    end

    local cls = self:newDomain(combined and combined.name or '', DomainType.kClass, AccessType.kPublic)
    local block = self:pushBlock(cls, 1, '}')
    self:popBlock()
end

function parserMeta:processVariate()
    local temporary = self.domain.temporary
    if not temporary.type or not temporary.identify then
        self:error("declare variate is not completed")
    end

    --TODO: 完善变量声明
    local var = {
        type = ObjectType.kVariate,
        name = temporary.identify.name,
        _type = temporary.type.name,
        qualifier = temporary.qualifier,
        assign = temporary.assign,
        array = temporary.array;
    }
    table.insert(self.domain.vars, var)
end

function parserMeta:setDeclSeq(seq)
    local sen = self.sentance
    if sen.state == SentanceState.kSpecifierSeq and not sen.specifierSeq.info then
        sen.specifierSeq.info = seq
    elseif sen.state == SentanceState.kDeclcrator and not sen.declarator.info then
        sen.declarator.info = seq
        sen.state = SentanceState.kSurffix
    else
        self:error()
    end
end

local function makeType(seq, qualifer)
    if not seq then
        return
    end

    local type = {name = seq.value, qualifer = qualifer}
    if seq.kind == CombinedKind.kRaw then
        type.kine = TypeKind.kRaw
    elseif seq.kind == CombinedKind.kIdentify or seq.kind == CombinedKind.kRefer then
        type.kind = TypeKind.kRefer
    else
        self:error("")
    end
    return type
end

function parserMeta:appendQualifier(token)
    local s = self.sentance
    if s.declarator then
        table.inset(s.declarator.type.qualifier, token.value)
    elseif token.value == '*' or token.value == '&' or token.value == "&&" then
        self:buildDeclarator()
        table.inset(s.declarator.type.qualifier, token.value)
    else
        table.insert(s.qualifier, token.value)
    end
end

function parserMeta:setSpecifier(com)
    local s = self.sentance
    if s.speqPos or s.declarator.seq or s.specifier then
        self:error("not allow com")
    else
        s.specifier = com
    end
end

function parserMeta:buildDeclarator()
    if self.sentance.declarator then
        self:error()
    end
    s.declarator = {type = makeType(s.specifier, s.qualifer)}
end

function parserMeta:setDeclaretor(com)

end

function parserMeta:appendDeclSeq(combine)
    -- sepcifier or declarator?
    local s = self.sentance
    local kind = combine.kind
    if kind == CombinedKind.kMemberPtr then
        if not s.declarator then
            self:buildDeclarator()
        end
        if s.declarator.seq then
            self:error("")
        end

        s.type = {kind = TypeKind.kMemberPtr, ret = s.type, class = combine.value}
    elseif kind == CombinedKind.kOperatorLiteral or
        kind == CombinedKind.kOperatorSymbol or
        kind == CombinedKind.kOperatorLiteral then
        if not s.declarator then
            self:buildDeclarator()
        end
        if s.declarator.seq then
            self:error("")
        end

        s.declarator.seq = combine
    elseif not s.declarator and not s.specifier then
        s.specifier = combine
    elseif not s.declarator then
        self:buildDeclarator()
        s.declarator.seq = combine
    elseif not s.declarator.seq then
        s.declarator.seq = combine
    else
        self:error("")
    end
end

function parserMeta:parseDeclarator()
    local token = self:getToken(true)
    local value = token.value
    if value == ':' then
    elseif value == '(' then
    elseif value == '=' then
    elseif value == '{' then
    elseif value == ',' then
    elseif value == ';' then
    end
end

function parserMeta:pushCombine(combine)
    if not combine then
        self:error()
    end

    local sen = self.sentance
    if combine.isMemberPtr then
        if sen.var then
            self:error("")
        end

        sen.var = {qualifier = {}, classRef = combine}
    else
        if not sen.var then
            if not sen.type then
                sen.type = {qualifier = {}}
            end

            if not sen.type.id then
                sen.type.id = combine
            else
                sen.var = {qualifier = {}, id = combine}
            end
        else
            sen.var.id = combine
        end
    end
end

function parserMeta:setDeclaretorState()
    local sen = self.block.sentance
    sen.state = SentanceState.kDeclcrator
end

function parserMeta:closeTemperory()
    local temporary = self.domain.temporary
    if temporary.type and temporary.identify then
        self:processVariate()
    elseif temporary.type then
        --TODO: 这里是否需要校验错误
        --self:error("")
    end
end

-- 逗号
function parserMeta:meetComma()
    local s = self.sentance
    self:produce()
    self.sentance = makeSentance()
    if self.block.kind == BlockKind.kBracket then   -- 一次声明多个变量
        self.sentance.specifier = s.specifier
        if s.speqPos then
            for i = 1, s.speqPos - 1 do
                table.insert(self.sentance.qualifier, s.qualifier[i])
            end
        else
            self.sentance.qualifier = s.qualifier
        end
    end
end

-- 遇到分号
function parserMeta:meetSemicolon()
    self:produce()
    self.sentance = makeSentance()
end

-- 关闭花括号
function parserMeta:closeBrace()
    local category = self.domain.category
    if category ~= DomainType.kNamespace and
        category ~= DomainType.kClass and
        category ~= DomainType.kFunction then
            print("1111111111", category)
        self:error("unknown brace closed")
    end

    --TODO: 这里可以检查是否非正常结束
    self:resetMark()
    self.domain.temporary = nil
    self:popDomain()
end

local function isConstructImpl(s)
    return false
end

local function isTypeName(s)
end

function parserMeta:parseBracket()
    local sen = makeSentance()
end

function parserMeta:tryParseFuncAttr()
    return {}
end

function parserMeta:tryParseQualifier(q)
    local token = self:getToken()
    q = q or {}
    while token and (token.value == "const" or token.value == "mutable" or token.value == "volatile") do
        print("222222", token.value)
        table.insert(q, token.value)
        token = self:getToken()
    end
    if token then
        self:rollback(token)
    end
    return q
end

function parserMeta:tryParseDeclAruments()
    
    --if not token or token.value ~= '(' then
    --    return
    --end
    --print(debug.traceback())

    local token = self:getToken(true)
    while token.value ~= ')' do
        print("111111111", token.value)
        token = self:getToken(true)
    end
    --self:rollback(token)
    print(string.sub(self.lexer.source, self.lexer.cursor))
    return {}


    --[[
    token = self:getToken()
    if not token then
        return
    end

    if token.value ~= '(' then
        self:rollback(token)
        return
    end

    local block = self:pushBlock(self.domain, 1, ')')
    self:doParse()
    self:popBlock()
    --TODO: 解析参数, 检查参数列表
    return block.objs, {}
    ]]
end

-- 检查是否有效声明ID，用来区分(S)是否函数声明
function parserMeta:isValidDeclID(s)
    return true
end

function parserMeta:tryParseDecl()
    local cursor = self.lexer.cursor
    local token = self:getToken(true)
    local qualifier = {}
    local decl, innerQ
    if token.value == ')' then
        decl = {id = "", qualifier = {}, range = token.range}
    elseif token.value == '*' or token.vlaue == '&' or token.value == '&&' then
        local declQual = {token.value}
        self:tryParseQualifier(declQual)

        local next = self:getToken(true)
        if next.value == '(' then
            decl, innerQ = self:tryParseDecl()
            if not decl then
                self.lexer:SetCursor(cursor)
                return
            end

            next = self:getToken(true)
        elseif next.type == TokenType.kIdentifier then
            decl = {id = next.value, qualifier = declQual, range = next.range}
            next = self:getToken(true)
        else
            
        end

        if next.value ~= ')' then
            self.lexer:SetCursor(cursor)
            return
        end

        decl = decl or {id = "", qualifier = qualifier, range = next.range}
    elseif token.value == '(' then
        decl, innerQ = self:tryParseDecl()
        if not decl then
            self.lexer:SetCursor(cursor)
            return
        end

        local next = self:getToken(true)
        if next.value ~= ')' then
            self.lexer:SetCursor(cursor)
            return
        end
    elseif value == "::" or token.type == TokenType.kIdentifier then
        local combine = self:tryCombine(token)
        if not combine.isMemberPtr or not self:isValidDeclID(combine.value) then
            self.lexer:SetCursor(cursor)
            return
        end

        local next = self:getToken(true)
        if combine.isMemberPtr then
            self:tryParseQualifier(qualifier)
            if next.value == '(' then
                decl, innerQ = self:tryParseDecl()
                decl.memberRef = combine.value
                next = self:getToken()
            elseif next.type == TokenType.kIdentifier then
                decl = {id = next.value, range = next.range, qualifier = qualifier, memberRef = combine.value}
                next = self:getToken(true)
            else
                decl = {id = next.value, range = next.range, qualifier = qualifier, memberRef = combine.value}
            end
        else
            decl = {id = combine.value, qualifier = qualifier, range = combine.range}
        end

        if next.value ~= ')' then
            self.lexer:SetCursor(cursor)
            return
        end

        qualifier = {}
    else
        self.lexer:SetCursor(cursor)
        return
    end

    if not d.isFunction and not d.memberRef then
        decl.qualifier = mergeQualifier(qualifier, d.qualifier)
        qualifier = {}
    else
        qualifier = mergeQualifier(qualifier, q)
    end
end

--[[ 尝试解析括号
    括号可能是函数、变量声明
]]
function parserMeta:tryParseDecl2(decl)
    local cursor = self.lexer.cursor
    local token = self:getToken(true)
    local qualifier = {}
    if token.value == ')' then
        decl.id = ""
        decl.range = token.range
        decl.paramenters = {}
        self:rollback(token)
    elseif token.value == '*' or token.vlaue == '&' or token.value == '&&' then
        table.insert(qualifier, token.value)
        self:tryParseQualifier(qualifier)

        local next = self:getToken(true)
        if next.value == '(' then
            if not self:tryParseDecl2(decl) then
                self:error("")
                return
            end
        elseif next.type == TokenType.kIdentifier then
            decl.id = next.value
            decl.range = next.range
        else
            decl.id = ""
            decl.range = next.range
            self:rollback(next)
        end
    elseif token.value == '(' then
        if not self:tryParseDecl2(decl) then
            self:error("")
            return
        end
    elseif value == "::" or token.type == TokenType.kIdentifier then
        local combine = self:tryCombine(token)  --TODO: trycombine 内部有问题，多读取了一个token
        if not combine.isMemberPtr and not self:isValidDeclID(combine.value) then
            self:error("")
            return
        end
        print("yyyyyy", combine.value, combine.id)
        print(string.sub(self.lexer.source, self.lexer.cursor))
        if combine.isMemberPtr then
            self:tryParseQualifier(qualifier)
            decl.memberRef = combine.value

            local next = self:getToken(true)
            if next.value == '(' then
                if not self:tryParseDecl2(decl) then
                    self:error("")
                    return
                end
            elseif next.type == TokenType.kIdentifier then
                decl.id = next.value
                decl.range = next.range
            else
                decl.id = ""
                decl.range = next.range
                self:rollback(next)
            end
        else
            decl.id = combine.value
            decl.range = combine.range
        end
    else
        return
    end

    local next = self:getToken(true)
    if next.value == '(' then
        print(string.sub(self.lexer.source, self.lexer.cursor))
        if not decl.paramenters then
            decl.paramenters = self:tryParseDeclAruments()
        else
            self:error("")
        end
        next = self:getToken(true)
    end

    if next.value ~= ')' then
        self:error("")
        return
    end

    -- 函数与成员指针需要区分类型修饰符的位置（变量、返回值）
    if decl.paramenters or decl.memberRef then
        decl.typeQualifier = mergeQualifier(qualifier, decl.typeQualifier)
    else
        decl.qualifier = mergeQualifier(qualifier, decl.qualifier)
    end

    print("xxxxxxxxxxxxxxxx", string.sub(self.lexer.source, self.lexer.cursor))
    lib.Log(decl)

    --lib.Log(qualifier)
    --print(decl.paramenters, decl.memberRef)

    if not decl.paramenters then
        next = self:getToken()
        if next then
            if next.value == '(' then
                decl.paramenters = self:tryParseDeclAruments()
            else
                self:rollback(next)
            end
        end
    end
    return true
end

function parserMeta:isConstructImpl(s)
    return false
end

-- 打开圆括号
function parserMeta:openBracket()
    local sen = self.sentance
    if not sen.specifier or not sen.specifier.seq then
        self:error("unknown \"()\"")
    end

    if not sen.declarator or not sen.declarator.seq then
        if self:isConstructImpl(sen.specifier.seq.value) then
            --TODO: skip 构造函数实现
        else
            --try
            local cursor = self.lexer.cursor
            local decl = {typeQualifier = {}, qualifier = {}}
            if self:tryParseDecl2(decl) then
                local s = self.sentance
                self:buildDeclarator()

                --TODO:
            else
                self.lexer:SetCursor(cursor)
            end

        end
    end

--[[
    local block = self:pushBlock(self.domain, 1, ')')
    self:doParse()
    self:popBlock()

    if self:peekToken().value == '(' then
        -- 函数声明
    elseif sen.isUsing or sen.isTypedefing then
        -- 类成员指针
    else
        
    end
]]
--[=[
    local domain = self.domain
    local type = domain.temporary.type
    local identify = domain.temporary.identify
    if not type then
        self:resetMark()
        print("unknown ()")
        if not self.lexer:JumpCtrl(')') then
            self:error("111111")
        end
        return  -- unkown ()
    end

    local isConstruct
    print("22222222222", identify, type)
    --lib.Log(identify)
    if not identify then
        isConstruct = domain.type == DomainType.kClass and domain.id == type.id
        identify = type
        type = nil
    end

    -- 跳过成员函数实现、未知宏调用等
    print("111111111111", identify.id, isConstruct, type)
    --lib.Log(identify)
    if #lib.Split(identify.id, "::") > 1 or (not isConstruct and not type) then
        print("skip unknown (...)")
        self:resetMark()
        if not self.lexer:JumpCtrl(')') then
            self:error("1111")
        end
        return
    end

    local func = {
        category = ObjectType.kFunction,
        id = identify.id,
        ret = type,
        args = {},
        location = identify.location,
    }

    local obj = domain.children[identify.id]
    if obj then
        domain.children[identify.id] = {
            category = ObjectType.kOverload,    -- 重载函数
            parent = obj.parent,
            overloads = {obj, func}
        }
    else
        domain.children[identify.id] = func
    end

    local temp = createDomain("", DomainType.kFunction)
    temp.parent = domain
    temp.func = func
    self:resetMark()
    self:pushDomain(temp)
]=]
end

local function isBlockEmpty(decl)
end

-- 关闭圆括号
function parserMeta:closeBracket()
    local sen = self.sentance
    -- 处理未结束的语句

    local isSpeEmpty = isBlockEmpty(sen.specifierSeq)
    local isDeclEmpty = isBlockEmpty(sen.declarator)

    -- (), (void)
    -- (*)
    -- (A::*)
    -- (int)
    -- (int*)
    -- ("literal")
    -- (value)

    if sen.constVal then
        if not isSpeEmpty or not isDeclEmpty then
            self:error("not allow")
        end
        table.insert(self.block.vals, {})
    elseif not isSpeEmpty and not isDeclEmpty then
        table.insert(self.block.vars, {kind = ObjectType.kVariate})
    elseif not isDeclEmpty then
        table.insert(self.block.vals, {})
    elseif not isSpeEmpty then
        if not sen.specifierSeq.info.id then
            self:error()
        elseif sen.specifierSeq.info.id ~= "void" then
            table.insert(self.block.vars, {kind = ObjectType.kVariate})
        end
    end

    --[=[
    local domain = self.domain
    local func = domain.func
    if domain.category ~= DomainType.kFunction then
        self:error("not a function")
    end

    -- 函数后缀
    while true do
        local value = self:getToken().value
        if value == "const" then
            func.isConst = true
        elseif value == "noexcept" then
            func.isNoExcept = true
        elseif value == "->" then   -- 后置返回类型声明
            print(string.sub(self.lexer.source, self.lexer.cursor))
            local combined = self:combineNames()
            --TODO: 整理函数数据结构
            if func.ret.id ~= "auto" or not combined then
                self:error("")
            end
            func.ret = {name = combined.value, tokens = combined.tokens}
        elseif value == '=' then
            local code = self:getToken().value
            if code == 0 then
                func.isAbstact = true
            elseif code == "delete" then
                func.isDeleted = true
            elseif code == "default" then
                func.isDefault = true
            else
                self:error("unexpect")
            end
        elseif value == ';' then
            break;
        elseif value == '{' then
            if not self.lexer:JumpCtrl('}') then
                self:error("11111")
            end
            break
        else
            self:error("unexpect")
        end
    end

    self:resetMark()
    self:popDomain()
    ]=]
end

--[[
function parserMeta:processFunction()
    local type = self.domain.temporary.type
    local identify = self.domain.temporary.identify
    if not type then
        self:resetMark()
        print("unknown ()")
        if not self.lexer:JumpCtrl(')') then
            self:error("111111")
        end
        return  -- unkown ()
    end

    local isConstruct
    if not identify then
        isConstruct = self.domain.type == DomainType.kClass and self.domain.name == self.domain.temporary.type.name
        identify = type
        type = {name = ''}
    end

    print("1111111111")
    log(type)
    print("2222222222")
    log(identify)
    print(identify.name)

    -- 跳过成员函数实现、未知宏调用等
    if #lib.Split(identify.name, "::") > 1 or (not isConstruct and type.name == '') then
        self:resetMark()
        if not self.lexer:JumpCtrl(')') then
            self:error("1111")
        end
        return
    end

    --self.mark.isFunction = true
    local func = {
        type = ObjectType.kFunction,
        name = identify.name,
        ret = type,
        identify = identify,
        isConstruct = isConstruct,
        params = {},
    }

    -- 解析参数列表
    while true do
        local combined = self:combineNames()
        local param = {}
        if combined then
            param.type = {name = combined.value, tokens = combined.tokens}
            local qualifier = self:combinedQualifier()
            local identify = self:getToken()
            local symbol
            print("identify", identify.value)
            print("combine", combined.value)
            if identify.type == TokenType.kIdentifier then
                param.name = identify.name
                symbol = self:getToken()
            else
                param.name = ''
                symbol = identify
                identity = nil
            end
            if symbol.type ~= TokenType.kSymbol then
                self:error()
            end

            local c = symbol.value
            print("xxx", c, self.lexer:Pos().pos)
            if c == '[' then
                self.lexer:JumpCtrl(']')
                c = self:getToken().value
            end
            if c == '=' then
                param.hasDefault = true
                c = self.lexer:JumpCtrl(',', ')')
            end
            print("xxx", c, self.lexer:Pos().pos)
            if c == ',' then
                table.insert(func.params, param)
            elseif c == ')' then
                table.insert(func.params, param)
                break
            else
                self:error("")
            end
        else
            local token = self:getToken()
            if token.value == "..." then
                table.insert(func.params, {name = '...'})
                self:expect(')')
                break
            elseif token.value == "void" then
                self:expect(')')
                break
            elseif token.value == ')' then
                break
            end
        end
    end

    -- 函数后缀
    while true do
        local value = self:getToken().value
        if value == "const" then
            func.isConst = true
        elseif value == "noexcept" then
            func.isNoExcept = true
        elseif value == "->" then   -- 后置返回类型声明
            local combined = self:combineNames()
            if func.ret.name ~= "auto" or not combined then
                self:error("")
            end
            func.ret = {name = combined.value, tokens = combined.tokens}
        elseif value == '=' then
            local code = self:getToken().value
            if code == 0 then
                func.isAbstact = true
            elseif code == "delete" then
                func.isDeleted = true
            elseif code == "default" then
                func.isDefault = true
            else
                self:error("unexpect")
            end
        elseif value == ';' then
            break;
        elseif value == '{' then
            if not self.lexer:JumpCtrl('}') then
                self:error("11111")
            end
            break
        else
            self:error("unexpect")
        end
    end

    self:resetMark()
    print("func", func.name)
    --lib.Log(func)
    if not func.isDeleted then
        --TODO
     end
end
]]

-- 不支持对函数声明操作 [= 0, = delete, = default]
function parserMeta:processAssign()
    local cursor = self.lexer.cursor
    local c = self.lexer:JumpCtrl(',', ';', ')')
    if not c then
        self:error("expcet some ctrl code")
    else
        assert(self.lexer.cursor - cursor < 100)
        self.lexer:MoveCursor(-1)
        local assign = string.sub(self.lexer.source, cursor, self.lexer.cursor - 1)
        self.domain.temporary.assign = lib.Trim(assign)
        print("assign", self.domain.temporary.assign)
    end
end

function parserMeta:processArray()
    local cursor = self.lexer.cursor
    local c = self.lexer:JumpCtrl(']')
    if not c then
        self:error("expect ']'")
    else
        local len = string.sub(self.lexer.source, cursor, self.lexer.cursor - 2)
        len = lib.Trim(len)
        if not self.domain.temporary.array then
            self.domain.temporary.array = {len}
        else
            table.insert(self.domain.temporary.array, len)
        end
    end
end

-- 当前语句产生的对象类型
function parserMeta:produce()
    local s = self.sentance

    if s.isUsing then

    elseif s.isTypedefing then
    end

    local decl = s.declarator
    if not decl and not s.specifier then
        return -- empty
    end
    
    if not s.declarator then
        
    end
    if s.specifier then
    elseif s.declarator then
    elseif s.constant then
    else
    end
end

local parser = CreateParser()
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
--parser:processClass()

--parser:ParseSource([[*foo_6)(void) ]])
parser:ParseSource([[(*(foo_6))(void)) ]])
--parser:ParseSource[[*(*(*const foo_4)(void))) ]]    -- 这里为什么需要一个空格？
