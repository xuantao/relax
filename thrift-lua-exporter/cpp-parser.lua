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

local function buildPos(s, i)
    local l, c = calcline(s, i)
    return {pos = i, line = l, column = c}
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

-- 跳过花括号范围
function skipBraceBlock()
end

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
        "namespace"
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

function lexerMeta:Pos(p)
    local l, c = calcline(self.source, p or self.cursor)
    return {pos = p or self.cursor, line = l, column = c}
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

    local l, c = calcline(self.source, self.cursor)
    t.startCur = {pos = self.cursor, line = l, column = c}
    l, c = calcline(self.source, p)
    t.endCur = {pos = p, line = l, column = c}

    adjustLexerCursor(self, p)
    --print("lexer", t.type, t.value, t.startCur.line, t.startCur.column)
    return t
end

-- 一组token，如std::vector<int>可合并成一组
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

function lexerMeta:Capture(type)
    local patt = c_skip_patts[type]
    if not patt then return end

    local s, p = lpeg.match(patt, self.source, self.cursor)
    if not s then return end

    local token = {
        type = TokenType.kNone,
        value = s,
        startCur = self:Pos(self.cursor),
        endCur = self:Pos(p),
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
}

local env = {
    domain = { type = 1, parent = nil, children = {}, enums = {}, vars = {}, funcs = {}, classes = {}, typedefs = {} },
    domainMap = {},
    marks = { isStatic = false, qualifiers = {}, },
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
        access = access,
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
            domain = createDomain("", DomainType.kGlobal, AccessType.kPublic ),
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
    self:doParse()
end

function parserMeta:setSource(source)
    self.lexer = CreateLexer(source)
end

function parserMeta:resetMark()
    self.domain.temporary = createMark()
    --self.mark = createMark()
end

--function parserMeta:close

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

function parserMeta:peekToken()
    local cursor = self.lexer.cursor
    local token = self:getToken()
    self.lexer:SetCursor(cursor)
    return token
end

function parserMeta:rollback(token)
    self.lexer:SetCursor(token.startCur.pos)
end

function parserMeta:expect(code)
    local cursor = self.lexer.cursor
    local token = self:getToken()
    if not token or token.value ~= code then
        error("expect code is not exist", code)
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
    error(desc)
end

function parserMeta:doParse()
    while true do
        local token = self:getToken()
        local value = token.value
        if token.type == TokenType.kKeyword then
            if value == "public" or value == "proctected" or value == "private" then
                if self.domain.type ~= DomainType.kClass then
                    self:error("only class/struct accept it")
                end
                self:expect(':')
                self.domain.access = toAccessType(value)
            elseif value == "const" or value == "volatile" or value == "mutable" then
                table.insert(self.domain.temporary.qualifier, value)
            elseif value == "using" then
            elseif value == "typename" then
                self:setType(self:combinedType())
            elseif value == "auto" then
                --TODO:
                self.domain.temporary.isAuto = true
            elseif value == "decltype" then
                self:rollback(token)
                self:setType(self:combinedType())
            elseif value == "namespace" then
                self:processNamespace()
            elseif value == "struct" or value == "class" then
                self:processClass(value == "class")
            elseif value == "enum" then
                self:processEnum()
            elseif value == "virtual" then
                self.domain.temporary.isVirtual = true
            elseif value == "operator" then
                --TODO:
            elseif value == "inline" or value == "override" or value == "final" then
                -- ignore
            elseif value == "friend" or value == "template" or value == "static_cast" then
                local c = self.lexer:JumpCtrl(';', '{')
                if not c then
                    self:error("xxxxxxxxxxx")
                elseif c == '{' then
                    if not self.lexer:JumpCtrl('}') then
                        self:error("yyyyyyyyyy")
                    end
                end
            else
                error("")
            end
        elseif token.type == TokenType.kSymbol then
            if token.value == '{' then  -- jump out unamed scope
                local domain = self.domain
                if (domain.type == DomainType.kGlobal or domain.type == DomainType.kClass) and 
                    domain.temporary.type and domain.temporary.identify then
                    self:rollback(token)
                    self:processAssign()    -- 变量赋初始值
                else
                    if domain.type == DomainType.kGlobal or domain.type == DomainType.kFunction then
                        if not lexer:JumpCtrl('}') then
                            error("")
                        end
                    end
                    if domain.type == DomainType.kFunction then
                        self:popDomain()
                        self:addFunction(domain)
                    end
                end
            elseif token.value == '}' then
                --assert(self.domain.type ~= DomainType.kGlobal)
                --self:popDomain();
                break   -- 作用域结束
            elseif token.value == '#' then
                self:preProcess()
            elseif token.value == "::" then
                self:rollback(token)
                self:setType(self:combinedType())
            elseif token.value == ',' then
                self:processVariate()
            elseif token.value == ";" then
                --TODO:
                self:closeTemperory()
                self:resetMark()
            elseif token.value == '(' then
                self:processFunction()
            elseif token.value == '=' then
                self:processAssign()
            elseif token.value == '[' then
                self:processArray()
            elseif token.value == "[[" then
                if not self.lexer:JumpCtrl("]]") then
                    self:error("can not process attribute")
                end
            elseif value == '&&' or value == '&' or value == '*' then
                table.insert(self.domain.temporary.qualifier, value)
            else
                error("");
            end
        elseif token.type == TokenType.kIdentifier then
            if self.tags[token.value] then
                self:processTag(token)
            else
                self:rollback(token)
                local combined = self:combinedType()
                if not self.domain.temporary.type then
                    self:setType(combined)
                elseif not self.domain.temporary.identify then
                    self:setIdentify(combined)
                else
                    --print("self.mark", self.mark.type.name, self.mark.identify.name)
                    self:error("unexpect identify " .. combined.value)
                end
            end
        else
            self:error("unknown")
        end
    end
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
end

function parserMeta:popDomain()
    assert(self.domain.parent)
    self.domain = self.domain.parent
end

local function mergetQualifier(l, r)
    return {}
end

function parserMeta:makeType(combined)
    if type(combined) == "string" then
        return {
            name = combined,
            
        }
    else
    end
end

function parserMeta:setType(combined)
    --print("parserMeta:setType", self.mark, self.mark.type)
    --log(self.mark)
    --log(combined)
    assert(self.domain.temporary.type == nil)--, self.mark.type.name)

    if not combined then
        self.domain.temporary.type = {
            name = '', 
            qualifier = self.domain.temporary.qualifier,
        }
    else
        self.domain.temporary.type = {
            name = combined.value,
            tokens = combined.tokens,
            qualifiers = self.domain.temporary.qualifier,
        }
    end
    self.domain.temporary.qualifier = {}
end

function parserMeta:setIdentify(combined)
    assert(self.domain.temporary.identify == nil)
    self.domain.temporary.identify = {
        name = combined.value,
        tokens = combined.tokens,
    }
end

function parserMeta:processInherit()
    assert(self.domain.type == DomainType.kClass)
    local element = {access = self.domain.access}
    while true do
        local token = self:getToken(true)
        local value = token.value
        if token.type == TokenType.kKeyword then
            if value == "virtual" then
                element.virtual = true
            elseif value == "decltype" then
                self:rollback(token)
                self:combinedType()
            elseif value == "public" or value == "protected" or value == "private" then
                element.access = toAccessType(value)
                element.access = AccessType.kPrivate
            elseif value ~= "typename" and value ~= "struct" and value ~= "class" then
                self:error("unexpect keyword in class inherit list")
            end
        elseif token.type == TokenType.kIdentifier then
            self:rollback(token)
            self:combinedType()
        elseif token.type == TokenType.kSymbol then
            if token.value == '::' then
                self:rollback(token)
                self:combinedType()
            elseif token.value == ',' then
                table.insert(self.domain.inherit, element)
                element = {access = AccessType.kPrivate}
            elseif token.value == '{' then
                table.insert(self.domain.inherit, element)
                break
            else
                self:error("unexpect symbol")
            end
        end
    end
end

function parserMeta:combinedType()
    local combined = {}
    local token = self:getToken()
    local cursor = token and token.startCur.pos or self.lexer.cursor
    while token do
        local value = token.value
        if token.type == TokenType.kKeyword then
            if value == "decltype" then
                table.insert(combined, token)
                local first = self:expect('(')
                self:rollback(first)
                local detail = self.lexer:Capture(SkipType.kRound)
                table.insert(combined, detail)
            elseif value == "const" or value == "constexpr" or value == "volatile" or value == "mutable" then
                table.insert(self.domain.temporary.qualifier, value)
            elseif value == "typename" or value == "class" or value == "struct" then
                -- ignore
            else
                self.lexer:SetCursor(cursor)
                return -- not except token
            end
            token = self:getToken(true)
        else
            break
        end
    end

    if not token then
        return
    end

    if token.type == TokenType.kIdentifier then
        table.insert(combined, token)
        token = self:peekToken()
    else
        self:rollback(token)
    end

    while token and (token.value == "::" or token.value == '<') do
        if token.value == "::" then
            self.lexer:SetCursor(token.endCur.pos)
            local next = self:getToken(true)
            table.insert(combined, token)
            table.insert(combined, next)
            if next.value == "template" then
                next = self:getToken()
                if next.type ~= TokenType.kIdentifier then
                    self:error("need id")
                end
                table.insert(combined, next)
            end
        else
            if token.value == '<' then
                local next = self.lexer:Capture(SkipType.kAngle)
                table.insert(combined, next)
            end
        end

        token = self:peekToken()
    end

    if #combined == 0 then return end

    local value = ""
    for _, t in ipairs(combined) do
        value = value .. t.value
        if t.value == "template" then
            value = value .. ' '
        end
    end

    print("combined", value)
    return {
        value = value,
        tokens = combined,
    }
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
    local name = ''
    local token = self:getToken()
    if token.type == TokenType.kIdentifier then
        name = token.value
        token = self:getToken()
    end

    if token.value ~= '{' then
        self:error("namespace need symbol '{'")
    end

    local domain = self:getDomain(name)
    if not domain then
        domain = self:newDomain(name, DomainType.kNamespace, AccessType.kPublic)
    end

    self:pushDomain(domain)
    self:doParse()
    self:popDomain()
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
        local combined = self:combinedType()
        print("ccc", combined.value)

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

function parserMeta:processClass(isClass)
    local combined = self:combinedType()
    local token = self:getToken()
    if token.value == "final" then
        -- market
        token = self:getToken()
    end

    self:setType(combined)

    if token.value == ':' or token.value == '{' then
        if combined and #combined.tokens ~= 1 then
            self:error("struct identifier is error " .. combined.value)
        end

        local cls = self:newDomain(combined and combined.name or '', DomainType.kClass, AccessType.kPublic)
        self:pushDomain(cls)
        if token.value == ':' then
            print("process inherit")
            self:processInherit()   -- 处理继承列表
        end
        print("process body")
        self:doParse()              -- 解析类体
        self:popDomain()
    elseif token.value == ';' then
        self:resetMark()            -- 前置声明
    else
        self:rollback(token)        -- 定义变量
    end
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

function parserMeta:closeTemperory()
    local temporary = self.domain.temporary
    if temporary.type and temporary.identify then
        self:processVariate()
    elseif temporary.type then
        --TODO: 这里是否需要校验错误
        --self:error("")
    end
end

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
        local combined = self:combinedType()
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
            local combined = self:combinedType()
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

parser:ParseSource(source)
--parser:setSource(source)
--parser:getToken()
--parser:processClass()
