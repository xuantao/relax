-- convert clang index enum to lua def
local lib = require "lib"
local lpeg = require "lpeglabel"
local parser = require "thrift-parser"
local re = require "relabel"

local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C, Cb, Cf, Cg, Cp, Ct, Cmt = lpeg.C, lpeg.Cb, lpeg.Cf, lpeg.Cg, lpeg.Cp, lpeg.Ct, lpeg.Cmt

-- 基础模式
local p_space = lpeg.S' \t\r\n'^0                                                 -- 空白字符
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
-- 提取器
local c_annotation = (P'//' * P'/'^0 * C((1 - P'\n')^0) * (P'\n' + P(-1)) +
    P'/*' * (P'*' - P'*/')^0 * C((1 - P'*/')^0) * P'*/')^-1/1                   -- 提取注释
local c_tag = (P'/*<@' * p_space * C(p_reference) * p_space * P'>*/')^-1/1      -- 提取标记
local c_may_idenenity = p_identity^-1 / 1

local function prefer(sur, pre)
    --print(sur, pre)
    return sur and sur ~= "" and sur or pre
end

local cs = P {
    V'enum' * Cp() + (p_comment + p_multi_line_comment + 1) * V(1),
    enum = P{
        V'enum',
        enum = c_annotation * p_space * P'enum' * p_space * p_empty * c_may_idenenity * p_empty * V'body' * p_empty * c_may_idenenity * p_empty * P';' /
            function (desc, id_1, v, id_2) return {"enum", prefer(id_2, id_1), {desc = desc, vars = v}} end,
        body = P'{' * p_space * V'vars' * p_empty * P'}',
        vars = Ct(V'var'^0),
        var = p_space * c_annotation * p_space * C(p_identity) * (V'value'^-1/1) * p_empty * P','^0 * S' \t'^0 * c_annotation /
            function(p_d, id, v, s_d) return {id = id, value = v, desc = prefer(s_d, p_d)} end,
        value = p_empty * P'=' * p_empty * C(p_hexadecimal + p_decimal + p_reference),
    },
}

-- 解析文本
local function parseSource(ret, text)
    if not text then
        return
    end

    local pos = 1
    while true do
        local e
        e, pos = lpeg.match(cs, text, pos)
        if not e or not pos then
            break
        end
        table.insert(ret, e)
    end
    return true
end

-- 解析文件
local function parseFile(files)
    local ret = {}
    for _, f in ipairs(files) do
        parseSource(ret, lib.LoadFile(f))
    end
    return ret
end

local function trimEnumName(name)
    local s = string.gsub(name, "^(CX)", "")
    return s
end

local function trimEnumValue(name)
    local s = string.gsub(name, "^(CX.-_)", "")
    return s
end

local function writeDesc(f, desc, tab)
    if not desc or desc == "" then
        return
    end

    tab = tab or ""
    desc = lib.Trim(desc, " \t\r\n")
    if string.find(desc, '\n') then
        f:write(tab, "--[[\n ", tab, desc, "\n", tab, "]]\n")
    else
        f:write(tab, "--[[", desc, "]]\n")
    end
end

local preCode =
[=[-- clang
-- entrace
-- CXIndex = clang.createIndex(excludeDeclarationsFromPCH, displayDiagnostics)
local clang = require "luaclang-parser"

--[[ metatables
  CXIndex = {
    CXTranslationUnit = load(astFile),
    CXTranslationUnit = parse({/* args */})
  },

  CXTranslationUnit = {
    CXCursor = cursor(),
    file, time = file(filename),
    {} = diagnostics(),
    {} = codeCompleteAt(filename, line, col),
  },

  CXCursor = {
    {CXCursor} = children(),
    CXCursorKind = kind(),
    string = name(),
    string = displayName(),
    CXCursor = parent(),
    {} = arguments(),
    CXType = type(),
    CX_CXXAccessSpecifier = access(),
    file, line_b, col_b, line_e, col_e = location(),
    string = usr(),
    CXCursor = referenced(),
    CXCursor = definition(),
    boolean = isStatic(),
    boolean = isVirtual(),
    CXType = resultType(),
  },

  CXType = {
    string = name(),
    CXTypeKind = kind(),
    CXType = canonical(),
    CXType = pointee(),
    boolean = isPod(),
    boolean = isConst(),
    CXCursor = declaration(),
  }
]]

]=]

local function doExport()
    local ret = parseFile{"clang/CXErrorCode.h", "clang/Index.h"}
    local refs = {}

    local f = io.open("clang.lua", "w")
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write(preCode)
    for _, enum in ipairs(ret) do
        local name = enum[2]
        local data = enum[3]

        writeDesc(f, data.desc)
        f:write("clang.", trimEnumName(name), " = {\n")

        local prev = 0
        for _, var in ipairs(data.vars) do
            writeDesc(f, var.desc, "  ")

            local value
            if not var.value or var.value == "" then
                value = prev
            else
                value = tonumber(var.value)
                if not value then
                    value = refs[var.value]
                end
            end
            prev = value + 1
            refs[var.id] = value

            f:write("  ", trimEnumValue(var.id), " = ", value, ",\n")
        end

        f:write("}\n\n")
    end
    f:write("return clang\n")
    f:close()
end

doExport()
