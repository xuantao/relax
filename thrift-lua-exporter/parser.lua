-- thrift parser

local lib = require "lib"
local lpeg = require "lpeg"
local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C, Cb, Cf, Cg, Cp, Ct, Cmt = lpeg.C, lpeg.Cb, lpeg.Cf, lpeg.Cg, lpeg.Cp, lpeg.Ct, lpeg.Cmt

-- 基础模式
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
-- 提取器
local c_annotation = (P'//' * P'/'^0 * C((1 - P'\n')^0) * (P'\n' + P(-1)) +
    P'/*' * (P'*' - P'*/')^0 * C((1 - P'*/')^0) * P'*/')^-1/1                   -- 提取注释
local c_tag = (P'/*<@' * p_space * C(p_reference) * p_space * P'>*/')^-1/1      -- 提取标记

local function concatPath(p, f)
    if p == "" then
        return f
    else
        return string.format("%s/%s", p, f)
    end
end
local function getFilePath(f)
    local p_slash = f:find("[/\\][^/\\]*$")
    return p_slash and f:sub(1, p_slash) or ""
end
local function getFileName(f)
    local p_slash = f:find("[/\\][^/\\]*$") or 0
    local p_dot = f:find("%.") or #f + 1
    return f:sub(p_slash + 1, p_dot - 1)
end
local function loadFile(f)
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

-- thrift scanner
local path = {""}
local parseFile
local TS = P{
    (V'include' + V'typedef' + V'const' + V'enum') * Cp() + (p_comment + p_multi_line_comment + 1) * V(1),
    include = P'include' * p_empty * (P"'" * C(p_reference) * P"'" +  P'"' * C(p_reference) * P'"') /
        function (f)
            return {"include", getFileName(f), {file = f, vars = parseFile(f)}}
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

function parseFile(f)
    local fp
    if #path == 0 then
        fp = f
    else
        fp = concatPath(path[#path], f)
    end

    local t = loadFile(fp)
    if not t then
         return
    end

    table.insert(path, getFilePath(fp))
    local pos = 1
    local ret = {}
    while true do
        local e
        local last = pos
        e, pos = lpeg.match(TS, t, pos)
        if not e or not pos then
            break
        end

        table.insert(ret, e)
    end
    table.remove(path, #path)
    return ret
end

lib.Log(parseFile(arg[1]))
