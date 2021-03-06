﻿-- thrift parser
-- 解析thrift中定义的部分内容(const, enum等)
local lib = require "lib"
local lpeg = require "lpeglabel"
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
local c_reference = Ct(C(p_identity) * (p_empty * P'.' * p_empty * C(p_identity))^0) /
    function (ps) return table.concat(ps, '.') end
local c_tag = (P'/*<@' * p_space * c_reference * p_space * P'>*/')^-1/1      -- 提取标记
local c_type = P{
    V'list' + V'map' + V'normal',
    normal = c_reference / function(ref) return {"normal", ref} end,
    list = P'list' * p_space * P'<' * p_space * V(1) * p_space * P'>' /
        function(ref) return {"list", ref} end,
    map = P'map' * p_space * P'<' * p_space * V(1) * p_space * P',' * p_space * V(1) * p_space * P'>' /
        function(key, val) return {"map", key, val} end,
}

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

local function prefer(sur, pre)
    if sur and sur ~= "" then
        return sur
    end
    if type(pre) == "table" then
        if #pre > 0 then
            return pre[#pre]
        else
            return ""
        end
    end
    return pre
end

-- thrift scanner
local path = {}
local parseFile
local ts = P{
    (V'include' + V'typedef' + V'const' + V'enum' + V'namespace' + V'struct' + V'service') * Cp() + (p_comment + p_multi_line_comment + 1) * V(1),

    include = P'include' * p_empty * (P"'" * c_reference * P"'" +  P'"' * c_reference * P'"') /
        function (f) return {"include", getFileName(f), {file = f, vars = parseFile(f)}} end,

    namespace = P'namespace' * p_empty * C(p_identity) * p_empty * C(p_identity) / function (lan, id)
            return {"namespace", id, lan}
        end,

    typedef = c_annotation * p_space * P'typedef' * p_empty * c_reference * p_empty * C(p_identity) *
        S',;'^-1 * S' \t'^0 * c_annotation / function (pre_desc, type, id, suf_desc)
            return {"typedef", id, {type = type, desc = prefer(suf_desc, pre_desc)}}
        end,

    const = c_annotation * p_space * P'const' * p_empty * c_reference * p_empty * C(p_identity) * p_space * P'=' *
        p_space* C(p_decimal + p_hexadecimal + p_reference) * S',;'^-1 * S' \t'^0 * c_annotation / 
            function (pre_desc, type, id, value, suf_desc) return {"const", id, {type = type, value = value, desc = prefer(suf_desc, pre_desc)}} end,

    enum = P{
        V'enum',
        enum = c_annotation * p_space * P'enum' * p_space * c_tag * p_empty * C(p_identity) * p_empty * V'body' /
            function (desc, tag, id, v) return {"enum", id, {tag = tag, desc = desc, vars = v}} end,
        body = P'{' * p_empty * V'vars' * p_empty * P'}',
        vars = Ct(V'var'^0),
        var = p_empty * C(p_identity) * (V'value'^-1/1) * p_empty * S',;'^0 * S' \t'^0 * c_annotation /
            function(id, v, desc) return {id = id, value = v, desc = desc} end,
        value = p_empty * P'=' * p_empty * C(p_decimal + p_hexadecimal),
    },

    struct = P{
        V'struct',
        struct = P'struct' * p_space * c_tag * p_empty * C(p_identity) * p_empty * V'body' /
            function (tag, id, mems) return {"struct", id, {tag = tag, member = mems}} end,
        body = P'{' * p_empty * Ct(V'member'^0) * p_empty * P'}',
        member = p_decimal * p_empty * P':' * p_empty * V'opt' * p_empty * c_type * p_empty * C(p_identity) * p_space * S',;'^0 * p_empty /
            function(opt, type, id) return {opt = opt ~= "" and opt or "required", type = type, id = id} end,
        opt = C'required' + C'optional' + C'',
    },

    service = P {
        V'service',
        service = P'service' * p_empty * C(p_identity) * p_empty * V'body' /
            function (id, mems) return {"service", id, mems} end,
        body = P'{' * Ct(V'member'^0) * p_empty * P'}',
        member = Ct((p_space * V"annotation")^0) * p_space * P'oneway' * p_empty * P'void' * p_empty * C(p_identity) * p_empty *
            P'(' * Ct(V'argument'^0) * p_empty * P')' * S',;'^0 * S' \t'^0 * c_annotation /
            function (pre_desc, id, args, suf_desc) return {id = id, args = args, desc = prefer(suf_desc, pre_desc)} end,
        argument = p_empty * C(p_decimal) * p_space * P':' * p_space * c_type * p_empty * C(p_identity) * p_space * S',;'^0 /
            function (index, type, id) return {id = id, type = type, --[[index = index]]} end,
        annotation = C(P'//' * P'/'^0 * C((1 - P'\n')^0) * (P'\n' + P(-1)) +
            P'/*' * (P'*' - P'*/')^0 * C((1 - P'*/')^0) * P'*/'),
    },
}

-- 解析文本
local function parseSource(text)
    if not text then
        return
    end

    local pos = 1
    local ret = {}
    while true do
        local e
        e, pos = lpeg.match(ts, text, pos)
        if not e or not pos then
            break
        end
        table.insert(ret, e)
    end
    return ret
end

-- 解析文件
function parseFile(file)
    local fp
    if #path == 0 then
        fp = file
    else
        fp = concatPath(path[#path], file)
    end

    table.insert(path, getFilePath(fp))
    local ret = parseSource(lib.LoadFile(fp))
    table.remove(path, #path)
    return ret
end

return {
    ParseSource = parseSource,
    ParseFile = parseFile,
}
