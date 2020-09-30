-- constant
-- enum and constant

local lib = require "lib"
local gbk = require "gbk"
local json = require "json"

local function loadXls(file)
    local text = lib.LoadFile(file)
    if not text then
        return
    end

    local ret = {}
    for l in string.gmatch(text, "(.-)\r?\n") do
        table.insert(ret, lib.Split(l, '\t'))
    end
    return ret
end

local function saveXls(file, tabs)
    if _DEBUG_XLS_ then file = "d-" .. file end
    local f = io.open(file, 'w')
    for _, t in ipairs(tabs) do
        f:write(gbk.fromutf8(table.concat(t, '\t')))
        f:write("\n")
    end
    f:close()
    print(string.format("export file:%s", file))
end

local function trimLeft(str, pat)
    local b, l = string.find(str, pat)
    return string.sub(str, b + l)
end

local function exportTipCode(tipCodeFile, tips)
    if not tips then
        return  -- not exist
    end

    local kId = 1
    local kKey = 2
    local kCode = 4
    local ret = {}
    local src = loadXls(tipCodeFile)
    if not src then
        src = {
            {"nID", "", "", "", ""},
            {"nID", "szKey", "szDesc", "nCodeVal", "nBgID"},
            {"int", "string", "string", "int", "int"},
            {"nID", "关键字", "内容", "错误码值", "背景图"},
            {"默认值", "", "", "", ""},
        }
    end

    -- copy header
    for i = 1, 5 do
        table.insert(ret, src[i])
    end

    local map = {}
    for _, v in ipairs(src) do
        local c = tonumber(v[kCode])
        if c then
            map[c] = v
        end
    end

    for i, v in ipairs(tips) do
        local t = map[v.value]
        if not t then
            t = {i, trimLeft(v.name, "TipsNotifyCode_"), lib.Trim(v.desc), v.value, "", ""}
        else
            t[kId] = i
            t[kKey] = trimLeft(v.name, "TipsNotifyCode_")
        end

        table.insert(ret, t)
    end

    saveXls(tipCodeFile, ret)
end

local function mergeErrorCode(thrift, csharp)
    local ret = {}
    local cache = {}

    for _, info in ipairs(thrift.enum) do
        if info.errorCode then
            cache[info.realName] = info
        end
    end

    for _, info in ipairs(csharp) do
        if info.errorCode then
            local te = cache[info.realName]
            if te then
                cache[info.realName] = nil
                te.name = info.name
                table.insert(ret, te)
            else
                table.insert(ret, info)
            end
        end
    end

    for _, info in ipairs(thrift.enum) do
        if info.errorCode and cache[info.realName] then
            table.insert(ret, info)
        end
    end

    return ret
end

local function exportErrorCode(errorCodeFile, thrift, csharp)
    local kId = 1
    local kMode = 2
    local kKey = 3
    local kCode = 5
    local ret = {}
    local src = loadXls(errorCodeFile)
    local err = mergeErrorCode(thrift, csharp)

    if not src then
        src = {
            {"ID",      "",         "",         "",         "",         ""},
            {"nID",     "szModule", "szKey",    "szDesc",   "nCodeVal", "nBgID"},
            {"int",     "string",   "string",   "string",   "int",      "int"},
            {"nID",     "模块名",   "关键字",    "内容",    "错误码值", "背景图"},
            {"默认值",  "",         "",         "",         "",         ""},
        }
    end

    -- copy header
    for i = 1, 5 do
        table.insert(ret, src[i])
    end

    local cache = {}
    for i = 6, #src do
        local s = src[i]
        local k = s[kMode]
        local t = cache[k]
        if not t then
            t = {}
            cache[k] = t
        end
        t[s[kKey]] = s
    end

    local idx = 1
    for _, e in ipairs(err) do
        local old = cache[e.name]
        for _, c in ipairs(e.values) do
            local s = old and old[c.name]
            if s then
                s[kId] = idx
                s[kCode] = tonumber(c.value)
                table.insert(ret, s)
            else
                table.insert(ret, {idx, e.name, c.name, lib.Trim(c.desc or ""), tonumber(c.value), 0})
            end
            idx = idx + 1
        end
    end

    saveXls(errorCodeFile, ret)
end

local function writeLuaValue(f, val, prev, comma)
    local ml
    comma = comma or ''

    if val.desc and val.desc ~= "" then
        local b, e = string.find(val.desc, '\n')
        ml = e and e > 0
        if ml then
            f:write(string.format("--[[%s]]\n", val.desc))
        end
    end

    if val.type == "string" then
        f:write(string.format("%s%s = \"%s\"", prev, val.name, val.value))
    else
        f:write(string.format("%s%s = %s", prev, val.name, val.value))
    end
    f:write(comma)

    if not ml and val.desc and val.desc ~= "" then
        f:write(string.format("    --%s", val.desc))
    end
    f:write("\n");
end

local function exportEnumFile(enumFile, thrift, csharp)
    local f = io.open(enumFile, 'wb')
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write("--- EnumDef C#、Thrift(protocol_gs, protocol_shared)枚举定义\n")
    f:write("-- @module EnumDef\n\n")

    local default = {namespace = "", const = {}, enum = {}}
    local nsMap = {[""] = default}
    local nsList = {default}
    for _, info in ipairs(thrift.enum) do
        if not info.errorCode then
            local p = lib.Split(info.realName, '%.')
            local ns
            if #p > 1 then
                ns = nsMap[p[1]]
                if not ns then
                    ns = {namespace = p[1], const = {}, enum = {}}
                    nsMap[p[1]] = ns
                    table.insert(nsList, ns)
                end
            else
                ns = default
            end

            table.insert(ns.enum, info)
        end
    end

    for _, info in ipairs(thrift.const) do
        local p = lib.Split(info.realName, '%.')
        local ns
        if #p > 1 then
            ns = nsMap[p[1]]
            if not ns then
                ns = {namespace = p[1], const = {}, enum = {}}
                nsMap[p[1]] = ns
                table.insert(nsList, ns)
            end
        else
            ns = default
        end

        table.insert(ns.const, info)
    end

    -- 前置声明名字空间
    for i, ns in ipairs(nsList) do
        if ns.namespace ~= "" then
            f:write(string.format("%s = %s or {}\n", ns.namespace, ns.namespace))
        end
    end
    if #nsList > 0 then
        f:write("\n")
    end

    -- csharp相关定义
    for _, e in ipairs(csharp) do
        if not e.errorCode then
            f:write(string.format("--- %s\n", e.desc))
            f:write(string.format("-- @table %s\n", e.name))
            f:write(string.format("%s = {\n", e.name))
            for _, l in ipairs(e.values) do
                f:write(string.format("    %s = %s,\n", l.name, l.value))
            end
            f:write("}\n")
        end
    end

    -- thrift相关定义
    if #nsList > 0 then
        if #csharp > 0 then
            f:write("\n\n")
        end
        f:write("--- thrift定义的常量、枚举\n")
    end
    for i, ns in ipairs(nsList) do
        local prevConst = ""
        local isNs = ns.namespace ~= ""

        if i > 2 then   -- 第一个为空白名字空间
            f:write('\n')
        end

        if isNs then
            prevConst = string.format("%s.", ns.namespace)
            f:write(string.format("---@namespace %s\n", ns.namespace))
        end

        -- const
        for _, val in ipairs(ns.const) do
            writeLuaValue(f, val, prevConst, "")
        end

        if #ns.const > 0  and #ns.enum > 0then
            f:write("\n")
        end

        -- enum
        for _, info in ipairs(ns.enum) do
            if info.desc and info.desc ~= "" then
                f:write(string.format("--[[%s]]\n", info.desc))
            end
            if isNs then
                f:write(string.format("%s.%s = {\n", ns.namespace, info.name))
            else
                f:write(string.format("    %s = {\n", info.name))
            end
            for _, val in ipairs(info.values) do
                writeLuaValue(f, val, "    ", ',')
            end
            f:write("}\n")
        end
    end

    f:close()
end

local function export(thriftJsonFile, csharpJsonFile,
    enumLuaFile, tipXlsFile, errorXlsFile)

    local thrift = json:decode(lib.LoadFile(thriftJsonFile) or "{}")
    local csharp = json:decode(lib.LoadFile(csharpJsonFile) or "{}")
    exportTipCode(tipXlsFile, thrift.tip)
    exportErrorCode(errorXlsFile, thrift, csharp)
    exportEnumFile(enumLuaFile, thrift, csharp)
end

return {
    export = export,
}
