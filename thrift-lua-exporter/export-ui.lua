-- export-ui
-- 将C#、thrift定义的枚举文件导出到UI配置文件
-- 相关中间有: thrift.json, csharp.json
-- 导出目标有: EnumDef.lua, UIErrorCodeTab.xls, UITipsNotifyCodeTab.xls
local lib = require("lib")
local gbk = require("gbk")
local json, objdef = require("json")
local processData

local function concatNs(ns, name)
    if not ns or ns == "" then
        return name
    end
    return string.format("%s.%s", ns, name)
end

local function getType(type, id)
    local path = lib.Split(id, ".")
    for _, p in ipairs(path) do
        type = type[p]
    end
    return type
end

local function getRealType(env, name)
    local r = env.type[name]
    if not r then
        return name
    end
    return getRealType(env, r)
end

local function convValue(type, val)
    assert(type.tag == "builtin")
    if type.name == "byte" or
        type.name == "i16" or
        type.name == "i32" or
        type.name == "i64" or
        type.name == "double" then
        return tonumber(val)
    elseif type.name == "string" then
        return val
    end
    return val
end

local function newEnv(g, name)
    local scope = {}
    g.type[name] = scope
    return {
        name = name,
        namespace = "",
        type = setmetatable({}, {
        __index = g.type,
        __newindex = function(t, k, v)
            rawset(t, k, v)
            scope[k] = v
        end,})
    }
end

local function onInclude(g, item)
    local prev = g.env
    g.env = newEnv(g, item[2])
    processData(g, item[3].vars)
    g.env = prev
end

local function onNamespace(g, item)
    if item[3] == "csharp" then
        g.env.namespace = item[2]
    end
end

local function onTypedef(g, item)
    g.env.type[item[2]] = getType(g.env.type, item[3].type)
end

local function onConst(g, item)
    local c = item[3]
    local t = getType(g.env.type, c.type)
    local d = {
        name = item[2],
        namespace = g.env.namespace,
        realName = concatNs(g.env.namespace, item[2]),
        value = convValue(t, c.value),
        type = t,
        desc = c.desc
    }

    if c.type == "TipsNotifyCode" then
        table.insert(g.exp.tip, d)
    else
        table.insert(g.exp.const, d)
    end
end

local function onEnum(g, item)
    local lastVal = 0
    local e = item[3]
    local d = {
        tag = "enum",
        name = item[2],
        namespace = g.env.namespace,
        realName = concatNs(g.env.namespace, item[2]),
        values = {},
        desc = e.d,
        errorCode = (e.tag == "ErrorCode"),
    }

    for _, v in ipairs(e.vars) do
        if v.value ~= "" then
            lastVal = tonumber(v.value)
        else
            lastVal = lastVal + 1
        end

        table.insert(d.values, {name=v.id, value=lastVal, desc = v.desc})
    end

    g.env.type[d.name] = d
    table.insert(g.exp.enum, d)
end

local function normalizeId(id)
    return string.format("%s%s", string.upper(string.sub(id, 1, 1)), string.sub(id, 2))
end

local normaliszeType
function normaliszeType(env, type)
    if type[1] == "normal" then
        --print(env.type, type[2], type[1])
        type[2] = getType(env.type, type[2])
--[[        local t = getType(env.type, type[2])
        if t.tag == "builtin" then
            type[2] = t.name
        else
            type[2] = concatNs(t.namespace, t.namespace)
        end
]]
    elseif type[1] == "list" then
        normaliszeType(env, type[2])
    elseif type[1] == "map" then
        normaliszeType(env, type[2])
        normaliszeType(env, type[3])
    end
end

local function onStruct(g, item)
    local v = item[3]
    local d = {
        tag = "struct",
        name = item[2],
        namespace = g.env.namespace,
        realName = concatNs(g.env.namespace, item[2]),
        member = v.member,
        toLua = (string.lower(v.tag) == "tolua"),
    }

    for _, m in ipairs(v.member) do
        m.id = normalizeId(m.id)
        normaliszeType(g.env, m.type)
    end

    g.env.type[d.name] = d
    table.insert(g.exp.struct, d)
end

local procs = {
    include = onInclude,
    namespace = onNamespace,
    typedef = onTypedef,
    const = onConst,
    enum = onEnum,
    struct = onStruct,
}

function processData(g, data)
    for _, item in ipairs(data) do
        local p = procs[item[1]]
        if p then
            p(g, item)
        end
    end
end

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
            {"ID", "", "", "", ""},
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
            t = {i, trimLeft(v.name, "TipsNotifyCode_"), v.desc, v.value, "", ""}
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
        local t = cache[s[kMode]]
        if not t then
            t = {}
            cache[s[kMode]] = t
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

local function writeLuaValue(f, val, tab, comma)
    local ml
    comma = comma or ''

    if val.desc and val.desc ~= "" then
        local b, e = string.find(val.desc, '\n')
        ml = e and e > 0
        if ml then
            f:write(string.format("%s--[[%s]]\n", tab, val.desc))
        end
    end

    if val.type == "string" then
        f:write(string.format("%s%s = \"%s\"", tab, val.name, val.value))
    else
        f:write(string.format("%s%s = %s", tab, val.name, val.value))
    end
    f:write(comma)

    if not ml and val.desc and val.desc ~= "" then
        f:write(string.format("    --%s", val.desc))
    end
    f:write("\n");
end

local function exportEnumFile(enumFile, thrift, csharp)
    local f = io.open(enumFile, 'w')
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write("--- EnumDef C#枚举定义\n")
    f:write("-- @module EnumDef\n\n")
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
    local default = {namespace = "", const = {}, enum = {}}
    local nsMap = {[""] = default}
    local nsList = {default}
    for _, info in ipairs(thrift.enum) do
        if not info.errorCode then
            local p = lib.Split(info.realName, '.')
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
        local p = lib.Split(info.realName, '.')
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

    if #nsList > 0 then
        if #csharp > 0 then
            f:write("\n\n")
        end
        f:write("--- thrift定义的常量、枚举\n")
    end
    for i, ns in ipairs(nsList) do
        local tab = ""
        local tab2 = "    "
        local nsComma = ""
        local isNs = ns.namespace ~= ""

        if i > 2 then   -- 第一个为空白明明空间
            f:write('\n')
        end

        if isNs then
            tab = "    "
            tab2 = "        "
            nsComma = ','
            f:write(string.format("---@namespace %s\n", ns.namespace))
            f:write(string.format("%s = {\n", ns.namespace))
        end

        -- const
        for _, val in ipairs(ns.const) do
            writeLuaValue(f, val, tab, nsComma)
        end

        if #ns.const > 0  and #ns.enum > 0then
            f:write("\n")
        end

        -- enum
        for _, info in ipairs(ns.enum) do
            if info.desc and info.desc ~= "" then
                f:write(string.format("%s--[[%s]]\n", tab, info.desc))
            end
            f:write(string.format("%s%s = {\n", tab, info.name))
            for _, val in ipairs(info.values) do
                writeLuaValue(f, val, tab2, ',')
            end
            f:write(string.format("%s}%s\n", tab, nsComma))
        end

        if ns.namespace ~= "" then
            f:write("}\n")
        end
    end

    f:close()
end

local getInnerType, scanDependence, scanMembeType
local function getInnerType(type)
    if type[1] == "normal" then
        return type[2]
    elseif type[1] == "list" then
        return getInnerType(type[2])
    elseif type[1] == "map" then
        return getInnerType(type[3])
    end
end

function scanMembeType(g, c, t)
    if t[1] == "normal" then
        local type = t[2]
        local real = concatNs(type.namespace, type.name)
        if type.tag == "struct" and not c.ref[real] then
            c.ref[real] = true
            table.insert(c.toLua, type)
            scanDependence(g, c, type)
        end
    elseif t[1] == "list" then
        scanMembeType(g, c, t[2])
        local ref = getInnerType(t[2])
        local real = concatNs(ref.namespace, ref.name)
        if c.list[real] then
            table.insert(c.list[real], t)
        else
            c.list[real] = {t}
        end
    elseif t[1] == "map" then
        scanMembeType(g, c, t[2])
        scanMembeType(g, c, t[3])
        local ref = getInnerType(t[3])
        local real = concatNs(ref.namespace, ref.name)
        if c.map[real] then
            table.insert(c.map[real], t)
        else
            c.map[real] = {t}
        end
    end
end

function scanDependence(g, c, s)
    for _, m in ipairs(s.member) do
        scanMembeType(g, c, m.type)
    end
end

local builtin2Csharp = {
    bool = "bool",
    byte = "sbyte",
    i16 = "short",
    i32 = "int",
    i64 = "long",
    double = "double",
    string = "string",
}

local genTypeName
function genTypeName(type)
    --lib.Log(type)
    if type[1] == "normal" then
        local ty = type[2]
        if ty.tag == "builtin" then
            return builtin2Csharp[ty.name]
        else
            return ty.realName
        end
    elseif type[1] == "list" then
        return string.format("List<%s>", genTypeName(type[2]))
    elseif type[1] == "map" then
        return string.format("Dictionary<%s, %s>", genTypeName(type[2]), genTypeName(type[3]))
    end
end

local function exportStructList(f, list)
    if not list then
        return
    end

    for _, ty in ipairs(list) do
        local type = ty[2]
        local realName = genTypeName(ty)
        f:write(string.format("Slua.LuaTable ToLua(%s list) {\n", realName))
        f:write("  var ret = new Slua.LuaTable(Lua.Instance.luaState);\n")
        f:write("  int idx = 0;\n")
        f:write("  foreach (var val in list)\n")
        if type.tag == "builtin" or type.tag == "enum" then
            f:write("    ret[++idx] = val;\n")
        else
            f:write("    ret[++idx] = ToLua(val);\n")
        end
        f:write("  return ret;\n")
        f:write("}\n\n")

        f:write(string.format("void FromLua(out %s ret, Slua.LuaTable tab, bool opt == false) {\n", realName))
        f:write("  if (tab == null || !tab.isTable) {\n")
        f:write("    if (opt) ret = nil;\n")
        f:write(string.format("    else ret = new %s();\n", realName))
        f:write("    return;\n")
        f:write("  }\n\n")
        f:write(string.format("  ret = new %s();\n", realName))
        --TODO:
        f:write("}\n\n")
    end
end

local function exportStructMap(f, map)
    if not map then
        return
    end

    for _, ty in ipairs(map) do
        local realName = genTypeName(ty)
        local val = ty[3]
        f:write(string.format("Slua.LuaTable ToLua(%s dic) {\n", realName))
        f:write("var ret = new Slua.LuaTable(Lua.Instance.luaState);\n")
        f:write("  foreach (var pair in dic)\n")
        if val.tag == "builtin" or val.tag == "enum" then
            f:write("    ret[pair.key] = pair.value;\n")
        else
            f:write("    ret[pair.key] = ToLua(val);\n")
        end
        f:write("  return ret;\n")
        f:write("}\n\n")

        f:write(string.format("void FromLua(out %s ret, Slua.LuaTable tab, bool opt == false) {\n", realName))
        f:write("  if (tab == null || !tab.isTable) {\n")
        f:write("    if (opt) ret = nil;\n")
        f:write(string.format("    else ret = new %s();\n", realName))
        f:write("    return;\n")
        f:write("  }\n\n")
        f:write(string.format("  ret = new %s();\n", realName))
        --TODO:
        f:write("}\n\n")
    end
end

local function exportStruct(f, c, s)
    local hasOpt
    for _, m in ipairs(s.member) do
        if m.opt == "optional" then
            hasOpt = true
            break
        end
    end

    f:write(string.format("Slua.LuaTable ToLua(%s obj) {\n", s.realName))
    f:write("  var ret = new Slua.LuaTable(Lua.Instance.luaState);\n")
    for _, m in ipairs(s.member) do
        local ty = m.type[2]
        if ty.tag == "builtin" or ty.tag == "enum" then
            f:write(string.format("  ret[\"%s\"] = obj.%s;\n", m.id, m.id))
        else
            f:write(string.format("  ret[\"%s\"] = ToLua(obj.%s);\n", m.id, m.id))
        end
    end
    f:write("}\n\n")

    f:write(string.format("void FromLua(out %s ret, Slua.LuaTable tab, bool opt = false) {\n", s.realName))
    f:write("  if (tab == null || !tab.isTable) {\n")
    f:write("    if (opt) ret = nil;\n")
    f:write(string.format("    else ret = new %s();\n", s.realName))
    f:write("    return;\n")
    f:write("  }\n\n")
    if hasOpt then
        f:write("  Slua.LuaVar temp;\n")
    end
    f:write(string.format("  ret = new %s();\n", s.realName))
    for _, m in ipairs(s.member) do
        local ty = m.type[2]
        if ty.tag == "builtin" then
            local t = builtin2Csharp[ty.name]
            assert(t)
            if m.opt == "optional" then
                f:write(string.format("  temp = tab[\"%s\"];", m.id))
                f:write(string.format(" if (temp != null) ret.%s = (%s)temp;\n", m.id, t))
            else
                f:write(string.format("  ret.%s = (%s)tab[\"%s\"];\n", m.id, t, m.id))
            end
        elseif ty.tag == "enum" then
            if m.opt == "optional" then
                f:write(string.format("  temp = tab[\"%s\"];", m.id))
                f:write(string.format(" if (temp != null) ret.%s = (%s)temp;\n", m.id, ty.realName))
            else
                f:write(string.format("  ret.%s = (%s)tab[\"%s\"];\n", m.id, ty.realName, m.id))
            end
        else
            f:write(string.format("  FromLua(out ret.%s, tab[\"%s\"]", m.id, m.id))
            if m.opt == "optional" then
                f:write(", true")
            end
            f:write(");\n")
        end
    end
    f:write("}\n\n")

    exportStructList(f, c.list[s.realName])
    exportStructMap(f, c.map[s.realName])
end

local function exportToLuaStruct(g)
    local cache = {toLua = {}, ref = {}, sort = {}, list = {}, map = {}}
    for i, s in ipairs(g.exp.struct) do
        cache.sort[concatNs(s.namespace, s.name)] = i
        if s.toLua then
            table.insert(cache.toLua, s)
            scanDependence(g, cache, s)
        end
    end

    table.sort(cache.toLua, function (l, r)
        return cache.sort[concatNs(l.namespace, l.name)] < cache.sort[concatNs(r.namespace, r.name)]
    end)

    local f = io.open("thrift.cs", 'w')
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write("/* generate by export tool\n * do not manual modify this file\n*/\n\n")

    for _, s in ipairs(cache.toLua) do
        print(s.namespace, s.name)
        exportStruct(f, cache, s)
    end

    f:write("\n")
    f:close()
end

local function builtin(...)
    local ret = {}
    for _, t in ipairs({...}) do
        ret[t] = {tag = "builtin", name = t}
    end
    return ret
end

-- 解析thrift并导出成json
-- srcThrift: 源协议文件
-- destJson:  目标Json文件
local function parseThrift(srcThrift, destJson)
    local ret = require("thrift-parser").ParseFile(srcThrift)
    if not ret then
        print(string.format("parse file failed, file:%s", srcThrift))
        return
    end

    local g = {
        type = setmetatable({}, {__index = builtin("bool", "byte", "i16", "i32", "i64", "double", "string", "binary")}),
        exp = {const = {}, enum = {}, tip = {}, struct = {}}
    }
    g.env = newEnv(g, lib.GetFileName(srcThrift))
    processData(g, ret)

    exportToLuaStruct(g)

    --[[
    local etc ={}
    local str = json:encode({const=g.exp.const, enum=g.exp.enum, tip=g.exp.tip},
        etc, {pretty = true, indent = "  ", align_keys = false})
    local f = io.open(destJson, 'w')
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write(str)
    f:close()
    ]]
    --lib.Log(g.exp.struct)
end

-- 将中间文件导出到目标文件
-- cfg = {
--    thriftFile = "thrift.json",
--    csharpFile = "csharp.json",
--    enumDefFile = "EnumDef.lua",
--    errorCodeFile = "UIErrorCodeTab.xls",
--    tipCodeFile = "UITipsNotifyCodeTab.xls",
-- }
local function export(cfg)
    do return end
    local thrift = json:decode(lib.LoadFile(cfg.thriftFile) or "{}")
    local csharp = json:decode(lib.LoadFile(cfg.csharpFile) or "{}")

    exportTipCode(cfg.tipCodeFile, thrift.tip)
    exportErrorCode(cfg.errorCodeFile, thrift, csharp)
    exportEnumFile(cfg.enumDefFile, thrift, csharp)
end

return {
    ParseThrift = parseThrift,
    Export = export,
}
