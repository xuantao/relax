-- export-ui
-- 将C#、thrift定义的枚举文件导出到UI配置文件
-- 相关中间有: thrift.json, csharp.json
-- 导出目标有: EnumDef.lua, UIErrorCodeTab.xls, UITipsNotifyCodeTab.xls
local lib = require("lib")
local gbk = require("gbk")
local json, objdef = require("json")
local processData
-- 引用类型
local kRefNormal    = "normal"
local kRefList      = "list"
local kRefMap       = "map"
-- 类型标签
local kTagBuiltin   = "builtin"
local kTagEnum      = "enum"
local kTagStruct    = "struct"
-- 内建类型
local kBuiltinTypes = {
    bool   = {tag = kTagBuiltin, name = "bool",   realName = "bool",   csName = "bool",   csConv = "ToBoolean"},
    byte   = {tag = kTagBuiltin, name = "byte",   realName = "byte",   csName = "sbyte",  csConv = "ToSByte"},
    i16    = {tag = kTagBuiltin, name = "i16",    realName = "i16",    csName = "short",  csConv = "ToInt16"},
    i32    = {tag = kTagBuiltin, name = "i32",    realName = "i32",    csName = "int",    csConv = "ToInt32"},
    i64    = {tag = kTagBuiltin, name = "i64",    realName = "i64",    csName = "long",   csConv = "ToInt64"},
    double = {tag = kTagBuiltin, name = "double", realName = "double", csName = "double", csConv = "ToDouble"},
    string = {tag = kTagBuiltin, name = "string", realName = "string", csName = "string", csConv = "ToString"},
    binary = {tag = kTagBuiltin, name = "binary", realName = "binary", csName = "",       csConv = ""}, -- csharp not support
}
-- 需要导出的service
local kExportService = {"tcligs", "tcligs_lua"}

local getDeepType, scanMemberDep, scanTypeRef, genCsTypeName

local function concatNs(ns, name)
    if not ns or ns == "" then
        return name
    end
    return string.format("%s.%s", ns, name)
end

local function getType(type, id)
    local path = lib.Split(id, "%.")
    if #path == 0 then
        assert(false, id)
        return type[id]
    end

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
    local num = tonumber(val)
    return num or val
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
    local name = item[2]
    if g.type[name] then
        g.env.type[name] = g.type[name]
    else
        local prev = g.env
        g.env = newEnv(g, item[2])
        processData(g, item[3].vars)
        prev.type[item[2]] = g.env.type
        g.env = prev
    end
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
        realName = concatNs(g.env.namespace, item[2]),
        value = convValue(t, c.value),
        type = t.realName,
        desc = c.desc
    }

    if c.type == "TipsNotifyCode" then
        table.insert(g.exp.tip, d)
    else
        table.insert(g.exp.const, d)
    end
end

local function onEnum(g, item)
    local lastVal = -1
    local e = item[3]
    local d = {
        tag = kTagEnum,
        name = item[2],
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
function normaliszeType(env, ref)
    if ref[1] == kRefNormal then
        if not kBuiltinTypes[ref[2]] then
            ref[2] = concatNs(env.name, ref[2])
        end
    elseif ref[1] == kRefList then
        normaliszeType(env, ref[2])
    elseif ref[1] == kRefMap then
        normaliszeType(env, ref[2])
        normaliszeType(env, ref[3])
    end
end

local function onStruct(g, item)
    local v = item[3]
    for _, m in ipairs(v.member) do
        m.id = normalizeId(m.id)
        normaliszeType(g.env, m.type)
    end

    local d = {
        tag = kTagStruct,
        name = item[2],
        envName = g.env.name,
        realName = concatNs(g.env.namespace, item[2]),
        member = v.member,
        luaTag = v.tag,
    }
    g.env.type[d.name] = d
    table.insert(g.exp.struct, d)
end

local getArgType
function getArgType(env, ref)
    if ref[1] == kRefNormal then
        if kBuiltinTypes[ref[2]] or #lib.Split(ref[2], "%.") > 1 then
            return ref[2]
        else
            return concatNs(env.name, ref[2])
        end
    elseif ref[1] == kRefList then
        return string.format("list<%s>", getArgType(env, ref[2]))
    elseif ref[1] == kRefMap then
        return string.format("map<%s, %s>", getArgType(env, ref[2]), getArgType(env, ref[3]))
    else
        assert(false, ref[1], ref[2])
    end
end

local function onService(g, item)
    if not lib.Find(kExportService, g.env.name) then
        return  -- 过滤
    end

    local member = {}
    for _, m in ipairs(item[3]) do
        local mem = {id = m.id, args = {}, desc = m.desc}
        for _, a in ipairs(m.args) do
            table.insert(mem.args, {id = a.id, type = getArgType(g.env, a.type)})
        end
        table.insert(member, mem)
    end

    local s = {
        name = item[2],
        envName = g.env.name,
        member = member
    }
    table.insert(g.exp.service, s)
end

local procs = {
    include = onInclude,
    namespace = onNamespace,
    typedef = onTypedef,
    const = onConst,
    enum = onEnum,
    struct = onStruct,
    service = onService,
}

function processData(g, data)
    for _, item in ipairs(data) do
        local p = procs[item[1]]
        if p then
            p(g, item)
        end
    end
end
-- 后处理类型引用
local function processAfter(g)
    local visit
    visit = function (type)
        if type[1] == "ref" then
            local t = getType(g.type, type[2])
            assert(t)
            type[1] = "normal"
            type[2] = t
        elseif type[1] == "list" then
            visit(type[2])
        elseif type[1] == "map" then
            visit(type[2])
            visit(type[3])
        else
            assert(type[1] == "normal" or type[1] == "builtin")
        end
    end

    for _, s in ipairs(g.exp.struct) do
        for _, m in ipairs(s.member) do
            visit(m.type)
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

-- 获取内部类型
function getDeepType(g, ref)
    if ref[1] == kRefNormal then
        return getType(g.type, ref[2])
    elseif ref[1] == kRefList then
        return getDeepType(g, ref[2])
    elseif ref[1] == kRefMap then
        return getDeepType(g, ref[3])
    else
        assert(false, ref[1], ref[2])
    end
end
-- 获取扫描类型依赖
function scanTypeRef(g, c, ref)
    if ref[1] == kRefNormal then
        local type = getType(g.type, ref[2])
        if type.tag == kTagStruct and not c.unique[type.realName] then
            c.unique[type.realName] = true
            scanMemberDep(g, c, type)
            table.insert(c.exp.struct, type)
        end
    elseif ref[1] == kRefList then
        scanTypeRef(g, c, ref[2])
        local realName = genCsTypeName(g, ref)
        local type = getDeepType(g, ref[2])
        if not c.unique[realName] then
            c.unique[realName] = true
            table.insert(c.exp.list, ref)
        end
    elseif ref[1] == kRefMap then
        scanTypeRef(g, c, ref[2])
        scanTypeRef(g, c, ref[3])
        local realName = genCsTypeName(g, ref)
        local type = getDeepType(g, ref[3])
        if not c.unique[realName] then
            c.unique[realName] = true
            table.insert(c.exp.map, ref)
        end
    end
end

function scanMemberDep(g, c, s)
    for _, m in ipairs(s.member) do
        scanTypeRef(g, c, m.type)
    end
end

function genCsTypeName(g, ref)
    if ref[1] == kRefNormal then
        local type = getType(g.type, ref[2])
        if type.tag == kTagBuiltin then
            return kBuiltinTypes[type.name].csName
        else
            return type.realName
        end
    elseif ref[1] == kRefList then
        return string.format("List<%s>", genCsTypeName(g, ref[2]))
    elseif ref[1] == kRefMap then
        return string.format("Dictionary<%s, %s>", genCsTypeName(g, ref[2]), genCsTypeName(g, ref[3]))
    end
end

local function convBuiltinValue(type)
    if type.tag == "builtin" then
        return kBuiltinTypes[type.name].csName
    elseif type.tag == "enum" then
        return string.format("(%s)%s", type.realName, kBuiltinTypes['i32'].csName)
    end
end

local function exportStructList(f, g, list)
    for _, ref in ipairs(list) do
        local realName = genCsTypeName(g, ref)
        local deepRef = ref[2]
        local deepType
        if deepRef[1] == kRefNormal then
            deepType = getType(g.type, deepRef[2])
        end
        f:write(string.format("    public static SLua.LuaTable ToLua(%s list)\n", realName))
        f:write("    {\n")
        f:write("        if (list == null)\n")
        f:write("            return null;\n\n")
        f:write("        int idx = 0;\n")
        f:write("        var ret = new SLua.LuaTable(Lua.Instance.luaState);\n")
        f:write("        foreach (var val in list)\n")
        if deepType and deepType.tag ~= kTagStruct then
            f:write("            ret[++idx] = val;\n")
        else
            f:write("            ret[++idx] = ToLua(val);\n")
        end
        f:write("        return ret;\n")
        f:write("    }\n\n")

        f:write(string.format("    public static void FromLua(out %s ret, SLua.LuaTable tab)\n", realName))
        f:write("    {\n")
        f:write(string.format("        ret = new %s();\n", realName))
        f:write("        if (tab == null)\n")
        f:write("            return;\n\n")
        f:write("        foreach (var pair in tab)\n")
        if not deepType or deepType.tag == kTagStruct then
            f:write("        {\n")
            f:write(string.format("            %s val;\n", genCsTypeName(g, deepRef)))
            f:write("            FromLua(out val, pair.value as SLua.LuaTable);\n")
            f:write("            ret.Add(val);\n")
            f:write("        }\n")
        elseif deepType.tag == kTagBuiltin then
            f:write(string.format("            ret.Add(Convert.%s(pair.value));\n", deepType.csConv))
        else
            f:write(string.format("            ret.Add((%s)Convert.ToInt32(pair.value));\n", deepType.realName))
        end
        f:write("    }\n\n")
    end
end

local function exportStructMap(f, g, map)
    for _, ref in ipairs(map) do
        local realName = genCsTypeName(g, ref)
        local keyRef = ref[2]
        local valRef = ref[3]
        local keyType, valType
        if keyRef[1] == kRefNormal then
            keyType = getType(g.type, keyRef[2])
        end
        if valRef[1] == kRefNormal then
            valType = getType(g.type, valRef[2])
        end

        f:write(string.format("    public static SLua.LuaTable ToLua(%s dic)\n", realName))
        f:write("    {\n")
        f:write("        if (dic == null)\n")
        f:write("            return null;\n\n")
        f:write("        var ret = new SLua.LuaTable(Lua.Instance.luaState);\n")
        f:write("        foreach (var pair in dic)\n")
        if not valType or valType.tag == kTagStruct then
            f:write("            ret[pair.Key] = ToLua(pair.Value);\n")
        else
            f:write("            ret[pair.Key] = pair.Value;\n")
        end
        f:write("        return ret;\n")
        f:write("    }\n\n")

        f:write(string.format("    public static void FromLua(out %s ret, SLua.LuaTable tab)\n", realName))
        f:write("    {\n")
        f:write(string.format("        ret = new %s();\n", realName))
        f:write("        if (tab == null)\n")
        f:write("            return;\n\n")
        f:write("        foreach (var pair in tab)\n")
        f:write("        {\n")
        if keyType.tag == kTagEnum then
            f:write(string.format("            var key = (%s)Convert.ToInt32(pair.key);\n", keyType.realName))
        else
            f:write(string.format("            var key = Convert.%s(pair.key);\n", keyType.csConv))
        end

        if not valType or valType.tag == kTagStruct then
            f:write(string.format("            %s val;\n", genCsTypeName(g, valRef)))
            f:write("            FromLua(out val, pair.value as SLua.LuaTable);\n")
        elseif valType.tag == kTagEnum then
            f:write(string.format("            var val = (%s)Convert.ToInt32(pair.value);\n", valType.realName))
        else
            f:write(string.format("            var val = %s(pair.value);\n", valType.csConv))
        end
        f:write("            ret.Add(key, val);\n")
        f:write("        }\n")
        f:write("    }\n\n")
    end
end

local function exportStructLuaConv2(f, g, s)
    local hasOpt
    for _, m in ipairs(s.member) do
        if m.opt == "optional" then
            hasOpt = true
            break
        end
    end

    f:write(string.format("    public static SLua.LuaTable ToLua(%s obj)\n", s.realName))
    f:write("    {\n")
    f:write("        if (obj == null)\n")
    f:write("            return null;\n\n")
    f:write("        var ret = new SLua.LuaTable(Lua.Instance.luaState);\n")
    for _, m in ipairs(s.member) do
        local type
        local ref = m.type
        if ref[1] == kRefNormal then
            type = getType(g.type, ref[2])
        end
        if not type or type.tag == kTagStruct then
            f:write(string.format("        ret[\"%s\"] = ToLua(obj.%s);\n", m.id, m.id))
        else
            f:write(string.format("        ret[\"%s\"] = obj.%s;\n", m.id, m.id))
        end
    end
    f:write("        return ret;\n")
    f:write("    }\n\n")

    f:write(string.format("    public static void FromLua(out %s ret, SLua.LuaTable tab)\n", s.realName))
    f:write("    {\n")
    if hasOpt then
        f:write("        object tmp;\n")
    end
    f:write(string.format("        ret = new %s();\n", s.realName))
    f:write("        if (tab == null)\n")
    f:write("            return;\n\n")
    for _, m in ipairs(s.member) do
        local type
        local ref = m.type
        local realName = genCsTypeName(g, ref)
        if ref[1] == kRefNormal then
            type = getType(g.type, ref[2])
        end
        if not type or type.tag == kTagStruct then
            if m.opt == "optional" then
                f:write(string.format("        tmp = tab[\"%s\"];\n", m.id))
                f:write("        if (tmp != null)\n")
                f:write("        {\n")
                f:write(string.format("            %s %s;\n", realName, m.id))
                f:write(string.format("            FromLua(out %s, tmp as SLua.LuaTable);\n", m.id))
                f:write(string.format("            ret.%s = %s;\n", m.id, m.id))
                f:write("        }\n")
            else
                f:write(string.format("        %s %s;\n", realName, m.id))
                f:write(string.format("        FromLua(out %s, tab[\"%s\"] as SLua.LuaTable);\n", m.id, m.id))
                f:write(string.format("        ret.%s = %s;\n", m.id, m.id))
            end
        else
            if m.opt == "optional" then
                f:write(string.format("        tmp = tab[\"%s\"];", m.id))
                if type.tag == kTagEnum then
                    f:write(string.format(" if (tmp != null) ret.%s = (%s)Convert.ToInt32(tmp);\n", m.id, realName))
                else
                    f:write(string.format(" if (tmp != null) ret.%s = Convert.%s(tmp);\n", m.id, type.csConv))
                end
            else
                if type.tag == kTagEnum then
                    f:write(string.format("        ret.%s = (%s)Convert.ToInt32(tab[\"%s\"]);\n", m.id, realName, m.id))
                else
                    f:write(string.format("        ret.%s = Convert.%s(tab[\"%s\"]);\n", m.id, type.csConv, m.id))
                end
            end
        end
    end
    f:write("    }\n\n")
end

local function exportStructLuaConv(g, csFile)
    local c = {
        exp = {struct = {}, list = {}, map = {}},    -- 导出类型
        unique = {},    -- 去重
    }

    for _, t in ipairs({"bool", "byte", "i16", "i32", "i64", "double", "string"}) do
        local ref = {kRefList, {kRefNormal, t}}
        c.unique[genCsTypeName(g, ref)] = true
        table.insert(c.exp.list, ref)
    end

    for i, s in ipairs(g.exp.struct) do
        if s.luaTag == "LuaConv" and not c.unique[s.realName] then
            c.unique[s.realName] = true
            scanMemberDep(g, c, s)
            table.insert(c.exp.struct, s)
        end
    end
    local f = io.open(csFile, 'w+b')
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write("/* generate by export tool\n * do not modify this file manually\n*/\n")
    f:write("using System;\n")
    f:write("using System.Collections.Generic;\n\n")
    f:write("public static class LuaConv\n{\n")

    exportStructList(f, g, c.exp.list)
    exportStructMap(f, g, c.exp.map)
    for _, s in ipairs(c.exp.struct) do
        exportStructLuaConv2(f, g, s)
    end

    f:write("} // class LuaConv\n")
    f:close()
end

local genGenericLuaName2
function genGenericLuaName2(g, ref)
    if ref[1] == kRefNormal then
        local type = getType(g.type, ref[2])
        if type.tag == kTagBuiltin then
            return type.csName
        else
            local s = string.gsub(type.realName, "(%.)", '_')
            return s
        end
    elseif ref[1] == kRefList then
        return string.format("list_%s", genGenericLuaName2(g, ref[2]))
    elseif ref[1] == kRefMap then
        return string.format("map_%s_%s", genGenericLuaName2(g, ref[2]), genGenericLuaName2(g, ref[3]))
    end
end

local function genGenericLuaName(g, ref)
    if ref[1] == kRefNormal then
        return genGenericLuaName2(g, ref)
    elseif ref[1] == kRefList then
        return string.format("generic_list.%s", genGenericLuaName2(g, ref[2]))
    elseif ref[1] == kRefMap then
        return string.format("generic_map.%s_%s", genGenericLuaName2(g, ref[2]), genGenericLuaName2(g, ref[3]))
    end
end

local function exportStructToLua(g, csFile)
    local c = {
        exp = {struct = {}, list = {}, map = {}},    -- 导出类型
        unique = {},    -- 去重
    }

    for _, t in ipairs({"bool", "byte", "i16", "i32", "i64", "double", "string"}) do
        local ref = {kRefList, {kRefNormal, t}}
        c.unique[genCsTypeName(g, ref)] = true
        table.insert(c.exp.list, ref)
    end

    for i, s in ipairs(g.exp.struct) do
        --if (s.envName == "tcligs" or s.envName == "shared") and not c.unique[s.realName] then
        if s.luaTag == "ToLua" and not c.unique[s.realName] then
            c.unique[s.realName] = true
            scanMemberDep(g, c, s)
            table.insert(c.exp.struct, s)
        end
    end

    local f = io.open(csFile, 'w+b')
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write("/* generate by export tool\n * do not modify this file manually\n*/\n")
    f:write("using System;\n")
    f:write("using System.Collections.Generic;\n\n")
    f:write("public static class ThriftToLua\n{\n")
    f:write("    public static void AddThrift(SLua.LuaCodeGen.ExportGenericDelegate add)\n")
    f:write("    {\n")

    for _, l in ipairs(c.exp.list) do
        f:write(string.format("        add(typeof(%s), \"%s\");\n", genCsTypeName(g, l), genGenericLuaName(g, l)))
    end
    for _, m in ipairs(c.exp.map) do
        f:write(string.format("        add(typeof(%s), \"%s\");\n", genCsTypeName(g, m), genGenericLuaName(g, m)))
    end
    for _, s in ipairs(c.exp.struct) do
        f:write(string.format("        add(typeof(%s), null);\n", s.realName))
    end

    f:write("    }\n")
    f:write("} // class ThriftToLua\n")
    f:close()
end

local function exportServiceToLua(services, destPath)
    local prev = json:decode(lib.LoadFile("service.json") or "[]")
    local mapNew = {}
    for _, s1 in ipairs(prev) do
        local s = {name =s1.name, envName = prev.envName, member = {}}
        for _, m in ipairs(s1.member) do
            s.member[m.id] = m
        end
        mapOld[s1.name] = s
    end

    local mapNew = {}
    for _, s1 in ipairs(services) do
        local s = {name =s1.name, envName = prev.envName, member = {}}
        for _, m in ipairs(s1.member) do
            s.member[m.id] = m
        end
        mapNew[s1.name] = s
    end

    for _, s in ipairs(service) do
        if not lib.EndWith(s.name, "S2C") then
            print("sss", s.name)
        else
            local serviceName = string.format("%sHandler", s.name)
            local serviceFile = string.format("%s.lua", serviceName)
            local fd = lib.LoadFile(serviceFile)
            if not fd then
                -- write new file
            else

            end

        end
    end
end

-- 解析thrift并导出成json
-- srcThrift: 源协议文件
-- destJson:  目标Json文件
-- csFile:    目标Csharp文件
local function parseThrift(srcThrift, destJson, csFile)
    local ret = require("thrift-parser").ParseFile(srcThrift)
    if not ret then
        print(string.format("parse thrift file failed, file:%s", srcThrift))
        return
    end

    local g = {
        type = setmetatable({}, {__index = kBuiltinTypes}),
        exp = {const = {}, enum = {}, tip = {}, struct = {}, service = {}}
    }
    g.env = newEnv(g, lib.GetFileName(srcThrift))
    processData(g, ret)

    -- 导出Lua转换代码
    exportStructLuaConv(g, "22" .. csFile)
    exportStructToLua(g, csFile)
    exportServiceToLua(g.exp.service)

    local etc ={}
    local str = json:encode({const=g.exp.const, enum=g.exp.enum, tip=g.exp.tip},
        etc, {pretty = true, indent = "  ", align_keys = false})
    local f = io.open(destJson, 'w')
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write(str)
    f:close()
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
