local lib = require("lib")
local josn = require("Json")
--require("lib").Log(parseFile(arg[1]))

--local f = io.open("ret1.txt", 'w')
--f:write(string.char(0xef, 0xbb, 0xbf))
--f:write(require("lib").ToStr(ret))
--f:close()

local processData

local function concatNs(ns, name)
    if not ns or ns == "" then
        return name
    end
    return string.format("%s.%s", ns, name)
end

local function getRealType(env, name)
    local r = env.type[name]
    if not r then
        return name
    end
    return getRealType(env, r)
end

local function convValue(type, val)
    if type == "byte" or
        type == "i16" or
        type == "i32" or
        type == "i64" or
        type == "double" then
        return tonumber(val)
    elseif type == "string" then
        return val
    end
    return val
end

local function onInclude(g, item)
    local prev = g.env
    g.env = {name = item[2], namespace = "", type = prev.type}
    processData(g, item[3])
    g.env = prev
end

local function onNamespace(g, item)
    if item[3] == "csharp" then
        g.env.namespace = item[2]
    end
end

local function onTypedef(g, item)
    g.env.type[item[2]] = item[3].type
end

local function onConst(g, item)
    local c = item[3]
    local t = getRealType(g.env, c.type)
    local d = {
        name = item[2],
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

    table.insert(g.exp.enum, d)
end

local procs = {
    include = onInclude,
    namespace = onNamespace,
    typedef = onTypedef,
    const = onConst,
    enum = onEnum,
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
        local e = {}
        string.gsub(l, "([^\t]+)", function(c) table.insert(e, c) end)
        table.insert(ret, e)
    end
    return ret
end

local function saveXls(file, tabs)
    local f = io.open(file, 'w')
    f:write(string.char(0xef, 0xbb, 0xbf))
    for _, t in ipairs(tabs) do
        f:write(table.concat(t, '\t'))
        f:write("\n")
    end
    f:close()
end

local function trans()
    local file = "./test/tcligs.thrift"
    local ret = require("thrift-parser").ParseFile(file)
    if not ret then
        print(string.format("parse file failed, file:%s", file))
        return
    end

    local g = {
        env = {name = "tcligs", namespace = "", type = {}},
        exp = {const = {}, enum = {}, tip = {}}
    }
    processData(g, ret)

    local etc ={}
    local str = josn:encode(g.exp, etc, {pretty = true, indent = "  ", align_keys = false})
    local f = io.open("test.json", 'w')
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write(str)
    f:close()
    --lib.Log("v =", g.exp)
end

--trans()
--lib.Log(loadXls("UITipsNotifyCodeTab.xls"))

local function trimLeft(str, pat)
    local b, l = string.find(str, pat)
    return string.sub(str, b + l)
end

local function updateTip(tips)
    local kId = 1
    local kKey = 2
    local kCode = 4
    local kFile = "UITipsNotifyCodeTab.xls"
    local ret = {}
    local src = loadXls(kFile)

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

    saveXls("UITipsNotifyCodeTab_new.xls", ret)
    return ret
end

local function mergeErrorDef(thrift)
    local csharp = josn:decode(lib.LoadFile("CustomEnum.json"))
    local ret = {}
    local cache = {}

    for _, e in ipairs(thrift.enum) do
        if e.errorCode then
            cache[e.realName] = v
        end
    end

    for _, ce in ipairs(csharp) do
        if ce.errorCode then
            local te = cache[ce.realName]
            if te then
                cache[ce.realName] = nil
                print("111", te.name, ce.name)
                te.name = ce.name
                table.insert(ret, te)
            else
                print("222", ce.name)
                table.insert(ret, ce)
            end
        end
    end

    for _, te in ipairs(thrift.enum) do
        if te.errorCode and not cache[te.realName] then
            table.insert(ret, te)
        end
    end

    return ret
end

local function updateErrorCodeXls(thrift)
    local kId = 1
    local kMode = 2
    local kKey = 3
    local kCode = 5
    local kFile = "UIErrorCodeTab.xls"
    local ret = {}
    local src = loadXls(kFile)
    local err = mergeErrorDef(thrift)

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

    saveXls("UIErrorCodeTab_new.xls", ret)
    return ret
end

local function updateDef(thrift, csharp)
end

local function updateFiles()
    local text = lib.LoadFile("test.json")
    local thrift = josn:decode(text)

    updateTip(thrift.tip)
    updateErrorCodeXls(thrift)
end

print("updateFiles")
updateFiles()
