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

local function writeJson(exp, fileName)
    local f = io.open(fileName, 'w')
    f:write(string.char(0xef, 0xbb, 0xbf))
    f:write("{\n")

    -- const
    f:write("  \"const\": [\n")
    for _, c in ipairs(exp.const) do
        --f:write(string.format("\"name\": \"%s\", \"realName\":\"%s\", value:%s\n"), )
    end
    if #exp.const > 0 then
        f:seek("cur", -2)
        f:write("\n")
    end
    f:write("  ],\n")

    -- tip
    f:write("  \"tip\": [\n")
    for _, t in ipairs(exp.tip) do
        --f:write(string.format("\"name\": \"%s\", \"realName\":\"%s\", value:%s\n"), )
    end
    if #exp.tip > 0 then
        f:seek("cur", -2)
        f:write("\n")
    end
    f:write("  ],\n")

    -- enum
    f:write("  \"enum\": [\n")
    for _, e in ipairs(exp.enum) do
        --f:write(string.format("\"name\": \"%s\", \"realName\":\"%s\", value:%s\n"), )
    end
    if #exp.enum > 0 then
        f:seek("cur", -2)
        f:write("\n")
    end
    f:write("  ]\n")

    f:write("}")
    f:close()
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

local function upateTip(tips)
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
    return true
end

local function updateFiles()
    local text = lib.LoadFile("test.json")
    local thrift = josn:decode(text)
    upateTip(thrift.tip)
end

print("updateFiles")
updateFiles()
