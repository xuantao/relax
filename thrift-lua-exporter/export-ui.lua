local lib = require("lib")
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

local function trans()
    local ret = require("thrift-parser").ParseFile("./test/tcligs.thrift")
    if not ret then
        print(string.format("parse file failed"))
        return
    end

    local g = {
        env = {name = "tcligs", namespace = "", type = {}},
        exp = {const = {}, enum = {}, tip = {}}
    }

    processData(g, ret)
    lib.Log("v =", g.exp)
end

trans()

