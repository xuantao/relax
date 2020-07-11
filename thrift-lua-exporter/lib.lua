
local function visitTable(val, p)
    local np = function (s, d)
        p(string.format("%s%s", string.rep(' ', d*2), s))
    end

    local stack = {"self"}
    local visit
    visit = function(t, d, r)
        for k, v in pairs(t) do
            local ty = type(v)
            if ty == "string" then
                np(string.format("[%s]=\"%s\",", tostring(k), v), d)
            elseif ty == "table" then
                if r[v] then
                    np(string.format("[%s]={%s},", tostring(k), r[v]), d)
                else
                    table.insert(stack, tostring(k))
                    r[v] = table.concat(stack, '.')
                    np(string.format("[%s]={", tostring(k)), d)
                    visit(v, d + 1, r)
                    np("},", d)
                    table.remove(stack, #stack)
                end
            else
                np(string.format("[%s]=%s,", tostring(k), tostring(v)), d)
            end
        end
    end
    p("{")
    visit(val, 1, {[val] = "self"})
    p("}")
end

-- 转换位字符串
local function toStr(val, sep)
    local str
    sep = sep or '\n'
    if type(val) == "table" then
        visitTable(val, function (s)
            if str then
                str = string.format("%s%s%s", str, sep, s)
            else
                str = s
            end
        end)
    else
        str = tostring(val)
    end
    return str
end

local function log(val)
    print(toStr(val))
end

return {
    ToStr = toStr,
    Log = log,
}
