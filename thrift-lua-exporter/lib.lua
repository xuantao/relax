local lib = {}

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
function lib.ToStr(val, sep)
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

function lib.Log(...)
    local s = {}
    for _, v in ipairs({...}) do
        table.insert(s, lib.ToStr(v))
    end
    print(table.unpack(s))
end

function lib.LoadFile(fileName)
    local f = io.open(fileName, 'r')
    if not f then
        return
    end

    local s = f:read('a')
    f:close()
    if #s >= 3 and
        s:byte(1) == 0xef and
        s:byte(2) == 0xbb and 
        s:byte(3) == 0xbf then
        return s:sub(4)
    end
    return s
end

function lib.TrimLeft(str, pat)
    pat = pat or " \t\r\t"
    local b, l = string.find(str, string.format("^[%s]*", pat))
    print(b, l, str, string.sub(str, b + l))
    return string.sub(str, b + l)
end

function lib.Trim(str, pat)
    pat = pat or " \t\r\t"
    local b, l = string.find(str, string.format("^[%s]*", pat))
    local e, l2 = string.find(str, string.format("[%s]*$", pat))
    return string.sub(str, b + l, e - 1)
end

return lib
