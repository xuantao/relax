local iconv = require "luaiconv"
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

function lib.TrimLeft(str, pat)
    pat = pat or " \t\r\t"
    local b, l = string.find(str, string.format("^[%s]*", pat))
    return string.sub(str, b + l)
end

function lib.Trim(str, pat)
    pat = pat or " \t\r\t"
    local b, l = string.find(str, string.format("^[%s]*", pat))
    local e, l2 = string.find(str, string.format("[%s]*$", pat))
    return string.sub(str, b + l, e - 1)
end

function lib.Split(str, sep)
    local result = {}
    if str == nil or str == '' or sep == nil then
        return result
    end

    for match in (str..sep):gmatch("(.-)"..sep) do
        table.insert(result, match)
    end
    return result
end

function lib.IsUtf8(s)
    local i = 1
    local l = string.len(s)
    local cp, b, b2, trail
    local min
    if l >= 3 and s:byte(1) == 0xef and s:byte(2) == 0xbb and s:byte(3) == 0xbf then
        return 0
    end

    while i <= l do
        b = string.byte(s, i )
        if b < 0x80 then
            cp = b
            trail = 0
            min = 0
        elseif b < 0xc2 then
            return 1
        elseif b < 0xe0 then
            trail = 1
            cp = b - 0xc0
            min = 0x80
        elseif b < 0xf0 then
            trail = 2
            cp = b - 0xe0
            min = 0x800
        elseif b < 0xf4 then
            trail = 3
            cp = b - 0xf0
            min = 0x10000
        elseif b == 0xf4 then
            if string.byte( s, i + 1 ) > 0x8f then
                return 2
            end
            trail = 3
            cp = 4
            min = 0x100000
        else
            return 3
        end

        for j = i + 1, i + trail do
            b = string.byte(s, j )
            if not b or b < 0x80 or b > 0xbf then
                return 4
            end
            cp = cp * 0x40 + b - 0x80
        end
        if cp < min then
            return 5
        end
        i = i + 1 + trail
    end
    return 0
end

local convObj = {}
-- 编码转换
-- 将字符串(s)从源编码(from)转换到目标编码(to)
function lib.Conv(s, from, to)
    to = to or "utf-8"
    local cd = iconv.new(to .. "//TRANSLIT", from)
    local ostr, err = cd:iconv(s)
    if err == iconv.ERROR_INCOMPLETE then
      print("ERROR: Incomplete input.", s)
    elseif err == iconv.ERROR_INVALID then
      print("ERROR: Invalid input.", s)
    elseif err == iconv.ERROR_NO_MEMORY then
      print("ERROR: Failed to allocate memory.", s)
    elseif err == iconv.ERROR_UNKNOWN then
      print("ERROR: There was an unknown error.", s)
    end
    return ostr
end

function lib.LoadFile(fileName)
    local f = io.open(fileName, 'r')
    if not f then
        return
    end

    local s = f:read('a')
    f:close()
    if lib.IsUtf8(s) == 0 then
        if s:byte(1) == 0xef and s:byte(2) == 0xbb and s:byte(3) == 0xbf then
            s = s:sub(4)
        end
    else
        s = lib.Conv(s, "gbk", "utf-8")
    end
    return s
end

return lib
