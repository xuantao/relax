-- export thrift error code
local lib = require "lib"
local lpeg = require "lpeg"
local source = [[
enum /*<@ErrorCode>*/ LSError {
}
enum /*<@EnumDef>*/ KickReason {
}
]]

local space = lpeg.S ' \t\n'^0
local comm = space * lpeg.P'//' * (1-lpeg.P'\n')^0 * lpeg.P'\n'
local m_comm = space * lpeg.P'/*' * (1 - lpeg.P'*/')^0 * lpeg.P'*/'
local letter = (lpeg.R'az' + lpeg.R'AZ' + lpeg.R'09' + lpeg.S'_')
local c_name = lpeg.C(letter^1)
local enum_label = lpeg.P'/*<@' * c_name * lpeg.P'>*/'

local c_enum = lpeg.P'enum' * space * enum_label * space * c_name
local c_value = lpeg.S'=' * space * lpeg.C(lpeg.R'09'^0)
local c_note = lpeg.P'//' * space * lpeg.C((1-lpeg.P'\n')^0) * lpeg.P'\n'^0
local c_element = space * c_name * space * c_value^-1 * space * lpeg.S',;' * space * c_note^-1

print("000", table.concat({c_element:match(" xuantao = 123,  // nihao  ")}, '-')..'&')
print("000", c_element:match(" xuantao = 246 ,"))


function anywhere(...)
    local i = lpeg.Cp()
    local p = nil
    for _, key in ipairs({...}) do
        if not p then
            p = lpeg.P(key)
        else
            p = p + lpeg.P(key)
        end
    end
    return lpeg.P{ i * p * i + 1 * lpeg.V(1) }
end

local intr = anywhere("enum", "struct", "include")
--local intr = anywhere("enum")



--local 

local text_com = [[
 // xuangag //
    /* */
    /****xagag*/
    /**/
enum test {
}

struct {

}

include ;
]]

local pos = 1
while true do
    local l
    pos, l = intr:match(text_com, pos)
    print(pos, l)
    if not pos then
        break
    end
    pos = pos + 1
end

local s = "/* */"
print("111", lpeg.match(m_comm, s))


local elements = {
    comm, m_comm
}

print("222", enum_label:match("/*<@EnumDef>*/"))

print("333", c_enum:match([[
enum /*<@ErrorCode>*/ LSError {
}
]]))



-- 扫描
--print(comm:match(text_com))
local at = 1
local i = 0
--[[
while true do
    local p
    for id, pat in ipairs(elements) do
        p = pat:match(text_com, at)
        if p then
            print("id", id)
            break
        end
    end
    if not p then
        break
    end

    at = p
    print("at", at)
    i = i + 1
    if i > 10 then
        break
    end
end
]]
--print(text_com:sub(at))






