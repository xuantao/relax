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
local comm = space * lpeg.P'//' * (1-lpeg.P'\n')^0 * lpeg.P'\n'^-1
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
enumstruct
]]

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

local function testScan()
    local intr = anywhere("enum", "struct", "include")
    local pos = 1
    while true do
        local l
        pos, l = intr:match(text_com, pos)
        if not pos then
            break
        end

        print("intr", pos, l, string.sub(text_com, pos, pos + 10))
        pos = pos + 1
    end
end

local function genScanner(...)
    local i = lpeg.Cp()
    local p = nil
    for _, key in ipairs({...}) do
        if not p then
            p = lpeg.C(key)
        else
            p = p + lpeg.C(key)
        end
    end
    return lpeg.P{ i * p * i + 1 * lpeg.V(1) } / function (b, k, e) return k, b, e end
end

local function testCapture()
    print("testCapture")

    local scanner = genScanner("//", "enum", "include", "struct")
    local p = 1
    while true do
        local k, b, e = scanner:match(text_com, p)
        if not k then
            break
        end

        p = e
        print(k, b, e)
    end
end
--testCapture()


local function test()
    local I = lpeg.Cp()
    local P = comm * I

    local t1 = [[
        // faldgladjga
    ]]

    print(P:match("////// xuagakdgka"))
    print(P:match(t1))

    local P2 = comm / ""
    print(P2:match("////// xuagakdgka") .. '$')
    print(P2:match(t1) .. '$')
end
test()


