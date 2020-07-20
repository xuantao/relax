-- startup root
--require "export_thrift_error"


local function Trim(str)
    local b, l = string.find(str, "^[ \t\n]*")
    local e, l2 = string.find(str, "[ \t\n]*$")
    return string.sub(str, b + l, e - 1)
end

print(Trim("xuantao") .."$")
print(Trim("xuan tao") .."$")
print(Trim(" xuantao") .."$")
print(Trim("xuantao ") .."$")
print(Trim(" xuantao ") .."$")
print(Trim("\txuantao\t") .."$")
print(Trim("\t xuantao\t ") .."$")



