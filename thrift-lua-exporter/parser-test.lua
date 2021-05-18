local lib = require "lib"
local parser = require "cpp-parser"

local s = lib.LoadFile("cpp_test_unit.cpp")
local p = parser.CreateParser()

--print(s)
p:ParseSource(s)






