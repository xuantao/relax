-- startup root
local lib = require "lib"
local exp = require "export-ui"

local cfg = {
    thriftFile = "test.json",
    csharpFile = "CustomEnum.json",
    enumDefFile = "EnumDef.lua",
    errorCodeFile = "UIErrorCodeTab.xls",
    tipCodeFile = "UITipsNotifyCodeTab.xls",
}
_DEBUG_XLS_ = true  -- debug模式
--exp.Export(cfg)

local function on_thrift_upate()
end

local function on_csharp_update()
end

local function export_tabs()
end

print("argc", arg[1])

--print(lib.Conv("宣涛", "utf-8", "gbk"))





