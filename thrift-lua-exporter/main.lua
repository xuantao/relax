-- startup root
local lib = require "lib"
local exp = require "export-ui"

local cfg = {
    thriftFile = "tcligs.json",
    csharpFile = "CustomEnum.json",
    enumDefFile = "EnumDef.lua",
    errorCodeFile = "UIErrorCodeTab.xls",
    tipCodeFile = "UITipsNotifyCodeTab.xls",
}

_DEBUG_XLS_ = true  -- debug模式
exp.ParseThrift("test/tcligs.thrift", "tcligs.json", "LuaConv.cs")
exp.Export(cfg)

local actions = {}
function actions.UpdateCsharp()
    exp.Export(cfg)
end

function actions.UpdateThrift()
    exp.ParseThrift("test/tcligs.thrift", "tcligs.json", "LuaConv.cs")
    exp.Export(cfg)
end

function actions.ExportTabs()
end


if actions[arg[1]] then
    actions[arg[1]]()
else
    print(string.format("target action[%s] does not exist ", arg[1]))
end





