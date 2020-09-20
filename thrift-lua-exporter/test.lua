-- thrift parser
-- 解析thrift中定义的部分内容(const, enum等)
local lpeg = require "lpeglabel"
local lib = require "lib"
local parser = require "thrift-parser"
local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C, Cb, Cf, Cg, Cp, Ct, Cmt = lpeg.C, lpeg.Cb, lpeg.Cf, lpeg.Cg, lpeg.Cp, lpeg.Ct, lpeg.Cmt

-- 基础模式
local p_space = lpeg.S' \t\n'^0                                                 -- 空白字符
local p_comment = lpeg.P'//' * (1 - lpeg.P'\n')^0 * (lpeg.P'\n' + -lpeg.P(1))   -- 单行注释
local p_multi_line_comment = lpeg.P'/*' * (1 - lpeg.P'*/')^0 * lpeg.P'*/'       -- 多行注释
local p_empty = (lpeg.S' \t\n' + p_comment + p_multi_line_comment)^0            -- 空白内容
local p_sign = lpeg.S'+-'^-1                                                    -- 符号(+-)
local p_digit = lpeg.R('09')                                                    -- 数字
local p_decimal = p_digit^1                                                     -- 十进制
local p_hexadecimal = lpeg.P'0' * lpeg.S'xX' * lpeg.R('09', 'af', "AF")^1       -- 十六进制
local p_float = (p_digit^1 * lpeg.P'.' * p_digit^0 + lpeg.P'.' * p_digit^1) *
    (lpeg.S'eE' * p_sign * p_digit^1)^-1                                        -- 浮点数值
local p_idsafe = lpeg.R('AZ', 'az', '\127\255') + lpeg.P'_'                     -- id safe begin
local p_identity = p_idsafe * (p_idsafe + p_digit)^0                            -- identity
local p_reference = p_identity * (p_empty * P'.' * p_empty * p_identity)^0      -- reference
-- 提取器
local c_reference = Ct(C(p_identity) * (p_empty * P'.' * p_empty * C(p_identity))^0) /
    function (ps) return table.concat(ps, '.') end
local c_annotation = (P'//' * P'/'^0 * C((1 - P'\n')^0) * (P'\n' + P(-1)) +
    P'/*' * (P'*' - P'*/')^0 * C((1 - P'*/')^0) * P'*/')^-1/1                   -- 提取注释
local c_tag = (P'/*<@' * p_space * C(p_reference) * p_space * P'>*/')^-1/1      -- 提取标记

local c_include = P'#' * p_empty * P'include' * p_empty *
    (P'"' * C(1 - P'"') * P'"' +  P'<' * C(1 - P'>') * P'>')
local c_typedef = P'typedef'
local c_type = P {
    V'list' + V'map' + V'normal',
    normal = C(p_reference) / function(ref) return {"normal", ref} end,
    list = P'list' * p_space * P'<' * p_space * V(1) * p_space * P'>' /
        function(ref) return {"list", ref} end,
    map = P'map' * p_space * P'<' * p_space * V(1) * p_space * P',' * p_space * V(1) * p_space * P'>' /
        function(key, val) return {"map", key, val} end,
}

local function checkPair(str)
    local b = lpeg.P{ "{" * ((1 - lpeg.S"{}") + lpeg.V(1))^0 * "}" }
    print(b:match(str))
end

local function prefer(sur, pre)
    return sur and sur ~= "" and sur or pre
end

local struct = P {
    V'struct',
    struct = P'struct' * p_space * c_tag * p_empty * C(p_identity) * p_empty * V'body' /
        function (tag, id, mems) return {"struct", id, {tag = tag, member = mems}} end,
    body = P'{' * p_empty * Ct(V'member'^0) * p_empty * P'}',
    member = p_decimal * p_empty * P':' * p_empty * V'opt' * p_empty * c_type * p_empty * C(p_identity) * p_empty * S','^0 * p_empty /
        function(opt, type, id) return {opt = opt, type = type, id = id} end,
    opt = C'required' + C'optional',
}

local service = P {
    V"service",
    service = P'service' * p_empty * C(p_identity) * p_empty * V'body' /
        function (id, mems) return {"service", id, mems} end,
    body = P'{' * Ct(V'member'^0) * p_empty * P'}',
    member = p_space * c_annotation * p_space * P'oneway' * p_empty * P'void' * p_empty * C(p_identity) * p_empty *
        P'(' * Ct(V'argument'^0) * p_empty * P')' * S',;'^0 * S' \t'^0 * c_annotation /
        function (pre_desc, id, args, suf_desc) return {id = id, args = args, desc = prefer(suf_desc, pre_desc)} end,
    argument = p_empty * C(p_decimal) * p_space * P':' * p_space * c_type * p_empty * C(p_identity) * p_space * S',;'^0 /
        function (index, type, id) return {id = id, type = type, --[[index = index]]} end
}

local soruce = [[
service LuaMiscServiceC2S {
    // 通用抽奖
	oneway void QueryPrizePoolInfo(1:list<i16> indexList)
    oneway void DrawOutActivityPrize(1:i16 poolIndex, 2:i16 drawOutCount, 3:list<i16> posIndexList, 4:i64 stackCount)
    oneway void SelectExtraPrizeItem(1:i16 poolIndex, 2:list<i16> prizeIndexList)
    oneway void RedoubleForPrize(1:i16 poolIndex, 2:bool redouble)
    oneway void ActivePrizePool(1:i16 poolIndex)

    oneway void UseItemToTargetUser(1:tcligs.ItemInfo itemInfo, 2:i64 targetUid, 3:i16 targetPlayerKind)
    oneway void GetUserFeatureList()
    oneway void BuyFeature(1:i64 playerId, 2:i16 featureId)
    oneway void ActivateFeature(1:i64 playerId, 2:tcligs.ItemInfo itemInfo)
    oneway void ApplyFeature(1:i64 playerId, 2:byte slotIdx, 3:i16 featureId)
    oneway void BatchBuyAndApplyFeature(1:i64 playerId, 2:list<i16> featureIds)

    // 指尖柏青哥 pachinko
    oneway void QueryPachinkoInfo() // 查询信息
    oneway void PachinkoUseBall(1:PachinkoUseBallResult result) // 客户端通知服务器钢珠结果，扣减钢珠数目，若进洞，则增加抽奖次数
    oneway void PachinkoDrawLottery(1:PachinkoDrawLotteryMode mode) // 抽奖：从普通奖池中抽奖，并通知客户端奖励，注明普通奖池的结果
    oneway void QueryPachinkoAwardBallCount(1:list<i64> uids) // 批量查询玩家奖励获得的钢珠数目
    oneway void BlessPachinkoBall(1:i64 targetUid) // 祝福双方获得钢珠
    oneway void QueryPachinkoBallAwardInfo() // 查询玩家当前领取过的柏青哥钢珠奖励项的列表
    oneway void TakePachinkoBallAward(1:i16 awardID) // 领取奖励项对应奖励

    // 复活
    oneway void Revive(1:ReviveInfo reviveInfo)

    // 魔法属性周卡
    oneway void QueryEffectCardInfo(1:list<i32> effectCardIds) // 查询魔法属性周卡信息
    oneway void CoinBuyEffectCard(1:i32 effectCardId) // 通宝购买魔法属性周卡

    // 坐骑
    oneway void QueryHorseInfo()
}
]]

local source2 = [[
service LuaMiscServiceS2C {
        // 通用抽奖
        oneway void QueryPrizePoolInfoRsp(1:LMSError asErr, 2:list<PrizePoolInfo> infoList)
        oneway void DrawOutActivityPrizeRsp(1:LMSError asErr, 2:list<DrawOutPrizeResultInfo> infoList, 3:PrizePoolInfo poolInfo)
        oneway void SelectExtraPrizeItemRsp(1:LMSError asErr, 2:PrizePoolInfo info)
        oneway void RedoubleForPrizeRsp(1:LMSError asErr, 2:i16 poolIndex, 3:DrawOutPrizeResultInfo info, 4:double magnification, 5:i32 fakeLuckyValue)
        oneway void ActivePrizePoolRsp(1:LMSError asErr, 2:list<DrawOutPrizeResultInfo> infoList, 3:PrizePoolInfo poolInfo)
    
        oneway void UseItemToTargetUserRsp(1:LMSError asErr, 2:tcligs.ItemInfo itemInfo, 3:i64 targetUid, 4:i16 targetPlayerKind)
        oneway void GetUserFeatureListRsp(1:list<UserFeatureInfo> featureList)
        oneway void BuyFeatureRsp(1:tcligs.ASError asErr, 2:i64 playerId, 3:UserFeatureInfo featureInfo)
        oneway void ActivateFeatureRsp(1:tcligs.ASError asErr, 2:i64 playerId, 3:UserFeatureInfo featureInfo)
        oneway void ApplyFeatureRsp(1:tcligs.ASError asErr, 2:i64 playerId, 3:byte slotIdx, 4:tcligs.FeatureSlotInfo slotInfo, 5:UserFeatureInfo featureInfo)
        oneway void BatchBuyAndApplyFeatureRsp(1:tcligs.ASError asErr, 2:map<byte,i16> slot2IdMap)
    
        // 指尖柏青哥 pachinko
        oneway void QueryPachinkoInfoRsp(1:PachinkoInfo info) // 查询信息
        oneway void PachinkoUseBallRsp(1:LMSError asErr, 2:PachinkoBallInfo ballInfo, 3:PachinkoUseBallResult result) // 使用弹珠后告知客户端当前钢珠的统计数据
        oneway void PachinkoDrawLotteryRsp(1:LMSError asErr, 2:PachinkoDrawLotteryMode mode, 3:PachinkoDrawLotteryResult result) // 告知客户端抽奖结果以及相关统计数据
        oneway void OnStartRateChangeMode(1:i64 endTime) // 普通奖池抽奖x次后进入确变判定模式，并随着普通奖池失败的数目来提升判定概率。当成功触发确变时，进入确变模式（有效期根据策划配置，如60s），通知客户端
        oneway void OnStopRateChangeMode() // 到时间后或者抽完大奖后服务器通知客户端停止确变模式
        oneway void OnStartCoverWorstCase(1:i64 endTime) // 保底机制：当玩家丢失一定数量的球且没有中过一次奖的情况出现之后，这段时间内所有丢失的球均视为中奖
        oneway void OnStopCoverWorstCase()
        oneway void OnStopMagnificationHole() // 倍率洞结束(开始则是在抽奖获得倍率洞的时候作为结果下发)
        oneway void OnStartTreasureHole(1:i64 endTime) // 藏宝洞：每丢失100个球，有5%的概率判定开启。开启后在有效期间击中藏宝洞（由客户端判定），可以抽一次藏宝洞的奖池
        oneway void OnStopTreasureHole()
        oneway void QueryPachinkoAwardBallCountRsp(1:list<i32> awardBallCounts)
        oneway void BlessPachinkoBallRsp(1:LMSError asErr) // 祝福双方获得钢珠
        oneway void QueryPachinkoBallAwardInfoRsp(1:list<i16> rewardedSet) // 查询玩家当前领取过的柏青哥钢珠奖励项的列表
        oneway void TakePachinkoBallAwardRsp(1:LMSError asErr, 2:list<i16> rewardedSet) // 领取奖励项对应奖励
    
        // 复活相关
        oneway void ReviveRsp(1:LMSError asErr)
        oneway void OnSyncPlayerReviveCtrl(1:PlayerReviveCtrl ctrlInfo)
        oneway void OnBroadcastUserReviveInfo(1:list<BattlegroundReviveInfo> infoList)
    
        // 魔法属性周卡
        oneway void QueryEffectCardInfoRsp(1:list<EffectCardInfo> info)
        oneway void CoinBuyEffectCardRsp(1:LMSError asErr) // 通宝购买魔法属性周卡
    
        // 坐骑
        oneway void QueryHorseInfoRsp(1:i32 curHorseID, 2:list<i32> horseIDList)
        oneway void OnAddHorse(1:i32 horseID)
    }
        
]]

--local ret = service:match(soruce)
--lib.Log(ret)

--print("1111111111111111111111")
--ret = service:match(source2)
--lib.Log(ret)

function EndWith(str, s)
    local b, e = string.find(str, string.format("%s$", s))
    return b, e
end

--print(EndWith("ssssS2C", "S2C"))
--print(EndWith("ssssS2C", "C2S"))


local comment111 = P {
    P"--" * V"LongStr" / 1 + P"--" * (P(1) - P"\n")^0;
    LongStr  = V"Open" * (P(1) - V"CloseEq")^0 * V"Close";
    Open     = "[" * Cg(V"Equals", "openEq") * "[" * P"\n"^-1;
    Close    = "]" * C(V"Equals") * "]";
    Equals   = P"="^0;
    CloseEq  = Cmt(V"Close" * Cb("openEq"), function (s, i, closeEq, openEq) return #openEq == #closeEq end);
}

--local lastComment22 = P'\n' * S" \r\t"^0 * comment * (S" \r\t\n" + comment)^0 * P(-1)

local longStr = P {
    V"string",
    equals = lpeg.P"="^0,
    open = "[" * lpeg.Cg(V"equals", "init") * "[" * lpeg.P"\n"^-1,
    close = "]" * lpeg.C(V"equals") * "]",
    closeeq = lpeg.Cmt(V"close" * lpeg.Cb("init"), function (s, i, a, b) return a == b end),
    string = V"open" * lpeg.C((lpeg.P(1) - V"closeeq")^0) * V"close" / 1,
}

local comment = P"--" * (longStr / 0) + P"--" * (P(1) - P"\n")^0
local lastComment = P'\n' * S" \r\t"^0 * comment * (S" \r\t\n" + comment)^0 * P(-1)

local src = [[
  --[=[xuantao
  ]=]

    --[=[ dfadfadta
    -- abcd]=]
]]

local function parseSource(text)
    if not text then
        return
    end

    local pos = 1
    local ret = {}
    local len = #text
    while pos < len do
        local e, p = lpeg.match(lastComment, text, pos)
        print("0000000000", e, p, pos)
        if not e then
            --print("1111111111", e, p, pos)
            e, p = lpeg.match(comment, text, pos)
            print("2222222222", e, p, pos)
            if e then
                pos = e
                --print(string.sub(text, e))
            else
                pos = pos + 1
            end
        else
            --pos = e
            break
        end
        --[[e, pos = lpeg.match(lastComment, text, pos)
        if not e or not pos then
            print(e, pos)
            break
        end
        print(e, pos)
        table.insert(ret, e)
        ]]
    end
    print (pos, string.sub(text, pos))
    return ret
end

local ss = [=[
function LoginServiceS2CHandler:RecoverLoginRsp(lsErr, clientReOutN, recoverToken)
    --@TODO:
end


--@Args[[tcligs.LSError lsErr, string recoverToken]]

]=]
--parseSource(src)
--print(longStr:match("[=[xxxxxxxxxxxx]=]"))
--print(lastComment:match(src))

local p_longStr = lpeg.P {
    lpeg.V"string",
    equals = lpeg.P"="^0,
    open = "[" * lpeg.Cg(lpeg.V"equals", "init") * "[" * lpeg.P"\n"^-1,
    close = "]" * lpeg.C(lpeg.V"equals") * "]",
    closeeq = lpeg.Cmt(lpeg.V"close" * lpeg.Cb("init"), function (s, i, a, b) return a == b end),
    string = lpeg.V"open" * lpeg.C((lpeg.P(1) - lpeg.V"closeeq")^0) * lpeg.V"close" / 1,
}

local p_comment = lpeg.P"--" * (p_longStr / 0) + lpeg.P"--" * (lpeg.P(1) - lpeg.P"\n")^0
local p_desc = lpeg.P"\n" * lpeg.S" \r\t"^0 * p_comment * (lpeg.S" \r\t\n" + p_comment)^0 * lpeg.P(-1)

local function parseDescPos(source)
    local pos = 1
    local len = #source
    local ret = len
    while pos < len do
        local p = lpeg.match(p_desc, source, pos)
        
        if p then
            ret = p + 1
            break
        else
            p = lpeg.match(p_comment, source, pos)
            print("p", pos, p)
            pos = p or (pos + 1)
        end
    end
    return ret
end
local ok = [[

-- sss
]]

local pos = parseDescPos(ss)
print(pos)
print(string.sub(ss, pos))

print("222222222222")
pos = parseDescPos(ok)
print(pos)
print(string.sub(ok, pos))

