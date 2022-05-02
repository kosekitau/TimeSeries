library(urca)
library(fpp)
library(vars)
library(ggplot2)
library(ggfortify)

# アメリカの消費・収入の増加率データ
usconsumption
autoplot(usconsumption, fasets=T)

# 単位根の確認
summary(ur.df(usconsumption[, "consumption"], type="drift"))
summary(ur.df(usconsumption[, "income"], type="drift"))

# カーネル密度推定
plot(density(usconsumption[, "consumption"]))
plot(density(usconsumption[, "income"]))

# 相互相関の計算
autoplot(ccf(usconsumption[, "consumption"], usconsumption[, "income"], plot=F))

# VARモデルの次数を決める作業(constで定数項あり)
select_result <- VARselect(usconsumption, lag.max=10, type="const")
select_result # AICが最も高い次数を選ぶ

# VARモデル化
var_bestorder <- VAR(
  y = usconsumption,
  type = "const",
  p = select_result$selection[1]
)
summary(var_bestorder)

# 予測
predict(var_bestorder, n.ahead=4)
autoplot(
  predict(var_bestorder, n.ahead=20), 
  ts.colour=1, 
  predict.colour=1,
  predict.linetype = "dashed"
  )

# Granger因果性検定
# Granger：帰無仮説がGrangerの因果がない
# Instant：Granger瞬時因果性同時刻における影響

# incomeがconsumptionに与える影響
causality(var_bestorder, cause="income")

# consumptionがincomeに与える影響
causality(var_bestorder, cause="consumption")


# インパルス応答関数
irf_consumption <- irf(
  var_bestorder,
  impulse = "consumption",
  response = c("consumption", "income"),
  n.ahead = 12,
  boot = T
)
plot(irf_consumption)

# 分散分解
plot(fevd(var_bestorder, n.ahead=12))









