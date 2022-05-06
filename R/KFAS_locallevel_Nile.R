library(KFAS)
library(ggplot2)

# データの準備
nile_train <- window(Nile, end=1950)
nile_train[41:60] <- NA # 欠損値
head(Nile)
plot(Nile)
kernel_density <- density(Nile)
plot(kernel_density)


# モデルの構造を決める
build_kfas <- SSModel(
  H = NA, # 観測誤差は不明(推測する)
  nile_train ~ SSMtrend(degree=1, Q=NA) #nile_trainはローカルレベルモデルに従う、過程誤差は不明
)

# 最尤法で過程誤差と観測誤差を推定する
# initsで過程、観測誤差の分散は適当な初期値で設定
fit_kfas <- fitSSM(build_kfas, inits=c(1, 1))

# フィルタリングと平滑化
result_kfas <- KFS(
  fit_kfas$model, # $modelに推定したパラメタを使ったmodelが入ってる
  filtering=c("state", "mean"),
  smoothing=c("state", "mean")
)

# フィルタ化推定量の取り出し
# $aは状態の1期先予測(フィルタリングされる前の値なので前期のフィルタ化推定量)
mu_filter_kfas <- result_kfas$a[-1]
# 平滑化状態の取り出し
mu_smooth_kfas <- result_kfas$alphahat


# フィルタ化推定量の表示
df_filter <- data.frame(
  y = as.numeric(Nile[1:80]),
  time = 1871:1950,
  mu_filter = mu_filter_kfas
)
ggplot(data = df_filter, aes(x=time, y=y)) +
  geom_point(alpha=0.6) +
  geom_line(aes(y=mu_filter), size=1.2)

# 信頼区間の計算
smooth_conf <- predict(fit_kfas$model, interval = "confidence", level=0.95)
# 予測区間の計算
smooth_pred <- predict(fit_kfas$model, interval = "prediction", level=0.95)

# 20時点先を予測
forecast_pred <- predict(fit_kfas$model, interval="prediction", level=0.95, n.ahead=20)
# 予測区間と予測結果を結合
estimate_all <- rbind(smooth_pred, forecast_pred)
estimate_all


# Nileと平滑化結果と予測と予測区間の表示
df_forecast <- cbind(
  data.frame(y=as.numeric(Nile), time=1871:1970),
  as.data.frame(estimate_all)
)
ggplot(data=df_forecast, aes(x=time, y=y)) +
  geom_point(alpha=0.5) +
  geom_line(aes(y=fit), size=1.2) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3)






