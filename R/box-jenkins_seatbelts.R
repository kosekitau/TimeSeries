library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)

front <- Seatbelts[, 'front']

hist(front)

# 対数をとる
log_fornt <- log(front)
hist(log_front)
ggtsdisplay(log_front)

# 対数差分
log_diff <- diff(log_front)
hist(log_diff)
ggtsdisplay(log_diff)

ggsubseriesplot(front)

# 12レコード前との差分
diff(front, lag=12)
front
# 対数をとって、季節差分
seas_log_diff <- diff(log_diff, lag=12)
hist(seas_log_diff)
ggtsdisplay(seas_log_diff)

# 死者数、ガソリンの値段、シートベルト義務化の法律フラグ
Seatbelts_log <- Seatbelts[, c("front", "PetrolPrice", "law")]
Seatbelts_log[, "front"] <- log(Seatbelts[, "front"]) # 目的変数は対数変換
max(Seatbelts[, "PetrolPrice"])
Seatbelts_log[, "PetrolPrice"] <- log(Seatbelts[, "PetrolPrice"])

# データの分割
train <- window(Seatbelts_log, end=c(1983, 12))
test <- window(Seatbelts_log, start=c(1984, 1))
# 説明変数
petro_law <- train[, c("PetrolPrice", "law")]
  
# SARIMAX
model_sarimax <- Arima(
  y = train[, "front"], # 目的変数
  order = c(1, 1, 1), # p=1, d=1, q=1
  seasonal = list(order = c(1, 0, 0)), # P=1, D=1, Q=1
  xreg = petro_law
)

model_sarimax

sarimax_petro_law <- auto.arima(
  y = train[, "front"], 
  xreg = petro_law,
  ic = "aic",
  seasonal.test = "ch",
  max.order = 7, # p+q+P+Q <= 7
  stepwise = F,
  approximation = F,
  parallel = T,
  num.cores = 4
)


sarimax_petro_law

checkresiduals(sarimax_petro_law)

# 残差の正規性検定
# 帰無仮説が正規分布に従う
jarque.bera.test(resid(sarimax_petro_law))


# 予測
petro_law_test <- test[, c("PetrolPrice", "law")]
sarimax_f <- forecast(
  sarimax_petro_law,
  xreg = petro_law_test,
  h = 12,
  level = c(95, 70)
)
autoplot(sarimax_f, predict.colour=1)


# RMSE
accuracy(sarimax_f, x=test[, "front"])["Test set", "RMSE"]

