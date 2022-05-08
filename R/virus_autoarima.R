library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)

# データ読み込み
data <- read.csv('https://covid19.mhlw.go.jp/public/opendata/newly_confirmed_cases_daily.csv')
head(data)

# 対数
train <- log1p(data["Tokyo"])
autoplot(train)

model <- auto.arima(
  y = train, 
  ic = "aic",
  seasonal.test = "ch",
  max.order = 12, # p+q+P+Q <= 7
  seasonal = TRUE,
  approximation = F,
  parallel = T,
  num.cores = 4
)

model
# 残差プロット
checkresiduals(model)# 残差に自己相関ある
# 残差の正規性検定
jarque.bera.test(resid(sarimax_petro_law))


sarimax_f <- forecast(
  model,
  h = 7,
  level = c(95, 70)
)
autoplot(sarimax_f, predict.colour=1)
exp(sarimax_f$Forecast)
exp(sarimax_f$x)

sarimax_f$x <- expm1(sarimax_f$x)
sarimax_f$mean <- expm1(sarimax_f$mean)
sarimax_f$lower <- expm1(sarimax_f$lower)
sarimax_f$upper <- expm1(sarimax_f$upper)
autoplot(sarimax_f, predict.colour=1, x)

# 精度
accuracy(sarimax_f)







