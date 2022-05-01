library(urca)
library(lmtest)
library(prais)
library(ggplot2)
library(ggfortify)
library(gridExtra)

# ホワイトノイズ同士で回帰
n_sample <- 400
set.seed(1)
y_wn <- rnorm(n = n_sample)
x_wn <- rnorm(n = n_sample)

mod_ols_wn <- lm(y_wn ~ x_wn) 
# Pr(>|t|)は帰無仮説を0としたときのP値
summary(mod_ols_wn)


# 単位根のあるデータ同士で回帰
set.seed(1)

# 確率的トレンドを持たせる
y_rw <- cumsum(rnorm(n = n_sample))
x_rw <- cumsum(rnorm(n = n_sample))

mod_ols_rw <- lm(y_rw ~ x_rw)
summary(mod_ols_rw)

# ランダムウォーク2種を表示
grid.arrange(autoplot(ts(y_rw)), autoplot(ts(x_rw)), nrow=2)

# AR(1)に従うデータを生成
y_ar <- arima.sim(
  n = n_sample,
  model = list(order = c(1, 0, 0), ar = c(0.8))
)
x_ar <- arima.sim(
  n = n_sample,
  model = list(order = c(1, 0, 0), ar = c(0.8))
)
grid.arrange(autoplot(ts(y_ar)), autoplot(ts(x_ar)), nrow=2)
# 相互相関を見る
print(ccf(x = x_ar, y = y_ar, lag.max = 30))

mod_ols_ar <- lm(y_ar ~ x_ar)
summary(mod_ols_ar)


# Durbin-Watson検定(帰無仮説は自己相関が0)
# ランダムウォーク回帰の残差
resid_ols <- mod_ols_rw$residuals
# DW検定(p-value = 0)
dwtest(mod_ols_rw)
# 自己相関のプロット
ggtsdisplay(resid_ols)

# ホワイトノイズ(p-value = 0.8261)
dwtest(mod_ols_wn)
ggtsdisplay(mod_ols_wn$residuals)

# AR(1)(p-value = 0)
dwtest(mod_ols_ar)
ggtsdisplay(mod_ols_ar$residuals)


###############################
## 見せかけの回帰を防ぎつつ予測

# RWヘ単位根を確認
summary(ur.df(y_rw, type="none"))
summary(ur.df(x_rw, type="none"))

# AR(1)へ単位根を確認
summary(ur.df(y_ar, type="none"))
summary(ur.df(x_ar, type="none"))

#AR(1)へGLS
d <- data.frame(
  y_ar = y_ar,
  x_ar = x_ar
)
mod_gls_PW <- prais_winsten(y_ar ~ x_ar, data=d, max_iter = 1)
summary(mod_gls_PW)

# 単位根へは差分を取る
mod_lm_diff <- lm(diff(y_rw) ~ diff(x_rw))
summary(mod_lm_diff)






