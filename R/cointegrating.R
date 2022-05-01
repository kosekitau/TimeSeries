library(urca)
library(lmtest)
library(prais)
library(ggplot2)
library(ggfortify)
library(gridExtra)

set.seed(10)
n_sample <- 400
rw <- cumsum(rnorm(n = n_sample)) # ランダムウォーク
x_co <- 0.6 * rw + rnorm(n = n_sample)
y_co <- 0.4 * rw + rnorm(n = n_sample)

autoplot(ts(x_co))
autoplot(ts(y_co))

# ADF検定
# 棄却できない
summary(ur.df(x_co, type="none"))
summary(ur.df(y_co, type="none"))
# 差分をとると棄却できる
summary(ur.df(diff(x_co), type="none"))
summary(ur.df(diff(y_co), type="none"))
## 原系列が非定常であり、差分系列が定常のため
## x_coもy_coも単位根過程


# 共に単位根だが、意味のある関係性を持っている(共和分関係)
df <- data.frame(
  y_co = y_co,
  x_co = x_co,
  z = x_co - (0.6/0.4)*y_co
)
ts_df <- ts(df)
autoplot(ts_df, facets = T)


# 共和分検定(phillips-Ouliaris検定)
# 帰無仮説は共和分関係がない
data_mat <- matrix(nrow = n_sample, ncol=2)
data_mat[, 1] <- y_co
data_mat[, 2] <- x_co
summary(ca.po(data_mat, demean = "none"))

# 単位根だが、共和分関係にあるデータを
# 差分とって回帰すると有意な回帰係数を得られない
y_co_diff <- diff(y_co)
x_co_diff <- diff(x_co)
mod_lm_diff_cointegrate <- lm(y_co_diff ~ x_co_diff)
summary(mod_lm_diff_cointegrate)











