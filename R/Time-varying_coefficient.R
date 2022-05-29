# 時変係数モデル
# 売り上げ ~ 宣伝
# 宣伝の効果(係数)が徐々に小さくなることを想定

library(rstan)
library(brms)
library(bayesplot)
library(ggfortify)
library(gridExtra)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

df <- read.csv("5-4-1-sales-ts-2.csv")
df$date <- as.POSIXct(df$date)
head(df)
plot(density(df$sales))

T = 90 # 学習データ数
T_pred = 10 # テストデータ数

data_list <- list(
  y = df$sales[1:90],
  ex = df$publicity[1:90],
  T = T,
  T_pred = T_pred,
  ex_test = df$publicity[91:100]
)

### 学習
model <- stan(
   file = "../Stan/Time-varying_coefficient.stan",
   data = data_list,
   seed = 1,
   iter = 8000,
   warmup = 2000,
   thin = 6
)

### 評価
# MCMCサンプルの代表値の計算
print(model, probs = c(0.025, 0.5, 0.975))
# 指定のパラメータについてみたい場合
print(model, pars = c("s_w", "s_t", "s_v"), probs = c(0.025, 0.5, 0.975))
print(model, pars = c("mu"), probs = c(0.025, 0.5, 0.975))
print(model, pars = c("b"), probs = c(0.025, 0.5, 0.975))


# MCMCサンプルを取り出す
mcmc_sample <- rstan::extract(model, permuted=FALSE)
# 事後分布とトレースプロットの可視化
mcmc_combo(mcmc_sample, pars=c("s_w", "s_t", "s_v"))

# 予測のプロット
mcmc_sample_pred <- rstan::extract(model)
y = df$sales[(T-T_pred+1):T] # 正解
y_pred = rep(c(0), T_pred) # 予測
for(i in 1:T_pred){
  # 各時点のMCMCサンプルの平均値を出す
  y_pred[i] = mean(mcmc_sample_pred$y_pred[, T+i])
}

# y_predの時系列プロット
plot(y,lwd=2,type="l",col="red")
par(new=T)
plot(y_pred,lwd=2,type="l",col="blue",axes=F)
axis(4)









