library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)
library(Metrics)
rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

### データの準備
AirPassengers

length(window(AirPassengers, start=c(1959, 1)))
# 学習データの長さ
T = length(window(AirPassengers, end=c(1958, 12)))
# テストデータの長さ
T_pred = length(window(AirPassengers, start=c(1959, 1)))

data_list <- list(
  y = window(AirPassengers, end=c(1958, 12)),
  T = T,
  T_pred = T_pred
)
data_list


### 学習
basic_structual <- stan(
  file = "../Stan/RegTrendSeason.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6,
  control = list(adapt_delta=0.97, max_treedepth=15)
)

### 評価

# MCMCサンプルの代表値の計算
print(basic_structual, probs = c(0.025, 0.5, 0.975))
# 指定のパラメータについてみたい場合
print(basic_structual, pars = c("s_z", "s_v", "s_s"), probs = c(0.025, 0.5, 0.975))
print(basic_structual, pars = c("mu_pred"), probs = c(0.025, 0.5, 0.975))

# MCMCサンプルを取り出す
mcmc_sample <- rstan::extract(basic_structual, permuted=FALSE)

# 事後分布とトレースプロットの可視化
mcmc_combo(mcmc_sample, pars=c("s_z", "s_v", "s_s"))

# インデックスの作成
#date_plot <- seq(from = as.POSIXct("1949-01-01"), by = "months", len = length(AirPassengers))
#mcmc_sample_pred <- rstan::extract(basic_structual)
#plotSSM(mcmc_sample = mcmc_sample_pred, time_vec = date_plot, state_name = "y_pred", graph_title = "AirPassengers", y_label = "AirPassengers")

# 予測のプロット
y = AirPassengers[(T-T_pred+1):T]
y_pred = rep(c(0), T_pred)
for(i in 1:T_pred){
  # 各時点のMCMCサンプルの平均値を出す
  y_pred[i] = mean(mcmc_sample_pred$y_pred[, T+i])
}

# 時系列プロット
plot(y,lwd=2,type="l",col="red")
par(new=T)
plot(y_pred,lwd=2,type="l",col="blue",axes=F)
axis(4)

# RMSE, MAE計算
rmse(y, y_pred)
mae(y, y_pred)











