# MCMCで単回帰モデルのパラメータ推定

library(rstan)
library(bayesplot)
library(ggplot2)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())


temp_power <- read.csv("../data/temp_power.csv")
max(temp_power$max) # 35.4
min(temp_power$min) # -1.4
# カーネル密度推定
plot(density(temp_power$actual))
# 散布図
A <- data.frame(
  actual = temp_power[, "actual"],
  max_temp = temp_power[, "max"]
)
ggplot(A, aes(x = max_temp, y = actual)) + geom_point()

# 気温が11~30度に変化した際の消費量をみる
temperature_pred <- 11:30

data_list_pred <- list(
  N = nrow(temp_power),
  actual = temp_power$actual,
  temperature = temp_power$max, # 最高気温
  N_pred = length(temperature_pred),
  temperature_pred = temperature_pred
)

mcmc_result_pred <- stan(
  file = "../data/Stan/simple_lm_mcmc.stan",
  data = data_list_pred,
  seed = 1
)

print(mcmc_result_pred, probs=c(0.025, 0.5, 0.975))

dim(rstan::extract(mcmc_result_pred)$mu_pred) # (4000, 20)
dim(rstan::extract(mcmc_result_pred)$actual_pred) # (4000, 20)

dim(rstan::extract(mcmc_result_pred)$Intercept) # (4000, )

its = rstan::extract(mcmc_result_pred)$Intercept
bt = rstan::extract(mcmc_result_pred)$beta
its + bt*30
rstan::extract(mcmc_result_pred)$mu_pred[, 20]# 気温30度の時
# 上2つの結果は一致する

rstan::extract(mcmc_result_pred)$mu_pred[, 20]
rstan::extract(mcmc_result_pred)$actual_pred[, 20]

# mcmcサンプル抽出
mcmc_sample_pred <- rstan::extract(mcmc_result_pred, permuted=FALSE)

# Interceptなどの事後分布
mcmc_combo(
  mcmc_sample_pred, 
  pars = c("Intercept", "beta", "sigma")
)

# actual_predの95%ベイズ予測区間を出す
mcmc_intervals(
  mcmc_sample_pred,
  regex_pars = c("actual_pred."),
  prob = 0.8,
  prob_outer = 0.95
)

# 11度と30度の予測分布の比較
mcmc_areas(
  mcmc_sample_pred,
  pars = c("actual_pred[1]", "actual_pred[20]"),
  prob = 0.6,
  prob_outer = 0.99
)

# 予測結果の取り出し
actual_pred <- rstan::extract(mcmc_result_pred)$actual_pred
dim(actual_pred)
# 各気温の1番目のMCMCサンプル
hist(actual_pred[1,])
# 各気温の2000番目のMCMCサンプル
hist(actual_pred[2000,])
# 各気温の4000番目のMCMCサンプル
hist(actual_pred[4000,])









