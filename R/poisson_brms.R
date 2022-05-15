library(rstan)
library(bayesplot)
library(ggplot2)
library(brms)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# ブルックリン橋の自転車カウントの1日あたりの合計
bicycle_cnt <- read.csv("https://gist.github.com/sachinsdate/c17931a3f000492c1c42cf78bf4ce9fe/raw/7a5131d3f02575668b3c7e8c146b6a285acd2cd7/nyc_bb_bicyclist_counts.csv")

#Data:Data
head(bicycle_cnt)
summary(bicycle_cnt) # 各データの要約
hist(bicycle_cnt$BB_COUNT) # 自転車のカウント数のヒストグラム
plot(density(bicycle_cnt$BB_COUNT)) # 自転車のカウント数のカーネル密度推定

ggplot(data = bicycle_cnt, mapping = aes(x=LOW_T, y=BB_COUNT)) + geom_point()
ggplot(data = bicycle_cnt, mapping = aes(x=HIGH_T, y=BB_COUNT)) + geom_point()
ggplot(data = bicycle_cnt, mapping = aes(x=PRECIP, y=BB_COUNT)) + geom_point()

bicycle_cnt_train <- bicycle_cnt[1:200, ] # 学習
bicycle_cnt_test <- bicycle_cnt[200:214, ] # テスト

glm_pois_brms <- brm(
  formula = BB_COUNT ~ HIGH_T + LOW_T + PRECIP,
  family = poisson(),
  data = bicycle_cnt_train,
  seed = 1,
  prior = c(set_prior("", class="Intercept")) # 切片の事前分布を無情報事前分布にする
)
glm_pois_brms

# 事後分布とトレースプロット
plot(glm_pois_brms)

# brmsでmcmcサンプルを取得する方法
mcmc_sample_pois = as.mcmc(glm_pois_brms, combine_chains=TRUE)
mcmc_sample_pois

# 当てはめを確認する
set.seed(1)
pred <- predict(glm_pois_brms, bicycle_cnt_train)
pred_bb <- pred[, 1] # 予測の期待値
hist(pred_bb)
#DataFrameに変換
bicycle_data = data.frame(time=1:nrow(bicycle_cnt_train), bb_count=bicycle_cnt_train$BB_COUNT)
pred_bicycle_data = data.frame(time=1:nrow(bicycle_cnt_train), bb_count=pred_bb)
# 時系列プロット
plot(bicycle_data,lwd=2,type="l",col="red")
par(new=T)
plot(pred_bicycle_data,lwd=2,type="l",col="blue",axes=F)
axis(4)
# RMSE
sqrt(mean((bicycle_data$bb_count - pred_bicycle_data$bb_count)^2))
# MAE
mean(abs(bicycle_data$bb_count - pred_bicycle_data$bb_count))

# テストデータで評価
set.seed(1)
pred <- predict(glm_pois_brms, bicycle_cnt_test)
pred_bb <- pred[, 1] # 予測の期待値
#DataFrameに変換
bicycle_data = data.frame(time=1:nrow(bicycle_cnt_test), bb_count=bicycle_cnt_test$BB_COUNT)
pred_bicycle_data = data.frame(time=1:nrow(bicycle_cnt_test), bb_count=pred_bb)
# 時系列プロット
plot(bicycle_data,lwd=2,type="l",col="red")
par(new=T)
plot(pred_bicycle_data,lwd=2,type="l",col="blue",axes=F)
axis(4)
# RMSE
sqrt(mean((bicycle_data$bb_count - pred_bicycle_data$bb_count)^2))
# MAE
mean(abs(bicycle_data$bb_count - pred_bicycle_data$bb_count))




# Stanをまわす場合
bicycle_cnt_train <- bicycle_cnt[1:200, ] # 学習
bicycle_cnt_test <- bicycle_cnt[200:214, ] # テスト

data_list_pred <- list(
  N = nrow(bicycle_cnt_train),
  bb_count = bicycle_cnt_train$BB_COUNT,
  high_t = bicycle_cnt_train$HIGH_T,
  low_t = bicycle_cnt_train$LOW_T,
  precip = bicycle_cnt_train$PRECIP,
  N_pred = nrow(bicycle_cnt_test),
  high_t_pred = bicycle_cnt_test$HIGH_T,
  low_t_pred = bicycle_cnt_test$LOW_T,
  precip_pred = bicycle_cnt_test$PRECIP
)

mcmc_result_pred <- stan(
  file = "../Stan/poisson.stan",
  data = data_list_pred,
  seed = 1
)
# 各パラメータの推定値など
print(mcmc_result_pred, probs=c(0.025, 0.5, 0.975))

# mcmcサンプル抽出
mcmc_sample_pred <- rstan::extract(mcmc_result_pred, permuted=FALSE)

# Interceptなどの事後分布
mcmc_combo(
  mcmc_sample_pred, 
  pars = c("Intercept", "b_high_t", "b_low_t", "b_precip")
)

# 95%ベイズ予測区間
mcmc_intervals(
  mcmc_sample_pred,
  regex_pars = c("bb_count_pred."),
  prob = 0.8,
  prob_outer = 0.95
)
bb_count_pred <- rstan::extract(mcmc_result_pred)$bb_count_pred
mean(bb_count_pred)
dim(bb_count_pred) # (2000, 15)

mcmc_result_pred








