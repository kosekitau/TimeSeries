library(rstan)
library(brms)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

temp_power <- read.csv("../data/temp_power.csv")

simple_lm_brms <- brm(
  formula = actual ~ max,
  family = gaussian(link = "identity"),
  data = temp_power,
  seed = 1
)
simple_lm_brms

# 切片や係数の事後分布を表示
plot(simple_lm_brms)

# 各係数の95%ベイズ信用区間を表示
stanplot(
  simple_lm_brms,
  type="intervals",
  pars="b_Intercept",
  prob=0.8,
  prob_outer=0.95
)
stanplot(
  simple_lm_brms,
  type="intervals",
  pars="b_max",
  prob=0.8,
  prob_outer=0.95
)

# 30度の時のactualを予測する
new_data <- data.frame(max=30)
# 予測値の95%ベイズ信用区間
fitted(simple_lm_brms, new_data)
set.seed(1)
predict(simple_lm_brms, new_data)

# 95%ベイズ信用区間付きのグラフ
eff <- marginal_effects(simple_lm_brms)
plot(eff, points=TRUE)

# 95%ベイズ予測区間付きのグラフ
set.seed(1)
eff_pre <- marginal_effects(simple_lm_brms, method="predict")
plot(eff_pre, points=TRUE)









