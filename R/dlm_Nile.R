library(dlm)
library(KFAS)
library(ggplot2)
library(ggfortify)

# モデルを指定する
build_local_level_dlm <- function(theta){
  # order=1でローカルレベルモデル
  dlmModPoly(order = 1, dV = exp(theta[1]), dW = exp(theta[2]))
} 

# とりあえずモデルを作りながら最尤法でパラメタを計算する
# パラメタ(過程誤差の分散と観測誤差の分散)
par_local_level_dlm <- dlmMLE(Nile, parm=c(1, 1), build_local_level_dlm)
# 計算したパラメタでモデルを再定義
fit_local_level_dlm <- build_local_level_dlm(par_local_level_dlm$par)

# 新しいモデルでもう一度フィルタリング
filter_local_level_dlm <- dlmFilter(Nile, fit_local_level_dlm)

# 最後にスムージング
smooth_local_level_dlm <- dlmSmooth(filter_local_level_dlm)

# フィルタリング後のフィルタ化推定量をプロット
autoplot(filter_local_level_dlm, fitted.colour="black", fitted.size=1.5)

# 平滑化状態をプロット
p_nile <- autoplot(Nile)
autoplot(smooth_local_level_dlm, fitted.colour="black", colour="black", size=1.5, p=p_nile)












