data{
  int N; // サンプルサイズ
  vector[N] actual; // 消費量
  vector[N] temperature; // 最高気温
  int N_pred; // 予測したいデータのサイズ
  vector[N_pred] temperature_pred; // 予測に使う気温データ
}
parameters{
  real Intercept; // 切片
  real beta; // 気温の係数
  real<lower=0> sigma; // 消費量は標準偏差sigmaの正規分布に従う
}
model{
  for(i in 1:N){
    actual[i] ~ normal(Intercept + beta*temperature[i], sigma);
  }
}
generated quantities{
  vector[N_pred] mu_pred;
  vector[N_pred] actual_pred;
  for(i in 1:N_pred){
    // InterceptとbetaのMCMCサンプルの4000個のMCMCサンプルを使ってtemperature_pred[i]の時のmu_predを4000個得る
    mu_pred[i] = Intercept + beta*temperature_pred[i];
    // 平均mu_pred[i]に従うactual_pred[i]を得る、mu_pred[i]が4000個あるのでactual_pred[i]も4000個
    actual_pred[i] = normal_rng(mu_pred[i], sigma);
  }
}
