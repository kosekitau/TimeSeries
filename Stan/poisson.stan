data{
  int N; // 学習データのサイズ
  int bb_count[N];
  vector[N] high_t;
  vector[N] low_t;
  vector[N] precip;
  int N_pred; // 予測したいデータのサイズ
  vector[N_pred] high_t_pred;
  vector[N_pred] low_t_pred;
  vector[N_pred] precip_pred;
}
parameters{
  real Intercept;
  real b_high_t;
  real b_low_t;
  real b_precip;
}
model{
  vector[N] lambda = exp(Intercept + b_high_t*high_t + b_low_t*low_t + b_precip*precip);
  bb_count ~ poisson(lambda);
}
generated quantities{
  vector[N_pred] lambda_pred;
  vector[N_pred] bb_count_pred;
  for(i in 1:N_pred){
    lambda_pred[i] = exp(Intercept + b_high_t*high_t_pred[i] + b_low_t*low_t_pred[i] + b_precip*precip_pred[i]);
    bb_count_pred[i] = poisson_rng(lambda_pred[i]);
  }
}
