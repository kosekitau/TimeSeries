data{
  int T;
  vector[T] y;
  int T_pred;
}
parameters{
  vector[T] mu;
  vector[T] gamma;
  real<lower=0> s_z;
  real<lower=0> s_v;
  real<lower=0> s_s;
}
transformed parameters{
  vector[T] alpha;
  for(i in 1:T){
    // トレンド成分 + 季節成分
    alpha[i] = mu[i] + gamma[i];
  }
}
model{
  // 平滑化トレンド部分
  for(i in 3:T){
    // (mu[i] - mu[i-1]) - (mu[i-1] - mu[i-2]) ~ N(0, s_z)
    mu[i] ~ normal(2*mu[i-1]-mu[i-2], s_z);
  }
  // 季節成分
  for(i in 12:T){
    // sum(gamma[(i-7):(i-1)]) ~ N(0, s_s)
    gamma[i] ~ normal(-sum(gamma[(i-11):(i-1)]), s_s);
  }
  for(i in 1:T){
    // トレンド成分 + 季節成分 + 観測誤差
    y[i] ~ normal(alpha[i], s_v);
  }
  
}
generated quantities{
  vector[T+T_pred] mu_pred;
  vector[T+T_pred] gamma_pred;
  vector[T+T_pred] alpha_pred;
  vector[T+T_pred] y_pred;
  
  mu_pred[1:T] = mu;
  gamma_pred[1:T] = gamma;
  alpha_pred[1:T] = alpha;
  y_pred[1:T] = y;
  
  for(i in 1:T_pred){
    mu_pred[T+i] = normal_rng(2*mu_pred[T+i-1]-mu_pred[T+i-2], s_z);
  }
  for(i in 1:T_pred){
    gamma_pred[T+i] = normal_rng(-sum(gamma_pred[(T+i-11):(T+i-1)]), s_s);
  }
  for(i in 1:T_pred){
    alpha_pred[T+i] = mu_pred[T+i] + gamma_pred[T+i];
  }
  for(i in 1:T_pred){
    y_pred[T+i] = normal_rng(alpha_pred[T+i], s_v);
  }
}
