data{
  int T;
  vector[T] ex;
  vector[T] y;
  int T_pred;
  vector[T_pred] ex_test;
}
parameters{
  vector[T] mu;
  vector[T] b;
  real<lower=0> s_w; // 状態誤差
  real<lower=0> s_t;
  real<lower=0> s_v;
}
transformed parameters{
  vector[T] alpha;
  for(i in 1:T){
    // 状態はランダムウォーク+回帰成分で決まると仮定
    alpha[i] = mu[i] + b[i] * ex[i];
  }
}
model{
  // 切片も係数もランダムウォークと仮定
  for(i in 2:T){
    mu[i] ~ normal(mu[i-1], s_w); // mu[i]=mu[i-1]+s_w    s_w~N(0, s_w)
    b[i] ~ normal(b[i-1], s_t); // bu[i]=bu[i-1]+s_t     s_t~N(0, s_t)
  }
  for(i in 1:T){
    y[i] ~ normal(alpha[i], s_v);
  }
}
generated quantities{
  vector[T+T_pred] mu_pred;
  vector[T+T_pred] b_pred;
  vector[T+T_pred] alpha_pred;
  vector[T+T_pred] y_pred;
  
  mu_pred[1:T] = mu;
  b_pred[1:T] = b;
  alpha_pred[1:T] = alpha;
  y_pred[1:T] = y;
  
  for(i in 1:T_pred){
    mu_pred[T+i] = normal_rng(mu_pred[T+i-1], s_w);
    b_pred[T+i] = normal_rng(b_pred[T+i-1], s_t);
  }
  for(i in 1:T_pred){
    alpha_pred[T+i] = mu_pred[T+i] + b_pred[T+i]*ex_test[i];
  }
  for(i in 1:T_pred){
    y_pred[T+i] = normal_rng(alpha_pred[T+i], s_v);
  }
}
