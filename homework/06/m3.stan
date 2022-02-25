data {
  array[48] int D;
  array[48] int Su;
  vector[48] stdD;
  array[48] int S;
  array[48] int P;
  array[48] int T;
}
parameters {
  vector[48] zA;
  matrix[2, 2] zB;
  vector[2] zG;
  real a_bar;
  real<lower=0> sA;
  real<lower=0> sB;
  real<lower=0> sG;
}
model {
  vector[48] p;
  sG ~ exponential(5);
  sB ~ exponential(1);
  sA ~ exponential(1);
  a_bar ~ normal(0, 1.5);
  zG ~ normal(0, 1);
  to_vector(zB) ~ normal(0, 1);
  zA ~ normal(0, 1);
  for (i in 1 : 48) {
    p[i] = a_bar + (zA[T[i]] * sA) + (zB[P[i], S[i]] * sB)
           + ((zG[P[i]] * sG) * stdD[i]);
    p[i] = inv_logit(p[i]);
  }
  Su ~ binomial(D, p);
}
generated quantities {
  vector[48] log_lik;
  vector[48] p;
  vector[48] a;
  matrix[2, 2] b;
  vector[2] g;
  g = zG * sG;
  b = zB * sB;
  a = a_bar + zA * sA;
  for (i in 1 : 48) {
    p[i] = a_bar + (zA[T[i]] * sA) + (zB[P[i], S[i]] * sB)
           + ((zG[P[i]] * sG) * stdD[i]);
    p[i] = inv_logit(p[i]);
  }
  for (i in 1 : 48)
    log_lik[i] = binomial_lpmf(Su[i] | D[i], p[i]);
}
