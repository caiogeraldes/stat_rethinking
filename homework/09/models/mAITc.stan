data{
    int Nh;
    int S[2314];
    vector[2314] A;
    int I[2314];
    real Tbar;
    vector[2314] T;
}
parameters{
    vector[Nh] zA;
    real abar;
    real zB;
    real g;
    real<lower=0> sigma_T;
    real<lower=0> sA;
    real<lower=0> sB;
}
model{
    vector[2314] p;
    sB ~ exponential( 1 );
    sA ~ exponential( 1 );
    sigma_T ~ exponential( 1 );
    T ~ normal( Tbar , sigma_T );
    g ~ normal( 0 , 1 );
    zB ~ normal( 0 , 1 );
    abar ~ normal( 0 , 0.1 );
    zA ~ normal( 0 , 1 );
    for ( i in 1:2314 ) {
        p[i] = abar + zA[I[i]] * sA + zB * sB * A[i]^2 + g * T[i];
        p[i] = inv_logit(p[i]);
    }
    S ~ bernoulli( p );
}
generated quantities{
    vector[Nh] a;
    real b;
    b = zB * sB;
    a = abar + zA * sA;
}


