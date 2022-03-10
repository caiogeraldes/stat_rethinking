data{
    int Nh;
    int S[14364];
    vector[14364] A;
    int I[14364];
}
parameters{
    vector[Nh] zA;
    real zB;
    real abar;
    real<lower=0> sA;
    real<lower=0> sB;
}
model{
    vector[14364] p;
    sB ~ exponential( 1 );
    sA ~ exponential( 1 );
    abar ~ normal( 0 , 0.1 );
    zB ~ normal( 0 , 1 );
    zA ~ normal( 0 , 1 );
    for ( i in 1:14364 ) {
        p[i] = abar + zA[I[i]] * sA + zB * sB * A[i]^2;
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


