data{
    int Nh;
    int S[14364];
    vector[14364] A;
}
parameters{
    real a;
    real b;
}
model{
    vector[14364] p;
    b ~ normal( 0 , 0.05 );
    a ~ normal( 0 , 0.1 );
    for ( i in 1:14364 ) {
        p[i] = a + b * A[i]^2;
        p[i] = inv_logit(p[i]);
    }
    S ~ bernoulli( p );
}


