functions{


    vector merge_missing( int[] miss_indexes , vector x_obs , vector x_miss ) {
        int N = dims(x_obs)[1];
        int N_miss = dims(x_miss)[1];
        vector[N] merged;
        merged = x_obs;
        for ( i in 1:N_miss )
            merged[ miss_indexes[i] ] = x_miss[i];
        return merged;
    }
}
data{
    int Nh;
    int S[14364];
    vector[14364] A;
    int I[14364];
    vector[14364] T;
    int T_missidx[12050];
    real Tbar;
}
parameters{
    vector[Nh] zA;
    real abar;
    real zB;
    real g;
    real mu_T;
    real<lower=0> sigma_T;
    real<lower=0> sA;
    real<lower=0> sB;
    vector[12050] T_impute;
}
model{
    vector[14364] p;
    vector[14364] T_merge;
    sB ~ exponential( 1 );
    sA ~ exponential( 1 );
    sigma_T ~ exponential( 1 );
    mu_T ~ normal( Tbar , 1 );
    T_merge = merge_missing(T_missidx, to_vector(T), T_impute);
    T_merge ~ normal( mu_T , sigma_T );
    g ~ normal( 0 , 1 );
    zB ~ normal( 0 , 1 );
    abar ~ normal( 0 , 0.1 );
    zA ~ normal( 0 , 1 );
    for ( i in 1:14364 ) {
        p[i] = abar + zA[I[i]] * sA + zB * sB * A[i]^2 + g * T_merge[i];
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


