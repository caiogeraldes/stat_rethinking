require(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
require(rethinking)

data("Kline2")
data("islandsDistMatrix")
d <- tibble(Kline2)

d

d$contact_id <- ifelse(d$contact == "high", 2, 1)

dat <- list(
    T = d$total_tools,
    P = d$population,
    cid = d$contact_id,
    S = 1:10,
    D = islandsDistMatrix
)


# T_i ~ Poisson(lambda_i)
# lambda_i = α[C_i]*P^β[C_i] / γ

f.m1 <- alist(
      T ~ dpois(lambda),
      lambda <- exp(sA*zA[cid])*P^b[cid]/g,
      zA[cid] ~ dnorm(0,1),
      sA ~ dexp(1),
      b[cid] ~ dexp(1),
      g ~ dexp(1),
      gq> vector[cid]:a <<- zA*sA
)

m1 <- ulam(
    flist = f.m1,
    data = dat,
    cores = 4,
    chains = 4,
    log_lik = TRUE,
    file = "./models/oceanic.m1"
)

# T_i ~ Poisson(lambda_i)
# log(lambda_i) = abar + a[S_i]

f.m2 <- alist(
    T ~ poisson(lambda),
    log(lambda) <- abar + a[S],

    # transformative priors - noncentenred
    transpars> vector[10]:a <<- L_K * z,
    vector[10]:z ~ normal(0,1),
    transpars> matrix[10,10]:L_K <<- cholesky_decompose(K),
    transpars> matrix[10,10]:K <- cov_GPL2(D, etasq, rhosq, 0.01),

    # fixed prior
    abar ~ normal(3, 0.5),
    etasq ~ exponential(2),
    rhosq ~ exponential(0.5)
)

m2 <- ulam(
    flist = f.m2,
    data = dat,
    cores = 4,
    chains = 4,
    iter = 4000,
    log_lik = TRUE,
    file = "./models/oceanic.m2"
)


# repeat priors
n <- 30
etasq <- rexp(n,2)
rhosq <- rexp(n,1)
plot( NULL , xlim=c(0,7) , ylim=c(0,2) , xlab="distance (thousand km)" , ylab="covariance" )
for ( i in 1:n )
    curve( etasq[i]*exp(-rhosq[i]*x^2) , add=TRUE , lwd=4 , col=col.alpha(1,0.5) )
# post
n <- 30
post <- extract.samples(m2)
for ( i in 1:n )
    curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE , lwd=4 , col=col.alpha(2,0.5) )

### M3

# T_i ~ Poisson(lambda_i)
# lambda_i = (abar*P^β / γ)* + a[S_i]

f.m3 <- alist(
    T ~ dpois(lambda),
    lambda <- (abar*P^b/g)*exp(a[S]),

    # transformative priors - noncentenred
    transpars> vector[10]:a <<- L_K * z,
    vector[10]:z ~ normal(0,1),
    transpars> matrix[10,10]:L_K <<- cholesky_decompose(K),
    transpars> matrix[10,10]:K <- cov_GPL2(D, etasq, rhosq, 0.01),

    # fixed prior
    c(abar, b, g) ~ dexp(1),
    etasq ~ exponential(2),
    rhosq ~ exponential(0.5)
)

m3 <- ulam(
    flist = f.m3,
    data = dat,
    cores = 4,
    chains = 4,
    iter = 4000,
    log_lik = TRUE,
    file = "./models/oceanic.m3"
)

### M4


# T_i ~ Poisson(lambda_i)
# lambda_i = (α[C_i]*P^β[C_i] / γ)* + a[S_i]
f.m4 <- alist(
    T ~ dpois(lambda),
    lambda <- (abar[cid]*P^b[cid]/g)*exp(a[S]),

    # transformative priors - noncentenred
    transpars> vector[10]:a <<- L_K * z,
    vector[10]:z ~ normal(0,1),
    transpars> matrix[10,10]:L_K <<- cholesky_decompose(K),
    transpars> matrix[10,10]:K <- cov_GPL2(D, etasq, rhosq, 0.01),

    # fixed prior
    vector[2]:abar ~ exponential(1),
    vector[2]:b ~ exponential(1),
    g ~ dexp(1),
    etasq ~ exponential(2),
    rhosq ~ exponential(0.5)
)

m4 <- ulam(
    flist = f.m4,
    data = dat,
    cores = 4,
    chains = 4,
    iter = 4000,
    log_lik = TRUE,
    file = "./models/oceanic.m4"
)

compare(m1,m2,m3,m4)


# repeat priors
n <- 40
etasq <- rexp(n,2)
rhosq <- rexp(n,1)
plot( NULL , xlim=c(0,7) , ylim=c(0,2) , xlab="distance (thousand km)" , ylab="covariance" )
for ( i in 1:n )
    curve( etasq[i]*exp(-rhosq[i]*x^2) , add=TRUE , lwd=4 , col=col.alpha(1,0.3) ) # black
# post m2
post <- extract.samples(m2)
for ( i in 1:n )
    curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE , lwd=4 , col=col.alpha(2,0.3) ) # red

# post m3
post <- extract.samples(m3)
for ( i in 1:n )
    curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE , lwd=4 , col=col.alpha(3,0.3) ) # green

# post m4
post <- extract.samples(m4)
for ( i in 1:n )
    curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE , lwd=4 , col=col.alpha(4,0.5) ) # blue


post.df <- tibble(
    etasq = post$etasq[1:30],
    rhosq = post$rhosq[1:30],
    )

post.df %>%
  ggplot() +
  xlim(0, 7) +
  geom_function(fun= function(x) etasq*exp(-rhosq*x^2))
