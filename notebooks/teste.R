require(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(cmdstanr)
library(rethinking)

data('foxes')
d <- foxes

dat <- list(
            F = standardize(d$avgfood),
            W = standardize(d$weight),
            G = d$group
)

m1 <- ulam(
           alist(
                 W ~ dlnorm(mu, sigma),
                 mu <- a + b*F,
                 a ~ dlnorm(0,1),
                 b ~ dnorm(0,1),
                 sigma ~ dexp(1)
                 ),
           data=dat,
           cores = 6, chains=6,
           log_lik = TRUE,
           file = "./models/m1" # saves a loadable model fit into ./models/
)

writeLines(stancode(m1), "./models/m1.stan")

m2 <- ulam(
           alist(
                 W ~ dlnorm(mu, sigma),
                 mu <- a[G] + b[G]*F,
                 a[G] ~ dlnorm(0,1),
                 b[G] ~ dnorm(0,1),
                 sigma ~ dexp(1)
                 ),
           data=dat,
           cores = 6, chains=6,
           log_lik = TRUE,
           file = "./models/m2" # saves a loadable model fit into ./models/
)

writeLines(stancode(m2), "./models/m2.stan")

compare(m1,m2)
