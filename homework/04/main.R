library(tidyverse)
library(rethinking)
library(dagitty)

d <- sim_happiness(seed=1997, N_years=1000)

d2 <- d[ d$age>17 , ] # only adults
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )
d2$mid <- d2$married + 1

m6.9 <- ulam(
          alist(
              happiness ~ dnorm(mu, sigma),
              mu <- a[mid] + bA*A,
              a[mid] ~ dnorm(0,1),
              bA ~ dnorm(0,2),
              sigma ~ dexp(1)
              ),
          data=d2,
          chains=4,
          cores=4,
          log_lik=TRUE
        )

m6.10 <- ulam(
          alist(
              happiness ~ dnorm(mu, sigma),
              mu <- a + bA*A,
              a ~ dnorm(0,1),
              bA ~ dnorm(0,2),
              sigma ~ dexp(1)
              ),
          data=d2,
          chains=4,
          cores=4,
          log_lik=TRUE
        )