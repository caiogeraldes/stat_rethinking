require(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(cmdstanr)
library(rethinking)


data('Kline')

d <- Kline
d$contact_id <- ifelse(d$contact == "high", 2, 1)

dat <- list(
            T = d$total_tools,
            P = d$population,
            cid = d$contact_id
)

# λ = α*P^β / γ

m1 <- ulam(
          alist(
                T ~ dpois(lambda),
                lambda <- exp(sA*zA[cid])*P^b[cid]/g,
                zA[cid] ~ dnorm(0,1),
                sA ~ dexp(1),
                b[cid] ~ dexp(1),
                g ~ dexp(1),
                gq> vector[cid]:a <<- zA*sA
                ),
          data = dat, cores = 6, chains = 6,
          log_lik = TRUE
)

precis(m1, depth=2)
trankplot(m1)
dashboard(m1)

# interaction model
m2 <- ulam(
          alist(
          T ~ dpois( lambda ),
          log(lambda) <- a[cid] + b[cid]*log(P),
          a[cid] ~ dnorm(3 , 0.5),
          b[cid] ~ dnorm(0 , 0.2)
          ),
         data=dat, chains=6, cores=6, log_lik=TRUE
)

precis(m2, depth=2)
trankplot(m2)
dashboard(m2)

# interaction model
m3 <- ulam(
          alist(
          T ~ dpois( lambda ),
          log(lambda) <- a_bar + zA[cid]*sA + zB[cid]*sB*log(P),
          zA[cid] ~ dnorm(0 , 1),
          sA ~ dexp(1),
          a_bar ~ dnorm(0, 1.5),
          zB[cid] ~ dnorm(0 , 1),
          sB ~ dexp(1),
          gq> vector[cid]:a <<- a_bar + zA*sA,
          gq> vector[cid]:b <<- zB*sB
          ),
         data=dat, chains=6, cores=6, log_lik=TRUE
)

precis(m3, depth=2)
trankplot(m3)
dashboard(m3)

# interaction model
m4 <- ulam(
          alist(
          T ~ dpois( lambda ),
          log(lambda) <- a_bar + zA[cid]*sA + zB[cid]*sB*log(P),
          zA[cid] ~ dnorm(0 , 1),
          sA ~ dexp(1),
          a_bar ~ dlnorm(3, 0.5),
          zB[cid] ~ dnorm(0 , 1),
          sB ~ dexp(1),
          gq> vector[cid]:a <<- a_bar + zA*sA,
          gq> vector[cid]:b <<- zB*sB
          ),
         data=dat, chains=6, cores=6, log_lik=TRUE
)

precis(m4, depth=2)
trankplot(m4)
dashboard(m4)

# Make Oceanic plot
oceanic_plot <- function(m, dat) {
  k <- PSIS(m, pointwise=TRUE)$k
  ns <- 100
  P_seq <- seq(from = 1000, to= 280000, length.out=ns)
  lambda1 = link(m, data=data.frame(P=P_seq, cid=1))
  contact1.df <- tibble(
                        P = round(P_seq),
                        lmu = apply(lambda1, 2, mean),
                        lcimin = apply(lambda1, 2, PI)[1,],
                        lcimax = apply(lambda1, 2, PI)[2,]
  )
  lambda2 = link(m, data=data.frame(P=P_seq, cid=2))
  contact2.df <- tibble(
                        P = round(P_seq),
                        lmu = apply(lambda2, 2, mean),
                        lcimin = apply(lambda2, 2, PI)[1,],
                        lcimax = apply(lambda2, 2, PI)[2,]
  )

  plt <- dat %>%
    as_tibble() %>%
    mutate(culture = d$culture, contact = d$contact) %>%
    ggplot() +
    geom_ribbon(aes(x=P, ymin=lcimin, ymax=lcimax), data=contact1.df, alpha=0.2) +
    geom_ribbon(aes(x=P, ymin=lcimin, ymax=lcimax), data=contact2.df, alpha=0.2) +
    geom_line(aes(x=P_seq, y=lmu), color=1, linetype=1, data=contact1.df) +
    geom_line(aes(x=P_seq, y=lmu), color=1, linetype=2, data=contact2.df) +
    geom_point(aes(x=P, y=T, color=culture, shape=contact),size=6) +
    ylab("tools") +
    xlab("population (log10 scale)") +
    scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +#, trans='log10', n.breaks=10) +
    scale_y_continuous(limits=c(0,130), oob=scales::squish)
  return(plt)
}


oceanic_plot(m1)
oceanic_plot(m2)
oceanic_plot(m3)
oceanic_plot(m4)
