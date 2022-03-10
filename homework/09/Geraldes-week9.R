require(tidyverse)
require(rethinking)
require(dagitty)
require(kableExtra)

data(Achehunting)
d <- tibble(Achehunting)
d$S <- ifelse(d$kg.meat > 0, 1, 0)
d$A <- standardize(d$age)
d$T <- standardize(d$hours)

dat <- list(
  Nh = length(unique(d$id)),
  S = d$S,
  A = d$A
)

# EXERCICE 1

f.mA <- alist(
  S ~ bernoulli(p),
  logit(p) <- a + b*A^2,
  a ~ normal(0, 0.1),
  b ~ normal(0, 0.05)
)

mA <- ulam( flist = f.mA, dat = dat, cores = 4, chains = 4,  iter = 2000, file = "./models/mA")
write_lines(stancode(mA), "./models/mA.stan")
beepr::beep(3)

precis(mA)
#         mean         sd       5.5%      94.5%    n_eff    Rhat4
# a  0.1371577 0.02232976  0.1009685  0.1736372 1380.094 1.001854
# b -0.2351356 0.01524043 -0.2594043 -0.2106380 1485.530 1.000031

# EXERCICE 2

dat <- list(
  Nh = length(unique(d$id)),
  I = as.factor(d$id),
  S = d$S,
  A = d$A
)

f.mAI <- alist(
  S ~ bernoulli(p),
  logit(p) <- abar + zA[I]*sA + zB*sB*A^2,
  vector[Nh]:zA ~ normal(0, 1),
  zB ~ normal(0, 1),
  abar ~ normal(0, 0.1),
  sA ~ exponential(1),
  sB ~ exponential(1),
  gq> vector[Nh]:a <<- abar + zA*sA,
  gq> real[1]:b <<- zB*sB
)

mAI <- ulam( flist = f.mAI, dat = dat, cores = 4, chains = 4,  iter = 2000, file = "./models/mAI")
write_lines(stancode(mAI), "./models/mAI.stan")
beepr::beep(3)

precis(mAI, pars=c("abar", "b", "sA", "sB"))
#             mean         sd        5.5%      94.5%     n_eff    Rhat4
# abar  0.01441468 0.05768174 -0.07571847  0.1057518  898.4824 1.001905
# b    -0.28926879 0.02240940 -0.32476412 -0.2537649 3728.3862 1.000386
# sA    0.57322113 0.05804444  0.48758282  0.6734943  805.6828 1.000326
# sB    0.66486412 0.48710636  0.18359077  1.6266105 2285.0854 1.003606

# EXERCICE 3

dat.inc <- list(
  Nh = length(unique(d$id)),
  I = as.factor(d$id),
  T = d$hours,
  Tbar = mean(d$hours, na.rm = TRUE),
  S = d$S,
  A = d$A
)

d.cmp <- d %>% dplyr::filter(datatype == 1)

dat.cmp <- list(
  Nh = length(unique(d.cmp$id)),
  I = as.factor(d.cmp$id),
  T = d.cmp$hours,
  Tbar = mean(d.cmp$hours),
  S = d.cmp$S,
  A = d.cmp$A
)

f.mAITc <- alist(
  S ~ bernoulli(p),
  logit(p) <- abar + zA[I]*sA + zB*sB*A^2 + g*T,
  vector[Nh]:zA ~ normal(0, 1),
  abar ~ normal(0, 0.1),
  zB ~ normal(0, 1),
  g ~ normal(0,1),
  T ~ normal(Tbar, sigma_T),
  sigma_T ~ exponential(1),
  sA ~ exponential(1),
  sB ~ exponential(1),
  gq> vector[Nh]:a <<- abar + zA*sA,
  gq> real[1]:b <<- zB*sB
)

mAITc <- ulam(flist = f.mAITc, data=dat.cmp, chains = 4, cores = 4, iter = 2000, file = "./models/mAITc")
write_lines(stancode(mAITc), "./models/mAITc.stan")
beepr::beep(3)

precis(mAITc, pars=c('abar', 'b', 'g', 'sA', 'sB', 'sigma_T'))
#                mean         sd       5.5%      94.5%     n_eff     Rhat4
# abar    -0.14938595 0.08576928 -0.2852303 -0.0111007 3942.8947 0.9998280
# b       -0.43485848 0.03848641 -0.4957938 -0.3722311 3931.1464 0.9999548
# g        0.09477731 0.01503317  0.0705393  0.1187461 3013.7018 0.9993810
# sA       0.55662314 0.07574098  0.4403638  0.6768594 1458.9934 1.0010211
# sB       0.86298567 0.66413824  0.2591060  2.1704082  974.5472 1.0024236
# sigma_T  2.19351084 0.03218709  2.1426895  2.2458888 5461.0229 0.9996389


f.mAITi <- alist(
  S ~ bernoulli(p),
  logit(p) <- abar + zA[I]*sA + zB*sB*A^2 + g*T,
  vector[Nh]:zA ~ normal(0, 1),
  abar ~ normal(0, 0.1),
  zB ~ normal(0, 1),
  g ~ normal(0,1),
  T ~ normal(mu_T, sigma_T),
  mu_T ~ normal(Tbar, 1),
  sigma_T ~ exponential(1),
  sA ~ exponential(1),
  sB ~ exponential(1),
  gq> vector[Nh]:a <<- abar + zA*sA,
  gq> real[1]:b <<- zB*sB
)

mAITi <- ulam(flist = f.mAITi, data=dat.inc, chains = 4, cores = 4, iter = 2000, file = "./models/mAITi")
write_lines(stancode(mAITi), "./models/mAITi.stan")
beepr::beep(3)

precis(mAITi, pars=c('abar', 'b', 'g', 'mu_T', 'sA', 'sB', 'sigma_T'))
#                mean         sd        5.5%       94.5%     n_eff    Rhat4
# abar    -0.25440080 0.08416436 -0.39105277 -0.12107320 1907.3406 1.000657
# b       -0.31158738 0.02366889 -0.34930354 -0.27509941 3531.0733 1.000362
# g        0.05909887 0.01313761  0.03868097  0.08030076 1078.6988 1.002012
# mu_T     6.83155892 0.04643808  6.75676000  6.90399220  853.7891 1.003270
# sA       0.57203169 0.05905451  0.48297537  0.67312269  758.0940 1.004624
# sB       0.69653524 0.55426154  0.18785232  1.75762715 2976.3453 1.002058
# sigma_T  2.19342142 0.03190784  2.14291890  2.24582660  498.2330 1.006443


ps.mAI <- extract.samples(mAI)

sigmas <- tibble(
  sigmaA = ps.mAI$sA,
  sigmaB = ps.mAI$sB
  )

splot<- sigmas %>%
  ggplot(aes()) +
  geom_density(aes(x=sigmaA, color="α")) +
  geom_density(aes(x=sigmaB, color="β")) +
  xlab("posterior distribution of σ") +
  scale_color_discrete("Parameter")
ggsave("./plots/sigma.posterior.mAI.png", splot, dpi=300, width=1500, height=1000, units="px")

alpha <- ps.mAI$a
dimnames(alpha)[[2]] <- levels(dat$I)

as_tibble(alpha) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x=value, color=name)) +
  geom_density()

alphaH.mean <- apply(alpha,2,mean)
alphaH.hpdi <- apply(alpha,2,HPDI)

d$I <- as.factor(d$id)

obs.mean = (d %>% group_by(I) %>% mutate(S.mean = mean(S)) %>% select(I, S.mean) %>% distinct())$S.mean
n = (d %>% group_by(I) %>% count())$n


tibble(I = dimnames(alpha)[[2]], mean = alphaH.mean, lhpdi = alphaH.hpdi[1,], hhpdi = alphaH.hpdi[2,], obs.mean=obs.mean, n=n) %>%
  ggplot(aes(y=mean, x=reorder(I, mean))) +
  geom_pointrange(aes(ymax=hhpdi, ymin=lhpdi), color=2) +
  geom_point(aes(y=obs.mean, size=n), shape=21)


beta <- ps.mAI$b

as_tibble(beta) %>%
  ggplot(aes(x=value)) +
  geom_density(color=2)

tmp_link <- function(model) {
  ps <- extract.samples(model)
  a = ps$abar
  b = ps$b
  n = length(b)
  p <- matrix(nrow=n, ncol=100)
  for (i in 1:100) for (j in 1:n) {
    p[j,i] <- inv_logit(a[j] + b[j]*seq(-3, 3, length.out=100)[i]^2)
  }
  return(p)
}

p <- tmp_link(mAI)
p.mean <- apply(p,2,mean)
p.hpdi <- apply(p,2,HPDI)

age.mean <- d %>% group_by(A) %>% mutate(smean = mean(S))
age.mean <- age.mean %>% select(A, smean) %>% distinct()
age.mean$n <- (d %>% group_by(A) %>% count())$n

tibble(
  A = seq(-3,3, length.out = 100),
  mean = p.mean,
  min = p.hpdi[1,],
  max = p.hpdi[2,]) %>%
  ggplot(aes(x=A, y=mean))+
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2) +
  geom_line(col=2, size=2) +
  geom_point(aes(y=smean, size=n), shape=21, data=age.mean, show.legend = FALSE) +
  xlab("Age (z-scores)") +
  ylab("Avarage probability of success by age (prob)")
ggsave("./plots/p.posterior.mAI.png", dpi=300, width=2500, height=1500, units="px")

p <- link(mAI)
p.mean <- apply(p,2,mean)
p.hpdi <- apply(p,2,HPDI)

age.id.mean = (d %>% group_by(A,I) %>% mutate(s.mean=mean(S)))$s.mean

set.seed(10)
tibble(
  I = d$I,
  A = d$A,
  p.mean = p.mean,
  p.max = p.hpdi[1,],
  p.min = p.hpdi[2,],
  s.mean = age.id.mean
  ) %>%
  dplyr::filter(I %in% sample(d$I, 10)) %>%
  ggplot(aes(x=A)) +
  geom_ribbon(aes(ymin=p.min, ymax=p.max, fill=I), alpha=0.2) +
  geom_line(aes(y=p.mean, color=I)) +
  geom_point(aes(y=s.mean, color=I), size=2)
ggsave("./plots/p.posterior.mAI.2.png", dpi=300, width=2500, height=1500, units="px")


tmp_link <- function(model) {
  ps <- extract.samples(model)
  a = ps$a
  dimnames(a)[[2]] <- levels(d$I)
  aH <- a[,"3131"]
  min = d %>% dplyr::filter(I == 3131) %>% select(A) %>% min()
  max = d %>% dplyr::filter(I == 3131) %>% select(A) %>% max()
  b = ps$b
  n = length(b)
  p <- matrix(nrow=n, ncol=100)
  for (i in 1:100) for (j in 1:n) {
    p[j,i] <- inv_logit(aH[j] + b[j]*seq(min, max, length.out=100)[i]^2)
  }
  return(p)
}

p <- tmp_link(mAI)

p.mean <- apply(p,2,mean)
p.hpdi <- apply(p,2,HPDI)

age.mean <- d %>% dplyr::filter(I == 3131) %>% group_by(A) %>% mutate(smean = mean(S))
age.mean <- age.mean %>% select(A, smean) %>% distinct()

min = d %>% dplyr::filter(I == 3131) %>% select(A) %>% min()
max = d %>% dplyr::filter(I == 3131) %>% select(A) %>% max()

tibble(
  A = seq(min, max, length.out = 100),
  mean = p.mean,
  min = p.hpdi[1,],
  max = p.hpdi[2,]) %>%
  ggplot(aes(x=A, y=mean))+
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2) +
  geom_line(col=2, size=2) +
  geom_point(aes(y=smean), shape=21, data=age.mean, show.legend = FALSE) +
  xlab("Age (z-scores)") +
  ylab("Avarage probability of success by age (prob)")
ggsave("./plots/p.posterior.mAI.3.png", dpi=300, width=2500, height=1500, units="px")

a <- inv_logit(ps.mAI$a)

hpdi.50 <- function(x) {
  return(HPDI(x, prob=.50))
}

a.mean <- apply(a, 2, mean)
a.hpdi <- apply(a, 2, hpdi.50)

obs.mean <- (d %>% group_by(I) %>% mutate(s.mean = mean(S)) %>% select(I, s.mean) %>% distinct())$s.mean
n = (d %>% group_by(I) %>% count())$n

tibble(
  I = levels(d$I),
  mean = a.mean,
  obs.mean = obs.mean,
  max = a.hpdi[2,],
  min = a.hpdi[1,],
  n=n
) %>%
  ggplot(aes(x=reorder(as.factor(I), mean))) +
  geom_pointrange(aes(y=mean, ymin=min, ymax=max), color=2) +
  geom_point(aes(y=obs.mean, size=n), shape=21, show.legend = FALSE) +
  xlab("Individual id (sorted by predicted mean)") +
  ylab("Avarage probability of success by individual (prob)") +theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
ggsave("./plots/p.posterior.mAI.4.png", dpi=300, width=3000, height=1500, units="px")


ps.mAITi <- extract.samples(mAITi)

