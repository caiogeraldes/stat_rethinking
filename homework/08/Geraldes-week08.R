#!/usr/bin/env Rscript

# Homework - Week 8
# author: Caio Geraldes

require(tidyverse)
require(rethinking)

monks <- read_csv("./week08_Monks.csv")

dat <- list(
    N = nrow(monks),
    D = monks$dyad_id,
    mA = monks$A,
    mB = monks$B,
    LAB = monks$like_AB,
    LBA = monks$like_BA,
    DAB = monks$dislike_AB,
    DBA = monks$dislike_BA
)


# exercice 1

f.m1 <- alist(
    LAB ~ binomial(3, pLAB),
    LBA ~ binomial(3, pLBA),

    logit(pLAB) <- a + pT[D,1],
    logit(pLBA) <- a + pT[D,2],

    a ~ normal(0,1),

    transpars> matrix[N,2]:pT <- compose_noncentered(rep_vector(sigma_pT, 2), L_Rho_pT, z),
    matrix[2,N]:z ~ normal(0,1),
    cholesky_factor_corr[2]:L_Rho_pT ~ lkj_corr_cholesky(2),
    sigma_pT ~ exponential(1),

    gq> matrix[2,2]:Rho_pT <<- Chol_to_Corr(L_Rho_pT)
)

m1 <- ulam(
    flist = f.m1,
    data = dat,
    chains=4,
    cores=4,
    iter=2000,
    log_lik=TRUE,
    file="./models/m1")

precis(m1, pars=c("a", "sigma_pT", "Rho_pT"), depth=3)
plot(precis(m1, pars=c("a", "sigma_pT", "Rho_pT"), depth=3))

post.m1 <- extract.samples(m1)
rho_pt.df <- tibble(x=post.m1$Rho_pT[,1,2])
ggplot(data=rho_pt.df, aes(x=x)) +
  geom_density(bw=0.007, size=2, color=2) +
  xlab("correlation within dyads") +
  xlim(c(-1,1)) +
  labs(title="Reciprocity between dyads of monks (likes)")


f.m2 <- alist(
    DAB ~ binomial(3, pDAB),
    DBA ~ binomial(3, pDBA),

    logit(pDAB) <- a + nT[D,1],
    logit(pDBA) <- a + nT[D,2],

    a ~ normal(0,1),

    transpars> matrix[N,2]:nT <- compose_noncentered(rep_vector(sigma_nT, 2), L_Rho_nT, z),
    matrix[2,N]:z ~ normal(0,1),
    cholesky_factor_corr[2]:L_Rho_nT ~ lkj_corr_cholesky(2),
    sigma_nT ~ exponential(1),

    gq> matrix[2,2]:Rho_nT <<- Chol_to_Corr(L_Rho_nT)
)

m2 <- ulam(
    flist = f.m2,
    data = dat,
    chains=4,
    cores=4,
    iter=2000,
    log_lik=TRUE,
    file="./models/m2")

precis(m2, pars=c("a", "sigma_nT", "Rho_nT"), depth=3)
plot(precis(m2, pars=c("a", "sigma_nT", "Rho_nT"), depth=3))

post.m2 <- extract.samples(m2)
rho_nt.df <- tibble(x=post.m2$Rho_nT[,1,2])
ggplot(data=rho_nt.df, aes(x=x)) +
  geom_density(aes(color="Dislike"), bw=0.007, size=2) +
  geom_density(mapping=aes(color="Like"), bw=0.007, size=2, data=rho_pt.df) +
  xlab("correlation within dyads") +
  xlim(c(-1,1)) +
  labs(title="Reciprocity between dyads of monks (like, dislike)")


diff.df <- tibble(x=rho_pt.df$x - rho_nt.df$x)
diff.df %>%
  ggplot(aes(x=x)) +
  geom_density(color=2, size=2, bw=0.007) +
  geom_vline(xintercept=0, linetype=2) +
  xlab("contrast positive Ties - negative Ties") +
  xlim(c(-1,1)) +
  labs(title="Contrast of reciprocity")

f.m6 <- alist(
    LAB ~ binomial(3, pLAB),
    LBA ~ binomial(3, pLBA),
    DAB ~ binomial(3, pDAB),
    DBA ~ binomial(3, pDBA),

    logit(pLAB) <- a[1] + pT[D,1] + rlrd[mB,1],
    logit(pLBA) <- a[1] + pT[D,2] + rlrd[mA,1],
    logit(pDAB) <- a[2] + nT[D,1] + rlrd[mB,2],
    logit(pDBA) <- a[2] + nT[D,2] + rlrd[mA,2],

    vector[2]:a ~ normal(0, 1),

    transpars>matrix[18,2]:rlrd <- compose_noncentered(rep_vector(sigma_rlrd, 2), L_Rho_rlrd, zR),
    matrix[2,18]:zR ~ normal(0,1),
    cholesky_factor_corr[2]:L_Rho_rlrd ~ lkj_corr_cholesky(2),
    sigma_rlrd ~ exponential(1),

    transpars> matrix[N,2]:pT <- compose_noncentered(rep_vector(sigma_pT, 2), L_Rho_pT, zP),
    matrix[2,N]:zP ~ normal(0,1),
    cholesky_factor_corr[2]:L_Rho_pT ~ lkj_corr_cholesky(2),
    sigma_pT ~ exponential(1),

    transpars> matrix[N,2]:nT <- compose_noncentered(rep_vector(sigma_nT, 2), L_Rho_nT, zN),
    matrix[2,N]:zN ~ normal(0,1),
    cholesky_factor_corr[2]:L_Rho_nT ~ lkj_corr_cholesky(2),
    sigma_nT ~ exponential(1),

    gq> matrix[2,2]:Rho_pT <<- Chol_to_Corr(L_Rho_pT),
    gq> matrix[2,2]:Rho_nT <<- Chol_to_Corr(L_Rho_nT),
    gq> matrix[2,2]:Rho_rlrd <<- Chol_to_Corr(L_Rho_rlrd)
)

m6 <- ulam(flist = f.m6, data=dat, cores=4, chains=4, iter=2000, log_lik=TRUE, file="./models/m6")


precis(m6, pars=c("a", "sigma_pT", "sigma_nT", "sigma_rlrd", "Rho_pT", "Rho_nT", "Rho_rlrd"), depth=3)
plot(precis(m6, pars=c("a", "sigma_pT", "sigma_nT", "sigma_rlrd", "Rho_pT", "Rho_nT", "Rho_rlrd"), depth=3))


post.m6 <- extract.samples(m6)
m6.df <- tibble(pt=post.m6$Rho_pT[,1,2],
                nt=post.m6$Rho_nT[,1,2],
                rlrd=post.m6$Rho_rlrd[,1,2]) %>%
  mutate(difft = pt-nt)

m6.df %>%
  ggplot(aes(x=pt)) +
    geom_density(aes(color="Like"), bw=0.007, size=2) +
    geom_density(mapping=aes(color="Disike", x=nt), bw=0.007, size=2) +
    xlab("correlation within dyads") +
    xlim(c(-1,1)) +
    labs(title="Reciprocity between dyads of monks (like, dislike)")

m6.df %>%
  ggplot(aes(x=difft)) +
  geom_density(color=2, size=2, bw=0.007) +
  geom_vline(xintercept=0, linetype=2) +
  xlab("contrast positive Ties - negative Ties") +
  xlim(c(-1,1)) +
  labs(title="Contrast of reciprocity")

m6.df %>%
  ggplot(aes(x=rlrd)) +
  geom_density(color=4, size=2, bw=0.02) +
  geom_vline(xintercept=0, linetype=2) +
  xlab("correlation of generalized recieving like-dislike") +
  xlim(c(-1,1)) +
  labs(title="Generalize recieving like-dislike")


monknames <- (monks %>% select(B, B_name) %>% distinct() %>% add_row(B=1, B_name="ROMUL") %>% arrange(B))$B_name

l <- sapply(1:18, function(i) post.m6$a[1] + post.m6$rlrd[,i,1])
d <- sapply(1:18, function(i) post.m6$a[2] + post.m6$rlrd[,i,2])
l_mu <- apply(inv_logit(l), 2, mean)
d_mu <- apply(inv_logit(d), 2, mean)
HPDI.50 <- function(samples) HPDI(samples, prob=.5)
l_lhpdi <- apply(inv_logit(l), 2, HPDI.50)[1,]
l_hhpdi <- apply(inv_logit(l), 2, HPDI.50)[2,]
d_lhpdi <- apply(inv_logit(d), 2, HPDI.50)[1,]
d_hhpdi <- apply(inv_logit(d), 2, HPDI.50)[2,]

require(ggforce)
ld.df <- tibble(monk=monknames,
                like=l_mu,
                dislike=d_mu,
                l_min = l_lhpdi,
                l_max = l_hhpdi,
                l_range = l_lhpdi - l_hhpdi,
                d_min = d_lhpdi,
                d_max = d_hhpdi,
                d_range = d_lhpdi - d_hhpdi
)

ld.df %>%
  ggplot(aes(like, dislike, color=monk)) +
    geom_ellipse(aes(x0=like, y0=dislike, a=l_range, b=d_range, angle=0), linetype=2) +
    geom_point(size=5) +
    geom_abline(intercept=0, slope=inv_logit(mean(m6.df$rlrd)), linetype=2) +
    scale_color_discrete("Monk")

compare(m1,m2,m6)
plot(compare(m1,m2,m6))
