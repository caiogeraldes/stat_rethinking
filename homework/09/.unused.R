
f.m3 <- alist(
  S ~ bernoulli(p),
  logit(p) <- a + b*sum(delta_j[1:AG]) + gA*A + gB*A^2,
  a ~ normal(0, 0.1),
  b ~ dlnorm(-1,0.5),
  gA ~ normal(0, 0.5),
  gB ~ normal(0, 0.5)
  vector[7]:delta_j <<- append_row(0, delta),
  simplex[6]:delta ~ dirichlet(alpha)
)

test_link <- function(model, dat) {
  ps <- extract.samples(model)
  a <- ps$a
  b <- ps$b
  gA <- ps$gA
  gB <- ps$gB
  delta <- ps$d
  d0 <- rep(0, nrow(delta))
  delta_j <- cbind(d0, delta)
  lp <- sapply(1:length(dat$AG), function(i) ps$a + b*apply(as.matrix(delta_j[,1:(dat$AG[i])]),1,sum) + gA*dat$A[i] + gB*dat$A[i]^2)
  return(inv_logit(lp))
}

p <- test_link(m3, dat)
