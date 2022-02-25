grid.size <- 100
p.grid <- seq(from=0, to=1, length.out=grid.size)
prior <- rep(1, grid.size)
likelihood <- dbinom(4, size=15, prob=p.grid)
unstd.posterior  <- likelihood * prior
posterior  <- unstd.posterior / sum(unstd.posterior)

# Plotting
require('tidyverse')
df <- tibble(p.grid, prior, likelihood, posterior)
df <- gather(df, variable, value, -p.grid)
df$class <- factor(df$variable, levels=c('prior', 'likelihood', 'posterior'))
df %>% ggplot(aes(p.grid, value)) +
  geom_point(alpha=0.5) +
  geom_line() +
  facet_wrap(class ~ ., scales = 'free') +
  ylab("density") +
  xlab("proportion of water") +
  theme(text=element_text(size=30))

ggsave(file="../figs/ex01.png", width=18, height=6, dpi=600)
