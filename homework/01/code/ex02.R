grid_size <- 100
p_grid <- seq(from=0, to=1, length.out=grid_size)
prior <- ifelse(p_grid < 0.5 , 0 , 1)
likelihood <- dbinom(4, size=6, prob=p_grid)
unstd.posterior  <- likelihood * prior
posterior  <- unstd.posterior / sum(unstd.posterior)

# Plotting
require('tidyverse')
df <- tibble(p_grid, prior, likelihood, posterior)
data <- gather(df, variable, value, -p_grid)
data$class <- factor(data$variable, levels=c('prior', 'likelihood', 'posterior'))
data %>% ggplot(aes(p_grid, value)) +
  geom_point(alpha=0.5) +
  geom_line() +
  facet_wrap(class ~ ., scales = 'free') +
  ylab("density") +
  xlab("proportion of water") +
  theme(text=element_text(size=30))

ggsave(file="../figs/ex02.png", width=18, height=6, dpi=600)
