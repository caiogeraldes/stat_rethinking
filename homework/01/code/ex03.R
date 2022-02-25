source("./ex02.R")

set.seed(10) # to reproduce the values sampled
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
tibble(samples) %>% ggplot(aes(x=samples)) +
    geom_density() +
    theme(text=element_text(size=30))
ggsave(file="../figs/ex03samples.png", width=16, height=4, dpi=600)

percentile  <- rethinking::PI(samples, .89)
#        5%        94% 
# 0.5252525 0.8787879
print(str_c("Percentile width: ", as.numeric(percentile[2] - percentile[1])))
# [1] "Percentile width: 0.353535353535354"
hpdi <- rethinking::HPDI(samples, .89)
#     |0.89     0.89| 
# 0.5050505 0.8383838 
print(str_c("HPDI width: ", as.numeric(hpdi[2] - hpdi[1])))
# [1] "HPDI width: 0.333333333333333"

percentile.plot <- df %>% ggplot(aes(x=p_grid, y=posterior)) +
  geom_area(data=subset(df, p_grid > percentile[1] & p_grid< percentile[2]), fill = "red", alpha=0.5) +
  geom_line() +
  geom_point(alpha=0.5) +
  ylab("density") +
  xlab("proportion of water") +
  theme(text=element_text(size=30))
ggsave(file="../figs/ex03percentile.png", width=6, height=4, dpi=600)

hpdi.plot <- df %>% ggplot(aes(x=p_grid, y=posterior)) +
  geom_area(data=subset(df, p_grid > hpdi[1] & p_grid< hpdi[2]), fill = "blue", alpha=0.5) +
  geom_line() +
  geom_point(alpha=0.5) +
  ylab("density") +
  xlab("proportion of water") +
  theme(text=element_text(size=30))
ggsave(file="../figs/ex03hpdi.png", width=6, height=4, dpi=600)

test.plot <- df %>% ggplot(aes(x=p_grid, y=posterior)) +
  geom_area(data=subset(df, p_grid > hpdi[1] & p_grid< hpdi[2]), fill = "blue", alpha=0.5) +
  geom_area(data=subset(df, p_grid > percentile[1] & p_grid< percentile[2]), fill = "red", alpha=0.5) +
  geom_line() +
  geom_point(alpha=0.5) +
  ylab("density") +
  xlab("proportion of water")
ggsave(file="../figs/ex03test.png", width=6, height=4, dpi=600)
