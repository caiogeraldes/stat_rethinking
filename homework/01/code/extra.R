
samples.df <- with(density(samples), data.frame(x, y))
samples.df %>% ggplot(samples.df, mapping = aes(x = x, y = y)) +
    geom_line()+
    geom_area(mapping = aes(x = ifelse(x>percentile[1] & x< percentile[2] , x, 0)), fill = "red", alpha=0.5) +
    xlim(0.1, 1)
samples.df %>% ggplot(samples.df, mapping = aes(x = x, y = y)) +
    geom_line()+
    geom_area(mapping = aes(x = ifelse(x>hdpi[1] & x< hdpi[2] , x, 0)), fill = "blue", alpha=0.5) +
    xlim(0.1, 1)
samples.df %>% ggplot(samples.df, mapping = aes(x = x, y = y)) +
    geom_line()+
    geom_area(mapping = aes(x = ifelse(x>percentile[1] & x< percentile[2] , x, 0)), fill = "red", alpha=0.5) +
    geom_area(mapping = aes(x = ifelse(x>hdpi[1] & x< hdpi[2] , x, 0)), fill = "blue", alpha=0.5) +
    xlim(0.4, 1)
