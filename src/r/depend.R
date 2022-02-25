# Dependencies for Statistical Rethinking course by Somomon Kurz
# https://bookdown.org/content/4857/

packages <- c("ape",
              "bayesplot",
              "brms",
              "broom",
              "dagitty",
              "devtools",
              "flextable",
              "GGally",
              "ggdag",
              "ggdark",
              "ggmcmc",
              "ggrepel",
              "ggthemes",
              "ggtree",
              "ghibli",
              "gtools",
              "loo",
              "patchwork",
              "psych",
              "rcartocolor",
              "Rcpp",
              "remotes",
              "rstan",
              "StanHeaders",
              "statebins",
              "tidybayes",
              "tidyverse",
              "viridis",
              "viridisLite",
              "wesanderson")


install.packages(packages, dependencies = T)

devtools::install_github("stan-dev/cmdstanr")
devtools::install_github("EdwinTh/dutchmasters")
devtools::install_github("gadenbuie/ggpomological")
devtools::install_github("rmcelreath/rethinking")
devtools::install_github("UrbanInstitute/urbnmapr")
remotes::install_github("stan-dev/posterior")
