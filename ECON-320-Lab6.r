

library(tinytex)

library(tidyverse)

library(dslabs)
library(dplyr)

library(ggplot2)

library(tibble)
library(modelsummary)

library(broom)

library(haven)



n <- 1000

set.seed(1)

# Generate data in a tibble
data_sim = tibble(
  e1 = rnorm(n, sd = 3),
  e2 = rnorm(n, sd = 2),
  e3 = rnorm(n, sd = 1),
  x = runif(n, min = 0, max = 10),
  y = runif(n, min = 10, max = 20),
  z = 20 - 0.3*y + 3*x + e1,
  a = 6 + 2*x - 1.5*y + e2,
  b = 10 - 0.5*y + 4*z + e3)

lm1 = lm(data = data_sim, a ~ x)
lm2 = lm(data = data_sim, a ~ y)
lm3 = lm(data = data_sim, a ~ x + y)




modelsummary(
  list("Model 1" = lm1, "Model 2" = lm2, "Model 3" = lm3),
  stars = TRUE,
  statistic = "std.error",
  output = "tinytable",
  title = "Results From Simulated Data",
  gof_omit = ".*"  # remove AIC, BIC, etc.
)



lm4 = lm(data = data_sim, b ~ y)
lm5 = lm(data = data_sim, b ~ z)
lm6 = lm(data = data_sim, b ~ y + z)



modelsummary(
  list("Model 1" = lm4, "Model 2" = lm5, "Model 3" = lm6),
  stars = TRUE,
  statistic = "std.error",
  output = "tinytable",
  title = "Results From Simulated Data",
  gof_omit = ".*"  # remove AIC, BIC, etc.
)



data_sim <- data_sim %>% 
  mutate(c = rnorm(n, mean = 10, sd = 5))

lm9 = lm(data = data_sim, a ~ c + y)


data_sim <- data_sim %>%
  mutate(plus = y + z)

restricted = lm(data = data_sim, b ~ plus)
unrestricted = lm(data = data_sim, b ~ y + z)



res_r_sq = (unname(resid(restricted)))^2
res_u_sq = (unname(resid(unrestricted)))^2

rss_r = sum(res_r_sq)
rss_u = sum(res_u_sq)

q = 1
k = 2

F_stat <- ((rss_r - rss_u)/q)/(rss_u/(n - k - 1))
F_stat


