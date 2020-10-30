## exemplo3



library(tidyverse)
library(magrittr)
library(wesanderson)
library(stargazer)

pal <- wes_palette("Cavalcanti1")
cor <- wes_palette("Cavalcanti1") %>% 
  pluck(1)


(tibble(x = runif(1000, -10, 10),
        y = 100 + 5*x - 2*x^2 + rnorm(length(x), mean = 5, sd = 10)) -> 
  data)

data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, color = cor, alpha = .4) +
  theme_minimal() +
  labs(title = "y(x) = 100 + 5x + 2x^2 + resíduo",
       subtitle = "Resíduo aleatórios tirados de uma N(5, 10)",
       x = "X",
       y = "Y")

lm(y ~ x, data = data) -> modelo1
lm(y ~ x + x^2, data = data) -> modelo2

stargazer(modelo1, modelo2, align = TRUE, out = "tabelas/exemplo3.tex")
