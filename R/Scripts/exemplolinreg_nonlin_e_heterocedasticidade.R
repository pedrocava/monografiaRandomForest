## exemplo3



library(tidyverse)
library(magrittr)
library(wesanderson)
library(stargazer)

pal <- wes_palette("Cavalcanti1")
cor <- wes_palette("Cavalcanti1") %>% 
  pluck(1)


(tibble(x = runif(1000, -10, 10),
        x2 = x^2,
        y = 100 + 5*x - 2*x^2 + rnorm(length(x), sd = 20)) -> 
  data)

png("imagens/exemplo3_dist.png", width = 940, height = 678, res = 120)
data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, color = cor, alpha = .4) +
  theme_minimal() +
  labs(title = "y(x) = 100 + 5x - 2x^2 + resíduo",
       subtitle = "Resíduo aleatórios tirados de uma N(0, 20)",
       x = "X",
       y = "Y")
dev.off()

lm(y ~ x, data = data) -> modelo1
lm(y ~ x + x2, data = data) -> modelo2

stargazer(modelo1, modelo2, align = TRUE, out = "tabelas/exemplo3.tex")

### ---------------------------


(tibble(x = runif(1000, -10, 10),
        x2 = x^2,
        y = 100 + 5*x - 2*x^2 + x*rnorm(length(x), sd = 20)) -> 
    data2)

png("imagens/exemplo_heteroske.png", width = 940, height = 678, res = 120)
data2 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, color = cor, alpha = .4) +
  theme_minimal() +
  labs(title = "y(x) = 100 + 5x - 2x^2 + x*resíduo",
       subtitle = "Resíduo aleatórios tirados de uma N(0, 20)",
       x = "X",
       y = "Y")

dev.off()

lm(y ~ x, data = data2)  %>%
  stargazer(align = TRUE, out = "tabelas/exemplo_heteroskeda")



