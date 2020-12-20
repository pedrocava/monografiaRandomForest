## exemplo3



library(tidyverse)
library(magrittr)
library(wesanderson)
library(stargazer)
library(broom)
library(knitr)

pal <- wes_palette("Cavalcanti1")
cor <- wes_palette("Cavalcanti1") %>% 
  pluck(1)


(tibble(x = runif(1000, -10, 10),
        x2 = x^2,
        y = 100 + 2*x - 5*x^2 + rnorm(length(x), sd = 20)) -> 
  data)

png("imagens/exemplo4_dist.png", width = 940, height = 678, res = 120)
data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, color = cor, alpha = .4) +
  theme_minimal() +
  labs(title = "y(x) = 100 + 2x - 5x^2 + resíduo",
       subtitle = "Resíduo aleatórios tirados de uma N(0, 20)",
       x = "X",
       y = "Y")
dev.off()

lm(y ~ x, data = data) -> modelo1
lm(y ~ x + x2, data = data) -> modelo2


tabela1_4 <- file('tabelas/tabela1_exemplo4.tex')

tidy(modelo1) %>%
  rename(
    termo = term,
    estimativa = estimate,
    erro_padrao = std.error,
    estatistica_t = statistic) %>%
  select(-p.value) %>%
  kable(format = 'latex',
        caption = 'Modelo sem termo quadrático',
        label = 'tabela1_exemplo4',
        digits = 2) %>%
  write_lines(tabela1_4)

# close(tabela1_4)

tabela2_4 <- file('tabelas/tabela2_exemplo4.tex')

tidy(modelo2) %>%
  rename(
    termo = term,
    estimativa = estimate,
    erro_padrao = std.error,
    estatistica_t = statistic) %>%
  select(-p.value) %>%
  kable(format = 'latex',
        caption = 'Modelo com termo quadrático',
        label = 'tabela2_exemplo4',
        digits = 2) %>%
  write_lines(tabela2_4)


### ---------------------------


(tibble(x = runif(1000, 0, 10),
        x2 = x^2,
        y = 100 + 5*x - 2*x2 + x*abs(rnorm(length(x), sd = 20))) -> 
    data2)

png("imagens/exemplo_heteroske.png", width = 940, height = 678, res = 120)
data2 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, color = cor, alpha = .4) +
  theme_minimal() +
  labs(title = "y(x) = 100 + 5x - 2x^2 + |x|*resíduo",
       subtitle = "Resíduo aleatórios tirados de uma N(0, 20)",
       x = "X",
       y = "Y")

dev.off()

lm(y ~ x, data = data2) -> modelo21
lm(y ~ x + x2, data = data2) -> modelo22


# close(tabela1_4)

tabela_hetero <- file('tabelas/tabela_heteroskeda.tex')

tidy(modelo22) %>%
  rename(
    termo = term,
    estimativa = estimate,
    erro_padrao = std.error,
    estatistica_t = statistic) %>%
  select(-p.value) %>%
  kable(format = 'latex',
        caption = 'Modelo com termo quadrático',
        label = 'tabela2_exemplo4',
        digits = 2) %>%
  write_lines(tabela_hetero)

stargazer(modelo21, modelo22, 
          align = TRUE, out = "tabelas/exemplo_heteroskeda.tex")



