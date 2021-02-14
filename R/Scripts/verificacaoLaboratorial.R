

library(tidyverse)
library(randomForest)
library(wesanderson)
library(patchwork)
library(stargazer)
library(magrittr)


pal <- wes_palette("IsleofDogs1")
cor <- wes_palette("Cavalcanti1") %>% 
  pluck(1)


N <- 10000
beta <- 1.5

(fake_data <- tibble(
  X = rnorm(n = N, mean = 5, sd = 2),
  epsilon = rnorm(n = N, sd = 3),
  Y = 10 + beta*X + epsilon))


fake_data %>%
  ggplot(aes(x = X, y = Y, color = cor)) +
  geom_point(size = 1.2, alpha = .5) +
  theme_minimal() +
  labs(
    X = "Regressor",
    Y = "Resposta",
    title = "Dados simulados",
    subtitle = "Processo simulado: y_i = 10 + 1,5 * x_i + erro") +
  theme(legend.position = 'none')


modelo <- randomForest(formula = Y ~ X, nodesize = 30, ntree = 100, data = fake_data)

eval_verif <- tibble(X = seq(-1, 13, by = .01)) %>%
  mutate(predito = map_dbl(X, 
    ~ tibble(X = .x) %>% 
      mutate(., predito = predict(modelo, newdata = .)) %>%
      pull(predito)))  

frame_marginal_effects(eval_verif, X) 

eval_verif %>%
  mutate(verdadeiro = 10 + beta*X) %>%
  ggplot(aes(x = predito, y = verdadeiro)) +
  geom_point() +
  labs(
    X = "Predito",
    Y = "Valor verdadeiro",
    title = "Comparação de predições do modelo com valores corretos") +
  xlim(10, 30) +
  ylim(10, 30) +
  theme_minimal() +
  geom_smooth(method = 'lm')
  
lm(predito ~ X, data = eval_verif) %>%
  stargazer(out = 'tabelas/verif_lab.tex')



eval_verif %>%
  mutate(verdadeiro = 10 + beta*X, residuo = verdadeiro - predito) %>%
  ggplot(aes(x = residuo)) +
  geom_histogram(binwidth = .4)
