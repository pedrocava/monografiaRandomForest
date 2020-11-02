
library(tidyverse)
library(magrittr)
library(wesanderson)
library(stargazer)
library(knitr)
library(rpart)
library(rpart.plot)


pal <- wes_palette("Cavalcanti1")
cor <- wes_palette("Cavalcanti1") %>% 
  pluck(1)



(houses <- readRDS("R/Dados/houses_clean.Rds"))

(casa <- tibble(
  cidade = "Rio de Janeiro",
  area = 82,
  quartos = 2,
  banheiros = 1,
  vagas = 1,
  andar = 5,
  mobiliado = 0,
  aceita_animal = 1))


(map(1:1000,
    ~ sample_frac(houses, .3), 100) %>%
  tibble(data = .) %>%
  mutate(arvores = map(data, ~ rpart(aluguel ~ ., data = .x)),
         ols = map(data, ~ lm(aluguel ~ ., data = .x))) %>%
  pivot_longer(arvores:ols, names_to = "modelo") %>%
  mutate(predict = map_dbl(value, ~ predict(.x, casa))) ->
  models)


models %>% 
  group_by(modelo) %>%
  summarise(media = mean(predict),
            SD = sd(predict)) %>%
  pivot_wider(values_from = media:SD)
  

models %>%
  ggplot(aes(x = predict, fill = modelo)) +
  geom_density() +
  labs(title = "Densidade das previsões para o apartamento, por modelo",
       x = "Aluguel Presumido",
       y = "Densidade") +
  theme_minimal() +
  scale_fill_manual(values = pal) 
  




