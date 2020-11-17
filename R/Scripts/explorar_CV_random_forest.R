

library(tidyverse)
library(rio)
library(logger)
library(magrittr)
library(wesanderson)
library(glue)
pal <- wes_palette("IsleofDogs1")
cor <- wes_palette("Cavalcanti1") %>% 
  pluck(1)

metricas %>%
  mutate(.metric = toupper(.metric)) %>%
  ggplot(aes(
    x = min_n, 
    group = min_n,
    fill = .metric, 
    y = mean)) +
  labs(
    x = "Amostra Mínima para Folha",
    y = "Métrica"
  ) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  facet_wrap(~ .metric, scales = "free") +
  theme(legend.position = "none")

metricas %>%
  mutate(.metric = toupper(.metric)) %>%
  ggplot(aes(
    x = mtry, 
    group = mtry,
    fill = .metric, 
    y = mean)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Variáveis para cada árvore",
    y = "Métrica"
  ) +
  scale_fill_manual(values = pal) +
  facet_wrap(~ .metric, scales = "free") +
  theme(legend.position = "none")


predicoes %>%
  summarise(dif = aluguel - .pred) %>%
  ggplot(aes(x = dif)) +
  geom_histogram(
    aes(y = ..density..), 
    binwidth = 50, fill = cor) +
  geom_vline(size = 1.2, xintercept = 0) +
  theme(legend.position = "none") +
  theme_minimal()




predicoes %>%
  ggplot(aes(x = .pred, y = aluguel)) +
  geom_density_2d(size = 1.2, color = cor) +
  geom_abline(
    size = 1.2, 
    slope = 1, 
    intercept = 0) +
  theme_minimal() +
  labs(
    x = "Valores previstos",
    y = "Valor real",
    title = "Distribuição conjunta das previsões e erros")
