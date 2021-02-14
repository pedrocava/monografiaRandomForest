

library(tidyverse)
library(randomForest)
library(wesanderson)
library(patchwork)
library(stargazer)
library(magrittr)
library(knitr)


pal <- wes_palette("IsleofDogs1")
cor <- wes_palette("Cavalcanti1") %>% 
  pluck(1)

houses <- readRDS("R/Dados/houses_clean.Rds")

houses %>%
  ggplot(aes(x = aluguel)) +
  geom_histogram(fill = cor, binwidth = 200) +
  labs(
    x = "Aluguel",
    y = "",
    title = "Histograma dos Alugueis") +
  theme_minimal() -> aluguel

houses %>%
  ggplot(aes(x = area)) +
  geom_histogram(fill = cor, binwidth = 10) +
  labs(
    x = "Metragem",
    y = "",
    title = "Histograma das Metragens") +
  theme_minimal() -> metragem

houses %>%
  ggplot(aes(x = andar)) +
  geom_histogram(fill = cor, binwidth = 1) +
  labs(
    x = "Andar",
    y = "",
    title = "Histograma dos Andares") +
  theme_minimal() -> andar

tabela <- file('tabelas/exploratoria_cidades.tex')

houses %>%
  group_by(cidade) %>%
  summarise(
    area_media = mean(area),
    quartos_media = mean(quartos),
    aluguel_medio = mean(aluguel),
    custo_m2_medio = mean(aluguel/area)) %>%
  kable("latex") %>%
  enc2utf8() %>%
  writeLines(tabela, useBytes = TRUE)

close(tabela)


  