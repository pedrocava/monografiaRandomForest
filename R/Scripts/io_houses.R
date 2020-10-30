## IO dados de casas

library(tidyverse)
library(rio)
library(logger)
library(magrittr)
library(wesanderson)
library(glue)

(houses <- import("R/Dados/houses.csv", 
                  setclass = "tibble",
                  encoding = "UTF-8") %>%
    rename(
      cidade = city,
      quartos = rooms,
      banheiros = bathroom,
      vagas = `parking spaces`,
      andar = floor,
      mobiliado = furniture,
      condominio = `hoa (R$)`,
      aluguel = `rent amount (R$)`,
      iptu = `property tax (R$)`,
      seguro_incendio = `fire insurance (R$)`,
      total = `total (R$)`
    ) %>%
    mutate(
      aceita_animal = if_else(animal == "acept", 1, 0),
      animal = NULL,
      andar = ifelse(andar == "-", 0, as.numeric(andar)),
      mobiliado = if_else(mobiliado == "furnished", 1, 0),
      cidade = factor(cidade)) %T>%
    saveRDS("R/Dados/houses_full.Rds") %T>%
    write_csv("R/Dados/houses_full.csv") %>%
    filter(area < 500, aluguel < 10000, cidade != "Campinas", andar < 40) %>%
    select(-iptu, -seguro_incendio, -total, -condominio) %T>%
    saveRDS("R/Dados/houses_clean.Rds") %T>%
    write_csv("R/Dados/houses_clean.csv")
)

normalize <- function(v) (v - mean(v)) / sd(v)


gsave <- partial(
  ggsave,
  device = "png",
  path = "Imagens",
  dpi = "retina",
  width = 16,
  height = 10,
  units = "cm" )



pal <- wes_palette("Cavalcanti1")
cor <- wes_palette("Cavalcanti1") %>% 
  pluck(1)

houses %>%
  ggplot(aes(x = area, fill = cidade)) +
  geom_histogram(aes(y = ..density..)) +
  theme_minimal() +
  labs(title = "Distribuição dos apartamentos por área e cidade",
       subtitle = "Apartamentos disponíveis para aluguel em Março de 2020",
       x = "Área (M^2)",
       y = "Densidade",
       fill = "Cidade") +
  scale_fill_manual(values = pal) +
  facet_wrap(~ cidade) +
  theme(legend.position = "none")

houses %>%
  ggplot(aes(x = aluguel, fill = cidade)) +
  geom_histogram(aes(y = ..density..)) +
  theme_minimal() +
  labs(title = "Distribuição dos aluguéis por cidade",
       subtitle = "Apartamentos disponíveis para aluguel em Março de 2020",
       x = "Aluguel",
       y = "Densidade",
       fill = "Cidade") +
  scale_fill_manual(values = pal) +
  facet_wrap(~ cidade) +
  theme(legend.position = "none")

houses %>%
  ggplot(aes(x = andar, fill = cidade)) +
  geom_histogram(aes(y = ..density..)) +
  theme_minimal() +
  labs(title = "Distribuição dos andares de apartamentos para aluguel",
       subtitle = "Apartamentos disponíveis em Março de 2020",
       x = "Andar",
       y = "Densidade",
       fill = "Cidade") +
  scale_fill_manual(values = pal) +
  facet_wrap(~ cidade) +
  theme(legend.position = "none")


houses %>%
  ggplot(aes(x = quartos, fill = cidade)) +
  geom_histogram(aes(y = ..density..)) +
  theme_minimal() +
  labs(title = "Distribuição dos quartos por cidade",
       subtitle = "Apartamentos disponíveis para aluguel em Março de 2020",
       x = "quartos",
       y = "Densidade",
       fill = "Cidade") +
  scale_fill_manual(values = pal) +
  facet_wrap(~ cidade) +
  theme(legend.position = "none")

houses %>%
  ggplot(aes(x = area, y = aluguel, color = cidade)) +
  geom_point(alpha = .3) +
  theme_minimal() +
  labs(title = "Distribuição dos aluguéis e áreas por cidade",
       subtitle = "Tendência estimada por GAM",
       x = "Área",
       y = "Aluguel") +
  geom_smooth(size = 1.2) +
  facet_wrap(~ cidade) +
  scale_color_manual(values = pal) +
  theme(legend.position = "none")

houses %>%
  ggplot(aes(x = quartos, y = aluguel, color = cidade)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "loess", size = 1.2) +
  theme_minimal() +
  labs(title = "Quartos e Aluguéis por cidade",
       subtitle = "Tendência estimada por GAM",
       x = "Número de Quartos",
       y = "Aluguel") +
  facet_wrap(~ cidade) +
  scale_x_discrete(limits = 1:6) + 
  scale_color_manual(values = pal) +
  theme(legend.position = "none")
