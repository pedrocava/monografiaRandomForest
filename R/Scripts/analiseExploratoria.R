## Script de Leitura, Tratamento e Geração de Dados a partir dos microdados de ocorrência da PRF

## Pedro Cavalcante <pedrocolrj@gmail.com>

## -----------------------------------------

library(dplyr)
library(ggplot2)
library(magrittr)
library(purrr)

ggSave <- partial(ggplot2::ggsave, 
                  dpi = 200,
                  path = paste0(getwd(),"/imagens"),
                  width = 10.6,
                  height = 5.62,
                  units = "in")


dados <- readRDS("R/Dados/acidentes.Rds")

## -----------------------------------------

dados %>%
  ggplot(aes(x = longitude, y = latitude, color = br)) +
  geom_point() +
  labs(title = "Reconstruindo a Rede de Estradas Federais no RJ pelos Acidentes",
       x = "Longitude",
       y = "Latitude",
       caption = "Fonte: Polícia Rodoviária Federal") +
  theme_minimal()


ggSave(filename = "redeEstradasAcidentes.png")






