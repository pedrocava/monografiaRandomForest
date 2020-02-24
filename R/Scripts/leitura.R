## Script de Leitura, Tratamento e Geração de Dados a partir dos microdados de ocorrência da PRF

## Pedro Cavalcante <pedrocolrj@gmail.com>

## -----------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(magrittr)
library(rlang)
library(vctrs)

## -----------------------------------------

datatran2019 <- read_csv2("R/Dados/datatran2019.csv",
                           locale = readr::locale(encoding = "latin1")) %>%
                  mutate_if(is_character, as_factor) %>%
                  mutate(id = vec_cast(id, character())) %T>% # preservando o id
  saveRDS("R/Dados/acidentes.Rds")








