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
library(lubridate)

## -----------------------------------------

datatran2019 <- read_csv2("R/Dados/datatran2019.csv",
                           locale = readr::locale(encoding = "latin1")) %>%
                  filter(uf %in% c("RJ", "SP", "MG", "ES") & !is.na(br)) %>%
                  mutate_if(is_character, factor) %>%
                  mutate(id = vec_cast(id, character()),
                         br = factor(br),
                         horario = hour(horario)) %T>%  
                  saveRDS("R/Dados/acidentes.Rds")

## ---------------------------------------------------------









