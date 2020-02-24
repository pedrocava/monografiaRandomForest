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
                  filter(uf == "RJ" & !is.na(br)) %>%
                  mutate_if(is_character, as_factor) %>%
                  mutate(id = vec_cast(id, character()),
                         uf = NULL,
                         br = as_factor(br),
                         horario = hour(horario)) %T>%  
                  saveRDS("R/Dados/acidentes.Rds")

## ---------------------------------------------------------









