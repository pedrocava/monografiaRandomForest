## Script de Leitura, Tratamento e Geração de Dados a partir dos microdados de ocorrência da PRF

## Pedro Cavalcante <pedrocolrj@gmail.com>

## -----------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
library(rlang)
library(vctrs)
library(rvest)
library(data.table)
library(glue)
library(electionsBR)
library(rio)
library(stringi)
library(tsibble)

## -----------------------------------------

gen_key <- function(.data) {
  
  .data %>%
    mutate(key = as.character(glue("{municipio} - {uf}")))
  
}


get_raw_accidents <- function(reference_year) {
  
  html_content <-
    read_html("https://portal.prf.gov.br/dados-abertos-acidentes") %>%
    html_node("#acontent ul") %>%
    html_nodes("li")
  
  acidentes <- tibble(
    href = {
      html_content %>%
        html_children %>%
        html_attr("href")
    },
    year = {
      html_content %>%
        html_text() %>%
        as.numeric()
    }
  ) %>%
    filter(year == reference_year) %>%
    mutate(href = glue("curl {href}/download | funzip")) %>%
    pull(href) %>%
    fread(encoding = "Latin-1") %>%
    as_tibble()
  
  acidentes %>%
    mutate(id = as.character(id),
           br = as.character(br),
           timestamp = parse_date_time(glue("{data_inversa} {horario}"), "Ymd HMS", truncated = 3),
           municipio = str_to_title(municipio),
           mun_uf = as.character(glue("{municipio} - {uf}"))) %>%
    select(-data_inversa, -horario, -dia_semana)

}

#slow_foo <- slowly(get_raw_accidents, rate_delay(5, max_times = 100))

tibble(ano = 2017:2019) %>%
  mutate(data = map(ano, ~ get_raw_accidents(.x))) %T>%
  saveRDS("R/Dados/painel_acidentes_raw.Rds")


readRDS("R/Dados/painel_acidentes_raw.Rds") %>%
  pull(data) %>%
  reduce(bind_rows) %>%
  mutate(latitude = parse_double(latitude, locale = locale(decimal_mark = ',')),
         longitude = parse_double(longitude, locale = locale(decimal_mark = ','))) %>%
  saveRDS("R/Dados/acidentes.Rds")


readRDS("R/Dados/painel_acidentes_raw.Rds") %>%
  pull(data) %>%
  reduce(bind_rows) %>%
  mutate(anomes = yearmonth(timestamp)) %>%
  group_by(municipio, uf, anomes) %>%
  summarise(acidentes = n(),
            mortos = sum(mortos),
            .groups = "drop") %T>%
  saveRDS("R/Dados/acidentes_agregado.Rds") ->
  acidentes_agregado
  
### Votos ------------------------

(import("R/Dados/votos.csv", setclass = "tibble") %>%
  mutate(NOME_MUNICIPIO = stri_trans_general(NOME_MUNICIPIO, "Latin-ASCII")) %>%
  rename(municipio = NOME_MUNICIPIO,
         uf = UF, 
         votos = QTDE_VOTOS) %>%
  filter(NUM_TURNO == 1) %>%
  group_by(COD_MUN_IBGE) %>%
  mutate(total_votos = sum(votos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(NUMERO_CANDIDATO == 17) %>%
  select(municipio, uf, votos, total_votos) %>%
  group_by(municipio, uf) %>%
  summarise(votos_primeiro_turno = votos/total_votos) %T>%
  saveRDS("R/Dados/votos_limpos.Rds") ->
  votos)
  
(inner_join(acidentes_agregado,
           votos) %>%
  mutate(
    log_acidentes = log(acidentes),
    log_mortes = log(1 + mortos),
    eleicao = if_else(anomes > yearmonth(ymd(20181026)), 1, 0),
    governo_bolsonaro = if_else(anomes > yearmonth(ymd(20190101)), 1, 0)) %>%
    gen_key() %T>%
  saveRDS("R/Dados/data_acidentes.Rds") ->
  data)
  


# datatran2019 <- read_csv2("R/Dados/datatran2019.csv",
#                            locale = readr::locale(encoding = "latin1")) %>%
#                   filter(uf %in% c("RJ", "SP", "MG", "ES") & !is.na(br)) %>%
#                   mutate_if(is_character, factor) %>%
#                   mutate(id = vec_cast(id, character()),
#                          br = factor(br),
#                          horario = hour(horario)) %T>%  
#                   saveRDS("R/Dados/acidentes.Rds")

## ---------------------------------------------------------









