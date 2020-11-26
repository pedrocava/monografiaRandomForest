## Procedimento

library(tidyverse)
library(randomForest)
library(magrittr)
library(gtools)
library(rlang)
library(patchwork)
library(glue)

## helper ------------------------

recover <- function(.data) {
  
  .data %>%
    mutate(area = exp(area),
           quartos = exp(quartos))
  
}


## treinar os modelos ---------------------

(houses_mod <- readRDS("R/Dados/houses_clean.Rds") %>%
   mutate(area = log(area),
          quartos = log(quartos)))

houses_mod %>%
  mutate(val = runif(n = nrow(.)) < .75) %>% {
    
    treino <<- filter(., val) %>%
      select(-val)
    
    teste <<- filter(., !val) %>%
      select(-val)
    
  }

(expand_grid(
  mtry = 3:8,
  min_n = seq(20, 200, 20)) ->
   grade_params)

(grade_params %>%
  mutate(
    melhor_modelo = mtry == 3 & min_n == 20,
    modelos = map2(
      .x = mtry,
      .y = min_n,
      ~ randomForest(
        formula = aluguel ~ .,
        data = treino,
        mtry = .x,
        nodesize = .y,
        ntree = 12*(ncol(houses_mod) - 2),
        importance = TRUE,
        keep.forest = TRUE))) ->
  modelos)

## gerar a grade --------------------------

source("R/Scripts/thicken.R")

cidades <- c("Rio de Janeiro", "São Paulo", "Belo Horizonte", "Campinas", "Porto Alegre")

(apt_representativo <- tibble(
  cidade = factor("Rio de Janeiro", levels = cidades),
  area = log(92),
  quartos = log(3),
  banheiros = 2L,
  vagas = 1L,
  andar = 8,
  mobiliado = 0,
  aceita_animal = 1,
  aluguel = NA_integer_))

(grade <- apt_representativo %>%
  thicken(area, log(60), log(120), n = 120) %>%
    thicken(andar, 0, 22, n = 24))

## avaliar --------------------------------

(modelos %>%
  mutate(
    id_modelo = as.character(1:nrow(.)),
    a_tirar = map(
      modelos,
      ~ predict(.x, newdata = grade) %>%
        tibble(predito = .) %>%
        bind_cols(grade))) %>%
  unnest(a_tirar) %>%
  recover() %>%
  mutate(area = floor(area)) ->
  eval)

curva <- function(.data, var) {
  
  .data %>%
    arrange({{var}}) %>%
    group_by({{var}}) %>%
    summarise(
      predito = mean(predito),
      .groups = "drop") %>%
    mutate(dif = predito - lag(predito, n = 1)) %>%
    slice(2:nrow(.))
  
}

## plotar ---------------------------------

plot_curva <- function(.curva, type = "predito") {
  
  var <- .curva %>%
    names() %>%
    discard(~ .x %in% c("predito", "dif")) %>%
    sym()
  
  if(type == "predito") {
    
    type <- sym(type)
    
  } else if (type == "dif") {
    
    type <- sym("dif")
    
  }
    
  .curva %>%
    ggplot(aes(x = {{var}}, y = {{type}})) +
    geom_col(alpha  = .7, fill = cor) +
    theme_minimal() +
    labs(
      x = glue("{as_string(var)}"),
      y = glue("Comportamento {ifelse(as_string(type) == 'predito', 'Variável Resposta', 'Efeitos Marginais')}"),
      title = glue("Comportamento {ifelse(as_string(type) == 'predito', 'da Predição Média', 'dos Efeitos Marginais')}")
    )
  
}

frame_marginal_effects <- function(.data, var) {
  
  eval %>%
    curva({{var}}) %>%
    plot_curva("dif") +
    eval %>%
    curva({{var}}) %>%
    plot_curva()
  
  
}

frame_marginal_effects(eval, andar) /
  frame_marginal_effects(eval, area)
 

curva_graf <- function(.data, var, subtitle = NULL) {
  
  .data %>%
    group_by({{var}}, id_modelo) %>%
    summarise(predit = mean(predito), .groups = "drop") %>%
    ggplot(aes(x = {{var}}, y = predit, group = id_modelo)) +
    geom_line(size = 1.2, alpha  = .7, color = cor) +
    theme_minimal() +
    labs(
      x = "Área (m^2)",
      y = "Aluguel Predito",
      title = glue("Curvas de Previsões"),
      subtitle = subtitle
    )
  
}

curva_graf(eval, andar) + curva_graf(eval, area) 




