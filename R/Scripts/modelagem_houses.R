

library(tidymodels)
library(tidyverse)
library(randomForest)
library(magrittr)
library(rpart)
library(knitr)
library("rpart.plot")

set.seed(1234)

(houses <- readRDS("R/Dados/houses_clean.Rds"))

## Árvore de Decisão ------------------------

arvore <- rpart(aluguel ~ .,
                data = houses)

rpart.plot(arvore) 


pal <- wes_palette("BottleRocket1", n = 6)
cor <- wes_palette("BottleRocket1", n = 6) %>% 
  pluck(1)

## Tidymodels ------------------------------

(houses_split <- initial_split(houses, .8))

treino <- training(houses_split)
teste <- testing(houses_split)
(dados_cv <- vfold_cv(treino))


(receita_dados <- 
  recipe(
    aluguel ~ .,
    data = treino) %>% 
  step_log(area, andar, quartos))

(prep_rec <- prep(receita_dados))

(modelo_spec_rand <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 300) %>%
  set_mode("regression") %>%
  set_engine("ranger"))

modelo_lin_reg <- linear_reg() %>%
  set_engine("lm")

(fluxo <- workflow() %>%
  add_recipe(prep_rec) %>%
  add_model(modelo_spec_rand))


doParallel::registerDoParallel()

(expand_grid(
  mtry = 3:8,
  min_n = seq(20, 200, 20)) ->
    grade_params)

medidas <- metric_set(
  rpiq,
  ccc,
  mae,
  mpe,
  rsq,
  rmse
)

(tune_res <- tune_grid(
  object = fluxo,
  resamples = dados_cv,
  grid = grade_params,
  metrics = medidas,
  control = control_grid(save_pred = TRUE)) %T>%
    saveRDS("R/Dados/modelo_reg_houses.Rds"))

(collect_metrics(
  tune_res) %T>%
    saveRDS("R/Dados/metricas_reg_houses.Rds") ->
    metricas) 

(collect_predictions(tune_res) %T>%
    saveRDS("R/Dados/predicoes_reg_houses.Rds") -> 
    predicoes)
  
  
lm(aluguel ~ log(area) + log(quartos) + log(andar + 1) + mobiliado + banheiros,
   data = houses) -> 
  modelo 

modelo %>%
  summary() 

### tabular melhor modelo por métrica


file.create("tabelas/hiper_metricas.tex")

tabela <- file("tabelas/hiper_metricas.tex")

c(
  "rpiq",
  "ccc",
  "mae",
  "mpe",
  "rsq",
  "rmse") %>%
  map_dfr(
    ~ select_by_one_std_err(tune_res, metric = .x, .x) %>%
      mutate(metrica = toupper(.x)) %>%
      select(-.config)) %>%
  rename(
    `Variáveis por Árvore` = mtry,
    `Amostra Mínima para Folha` = min_n) %>%
  kable(
    "latex", 
    align = 'c',
    caption = 'Melhor modelo de acordo com cada métrica. Elaboração Própria.',
    label = 'tabela_metricas') %>%
  enc2utf8() %>%
  writeLines(tabela, useBytes = TRUE)

close(tabela)


