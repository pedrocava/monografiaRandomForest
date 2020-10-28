

library(tidymodels)
library(randomForest)
library(magrittr)
library(rpart)
library("rpart.plot")

(houses <- readRDS("R/Dados/houses_clean.Rds"))


## Árvore de Decisão ------------------------

arvore <- rpart(aluguel ~ .,
                data = houses)

rpart.plot(arvore) 


## Tidymodels ------------------------------

houses_split <- initial_split(houses, .8)

treino <- training(houses_split)
teste <- testing(houses_split)
dados_cv <- vfold_cv(treino)


receita_dados <- 
  recipe(
    aluguel ~ .,
    data = treino) %>% 
  step_log(area, andar, -all_outcomes())

prep_rec <- prep(receita_dados)

modelo_spec_rand <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 500) %>%
  set_mode("regression") %>%
  set_engine("ranger")

modelo_lin_reg <- linear_reg() %>%
  set_engine("lm")

fluxo <- workflow() %>%
  add_recipe(prep_rec) %>%
  add_model(modelo_spec_rand)


doParallel::registerDoParallel()

(grade_params <- expand_grid(mtry = seq(5, 30, 3),
                            min_n = seq(4, 20, 2)))

(tune_res <- tune_grid(
  object = fluxo,
  resamples = dados_cv,
  grid = grade_params
))

tune_res %>%
  collect_metrics()


