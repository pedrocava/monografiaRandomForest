### modelagem


library(tidymodels)
library(workflows)
library(tidyverse)
library(vip)
library(magrittr)

set.seed(1234)

dados <- readRDS("R/Dados/acidentes.Rds") %>%
  mutate(classificacao_acidente = if_else(classificacao_acidente == "Com Vítimas Fatais",
                                          "Fatal",
                                          "Não-Fatal"),
         br = paste0("BR-", br)) %>%
  select(-pessoas, -mortos, -feridos_leves, -id, -delegacia,
         -municipio, -feridos_graves, -ilesos, -ignorados, -uop, -feridos,
         -tipo_acidente,-dia_semana, -latitude, -longitude, -regional, -data_inversa) %T>% {
  
  dados_split <<- initial_split(., .8)
  
  treino <<- training(dados_split)
  teste <<- testing(dados_split)
  dados_cv <<- vfold_cv(treino)
  
}


receita_dados <- recipe(classificacao_acidente ~ ., data = treino) %>% 
  step_dummy(all_nominal(), -all_outcomes())

prep_rec <- prep(receita_dados)

(juiced_rec <- juice(prep_rec))
  
modelo_spec_rand <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 500) %>%
  set_mode("classification") %>%
  set_engine("ranger")

modelo_spec_logit <- logistic_reg("classification") %>%
  set_engine("glm")

ciclo <- workflow() %>%
  add_recipe(prep_rec) %>%
  add_model(modelo_spec_rand) 


### --------------------

doParallel::registerDoParallel()

grade_params <- expand_grid(mtry = 8:15,
                            min_n = 4:10)

tune_res <- tune_grid(
  ciclo,
  resamples = dados_cv,
  grid = grade_params
)

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

best_auc <- select_best(tune_res, "roc_auc")

final_rf <- finalize_model(
  modelo_spec_rand,
  best_auc
)

final_rf

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(classificacao_acidente ~ .,
      data = juice(prep_rec)
  ) %>%
  vip(geom = "point")
