### modelagem


library(tidymodels)
library(tidyverse)
library(magrittr)

set.seed(1234)

dados <- readRDS("R/Dados/acidentes.Rds") %>%
  mutate(classificacao_adicente = if_else(classificacao_acidente == "Com Vítimas Fatais",
                                          true = "Fatal",
                                          false = "Não-Fatal")) %>%
  select(-pessoas, -mortos, -feridos_leves, 
         -feridos_graves, -ilesos, -ignorados,
         -dia_semana, -latitude, -longitude, -regional) %T>% {
  
  dados_split <<- initial_split(., .8)
  
  treino <<- training(dados_split)
  teste <<- testing(dados_split)
  dados_cv <- vfold_cv(treino)
  
}


receita_dados <- recipe(classificacao_acidente ~ ., data = dados) %>%
  update_role(id, new_role = "id") %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_date(data_inversa, features = c("dow", "month")) 

prep_rec <- prep(receita_dados)

juiced_rec <- juic(prep_rec)

  
  