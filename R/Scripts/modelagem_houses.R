

library(tidymodels)
library(tidyverse)
library(randomForest)
library(magrittr)
library(rpart)
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
  trees = tune()) %>%
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
  min_n = seq(20, 200, 20),
  trees = seq(100, 400, 100)) ->
    grade_params)

medidas <- metric_set(
  huber_loss,
  mae,
  mape,
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

metricas %>%
  filter(.metric == "mpe") %>%
  group_by(trees, min_n) %>%
  summarise(medida = mean(mean)) %>%
  pivot_wider(values_from = medida, names_from = min_n)

  theme_minimal() +
  scale_fill_manual(values = pal)

(collect_predictions(tune_res) -> 
    predicoes)
  
  houses
  
lm(aluguel ~ log(area) + log(quartos) + log(andar + 1) + mobiliado + banheiros,
   data = houses) -> 
  modelo 

modelo %>%
  summary() 

metricas %>%
  ggplot(aes(
    x = min_n, 
    group = min_n,
    fill = .metric, 
    y = mean)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  facet_wrap(~ .metric, scales = "free")

metricas %>%
  ggplot(aes(
    x = trees, 
    group = trees,
    fill = .metric, 
    y = mean)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  facet_wrap(~ .metric, scales = "free")

metricas %>%
  ggplot(aes(
    x = mtry, 
    group = mtry,
    fill = .metric, 
    y = mean)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  facet_wrap(~ .metric, scales = "free")

  
predicoes %>%
  summarise(dif = aluguel - .pred) %>%
  ggplot(aes(x = dif)) +
  geom_histogram(
    aes(y = ..density..), 
    binwidth = 50, fill = cor) +
  geom_vline(size = 1.2, xintercept = 0) +
  theme(legend.position = "none") +
  theme_minimal()


  
  
predicoes %>%
  ggplot(aes(x = .pred, y = aluguel)) +
  geom_density_2d(size = 1.2, color = cor) +
  geom_abline(
    size = 1.2, 
    slope = 1, 
    intercept = 0) +
  theme_minimal() +
  labs(
    x = "Valores previstos",
    y = "Valor real",
    title = "Distribuição conjunta das previsões e erros")
