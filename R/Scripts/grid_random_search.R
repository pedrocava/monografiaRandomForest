library(tidyverse)
library(patchwork)

(grid <- expand_grid(
  A = 1:10,
  B = 1:10))

(random <- tibble(
  A = runif(100, 1, 10),
  B = runif(100, 1, 10)))

grid %>%
  ggplot(aes(x = A, y = B)) +
  geom_point(size = 2, color = cor) +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "Grid Search") +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) ->
  gridgg

random %>%
  ggplot(aes(x = A, y = B)) +
  geom_point(size = 2, color = cor) +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "Random Search") +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) ->
  randomgg

gridgg + randomgg
