



library(tidyverse)
library(magrittr)
library(wesanderson)
library(stargazer)
library(knitr)
library(rpart)
library(rpart.plot)


pal <- wes_palette("Cavalcanti1")
cor <- wes_palette("Cavalcanti1") %>% 
  pluck(1)


(houses <- readRDS("R/Dados/houses_clean.Rds"))

file.create("tabelas/tabela_arvore_reg.tex")

tabela <- file("tabelas/tabela_arvore_reg.tex")




houses %>%
  group_by(cidade) %>%
  summarise(across(area:aceita_animal, function(v) round(mean(v), digits = 1))) %>%
  kable("latex") %>%
  enc2utf8() %>%
  writeLines(tabela, useBytes = TRUE)

close(tabela)


arvore <- rpart(aluguel ~ .,
                data = houses)
png("imagens/arvore_reg_casas.png")
rpart.plot(arvore)
dev.off()