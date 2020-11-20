
thicken <- function(
  .dat,
  var,
  min,
  max,
  n = 20) {
  
  var <- ensym(var)  
  
  valores_unicos <- .dat %>%
    pull(var) %>%
    unique() %>%
    length()
  
  if(valores_unicos != 1) rlang::abort("Values in selected column must be unique")
  
  if(.dat %>% pull(var) %>% length() > 1) {
    
    original <- .dat
    
    aditivo <- tibble({{var}} :=  seq(min, max, length.out = n - 1)) 
    
    original %>%
      select(-any_of(var)) %>%
      mutate({{var}} := rep(aditivo, times = nrow(.))) %>%
      unnest({{var}}) %>%
      return()
    
  } else {
    
    (.dat %>%
       select(- var) ->
       original)
    
    tibble({{var}} :=  seq(min, max, length.out = n - 1)) %>%
      cbind(original) %>%
      as_tibble() %>%
      bind_rows(.dat) %>%
      return()
      
  }
  
  
}

tibble(
  A = 1, 
  B = 5, 
  C = 7) %>%
  thicken(A, 0, 4) %>%
  thicken(C, -3, 12)
