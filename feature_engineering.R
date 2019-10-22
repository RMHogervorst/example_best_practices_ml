# Feature engineering
create_FE_recipe <- function(train_data){
  # target Class, use everything else to predict
  # These next steps are only the procedure. We do not yet actually compute and 
  #  execute on the steps.
  rec <- recipe(train_data, Class~.) %>% 
    # - deal with missings
    step_knnimpute(V1, V2, V3, V7, V9, 
                   neighbors = 6,
                   id = "Missing imputation") %>% 
    step_mutate( # some manual reworking because I thought these were useful
      V2sign = V2 <0,
      V2 = log1p(abs(V2)),
      V5sign = V5 <0,
      V5 = log1p(abs(V5)),
      V6sign = V6 <0,
      V6 = log1p(abs(V6)),
      V7sign = V7 <0,
      V7 = sqrt(abs(V7)),
      V9sign = V9 < 0,
      V9 = log1p(abs(V9)),
      id = "Sign and logging"
    ) %>% 
    # - transform yeo_something
    step_YeoJohnson(all_numeric(),id = "transformation of vars") %>% 
    # - scale and center
    step_center(all_numeric(), id = "centering vars") %>% 
    step_scale(all_numeric(), id = "scaling vars")
  print_steps(rec)
  rec
}


print_steps <- function(recipe){
  steps <- recipe$steps %>% purrr::map_chr("id") %>% stringr::str_c(collapse = ", ")
  message(paste0("The recipe contains the following steps: ", steps))
}