# return model
model_rf_ranger <- function(FE_data){
  rand_forest(trees = 100, mode = "classification") %>%
    set_engine("ranger") %>% 
    fit(Class~., train_data_FE)
}
