library(mlbench)
data("Shuttle")
library(tidymodels)
# recipes for feature eng
data_set <- Shuttle
# predict HHV, or use credit_data to predict Status
# or create dataset of webtraffic and what they buy? Churn or something
# or shuttle
# Make better real life example by dropping random 

# NASA Shuttle database deals with the positioning of radiators169in the Space
# Shuttle. In the original, it consists of 43500 training objects and 14500 test
# objects. Each170instance is described by nine continuous attributes and is
# assigned to one of seven classes with the171frequencies

# this should probably be separate because no one would introduce missings 
# during the training on purpose.
# add missings
set.seed(22345)
len <- nrow(data_set)
data_set$V1[sample(x = 1:len,size = 10)] <- NA
data_set$V2[sample(x = 1:len,size = 10)] <- NA
data_set$V3[sample(x = 1:len,size = 10)] <- NA
data_set$V7[sample(x = 1:len,size = 10)] <- NA
data_set$V9[sample(x = 1:len,size = 10)] <- NA

# split
train_test_samples <- rsample::initial_split(data_set,prop = 3/4)
train_data <- training(train_test_samples)
test_data <- testing(train_test_samples)
rm(data_set, Shuttle)
# target Class
rec <- recipe(train_data, Class~.) %>% 
# Feature engineering
  # - deal with missings
    step_knnimpute(V1, V2, V3, V7, V9, 
                   neighbors = 6,
                   id = "Missing imputation") %>% 
    step_mutate(
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
      #Class = fct_collapse(Class, "other"=c("Bpv.Open", "Bpv.Close")),
      id = "Sign and logging"
    ) %>% 
# - scale
    step_YeoJohnson(all_numeric(),id = "transformation of vars") %>% 
# - transform yeo_something
    step_center(all_numeric(), id = "centering vars") %>% 
    step_scale(all_numeric(), id = "scaling vars")
# - partial least squares?
rec_prepped <- prep(rec, training = train_data)
# Train model
train_data_FE <- bake(rec_prepped, train_data)
test_data_FE <- bake(rec_prepped, test_data)
# cross validate and use resample to estimate best value
# predict on test set
# 

# it was actually extremely great performance by default.
# but for the sake of the argument let's find the best parameters for the 
# model
shuttle_ranger <- 
  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>% 
  fit(Class~., train_data_FE)

#%>% 
#  fit(Class~., train_data_FE)

# bst_grid <- grid_random(
#   trees() %>% range_set( c( 70,  200)), 
#   mtry() %>% range_set(c( 2,  9)), 
#   size = 10, original = TRUE
# )


prediction <- predict(shuttle_ranger, test_data_FE)
test_data_FE %>% 
  bind_cols(prediction) %>% 
  metrics(truth = Class, estimate = .pred_class)

prediction2 <- predict(shuttle_ranger, test_data_FE,type = "prob")

probs <- 
  test_data_FE %>% 
  bind_cols(prediction2)

plot <- probs %>% 
  gain_curve(Class, .pred_Rad.Flow:.pred_Bpv.Open) %>%
  autoplot() + 
  ggtitle("Gain curve for every class")

probs %>% 
  roc_curve(Class, .pred_Rad.Flow:.pred_Bpv.Open) %>%
  autoplot()
