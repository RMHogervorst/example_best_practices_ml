############### An ML example #####################
# In this example we will read in data
# split the data into train and test
# train a model on the train data data
# validate against the test data
# and return some metrics.


## Load the packages  ----
library(tidymodels)

# Load functions
source("load_data.R")

# split the data into train and test data ----
# This is not a time series so we don't care where the data is split.
set.seed(3456) # setting seed here, so we can run this part indepentenly
train_test_samples <- rsample::initial_split(data_set,prop = 3/4)
train_data <- training(train_test_samples)
test_data <- testing(train_test_samples)

# optionally remove data we no longer need.
rm(data_set, Shuttle)

# Feature Engineering -----
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

# Learn what the values should be, from the training data
rec_prepped <- prep(rec, training = train_data)

# Execute the Featuer engineering on the training and test data
# this might seem silly here:
# * we first tell it what to do in global
# * then learn what to do specifically
# * and only then execute 
# 
# And this is slightly overkill for here, But when you run multiple models, 
# or use resampling it makes a lot of sense to first specify your steps, 
# and execute them in seperate steps. 
# 
# Run feature Engineering on Test and training data
train_data_FE <- bake(rec_prepped, train_data)
test_data_FE <- bake(rec_prepped, test_data)



# Run a model on the training data. -----
shuttle_ranger <- 
  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>% 
  fit(Class~., train_data_FE)


### Metrics -----
prediction <- predict(shuttle_ranger, test_data_FE)
test_data_FE %>% 
  bind_cols(prediction) %>% 
  metrics(truth = Class, estimate = .pred_class)

# A slight exploration of performance per class
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
