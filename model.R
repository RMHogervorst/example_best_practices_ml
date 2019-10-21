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
source("feature_engineering.R")

# split the data into train and test data ----
# This is not a time series so we don't care where the data is split.
set.seed(3456) # setting seed here, so we can run this part indepentenly
train_test_samples <- rsample::initial_split(data_set,prop = 3/4)
train_data <- training(train_test_samples)
test_data <- testing(train_test_samples)

# optionally remove data we no longer need.
rm(data_set, Shuttle)

# Feature Engineering -----
rec <- create_FE_recipe(train_data = train_data)
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
