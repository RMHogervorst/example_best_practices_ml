# Data loading

load_data <- function(){
  # load the data -----
  library(mlbench)
  data("Shuttle")
  data_set <- Shuttle
  
  
  # NASA Shuttle database deals with the positioning of radiators in the Space
  # Shuttle. In the original, it consists of 43500 training objects and 14500 test
  # objects. Each instance is described by nine continuous attributes and is
  # assigned to one of seven classes with the171frequencies
  
  # this should probably be separate because no one would introduce missings 
  # during the training on purpose.
  # add missings ----
  set.seed(22345)
  len <- nrow(data_set)
  data_set$V1[sample(x = 1:len,size = 10)] <- NA
  data_set$V2[sample(x = 1:len,size = 10)] <- NA
  data_set$V3[sample(x = 1:len,size = 10)] <- NA
  data_set$V7[sample(x = 1:len,size = 10)] <- NA
  data_set$V9[sample(x = 1:len,size = 10)] <- NA
  
  data_set  
}
