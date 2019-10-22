# Data loading

load_data <- function(){
  # load the data -----
  library(mlbench)
  # NASA Shuttle database deals with the positioning of radiators in the Space
  # Shuttle. In the original, it consists of 43500 training objects and 14500 test
  # objects. Each instance is described by nine continuous attributes and is
  # assigned to one of seven classes with the171frequencies
  data("Shuttle")
  message("Loading shuttle data ...")
  data_set <- Shuttle
  
  data_set <- add_missing(data_set)

  # clean up
  detach("package:mlbench", unload=TRUE)
  monitor_row_change(data_set, label = "input_data")
  return(data_set) # not strictly necessary but makes it more clear  
}


add_missing <- function(data_set){
  # this should probably be separate because no one would introduce missings 
  # during the training on purpose.
  # add missings ----
  message("Make the data more real by removing values")
  set.seed(22345)
  len <- nrow(data_set)
  data_set$V1[sample(x = 1:len,size = 10)] <- NA
  data_set$V2[sample(x = 1:len,size = 10)] <- NA
  data_set$V3[sample(x = 1:len,size = 10)] <- NA
  data_set$V7[sample(x = 1:len,size = 10)] <- NA
  data_set$V9[sample(x = 1:len,size = 10)] <- NA
  # and return
  data_set
}