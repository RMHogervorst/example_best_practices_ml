# This should send it to a service
# but for now it writes to normal out.
send_metric <- function(metric, value){
  print(paste(metric, "= ",value))
}

monitor_row_change <- function(dataframe, label){
  message = paste0(label, ".N_rows")
  value = nrow(dataframe)
  send_metric(metric = message, value = value)
}

monitor_metrics <- function(dataframe, label){
  walk2(dataframe$.metric, dataframe$.estimate, ~send_metric(metric = .x, value = .y))
}