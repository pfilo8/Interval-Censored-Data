count_light_replacement <- function(data){
  return(sum(data$intervals$censored))
}

count_percentage_time_without_light <- function(data){
  result <- cumsum(data$intervals$right) - data$light_failures_times
  result <- sum(head(result, -1))
  return(result/data$time_end)
}

count_number_of_inspections <- function(data) {
  return(length(data$inspection_times)-1)
}

count_number_of_failures <- function(data) {
  return(length(data$light_failures_times))
}

count_percentage_of_left_interval_nonzero <- function(data) {
  return(mean(data$intervals$left == 0))
}

data <- generate_censored_data(1/10, 1/20, 30)
