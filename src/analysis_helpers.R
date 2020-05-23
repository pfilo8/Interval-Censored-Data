count_light_replacement <- function(data){
  return(sum(data$intervals$censored))
}

count_percentage_time_without_light <- function(data){
  result <- cumsum(data$intervals$right) - data$light_failures_times
  result <- result[-length(result)]
  result <- sum(result)
  return(result/data$time_end)
}



data <- generate_censored_data(1/10, 1/20, 30)
