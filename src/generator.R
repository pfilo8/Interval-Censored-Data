# Lambda - failure rate
# Nu - inspection rate

generate_censored_data <- function(lambda, nu, time_end) {
  time_now <- 0
  lightbulb_next_failure <- rexp(1, rate = lambda)
  
  inspection_times <- c()
  light_failures_times <- c(lightbulb_next_failure)
  intervals <- list()
  
  lightbulb_last_check <- 0
  lightbuld_last_change <- 0
  
  while (time_now < time_end){
    if (time_now > lightbulb_next_failure){
      # Save censored interval of failure
      intervals$left <- c(intervals$left, lightbulb_last_check - lightbuld_last_change)
      intervals$right <- c(intervals$right, time_now - lightbuld_last_change)
      intervals$censored <- c(intervals$censored, 1)
      # Change lightbuld and generate next failure time
      lightbuld_last_change <- time_now
      lightbulb_next_failure <- time_now + rexp(1, rate = lambda)
      # Save real time of future failure
      light_failures_times <- c(light_failures_times, lightbulb_next_failure)
    }
    lightbulb_last_check <- time_now
    inspection_times <- c(inspection_times, lightbulb_last_check)
    time_now <- time_now + rexp(1, rate = nu)
  }
  
  intervals$left <- c(intervals$left, lightbulb_last_check - lightbuld_last_change)
  intervals$right <- c(intervals$right, Inf)
  intervals$censored <- c(intervals$censored, 0)
  
  return(list(
    inspection_times=inspection_times, 
    light_failures_times=light_failures_times, 
    intervals=intervals,
    lambda=lambda,
    nu=nu,
    time_end=time_end))
}
