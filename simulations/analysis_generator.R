library(data.table)

source('src/generator.R')
source('src/analysis_helpers.R')

simulation <- function(lambda, nu, t_end, mcs_steps=100) {
  results_light_replacements <- c()
  results_light_without <- c()
  results_inspections <- c()
  results_failures <- c()
  results_nonzero_left <- c()
  
  for (i in 1:mcs_steps){
    data <- generate_censored_data(lambda, nu, t_end)
    
    results_light_replacements <- c(results_light_replacements, count_light_replacement(data))
    results_light_without <- c(results_light_without, count_percentage_time_without_light(data))
    results_inspections <- c(results_inspections, count_number_of_inspections(data))
    results_failures <- c(results_failures, count_number_of_failures(data))
    results_nonzero_left <- c(results_nonzero_left, count_percentage_of_left_interval_nonzero(data))
  }
  
  return(c(
    mean(results_light_replacements),
    mean(results_light_without),
    mean(results_inspections),
    mean(results_failures),
    mean(results_nonzero_left)
    ))
}

results <- expand.grid(seq(1, 10, 1), seq(1, 10, 1), seq(100, 700, 300))
colnames(results) <- c("lambda", "nu", "T0")

x <- apply(results, 
           MARGIN = 1, 
           FUN = function(row) {
             simulation(
               row['lambda'], 
               row['nu'],
               row['T0'],
               300
             )
           }
)
x <- data.table::transpose(data.frame(x))
colnames(x) <- c('replacements', 'without', 'inspections', 'failures', 'nonzero.left')

results <- cbind(results, x)

write.csv(results, 'results/results_analysis_generator.csv', row.names=FALSE)
  