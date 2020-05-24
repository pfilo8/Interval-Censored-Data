library(data.table)

source('src/generator.R')
source('src/analysis_helpers.R')

simulation <- function(lambda, nu, t_end, mcs_steps=100) {
  results_light_replacements <- c()
  results_light_without <- c()
  
  for (i in 1:mcs_steps){
    data <- generate_censored_data(lambda, nu, t_end)
    res1 <- count_light_replacement(data)
    res2 <- count_percentage_time_without_light(data)
    results_light_replacements <- c(results_light_replacements, res1)
    results_light_without <- c(results_light_without, res2)
  }
  
  return(c(mean(results_light_replacements),mean(results_light_without)))
}

results <- expand.grid(seq(1, 5, 2), seq(1, 5, 2), seq(100, 900, 100))
colnames(results) <- c("lambda", "nu", "T0")

x <- apply(results, 
           MARGIN = 1, 
           FUN = function(row) {
             simulation(
               row['lambda'], 
               row['nu'],
               row['T0']
             )
           }
)
x <- transpose(data.frame(x))
colnames(x) <- c('replacements', 'without')

results <- cbind(results, x)

write.csv(results, 'results/results_analysis_generator.csv', row.names=FALSE)
