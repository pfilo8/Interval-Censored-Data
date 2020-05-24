realisation_of_the_process <- function(data) {
  ggplot(data.frame()) +
    geom_point(aes(x = data$inspection_times, 
                   y = rep_len(0, length(data$inspection_times)),
                   color = 'black')) +
    geom_point(aes(x = data$light_failures_times, 
                   y = rep_len(0, length(data$light_failures_times)), 
                   color = 'red')) +
    theme_minimal() +
    xlab(TeX('Time $t$')) +
    ylab('') +
    scale_color_manual(name = 'Type', 
                       values = c('black', 'red'), 
                       labels = c('Inspection', 'Lightbulb failed')) +
    theme(axis.text.y = element_blank(),
          legend.position = 'none')
}
