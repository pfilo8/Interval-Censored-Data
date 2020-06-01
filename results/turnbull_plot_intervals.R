library(tidyverse)
library(ggplot2)
library(latex2exp)
library(patchwork)

source('src/generator.R')
source('src/plots.R')


  data <- generate_censored_data(4, 2, 50)
  vector_intervals <- c(data$intervals$left, data$intervals$right)
  
  E <- matrix(0,length(vector_intervals),2)
  count <- seq(length(vector_intervals))
  for (i in count){
    E[i,1] <- vector_intervals[i]
    if (i < length(data$intervals$left)+1){
      E[i,2] <- 2
    } else {
      E[i,2] <- 1
    }
  } 
  E[i,2] <- -2
  
  E_2 <- E[order(E[ ,1]), ]
  loop_num <- seq(length(vector_intervals)-1)
  for (j in loop_num){
    if (E_2[j,1] == E_2[j+1,1] & E_2[j,2] > E_2[j+1,2]){
      a <- E_2[j, ]
      b <- E_2[j+1, ]
      E_2[j, ] <- b
      E_2[j+1, ] <- a
    }
  }
  
  intervals_left <- c()
  intervals_right <- c()
  type_left <- c()
  type_right <- c()
  for (k in loop_num){
    if (E_2[k,2] == 2 & E_2[k+1,2] == 1 || E_2[k,2] == 2 & E_2[k+1,2] == -2 ){
      intervals_left <- c(intervals_left, E_2[k,1])
      type_left <- c(type_left, E_2[k,2])
      intervals_right <- c(intervals_right, E_2[k+1,1])
      type_right <- c(type_right, E_2[k+1,2])
    }
  }
  
  org_intervals <- data.frame(E[1:length(data$intervals$left), ],
                              E[length(data$intervals$left) + 1 : length(data$intervals$left) , ])
  turnbull_interval <- data.frame(intervals_left, type_left, intervals_right, type_right)
 
  
  
  write.csv(org_intervals , file="results/org_interval.csv",row.names=FALSE)
  write.csv(turnbull_interval, file="results/turnbull_interval.csv",row.names=FALSE)
  