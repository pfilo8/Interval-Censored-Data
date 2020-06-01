library(tidyverse)
library(ggplot2)
library(latex2exp)
library(patchwork)

source('src/generator.R')
source('src/plots.R')

lambda_vector <- 3
nu_vector <- 3

data <- generate_censored_data(lambda_vector, nu_vector, 100)
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


num_row <- 1: length(data$intervals$left)
num_col <- 1: length(intervals_left)

n <- length(data$intervals$left)
m <- length(intervals_left)

A <- matrix(0,n,m)

for (k in num_row){
  for (l in num_col){
    if (turnbull_interval[l,1] >= org_intervals[k,1] & 
        turnbull_interval[l,3] <= org_intervals[k,3]){
      A[k,l] <- 1
    }
  }
}


count_interval_values <- function(A, m, eps=0.001, max_steps=100) {
  s <- rep(1/m, m)
  
  for (i in 1:max_steps) {
    d_j <- colSums(A*s/rowSums(A*s))
    n_j <- rev(cumsum(rev(d_j)))
    p_j <- (n_j - d_j)/ n_j
    S_j <- cumprod(p_j)
    s_temp <- c(1, S_j[1:length(S_j)-1]) - S_j
    if (sum(abs(s-s_temp)) < eps) {
      break
    }
    s <- s_temp
  }
  return(s)
}

count_interval_values(A, m)

