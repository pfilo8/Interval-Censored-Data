library(tidyverse)
library(ggplot2)
library(latex2exp)
library(patchwork)

source('src/generator.R')
source('src/plots.R')

turnbull <- function(lambda, nu) {
data <- generate_censored_data(lambda, nu, 500)
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


n <- length(data$intervals$left)
m <- length(intervals_left)

A <- matrix(0,n,m)

for (k in 1: n){
  for (l in 1:m){
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

prob <- count_interval_values(A, m)


length_intervals_decrease <- turnbull_interval[ ,3]- turnbull_interval[ ,1]
length_intervals_constant <- turnbull_interval[ ,1] - c(0, turnbull_interval[1:m-1 ,3])


heights <- 1
for (j in 1:m){
  heights <- c(heights, heights[j]-prob[j])
}

estimated_mean <- sum(length_intervals_constant*heights[1:m] + length_intervals_decrease*(heights[1:m]+heights[2:(m+1)])/2)
return(estimated_mean)
}



lambda_vector <- seq(1, 10, 1)
nu_vector <- seq(1, 10, 1)
mc <- seq(1, 100, 1)

lambdas <- c()
nus <- c()
means <- c()
vars <- c()
bias <- c()
mean_sq <- c()


for (j in 1:length(lambda_vector)){
  for (k in 1:length(nu_vector)){
    est_values <- c()
    for (i in mc){
      a <- turnbull(lambda_vector[j], nu_vector[k])
      if (a != Inf){
        est_values <- c(est_values, a)
      }
    }
    lambdas <- c(lambdas, lambda_vector[j])
    nus <- c(nus, nu_vector[k])
    means <- c(means, mean(est_values))
    vars <- c(vars, var(est_values))
    bias <- c(bias, mean(est_values - lambda_vector[j]))
    mean_sq <-  c(mean_sq, mean((est_values - lambda_vector[j])^2))
  }
}
# 
# 
# write.csv(data.frame(lambdas, nus, means), file="results/means_turnbull.csv",row.names=FALSE)
# write.csv(data.frame(lambdas, nus, vars), file="results/vars_turnbull.csv",row.names=FALSE)
# write.csv(data.frame(lambdas, nus, bias), file="results/bias_turnbull.csv",row.names=FALSE)
# write.csv(data.frame(lambdas, nus, mean_sq), file="results/mean_sq_turnbull.csv",row.names=FALSE)
