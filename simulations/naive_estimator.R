library(tidyverse)
library(ggplot2)
library(latex2exp)
library(patchwork)

source('src/generator.R')
source('src/plots.R')

lambda_vector <- seq(1, 10, 1)
nu_vector <- seq(1, 10, 1)

mc <- seq(1, 100, 1)

lambdas <- c()
nus <- c()
means <- c()
vars <- c()
bias <- c()
mean_sq <- c()
means_for_one_generation <- c()
vars_for_one_generation <- c()
bias_for_one_generation <- c()
mean_sq_for_one_generation <- c()

for (i in lambda_vector){
  for (j in nu_vector){
    for (m in mc){
      data <- generate_censored_data(i,j, 1000)
      means_for_one_generation[m] <- mean(data$intervals$right[1:length(data$intervals$right)-1])
      vars_for_one_generation[m] <- var(data$intervals$right[1:length(data$intervals$right)-1])
      bias_for_one_generation[m] <- mean(data$intervals$right[1:length(data$intervals$right)-1] - i)
      mean_sq_for_one_generation[m] <- mean((data$intervals$right[1:length(data$intervals$right)-1] - i)^2)
    }
    lambdas <- c(lambdas, i)
    nus <- c(nus, j)
    means <- c(means, mean(means_for_one_generation))
    vars <- c(vars, mean(vars_for_one_generation))
    bias <- c(bias, mean(bias_for_one_generation))
    mean_sq <-  c(mean_sq, mean(mean_sq_for_one_generation))
    means_for_one_generation <- c()
    vars_for_one_generation <- c()
    bias_for_one_generation <- c()
    mean_sq_for_one_generation <- c()
  }
}


write.csv(data.frame(lambdas, nus, means), file="results/means.csv",row.names=FALSE)
write.csv(data.frame(lambdas, nus,vars), file="results/vars.csv",row.names=FALSE)
write.csv(data.frame(lambdas, nus,bias), file="results/bias.csv",row.names=FALSE)
write.csv(data.frame(lambdas, nus,mean_sq), file="results/mean_sq.csv",row.names=FALSE)
