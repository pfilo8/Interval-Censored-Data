library(tidyverse)
library(ggplot2)
library(latex2exp)
library(patchwork)

source('src/generator.R')
source('src/plots.R')

lambda_vector <- seq(1, 10, 1)
nu_vector <- seq(1, 10, 1)
count <- 1:length(lambda_vector)
mc <- seq(1, 100, 1)

means_for_one_generation <- c()
vars_for_one_generation <- c()
bias_for_one_generation <- c()
mean_sq_for_one_generation <- c()
means <- matrix(0,length(lambda_vector),length(lambda_vector))
vars <- matrix(0,length(lambda_vector),length(lambda_vector))
bias <- matrix(0,length(lambda_vector),length(lambda_vector))
mean_sq <- matrix(0,length(lambda_vector),length(lambda_vector))

for (i in count){
  for (j in count){
    for (m in mc){
      data <- generate_censored_data(lambda_vector[i], nu_vector[j], 100)
      means_for_one_generation[m] <- mean(data$intervals$right[1:length(data$intervals$right)-1])
      vars_for_one_generation[m] <- var(data$intervals$right[1:length(data$intervals$right)-1])
      bias_for_one_generation[m] <- mean(data$intervals$right[1:length(data$intervals$right)-1] - lambda_vector)
      mean_sq_for_one_generation[m] <- mean((data$intervals$right[1:length(data$intervals$right)-1] - lambda_vector)^2)
    }
    means[i,j] <- mean(means_for_one_generation)
    vars[i,j] <- mean(vars_for_one_generation)
    bias[i,j] <- mean(bias_for_one_generation)
    mean_sq[i,j] <- mean(mean_sq_for_one_generation)
  }
}

write.csv(means, 'means.csv', row.names=FALSE)
write.csv(vars, 'vars.csv', row.names=FALSE)
write.csv(bias, 'bias.csv', row.names=FALSE)
write.csv(mean_sq, 'mean_sq.csv', row.names=FALSE)


data.frame(lambda_vector ,means[ ,1])  %>%
  ggplot(aes(x = lambda_vector, y = means[ ,1])) +
  geom_point() +
  geom_line() +
  xlab(TeX('$\\lambda$ - lightbulb failure rate')) +
  ylab(TeX('Mean of naive estimator of $\\lambda$')) +
  ggtitle('Mean of naive estimator of the failure rate with changing $\\lambda$') +
  theme_minimal()

data.frame(nu_vector ,means[1, ])  %>%
  ggplot(aes(x = lambda_vector, y = means[1, ])) +
  geom_point() +
  geom_line() +
  xlab(TeX('$\\nu$ - inspection rate')) +
  ylab(TeX('Mean of naive estimator of $\\lambda$')) +
  ggtitle('Mean of naive estimator of the failure rate with changing $\\nu$') +
  theme_minimal()


data.frame(lambda_vector ,vars[ ,1])  %>%
  ggplot(aes(x = lambda_vector, y = vars[ ,1])) +
  geom_point() +
  geom_line() +
  xlab(TeX('$\\lambda$ - lightbulb failure rate')) +
  ylab(TeX('Variance  of naive estimator of $\\lambda$')) +
  ggtitle('Variance  of naive estimator of the failure rate with changing $\\lambda$') +
  theme_minimal()

data.frame(nu_vector ,vars[1, ])  %>%
  ggplot(aes(x = lambda_vector, y = vars[1, ])) +
  geom_point() +
  geom_line() +
  xlab(TeX('$\\nu$ - inspection rate')) +
  ylab(TeX('Variance of naive estimator of $\\lambda$')) +
  ggtitle('Variance of naive estimator of the failure rate with changing $\\nu$') +
  theme_minimal()
  

data.frame(lambda_vector ,bias[ ,1])  %>%
  ggplot(aes(x = lambda_vector, y = bias[ ,1])) +
  geom_point() +
  geom_line() +
  xlab(TeX('$\\lambda$ - lightbulb failure rate')) +
  ylab(TeX('Bias of naive estimator of $\\lambda$')) +
  ggtitle('Bias of naive estimator of the failure rate with changing $\\lambda$') +
  theme_minimal()

data.frame(nu_vector ,bias[1, ])  %>%
  ggplot(aes(x = lambda_vector, y = bias[1, ])) +
  geom_point() +
  geom_line() +
  xlab(TeX('$\\nu$ - inspection rate')) +
  ylab(TeX('Bias of naive estimator of $\\lambda$')) +
  ggtitle('Bias of naive estimator of the failure rate with changing $\\nu$') +
  theme_minimal()


data.frame(lambda_vector ,mean_sq[ ,1])  %>%
  ggplot(aes(x = lambda_vector, y = mean_sq[ ,1])) +
  geom_point() +
  geom_line() +
  xlab(TeX('$\\lambda$ - lightbulb failure rate')) +
  ylab(TeX('Mean square error of naive estimator of $\\lambda$')) +
  ggtitle('Mean square error of naive estimator of the failure rate with changing $\\lambda$') +
  theme_minimal()

data.frame(nu_vector ,mean_sq[1, ])  %>%
  ggplot(aes(x = lambda_vector, y = mean_sq[1, ])) +
  geom_point() +
  geom_line() +
  xlab(TeX('$\\nu$ - inspection rate')) +
  ylab(TeX('Mean square error of naive estimator of $\\lambda$')) +
  ggtitle('Mean square error of naive estimator of the failure rate with changing $\\nu$') +
  theme_minimal()
