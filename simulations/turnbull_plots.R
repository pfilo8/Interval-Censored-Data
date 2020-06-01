library(tidyverse)
library(ggplot2)
library(latex2exp)
library(patchwork)

source('src/generator.R')
source('src/plots.R')

means<-read.csv("results/means_turnbull.csv")
vars<-read.csv("results/vars_turnbull.csv")
bias<-read.csv("results/bias_turnbull.csv")
mean_sq<-read.csv("results/mean_sq_turnbull.csv")

means  %>%
  ggplot(aes(x = lambdas, y = means, group = nus, color = nus)) +
  geom_point() +
  geom_line() +
  geom_line(aes(y = lambdas), color = "red", linetype = "dotted") +
  xlab(TeX('$\\lambda$ - lightbulb failure rate')) +
  ylab(TeX('Mean of naive estimator of $\\lambda$')) +
  ggtitle('Mean of naive estimator') +
  theme_minimal()


vars  %>%
  ggplot(aes(x = lambdas, y = vars, group = nus, color = nus)) +
  geom_point() +
  geom_line() +
  xlab(TeX('$\\lambda$ - lightbulb failure rate')) +
  ylab(TeX('Variance  of naive estimator of $\\lambda$')) +
  ggtitle('Variance of naive estimator') +
  theme_minimal()

bias %>%
  ggplot(aes(x = lambdas, y = bias, group = nus, color = nus)) +
  geom_point() +
  geom_line() +
  geom_line(aes(y = 0), color = "red", linetype = "dotted") +
  xlab(TeX('$\\lambda$ - lightbulb failure rate')) +
  ylab(TeX('Bias of naive estimator of $\\lambda$')) +
  ggtitle('Bias of naive estimator') +
  theme_minimal()

mean_sq  %>%
  ggplot(aes(x = lambdas, y = mean_sq, group = nus, color = nus)) +  geom_point() +
  geom_line() +
  xlab(TeX('$\\lambda$ - lightbulb failure rate')) +
  ylab(TeX('Mean square error of naive estimator of $\\lambda$')) +
  ggtitle('Mean square error of naive estimator') +
  theme_minimal()

