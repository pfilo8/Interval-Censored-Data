library(tidyverse)
library(ggplot2)
library(latex2exp)
library(patchwork)

source('src/generator.R')
source('src/plots.R')

lambda_vector <- 1
nu_vector <- 1

data <- generate_censored_data(lambda_vector, nu_vector, 100)
vector_intervals <- c(data$intervals$left, data$intervals$right)

#nadajê wartoœci e
E <- matrix(0,length(vector_intervals),2)
count <- seq(length(vector_intervals))
for (i in count){
  E[i,1] <- vector_intervals[i]
  if (i < length(data$intervals$left)-1){
    E[i,2] <- 2
  } else {
    E[i,2] <- 1
  }
} 
E[i,2] <- -2

# porz¹dkujê
E_2 <- E[order(E[ ,1]), ]
#sprawdzam wartoœci e
count_2 <- seq(length(vector_intervals)-1)
for (j in count_2){
  if (E_2[j,1] == E_2[j+1,1] & E_2[j,2] > E_2[j+1,2]){
    a <- E_2[j, ]
    b <- E_2[j+1, ]
    E_2[j, ] <- b
    E_2[j+1, ] <- a
  }
}

#wybieram przedzia³y
intervals <- matrix(0,length(vector_intervals),2)
count_3 <- 1
for (k in count_2){
  if (E_2[k,2] != E_2[k+1,2]){
    intervals[count_3, ] <- E_2[k, ]
    intervals[count_3+1, ] <- E_2[k+1, ]
    count_3 <- count_3 +2
  }
}


