'''
2021-2 - Data Mining Practicum
Prof. Hyebong Choi

Ch 8. Logistic Regression
'''

library(dplyr)
library(tidyr)
library(ggplot2)

### Data loading
path <- 'https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/'
admission <- read.csv(paste0(path, 'ex2data1.txt'), header = F, col.names = c('exam1', 'exam2', 'admitted'))

admission %>% 
  ggplot(aes(x = exam1, y = exam2, col = as.factor(admitted))) + 
  geom_point() +
  scale_color_manual('admitted', values = c('grey', 'skyblue'), labels = c('not admitted', 'admitted')) + 
  theme_classic()

## Initial setting
theta_vector <- c(0, 0, 0)

feature.df <- admission %>% 
  mutate(bias = 1) %>% 
  select(bias, exam1, exam2)

labelVector <- admission$admitted

str(feature.df)
str(labelVector)

## Hypothesis and Cost function
sigmoid <- function(x){1 / (1 + exp(-x))}
h <- function(x, theta_vector){sigmoid(theta_vector %*% x)}

costFunction <- function(theta_vector){
  hx <- apply(feature.df, 1, function(x){h(x, theta_vector)})
  costV <- labelVector * log(hx) + (1 - labelVector) * log(1 - hx)
  - mean(costV)
}

costFunction(theta_vector)

## Feature Normalization
feature.df[, 2:3] <- sapply(feature.df[, 2:3], function(x){(x - mean(x)) / sd(x)})
summary(feature.df)

## Gradient Descent
theta_vector <- c(0, 0, 0)
costDF <- data.frame(iter = 0, 
                     t0 = theta_vector[1], 
                     t1 = theta_vector[2], 
                     t2 = theta_vector[3], 
                     cost = costFunction(theta_vector))

# Find Optimal theta
num_iter <- 1000
m <- nrow(feature.df)
alpha <- 1

for (i in 1:num_iter){
  theta_update <- (t(as.matrix(feature.df)) %*% 
                     (apply(feature.df, 1, function(x){h(x, theta_vector)}) - labelVector)) / m * alpha
  theta_vector <- theta_vector - theta_update[, 1]
  costDF <- rbind(costDF, c(i, theta_vector, costFunction(theta_vector)))
}

## Learning Curve
costDF %>% 
  ggplot(aes(x = iter, y = cost)) + 
  geom_line() + 
  ggtitle('Learning Curve') + 
  theme_bw()

## Decision Boundary
slope <- -theta_vector[2] / theta_vector[3]
bias <- -theta_vector[1] / theta_vector[3]

ggplot(feature.df, aes(x = exam1, y = exam2)) + 
  geom_point(aes(col = as.factor(labelVector))) + 
  geom_abline(slope = slope, intercept = bias, col = 'red') + 
  scale_color_manual('admitted', values = c('grey', 'skyblue'), labels = c('not admitted', 'admitted')) + 
  ggtitle(sprintf("%.2f + %.2f * x1 + %.2f * x2 = 0", theta_vector[1], theta_vector[2], theta_vector[3])) + 
  theme_classic()

costFunction(theta_vector)
admission$test <- ifelse(as.matrix(feature.df) %*% t(t(as.matrix(theta_vector))) > 0, 1, 0)

## Decision Boundary with Original Scale
mu1 <- mean(admission$exam1)
mu2 <- mean(admission$exam2)
sig1 <- sd(admission$exam1)
sig2 <- sd(admission$exam2)

theta_origin <- 

slope <- 
bias <- 
  
admission %>% 
  ggplot(aes(x = exam1, y = exam2)) + 
  geom_point(aes(col = as.factor(labelVector))) + 
  geom_abline(slope = slope, intercept = bias, col = 'red') + 
  scale_color_manual('admitted', values = c('grey', 'skyblue'), labels = c('not admitted', 'admitted')) + 
  ggtitle(sprintf("%.2f + %.2f * x1 + %.2f * x2 = 0", theta_vector[1], theta_vector[2], theta_vector[3])) + 
  theme_classic()