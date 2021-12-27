'''
2021-2 - Data Mining Practicum
Prof. Hyebong Choi

Ch 7. Linear Regression
'''

library(dplyr)
library(tidyr)
library(ggplot2)

#### Linear Regression with univariate variables
path <- 'https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/'
exData <- read.csv(paste0(path, 'ex1data1.txt'), header = F, col.names = c('pop', 'profit'))

ggplot(exData, aes(x = pop, y = profit)) +
  geom_point(shape = 4, col = 'red')

## Hypothesis
# initial setting
theta0 <- 0
theta1 <- 1

# hypothesis
h <- function(x, t0, t1){
  t0 + t1 * x
}

ggplot(exData, aes(x = pop, y = profit)) +
  geom_point(shape = 4, col = 'red') +
  geom_abline(slope = theta1, intercept = theta1, col = 'blue')

## Cost Function
costJ <- function(t0, t1){
  m <- nrow(exData)
  1/(2*m) * sum((h(exData$pop, t0, t1) - exData$profit) ** 2)
}

# cost of initial setting
costJ(theta0, theta1)

# cost function for various theta1
costDF <- data.frame(t1 = seq(-1, 2, 0.1), 
                     cost = seq(-1, 2, 0.1) %>% 
                       sapply(function(x){costJ(theta0, x)}))

costDF %>% ggplot(aes(x = t1, y = cost)) + geom_point()

## Gradient
# find gradient
gradient_theta0 <- function(t0, t1){
  mean(h(exData$pop, t0, t1) - exData$profit)
}

gradient_theta1 <- function(t0, t1){
  mean((h(exData$pop, t0, t1) - exData$profit) * exData$pop)
}

gradient_theta0(theta0, theta1)
gradient_theta1(theta0, theta1)

## Gradient Descent
num_iter <- 1500
alpha <- 0.05

# initial setting
theta0 <- 0
theta1 <- 1

costDF <- data.frame(iter = 0, t0 = theta0, t1 = theta1, 
                     cost = costJ(theta0, theta1))

# gradient descent
for(i in 1:num_iter){
  theta0_update <- gradient_theta0(theta0, theta1)
  theta1_update <- gradient_theta1(theta0, theta1)
  
  # update
  theta0 <- theta0 - alpha * theta0_update
  theta1 <- theta1 - alpha * theta1_update
  
  costDF <- rbind(costDF, c(i, theta0, theta1, costJ(theta0, theta1)))
}

costDF[c(1:10, seq(100, 1500, 100)), ]

# visualization
costDF %>% ggplot(aes(x = iter, y = cost)) + geom_point(alpha = 0.5)

ggplot(exData, aes(x = pop, y = profit)) +
  geom_point(shape = 4, col = 'red') +
  geom_abline(slope = theta1, intercept = theta0, col = 'blue') +
  ggtitle(sprintf("h*(x) = %.2f + %.2f*x    cost  %.3f", round(theta0, 2), round(theta1, 2), costJ(theta0, theta1)))

# contour
costVisDF <- data.frame(t0 = rep(seq(-12, 4, 0.8), each = 21), t1 = rep(seq(0, 2, 0.1), 21))
costVisDF$cost <- mapply(FUN = costJ, costVisDF$t0, costVisDF$t1)

ggplot(costVisDF, aes(t0, t1, z = cost)) +
  geom_raster(aes(fill = log(cost))) +
  geom_contour(col = 'white', binwidth = 3) + 
  theme_bw() +
  scale_fill_gradient(low = 'red', high = 'white') +
  geom_point(data = costDF, aes(x = t0, y = t1), shape = 1, col = 'blue')

## Exercise
alpha <- 0.001
alpha <- 0.05
costDF <- data.frame(iter = 0, t0 = theta0, t1 = theta1, cost = costJ(theta0, theta1))

for (i in 1:num_iter){
  theta0_update <- gradient_theta0(theta0, theta1)
  theta1_update <- gradient_theta1(theta0, theta1)
  
  theta0 <- theta0 - alpha * theta0_update
  theta1 <- theta1 - alpha * theta1_update
  
  costDF <- rbind(costDF, c(i, theta1, theta1, costJ(theta0, theta1)))
}

###################### Univariate Linear Regression ######################
######################   Exercise - MY ALGORITHM    ######################
costJ <- function(df, feature_col, actual_col, t0, t1){
  1/(2*nrow(df)) * sum(((t0 + t1*df[, feature_col]) - df[, actual_col]) ** 2)
}

Linear_regression <- function(df, feature_col, actual_col, t0, t1, alpha, iter_num){
  output_df <- data.frame(iter = 0, t0 = t0, t1 = t1, cost = costJ(df, feature_col, actual_col, t0, t1))
  
  for (i in 1:iter_num){
    derivative0 <- alpha * mean((t0 + t1*df[, feature_col]) - df[, actual_col])
    derivative1 <- alpha * mean(((t0 + t1*df[, feature_col]) - df[, actual_col]) * df[, feature_col])
    t0 <- t0 - derivative0
    t1 <- t1 - derivative1
    
    output_df <- rbind(output_df, c(i, t0, t1, costJ(df, feature_col, actual_col, t0, t1)))
    
    if (output_df$cost[nrow(output_df) - 1] - output_df$cost[nrow(output_df)] <= 10e-4){
      break
    }
  }
  return(output_df) 
}

costDF <- Linear_regression(exData, 'pop', 'profit', 0, 1, 0.01, 1500)
costDF %>% ggplot(aes(x = iter, y = cost)) + geom_point(alpha = 0.5)

costDF <- Linear_regression(exData, 'pop', 'profit', 0, 1, 0.001, 1500)
costDF %>% ggplot(aes(x = iter, y = cost)) + geom_point(alpha = 0.5)

costDF <- Linear_regression(exData, 'pop', 'profit', 0, 1, 0.0001, 1500)
costDF %>% ggplot(aes(x = iter, y = cost)) + geom_point(alpha = 0.5)

costDF <- Linear_regression(exData, 'pop', 'profit', 0, 1, 0.00001, 1500)
costDF %>% ggplot(aes(x = iter, y = cost)) + geom_point(alpha = 0.5)

##########################################################################

#### Linear Regression with multiple variables
path <- 'https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/'
exData2 <- read.csv(paste0(path, 'ex1data2.txt'), header = F, col.names = c('size', 'num_bedroom', 'price'))
summary(exData2)

# normalization
mean1 <- mean(exData2$size)
sd1 <- sd(exData2$size)
mean2 <- mean(exData2$num_bedroom)
sd2 <- sd(exData2$num_bedroom)

exData2$size.norm <- (exData2$size - mean1) / sd1
exData2$num_bedroom.norm <- (exData2$num_bedroom - mean2) / sd2

featureDF <- exData2 %>% 
  mutate(bias = 1) %>% 
  select(bias, ends_with('norm')) %>% 
  as.matrix()

label_vector <- exData2$price

# theta vector
theta_vector <- c(0, 0, 0)

# hypothesis
h <- function(x, theta_vector){
  # x is feature vector
  theta_vector %*% x # inner product of two vectors
}

# cost function
costFUN <- function(theta_vector){
  v <- as.matrix(featureDF) %*% theta_vector - label_vector
  1 / 2 * mean(v ** 2)
  
  #(t(v) %*% v / (2*nrow(featureDF)))[1, 1]
}

costFUN(theta_vector)

## Gradient Descent
costDF <- data.frame(iter = 0, 
                     t0 = theta_vector[1], 
                     t1 = theta_vector[2], 
                     t2 = theta_vector[3], 
                     cost = costFUN(theta_vector))

# find optimal theta
num_iter <- 1500
alpha <- 0.05
n <- nrow(featureDF)

for (i in 1:num_iter){
  v <- featureDF %*% theta_vector - label_vector
  theta_update <- alpha / n * t(featureDF) %*% v
  # theta_update <- alpha * n * t(featureDF) %*% (featureDF %*% theta_vector - labelVector)
  theta_vector <- theta_vector - theta_update
  costDF <- rbind(costDF, c(i, theta_vector, costFUN(theta_vector)))
}

label_ko_num <- function(num){
  ko_num <- function(x){
    new_num <- x %/% 10**10
    return(paste(new_num, 'x10^10', sep = ''))
  }
  return(sapply(num, ko_num))
}

costDF %>% ggplot(aes(x = iter, y = cost)) +
  geom_line() +
  scale_y_continuous(labels = label_ko_num)
