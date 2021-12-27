### Parctice 8
### 7. Linear Regression

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(stringr)
library(Metrics)
set.seed(2020)

## 1. Changing the learning rate
Linear_regression <- function(df, feature_col, actual_col, t0, t1, alpha, iter_num){
  # (1) Hypothesis 설정
  h <- function(feature_col, t0, t1){
    t0 + t1 * df[, feature_col]
  }
  
  # (2) Cost 함수 설정
  cost_J <- function(df, feature_col, actual_col, t0, t1){
    1/(2*nrow(df)) * sum( (h(feature_col, t0, t1) - df[, actual_col]) ** 2 )
  }
  
  # (3) 처음에 설정한 theta0, theta1을 통해서 나온 Cost 함수 결과로 output_df 틀 만들기
  output_df <- data.frame(iter = 0, t0 = t0, t1 = t1, alpha = alpha,
                          cost = cost_J(df, feature_col, actual_col, t0, t1))
  
  # (4) iteration 횟수만큼 학습하면서 output_df에 결과 누적시키기
  for (i in 1:iter_num){
    derivative0 <- alpha * mean( h(feature_col, t0, t1) - df[, actual_col] )
    derivative1 <- alpha * mean( (h(feature_col, t0, t1) - df[, actual_col]) * df[, feature_col] )
    
    t0 <- t0 - derivative0
    t1 <- t1 - derivative1
    
    output_df <- rbind(output_df, c(i, t0, t1, alpha, cost_J(df, feature_col, actual_col, t0, t1)))
  }
  
  return (output_df)
}

path <- 'https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/'
exData <- read.csv(paste0(path, 'ex1data1.txt'), header = F, col.names = c('pop', 'profit'))

# alpha_vec <- c(0.001, 0.005, 0.01, 0.05, 0.1, 1)
alpha_vec <- c(0.0001, 0.001, 0.005, 0.01)
alpha_list <- list()
for (alpha in alpha_vec){
  alpha_list[[paste0('alpha_', alpha)]] <- Linear_regression(exData, 'pop', 'profit', 0, 1, alpha, 1500)
}

total_df <- data.frame()
for (i in alpha_list){
  total_df <- rbind(total_df, i[, c('iter', 'cost', 'alpha')])
}

table(total_df$alpha)

total_df %>% 
  filter(cost != Inf) %>% 
  ggplot(aes(x = iter, y = cost, color = as.factor(alpha))) + 
  geom_line()

## 2. Cost function descrease smallert than very small number
Linear_regression <- function(df, feature_col, actual_col, t0, t1, alpha, iter_num){
  # (1) Hypothesis 설정
  h <- function(feature_col, t0, t1){
    t0 + t1 * df[, feature_col]
  }
  
  # (2) Cost 함수 설정
  cost_J <- function(df, feature_col, actual_col, t0, t1){
    1/(2*nrow(df)) * sum( (h(feature_col, t0, t1) - df[, actual_col]) ** 2 )
  }
  
  # (3) 처음에 설정한 theta0, theta1을 통해서 나온 Cost 함수 결과로 output_df 틀 만들기
  output_df <- data.frame(iter = 0, t0 = t0, t1 = t1, alpha = alpha,
                          cost = cost_J(df, feature_col, actual_col, t0, t1))
  
  # (4) iteration 횟수만큼 학습하면서 output_df에 결과 누적시키기
  for (i in 1:iter_num){
    derivative0 <- alpha * mean( h(feature_col, t0, t1) - df[, actual_col] )
    derivative1 <- alpha * mean( (h(feature_col, t0, t1) - df[, actual_col]) * df[, feature_col] )
    
    t0 <- t0 - derivative0
    t1 <- t1 - derivative1
    
    output_df <- rbind(output_df, c(i, t0, t1, alpha, cost_J(df, feature_col, actual_col, t0, t1)))
    
    # (5) Cost 함수의 감소 폭이 10e-4 보다 작으면 iteration 횟수와 상관 없이 학습 중단
    if (output_df$cost[nrow(output_df) - 1] - output_df$cost[nrow(output_df)] <= 10e-4){break}
  }
  
  return (output_df)
}

# alpha_vec <- c(0.001, 0.005, 0.01, 0.05, 0.1, 1)
alpha_vec <- c(0.001, 0.005, 0.01, 0.05)
alpha_list <- list()
for (alpha in alpha_vec){
  alpha_list[[paste0('alpha_', alpha)]] <- Linear_regression(exData, 'pop', 'profit', 0, 1, alpha, 1500)
}

total_df <- data.frame()
for (i in alpha_list){
  total_df <- rbind(total_df, i[, c('iter', 'cost', 'alpha')])
}

total_df %>% 
  filter(cost != Inf) %>% 
  ggplot(aes(x = iter, y = cost, color = as.factor(alpha))) + 
  geom_line()

## 3. Multiple variables Linear Regression
exData2 <- read.csv(paste0(path, 'ex1data2.txt'), header = F, col.names = c('size', 'num_bedroom', 'price'))
summary(exData2)

# normalization
exData2 <- exData2 %>% 
  mutate(size.norm = (size - mean(size)) / sd(size), 
         num_bedroom.norm = (num_bedroom - mean(num_bedroom)) / sd(num_bedroom))

featureDF <- exData2 %>% 
  mutate(bias = 1) %>% 
  select(bias, ends_with('norm')) %>% 
  as.matrix()

label_vector <- exData2$price

regression <- function(n, num_iter, alpha){
  # hypothesis
  h <- function(x, theta_vector){
    # x is feature vector
    theta_vector %*% x # inner product of two vectors
  }
  
  # cost function
  costFUN <- function(theta_vector){
    v <- as.matrix(featureDF) %*% theta_vector - label_vector
    (t(v) %*% v / (2*nrow(featureDF)))[1, 1]
    # 1 / 2 * mean(v ** 2)
  }
  
  theta_vector <- c(0, 0, 0)
  
  # Gradient Descent
  costDF <- data.frame(iter = 0, 
                       t0 = theta_vector[1], 
                       t1 = theta_vector[2], 
                       t2 = theta_vector[3], 
                       cost = costFUN(theta_vector), 
                       alpha = alpha)
  
  for (i in 1:num_iter){
    v <- featureDF %*% theta_vector - label_vector
    theta_update <- alpha / n * t(featureDF) %*% v
    theta_vector <- theta_vector - theta_update
    costDF <- rbind(costDF, c(i, theta_vector, costFUN(theta_vector), alpha))
  }
  
  return (costDF)
}

alpha_vec <- c(0.001, 0.005, 0.01, 0.05, 0.1, 1)
alpha_list <- list()
for (alpha in alpha_vec){
  alpha_list[[paste0('alpha_', alpha)]] <- regression(nrow(featureDF), 1500, alpha)
}

total_df <- data.frame()
for (i in alpha_list){
  total_df <- rbind(total_df, i[, c('iter', 'cost', 'alpha')])
}

label_ko_num <- function(num){
  ko_num <- function(x){
    new_num <- x %/% 10**10
    return(paste(new_num, 'x10^10', sep = ''))
  }
  return(sapply(num, ko_num))
}

total_df %>% ggplot(aes(x = iter, y = cost, color = as.factor(alpha))) +
  geom_line() +
  scale_y_continuous(labels = label_ko_num)

## 4. Housing price prediction - ALGORITHM
real_estate <- read_excel('/Users/paul/Desktop/Data_Mining_Practicum/data/Real_estate_valuation.xlsx')

# Simple Preprocessing
real_estate <- real_estate[, c(-1, -2, -6, -7)]
colnames(real_estate) <- c('House_age', 'Distance_MRT', 'Number_of_stores', 'Price')

real_estate <- real_estate %>% 
  mutate(House_age = (House_age - mean(House_age)) / sd(House_age), 
         Distance_MRT = (Distance_MRT - mean(Distance_MRT)) / sd(Distance_MRT), 
         Number_of_stores = (Number_of_stores - mean(Number_of_stores)) / sd(Number_of_stores))

# Splitting train, test dataset
train_idx <- sample(1:nrow(real_estate), size = 0.8 * nrow(real_estate), replace = F)
test_idx <- setdiff(1:nrow(real_estate), train_idx)

train_df <- real_estate[train_idx, ]
test_df <- real_estate[test_idx, ]

featureDF <- train_df %>% 
  mutate(bias = 1) %>% 
  select(-Price) %>% 
  as.matrix()

label_vector <- train_df$Price

regression <- function(feature_df, label, n, num_iter, alpha){
  # (1) Hypothesis 설정
  h <- function(x, theta_vector){ # x is feature vector
    theta_vector %*% x            # inner product of two vectors
  }
  
  # (2) Cost 함수 설정
  cost_J <- function(theta_vector){
    v <- as.matrix(feature_df) %*% theta_vector - label
    (t(v) %*% v / (2*nrow(feature_df)))[1, 1]
    # 1 / 2 * mean(v ** 2)
  }
  
  # (3) feature_df의 열의 개수만큼 theta_vector를 정의해주고, Cost 함수 결과로 output_df 틀 만들기
  theta_vector <- rep(0, ncol(feature_df))
  
  theta_df <- as.data.frame(t(as.data.frame(theta_vector)))
  rownames(theta_df) <- 1
  colnames(theta_df) <- str_replace(colnames(theta_df), 'V', 't')
  
  output_df <- data.frame(iter = 0, cost = cost_J(theta_vector), alpha = alpha)
  output_df <- cbind(output_df, theta_df)
  
  # (4) iteration 횟수만큼 학습하면서 output_df에 결과 누적시키기
  for (i in 1:num_iter){
    v <- feature_df %*% theta_vector - label
    theta_update <- alpha / n * t(feature_df) %*% v
    theta_vector <- theta_vector - theta_update
    output_df <- rbind(output_df, c(i, cost_J(theta_vector), alpha, theta_vector))
  
  # (5) Cost 함수의 감소 폭이 10e-4 보다 작으면 iteration 횟수와 상관 없이 학습 중단
  if (output_df$cost[nrow(output_df) - 1] - output_df$cost[nrow(output_df)] <= 10e-4){break}
  }
  
  return (output_df)
}

alpha_vec <- c(0.001, 0.005, 0.01, 0.05, 0.1, 1)
alpha_list <- list()
for (alpha in alpha_vec){
  alpha_list[[paste0('alpha_', alpha)]] <- regression(featureDF, label_vector, nrow(featureDF), 1500, alpha)
}

total_df <- data.frame()
for (i in alpha_list){
  total_df <- rbind(total_df, i)
}

table(total_df$alpha)

total_df %>% 
  filter(cost != Inf) %>% 
  ggplot(aes(x = iter, y = cost, color = as.factor(alpha))) + 
  geom_line()

total_df %>% 
  group_by(alpha) %>% 
  filter(iter == max(iter))

theta <- total_df %>% 
  filter(alpha == 0.01) %>% 
  filter(iter == max(iter)) %>% 
  select(t1, t2, t3, t4)

test_price <- test_df$Price
df <- test_df %>% 
  mutate(bias = 1) %>% 
  select(-Price)

test_df <- test_df %>% 
  mutate(price_predict = as.matrix(df) %*% unlist(theta, use.names = FALSE))

rmse(test_df$Price, test_df$price_predict)

## 5. Housing price prediction - lm function
train_df <- train_df[, c(-5)]
test_df <- test_df[, c(-5)]

model <- lm(Price ~ ., data = train_df)
test_df <- test_df %>% 
  mutate(price_predict = predict(model, newdata = test_df))

rmse(test_df$Price, test_df$price_predict)

theta_test <- t(as.data.frame(model$coefficients))
rownames(theta_test) <- 'lm_function'

theta <- theta[, c('t4', 't1', 't2', 't3')]
rownames(theta) <- 'Own_algorithm'
colnames(theta) <- colnames(theta_test)

rbind(theta_test, theta)
