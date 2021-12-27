### Parctice 9
### 8. Logistic Regression

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(Metrics)

### 데이터 불러오기
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

feature.df[, 2:3] <- sapply(feature.df[, 2:3], function(x){(x - mean(x)) / sd(x)})
labelVector <- admission$admitted

## Hypothesis and Cost function
sigmoid <- function(x){1 / (1 + exp(-x))}
h <- function(x, theta_vector){sigmoid(theta_vector %*% x)}

costFunction <- function(theta_vector){
  hx <- apply(feature.df, 1, function(x){h(x, theta_vector)})
  costV <- labelVector * log(hx) + (1 - labelVector) * log(1 - hx)
  - mean(costV)
}

costFunction(theta_vector)

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
  ggtitle(sprintf("%.2f + %.2f * exam1 + %.2f * exam2 = 0", theta_vector[1], theta_vector[2], theta_vector[3])) + 
  theme_classic()

### Q1. Decision Boundary of original scale
mu1 <- mean(admission$exam1)
mu2 <- mean(admission$exam2)
sig1 <- sd(admission$exam1)
sig2 <- sd(admission$exam2)

theta_origin <- c(theta_vector[1] - (theta_vector[2]*mu1/sig1) - (theta_vector[3]*mu2/sig2), 
                  theta_vector[2] / sig1, 
                  theta_vector[3] / sig2)

slope <- -theta_origin[2] / theta_origin[3]
bias <- -theta_origin[1] / theta_origin[3]

ggplot(admission, aes(x = exam1, y = exam2)) + 
  geom_point(aes(col = as.factor(labelVector))) + 
  geom_abline(slope = slope, intercept = bias, col = 'red') + 
  scale_color_manual('admitted', values = c('grey', 'skyblue'), labels = c('not admitted', 'admitted')) + 
  ggtitle(sprintf("%.2f + %.2f * exam1 + %.2f * exam2 = 0", theta_origin[1], theta_origin[2], theta_origin[3])) + 
  theme_classic()

### Q2. Add input variables
Logistic_Regression <- function(feature_df, label, num_iter, alpha){
  # (1) Hypothesis와 Cost 함수 정의
  sigmoid <- function(x){1 / (1 + exp(-x))}
  h <- function(x, theta_vector){sigmoid(theta_vector %*% x)}
  
  costFunction <- function(theta_vector){
    hx <- apply(feature_df, 1, function(x){h(x, theta_vector)})
    costV <- label * log(hx) + (1 - label) * log(1 - hx)
    -mean(costV, na.rm = T)
  }
  
  # (2) feature_df의 열 개수만큼 theta_vector를 정의해주고, Cost 함수 결과로 output_df 만들기
  theta_vector <- rep(0, ncol(feature_df))
  
  theta_df <- as.data.frame(t(as.data.frame(theta_vector)))
  rownames(theta_df) <- 1
  colnames(theta_df) <- str_replace(colnames(theta_df), 'V', 't')
  
  output_df <- data.frame(iter = 0, cost = costFunction(theta_vector), alpha = alpha, theta_df)
  
  # (3) Iteration 횟수만큼 학습하면서 output_df에 결과 누적시키기
  m <- nrow(feature_df)
  for (i in 1:num_iter){
    theta_update <- (t(as.matrix(feature_df)) %*% 
                       (apply(feature_df, 1, function(x){h(x, theta_vector)}) - label)) / m * alpha
    theta_vector <- theta_vector - theta_update[, 1]
    output_df <- rbind(output_df, c(i, costFunction(theta_vector), alpha, theta_vector))
    
    # (4) Cost 함수의 감소 폭이 10e-4 보다 작으면 iteration 횟수와 상관 없이 학습 중단
    #if (output_df$cost[nrow(output_df) - 1] - output_df$cost[nrow(output_df)] <= 10e-4){break}
  }
  return (output_df)
}

feature.df <- admission[, 1:2]
feature.df[, 1:2] <- sapply(feature.df[, 1:2], function(x){(x - mean(x)) / sd(x)})
feature.df <- feature.df %>% 
  mutate(bias = 1, 
         exam1_square = exam1 ** 2, 
         exam2_square = exam2 ** 2, 
         exam1_2 = exam1 * exam2) %>%
  select(bias, exam1, exam2, exam1_square, exam2_square, exam1_2)

labelVector <- admission$admitted

alpha_vec <- c(0.001, 0.005, 0.01, 0.05, 0.1, 1)
alpha_list <- list()
for (alpha in alpha_vec){
  alpha_list[[paste0('alpha_', alpha)]] <- Logistic_Regression(feature.df, labelVector, 1000, alpha)
}

total_df <- data.frame()
for (i in alpha_list){
  total_df <- rbind(total_df, i)
}

total_df %>% 
  filter(cost != Inf) %>% 
  ggplot(aes(x = iter, y = cost, color = as.factor(alpha))) + 
  geom_line()

### Q3. Decision Boundary of Q2
theta_df <- total_df %>% 
  filter(alpha == 1) %>% 
  select(-alpha) %>% tail(1)

contour_df <- 
  data_frame(exam1 = rep(seq(min(feature.df$exam1), max(feature.df$exam2), length=100), times=100), 
             exam2 = rep(seq(min(feature.df$exam2), max(feature.df$exam2), length=100), each=100),
             z = theta_df$t1 + 
               theta_df$t2*exam1 + 
               theta_df$t3*exam2 + 
               theta_df$t4*exam1^2 + 
               theta_df$t5*exam2^2 +
               theta_df$t6*exam1*exam2)

ggplot(feature.df, aes(x = exam1, y = exam2)) + 
  geom_point(aes(col = as.factor(labelVector))) +
  geom_contour(data = contour_df, aes(x = exam1, y = exam2, z = z), breaks = c(0)) +
  ggtitle(sprintf("%.2f + %.2f * x1 + %.2f * x2 + %.2f * x1^2 + %.2f * x2^2 + %.2f * x1*x2 = 0", 
                  theta_df$t1, theta_df$t2, theta_df$t3, theta_df$t4, theta_df$t5, theta_df$t6)) + 
  theme_classic()

### Q4. Titanic data - classification model
train <- read.csv('Desktop/Data_Mining_Practicum/HW/train.csv')
test <- read.csv('Desktop/Data_Mining_Practicum/HW/test.csv')

ID <- test$PassengerId

colSums(is.na(train))
colSums(is.na(test))

train$name_label <- gsub("^.*, (.*?)\\..*$", "\\1", train$Name)
name_age <- aggregate(Age~name_label, train, mean)
colnames(name_age) <- c('name_label', 'Age_mean')

train <- train %>% 
  left_join(name_age, by = 'name_label') %>% 
  mutate(Age = ifelse(is.na(Age), Age_mean, Age)) %>% 
  select(-name_label, -Age_mean)

test$name_label <- gsub("^.*, (.*?)\\..*$", "\\1", test$Name)

test <- test %>% 
  left_join(name_age, by = 'name_label') %>% 
  mutate(Age = ifelse(is.na(Age), Age_mean, Age)) %>% 
  select(-name_label, -Age_mean)

test[is.na(test$Fare), ]

train %>% 
  filter(Embarked == 'S') %>% 
  filter(Pclass == 3) %>% 
  filter(Age >= 60) %>% 
  summarise(mean = mean(Fare))

test$Fare <- ifelse(is.na(test$Fare), 7.867, test$Fare)

colnames(train)
train <- train %>% 
  select(-PassengerId, -Name, -Ticket, -Cabin)

train <- train %>% 
  transform(Sex_male = ifelse(Sex == 'male', 1, 0), 
            Sex_female = ifelse(Sex == 'female', 1, 0), 
            Embarked_S = ifelse(Embarked == 'S', 1, 0), 
            Embarked_C = ifelse(Embarked == 'C', 1, 0), 
            Embarked_Q = ifelse(Embarked == 'Q', 1, 0)) %>% 
  select(-Sex, -Embarked) %>% 
  mutate(Age = log(Age), Fare = log(Fare)) %>% 
  mutate(Fare = ifelse(Fare == -Inf, 0, Fare))

test <- test %>% 
  select(-PassengerId, -Name, -Ticket, -Cabin)

test <- test %>% 
  transform(Sex_male = ifelse(Sex == 'male', 1, 0), 
            Sex_female = ifelse(Sex == 'female', 1, 0), 
            Embarked_S = ifelse(Embarked == 'S', 1, 0), 
            Embarked_C = ifelse(Embarked == 'C', 1, 0), 
            Embarked_Q = ifelse(Embarked == 'Q', 1, 0)) %>% 
  select(-Sex, -Embarked) %>% 
  mutate(Age = log(Age), Fare = log(Fare), bias = 1) %>% 
  mutate(Fare = ifelse(Fare == -Inf, 0, Fare))

# modeling
feature.df <- train %>% 
  mutate(bias = 1) %>% 
  select(-Survived)

labelVector <- train$Survived

alpha_vec <- c(0.001, 0.005, 0.01, 0.05, 0.1)
alpha_list <- list()
for (alpha in alpha_vec){
  alpha_list[[paste0('alpha_', alpha)]] <- Logistic_Regression(feature.df, labelVector, 1000, alpha)
}

total_df <- data.frame()
for (i in alpha_list){
  total_df <- rbind(total_df, i)
}

total_df %>% 
  filter(cost != Inf) %>% 
  ggplot(aes(x = iter, y = cost, color = as.factor(alpha))) + 
  geom_line()

### Q5. Titanic data - classification model apply
theta_df <- total_df %>% 
  filter(alpha == 0.1) %>% tail(1)
theta_df <- theta_df[4:14]

train$pred <- ifelse(as.matrix(feature.df) %*% t(as.matrix(theta_df)) > 0, 1, 0)
accuracy(train$Survived, train$pred)

test$Survived <- ifelse(as.matrix(test) %*% t(as.matrix(theta_df)) > 0, 1, 0)

df <- as.data.frame(cbind(PassengerId = ID, Survived = test$Survived))
colnames(df) <- c('PassengerId', 'Survived')
write.csv(df, '/Users/paul/Desktop/gender_submission.csv', row.names = FALSE)