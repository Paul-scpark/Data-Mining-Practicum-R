### Parctice 10
### 9. Multi-class classification (Regularization)

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(Metrics)
library(caret)
library(randomForest)

## Loading data
path <- 'https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/'
ori_train <- read.csv(paste0(path, 'motion_train.csv'), header = T)
test <- read.csv(paste0(path, 'motion_test.csv'), header = T)

str(ori_train)
colnames(ori_train)

ori_train %>% 
  group_by(Activity) %>% 
  summarise(count = n()) %>% 
  arrange(count) %>% 
  ggplot(aes(x = reorder(Activity, -count), y = count)) +
  geom_bar(stat = 'identity') + xlab('Activity') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label = count), vjust = 1.5, colour = 'white')

## 문제 1. train과 valid dataset으로 나눠서 Logistic regression 분류기 만들기 (Multi-class)
sum(colSums(is.na(ori_train)))
sum(colSums(is.na(test)))

ori_train <- ori_train %>% 
  transform(laying = ifelse(Activity == 'LAYING', 1, 0), 
            sitting = ifelse(Activity == 'SITTING', 1, 0), 
            standing = ifelse(Activity == 'STANDING', 1, 0), 
            walking = ifelse(Activity == 'WALKING', 1, 0),
            walking_down = ifelse(Activity == 'WALKING_DOWNSTAIRS', 1, 0), 
            walking_up = ifelse(Activity == 'WALKING_UPSTAIRS', 1, 0))

train_idx <- createDataPartition(ori_train$Activity, p = c(0.8, 0.2), list = F)
train <- ori_train[train_idx, ]
valid <- ori_train[-train_idx, ]

table(train$Activity) / nrow(train)
table(valid$Activity) / nrow(valid)

train_X <- train %>% 
  select(-subject, -Activity, -laying, -sitting, -standing, -walking, -walking_down, -walking_up)
train_y <- train %>% 
  select(laying, sitting, standing, walking, walking_down, walking_up)

valid_X <- valid %>% 
  select(-subject, -Activity, -laying, -sitting, -standing, -walking, -walking_down, -walking_up)
valid_y <- valid %>% 
  select(laying, sitting, standing, walking, walking_down, walking_up)

model_list <- list()
for (i in 1:6){
  model_list[[colnames(train_y)[i]]] <- glm(data = cbind(train_y[i], train_X), 
                                            formula = paste0(colnames(train_y)[i], '~.'), 
                                            family = binomial(link = 'logit'))
}

valid_df <- data.frame(matrix("", nrow = nrow(valid_X)))[-1]
for (model in names(model_list)){
  valid_df <- cbind(valid_df, predict(model_list[model], newdata = valid_X, type = 'response'))
}

valid_df <- valid_df %>% 
  mutate(pred = apply(valid_df, 1, which.max), 
         pred = ifelse(pred == 1, 'LAYING', 
                       ifelse(pred == 2, 'SITTING', 
                              ifelse(pred == 3, 'STANDING', 
                                     ifelse(pred == 4, 'WALKING', 
                                            ifelse(pred == 5, 'WALKING_DOWNSTAIRS', 'WALKING_UPSTAIRS'))))),
         actual = valid$Activity)

table(valid_df$pred, valid_df$actual)
mean(valid_df$pred == valid_df$actual)

test_df <- data.frame(matrix("", nrow = nrow(test)))[-1]
for (model in names(model_list)){
  test_df <- cbind(test_df, predict(model_list[model], newdata = test[, !names(test) %in% c('subject', 'Activity')], type = 'response'))
}

test_df <- test_df %>% 
  mutate(pred = apply(test_df, 1, which.max), 
         pred = ifelse(pred == 1, 'LAYING', 
                       ifelse(pred == 2, 'SITTING', 
                              ifelse(pred == 3, 'STANDING', 
                                     ifelse(pred == 4, 'WALKING', 
                                            ifelse(pred == 5, 'WALKING_DOWNSTAIRS', 'WALKING_UPSTAIRS'))))),
         actual = test$Activity)

table(test_df$pred, test_df$actual)
mean(test_df$pred == test_df$actual)

### 문제 2. Feature Engineering을 통해서 모델을 최적화해보기
# CASE 1. motion 별로 값에 차이가 큰 변수들을 추려보기
train_sd <- train %>% 
  select(-laying, -sitting, -standing, -walking, -walking_down, -walking_up) %>% 
  group_by(Activity) %>% 
  summarise_each(funs(sd))

train_sd <- cbind(c('LAYING', 'SITTING', 'STANDING', 'WALKING', 'WALKING_DOWNSTAIRS', 'WALKING_UPSTAIRS', 'STD'), 
                  rbind(train_sd[, -1], apply(train_sd[, -1], 2, sd)))

colnames(train_sd)[1] <- 'variable'
train_sd <- train_sd[, names(train_sd) != 'subject']
train_sd <- as.data.frame(t(train_sd))
colnames(train_sd) <- train_sd[1, ]
train_sd <- train_sd[-1, ]
train_sd$variable <- rownames(train_sd)
train_sd[, 1:7] <- as.data.frame(sapply(train_sd[, 1:7], as.double))
rownames(train_sd) <- NULL

target_variable <- train_sd %>% 
  filter(STD >= 0.13) %>% 
  select(variable) %>% 
  c()

var1 <- target_variable$variable[1:12]
var2 <- target_variable$variable[13:24]

train %>% 
  select(var1, Activity) %>% 
  gather(key, value, 1:12) %>% 
  ggplot(aes(x = value, fill = Activity)) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~ key, scales = 'free_y')

train %>% 
  select(var2, Activity) %>% 
  gather(key, value, 1:12) %>% 
  ggplot(aes(x = value, fill = Activity)) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~ key, scales = 'free_y')

model_list <- list()
for (i in 1:6){
  model_list[[colnames(train_y)[i]]] <- glm(data = cbind(train_y[i], train_X[, names(train_X) %in% target_variable$variable]), 
                                            formula = paste0(colnames(train_y)[i], '~.'), 
                                            family = binomial(link = 'logit'))
}

valid_df <- data.frame(matrix("", nrow = nrow(valid_X)))[-1]
for (model in names(model_list)){
  valid_df <- cbind(valid_df, predict(model_list[model], newdata = valid_X, type = 'response'))
}

valid_df <- valid_df %>% 
  mutate(pred = apply(valid_df, 1, which.max), 
         pred = ifelse(pred == 1, 'LAYING', 
                  ifelse(pred == 2, 'SITTING', 
                    ifelse(pred == 3, 'STANDING', 
                      ifelse(pred == 4, 'WALKING', 
                        ifelse(pred == 5, 'WALKING_DOWNSTAIRS', 'WALKING_UPSTAIRS'))))),
         actual = valid$Activity)

table(valid_df$pred, valid_df$actual)
mean(valid_df$pred == valid_df$actual)

# CASE 2. Random Forest 알고리즘을 통해 Feature Importance 확인
new_train <- train[, !names(train) %in% c('subject', 'laying', 'sitting', 'standing', 'walking', 'walking_down', 'walking_up')]
new_train$Activity <- as.factor(new_train$Activity)
rf_model <- randomForest(new_train$Activity ~ ., data = new_train, )
varImpPlot(rf_model)

rf_importance <- as.data.frame(rf_model$importance)
rf_importance$variable <- rownames(rf_importance)
rf_importance <- rf_importance[order(rf_importance$MeanDecreaseGini),]
input_variable <- tail(rf_importance$variable, 30)

var1 <- input_variable[1:12]
var2 <- input_variable[13:24]

train %>% 
  select(var1, Activity) %>% 
  gather(key, value, 1:12) %>% 
  ggplot(aes(x = value, fill = Activity)) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~ key, scales = 'free_y')

train %>% 
  select(var2, Activity) %>% 
  gather(key, value, 1:12) %>% 
  ggplot(aes(x = value, fill = Activity)) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~ key, scales = 'free_y')

model_list <- list()
for (i in 1:6){
  model_list[[colnames(train_y)[i]]] <- glm(data=cbind(train_y[i],train_X[,names(train_X) %in% input_variable]), 
                                            formula = paste0(colnames(train_y)[i], '~.'), 
                                            family = binomial(link = 'logit'))
}

valid_df <- data.frame(matrix("", nrow = nrow(valid_X)))[-1]
for (model in names(model_list)){
  valid_df <- cbind(valid_df, predict(model_list[model], newdata = valid_X, type = 'response'))
}

valid_df <- valid_df %>% 
  mutate(pred = apply(valid_df, 1, which.max), 
         pred = ifelse(pred == 1, 'LAYING', 
                  ifelse(pred == 2, 'SITTING', 
                    ifelse(pred == 3, 'STANDING', 
                      ifelse(pred == 4, 'WALKING', 
                        ifelse(pred == 5, 'WALKING_DOWNSTAIRS', 'WALKING_UPSTAIRS'))))),
         actual = valid$Activity)

table(valid_df$pred, valid_df$actual)
mean(valid_df$pred == valid_df$actual)

### 문제 3. Feature Engineering 후, test 데이터에 모델 적용해보기
final_train_X <- ori_train %>% 
  select(-subject, -Activity, -laying, -sitting, -standing, -walking, -walking_down, -walking_up)
final_train_y <- ori_train %>% 
  select(laying, sitting, standing, walking, walking_down, walking_up)

model_list <- list()
for (i in 1:6){
  model_list[[colnames(final_train_y)[i]]] <- glm(data=cbind(final_train_y[i],
                                                             final_train_X[,names(final_train_X) %in% input_variable]), 
                                                  formula = paste0(colnames(final_train_y)[i], '~.'), 
                                                  family = binomial(link = 'logit'))
}

test_df <- data.frame(matrix("", nrow = nrow(test)))[-1]
for (model in names(model_list)){
  test_df <- cbind(test_df, predict(model_list[model], newdata = test[, names(test) %in% input_variable], type = 'response'))
}

test_df <- test_df %>% 
  mutate(pred = apply(test_df, 1, which.max), 
         pred = ifelse(pred == 1, 'LAYING', 
                  ifelse(pred == 2, 'SITTING', 
                    ifelse(pred == 3, 'STANDING', 
                      ifelse(pred == 4, 'WALKING', 
                        ifelse(pred == 5, 'WALKING_DOWNSTAIRS', 'WALKING_UPSTAIRS'))))),
         actual = test$Activity)

table(test_df$pred, test_df$actual)
mean(test_df$pred == test_df$actual)