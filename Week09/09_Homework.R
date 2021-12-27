library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(corrplot)
library(cowplot)
library(rpart)
library(rpart.plot)
library(party)
library(caret)
library(e1071)
library(adabag)
library(randomForest)
library(xgboost)

train <- read.csv('/Users/paul/Desktop/week10/movies_train.csv')
test <- read.csv('/Users/paul/Desktop/week10/movies_test.csv')

## Preprocessing
str(train)
length(unique(train$distributor))

train %>% 
  filter(str_detect(distributor, '마인스')) %>% 
  select(title, distributor, genre, director, box_off_num)

remove <- c('[(주)]', '[(유)]', '[:blank:]')
train <- train %>% 
  mutate(distributor = str_remove_all(distributor, paste(remove, collapse = "|")), 
         distributor = ifelse(str_detect(distributor, '조이앤'), '조이앤시네마', 
                         ifelse(str_detect(distributor, '싸이더스'), '싸이더스', 
                            ifelse(str_detect(distributor, '쇼박스'), '쇼박스', 
                               ifelse(str_detect(distributor, '마운틴픽쳐스'), '마운틴픽쳐스', 
                                  ifelse(str_detect(distributor, 'CGV'), 'CGV아트하우스', 
                                     ifelse(str_detect(distributor, 'E&M'), 'CJE&M영화부문', distributor)))))))

test <- test %>% 
  mutate(distributor = str_remove_all(distributor, paste(remove, collapse = "|")), 
         distributor = ifelse(str_detect(distributor, '조이앤'), '조이앤시네마', 
                          ifelse(str_detect(distributor, '싸이더스'), '싸이더스', 
                            ifelse(str_detect(distributor, '쇼박스'), '쇼박스', 
                              ifelse(str_detect(distributor, '마운틴픽쳐스'), '마운틴픽쳐스', 
                                ifelse(str_detect(distributor, 'CGV'), 'CGV아트하우스', 
                                  ifelse(str_detect(distributor, 'E&M'), 'CJE&M영화부문', distributor)))))))

unique(train$genre)
unique(test$genre)

unique(train$screening_rat)
unique(test$screening_rat)

colSums(is.na(train))
colSums(is.na(test))

dir_bfnum_mean <- 
  bind_rows(train %>% select(director, dir_prev_bfnum), 
            test %>% select(director, dir_prev_bfnum)) %>% 
  group_by(director) %>% 
  mutate(mean = mean(dir_prev_bfnum, na.rm = T)) %>% 
  select(-dir_prev_bfnum) %>% 
  arrange(desc(mean)) %>% 
  filter(row_number() == 1)

train <- train %>% 
  left_join(dir_bfnum_mean, by = 'director') %>% 
  mutate(dir_prev_bfnum = ifelse(is.na(dir_prev_bfnum), mean, dir_prev_bfnum)) %>% 
  mutate(dir_prev_bfnum = ifelse(is.na(dir_prev_bfnum), 1, dir_prev_bfnum)) %>% 
  select(-mean)

test <- test %>% 
  left_join(dir_bfnum_mean, by = 'director') %>% 
  mutate(dir_prev_bfnum = ifelse(is.na(dir_prev_bfnum), mean, dir_prev_bfnum)) %>% 
  mutate(dir_prev_bfnum = ifelse(is.na(dir_prev_bfnum), 1, dir_prev_bfnum)) %>% 
  select(-mean)

hist1 <- train %>% 
  select(box_off_num) %>% 
  ggplot(aes(x = box_off_num)) + 
  geom_histogram()

hist2 <- train %>% 
  select(box_off_num) %>% 
  ggplot(aes(x = log(box_off_num))) + 
  geom_histogram()

title <- ggdraw() + 
  draw_label('box_off_num vs log(box_off_num)')
plot_grid(title, plot_grid(hist1, hist2), 
          ncol = 1, rel_heights = c(0.1, 1))

hist3 <- train %>% 
  select(dir_prev_bfnum) %>% 
  ggplot(aes(x = dir_prev_bfnum)) + 
  geom_histogram()

hist4 <- train %>% 
  select(dir_prev_bfnum) %>% 
  ggplot(aes(x = log(dir_prev_bfnum))) + 
  geom_histogram()

title <- ggdraw() + 
  draw_label('dir_prev_bfnum vs log(dir_prev_bfnum)')
plot_grid(title, plot_grid(hist3, hist4), 
          ncol = 1, rel_heights = c(0.1, 1))

hist5 <- train %>% 
  select(num_staff) %>% 
  ggplot(aes(x = num_staff)) + 
  geom_histogram()

hist6 <- train %>% 
  select(num_staff) %>% 
  ggplot(aes(x = log(num_staff))) + 
  geom_histogram()

title <- ggdraw() + 
  draw_label('num_staff vs log(num_staff)')
plot_grid(title, plot_grid(hist5, hist6), 
          ncol = 1, rel_heights = c(0.1, 1))

hist7 <- train %>% 
  select(num_actor) %>% 
  ggplot(aes(x = num_actor)) + 
  geom_histogram()

hist8 <- train %>% 
  select(num_actor) %>% 
  ggplot(aes(x = log(num_actor))) + 
  geom_histogram()

title <- ggdraw() + 
  draw_label('num_actor vs log(num_actor)')
plot_grid(title, plot_grid(hist7, hist8), 
          ncol = 1, rel_heights = c(0.1, 1))

train <- train %>% 
  mutate(num_staff = ifelse(num_staff == 0, 1, num_staff), 
         num_actor = ifelse(num_actor == 0, 1, num_actor)) %>% 
  mutate(dir_prev_bfnum = log(dir_prev_bfnum), 
         num_staff = log(num_staff), 
         num_actor = log(num_actor), 
         box_off_num = log(box_off_num))

test <- test %>% 
  mutate(num_staff = ifelse(num_staff == 0, 1, num_staff), 
         num_actor = ifelse(num_actor == 0, 1, num_actor)) %>% 
  mutate(dir_prev_bfnum = log(dir_prev_bfnum), 
         num_staff = log(num_staff), 
         num_actor = log(num_actor))

dist_median <- train %>% 
  group_by(distributor) %>% 
  summarise(median = median(box_off_num)) %>% 
  arrange(median) %>% 
  ungroup() %>% 
  mutate(distributor_rank = 1:n()) %>% 
  select(-median)

genre_mean <- train %>% 
  group_by(genre) %>% 
  summarise(mean = mean(box_off_num)) %>% 
  arrange(mean) %>% 
  ungroup() %>% 
  mutate(genre_rank = 1:n()) %>% 
  select(-mean)

train <- train %>% 
  left_join(dist_median, by = 'distributor') %>% 
  left_join(genre_mean, by = 'genre') %>% 
  select(-title, -distributor, -director, -release_time, -genre)
  
test <- test %>% 
  left_join(dist_median, by = 'distributor') %>% 
  left_join(genre_mean, by = 'genre') %>% 
  select(-distributor, -director, -release_time, -genre) %>% 
  mutate(distributor_rank = ifelse(is.na(distributor_rank), 0, distributor_rank))
  
train <- train %>% 
  mutate(over_12 = ifelse(screening_rat == '12세 관람가', 1, 0), 
         over_15 = ifelse(screening_rat == '15세 관람가', 1, 0), 
         over_19 = ifelse(screening_rat == '청소년 관람불가', 1, 0), 
         all = ifelse(screening_rat == '전체 관람가', 1, 0)) %>% 
  select(-screening_rat)

test <- test %>% 
  mutate(over_12 = ifelse(screening_rat == '12세 관람가', 1, 0), 
         over_15 = ifelse(screening_rat == '15세 관람가', 1, 0), 
         over_19 = ifelse(screening_rat == '청소년 관람불가', 1, 0), 
         all = ifelse(screening_rat == '전체 관람가', 1, 0)) %>% 
  select(-screening_rat)

train %>% 
  cor(use = 'complete.obs') %>% 
  corrplot(method = 'square')

## 1. Decision Tree
train_col <- colnames(train)
train[train_col] <- sapply(train[train_col], as.numeric)
test_col <- colnames(test)[-1]
test[test_col] <- sapply(test[test_col], as.numeric)

DT_regressor <- ctree(box_off_num ~ ., data = train)
plot(DT_regressor)

submission <- test %>% 
  mutate(box_off_num = exp(predict(DT_regressor, test[, c(-1)]))) %>% 
  select(title, box_off_num)

submission$box_off_num <- as.integer(submission$box_off_num)
write.csv(submission, '/Users/paul/Desktop/submission.csv', 
          fileEncoding = 'utf-8', row.names = FALSE)

## 2. Bagging
output_df <- data_frame()
for (num in seq(10, 50, 10)){
  for (method in c('cv', 'repeatedcv')){
    control <- trainControl(method = method, number = num)
    output <- train(box_off_num ~ ., data = train, method = 'treebag', trControl = control)
    output$result$num <- num
    output$result$method <- method
    output_df <- rbind(output_df, output$result)
  }
}

target <- output_df %>% arrange(RMSE) %>% filter(row_number() == 1)
control <- trainControl(method = target$method, number = target$num)
Bag_regressor <- train(box_off_num ~ ., data = train, method = 'treebag', trControl = control)

submission <- test %>% 
  mutate(box_off_num = exp(predict(Bag_regressor, test[, c(-1)]))) %>% 
  select(title, box_off_num)

submission$box_off_num <- as.integer(submission$box_off_num)
write.csv(submission, '/Users/paul/Desktop/submission.csv', 
          fileEncoding = 'utf-8', row.names = FALSE)

## 3. Random Forest
control <- trainControl(method = 'cv', number = 10)
RF_regressor <- train(box_off_num ~ ., data = train, method = 'rf', trControl = control)

importance <- varImp(RF_regressor, scale=FALSE)
print(importance)
plot(importance)

submission <- test %>% 
  mutate(box_off_num = exp(predict(RF_regressor, test[, c(-1)]))) %>% 
  select(title, box_off_num)

submission$box_off_num <- as.integer(submission$box_off_num)
write.csv(submission, '/Users/paul/Desktop/submission.csv', 
          fileEncoding = 'utf-8', row.names = FALSE)





control <- trainControl(method = 'cv', search = 'grid', number = 10)
grid <- expand.grid(mtry = 1:ncol(train)-1)
RF_regressor <- train(box_off_num ~ ., data = train, method = 'rf', 
                      trControl = control, tuneGrid = grid)

importance <- varImp(RF_regressor, scale=FALSE)
print(importance)
plot(importance)

submission <- test %>% 
  mutate(box_off_num = exp(predict(RF_regressor, test[, c(-1)]))) %>% 
  select(title, box_off_num)

submission$box_off_num <- as.integer(submission$box_off_num)
write.csv(submission, '/Users/paul/Desktop/submission.csv', 
          fileEncoding = 'utf-8', row.names = FALSE)

## 4. XGBoost
# CASE 1
grid <- expand.grid(nrounds = 1000, 
                    eta = seq(0.01, 0.3, 0.05), 
                    max_depth = seq(1:8), 
                    gamma = seq(0, 5, 1), 
                    colsample_bytree = 1, 
                    min_child_weight = 1, 
                    subsample = 1)

control <- trainControl(method = "cv", number = 10)
xgb_regressor <- train(box_off_num ~ ., data = train,
                       trControl = control, tuneGrid = grid, method = 'xgbTree')

submission <- test %>% 
  mutate(box_off_num = exp(predict(xgb_regressor, test[, c(-1)]))) %>% 
  select(title, box_off_num)

submission$box_off_num <- as.integer(submission$box_off_num)
write.csv(submission, '/Users/paul/Desktop/submission.csv', 
          fileEncoding = 'utf-8', row.names = FALSE)

# CASE 2
dtrain <- xgb.DMatrix(data = data.matrix(train[, c(-6)]), 
                      label = data.matrix(train[, c(6)]))
dtest <- xgb.DMatrix(data = data.matrix(test[, c(-1)]))

params <- list(booster = "gbtree", 
               objective = "reg:linear", 
               eta = 0.26,
               max_depth = 5, 
               gamma = 5)
xgb_base <- xgb.train(params = params,
                      data = dtrain,
                      nrounds =1000,
                      early_stopping_rounds = 50,
                      watchlist = list(train= dtrain))
submission <- test %>% 
  mutate(box_off_num = exp(predict(xgb_base, dtest))) %>% 
  select(title, box_off_num)

submission$box_off_num <- as.integer(submission$box_off_num)
write.csv(submission, '/Users/paul/Desktop/submission.csv', 
          fileEncoding = 'utf-8', row.names = FALSE)


'''
###
dtrain <- xgb.DMatrix(data = data.matrix(train[, c(-6)]), 
                      label = data.matrix(train[, c(6)]))
dtest <- xgb.DMatrix(data = data.matrix(test[, c(-1)]))

watchlist <- list(train = dtrain)
param <- list(
  objective = "reg:linear",
  eta = 0.3,
  max_depth = 8                
)

clf <- xgb.train(   
  params = param, 
  data = dtrain, 
  nrounds = 10000, 
  watchlist = watchlist,
  maximize = FALSE,
  early.stop.round = 100
)

###
dtrain <- xgb.DMatrix(data = data.matrix(train[, c(-6)]), 
                      label = data.matrix(train[, c(6)]))
dtest <- xgb.DMatrix(data = data.matrix(test[, c(-1)]))

params <- list(booster = "gbtree", 
               objective = "reg:linear")
xgb_base <- xgb.train(params = params,
                      data = dtrain,
                      nrounds =1000,
                      early_stopping_rounds = 50,
                      watchlist = list(train= dtrain))
                       
###
train_x <- data.matrix(train[, c(-6)])
train_y <- data.matrix(train[, c(6)])

xgb_regressor <- xgb.cv(data = train_x, label = train_y, objective = "reg:linear", nfold = 5, 
                        nrounds = 500, early_stopping_rounds = 150, verbose = F, prediction = T)

data.frame(iter = c(unlist(xgb_regressor$evaluation_log[, 1])), 
           train_RMSE_mean = c(unlist(xgb_regressor$evaluation_log[, 2])), 
           test_RMSE_mean = c(unlist(xgb_regressor$evaluation_log[, 4]))) %>% 
  gather(class, loss, 2:3) %>% 
  ggplot(aes(x = iter, y = loss, col = class)) + 
  geom_point(alpha = 0.2) + geom_smooth(alpha = 0.4, se = F)
'''