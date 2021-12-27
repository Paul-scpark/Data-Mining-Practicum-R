library(keras)
library(tensorflow)
library(dplyr)
# install_keras()

## Loading data
load('multiclass.RData')
y <- t(y)

dim(x)
dim(y)

set.seed(12345)
rgrp <- runif(nrow(x))

train_x <- x[rgrp < 0.8, ]
train_y <- y[rgrp < 0.8, ]
test_x <- x[rgrp >= 0.8, ]
test_y <- y[rgrp >= 0.8, ]

## Model Setting
model <- keras_model_sequential()

model %>% 
  layer_dense(units = 25, activation = 'sigmoid', input_shape = c(400)) %>% 
  layer_dense(units = 10, activation = 'softmax')

summary(model)

## Training
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

history <- model %>% fit(
  train_x, train_y, 
  epochs = 200, 
  validation_split = 0.1
)

model %>% evaluate(train_x, train_y)
model %>% evaluate(test_x, test_y)

plot(history)

## 2nd Trial
model %>% predict_classes(test_x[400:401])

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 25, activation = 'sigmoid', kernel_initializer = 'glorot_normal', input_shape = c(400)) %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 10, activation = 'softmax', kernel_initializer = 'glorot_normal')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam', 
  metrics = 'accuracy'
)

history <- model %>% fit(
  train_x, train_y,
  epochs = 200, 
  validation_split = 0.1
)

model %>% evaluate(train_x, train_y)
model %>% evaluate(test_x, test_y)

## 3rd Trial
model <- keras_model_sequential()
model %>%
  layer_dense(units = 400, activation = 'relu', kernel_initializer = 'glorot_normal', input_shape = c(400)) %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 200, activation = 'relu', kernel_initializer = 'glorot_normal') %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 10, activation = 'softmax', kernel_initializer = 'glorot_normal')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

history <- model %>% fit(
  train_x, train_y, 
  epochs = 100,
  validation_split = 0.1
)

model %>% evaluate(train_x, train_y)
model %>% evaluate(test_x, test_y)