library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

total_df <- read.csv('/Users/paul/Desktop/Data_Mining_Practicum/data/total_df.csv')
total_df <- total_df[, -1]

### Q1, Q2. Layer 수를 변경, Layer 당 unit의 수를 변경 + activation 함수 바꿔보기
g1 <- total_df %>% 
  filter(is.na(drop_out_rate)) %>% 
  filter(activation == 'sigmoid') %>% 
  gather(key, value, 1:2) %>% 
  ggplot(aes(x = unit, y = value, color = key, 
             text = paste('Value: ', round(value, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation))) + 
  geom_line(group = 'key') + geom_point(color = 'black', size = 1) +
  facet_grid(layer_num ~ data) + 
  ylim(c(0.4, 1.2)) + ggtitle('Changing number of layer, unit of layer - sigmoid') + 
  theme(plot.title = element_text(hjust = 0.5))

g2 <- total_df %>% 
  filter(is.na(drop_out_rate)) %>% 
  filter(activation == 'relu') %>% 
  gather(key, value, 1:2) %>% 
  ggplot(aes(x = unit, y = value, color = key, 
             text = paste('Value: ', round(value, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation))) + 
  geom_line(group = 'key') + geom_point(color = 'black', size = 1) +
  facet_grid(layer_num ~ data) + 
  ylim(c(0.4, 1.2)) + ggtitle('Changing number of layer, unit of layer - relu') + 
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(g1, tooltip = 'text')
ggplotly(g2, tooltip = 'text')

g1 <- total_df %>% 
  filter(is.na(drop_out_rate)) %>% 
  #filter(activation == 'relu') %>% 
  ggplot(aes(x = unit, y = accuracy, 
             text = paste('Value: ', round(accuracy, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation))) + 
  geom_line(group = 'activation') + geom_point(color = 'black', size = 1) +
  facet_grid(layer_num ~ data) + 
  ylim(c(0.82, 1)) + ggtitle('Changing number of layer, unit of layer - relu') + 
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(g1, tooltip = 'text')

########################
relu <- total_df %>% 
  filter(is.na(drop_out_rate)) %>% 
  filter(activation == 'relu')

g1 <- total_df %>% 
  filter(is.na(drop_out_rate)) %>% 
  filter(activation == 'sigmoid') %>% 
  ggplot(aes(x = unit, y = accuracy, 
             text = paste('Value: ', round(accuracy, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation))) + 
  geom_line(group = 'activation', color = 'red') + geom_label(aes(label = 'sigmoid')) + geom_point(color = 'black', size = 1) + 
  geom_line(data = relu, group = 'activation', color = 'blue') + geom_point(data = relu, color = 'black', size = 1) + 
  facet_grid(layer_num ~ data) + 
  ylim(c(0.82, 1)) + ggtitle('Changing number of layer, unit of layer - relu') + 
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(g1, tooltip = 'text')

g1 <- total_df %>% 
  filter(is.na(drop_out_rate)) %>% 
  filter(activation == 'sigmoid') %>% 
  ggplot(aes(x = unit, y = loss, 
             text = paste('Value: ', round(loss, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation))) + 
  geom_line(aes(group = 'activation', colour = 'sigmoid')) + geom_point(color = 'black', size = 1) + 
  geom_line(data = relu, aes(group = 'activation', color = 'relu')) + geom_point(data = relu, color = 'black', size = 1) + 
  facet_grid(layer_num ~ data) + scale_color_manual(name = 'Activation Function', values = c('sigmoid' = 'red', 'relu' = 'blue')) +
  ylim(c(0.3, 1)) + ggtitle('Changing number of layer, unit of layer - relu') + 
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(g1, tooltip = 'text')

total_df %>% 
  filter(is.na(drop_out_rate)) %>% filter(data == 'train') %>% 
  select(loss, accuracy, data, unit, layer_num, activation) %>% 
  arrange(desc(accuracy)) %>% head(10)
total_df %>% 
  filter(is.na(drop_out_rate)) %>% filter(data == 'test') %>% 
  select(loss, accuracy, data, unit, layer_num, activation) %>% 
  arrange(desc(accuracy)) %>% head(10) 
  

### Q3. drop-out을 추가 및 drop-out rate 바꿔보기
g1 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(is.na(batch_size)) %>% 
  filter(activation == 'sigmoid') %>% 
  filter(drop_out_rate == 0.1) %>% 
  gather(key, value, 1:2) %>% 
  ggplot(aes(x = unit, y = value, color = key,
             text = paste('Value: ', round(value, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation, 
                          '<br>Drop_out_rate:', drop_out_rate))) + 
  geom_line(group = 'key') + geom_point(color = 'black', size = 1) +
  facet_grid(layer_num ~ data) + 
  ylim(c(0.4, 1.2)) + ggtitle('Changing number of layer, unit of layer - sigmoid, drop out rate 0.1') + 
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(g1, tooltip = 'text')

####
d_o_2 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(is.na(batch_size)) %>% 
  filter(drop_out_rate == 0.2) %>% 
  filter(activation == 'sigmoid')
d_o_3 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(is.na(batch_size)) %>% 
  filter(drop_out_rate == 0.3) %>% 
  filter(activation == 'sigmoid')

g1 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(is.na(batch_size)) %>% 
  filter(drop_out_rate == 0.1) %>% 
  filter(activation == 'sigmoid') %>% 
  ggplot(aes(x = unit, y = accuracy, 
             text = paste('Accuracy: ', round(accuracy, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation))) + 
  geom_line(aes(group = 'drop_out_rate', colour = 'drop_out_rate = 0.1')) + geom_point(color = 'black', size = 1) + 
  geom_line(data = d_o_2, aes(group = 'drop_out_rate', color = 'drop_out_rate = 0.2')) + geom_point(data = d_o_2, color = 'black', size = 1) + 
  geom_line(data = d_o_3, aes(group = 'drop_out_rate', color = 'drop_out_rate = 0.3')) + geom_point(data = d_o_3, color = 'black', size = 1) + 
  facet_grid(layer_num ~ data) + scale_color_manual(name = 'Activation Function', 
                                                    values = c('drop_out_rate = 0.1' = 'red', 
                                                               'drop_out_rate = 0.3' = 'green')) +
  ylim(c(0.85, 0.95)) + ggtitle('Changing number of layer, unit of layer, drop out rate - sigmoid') + 
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(g1, tooltip = 'text')

####
batch_64 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(!is.na(batch_size)) %>% 
  filter(batch_size == 64) %>% 
  filter(activation == 'sigmoid')
batch_128 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(!is.na(batch_size)) %>% 
  filter(batch_size == 128) %>% 
  filter(activation == 'sigmoid')
batch_256 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(!is.na(batch_size)) %>% 
  filter(batch_size == 256) %>% 
  filter(activation == 'sigmoid')

g1 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(!is.na(batch_size)) %>% 
  filter(batch_size == 32) %>% 
  filter(activation == 'sigmoid') %>% 
  ggplot(aes(x = unit, y = accuracy, 
             text = paste('Accuracy: ', round(accuracy, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation))) + 
  geom_line(aes(group = 'batch_size', colour = 'batch_size = 32')) + geom_point(color = 'black', size = 1) + 
  geom_line(data = batch_64, aes(group = 'batch_size', color = 'batch_size = 64')) + geom_point(data = d_o_2, color = 'black', size = 1) + 
  geom_line(data = batch_128, aes(group = 'batch_size', color = 'batch_size = 128')) + geom_point(data = d_o_2, color = 'black', size = 1) + 
  geom_line(data = batch_256, aes(group = 'batch_size', color = 'batch_size = 256')) + geom_point(data = d_o_3, color = 'black', size = 1) + 
  facet_grid(layer_num ~ data) + scale_color_manual(name = 'Activation Function', 
                                                    values = c('batch_size = 32' = 'yellow', 
                                                               'batch_size = 64' = 'red', 
                                                               'batch_size = 128' = 'blue', 
                                                               'batch_size = 256' = 'green')) +
  ylim(c(0.85, 0.95)) + ggtitle('Changing number of layer, unit of layer, drop out rate - sigmoid') + 
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(g1, tooltip = 'text')


a <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(!is.na(batch_size)) %>% 
  filter(activation == 'sigmoid') %>% 
  filter(drop_out_rate == 0.1) %>% 
  gather(key, value, 1:2) %>% 
  filter(key == 'accuracy')

g1 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(!is.na(batch_size)) %>% 
  filter(activation == 'sigmoid') %>% 
  filter(drop_out_rate == 0.1) %>% 
  gather(key, value, 1:2) %>% 
  filter(key == 'loss') %>% 
  ggplot(aes(x = unit, y = value, color = factor(batch_size), 
             text = paste('Value: ', round(value, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation, 
                          '<br>Drop_out_rate:', drop_out_rate, 
                          '<br>Batch_size: ', batch_size))) + 
  geom_line(group = 'key') + geom_point(group = 'batch_size', color = c('red')) +
  geom_line(data = a, group = 'key') + geom_point(data = a, group = 'batch_size', color = c('blue')) + 
  facet_grid(layer_num ~ data) + 
  ylim(c(0.2, 1.2)) + ggtitle('Changing number of layer, unit of layer, batch size - sigmoid, drop out rate 0.1') + 
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(g1, tooltip = 'text')

g1 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(is.na(batch_size)) %>% 
  filter(activation == 'sigmoid') %>% 
  gather(key, value, 1:2) %>% 
  ggplot(aes(x = unit, y = value, color = key,
             text = paste('Value: ', round(value, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation, 
                          '<br>Drop_out_rate:', drop_out_rate))) + 
  geom_line(group = 'key') + geom_point(color = 'black', size = 1) +
  facet_grid(layer_num ~ data) + 
  ylim(c(0.4, 1.2)) + ggtitle('Changing number of layer, unit of layer, drop out rate - sigmoid') + 
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(g1, tooltip = 'text')

pal <- c('red', 'blue')
total_df %>% 
  plot_ly(x = ~loss, y = ~accuracy, color = ~data, colors = pal,
          text = ~paste('Unit: ', unit, 
                        '<br>Layer_num: ', layer_num, 
                        '<br>Activation: ', activation, 
                        '<br>Drop_out_rate:', drop_out_rate, 
                        '<br>Batch_size: ', batch_size))

g1 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(!is.na(batch_size)) %>% 
  filter(activation == 'relu') %>% 
  filter(drop_out_rate == 0.1) %>% 
  gather(key, value, 1:2) %>% 
  ggplot(aes(x = unit, y = value, color = factor(batch_size), 
             text = paste('Value: ', round(value, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation, 
                          '<br>Drop_out_rate:', drop_out_rate, 
                          '<br>Batch_size: ', batch_size))) + 
  geom_point(group = 'key') +
  facet_grid(layer_num ~ data) + 
  ylim(c(0.2, 1.2)) + ggtitle('Changing number of layer, unit of layer, batch size - relu, drop out rate 0.1') + 
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(g1, tooltip = 'text')

a <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(!is.na(batch_size)) %>% 
  filter(activation == 'relu') %>% 
  filter(drop_out_rate == 0.1) %>% 
  gather(key, value, 1:2) %>% 
  group_by(key) %>% 
  ggplot(aes(x = unit, y = value, color = factor(batch_size))) + 
  geom_line(group = 'key') + 
  facet_grid(layer_num ~ data)

a + geom_line(group = 'key')

a <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(!is.na(batch_size)) %>% 
  filter(activation == 'relu') %>% 
  filter(drop_out_rate == 0.1) %>% 
  gather(key, value, 1:2) %>% 
  filter(key == 'accuracy')

g1 <- total_df %>% 
  filter(!is.na(drop_out_rate)) %>% 
  filter(!is.na(batch_size)) %>% 
  filter(activation == 'relu') %>% 
  filter(drop_out_rate == 0.1) %>% 
  gather(key, value, 1:2) %>% 
  filter(key == 'loss') %>% 
  ggplot(aes(x = unit, y = value, color = factor(batch_size), 
             text = paste('Value: ', round(value, 3), 
                          '<br>Unit: ', unit, 
                          '<br>Layer_num: ', layer_num, 
                          '<br>Activation: ', activation, 
                          '<br>Drop_out_rate:', drop_out_rate, 
                          '<br>Batch_size: ', batch_size))) + 
  geom_line(group = 'key') + geom_point(group = 'batch_size', color = c('red')) +
  geom_line(data = a, group = 'key') + geom_point(data = a, group = 'batch_size', color = c('blue')) + 
  facet_grid(layer_num ~ data)

ggplotly(g1, tooltip = 'text')

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

### Q1, Q2. Layer 수를 변경, Layer 당 unit의 수를 변경 + activation 함수 바꿔보기
total_df <- data.frame()
# Output layer를 포함하여 Layer의 개수가 2개
for (unit_num in c(25, 50, 100, 200, 300, 400)){
  for (activation_fun in c('sigmoid', 'relu')){
    # (1) Model Setting
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = unit_num, activation = activation_fun, input_shape = c(400)) %>% 
      layer_dense(units = 10, activation = 'softmax')
    
    # (2) Training
    model %>% compile(
      loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')
    history <- model %>% fit(
      train_x, train_y, epochs = 100, validation_split = 0.1)
    
    # (3) Model Output
    train <- t(data.frame(model %>% evaluate(train_x, train_y)))
    test <- t(data.frame(model %>% evaluate(test_x, test_y)))
    df <- rbind(train, test) %>% as_tibble()
    df$data <- c('train', 'test')
    df$unit <- unit_num
    df$layer_num <- 2
    df$activation <- activation_fun
    
    total_df <- rbind(total_df, df)
  }
}

# Output layer를 포함하여 Layer의 개수가 3개
for (unit_num in c(25, 50, 100, 200, 300, 400)){
  for (activation_fun in c('sigmoid', 'relu')){
    # (1) Model Setting
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = unit_num, activation = activation_fun, input_shape = c(400)) %>% 
      layer_dense(units = unit_num, activation = activation_fun) %>% 
      layer_dense(units = 10, activation = 'softmax')
    
    # (2) Training
    model %>% compile(
      loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')
    history <- model %>% fit(
      train_x, train_y, epochs = 100, validation_split = 0.1)
    
    # (3) Model Output
    train <- t(data.frame(model %>% evaluate(train_x, train_y)))
    test <- t(data.frame(model %>% evaluate(test_x, test_y)))
    df <- rbind(train, test) %>% as_tibble()
    df$data <- c('train', 'test')
    df$unit <- unit_num
    df$layer_num <- 3
    df$activation <- activation_fun
    
    total_df <- rbind(total_df, df)
  }
}

# Output layer를 포함하여 Layer의 개수가 4개
for (unit_num in c(25, 50, 100, 200, 300, 400)){
  for (activation_fun in c('sigmoid', 'relu')){
    # (1) Model Setting
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = unit_num, activation = activation_fun, input_shape = c(400)) %>% 
      layer_dense(units = unit_num, activation = activation_fun) %>% 
      layer_dense(units = unit_num, activation = activation_fun) %>% 
      layer_dense(units = 10, activation = 'softmax')
    
    # (2) Training
    model %>% compile(
      loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')
    history <- model %>% fit(
      train_x, train_y, epochs = 100, validation_split = 0.1)
    
    # (3) Model Output
    train <- t(data.frame(model %>% evaluate(train_x, train_y)))
    test <- t(data.frame(model %>% evaluate(test_x, test_y)))
    df <- rbind(train, test) %>% as_tibble()
    df$data <- c('train', 'test')
    df$unit <- unit_num
    df$layer_num <- 4
    df$activation <- activation_fun
    
    total_df <- rbind(total_df, df)
  }
}


df1 <- total_df[1:84, ]
df1 %>% 
  ggplot(aes(x = unit, color = factor(activation))) + 
  geom_point(aes(y = loss), color = 'red') +
  geom_point(aes(y = accuracy), color = 'blue') + 
  facet_grid(layer_num ~ data)

df2 <- df1 %>% 
  gather(key, value, 1:2)

df2 %>% 
  filter(activation == 'sigmoid') %>% 
  ggplot(aes(x = unit, y = value, color = key)) + 
  geom_line() + geom_point(color="violetred") +
  facet_grid(layer_num ~ data) + 
  ylim(c(0.4, 1.2))

write.csv(df1, 'df1.csv')

### Q3. drop-out을 추가 및 drop-out rate 바꿔보기
total_df$drop_out_rate <- NA
for (unit_num in c(25, 50, 100, 200, 300, 400)){
  for (activation_fun in c('sigmoid', 'relu')){
    for (rate_num in c(0.1, 0.2, 0.3)){
      # (1) Model Setting
      model <- keras_model_sequential()
      model %>% 
        layer_dense(units = unit_num, activation = activation_fun, input_shape = c(400)) %>% 
        layer_dropout(rate = rate_num) %>% 
        layer_dense(units = 10, activation = 'softmax')
      
      # (2) Training
      model %>% compile(
        loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')
      history <- model %>% fit(
        train_x, train_y, epochs = 100, validation_split = 0.1)
      
      # (3) Model Output
      train <- t(data.frame(model %>% evaluate(train_x, train_y)))
      test <- t(data.frame(model %>% evaluate(test_x, test_y)))
      df <- rbind(train, test) %>% as_tibble()
      df$data <- c('train', 'test')
      df$unit <- unit_num
      df$drop_out_rate <- rate_num
      df$layer_num <- 2
      df$activation <- activation_fun
      
      total_df <- rbind(total_df, df)
    }
  }
}

# Output layer를 포함하여 Layer의 개수가 3개
for (unit_num in c(25, 50, 100, 200, 300, 400)){
  for (activation_fun in c('sigmoid', 'relu')){
    for (rate_num in c(0.1, 0.2, 0.3)){
      # (1) Model Setting
      model <- keras_model_sequential()
      model %>% 
        layer_dense(units = unit_num, activation = activation_fun, input_shape = c(400)) %>% 
        layer_dropout(rate = rate_num) %>% 
        layer_dense(units = unit_num, activation = activation_fun) %>% 
        layer_dropout(rate = rate_num) %>% 
        layer_dense(units = 10, activation = 'softmax')
      
      # (2) Training
      model %>% compile(
        loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')
      history <- model %>% fit(
        train_x, train_y, epochs = 100, validation_split = 0.1)
      
      # (3) Model Output
      train <- t(data.frame(model %>% evaluate(train_x, train_y)))
      test <- t(data.frame(model %>% evaluate(test_x, test_y)))
      df <- rbind(train, test) %>% as_tibble()
      df$data <- c('train', 'test')
      df$unit <- unit_num
      df$drop_out_rate <- rate_num
      df$layer_num <- 3
      df$activation <- activation_fun
      
      total_df <- rbind(total_df, df)
    }
  }
}

# Output layer를 포함하여 Layer의 개수가 4개
for (unit_num in c(25, 50, 100, 200, 300, 400)){
  for (activation_fun in c('sigmoid', 'relu')){
    for (rate_num in c(0.1, 0.2, 0.3)){
      # (1) Model Setting
      model <- keras_model_sequential()
      model %>% 
        layer_dense(units = unit_num, activation = activation_fun, input_shape = c(400)) %>% 
        layer_dropout(rate = rate_num) %>% 
        layer_dense(units = unit_num, activation = activation_fun) %>% 
        layer_dropout(rate = rate_num) %>%
        layer_dense(units = unit_num, activation = activation_fun) %>% 
        layer_dropout(rate = rate_num) %>%
        layer_dense(units = 10, activation = 'softmax')
      
      # (2) Training
      model %>% compile(
        loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')
      history <- model %>% fit(
        train_x, train_y, epochs = 100, validation_split = 0.1)
      
      # (3) Model Output
      train <- t(data.frame(model %>% evaluate(train_x, train_y)))
      test <- t(data.frame(model %>% evaluate(test_x, test_y)))
      df <- rbind(train, test) %>% as_tibble()
      df$data <- c('train', 'test')
      df$unit <- unit_num
      df$drop_out_rate <- rate_num
      df$layer_num <- 4
      df$activation <- activation_fun
      
      total_df <- rbind(total_df, df)
    }
  }
}

### Q4. Batch size를 바꿔보면서 모델의 결과 비교
total_df$batch_size <- NA
for (unit_num in c(25, 50, 100, 200, 300, 400)){
  for (activation_fun in c('sigmoid', 'relu')){
    for (rate_num in c(0.1, 0.2, 0.3)){
      for (batch_num in c(32, 64, 128, 256)){
        # (1) Model Setting
        model <- keras_model_sequential()
        model %>% 
          layer_dense(units = unit_num, activation = activation_fun, input_shape = c(400)) %>% 
          layer_dropout(rate = rate_num) %>% 
          layer_dense(units = 10, activation = 'softmax')
        
        # (2) Training
        model %>% compile(
          loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')
        history <- model %>% fit(
          train_x, train_y, epochs = 100, batch_size = batch_num, validation_split = 0.1)
        
        # (3) Model Output
        train <- t(data.frame(model %>% evaluate(train_x, train_y)))
        test <- t(data.frame(model %>% evaluate(test_x, test_y)))
        df <- rbind(train, test) %>% as_tibble()
        df$data <- c('train', 'test')
        df$unit <- unit_num
        df$drop_out_rate <- rate_num
        df$batch_size <- batch_num
        df$layer_num <- 2
        df$activation <- activation_fun
        
        total_df <- rbind(total_df, df)
      }
    }
  }
}

# Output layer를 포함하여 Layer의 개수가 3개
for (unit_num in c(25, 50, 100, 200, 300, 400)){
  for (activation_fun in c('sigmoid', 'relu')){
    for (rate_num in c(0.1, 0.2, 0.3)){
      for (batch_num in c(32, 64, 128, 256)){
        # (1) Model Setting
        model <- keras_model_sequential()
        model %>% 
          layer_dense(units = unit_num, activation = activation_fun, input_shape = c(400)) %>% 
          layer_dropout(rate = rate_num) %>% 
          layer_dense(units = unit_num, activation = activation_fun) %>% 
          layer_dropout(rate = rate_num) %>% 
          layer_dense(units = 10, activation = 'softmax')
        
        # (2) Training
        model %>% compile(
          loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')
        history <- model %>% fit(
          train_x, train_y, epochs = 100, batch_size = batch_num, validation_split = 0.1)
        
        # (3) Model Output
        train <- t(data.frame(model %>% evaluate(train_x, train_y)))
        test <- t(data.frame(model %>% evaluate(test_x, test_y)))
        df <- rbind(train, test) %>% as_tibble()
        df$data <- c('train', 'test')
        df$unit <- unit_num
        df$drop_out_rate <- rate_num
        df$batch_size <- batch_num
        df$layer_num <- 3
        df$activation <- activation_fun
        
        total_df <- rbind(total_df, df)
      }
    }
  }
}

# Output layer를 포함하여 Layer의 개수가 4개
for (unit_num in c(25, 50, 100, 200, 300, 400)){
  for (activation_fun in c('sigmoid', 'relu')){
    for (rate_num in c(0.1, 0.2, 0.3)){
      for (batch_num in c(32, 64, 128, 256)){
        # (1) Model Setting
        model <- keras_model_sequential()
        model %>% 
          layer_dense(units = unit_num, activation = activation_fun, input_shape = c(400)) %>% 
          layer_dropout(rate = rate_num) %>% 
          layer_dense(units = unit_num, activation = activation_fun) %>% 
          layer_dropout(rate = rate_num) %>%
          layer_dense(units = unit_num, activation = activation_fun) %>% 
          layer_dropout(rate = rate_num) %>%
          layer_dense(units = 10, activation = 'softmax')
        
        # (2) Training
        model %>% compile(
          loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')
        history <- model %>% fit(
          train_x, train_y, epochs = 100, batch_size = batch_num, validation_split = 0.1)
        
        # (3) Model Output
        train <- t(data.frame(model %>% evaluate(train_x, train_y)))
        test <- t(data.frame(model %>% evaluate(test_x, test_y)))
        df <- rbind(train, test) %>% as_tibble()
        df$data <- c('train', 'test')
        df$unit <- unit_num
        df$drop_out_rate <- rate_num
        df$batch_size <- batch_num
        df$layer_num <- 4
        df$activation <- activation_fun
        
        total_df <- rbind(total_df, df)
      }
    }
  }
}

write.csv(total_df, 'total_df.csv')