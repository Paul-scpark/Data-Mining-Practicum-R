## Practice 6
## Clustering

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dbscan)
library(cowplot)
library(proxy)

### 1. K-means algorithm
k_means_algo <- function(df, x_col, y_col, k){
  ### Euclidean distance
  u_dist <- function(x1, y1, x2, y2){sqrt(((x2-x1)**2) + ((y2-y1)**2))}
  
  ### Initial setting
  ndata <- nrow(df)
  cents <- data.frame(cl = 1:k)
  cents <- cbind(cents, df[sample(1:ndata, k), ])
  df[, 'cl'] <- factor(rep(1, ndata), levels = 1:k)
  
  ### Append original data
  output_list <- list()
  output_list['df'] <- list(df)
  output_list['cents'] <- list(cents)
  
  ### Algorithm
  start <- 1
  x <- deparse(substitute(x_col))
  y <- deparse(substitute(y_col))
  while (TRUE){
    past <- mean(cents$x1) + mean(cents$x2)
    for (row in 1:ndata){
      ## 1. k개의 각 군집들과의 거리를 구해주기
      for (k_value in 1:k){
        col_name <- paste0('dist_', k_value)
        if (row == 1){df[, col_name] <- 0} # 거리 계산 변수 만들기
        ## 2. 모든 data point에 대해 각 군집들과 거리를 구하기
        c_df <- cents %>% filter(cl == k_value)
        df[row, col_name] <- u_dist(df[row, x], df[row, y], c_df[, x], c_df[, y])}
      ## 3. 군집들과의 거리가 가장 짧은 곳으로 군집 재배치
      target_df <- df %>% select(starts_with('dist_'))
      df$cl[row] <- which.min(target_df[row, ])}
    ## 4. 새롭게 배치된 군집들의 평균으로 군집들의 중심점 이동
    cents <- df %>% 
      group_by(cl) %>% 
      summarise(x1 = mean(x1), x2 = mean(x2))
    ## 5. centroid가 더 이상 변화하지 않는다면, STOP.
    new <- mean(cents$x1) + mean(cents$x2)
    if (new == past){break}
    ## 6. 변화하는 데이터프레임 저장
    name1 <- paste0('df', start)
    name2 <- paste0('cents', start)
    output_list[name1] <- list(df)
    output_list[name2] <- list(cents)
    start <- start + 1
  }
  
  ### Find best model 
  output_vector <- c()
  start <- start - 1
  for (i in 1:start){
    name <- paste0('df', i)
    final_df <- as.data.frame(output_list[name])
    colnames(final_df) <- append(c('x1', 'x2', 'cl'), colnames(target_df))
    
    output_vector[i] <- final_df %>% 
      group_by(cl) %>% 
      summarise(n = n()) %>% 
      summarise(cal = max(n) - min(n)) %>% 
      pull()
  }
  
  ### Make plot by best model
  best_num <- which.min(output_vector)
  new_df <- as.data.frame(output_list[paste0('df', best_num)])
  colnames(new_df) <- append(c('x1', 'x2', 'cl'), colnames(target_df))
  new_cents <- as.data.frame(output_list[paste0('cents', best_num)])
  colnames(new_cents) <- c('cl', 'x1', 'x2')
  
  ori <- output_list$df %>% 
    ggplot(aes(x = x1, y = x2, col = cl)) +
    geom_point(shape = 1) + theme_bw() +
    geom_point(data = output_list$cents, shape = 4, col = 'red') + 
    ggtitle('Plot of original data') + 
    theme(plot.title = element_text(hjust=0.5))
  
  new <- new_df %>% 
    ggplot(aes(x = x1, y = x2, col = cl)) +
    geom_point(shape = 1) + theme_bw() + 
    geom_point(data = new_cents, shape = 4, col = 'red') + 
    ggtitle('Plot of best model data') + 
    theme(plot.title = element_text(hjust=0.5))
  
  print(plot_grid(ori, new, nrow = 2))
  return (new_df)
}

synth.data <- data.frame(x1 = c(rnorm(20, 3, 1.5), rnorm(20, 0, 1), rnorm(20, 5, 1)), 
                         x2 = c(rnorm(20, 0, 1), rnorm(20, 4, 1), rnorm(20, 5, 1)))

output <- k_means_algo(synth.data, x1, x2, 3)
output

### 2. k 값에 따른 CH Index와 WSS 그래프 표현
path <- 'https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/'
protein <- read.table(paste0(path, 'protein.txt'), sep = '\t', header = T)
var.to.use <- colnames(protein)[-1]
pmatrix <- scale(protein[, var.to.use]) # Z 정규화

total_df <- data.frame()
for (k in 1:10){
  pclusters <- kmeans(pmatrix, k, nstart = 100, iter.max = 100)
  
  BSS_output <- pclusters$betweenss
  WSS_output <- pclusters$tot.withinss
  CH_output <- (BSS_output / (k-1)) / (WSS_output / (nrow(pmatrix) - k))
  
  total_df <- rbind(total_df, c(k, CH_output, WSS_output))
}

colnames(total_df) <- c('k', 'ch', 'wss')
total_df$ch <- ifelse(total_df$ch == -Inf, NA, total_df$ch)

total_df %>% 
  gather(measure, score, 2:3) %>% 
  ggplot(aes(x = factor(k), y = score, fill = measure, group = 1)) +
  geom_line(aes(color = measure)) + 
  facet_grid(measure ~., scales = 'free_y') + 
  xlab('k')

### 3. 유클리디안 거리 외에 다양한 종류의 distance measure 조사하기
# 유클리디안 거리
x <-rnorm(2)
y <- rnorm(2)

slope <- diff(y) / diff(x)
intercept <- y[1] - slope*x[1]
dist <- u_dist(x[1], y[1], x[2], y[2])

plot(x, y, main = paste0('Euclidean distance = ', dist))
abline(intercept, slope, col = 'red')

# 맨해탄 거리
m_dist <- function(x1, x2, y1, y2){
  abs(x1 - x2) + abs(y1 - y2)
}

dist <- m_dist(x[1], y[1], x[2], y[2])

plot(x, y, main = paste0('Manhattan distance = ', dist))
abline(v = x[1], col = 'green')
abline(h = y[2], col = 'green')

# 코사인 거리
doc_1 <- c(1, 5)
doc_2 <- c(3, 4)
doc_3 <- c(30, 40)

doc_corpus <- rbind(doc_1, doc_2, doc_3)
colnames(doc_corpus) <- c('life', 'love')

doc_cosine <- as.matrix(dist(doc_corpus, method = 'cosine'))

doc1_slope <- diff(c(doc_1[2], doc_2[2])) / diff(c(doc_1[1], doc_2[1]))
doc1_intercept <- doc_1[2] - doc1_slope*doc_1[1]
doc2_slope <- diff(c(doc_2[2], doc_3[2])) / diff(c(doc_2[1], doc_3[1]))
doc2_intercept <- doc_2[2] - doc2_slope*doc_2[1]
  
plot(doc_corpus)
abline(doc1_intercept, doc1_slope, col = 'red')
abline(doc2_intercept, doc2_slope, col = 'blue')

u_dist(doc_1[1], doc_1[2], doc_2[1], doc_2[2])

# 자카드 거리
m <- matrix(c(0, 1, 1, 2, 3, 4), byrow = T, ncol = 2)
dist(m, method = 'Jaccard')

### 4. DBSCAN 알고리즘 구현
synth.data2 <- data.frame(x1 = c(runif(50, 1, 5), rnorm(50, 1, 0.5), 
                                 rnorm(50, 5, 1.5), rnorm(50, 8, 0.2)), 
                          x2 = c(rnorm(50, 3, 0.2), rnorm(50, -1, 0.5), 
                                 rnorm(50, 1, 0.3), runif(50, -1, 3)))

synth.data2 %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_point(shape = 1)

Eps <- 0.5
MinPts <- 6
ClusterCount <- 1
synth.data2$num <- rep(1:nrow(synth.data2))
synth.data2$cl <- NA

db <- dbscan(synth.data2[, 1:2], Eps, MinPts)
dbscan_plot <- cbind(synth.data2, db$cluster) %>% 
  ggplot(aes(x = x1, y = x2, col = factor(db$cluster))) + 
  geom_point(shape = 1) + 
  ggtitle('Output of DBSCAN package') + 
  theme(plot.title = element_text(hjust=0.5))

for (p in 1:nrow(synth.data2)){
  target <- synth.data2[p, ] # 임의로 시작점을 선택
  
  # 클러스터가 배정이 안된 경우에 실시
  if (is.na(target$cl)){
    # 시작점(target)으로부터 거리를 구해서, Epsilon 보다 작은 데이터만 추리기
    target_df <- synth.data2 %>% 
      mutate(dist = sqrt(((target$x1 - x1)**2) + ((target$x2 - x2)**2))) %>% 
      filter(dist <= Eps)
    # 추려진 데이터가 MinPts 개수보다 많은 경우
    if (nrow(target_df) >= MinPts){
      
      # 새로운 점이 추가되지 않을 때까지, 계속 반복
      # 처음으로 추린 데이터에 대하여 Eplison 범위 안에 점들을 계속해서 추가
      while (TRUE){
        ori <- nrow(target_df)
        # 추려진 데이터(target_df)에 대해 계속해서 Eplison 범위 안에 점들 찾기
        for (i in 1:nrow(target_df)){
          target <- target_df[i, ]
          new_df <- synth.data2 %>% 
            mutate(dist = sqrt(((target$x1 - x1)**2) + ((target$x2 - x2)**2))) %>% 
            filter(dist <= Eps)
          target_df <- rbind(target_df, new_df)}
        target_df <- target_df[!duplicated(target_df$num), ]
        new <- nrow(target_df)
        if (ori == new){break}} # for문 실행 전과 후의 데이터 개수가 동일하면, break
      
      # 추려진 데이터에 대해 이웃의 점 개수(neighbor) 구하기
      row.names(target_df) <- NULL
      for (i in 1:nrow(target_df)){
        target <- target_df[i, ]
        neighbor_df <- target_df %>% 
          mutate(dist = sqrt(((target$x1 - x1)**2) + ((target$x2 - x2)**2))) %>% 
          filter(dist <= Eps)
        target_df[i, 'neighbor'] <- nrow(neighbor_df)}
      
      # 이웃의 점 개수가 MinPts 보다 큰 점들을 core point로 정의
      # core point 점들에 대해서 Epsilon 범위 내에 모든 점을 같은 Cluster로 labeling
      core_df <- target_df[target_df$neighbor >= MinPts, ]
      row.names(core_df) <- NULL
      for (i in 1:nrow(core_df)){
        target <- core_df[i, ]
        # Core point에 대해 Epsilon 범위 내에 점들을 같은 Cluster로 배정
        final_df <- target_df %>% 
          mutate(dist = sqrt(((target$x1 - x1)**2) + ((target$x2 - x2)**2))) %>% 
          filter(dist <= Eps)
        synth.data2[final_df$num, 'cl'] <- ClusterCount}
    ClusterCount <- ClusterCount + 1} # Cluster 배정이 마무리되면, Cluster를 1 더해주기
  }
}

algo_plot <- synth.data2 %>% 
  ggplot(aes(x = x1, y = x2, col = factor(cl))) +
  geom_point(shape = 1) + 
  ggtitle('Output of my own DBSCAN algorithm') + 
  theme(plot.title = element_text(hjust=0.5))

plot_grid(dbscan_plot, algo_plot)