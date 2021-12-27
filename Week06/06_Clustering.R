'''
2021-2 - Data Mining Practicum
Prof. Hyebong Choi

Ch 4. Clustering
'''

library(dplyr)
library(tidyr)
library(ggplot2)
library(proxy)

### k-means clustering - code
# set.seed(2015)
synth.data <- data.frame(x1 = c(rnorm(20, 3, 1.5), rnorm(20, 0, 1), rnorm(20, 5, 1)), 
                         x2 = c(rnorm(20, 0, 1), rnorm(20, 4, 1), rnorm(20, 5, 1)))

ndata <- nrow(synth.data)
ndim <- ncol(synth.data)

synth.data %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_point(shape = 1) + theme_bw()

# Euclidean distance
u_dist <- function(x1, y1, x2, y2){ 
  sqrt(((x2-x1)**2) + ((y2-y1)**2))
}

# Initial Setting
k <- 3
cents <- data.frame(cl = 1:k)
cents <- cbind(cents, synth.data[sample(1:60, k), ])

synth.data$cl <- factor(rep(1, ndata), levels = 1:k)
synth.data %>% 
  ggplot(aes(x = x1, y = x2, col = cl)) +
  geom_point(shape = 1) + theme_bw() +
  geom_point(data = cents, shape = 4, col = 'red')

# 1. 전체 프로세스를 반복해줄 수 있는 무한반복문 만들기
# 2. 각 군집과 데이터들 사이에 거리를 구해주기
# 3. 구해준 거리에 대해 가장 가까운 군집으로 매칭시키기
# 4. 새롭게 구해준 군집에 대해 평균으로 중심점 조정하기
# 5. 1~4번 과정을 반복하다가, 탈출할 수 있는 조건문 주기

while (TRUE){
  past <- mean(cents$x1) + mean(cents$x2)
  
  for (row in 1:ndata){
    ### 1. k개의 각 군집들과의 거리를 구해주기
    for (k_value in 1:k){
      col_name <- paste0('dist_', k_value)
      if (row == 1){synth.data[, col_name] <- 0} # 거리 계산 변수 만들기
      
      ### 2. 모든 data point에 대해 각 군집들과 거리를 구하기
      c_df <- cents %>% filter(cl == k_value)
      synth.data[row, col_name] <- u_dist(synth.data$x1[row], synth.data$x2[row], 
                                          c_df$x1, c_df$x2)}
    
    ### 3. 군집들과의 거리가 가장 짧은 곳으로 군집 재배치
    df <- synth.data %>% select(starts_with('dist_'))
    synth.data$cl[row] <- which.min(df[row, ])}
  
  ### 4. 새롭게 배치된 군집들의 평균으로 군집들의 중심점 이동
  cents <- data.frame(cl = 1:k,
                      x1 = aggregate(x1~cl, synth.data, mean)$x1, 
                      x2 = aggregate(x2~cl, synth.data, mean)$x2)
  
  ### 5. centroid가 더 이상 변화하지 않는다면, STOP.
  new <- mean(cents$x1) + mean(cents$x2)
  if (new == past){break}
}

synth.data %>% 
  ggplot(aes(x = x1, y = x2, col = cl)) +
  geom_point(shape = 1) + theme_bw() +
  geom_point(data = cents, shape = 4, col = 'red')

sqrt.edist <- function(u, v){sum((u-v)**2)}

### WSS
WSS <- 
  sapply(1:k, function(i){
    sum(
      apply(split(synth.data, synth.data$cl) [[i]] [, 1:ndim], 1, function(x){
        sqrt.edist(x, cents[i, -1])
      }))
  })

WSS
sum(WSS)

wss <- sum(WSS)

### CH-index
all.center <- colMeans(synth.data[, 1:ndim])
tss <- sum(
  apply(synth.data[, 1:ndim], 1, function(x){sqrt.edist(x, all.center)}))

bss <- tss - wss
ch.index <- (bss/(k-1)) / (wss/(ndata-k))
ch.index

### Hierarchical Clustering
path <- 'https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/'
protein <- read.table(paste0(path, 'protein.txt'), sep = '\t', header = T)

var.to.use <- colnames(protein)[-1]
pmatrix <- scale(protein[, var.to.use]) # Z 정규화
pcenter <- attr(pmatrix, 'scaled:center')
pscale <- attr(pmatrix, 'scaled:scale')

d <- dist(pmatrix, method = 'euclidean') # 거리 matrix 만들기
pfit <- hclust(d, method = 'ward.D')
plot(pfit, labels = protein$Country, ylab = '', xlab = '', sub = '')

groups <- cutree(pfit, k = 5)

print_clusters <- function(labels, k){
  for (i in 1:k){
    print(paste('cluster', i))
    print(head(protein[labels == i, c('Country', 'RedMeat', 'Fish', 'Fr.Veg')], 2))
  }
}

print_clusters(groups, 5)

### Picking proper k
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

### DBSCAN
synth.data2 <- data.frame(x1 = c(runif(50, 1, 5), rnorm(50, 1, 0.5), 
                                 rnorm(50, 5, 1.5), rnorm(50, 8, 0.2)), 
                          x2 = c(rnorm(50, 3, 0.2), rnorm(50, -1, 0.5), 
                                 rnorm(50, 1, 0.3), runif(50, -1, 3)))

synth.data2 %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_point(shape = 1)

synth.data2$cl <- rep(1:4, each = 50)
synth.data2 %>% 
  ggplot(aes(x = x1, y = x2, col = factor(cl))) + 
  geom_point(shape = 1)

