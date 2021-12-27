'''
2021-2 - Data Mining Practicum
Prof. Hyebong Choi

Ch 5. Recommender System
'''

library(doBy)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(recommenderlab)

## User-based CF
# load('ratings.RData')

ratings <- data.frame('The_Avengers' = c(2, 5, NA, NA, NA, 4), 
                      'Sherlock' = c(NA, NA, NA, 1, NA, 5), 
                      'Transformer' = c(2, 4, 5, NA, 4, NA), 
                      'Matrix' = c(4, NA, NA, 5, NA, 1), 
                      'Titanic' = c(5, NA, 2, NA, NA, NA), 
                      'Me_Before_You' = c(NA, 1, NA, 4, 2, NA))

pearsonCor <- function(x, y){
  x_mean <- mean(x, na.rm = T)
  y_mean <- mean(y, na.rm = T)
  idx <- !is.na(x) & !is.na(y)
  
  if (sum(idx) == 0) return(NA)
  
  x_new <- x[idx]
  y_new <- y[idx]
  sum((x_new - x_mean) * (y_new - y_mean)) /
    sqrt(sum((x_new - x_mean) ** 2) * sum((y_new - y_mean) ** 2))
}

u <- ratings[5, ]
sim <- apply(ratings, 1, function(x){
  pearsonCor(u, x)
})

k <- 2
k_neighbors <- setdiff(which.maxn(sim, k+1), 5) # delete user 'E' itself
k_recommend <- apply(ratings[k_neighbors, ], 2, function(x){mean(x, na.rm = T)})

k_recommend_final <- k_recommend[is.na(u)]
sort(k_recommend_final, decreasing = T)

## recommenderlab: Reading Data
path <- 'https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/'
ratings.df <- read.csv(paste0(path, 'train_v2.csv'), header = T)
head(ratings.df)

rating_matrix <- ratings.df %>% 
  select(-ID) %>% 
  spread(movie, rating) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'user')

rating_rrm <- as(as(rating_matrix, 'matrix'), 'realRatingMatrix')
summary(rowCounts(rating_rrm))
summary(colCounts(rating_rrm))
ratings.df %>% 
  group_by(movie) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

rating_rrm <- rating_rrm[rowCounts(rating_rrm) > 50, 
                         colCounts(rating_rrm) > 100]

# Normalization of rating matrix
rating_rrm_norm <- normalize(rating_rrm)
image(rating_rrm[1:50, 1:50])
image(rating_rrm_norm[1:50, 1:50])

# Size comparison
print(object.size(rating_matrix), units = 'auto')
print(object.size(rating_rrm), units = 'auto')

# See available methods
recommenderRegistry$get_entries(dataType = 'realRatingMatrix')

# Making Recommendations
ubcf_model <- Recommender(rating_rrm, method = 'UBCF')
recom <- predict(ubcf_model, rating_rrm[1:2, ])
as(recom, 'list')

recom <- predict(ubcf_model, rating_rrm[1:2, ], type = 'ratings')
as(recom, 'matrix')[, 11:20]

# Comparison
e <- evaluationScheme(rating_rrm, method = 'split', train = 0.8, given = 10)
r1 <- Recommender(getData(e, 'train'), 'UBCF')
r2 <- Recommender(getData(e, 'train'), 'IBCF')

p1 <- predict(r1, getData(e, 'known'), type = 'ratings')
p2 <- predict(r2, getData(e, 'known'), type = 'ratings')

error <- rbind(
  calcPredictionAccuracy(p1, getData(e, 'unknown')), 
  calcPredictionAccuracy(p2, getData(e, 'unknown'))
)

rownames(error) <- c('UBCF', 'IBCF')
