
library(tidyverse)
library(stringr)
library(ggplot2)
library(recommenderlab)
library(forecast)
library(extrafont)
library(sysfonts)
font_import()
font_add_google('Montserrat')
theme_set(theme_grey(base_family='NanumGothic'))

load('./Desktop/recommender_watcha.RData')

# 1. 영화 분석
### (1) 데이터 분포 및 NA 확인
length(unique(movie_train$user_code))
length(unique(movie_train$movie_code))
length(unique(movie_train$movie_title))
colSums(is.na(movie_train))
sum(is.na(movie_train))

str(movie_train)

### (2) 영화의 평점 점수와 개수 비교
m_train_df <- movie_train %>% 
  group_by(movie_code) %>% 
  mutate(score_mean = mean(score), 
         n = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(n))

n_mean <- round(mean(m_train_df$n), 2)
ggplot(m_train_df, aes(x = n)) + 
  geom_histogram(bins = 50) + 
  geom_vline(xintercept = n_mean, size = 1.5, colour = 'red') +
  ggtitle('Distribution of films with ratings (Histogram)') + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) + 
  xlab('Number of ratings') + ylab('Count') + 
  geom_text(x = 280, y = 5000, label = paste0('Mean: ', n_mean))

movie_cov <- round(cov(m_train_df$score_mean, m_train_df$n), 2)
movie_cor <- round(cor(m_train_df$score_mean, m_train_df$n), 3)
ggplot(m_train_df, aes(x = score_mean, y = n)) + 
  geom_point(size = 1, alpha = 0.3) + 
  geom_smooth(method = lm) + 
  ggtitle('Distribution of average and number of ratings (Scatterplot)') + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) + 
  xlab('Average of ratings') + ylab('Number of ratings') +
  geom_text(x = 1, y = 1500, label = paste0('Cov: ', movie_cov)) + 
  geom_text(x = 1, y = 1400, label = paste0('Cor: ', movie_cor))

summary(m_train_df$n)

m_train_df$label <- cut(m_train_df$n, breaks = quantile(m_train_df$n, probs = seq(0, 1, by = 0.2)), 
    labels = c('very little', 'little', 'normal', 'much', 'very much'), include.lowest=TRUE)
table(m_train_df$label)

m_train_df %>% 
  ggplot(aes(x = label, y = score_mean, col = label)) + 
  geom_boxplot() + 
  ggtitle('Average rating by group (Boxplot)') + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) + 
  xlab('') + ylab('Average of ratings')

### (3) 평점의 수가 적지만 평점이 높은 영화, 평점의 수가 많지만 평점이 낮은 영화
bind_rows(m_train_df %>% 
            filter(label == 'very little') %>% 
            arrange(desc(score_mean), movie_title) %>% 
            head(7), 
          m_train_df %>% 
            filter(label == 'very much') %>% 
            arrange(score_mean, movie_title) %>% 
            head(7)) %>% 
  ggplot(aes(x = reorder(movie_title, -score_mean), y = score_mean, fill = label)) + 
  geom_col(show.legend = F) + 
  facet_wrap(~label, scales = 'free_y') +
  coord_flip() +
  labs(x = NULL, y = 'Average of ratings')

# 2. 도서 분석
### (1) 데이터 분포 및 NA 확인
length(unique(book_train$user_code))
length(unique(book_train$book_code))
length(unique(book_train$book_title))
colSums(is.na(book_train))
sum(is.na(book_train))

str(book_train)

### (2) 도서의 평점 점수와 개수 비교
b_train_df <- book_train %>% 
  group_by(book_code) %>% 
  mutate(score_mean = mean(score), 
         n = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(n))

n_b_mean <- round(mean(b_train_df$n), 2)
ggplot(b_train_df, aes(x = n)) + 
  geom_histogram(bins = 30) + 
  geom_vline(xintercept = n_b_mean, size = 1.5, colour = 'red') +
  ggtitle('Distribution of books with ratings (Histogram)') + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) + 
  xlab('Number of ratings') + ylab('Count') + 
  geom_text(x = 200, y = 3000, label = paste0('Mean: ', n_b_mean))

book_cov <- round(cov(b_train_df$score_mean, b_train_df$n), 2)
book_cor <- round(cor(b_train_df$score_mean, b_train_df$n), 3)
ggplot(b_train_df, aes(x = score_mean, y = n)) + 
  geom_point(size = 1, alpha = 0.3) + 
  geom_smooth(method = lm) + 
  ggtitle('Distribution of average and number of ratings (Scatterplot)') + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) + 
  xlab('Average of ratings') + ylab('Number of ratings') +
  geom_text(x = 1, y = 800, label = paste0('Cov: ', book_cov)) + 
  geom_text(x = 1, y = 750, label = paste0('Cor: ', book_cor))

b_train_df$label <- cut(b_train_df$n, breaks = quantile(b_train_df$n, probs = seq(0, 1, by = 0.2)), 
                        labels = c('very little', 'little', 'normal', 'much', 'very much'), include.lowest=TRUE)
table(b_train_df$label)

b_train_df %>% 
  ggplot(aes(x = label, y = score_mean, col = label)) + 
  geom_boxplot() + 
  ggtitle('Average rating by group (Boxplot)') + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) + 
  xlab('') + ylab('Average of ratings')

# 3. 사용자 분석
### (1) 도서와 영화 모두에 평점 기록이 있는 사용자 추출
common_user <- intersect(movie_train$user_code, book_train$user_code)

movie_train <- movie_train %>% 
  mutate(content = 'movie', 
         user_label = ifelse(user_code %in% common_user, 'common', 'rare'))
book_train <- book_train %>% 
  mutate(content = 'book', 
         user_label = ifelse(user_code %in% common_user, 'common', 'rare'))

bind_rows(movie_train %>% 
            select(content, user_label), 
          book_train %>% 
            select(content, user_label)) %>% 
  group_by(content, user_label) %>% 
  summarise(n = n()) %>% 
  group_by(content) %>% 
  mutate(mean = n / sum(n)) %>% 
  ggplot(aes(x = '', y = n, fill = factor(user_label))) + 
  geom_bar(stat = 'identity', position = position_fill()) + 
  coord_polar(theta = 'y') + 
  facet_wrap( ~ content) + 
  ggtitle('User distribution with rating records in both books and movies') + 
  geom_text(aes(label = paste(round(mean, 2), "%")), position = position_fill(vjust = 0.5)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.title=element_blank(), 
        plot.title = element_text(size = 15, hjust = 0.5)) + 
  theme(legend.position='bottom')
  
### (2) 영화 평점 기록이 많은 사용자들이 도서 평점 기록도 많은지 확인
movie_train <- movie_train %>% 
  filter(user_label == 'common') %>% 
  group_by(user_code) %>% 
  mutate(n = n())

book_train <- book_train %>% 
  filter(user_label == 'common') %>% 
  group_by(user_code) %>% 
  mutate(n = n())
  
m_mean <- round(mean(movie_train$n), 2)
movie_train %>% 
  group_by(user_code) %>% 
  filter(row_number() == 1) %>% 
  ggplot(aes(x = n)) + 
  geom_histogram(bins = 50) + 
  geom_vline(xintercept = m_mean, size = 1.5, colour = 'red') + 
  ggtitle('Distribution of movies with ratings by common users') +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) + 
  xlab('Number of ratings by users') + ylab('Count') + 
  geom_text(x = 1200, y = 300, label = paste0('Mean: ', m_mean))
  
quantile(movie_train$n, probs = seq(0, 1, by = 0.2))
movie_train$label <- cut(movie_train$n, breaks = quantile(movie_train$n, probs = seq(0, 1, by = 0.2)), 
                         labels = c('very little', 'little', 'normal', 'much', 'very much'), include.lowest=TRUE)

very_little <- movie_train %>% 
  filter(label == 'very little') %>% 
  distinct(user_code) %>% pull()

little <- movie_train %>% 
  filter(label == 'little') %>% 
  distinct(user_code) %>% pull()

normal <- movie_train %>% 
  filter(label == 'normal') %>% 
  distinct(user_code) %>% pull()

much <- movie_train %>% 
  filter(label == 'much') %>% 
  distinct(user_code) %>% pull()

very_much <- movie_train %>% 
  filter(label == 'very much') %>% 
  distinct(user_code) %>% pull()

bind_rows(movie_train %>% 
            group_by(user_code) %>% 
            filter(row_number() == 1), 
          book_train %>% 
            mutate(label = ifelse(user_code %in% very_little, 'very little', 
                             ifelse(user_code %in% little, 'little', 
                               ifelse(user_code %in% normal, 'normal', 
                                 ifelse(user_code %in% much, 'much', 'very much'))))) %>% 
            group_by(user_code) %>% 
            filter(row_number() == 1)) %>% 
  mutate(label = factor(label, levels = c('very little', 'little', 'normal', 'much', 'very much'))) %>% 
  ggplot(aes(x = label, y = n, fill = content)) + 
  geom_boxplot() + xlab('') + ylab('') + 
  ylim(0, 1400)

book_train <- book_train %>% 
  mutate(label = ifelse(user_code %in% very_little, 'very little', 
                        ifelse(user_code %in% little, 'little', 
                               ifelse(user_code %in% normal, 'normal', 
                                      ifelse(user_code %in% much, 'much', 'very much')))), 
         label = factor(label, levels = c('very little', 'little', 'normal', 'much', 'very much')))

df <- cbind(aggregate(n~label, movie_train, mean), aggregate(n~label, book_train, mean))[, c(1, 2, 4)]
colnames(df) <- c('label', 'movie', 'book')

### (3) 사용자의 평점을 매기는 기준이 영화와 도서에서 동일한지 확인
movie_book_df <- movie_train %>% 
  group_by(user_code) %>% 
  summarise(movie_rating = mean(score)) %>% 
  left_join(book_train %>% 
              group_by(user_code) %>% 
              summarise(book_rating = mean(score)), 
            by = 'user_code')

mb_cor <- round(cor(movie_book_df$movie_rating, movie_book_df$book_rating), 2)
mb_cov <- round(cov(movie_book_df$movie_rating, movie_book_df$book_rating), 2)
movie_mean <- round(mean(movie_book_df$movie_rating), 2)
book_mean <- round(mean(movie_book_df$book_rating), 2)
movie_book_df %>% 
  ggplot(aes(x = movie_rating, y = book_rating)) +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = 'lm') + 
  geom_vline(xintercept = movie_mean, size = 1, colour = 'red', alpha = 0.4) +
  geom_hline(yintercept = book_mean, size = 1, colour = 'red', alpha = 0.4) +
  ggtitle('Average movie and book ratings by the same user') + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) + 
  geom_text(x = 1.2, y = 5, label = paste0('Cov: ', mb_cov)) + 
  geom_text(x = 1.2, y = 4.8, label = paste0('Cor: ', mb_cor)) +
  geom_text(x = movie_mean, y = 5, label = paste0("Movie's mean: ", movie_mean)) + 
  geom_text(x = 1.2, y = book_mean, label = paste0("Book's mean: ", book_mean))

### (4) 영화 평점이 후한 사용자는 도서 평점도 후할까?
movie_book_df <- movie_book_df %>% 
  mutate(label = cut(movie_rating, breaks = seq(0, 5, by = 1), 
                     labels = c('0~1', '1~2', '2~3', '3~4', '4~5'))) 

movie_book_df %>% 
  group_by(label) %>% 
  summarise(movie_mean = mean(movie_rating), 
            book_mean = mean(book_rating))

movie_book_df %>% 
  gather(variable, value, 2:3) %>% 
  group_by(label) %>% 
  ggplot(aes(x = label, y = value, fill = variable)) + 
  geom_boxplot(width = 0.6) + 
  ylab('Range of ratings') + xlab('Category of ratings')
  
# 4. 영화 추천
### (1) recommenderlab 패키지에서 사용되는 추천 알고리즘 소개 및 전처리
recommenderRegistry$get_entries()[19]
recommenderRegistry$get_entries()[8]

load('./Desktop/recommender_watcha.RData')
paste('train data에서의 유일한 movie_code 개수:', length(unique(movie_train$movie_code)))
paste('test data에서의 유일한 movie_code 개수:', length(unique(movie_test$movie_code)))
paste('train과 test 데이터의 교집합 개수:', length(intersect(movie_train$movie_code, movie_test$movie_code)))

common_movie <- intersect(movie_train$movie_code, movie_test$movie_code)
movie_matrix <- movie_train %>% 
  filter(movie_code %in% common_movie) %>% 
  select(-movie_title) %>% 
  spread(movie_code, score) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'user_code')

movie_test_matrix <- movie_test %>% 
  select(-movie_title) %>% 
  spread(movie_code, score) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'user_code')

movie_rrm <- as(as(movie_matrix, 'matrix'), 'realRatingMatrix')
movie_test_rrm <- as(as(movie_test_matrix, 'matrix'), 'realRatingMatrix')
summary(rowCounts(movie_rrm))
summary(colCounts(movie_rrm))

hist(getRatings(movie_rrm), xlim = c(0, 5), breaks=100, xlab="Movie Ratings",
     main = " Histogram of Movie Ratings")
hist(getRatings(normalize(movie_rrm)), breaks=100, xlab="Movie Ratings",
     main = " Histogram of Movie Ratings (Normalized)")

### (2) 영화에 대해 train 데이터를 바탕으로 UBCF, IBCF 모델로 test 데이터를 예측
##### UBCF model
UBCF_model <- Recommender(movie_rrm, method = 'UBCF')

train_pred <- predict(UBCF_model, movie_rrm, type = 'ratingMatrix')
train_pred <- as(train_pred, 'data.frame')
colnames(train_pred) <- c('user_code', 'movie_code', 'score_pred')

UBCF_movie <- movie_test %>% 
  left_join(train_pred, by = c('user_code', 'movie_code')) %>% 
  as.data.frame()

UBCF_movie %>% 
  summarise(MAE = mean(abs(score - score_pred), na.rm = T))

##### IBCF model
IBCF_model <- Recommender(movie_rrm, method = 'IBCF')

train_pred <- predict(IBCF_model, movie_rrm, type = 'ratingMatrix')
train_pred <- as(train_pred, 'data.frame')
colnames(train_pred) <- c('user_code', 'movie_code', 'score_pred')

IBCF_movie <- movie_test %>% 
  left_join(train_pred, by = c('user_code', 'movie_code')) %>% 
  as.data.frame()

IBCF_movie %>% 
  summarise(MAE = mean(abs(score - score_pred), na.rm = T))

# write.csv(UBCF_movie, 'UBCF_movie.csv')
# write.csv(IBCF_movie, 'IBCF_movie.csv')

# UBCF_movie <- read.csv('/Users/paul/Desktop/UBCF_movie.csv')
# IBCF_movie <- read.csv('/Users/paul/Desktop/IBCF_movie.csv')

UBCF <- bind_cols(UBCF_movie %>% 
                    summarise(MAE = mean(abs(score - score_pred), na.rm = T)), 
                  UBCF_movie %>% 
                    summarise(NA_ratio = mean(is.na(score_pred))))

IBCF <- bind_cols(IBCF_movie %>% 
                    summarise(MAE = mean(abs(score - score_pred), na.rm = T)), 
                  IBCF_movie %>% 
                    summarise(NA_ratio = mean(is.na(score_pred))))

result <- bind_rows(UBCF, IBCF)
rownames(result) <- c('UBCF', 'IBCF')
result

UBCF_movie <- read.csv('/Users/paul/Desktop/UBCF_movie_result.csv')
UBCF_movie %>% 
  filter(similarity == 'cosine') %>% 
  filter(data == 'train') %>% 
  select(k_num, MAE, NA_ratio) %>% 
  gather(variable, value, -1) %>% 
  ggplot(aes(x = k_num, y = value)) + 
  theme_bw() + 
  geom_line() + 
  facet_grid(variable ~ ., scales = 'free_y') + 
  ggtitle('Graph of changes in MAE and NA ratio in UBCF model by k') + 
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 

IBCF_movie <- read.csv('/Users/paul/Desktop/IBCF_movie_result.csv')
IBCF_movie %>% 
  filter(similarity == 'cosine') %>% 
  filter(data == 'train') %>% 
  select(k_num, MAE, NA_ratio) %>% 
  gather(variable, value, -1) %>% 
  ggplot(aes(x = k_num, y = value)) + 
  theme_bw() + 
  geom_line() + 
  facet_grid(variable ~ ., scales = 'free_y') + 
  ggtitle('Graph of changes in MAE and NA ratio in IBCF model by k') + 
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 

final_movie <- bind_rows(UBCF_movie %>% 
                  filter(similarity == 'cosine', 
                         data == 'test', 
                         k_num == '240') %>% 
                  select(k_num, MAE, NA_ratio),
                IBCF_movie %>% 
                  filter(similarity == 'cosine', 
                         data == 'test', 
                         k_num == '760') %>% 
                  select(k_num, MAE, NA_ratio))

rownames(final_movie) <- c('UBCF', 'IBCF')
final_movie

# 5. 도서 추천
### (1) 도서 데이터에 UBCF, IBCF 모델을 적용할 수 있도록 전처리 
load('./recommender_watcha.RData')
paste('train data에서의 유일한 book_code 개수:', length(unique(book_train$book_code)))
paste('test data에서의 유일한 book_code 개수:', length(unique(book_test$book_code)))
paste('train과 test 데이터의 교집합 개수:', length(intersect(book_train$book_code, book_test$book_code)))

common_book <- intersect(book_train$book_code, book_test$book_code)
book_matrix <- book_train %>% 
  filter(book_code %in% common_book) %>% 
  select(-book_title) %>% 
  spread(book_code, score) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'user_code')

book_test_matrix <- book_test %>% 
  select(-book_title) %>% 
  spread(book_code, score) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'user_code')

book_rrm <- as(as(book_matrix, 'matrix'), 'realRatingMatrix')
book_test_rrm <- as(as(book_test_matrix, 'matrix'), 'realRatingMatrix')

summary(rowCounts(book_rrm))
summary(colCounts(book_rrm))

hist(getRatings(book_rrm), xlim = c(0, 5), breaks=100, xlab="Book Ratings",
     main = " Histogram of Book Ratings")
hist(getRatings(normalize(book_rrm)), breaks=100, xlab="Book Ratings",
     main = " Histogram of Book Ratings (Normalized)")

### (2) 도서에 대해 train 데이터를 바탕으로 UBCF, IBCF 모델로 test 데이터를 예측
##### UBCF model
UBCF_model <- Recommender(book_rrm, method = 'UBCF')

train_pred <- predict(UBCF_model, book_rrm, type = 'ratingMatrix')
train_pred <- as(train_pred, 'data.frame')
colnames(train_pred) <- c('user_code', 'book_code', 'score_pred')

UBCF_book <- book_test %>% 
  left_join(train_pred, by = c('user_code', 'book_code')) %>% 
  as.data.frame()

UBCF_book %>% 
  summarise(MAE = mean(abs(score - score_pred), na.rm = T))

##### IBCF model
IBCF_model <- Recommender(book_rrm, method = 'IBCF')

train_pred <- predict(IBCF_model, book_rrm, type = 'ratingMatrix')
train_pred <- as(train_pred, 'data.frame')
colnames(train_pred) <- c('user_code', 'book_code', 'score_pred')


IBCF_book <- book_test %>% 
  left_join(train_pred, by = c('user_code', 'book_code')) %>% 
  as.data.frame()

IBCF_book %>% 
  summarise(MAE = mean(abs(score - score_pred), na.rm = T))

UBCF_book <- read.csv('./UBCF_book.csv')
IBCF_book <- read.csv('./IBCF_book.csv')

UBCF <- bind_cols(UBCF_book %>% 
                    summarise(MAE = mean(abs(score - score_pred), na.rm = T)), 
                  UBCF_book %>% 
                    summarise(NA_ratio = mean(is.na(score_pred))))

IBCF <- bind_cols(IBCF_book %>% 
                    summarise(MAE = mean(abs(score - score_pred), na.rm = T)), 
                  IBCF_book %>% 
                    summarise(NA_ratio = mean(is.na(score_pred))))

result <- bind_rows(UBCF, IBCF)
rownames(result) <- c('UBCF', 'IBCF')
result

UBCF_book <- read.csv('/Users/paul/Desktop/UBCF_book_result.csv')
UBCF_book %>% 
  filter(similarity == 'cosine') %>% 
  filter(data == 'train') %>% 
  select(k_num, MAE, NA_ratio) %>% 
  gather(variable, value, -1) %>% 
  ggplot(aes(x = k_num, y = value)) + 
  theme_bw() + 
  geom_line() + 
  facet_grid(variable ~ ., scales = 'free_y') + 
  ggtitle('Graph of changes in MAE and NA ratio in UBCF model by k') + 
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 

IBCF_book <- read.csv('/Users/paul/Desktop/IBCF_book_result.csv')
IBCF_book %>% 
  filter(similarity == 'cosine') %>% 
  filter(data == 'train') %>% 
  select(k_num, MAE, NA_ratio) %>% 
  gather(variable, value, -1) %>% 
  ggplot(aes(x = k_num, y = value)) + 
  theme_bw() + 
  geom_line() + 
  facet_grid(variable ~ ., scales = 'free_y') + 
  ggtitle('Graph of changes in MAE and NA ratio in IBCF model by k') + 
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 

final_book <- bind_rows(UBCF_book %>% 
                         filter(similarity == 'cosine', 
                               data == 'test', 
                               k_num == '30') %>% 
                         select(k_num, MAE, NA_ratio),
                        IBCF_book %>% 
                          filter(similarity == 'cosine', 
                                 data == 'test', 
                                 k_num == '740') %>% 
                          select(k_num, MAE, NA_ratio))

rownames(final_book) <- c('UBCF', 'IBCF')
final_book

# 6. self 추천
movie_train %>% 
  group_by(movie_code) %>% 
  mutate(n = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(movie_title, -n), y = n)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Top 20 films with a high ratings record') +
  theme(axis.text.x=element_text(angle = -50, hjust = 0),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  xlab('') + ylab('Number of ratings')

movie_train %>% 
  group_by(movie_code) %>% 
  mutate(n = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(n)) %>% 
  tail(20) %>% 
  ggplot(aes(x = reorder(movie_title, -n), y = n)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Under 20 films with a high ratings record') +
  theme(axis.text.x=element_text(angle = -50, hjust = 0),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  xlab('') + ylab('Number of ratings')

self_movie <- movie_train %>% 
  group_by(movie_code) %>% 
  mutate(n = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  select(-user_code, -score, -n) %>% 
  mutate(user_code = 'US000000') %>% 
  select(user_code, movie_code, movie_title)

self_movie$score <- c(4.5, 4, 2, 2.5, 5, 4.5, 3, 5, 4, 4, 
                      2, 4, 4, 2, 3, 4, 5, 5, 5, 3)

self_movie <- bind_rows(self_movie[, c('user_code', 'movie_code', 
                                       'score', 'movie_title')], 
                        movie_train)

self_movie_matrix <- self_movie %>% 
  select(-movie_title) %>% 
  spread(movie_code, score) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'user_code')

self_movie_rrm <- as(as(self_movie_matrix, 'matrix'), 'realRatingMatrix')

self_UBCF_model <- Recommender(self_movie_rrm, method = 'UBCF')
train_pred <- predict(self_UBCF_model, self_movie_rrm[1], type = 'topNList')
train_pred <- as(train_pred, 'list')

self_recom1 <- self_movie %>% 
  filter(movie_code %in% train_pred$US000000) %>% 
  group_by(movie_code) %>% 
  filter(row_number() == 1)

self_IBCF_model <- Recommender(self_movie_rrm, method = 'IBCF')
train_pred <- predict(self_IBCF_model, self_movie_rrm[1], type = 'topNList')
train_pred <- as(train_pred, 'list')

self_recom <- self_movie %>% 
  filter(movie_code %in% train_pred$US000000) %>% 
  group_by(movie_code) %>% 
  filter(row_number() == 1)

write.csv(self_recom1, 'self_recom_UBCF.csv')

movie_train %>% 
  group_by(movie_code) %>% 
  mutate(n = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(n)) %>% 
  View()

self_movie <- movie_train %>% 
  filter(movie_code %in% c('MC0005482', 'MC0004177', 'MC0003414', 
                           'MC0011700', 'MC0006955', 'MC0007115', 
                           'MC0002720', 'MC0003692', 'MC0007330', 
                           'MC0004079', 'MC0005392', 'MC0003385', 
                           'MC0000317', 'MC0003264', 'MC0005114', 
                           'MC0004195', 'MC0004926', 'MC0003048', 
                           'MC0004047', 'MC0004196')) %>% 
  group_by(movie_code) %>% 
  filter(row_number() == 1) %>% 
  mutate(user_code = 'US000000') %>% 
  select(-score)

self_movie$score <- c(4, 3, 3, 4, 5, 2, 3, 3, 4, 4, 3, 5, 5, 3, 5, 4, 5, 4, 3, 3)
self_movie <- bind_rows(self_movie[, c('user_code', 'movie_code', 
                                       'score', 'movie_title')], 
                        movie_train)

self_movie_matrix <- self_movie %>% 
  select(-movie_title) %>% 
  spread(movie_code, score) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'user_code')

self_movie_rrm <- as(as(self_movie_matrix, 'matrix'), 'realRatingMatrix')

self_UBCF_model <- Recommender(self_movie_rrm, method = 'UBCF')
train_pred <- predict(self_UBCF_model, self_movie_rrm[1], type = 'topNList')
train_pred <- as(train_pred, 'list')

self_recom1 <- self_movie %>% 
  filter(movie_code %in% train_pred$US000000) %>% 
  group_by(movie_code) %>% 
  filter(row_number() == 1)

write.csv(self_recom1, 'self_recom2.csv')

self_IBCF_model <- Recommender(self_movie_rrm, method = 'IBCF')
train_pred <- predict(self_IBCF_model, self_movie_rrm[1], type = 'topNList')
train_pred <- as(train_pred, 'list')

self_recom <- self_movie %>% 
  filter(movie_code %in% train_pred$US000000) %>% 
  group_by(movie_code) %>% 
  filter(row_number() == 1)

write.csv(self_recom, 'self_recom_UBCF2.csv')

book_train %>% 
  group_by(book_code) %>% 
  mutate(n = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(book_title, -n), y = n)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Top 20 books with a high ratings record') +
  theme(axis.text.x=element_text(angle = -50, hjust = 0),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  xlab('') + ylab('Number of ratings')

self_book <- book_train %>% 
  group_by(book_code) %>% 
  mutate(n = n()) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  select(-user_code, -score, -n) %>% 
  mutate(user_code = 'US000000') %>% 
  select(user_code, book_code, book_title)

self_book$score <- c(5, 4, 3, 4, 5, 4, 2, 2, 2, 5, 
                     3, 4, 3, 3, 2, 3, 4, 4, 5, 3)

self_book <- bind_rows(self_book[, c('user_code', 'book_code', 
                                       'score', 'book_title')], 
                        book_train)

self_book_matrix <- self_book %>% 
  select(-book_title) %>% 
  spread(book_code, score) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'user_code')

self_book_rrm <- as(as(self_book_matrix, 'matrix'), 'realRatingMatrix')

self_UBCF_model <- Recommender(self_book_rrm, method = 'UBCF')
train_pred <- predict(self_UBCF_model, self_book_rrm[1], type = 'topNList')
train_pred <- as(train_pred, 'list')

self_recom1 <- self_book %>% 
  filter(book_code %in% train_pred$US000000) %>% 
  group_by(book_code) %>% 
  filter(row_number() == 1)

write.csv(self_recom1, 'self_recom_book1.csv')

self_IBCF_model <- Recommender(self_book_rrm, method = 'IBCF')
train_pred <- predict(self_IBCF_model, self_book_rrm[1], type = 'topNList')
train_pred <- as(train_pred, 'list')

self_recom <- self_book %>% 
  filter(book_code %in% train_pred$US000000) %>% 
  group_by(book_code) %>% 
  filter(row_number() == 1)

write.csv(self_recom, 'self_recom_book2.csv')

# 보너스 1. cross-platform 추천


# 보너스 2. 영화 추천 성능 개선
UBCF_movie <- read.csv('/Users/paul/Desktop/UBCF_movie_result.csv')
IBCF_movie <- read.csv('/Users/paul/Desktop/IBCF_movie_result.csv')

UBCF_book <- read.csv('/Users/paul/Desktop/UBCF_book_result.csv')
IBCF_book <- read.csv('/Users/paul/Desktop/IBCF_book_result.csv')

SVD_movie <- read.csv('/Users/paul/Desktop/SVD_movie_result.csv')
SVD_book <- read.csv('/Users/paul/Desktop/SVD_book_result.csv')

UBCF_movie %>% 
  filter(data == 'train', 
         k_num >= 300) %>% 
  select(-X, -data, -normalize, -model) %>% 
  gather(variable, value, 1:2) %>% 
  ggplot(aes(x = k_num, y = value, color = similarity)) + 
  geom_line() + 
  facet_grid(variable ~ ., scales = 'free_y') + 
  ggtitle('Graph of changes in MAE and NA ratio in UBCF model by k and similarity') + 
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 

IBCF_movie %>% 
  filter(data == 'train', 
         k_num >= 600, 
         k_num <= 800) %>% 
  select(-X, -data, -normalize, -model) %>% 
  gather(variable, value, 1:2) %>% 
  ggplot(aes(x = k_num, y = value, color = similarity)) + 
  geom_line() + 
  facet_grid(variable ~ ., scales = 'free_y')

SVD_movie %>% 
  filter(data == 'train') %>% 
  select(-X, -data, -model, -similarity, -normalize) %>% 
  gather(variable, value, 1:2) %>% 
  ggplot(aes(x = k_num, y = value)) + 
  geom_line() + 
  facet_grid(variable ~ ., scales = 'free_y')

bind_rows(UBCF_movie %>% 
            filter(data == 'train', 
                   k_num >= 300, 
                   similarity == 'cosine') %>% 
            select(-X, -data, -normalize) %>% 
            gather(variable, value, 1:2) %>% 
            mutate(num_range = rep(seq(1:10), 2)), 
          IBCF_movie %>% 
            filter(data == 'train', 
                   k_num >= 620, 
                   k_num <= 800, 
                   similarity == 'cosine') %>% 
            select(-X, -data, -normalize) %>% 
            gather(variable, value, 1:2) %>% 
            mutate(num_range = rep(seq(1:10), 2)), 
          SVD_movie %>% 
            filter(data == 'train', 
                   k_num >= 160) %>% 
            select(-X, -data, -similarity, -normalize) %>% 
            gather(variable, value, 1:2) %>% 
            mutate(similarity = 'SVD', 
                   num_range = rep(seq(1:10), 2))) %>% 
  ggplot(aes(x = num_range, y = value, color = model)) + 
  geom_line(alpha = 0.7) + 
  facet_grid(variable ~ ., scales = 'free_y') + 
  ggtitle('Graph of changes in MAE and NA ratio in each best model by k - train') + 
  theme(plot.title = element_text(size = 10, hjust = 0.5)) 

bind_rows(UBCF_movie %>% 
            filter(data == 'test', 
                   k_num >= 300, 
                   similarity == 'cosine') %>% 
            select(-X, -data, -normalize) %>% 
            gather(variable, value, 1:2) %>% 
            mutate(num_range = rep(seq(1:10), 2)), 
          IBCF_movie %>% 
            filter(data == 'test', 
                   k_num >= 620, 
                   k_num <= 800, 
                   similarity == 'cosine') %>% 
            select(-X, -data, -normalize) %>% 
            gather(variable, value, 1:2) %>% 
            mutate(num_range = rep(seq(1:10), 2)), 
          SVD_movie %>% 
            filter(data == 'test', 
                   k_num >= 160) %>% 
            select(-X, -data, -similarity, -normalize) %>% 
            gather(variable, value, 1:2) %>% 
            mutate(similarity = 'SVD', 
                   num_range = rep(seq(1:10), 2))) %>% 
  ggplot(aes(x = num_range, y = value, color = model)) + 
  geom_line(alpha = 0.7) + 
  facet_grid(variable ~ ., scales = 'free_y') + 
  ggtitle('Graph of changes in MAE and NA ratio in each best model by k - test') + 
  theme(plot.title = element_text(size = 10, hjust = 0.5)) 
