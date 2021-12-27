## Practice 5
## Text Mining 2

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidytext)
library(janeaustenr)
library(gutenbergr)
library(scales)
library(textdata)
library(wordcloud)
library(reshape2)
library(topicmodels)
library(wordcloud2)
library(ggrepel)
library(KoNLP)
library(memoise)
library(rJava)
library(tm)
library(zoo)
library(tidyquant)
theme_set(theme_grey(base_family='NanumGothic'))
options("scipen" = 1000)

### Loading data
Everytime_df <- readLines('/Users/paul/Desktop/Data_Mining_Practicum/data/eta_secret_board.txt')
Everytime_df <- data.frame(data.frame(values = Everytime_df)[-1, ])

Everytime_df <- Everytime_df %>% 
  separate(colnames(Everytime_df), c('type', 'datetime'), '", "', extra = 'merge') %>% 
  separate(datetime, c('datetime', 'content'), '", "', extra = 'merge')

Everytime_df <- Everytime_df %>% 
  mutate(type = gsub('"', '', type), 
         content = gsub('"', '', content))

### 1. Tokenization
# 띄어쓰기가 안되어 있는 데이터를 수정
# spaced_word <- read.csv('spaced_word.csv', header = T, fileEncoding = "CP949", encoding = "UTF-8")[, c('word', 'spaced_word')]

# spaced_word <- spaced_word %>% 
#   distinct(word, spaced_word)

# for (i in 1:dim(Everytime_df)[1]){
#   print(i)
#   target <- Everytime_df$content[i]
#   for (j in 1:dim(spaced_word)[1]){
#     if (str_detect(target, spaced_word$word[j])){
#       target <- gsub(spaced_word$word[j], spaced_word$spaced_word[j], target)
#     }
#   }
#   Everytime_df$new_content[i] <- target
# }

df <- read.csv('/Users/paul/Desktop/Data_Mining_Practicum/data/spaced_eta_data.csv')

Everytime_df <- df %>% 
  select(-X, -content)
colnames(Everytime_df)[3] <- 'content'

# 필요 없는 데이터 삭제 및 수정
Everytime_df <- Everytime_df[-grep('삭제된 댓글입니다.', Everytime_df$content), ]
Everytime_df$datetime <- ifelse(str_detect(Everytime_df$datetime, '분 전'), 
                                '10/05 12:00', Everytime_df$datetime)

str(Everytime_df)

# datetime을 date type으로 바꿔주기 위해 데이터 수정
Everytime_df$datetime <- ifelse(str_length(Everytime_df$datetime) == 11, 
                                paste0('20/', Everytime_df$datetime), 
                                Everytime_df$datetime)

Everytime_df$datetime <- as.POSIXct(Everytime_df$datetime, format = '%y/%m/%d %H:%M')

# post와 comment들을 매칭시킬 수 있도록 post_num 변수 추가
i <- 1
Everytime_df$post_num <- NA
for (row in 1:dim(Everytime_df)[1]){
  if (Everytime_df$type[row] == 'post'){
    Everytime_df$post_num[row] <- i
    i <- i + 1
  }
}

Everytime_df$post_num <- na.locf(Everytime_df$post_num)
ori_Everytime_df <- Everytime_df

# 특수 문자와 자음과 모음으로만 이루어진 문자 제거
Everytime_df <- Everytime_df %>% 
  mutate(content1 = gsub("[[:punct:]]", " ", content),     # 특수 문자 제거
         content2 = gsub("\\d+", " ", content1),           # 숫자로 이루어진 문자 제거
         content3 = gsub("[^가-힣]+", " ", content2),      # 한글이 아닌 것들 제거
         content4 = gsub("[ㄱ-ㅎㅏ-ㅣ]+", " ", content3),  # 자음과 모음으로만 이루어진 문자 제거       
         target_content = gsub("  ", " ", content4)) %>%   
  select(-content1, -content2, -content3, -content4)

# KoNLP 패키지의 SimplePos09로 형태소를 분리하기
Everytime_df <- Everytime_df %>% 
  unnest_tokens(word_type, target_content, SimplePos09)

# 체언과 용언만 남기기
noun <- Everytime_df %>% 
  filter(str_detect(word_type, '/n')) %>% 
  mutate(word = str_remove(word_type, '/.*$'))

verb <- Everytime_df %>% 
  filter(str_detect(word_type, '/p')) %>% 
  mutate(word = str_replace_all(word_type, '/.*$', '다'))

# 불용어 제거하기
Korean_stop_words <- readLines('https://raw.githubusercontent.com/stopwords-iso/stopwords-ko/master/stopwords-ko.txt')
Korean_stop_words <- data.frame(data.frame(Korean_stop_words))

Korean_stop_words2 <- as.data.frame(stopwords::stopwords('ko', source = 'marimo'))
colnames(Korean_stop_words2) <- colnames(Korean_stop_words)[1]
Korean_stop_words <- rbind(Korean_stop_words, Korean_stop_words2)

Everytime_df <- bind_rows(noun, verb) %>% 
  arrange(desc(type)) %>% 
  arrange(desc(datetime)) %>% 
  filter(nchar(word) > 1) %>% 
  select(-word_type) %>% 
  anti_join(Korean_stop_words, by = c('word' = 'Korean_stop_words'))

### 2. 데이터 탐색 및 결과 설명
# 하나의 글에 댓글이 평균적으로 몇 개 정도 달리는지?
comment_mean <- ori_Everytime_df %>% 
  group_by(post_num) %>% 
  dplyr::mutate(post_num_sum = sum(n()-1)) %>% 
  group_by(post_num) %>% 
  dplyr::summarise(post_num = unique(post_num_sum)) %>% 
  dplyr::summarise(mean = mean(post_num)) %>% 
  unlist() %>% 
  unname()

ori_Everytime_df %>% 
  group_by(post_num) %>% 
  dplyr::mutate(post_num_sum = sum(n()-1)) %>% 
  group_by(post_num) %>% 
  dplyr::summarise(post_num = unique(post_num_sum)) %>% 
  ggplot(aes(x = post_num)) + 
  geom_histogram(bins = 200, color = 'black') + 
  geom_vline(xintercept = comment_mean, color="red",  size = 2)+
  labs(title="Histogram of the number of comments per post",
       x= "Number of comment") + 
  theme(plot.title = element_text(hjust=0.5, size = 30))

# 학기 중과 방학 중에 글의 수
ori_Everytime_df %>% 
  group_by(year(datetime)) %>% 
  dplyr::summarise(sum = n())

ori_Everytime_df %>% 
  filter((year(datetime) == 2019) | (year(datetime) == 2020)) %>% 
  dplyr::mutate(label = ifelse(date(datetime) < '2019-02-25', 'vacation', 
                               ifelse(date(datetime) < '2019-06-15', 'semester', 
                                      ifelse(date(datetime) < '2019-08-26', 'vacation', 
                                             ifelse(date(datetime) < '2019-12-14', 'semester', 
                                                    ifelse(date(datetime) < '2020-02-24', 'vacation', 
                                                           ifelse(date(datetime) < '2020-06-20', 'semester', 
                                                                  ifelse(date(datetime) < '2020-08-31', 'vacation', 'semester')))))))) %>% 
  group_by(date(datetime)) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::rename(Date = 'date(datetime)') %>% 
  ggplot(aes(x = Date, y = count)) + geom_line(alpha = 0.3) + 
  geom_ma(ma_fun = SMA, n = 10, size = 2, linetype = 1) + 
  geom_vline(xintercept = as.numeric(ymd('2019-02-25')), color = 'red', size = 1, alpha = 0.4) + 
  geom_text(aes(x = as.Date('2019-02-25'), y = 200, label = 'Start 2019 Spring semester'), 
            size = 4, color = 'red', nudge_x = 35, check_overlap = TRUE) + 
  geom_vline(xintercept = as.numeric(ymd('2019-06-15')), color = 'orange', size = 1, alpha = 0.4) + 
  geom_text(aes(x = as.Date('2019-06-15'), y = 190, label = 'Start 2019 summer vacation'), 
            size = 4, color = 'orange', nudge_x = 35, check_overlap = TRUE) +
  geom_vline(xintercept = as.numeric(ymd('2019-08-26')), color = 'red', size = 1, alpha = 0.4) + 
  geom_text(aes(x = as.Date('2019-08-26'), y = 180, label = 'Start 2019 Fall semester'), 
            size = 4, color = 'red', nudge_x = 35, check_overlap = TRUE) + 
  geom_vline(xintercept = as.numeric(ymd('2019-12-14')), color = 'orange', size = 1) + 
  geom_text(aes(x = as.Date('2019-12-14'), y = 170, label = 'Start 2019 winter vacation'), 
            size = 4, color = 'orange', nudge_x = 35, check_overlap = TRUE) +
  geom_vline(xintercept = as.numeric(ymd('2020-02-24')), color = 'red', size = 1, alpha = 0.4) + 
  geom_text(aes(x = as.Date('2020-02-24'), y = 160, label = 'Start 2020 Spring semester'), 
            size = 4, color = 'red', nudge_x = 35, check_overlap = TRUE) + 
  geom_vline(xintercept = as.numeric(ymd('2020-06-20')), color = 'orange', size = 1) + 
  geom_text(aes(x = as.Date('2020-06-20'), y = 150, label = 'Start 2020 summer vacation'), 
            size = 4, color = 'orange', nudge_x = 35, check_overlap = TRUE) + 
  geom_vline(xintercept = as.numeric(ymd('2020-08-31')), color = 'red', size = 1, alpha = 0.4) + 
  geom_text(aes(x = as.Date('2020-08-31'), y = 140, label = 'Start 2020 Fall semester'), 
            size = 4, color = 'red', nudge_x = 35, check_overlap = TRUE)

# post와 comment에서 많이 사용된 단어
Everytime_df %>% 
  dplyr::count(word, sort = T) %>% 
  dplyr::filter(str_length(word) >= 2 & n > 1000) %>% 
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_col() + xlab("Word") + ylab("Count") +
  ggtitle("Most Frequent Words in Everytime(HGU)")+
  coord_flip() +   
  theme(plot.title = element_text(hjust=0.5))

noun %>% 
  dplyr::count(word, sort = T) %>% 
  dplyr::filter(str_length(word) >= 2 & n > 500) %>% 
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_col() + xlab("Word") + ylab("Count") +
  ggtitle("Most Frequent Words in Everytime(HGU)")+
  coord_flip() +   
  theme(plot.title = element_text(hjust=0.5))

### 3. 전체 구간을 학기와 방학으로 나누어 TF-IDF 계산
df_tf_idf <- Everytime_df %>% 
  dplyr::filter((year(datetime) == 2019) | (year(datetime) == 2020)) %>% 
  dplyr::mutate(label = ifelse(date(datetime) < '2019-02-25', '2019 winter vacation', 
                               ifelse(date(datetime) < '2019-06-15', '2019 spring semester', 
                                      ifelse(date(datetime) < '2019-08-26', '2019 summer vacation', 
                                             ifelse(date(datetime) < '2019-12-14', '2019 fall semester', 
                                                    ifelse(date(datetime) < '2020-02-24', '2020 winter vacation', 
                                                           ifelse(date(datetime) < '2020-06-20', '2020 spring semester', 
                                                                  ifelse(date(datetime) < '2020-08-31', '2020 summer vacation', '2020 fall semester')))))))) %>% 
  group_by(label) %>% 
  dplyr::count(word, sort = T) %>% 
  dplyr::filter(str_length(word) >= 2) %>% 
  group_by(label) %>% 
  dplyr::mutate(total = sum(n)) %>% 
  bind_tf_idf(word, label, n)%>% 
  dplyr::arrange(desc(tf_idf)) 

df_tf_idf %>% 
  dplyr::mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(label) %>% top_n(6) %>% ungroup() %>% 
  ggplot(aes(x = reorder(word, -tf_idf), y = tf_idf, fill = label)) +
  geom_col(show.legend = F) + labs(X = NULL, y = 'tf_idf') +
  facet_wrap(~label, ncol = 2, scales = 'free') + coord_flip()

### 4. Topic modeling 수행 - 주로 어떠한 주제들이 언급되는지 유추
label_dtm <- noun %>% 
  dplyr::filter(str_length(word) >= 2) %>% 
  dplyr::filter((year(datetime) == 2019) | (year(datetime) == 2020)) %>% 
  dplyr::mutate(label = ifelse(date(datetime) < '2019-02-25', '2019 winter vacation', 
                               ifelse(date(datetime) < '2019-06-15', '2019 spring semester', 
                                      ifelse(date(datetime) < '2019-08-26', '2019 summer vacation', 
                                             ifelse(date(datetime) < '2019-12-14', '2019 fall semester', 
                                                    ifelse(date(datetime) < '2020-02-24', '2020 winter vacation', 
                                                           ifelse(date(datetime) < '2020-06-20', '2020 spring semester', 
                                                                  ifelse(date(datetime) < '2020-08-31', '2020 summer vacation', '2020 fall semester')))))))) %>% 
  group_by(label) %>% 
  dplyr::count(word, sort = T) %>% 
  cast_dtm(label, word, n)

label_lda <- LDA(label_dtm, k = 12, control = list(seed = 1234))
label_topics <- tidy(label_lda, matrix = 'beta')

label_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = 'free') +
  coord_flip() + scale_x_reordered()

except_word <- c('사람', '생각', '진짜', '근데', '우리', '얘기', '때문', '정도', '여자', '남자', '그것', '누구', 
                 '감사합니', '자신', '사실', '자기', '경우', '마음', '사람들', '이유')

label_dtm <- noun %>% 
  dplyr::filter(str_length(word) >= 2) %>% 
  dplyr::filter((year(datetime) == 2019) | (year(datetime) == 2020)) %>% 
  dplyr::filter(!word %in% except_word) %>% 
  dplyr::mutate(label = ifelse(date(datetime) < '2019-02-25', '2019 winter vacation', 
                               ifelse(date(datetime) < '2019-06-15', '2019 spring semester', 
                                      ifelse(date(datetime) < '2019-08-26', '2019 summer vacation', 
                                             ifelse(date(datetime) < '2019-12-14', '2019 fall semester', 
                                                    ifelse(date(datetime) < '2020-02-24', '2020 winter vacation', 
                                                           ifelse(date(datetime) < '2020-06-20', '2020 spring semester', 
                                                                  ifelse(date(datetime) < '2020-08-31', '2020 summer vacation', '2020 fall semester')))))))) %>% 
  group_by(label) %>% 
  dplyr::count(word, sort = T) %>% 
  cast_dtm(label, word, n)

label_lda <- LDA(label_dtm, k = 4, control = list(seed = 1234))
label_topics <- tidy(label_lda, matrix = 'beta')

label_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = 'free') +
  coord_flip() + scale_x_reordered()

### 5. 시기별(분기, 월, 주차)로 감성분석을 수행하고, 결과 분석
pos.words <- read.table('https://raw.githubusercontent.com/park1200656/KnuSentiLex/master/KnuSentiLex/pos_pol_word.txt', skip = 20)
neg.words <- read.table('https://raw.githubusercontent.com/park1200656/KnuSentiLex/master/KnuSentiLex/neg_pol_word.txt', skip = 19)
colnames(pos.words) <- 'positive'
colnames(neg.words) <- 'negative'

sentiment_words <- read.delim('https://raw.githubusercontent.com/park1200656/KnuSentiLex/master/SentiWord_Dict.txt')
sentiment_words$X1 <- ifelse(is.na(sentiment_words$X1), -1, sentiment_words$X1)
sentiment_words$X... <- ifelse(sentiment_words$X... == '갈등 -1', '갈등', sentiment_words$X...)
colnames(sentiment_words) <- c('word', 'score')

sentiment_df <- Everytime_df %>% 
  left_join(sentiment_words, by = 'word')

sentiment_df %>% 
  dplyr::filter((year(datetime) == 2019) | (year(datetime) == 2020)) %>% 
  dplyr::mutate(label = ifelse(date(datetime) < '2019-02-25', '2019 winter vacation', 
                               ifelse(date(datetime) < '2019-06-15', '2019 spring semester', 
                                      ifelse(date(datetime) < '2019-08-26', '2019 summer vacation', 
                                             ifelse(date(datetime) < '2019-12-14', '2019 fall semester', 
                                                    ifelse(date(datetime) < '2020-02-24', '2020 winter vacation', 
                                                           ifelse(date(datetime) < '2020-06-20', '2020 spring semester', 
                                                                  ifelse(date(datetime) < '2020-08-31', '2020 summer vacation', '2020 fall semester')))))))) %>% 
  dplyr::summarise(na_sum = sum(is.na(score)))

# NA를 제거하여 감성 점수가 반영된 데이터의 시기 별 분포
sentiment_df[!is.na(sentiment_df$score), ] %>% 
  filter((year(datetime) == 2019) | (year(datetime) == 2020)) %>% 
  ggplot(aes(x = datetime)) + 
  geom_histogram(bins = 200, color = 'black')

# 학기 별 감성분석
sentiment_df %>% 
  dplyr::filter((year(datetime) == 2019) | (year(datetime) == 2020)) %>% 
  dplyr::mutate(label = ifelse(date(datetime) < '2019-02-25', '2019 winter vacation', 
                               ifelse(date(datetime) < '2019-06-15', '2019 spring semester', 
                                      ifelse(date(datetime) < '2019-08-26', '2019 summer vacation', 
                                             ifelse(date(datetime) < '2019-12-14', '2019 fall semester', 
                                                    ifelse(date(datetime) < '2020-02-24', '2020 winter vacation', 
                                                           ifelse(date(datetime) < '2020-06-20', '2020 spring semester', 
                                                                  ifelse(date(datetime) < '2020-08-31', '2020 summer vacation', '2020 fall semester')))))))) %>% 
  group_by(label) %>% 
  dplyr::summarise(mean_score = mean(score, na.rm = T)) %>% 
  dplyr::arrange(desc(mean_score)) %>% 
  ggplot(aes(x = factor(label), y = mean_score)) + 
  geom_bar(stat = 'identity')

# 월 별 감성분석
sentiment_df %>% 
  group_by(month(datetime)) %>% 
  dplyr::summarise(mean_score = mean(score, na.rm = T)) %>% 
  ggplot(aes(x = factor(`month(datetime)`), y = mean_score)) + 
  geom_bar(stat = "identity")

# 분기 별 감성분석
sentiment_df %>% 
  mutate(month = month(datetime), 
         month_label = ifelse(month %in% c(1, 2, 3), '1 quarter', 
                              ifelse(month %in% c(4, 5, 6), '2 quarter', 
                                     ifelse(month %in% c(7, 8, 9), '3 quarter', '4 quarter')))) %>% 
  group_by(month_label) %>% 
  dplyr::summarise(mean_score = mean(score, na.rm = T)) %>% 
  ggplot(aes(x = factor(month_label), y = mean_score)) + 
  geom_bar(stat = "identity")

help(cast_dtm)
