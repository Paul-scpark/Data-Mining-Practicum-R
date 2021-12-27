## Practice 4
## Text Mining

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
#devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
library(ggrepel)
theme_set(theme_grey(base_family='NanumGothic'))
options("scipen" = 1000)

### 1-1. Loading data
setwd('./Desktop/NIV_English_Bible/')
file_lists <- list.files(getwd(), pattern = "*.txt")

bible <- data_frame()
for (book in file_lists){
  df <- read.table(book, sep = '\n', header = T, quote = '')
  title <- colnames(df)
  
  df <- df %>% 
    separate(title, c('Chapter', 'Verse'), ':', extra = 'merge') %>% 
    separate(Verse, c('Verse', 'Script'), '\\s', extra = 'merge') %>% 
    mutate(Book = title)
  
  bible <- bind_rows(bible, df)
}

bible <- bible[, c('Book', 'Chapter', 'Verse', 'Script')]
unique(bible$Book)

### 1-2. Tokenizations, Frequency analysis, Visualization
data(stop_words)

# Tokenizations without stopwords
tidy_bible <- bible %>% 
  unnest_tokens(word, Script) %>% 
  anti_join(stop_words) %>% 
  filter(grepl("[A-za-z]", word)) %>% 
  mutate(word = gsub("'s", '', word))

bible_word <- tidy_bible %>% 
  count(word, sort = T)

# Frequecy analysis
old <- unique(tidy_bible$Book)[1:39]
new <- unique(tidy_bible$Book)[40:66]

tidy_bible %>% 
  count(Book, word) %>% 
  group_by(Book) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  filter(word %in% head(bible_word$word, 4)) %>% 
  spread(Book, proportion) %>% 
  replace(is.na(.), 0) %>% 
  melt(id = 'word') %>% 
  mutate(label = ifelse(variable %in% old, 'old', 'new')) %>% 
  ggplot(aes(x = variable, y = value, group = word)) +
  geom_line(aes(color = word)) + 
  facet_grid(rows = vars(label)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab(NULL) + ylab(NULL)

# Visualization
bible_word %>% 
  filter(n > 1000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() + xlab(NULL) + ylab(NULL) +
  coord_flip()

# wordcloud
wordcloud2(bible_word)

### 2-1. New, Old - Frequency analysis, frequent words
tidy_bible <- tidy_bible %>% 
  mutate(label = ifelse(Book %in% old, 'old', 'new'))

# Most common words by New and Old Testament
tidy_bible %>% 
  count(word, label, sort = T) %>% 
  group_by(label) %>% 
  top_n(20) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x = word, y = n, fill = label)) +
  geom_col(show.legend = F) +
  facet_wrap(~label, scales = 'free_y') +
  coord_flip() + xlab(NULL) + ylab(NULL)

# wordcloud by New and Old Testament
tidy_bible %>% 
  count(word, label, sort = T) %>% 
  acast(word ~ label, value.var = 'n', fill = 0) %>% 
  comparison.cloud(colors = c('darkred', 'darkcyan'), 
                   max.words = 300)

### 2-2. Additional stopwords
# 위에서 숫자와 어퍼스트로피 s를 제거했음!

### 2-3. Word appearance in both New and Old Testament
new_old_word <- tidy_bible %>% 
  count(word, label, sort = T) %>% 
  group_by(word) %>% 
  filter(n() == 2 & n >= 10) %>% 
  filter(n() == 2) %>% 
  left_join(bible_word, by = 'word') %>% 
  mutate(prop = n.x / n.y) %>% 
  select(-n.x, -n.y) %>% 
  spread(label, prop) %>% 
  rename(prop_A_new = new, prop_B_old = old) %>% 
  mutate(log_ratio = log(prop_A_new / prop_B_old)) %>% 
  arrange(desc(log_ratio))

bind_rows(head(new_old_word, 20) %>% mutate(label = 'top_20'), 
          tail(new_old_word, 20) %>% mutate(label = 'under_20')) %>% 
  ggplot(aes(x = word, y = log_ratio, fill = label)) +
  geom_col(show.legend = F) +
  facet_wrap(~label, scales = 'free_y', nrow = 2) +
  coord_flip() +
  xlab(NULL) + ylab(NULL) +
  theme(axis.text = element_text(size = 8))

### 3-1. Sentiment analysis by New and Old Testament
tidy_bible %>% 
  inner_join(get_sentiments('afinn')) %>% 
  group_by(label) %>% 
  summarise(sentiment_mean = mean(value))

bible_sentiment <- tidy_bible %>% 
  count(word, label, sort = T) %>% 
  inner_join(get_sentiments('bing')) 

bible_sentiment %>% 
  group_by(label, sentiment) %>% 
  summarise(count = n()) %>% 
  spread(sentiment, count)

### 3-2. Frequency of sentiment word by New and Old Testament
bible_sentiment %>% 
  arrange(desc(n)) %>% 
  filter(label == 'old') %>% 
  head(10)

bible_sentiment %>% 
  arrange(desc(n)) %>% 
  filter(label == 'new') %>% 
  head(10)

bible_sentiment %>% 
  group_by(word) %>% 
  filter(n() == 2 & n >= 100) %>% 
  filter(n() == 2) %>% 
  ggplot() + facet_grid(cols = vars(sentiment)) +
  geom_bar(aes(x = reorder(word, -n), y = n, fill = label), stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 1), 
        axis.text = element_text(size = 8)) + 
  xlab(NULL) + ylab(NULL)
  
### 3-3. Sentiment analysis by each bible book
tidy_bible %>% 
  inner_join(get_sentiments('bing')) %>% 
  group_by(Book, sentiment) %>% 
  summarise(value = n()) %>% 
  group_by(sentiment) %>% 
  mutate(prop = value / sum(value), 
         pos = (cumsum(c(0, prop)) + c(prop / 2, .01))[1:n()]) %>% 
  ggplot(aes(x = '', y = prop, fill = Book)) +
  coord_polar('y') + facet_grid(cols = vars(sentiment)) +
  geom_col(color = 'black', position = position_stack(reverse = T), show.legend = F) +
  geom_text_repel(aes(x = 1.4, y = pos, label = ifelse(prop >= 0.03, paste(Book, round(prop, 3)), '')), 
                  nudge_x = 0.3, segment.size = 0.5, size = 2.5, show.legend = FALSE) + 
  theme_void() + theme(strip.text.x = element_text(size = 15))

### 3-4. Sentiment analysis for long books by chapter
long_bible <- tidy_bible %>% 
  group_by(Book) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head()

bible_sentiment <- tidy_bible %>% 
  filter(Book %in% long_bible$Book) %>% 
  inner_join(get_sentiments('bing')) %>% 
  count(Book, index = as.integer(Chapter) %/% 5, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

bible_sentiment %>% 
  ggplot(aes(x = index, y = sentiment, fill = Book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Book, ncol = 2, scales = 'free_x')