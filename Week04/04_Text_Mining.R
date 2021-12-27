'''
2021-2 - Data Mining Practicum
Prof. Hyebong Choi

Ch 3. Text Mining
'''

install.packages('dplyr')
install.packages('tidyr')
install.packages('ggplot2')
install.packages('tidytext')
install.packages('gutenbergr')
install.packages('textdata')
install.packages('wordcloud')
install.packages('topicmodels')

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(janeaustenr)
library(stringr)
library(gutenbergr)
library(scales)
library(textdata)
library(wordcloud)
library(reshape2)
library(topicmodels)

## tidytext - unnest_tokens
text <- c('Because I could not stop for Death -', 
          'He kindly stopped for me -', 
          'The Carriage held but just Ourselves -', 
          'and Immortality')

text_df <- tibble(line = 1:4, text = text)
text_df %>% unnest_tokens(word, text)

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text, regex('^chapter [\\divxlc]', ignore_case = T)))) %>% 
  ungroup()

tidy_books <- original_books %>% 
  unnest_tokens(word, text)

## tidytext - removing stop-words
data(stop_words)

tidy_books <- tidy_books %>% 
  anti_join(stop_words)

tidy_books %>% 
  count(word, sort = T)

## Visualization
tidy_books %>% 
  count(word, sort = T) %>% 
  filter(n > 600) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## gutenbergr package
hgwells <- gutenberg_download(c(35, 36, 5230, 159), mirror = "http://mirrors.xmission.com/gutenberg/")
tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_hgwells %>% 
  count(word, sort = T)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767), mirror = "http://mirrors.xmission.com/gutenberg/")
tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_bronte %>% 
  count(word, sort = T)

## Comparison with frequent words
frequency <- bind_rows(mutate(tidy_bronte, author = 'Bronte Sisters'), 
                       mutate(tidy_hgwells, author = 'H.G. Wells'), 
                       mutate(tidy_books, author = 'Jane Austen')) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, 'Bronte Sisters':'H.G. Wells')

frequency %>% 
  arrange(word)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = 'gray40', lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = T, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = 'darkslategray4', high = 'gray75') +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position = 'none') +
  labs(y = 'Jane Austen', x = NULL)

cor.test(data = frequency[frequency$author == 'Bronte Sisters', ], ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == 'H.G. Wells', ], ~ proportion + `Jane Austen`)

## Sentiment Analysis
get_sentiments('afinn')
get_sentiments('bing')
get_sentiments('nrc')

## Sentiment Analysis with inner join
tidy_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = T)))) %>% 
  ungroup() %>% 
  unnest_tokens(word, text)

nrc_joy <- get_sentiments('nrc') %>% 
  filter(sentiment == 'joy')

tidy_books %>% 
  filter(book == 'Emma') %>% 
  inner_join(nrc_joy) %>% 
  count(word, sort = T)

jane_austen_sentiment <- tidy_books %>% 
  inner_join(get_sentiments('bing')) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = F) +
  facet_wrap(~book, ncol = 2, scales = 'free_x')

bing_word_counts <- tidy_books %>% 
  inner_join(get_sentiments('bing')) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()

bing_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = 'free_y') +
  labs(y = 'Contribution to sentiment', 
       x = NULL) + 
  coord_flip()

## Add a new word to stopwords list
custom_stop_words <- bind_rows(data_frame(word = c('miss'),
                                          lexicon = c('custom')), 
                               stop_words)

bing_word_counts %>% 
  anti_join(custom_stop_words) %>% 
  group_by(sentiment) %>%  top_n(5) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = 'free_y') +
  labs(y = 'Contribution to sentiment', 
       x = NULL) + 
  coord_flip()

## Wordcloud
tidy_books %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

tidy_books %>% 
  inner_join(get_sentiments('bing')) %>% 
  count(word, sentiment, sort = T) %>% 
  acast(word ~ sentiment, value.var = 'n', fill = 0) %>% 
  comparison.cloud(colors = c('darkred', 'darkcyan'), 
                   max.words = 100)

## Most negative chapters in each of Jane Austen's novels?
bingnegative <- get_sentiments('bing') %>% 
  filter(sentiment == 'negative')

wordcounts <- tidy_books %>% 
  group_by(book, chapter) %>% 
  summarize(words = n())

tidy_books %>% 
  semi_join(bingnegative) %>% 
  group_by(book, chapter) %>% 
  summarize(negativewords = n()) %>% 
  left_join(wordcounts, by = c('book', 'chapter')) %>% 
  mutate(ratio = negativewords / words) %>% 
  filter(chapter != 0) %>% 
  top_n(1) %>% 
  ungroup()

#############################################################################

## TF-IDF - Counting word frequency of Jane Austen's works
book_words <- austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(book, word, sort = T) %>% 
  ungroup()

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = F) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = 'free_y')

## term_frequency
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n / total)

## Zipf's law
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.1, alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10()

## bind_tf_idf
book_words <- book_words %>% 
  bind_tf_idf(word, book, n)

book_words %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

book_words %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = F) +
  labs(X = NULL, y = 'tf-idf') +
  facet_wrap(~book, ncol = 2, scales = 'free') +
  coord_flip()

## Topic modeling
data('AssociatedPress')

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = 'beta')

ap_top_terms <- ap_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

## word-topic probabilities
ap_top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = 'free') +
  coord_flip() +
  scale_x_reordered()

## terms with greatest difference in beta
beta_spread <- ap_topics %>% 
  mutate(topic = paste0('topic', topic)) %>% 
  spread(topic, beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>% 
  mutate(log_ratio = log2(topic2 / topic1))

bind_rows(beta_spread %>% arrange(desc(log_ratio)) %>% head(10), 
          beta_spread %>% arrange(desc(log_ratio)) %>% tail(10)) %>% 
  arrange(desc(log_ratio)) %>% 
  ggplot(aes(x = reorder(term, -log_ratio), y = log_ratio)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  coord_flip()

## Document-topic probabilities
ap_documents <- tidy(ap_lda, matrix = 'gamma')