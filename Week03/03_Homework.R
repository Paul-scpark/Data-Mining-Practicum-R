## Practice3
## Visualization

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(tidyquant)
theme_set(theme_grey(base_family='NanumGothic'))
options("scipen" = 1000)

### Loading data
GDP <- read.csv('https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/GDP_per_capita.csv')
life_expectancy <- read.csv('https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/Life_expectancy.csv')
pop <- read.csv('https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/population_total.csv')
covid_df <- read.csv('https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/covid19_210903.csv')
continent <- read.csv('https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/continent_table.csv')

### Preprocessing
colnames(GDP) <- GDP[2, ]
GDP <- GDP[3:199, ]
GDP <- gather(GDP, time, GDP, 3:243)
colnames(GDP)[2] <- 'name'

pop <- gather(pop, time, POP, 2:302)
pop$time <- gsub('X', '', pop$time)

str(GDP)
str(life_expectancy)
str(pop)
life_expectancy$time <- as.character(life_expectancy$time)

unique(GDP$geo) %>% length()
unique(life_expectancy$geo) %>% length()
unique(pop$country) %>% length()

colSums(is.na(GDP))
colSums(is.na(life_expectancy))
colSums(is.na(pop))
unique(GDP[is.na(GDP$GDP), ]$geo)
GDP <- drop_na(GDP)

total_df <- inner_join(GDP, life_expectancy, by = c('geo', 'time', 'name'))
total_df <- total_df[order(total_df$geo), ]
row.names(total_df) <- NULL

unique(total_df$geo) %>% length()
colSums(is.na(total_df))

setdiff(total_df$name, pop$country)
setdiff(pop$country, total_df$name)
total_df <- left_join(total_df, pop, by = c('name' = 'country', 'time'))
unique(total_df[is.na(total_df$POP), ]$name)

### 1-1. GDP & Life_expectancy's correlation
now_df <- total_df %>% 
            filter(time == '2021') %>% 
            mutate(pop = POP * 0.00001)

head(now_df)

now_df %>% 
  ggplot(aes(x = GDP, y = Life.expectancy, label = name)) +
  geom_point(aes(size = pop)) + 
  geom_text(aes(label=ifelse(GDP > 60000, as.character(name), '')), size = 3, hjust=0, vjust=0) + 
  geom_text(aes(label=ifelse(Life.expectancy < 59.5, as.character(name), '')), size = 3, hjust=0, vjust=0) + 
  scale_size_continuous(
    breaks = c(10, 500, 1000, 10000)) + 
  geom_smooth(method = lm) +
  ggtitle('GDP per capita vs Life expectancy, 2021') + 
  theme(plot.title = element_text(hjust=0.5))

cor(now_df$GDP, now_df$Life.expectancy, use = "all.obs", method = "pearson")

### 1-2. GDP(log) & Life_expectancy's correlation
now_df$GDP_log <- log(now_df$GDP)
now_df %>% 
  ggplot(aes(x = GDP_log, y = Life.expectancy, label = name)) +
  geom_point(aes(size = pop)) + 
  geom_text(aes(label=ifelse(GDP_log > 11.1, as.character(name), '')), size = 3, hjust=0, vjust=0) + 
  geom_text(aes(label=ifelse(Life.expectancy < 59.5, as.character(name), '')), size = 3, hjust=0, vjust=0) + 
  scale_size_continuous(
    breaks = c(0, 10, 500, 1000, 10000)) + 
  geom_smooth(method = lm) +
  ggtitle('GDP(log) per capita vs Life expectancy, 2021') + 
  theme(plot.title = element_text(hjust=0.5))

cor(now_df$GDP_log, now_df$Life.expectancy, use = "all.obs", method = "pearson")

### 2-1. Distribution of COVID-19 Confirmers and Death by Continent
str(covid_df)
covid_df$date <- as.Date(covid_df$date)
covid_df %>% 
  filter(date == '2020-01-22', 
         country == 'Afghanistan') %>% 
  head()

covid_df <- spread(covid_df, type, cases)
head(covid_df)

total_df <- total_df %>% 
  mutate(name = replace(name, name == 'Congo, Rep.', 'Republic of Congo'), 
         name = replace(name, name == 'Czech Republic', 'Czechia'), 
         name = replace(name, name == 'Dominican Republic', 'Dominica'), 
         name = replace(name, name == 'Lao', 'Laos'), 
         name = replace(name, name == 'Micronesia, Fed. Sts.', 'Micronesia'), 
         name = replace(name, name == 'Macedonia, FYR', 'North Macedonia'), 
         name = replace(name, name == 'Slovak Republic', 'Slovakia'), 
         name = replace(name, name == 'St. Lucia', 'Saint Lucia'), 
         name = replace(name, name == 'St. Vincent and the Grenadines', 'Saint Vincent and the Grenadines'), 
         name = replace(name, name == 'United States', 'USA'), 
         name = replace(name, name == 'United Kingdom', 'UK'))

covid_df <- total_df %>% 
              filter(time %in% c(2020, 2021)) %>% 
              group_by(name) %>% 
              summarise(GDP = mean(GDP), 
                        Life.expectancy = mean(Life.expectancy), 
                        POP = mean(POP)) %>% 
              right_join(covid_df, by = c('name' = 'country')) %>% 
              select(date, name, lat, long, GDP, Life.expectancy, POP, confirmed, death, recovered)

unique(covid_df[is.na(covid_df$GDP), ]$name)

covid_df %>% 
  group_by(name) %>% 
  mutate(confirmed_cumsum = cumsum(replace_na(confirmed, 0)), 
         death_cumsum = cumsum(replace_na(death, 0))) %>% 
  filter(date == '2021-05-27', name == 'Australia')
  
now_covid_df <- covid_df %>% 
                  group_by(name) %>% 
                  mutate(confirmed_cumsum = cumsum(replace_na(confirmed, 0)), 
                         death_cumsum = cumsum(replace_na(death, 0)),
                         recovered_cumsum = cumsum(replace_na(recovered, 0))) %>% 
                  filter(date == '2021-05-27') %>% 
                  group_by(name) %>%
                  filter(row_number() == n()) %>% 
                  select(-confirmed, -death, -recovered) %>% 
                  left_join(continent[, c('continent', 'location')], by = c('name' = 'location'))

now_covid_df[is.na(now_covid_df$continent), ]
now_covid_df[now_covid_df$name == 'USA', ]$continent <- 'North America'
now_covid_df[now_covid_df$name == 'UK', ]$continent <- 'Europe'
now_covid_df[now_covid_df$name == 'Kiribati', ]$continent <- 'Oceania'
now_covid_df[now_covid_df$name == 'Timor-Leste', ]$continent <- 'Asia'
now_covid_df[now_covid_df$name == 'Burma', ]$continent <- 'Asia'
now_covid_df[now_covid_df$name == 'Cabo Verde', ]$continent <- 'Africa'
now_covid_df[now_covid_df$name == 'Republic of Congo', ]$continent <- 'Africa'
now_covid_df[now_covid_df$name == 'Micronesia', ]$continent <- 'Oceania'
now_covid_df[now_covid_df$name == 'West Bank and Gaza', ]$continent <- 'Asia'

now_covid_df$confirmed_per_10k <- now_covid_df$confirmed_cumsum / (now_covid_df$POP / 10000)
now_covid_df$death_per_10k <- now_covid_df$death_cumsum / (now_covid_df$POP / 10000)
now_covid_df$recovered_per_10k <- now_covid_df$recovered_cumsum / (now_covid_df$POP / 10000)
now_covid_df <- na.omit(now_covid_df)
head(now_covid_df)

now_covid_df %>% 
  filter(!is.na(confirmed_per_10k)) %>% 
  ggplot(aes(x = reorder(continent, -confirmed_per_10k), y = confirmed_per_10k)) +
  geom_boxplot(aes(color = continent)) + 
  geom_jitter(aes(color = continent), position=position_jitter(width=0.3, height=0.2), size=0.5) +
  ggtitle('Confirmed_cumsum by continent, 2021-05-27') + 
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x=element_text(angle=40, hjust=1))

now_covid_df %>% 
  filter(!is.na(death_per_10k)) %>% 
  ggplot(aes(x = reorder(continent, -death_per_10k), y = death_per_10k)) +
  geom_boxplot(aes(color = continent)) + 
  geom_jitter(aes(color = continent), position=position_jitter(width=0.3, height=0.2), size=0.5) +
  ggtitle('Death_cumsum by continent, 2021-05-27') + 
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x=element_text(angle=40, hjust=1))

now_covid_df %>% 
  filter(!is.na(confirmed_per_10k)) %>% 
  group_by(continent) %>% 
  summarise(mean_confirmed = mean(confirmed_per_10k), 
            mean_death = mean(death_per_10k)) %>% 
  arrange(desc(mean_confirmed))

### 3-1. Comparison of COVID-19 situation by countries
top_confirmed_countries <- now_covid_df %>% 
                            arrange(desc(confirmed_cumsum)) %>% 
                            select(name) %>% 
                            head(5) %>% 
                            as.matrix() %>% 
                            c()

top_death_countries <- now_covid_df %>% 
                         arrange(desc(death_cumsum)) %>% 
                         select(name) %>% 
                         head(5) %>% 
                         as.matrix() %>% 
                         c()

covid_df %>% 
  filter(name %in% top_confirmed_countries) %>% 
  group_by(name, date) %>%
  filter(row_number() == n()) %>% 
  ggplot(aes(x = date, y = confirmed, group = name)) + 
  geom_line(aes(color = name)) + 
  geom_vline(xintercept = as.numeric(ymd("2020-12-14")), 
             color = 'black', size = 1) + 
  geom_text(aes(x = as.Date('2020-11-01'), y = 600000, label = 'First vaccine injection'), 
            size = 8, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(min(covid_df$date), max(covid_df$date), by = 40)) + 
  ggtitle('Distribution of daily confirmed for the top 5 countries') + 
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x=element_text(angle=20, hjust=1))

covid_df %>% 
  filter(name %in% top_confirmed_countries) %>% 
  group_by(name) %>% 
  mutate(csum_confirmed = cumsum(replace_na(confirmed, 0)), 
         csum_death = cumsum(replace_na(death, 0)), 
         csum_recovered = cumsum(replace_na(recovered, 0))) %>% 
  group_by(name, date) %>%
  filter(row_number() == n()) %>% 
  ggplot(aes(x = date, y = csum_confirmed, group = name)) + 
  geom_line(aes(color = name)) + 
  geom_vline(xintercept = as.numeric(ymd("2020-12-14")), 
             color = 'black', size = 1) + 
  geom_text(aes(x = as.Date('2020-11-01'), y = 30000000, label = 'First vaccine injection'), 
            size = 8, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(min(covid_df$date), max(covid_df$date), by = 40)) + 
  ggtitle('Distribution of daily confirmed(cumsum) for the top 5 countries') + 
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x=element_text(angle=20, hjust=1))

covid_df %>% 
  filter(name %in% top_death_countries) %>% 
  group_by(name, date) %>%
  filter(row_number() == n()) %>% 
  ggplot(aes(x = date, y = death, group = name)) + 
  geom_line(aes(color = name), alpha = 0.5) +
  geom_ma(aes(color = name), ma_fun = SMA, n = 20, size = 1, linetype = 1) +
  geom_vline(xintercept = as.numeric(ymd("2020-12-14")), 
             color = 'black', size = 1) + 
  geom_text(aes(x = as.Date('2020-11-01'), y = 4000, label = 'First vaccine injection'), 
            size = 8, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(min(covid_df$date), max(covid_df$date), by = 40)) + 
  ggtitle('Distribution of daily death number for the top 5 countries') + 
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x=element_text(angle=20, hjust=1))

covid_df %>% 
  filter(name %in% top_death_countries) %>% 
  group_by(name) %>% 
  mutate(csum_confirmed = cumsum(replace_na(confirmed, 0)), 
         csum_death = cumsum(replace_na(death, 0)), 
         csum_recovered = cumsum(replace_na(recovered, 0))) %>% 
  group_by(name, date) %>%
  filter(row_number() == n()) %>% 
  ggplot(aes(x = date, y = csum_death, group = name)) + 
  geom_line(aes(color = name)) + 
  geom_vline(xintercept = as.numeric(ymd("2020-12-14")), 
             color = 'black', size = 1) + 
  geom_text(aes(x = as.Date('2020-11-01'), y = 600000, label = 'First vaccine injection'), 
            size = 8, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(min(covid_df$date), max(covid_df$date), by = 40)) + 
  ggtitle('Distribution of daily death number(cumsum) for the top 5 countries') + 
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x=element_text(angle=20, hjust=1))