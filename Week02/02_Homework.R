## Practice2
## Trade(Export, Import) data of Korea.

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(unikn)
theme_set(theme_grey(base_family='NanumGothic'))

### Loading data
trade_df <- read.csv('https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/Monthly_export_import.csv', header = T, fileEncoding = "euc-kr")
ISO_code <- read.csv('https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/ISO_code.csv', header = T, fileEncoding = "euc-kr")
continent <- read.csv('https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/continent_table.csv')

### 1. Preprocessing
str(trade_df)
length(unique(trade_df$국가명))
colSums(is.na(trade_df))

trade_df$기간 <- as.character(trade_df$기간)
trade_df[str_length(trade_df$기간) != 7, ]

trade_df$기간 <- str_pad(trade_df$기간, width = 7, side = 'right', pad = '0')
trade_df$기간 <- as.Date(paste0(as.character(trade_df$기간), '.01'), format = '%Y.%m.%d')
unique(trade_df$기간)

trade_df[, 3:7] <- lapply(trade_df[, 3:7], gsub, pattern = ',', replacement = '')
trade_df[, 3:7] <- lapply(trade_df[, 3:7], as.double)
str(trade_df)
head(trade_df)

### 2. Total export and import volumes of Korea
Total_export_import <- as.data.frame(left_join(aggregate(수출건수~기간, trade_df, sum), aggregate(수입건수~기간, trade_df, sum), by = '기간'))
colnames(Total_export_import) <- c('Date', 'Total_export', 'Total_import')
head(Total_export_import)

options("scipen" = 1000)
df_long <- melt(Total_export_import, id = 'Date')
ggplot(df_long, aes(x = Date, y = value, col = variable)) + geom_line()

Total_export_import %>% 
  mutate(month = month(Date), 
         season = ifelse(month %in% c(3, 4, 5), '봄', 
                    ifelse(month %in% c(6, 7, 8), '여름', 
                      ifelse(month %in% c(9, 10, 11), '가을', '겨울')))) %>% 
  select(Total_export, Total_import, season) %>% 
  melt(id = 'season') %>% 
  ggplot(aes(x = factor(season), y = value, fill = variable)) +
  geom_boxplot() +
  ggtitle('계절별 수출 및 수입 건수의 합계') +
  theme(plot.title = element_text(hjust=0.5))

Total_export_import %>% 
  mutate(year = year(Date)) %>% 
  group_by(year) %>% 
  summarise(yearly_export_sum = sum(Total_export), 
            yearly_import_sum = sum(Total_import), 
            yearly_export_mean = mean(Total_export), 
            yearly_import_mean = mean(Total_import)) %>% 
  melt(id = 'year') %>% 
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  ggtitle('연도별 수출, 수입 건수의 합계 및 평균') +
  theme(plot.title = element_text(hjust=0.5))

### 3. Impact of COVID-19 by Korea's export and import
trade_df %>% 
  filter(기간 >= '2010-01-01') %>% 
  group_by(기간) %>% 
  summarise(평균_수출건수 = mean(수출건수), 
            평균_수입건수 = mean(수입건수)) %>% 
  gather(colname, value, 2:3) %>% 
  ggplot(aes(x = 기간, y = value, color = colname)) +
  geom_line(aes(color = colname)) + 
  geom_point(aes(color = colname), size = 0.2) +
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), 
             color = 'black', size = 1, alpha = 0.4) +
  ggtitle('대한민국의 평균 수입, 수출 건수') + 
  theme(plot.title = element_text(hjust=0.5))
  
trade_df %>% 
  group_by(기간) %>% 
  summarise(평균_수출금액 = mean(수출금액), 
            평균_수입금액 = mean(수입금액)) %>% 
  gather(colname, value, 2:3) %>% 
  ggplot(aes(x = 기간, y = value, color = colname)) +
  geom_line(aes(color = colname)) + 
  geom_point(aes(color = colname), size = 0.2) +
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), 
             color = 'black', size = 1) +
  ggtitle('대한민국의 평균 수입, 수출 금액') + 
  theme(plot.title = element_text(hjust=0.5))

trade_df %>% 
  filter(국가명 %in% c('미국', '중국', '일본', '베트남', '싱가포르', '대만'), 
         기간 >= '2010-01-01') %>% 
  select(기간, 국가명, 수출금액, 수입금액, 무역수지) %>% 
  gather(colname, value, 3:5) %>% 
  ggplot(aes(x = 기간, y = value, color = 국가명)) +
  geom_line(aes(color = 국가명)) + 
  geom_point(aes(color = 국가명), size = 0.1) +
  facet_wrap(colname ~ ., ncol = 1) +
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), 
             color = 'black', size = 1)

trade_df %>% 
  filter(국가명 %in% c('미국', '중국', '일본', '베트남', '싱가포르', '대만'), 
         기간 >= '2010-01-01') %>% 
  select(기간, 국가명, 수출건수, 수입건수) %>% 
  gather(colname, value, 3:4) %>% 
  ggplot(aes(x = 기간, y = value, color = 국가명)) +
  geom_line(aes(color = 국가명)) + 
  geom_point(aes(color = 국가명), size = 0.1) +
  facet_wrap(colname ~ ., ncol = 1) +
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), 
             color = 'black', size = 1)

### 4. Adding continent information
ISO_code <- ISO_code %>% 
  select(ISO3, Korean, English) %>% 
  rename(iso_code = ISO3, location = English) %>% 
  mutate(Korean = gsub(' ', '', Korean))

continent$location <- toupper(continent$location)

length(unique(trade_df$국가명))
c(length(unique(ISO_code$iso_code)), length(unique(ISO_code$Korean)))
c(length(unique(continent$iso_code)), length(unique(continent$location)), length(unique(continent$continent)))

length(intersect(ISO_code$iso_code, continent$iso_code))
setdiff(continent$iso_code, ISO_code$iso_code)
setdiff(ISO_code$iso_code, continent$iso_code)

continent_df <- full_join(ISO_code, continent, by = 'iso_code')
dim(continent_df[!is.na(continent_df$continent), ])

continent_df <- left_join(ISO_code, continent, by = 'iso_code')
dim(continent_df[!is.na(continent_df$continent), ])

continent_df %>% 
  filter(location.x != location.y) %>% 
  head(10)

trade_df$국가명 <- gsub(' ', '', trade_df$국가명)
trade_df <- continent_df %>% 
  filter(!is.na(continent)) %>% 
  select(Korean, continent) %>% 
  rename(국가명=Korean) %>% 
  right_join(trade_df, by = '국가명')

unique(trade_df[is.na(trade_df$continent), ]$국가명)

ISO_code <- 
  ISO_code %>% 
    mutate_all(~case_when(. == '건지섬' ~ '건지',  . == '과들루프' ~ '과델로프',  
                          . == '그린란드' ~ '그린랜드',  . == '나우루' ~ '나우르',  
                          . == '도미니카연방' ~ '도미니카',  
                          . == '러시아' ~ '러시아연방',  . == '르완다' ~ '루안다',  
                          . == '룩셈부르크' ~ '룩셈부르그',  . == '마다가스카르' ~ '마다카스카르',  
                          . == '마르티니크' ~ '마티니크',  . == '몬트세랫' ~ '몬트세라트',  
                          . == '미국령버진아일랜드' ~ '미령버진군도',  . == '베네수엘라' ~ '베네주엘라',  
                          . == '벨라루스' ~ '베라루스',  . == '벨리즈' ~ '벨리제',  
                          . == '보스니아헤르체고비나' ~ '보스니아-헤르체고비나',  . == '북마리아나제도' ~ '북마리아나군도',  
                          . == '상투메프린시페' ~ '상토메프린스페',  . == '세이셸' ~ '세이쉘',  
                          . == '솔로몬제도' ~ '솔로몬군도',  . == '스와질란드' ~ '스와질랜드',  
                          . == '아랍에미리트' ~ '아랍에미리트연합',  . == '아프가니스탄' ~ '아프카니스탄',  
                          . == '에리트레아' ~ '에리트리아',  
                          . == '에콰도르' ~ '에쿠아도르',  . == '영국령인도양지역' ~ '영령인도양',  
                          . == '영국령버진아일랜드' ~ '영령버진군도',  . == '예멘' ~ '예맨',  
                          . == '왈리스퓌튀나' ~ '왈라스&퓨투나군도',  . == '우즈베키스탄' ~ '우즈베크',  
                          . == '에티오피아' ~ '이디오피아',  . == '자메이카' ~ '자마이카',  
                          . == '저지섬' ~ '저어지',  . == '중화인민공화국' ~ '중국',  
                          . == '중화민국' ~ '대만',  . == '지브롤터' ~ '지브랄타',
                          . == '차드' ~ '챠드',  . == '체코' ~ '체코공화국',  
                          . == '코모로' ~ '코모로스',  . == '코트디부아르' ~ '코드디봐르',  
                          . == '크리스마스섬' ~ '크리스마스아일랜드',  
                          . == '키프로스' ~ '사이프러스',  . == '타이' ~ '태국',  
                          . == '조선민주주의인민공화국' ~ '북한',  . == '투르크메니스탄' ~ '투르크멘',
                          . == '팔레스타인' ~ '팔레스타인해방기구',  . == '포르투갈' ~ '포루투갈',
                          . == '포클랜드제도' ~ '포클랜드군도',  . == '허드맥도널드제도' ~ '허드앤맥도날드군도',
                          . == '오스트레일리아' ~ '호주', TRUE ~ .))

continent_df <- full_join(ISO_code, continent, by = 'iso_code')
trade_df <- trade_df[, c('국가명', '기간', '수출건수', '수출금액', '수입건수', '수입금액', '무역수지')]
trade_df <- continent_df %>% 
  filter(!is.na(continent)) %>% 
  select(Korean, continent) %>% 
  rename(국가명=Korean) %>% 
  right_join(trade_df, by = '국가명')

unique(trade_df[is.na(trade_df$continent), ]$국가명)

trade_df %>% 
  filter(기간 == '2000-01-01') %>% 
  select(-기간) %>% 
  head(10)

### 5. 대한민국과 무역을 가장 많이 하는 국가와 대륙
trade_df %>%
  group_by(국가명, year(기간)) %>%
  mutate(yearly_total = sum(수출건수 + 수입건수), 
         yearly_import = sum(수입건수), 
         yearly_export = sum(수출건수)) %>% 
  select(continent, yearly_total, yearly_import, yearly_export) %>% 
  unique() %>% 
  group_by(continent) %>% 
  summarise(total_export_import = sum(yearly_total), 
            total_import = sum(yearly_import), 
            total_export = sum(yearly_export)) %>% 
  melt(id = 'continent') %>% 
  ggplot(aes(x = reorder(continent, -value), y = value, fill = variable)) +
  geom_bar(stat = "identity", color = 'black') +
  labs(title = "2000~2021년 대륙별 수출 및 수입건수 합계") + xlab("continent") +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x=element_text(color = "black", size = 10, angle=30, vjust=.8, hjust=0.8))
  
trade_df %>%
  group_by(국가명, year(기간)) %>%
  mutate(yearly_total = sum(수출건수 + 수입건수), 
         yearly_import = sum(수입건수), 
         yearly_export = sum(수출건수)) %>% 
  select(continent, yearly_total, yearly_import, yearly_export) %>% 
  unique() %>% 
  group_by(국가명) %>% 
  summarise(total_export_import = sum(yearly_total), 
            total_import = sum(yearly_import), 
            total_export = sum(yearly_export)) %>% 
  arrange(desc(total_export_import)) %>% 
  head(10) %>% 
  melt(id = '국가명') %>% 
  ggplot(aes(x = reorder(국가명, -value), y = value, fill = variable)) +
  geom_bar(stat = "identity", color = 'black') +
  labs(title = "2000~2021년 대한민국과의 무역 건수 상위 10개국") + xlab("국가명") +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x=element_text(color = "black", size = 10, angle=30, vjust=.8, hjust=0.8))

trade_df %>%
  group_by(국가명, year(기간)) %>%
  mutate(yearly_total = sum(수출건수 + 수입건수), 
         yearly_import = sum(수입건수), 
         yearly_export = sum(수출건수)) %>% 
  filter(year(기간) >= 2018) %>% 
  select(continent, yearly_total, yearly_import, yearly_export) %>% 
  unique() %>% 
  group_by(continent) %>% 
  summarise(total_export_import = sum(yearly_total), 
            total_import = sum(yearly_import), 
            total_export = sum(yearly_export)) %>% 
  melt(id = 'continent') %>% 
  ggplot(aes(x = reorder(continent, -value), y = value, fill = variable)) +
  geom_bar(stat = "identity", color = 'black') +
  labs(title = "2018~2021년 대륙별 수출 및 수입건수 합계") + xlab("continent") +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x=element_text(color = "black", size = 10, angle=30, vjust=.8, hjust=0.8))

trade_df %>%
  group_by(국가명, year(기간)) %>%
  mutate(yearly_total = sum(수출건수 + 수입건수), 
         yearly_import = sum(수입건수), 
         yearly_export = sum(수출건수)) %>% 
  filter(year(기간) >= 2018) %>% 
  select(continent, yearly_total, yearly_import, yearly_export) %>% 
  unique() %>% 
  group_by(국가명) %>% 
  summarise(total_export_import = sum(yearly_total), 
            total_import = sum(yearly_import), 
            total_export = sum(yearly_export)) %>% 
  arrange(desc(total_export_import)) %>% 
  head(10) %>% 
  melt(id = '국가명') %>% 
  ggplot(aes(x = reorder(국가명, -value), y = value, fill = variable)) +
  geom_bar(stat = "identity", color = 'black') +
  labs(title = "2018~2021년 대한민국과의 무역 건수 상위 10개국") + xlab("국가명") +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x=element_text(color = "black", size = 10, angle=30, vjust=.8, hjust=0.8))

### 6. 2000년대 들어와서 우리나라와 무역 규모가 가장 크게 상승한 국가
std_df <- trade_df %>% 
            group_by(국가명, year(기간)) %>% 
            mutate(yearly_total = sum(수출건수 + 수입건수), 
                   yearly_import = sum(수입건수), 
                   yearly_export = sum(수출건수)) %>% 
            select(continent, yearly_total, yearly_import, yearly_export) %>% 
            unique() %>% 
            group_by(국가명) %>% 
            summarise(std = sd(yearly_total)) %>% 
            arrange(desc(std))

head(std_df, 10)
tail(std_df, 10)

top_5 <- head(std_df, 5)$국가명
top_5 <- trade_df %>% 
          group_by(국가명, year(기간)) %>% 
          mutate(yearly_total = sum(수출건수 + 수입건수), 
                 yearly_import = sum(수입건수), 
                 yearly_export = sum(수출건수)) %>% 
          select(continent, yearly_total, yearly_import, yearly_export) %>% 
          unique() %>% 
          filter(국가명 %in% top_5) %>% 
          select(국가명, 'year(기간)', yearly_total)

ggplot(top_5, aes(x = `year(기간)`)) +
  geom_line(aes(y = yearly_total, col = 국가명))

top_5 %>% 
  filter(국가명 %in% c('베트남', '일본', '독일')) %>% 
  ggplot(aes(x = `year(기간)`)) +
  geom_line(aes(y = yearly_total, col = 국가명))

under_10 <- tail(std_df, 10)$국가명  
under_10 <- trade_df %>% 
              group_by(국가명, year(기간)) %>% 
              mutate(yearly_total = sum(수출건수 + 수입건수), 
                     yearly_import = sum(수입건수), 
                     yearly_export = sum(수출건수)) %>% 
              select(continent, yearly_total, yearly_import, yearly_export) %>% 
              unique() %>% 
              filter(국가명 %in% under_10) %>% 
              select(국가명, 'year(기간)', yearly_total)

ggplot(under_10, aes(x = `year(기간)`)) +
  geom_line(aes(y = yearly_total, col = 국가명))

ggplot(under_10, aes(x = `year(기간)`)) +
  geom_line(aes(y = yearly_total, col = 국가명)) +
  ylim(0, 100)

top_country <- trade_df %>% 
                 group_by(국가명) %>% 
                 summarise(Total_trade = sum(수출건수 + 수입건수)) %>% 
                 arrange(desc(Total_trade))

summary(top_country)
top_country <- top_country %>% 
                filter(Total_trade >= 20932) %>% 
                tail(10) %>% 
                select(국가명)

new_under_10 <- trade_df %>% 
                  group_by(국가명, year(기간)) %>% 
                  mutate(yearly_total = sum(수출건수 + 수입건수), 
                         yearly_import = sum(수입건수), 
                         yearly_export = sum(수출건수)) %>% 
                  select(continent, yearly_total, yearly_import, yearly_export) %>% 
                  unique() %>% 
                  filter(국가명 %in% as.vector(top_country$국가명)) %>% 
                  select(국가명, 'year(기간)', yearly_total)

ggplot(new_under_10, aes(x = `year(기간)`)) +
  geom_line(aes(y = yearly_total, col = 국가명))

### 7. 집권한 정부마다의 무역량 변화 추이
trade_df <- trade_df %>% 
              mutate(역대정부 = case_when(
                between(기간, as.Date('1998-02-25'), as.Date('2003-02-24')) ~ '김대중_정부',
                between(기간, as.Date('2003-02-25'), as.Date('2008-02-25')) ~ '노무현_정부',
                between(기간, as.Date('2008-02-26'), as.Date('2013-02-25')) ~ '이명박_정부',
                between(기간, as.Date('2013-02-26'), as.Date('2017-05-10')) ~ '박근혜_정부',
                between(기간, as.Date('2017-05-11'), as.Date('2022-05-09')) ~ '문재인_정부'), 
                    정권성향 = case_when(
                between(기간, as.Date('1998-02-25'), as.Date('2003-02-24')) ~ '진보',
                between(기간, as.Date('2003-02-25'), as.Date('2008-02-25')) ~ '진보',
                between(기간, as.Date('2008-02-26'), as.Date('2013-02-25')) ~ '보수',
                between(기간, as.Date('2013-02-26'), as.Date('2017-05-10')) ~ '보수',
                between(기간, as.Date('2017-05-11'), as.Date('2022-05-09')) ~ '진보'),      
                )

start <- trade_df %>% 
           filter(!duplicated(역대정부)) %>% 
           select(기간, 역대정부, 정권성향) %>%
           rename(start = 기간)

end <- trade_df %>% 
         arrange(desc(기간)) %>% 
         filter(!duplicated(역대정부)) %>% 
         select(기간, 역대정부, 정권성향) %>% 
         rename(end = 기간)

start_end <- right_join(start, end, by = c('역대정부', '정권성향'))
start_end <- start_end[, c('start', 'end', '역대정부', '정권성향')]
start_end

palette(usecol(pal_unikn, n = 5))  
trade_df %>% 
  group_by(역대정부, 기간) %>% 
  summarise(전체수출건수 = sum(수출건수), 
            전체수입건수 = sum(수입건수)) %>% 
  melt(id=c('역대정부', '기간')) %>% 
  ggplot(aes(x = 기간, y = value, fill = variable)) +
  geom_rect(aes(xmin = as.Date('2000-01-01'), xmax = as.Date('2003-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau5, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2003-03-01'), xmax = as.Date('2008-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau4, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2008-03-01'), xmax = as.Date('2013-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau3, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2013-03-01'), xmax = as.Date('2017-05-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau2, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2017-06-01'), xmax = as.Date('2021-07-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau1, alpha = 0.01) +
  geom_line(aes(color = variable), size = 1) +
  geom_text(aes(x = as.Date('2000-01-01'), y = 3000000, label = 'DJ'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2003-03-01'), y = 3000000, label = 'MH'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2008-03-01'), y = 3000000, label = 'MB'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2013-03-01'), y = 3000000, label = 'GH'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2017-06-01'), y = 3000000, label = 'JI'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  ggtitle('대한민국 각 정부별 전체 수출 및 수입 건수의 합계') + 
  theme(plot.title = element_text(hjust=0.5))
  
trade_df %>% 
  filter(국가명 %in% c('미국', '중국', '일본', '러시아연방', 
                       '독일', '영국')) %>% 
  select(국가명, 기간, 무역수지) %>% 
  ggplot(aes(x = 기간, y = 무역수지, fill = 국가명)) +
  geom_rect(aes(xmin = as.Date('2000-01-01'), xmax = as.Date('2003-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau5, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2003-03-01'), xmax = as.Date('2008-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau4, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2008-03-01'), xmax = as.Date('2013-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau3, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2013-03-01'), xmax = as.Date('2017-05-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau2, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2017-06-01'), xmax = as.Date('2021-07-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau1, alpha = 0.01) +
  geom_line(aes(color = 국가명)) + 
  geom_text(aes(x = as.Date('2000-01-01'), y = 7500000, label = 'DJ'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2003-03-01'), y = 7500000, label = 'MH'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2008-03-01'), y = 7500000, label = 'MB'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2013-03-01'), y = 7500000, label = 'GH'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2017-06-01'), y = 7500000, label = 'JI'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  ggtitle('대한민국 각 정부별 주요 국가에 대한 무역수지 추이') + 
  theme(plot.title = element_text(hjust=0.5))

under_mean <- trade_df %>% 
                group_by(국가명) %>% 
                summarise(평균 = mean(무역수지)) %>% 
                arrange(평균) %>% 
                select(국가명) %>% 
                head(5)

top_mean <- trade_df %>% 
              group_by(국가명) %>% 
              summarise(평균 = mean(무역수지)) %>% 
              arrange(desc(평균)) %>% 
              select(국가명) %>% 
              head(5)

top_under <- rbind(top_mean, under_mean)

trade_df %>% 
  filter(국가명 %in% top_mean$국가명) %>% 
  select(국가명, 기간, 무역수지) %>% 
  ggplot(aes(x = 기간, y = 무역수지, fill = 국가명)) +
  geom_rect(aes(xmin = as.Date('2000-01-01'), xmax = as.Date('2003-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau5, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2003-03-01'), xmax = as.Date('2008-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau4, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2008-03-01'), xmax = as.Date('2013-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau3, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2013-03-01'), xmax = as.Date('2017-05-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau2, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2017-06-01'), xmax = as.Date('2021-07-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau1, alpha = 0.01) +
  geom_line(aes(color = 국가명)) + 
  geom_text(aes(x = as.Date('2000-01-01'), y = 7500000, label = 'DJ'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2003-03-01'), y = 7500000, label = 'MH'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2008-03-01'), y = 7500000, label = 'MB'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2013-03-01'), y = 7500000, label = 'GH'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2017-06-01'), y = 7500000, label = 'JI'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  ggtitle('대한민국 각 정부별 주요 국가에 대한 무역수지 상위 국가 추이') + 
  theme(plot.title = element_text(hjust=0.5))

trade_df %>% 
  filter(국가명 %in% under_mean$국가명) %>% 
  select(국가명, 기간, 무역수지) %>% 
  ggplot(aes(x = 기간, y = 무역수지, fill = 국가명)) +
  geom_rect(aes(xmin = as.Date('2000-01-01'), xmax = as.Date('2003-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau5, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2003-03-01'), xmax = as.Date('2008-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau4, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2008-03-01'), xmax = as.Date('2013-02-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau3, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2013-03-01'), xmax = as.Date('2017-05-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau2, alpha = 0.01) +
  geom_rect(aes(xmin = as.Date('2017-06-01'), xmax = as.Date('2021-07-01'), 
                ymin = -Inf, ymax = Inf), fill = pal_unikn$seeblau1, alpha = 0.01) +
  geom_line(aes(color = 국가명)) + 
  geom_text(aes(x = as.Date('2000-01-01'), y = 4000000, label = 'DJ'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2003-03-01'), y = 4000000, label = 'MH'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2008-03-01'), y = 4000000, label = 'MB'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2013-03-01'), y = 4000000, label = 'GH'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  geom_text(aes(x = as.Date('2017-06-01'), y = 4000000, label = 'JI'), 
            size = 10, vjust = 0, hjust = 0, nudge_x = 50, check_overlap = TRUE) +
  ggtitle('대한민국 각 정부별 주요 국가에 대한 무역수지 하위 국가 추이') + 
  theme(plot.title = element_text(hjust=0.5))