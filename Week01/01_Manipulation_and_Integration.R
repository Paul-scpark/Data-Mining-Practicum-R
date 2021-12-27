'''
2021-2 - Data Mining Practicum
Prof. Hyebong Choi

Ch 1. Data Manipulation and Integration
'''

install.packages('dplyr')
install.packages('tibble')
library(dplyr)
library(tibble)

load(url('http://github.com/hbchoi/SampleData/raw/master/hflights.RData'))
load(url('http://github.com/hbchoi/SampleData/raw/master/join_practice.RData'))

## Pipe operator
x <- 1:10
x %>% sum() # sum(x) = 55

x %>% 
  range() %>% 
  diff() %>% 
  abs()

abs(diff(range(x))) # 9

## select
colnames(hf_data)
select(hf_data, ActualElapsedTime, AirTime, ArrDelay, DepDelay)
select(hf_data, Origin:Cancelled)
select(hf_data, Year:DayOfWeek, ArrDelay:Diverted)
select(hf_data, ends_with('Delay')) # starts_with, ends_with, contains, matches, num_range, one_of
select(hf_data, UniqueCarrier, ends_with('Num'), starts_with('Cancell'))
select(hf_data, ends_with('Time'), ends_with('Delay'))

## mutate
mutate(hf_data, ActualGroundTime = ActualElapsedTime - AirTime) # new columns (ActualGroundTime)
mutate(hf_data, GroundTime = TaxiIn + TaxiOut) # new columns (GroundTime)
mutate(hf_data, AverageSpeed = Distance / AirTime * 60) # new columns (AverageSpeed)

## filter
filter(hf_data, UniqueCarrier %in% c('JetBlue', 'Southwest', 'Delta'))
filter(hf_data, Distance >= 3000)

## arrange
dtc <- filter(hf_data, Cancelled == 1, !is.na(DepDelay))
arrange(dtc, DepDelay)
arrange(dtc, CancellationCode)
arrange(dtc, UniqueCarrier, DepDelay)
arrange(hf_data, UniqueCarrier, desc(DepDelay))

## summarise
summarise(hf_data, min_dist = min(Distance), max_dist = max(Distance))
summarise(filter(hf_data, Diverted == 1), max_div = max(Distance))

temp1 <- filter(hf_data, !is.na(ArrDelay))
summarise(temp1, earliest = min(ArrDelay), average = mean(ArrDelay), latest = max(ArrDelay), sd = sd(ArrDelay))
temp2 <- filter(hf_data, !is.na(TaxiIn), !is.na(TaxiOut))
summarise(temp2, max_taxi_diff = max(abs(TaxiIn - TaxiOut)))

summarise(hf_data, 
          n_obs = n(), 
          n_carrier = n_distinct(UniqueCarrier), 
          n_dest = n_distinct(Dest))

summarise(filter(hf_data, UniqueCarrier == 'American'), 
          n_flights = n(), 
          n_canc = sum(Cancelled), 
          avg_delay = mean(ArrDelay, na.rm = T))

## Pipe operation
hf_data %>% 
  mutate(diff = TaxiOut - TaxiIn) %>% 
  filter(!is.na(diff)) %>% 
  summarise(avg = mean(diff))

## group_by
hf_data %>% 
  group_by(UniqueCarrier) %>% 
  summarise(avgDep = mean(DepDelay, na.rm = T), 
            avgArr = mean(ArrDelay, na.rm = T)) %>% 
  arrange(avgArr, avgDep)

### Exercise
## Q1. UniqueCarrier 별로, 비행편수를 계산해서 해당 기간동안 가장 많은 비행 편수가 큰 순서대로 정렬.
hf_data %>% 
  group_by(UniqueCarrier) %>% 
  summarise(flight_frequency = n()) %>% 
  arrange(flight_frequency)

## Q2. Dest 별로, ActualElapsedTime의 평균을 계산하여 가장 오래 걸리거나, 적게 걸리는 목적지는 어디인지 확인.
hf_data %>% 
  group_by(Dest) %>% 
  summarise(mean = mean(ActualElapsedTime, na.rm = T)) %>% 
  arrange(mean)

## Q3. UniqueCarrier 별로, cancel 된 항공편의 비율을 계산해보고, 비율이 높은 항공사부터 정렬.
hf_data %>% 
  group_by(UniqueCarrier) %>% 
  summarise(cancel_mean = mean(Cancelled)) %>% 
  arrange(cancel_mean)

## left_join, right_join, inner_join, full_join
left_join(names, plays, by = 'name')
right_join(names, plays, by = 'name')
inner_join(names, plays, by = 'name')
full_join(names, plays, by = 'name')

### Exercise
## Q1. bands와 artists 테이블을 left join과 right join 해보고, 결과 비교.
left_join(bands, artists, by = 'first')
right_join(bands, artists, by = 'last')

## Q2. artists, bands, songs, albums 테이블을 full join으로 합치기.
artists %>% 
  full_join(bands, by = c('first', 'last')) %>% 
  full_join(songs, by = c('first', 'last')) %>% 
  full_join(albums, by = c('album', 'band'))

## semi_join, anti_join
semi_join(names, plays, by = 'name')
anti_join(names, plays, by = 'name')

artists %>% 
  semi_join(songs, by = c('first', 'last'))

artists %>% 
  anti_join(bands, by = c('first', 'last'))

## Ex1. Count the number of albums made by a band
albums %>% 
  semi_join(bands, by = 'band') %>% 
  nrow()

## Ex2. How many rows in songs match a label in labels
songs %>% 
  semi_join(labels, by = 'album') %>% 
  nrow()

## Set operations - union, intersect, setdiff
more_names <- as.data.frame(cbind(c('Keith', 'Mick', 'John', 'John'), c('Stones', 'Stones', 'Beatles', 'Beatles')))
colnames(more_names) <- c('name', 'band')

union(names, more_names)
intersect(names, more_names)
setdiff(names, more_names)
setequal(stones1, stones2)

## Exercise
aerosmith %>% 
  union(greatest_hits) %>% 
  nrow()

aerosmith %>% intersect(greatest_hits)

## Missing key values
namesNA <- names
namesNA[2, 1] <- NA
namesNA %>% 
  filter(!is.na(name)) %>% 
  left_join(plays, by = 'name')

## Specifying key columns 
left_join(names, plays, by = 'name')
left_join(names, plays)

## Mismatched key names
members <- names
colnames(members)[1] <- 'member'
left_join(members, plays, by = c('member' = 'name'))

## Conflicting names
playwith <- names
colnames(playwith)[2] <- 'plays'
left_join(playwith, plays, by = 'name', suffix = c('1', '2'))

## Exercise
stage_songs %>% 
  rownames_to_column('song') %>% 
  left_join(stage_writers, by = 'song')

movie_years %>% 
  left_join(movie_studios, by = 'movie') %>% 
  rename(artist = name.x, studio = name.y)

elvis_movies %>% 
  left_join(elvis_songs, by = c('name' = 'movie')) %>% 
  rename(movie = name, song = name.y)

movie_directors
movie_years

movie_years %>% 
  left_join(movie_directors, by = c('movie' = 'name')) %>% 
  select(year, movie, artist = name, director, studio)