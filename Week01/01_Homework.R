## Practice1 - Data Manipulation
## Data on COVID-19 by our World in Data

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

### Loading data
owid <- read.csv('http://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv', stringsAsFactors = F)
dim(owid)
head(names(owid))

### 1. Column selection
owid_selected <- owid %>% select(iso_code, continent, location, date, new_cases, new_deaths, new_cases_per_million,
                                 new_deaths_per_million, reproduction_rate, icu_patients, icu_patients_per_million,
                                 hosp_patients, hosp_patients_per_million, weekly_icu_admissions,
                                 weekly_icu_admissions_per_million, weekly_hosp_admissions,
                                 weekly_hosp_admissions_per_million, new_tests, new_tests_per_thousand, positive_rate,
                                 total_vaccinations, people_vaccinated, people_fully_vaccinated, new_vaccinations,
                                 people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, stringency_index,
                                 population, population_density, median_age, aged_65_older, aged_70_older, gdp_per_capita,
                                 extreme_poverty, cardiovasc_death_rate, diabetes_prevalence, female_smokers,
                                 male_smokers, handwashing_facilities, hospital_beds_per_thousand, life_expectancy,
                                 human_development_index, excess_mortality)

dim(owid_selected)
names(owid_selected)

### 2. Excluding non-country location
owid_countries <- 
  owid_selected %>% 
    filter(!location %in% c('Africa', 'Asia', 'Europe', 'European Union', 
                            'International', 'North America', 'Oceania', 
                            'South America', 'World'))

dim(owid_countries)
length(unique(owid_countries$location))

owid_countries$location %>% 
  unique %>% 
  head(30)

### 3. Changing column names
colnames(owid_countries) <- gsub('^new_', 'daily_', colnames(owid_countries))

owid_countries %>% 
  select(starts_with('daily_')) %>% 
  head()

### 4. Change type of column
owid_countries$date <- as.Date(owid_countries$date)
str(owid_countries)

### 5. Proportion of COVID-19 contracted people
# Make cumulative cases and proportion.
owid_countries <- 
  owid_countries %>% 
    group_by(location) %>% 
    mutate(cumulative_cases = cumsum(replace_na(daily_cases, 0)), 
           prop_contr_people_column = cumulative_cases / population)

owid_countries %>% 
  select(location, daily_cases, cumulative_cases, prop_contr_people_column) %>% 
  head(20)

# Select the latest date result.
owid_countries %>% 
  filter(date == max(owid_countries$date)) %>% 
  arrange(desc(prop_contr_people_column)) %>% 
  select(location, date, cumulative_cases, population, prop_contr_people_column) %>% 
  head(5)

owid_countries %>% 
  filter(date == max(owid_countries$date)) %>% 
  arrange(desc(prop_contr_people_column)) %>% 
  select(location, date, cumulative_cases, population, prop_contr_people_column) %>% 
  filter(prop_contr_people_column == 0)

### 6. 백신 접종 격차에 대해
# Make total_vaccinations_rate, vaccinations_rate_group.
vaccinations_gdp <- 
  owid_countries %>% 
  group_by(location) %>% 
  fill(people_vaccinated, .direction = c('down')) %>% 
  filter(date == max(owid_countries$date)) %>% 
  mutate(people_vaccinated = ifelse(is.na(people_vaccinated), 0, people_vaccinated),
         v_rate = people_vaccinated / population, 
         v_rate_group = cut(v_rate, breaks = c(-Inf, 0.1, 0.5, Inf), 
                            labels = c('under_10%', '10%~50%', 'over_50%')))

# Calculate mean of GDP by vaccinations_rate_group.
vaccinations_gdp %>% 
  group_by(v_rate_group) %>% 
  summarise(GDP_mean = mean(na.omit(gdp_per_capita))) %>% 
  arrange(desc(GDP_mean))

vaccinations_gdp %>% 
  select(location, gdp_per_capita, v_rate, v_rate_group) %>% 
  ggplot(mapping = aes(x = v_rate_group, y = gdp_per_capita)) +
  geom_boxplot(alpha = 0.1, aes(col = v_rate_group, fill = v_rate_group)) +
  geom_jitter(alpha = 0.3, aes(col = v_rate_group)) +
  coord_flip()

v_df <- vaccinations_gdp %>% 
          select(location, gdp_per_capita, v_rate, v_rate_group)

plot(v_rate ~ gdp_per_capita, data = v_df)
abline(lm(v_rate ~ gdp_per_capita, data = v_df), col = 'red')

# Check distribution about vaccine_rate and GDP with graph.
ggplot(vaccinations_gdp, aes(x = location)) +
  geom_col(aes(x = reorder(location, -gdp_per_capita), y = gdp_per_capita, fill = 'redfill'))  +
  geom_line(aes(y = v_rate*100000, group = 1, color = 'blackline')) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 100000)) +
  scale_fill_manual('', labels = 'GDP', values = "#C00000") +
  scale_color_manual('', labels = 'vaccine_rate', values = 'black') +
  theme(axis.text.x = element_blank())

### 7. Continent comparison
# Compare various index
v_gap_stat <- 
  vaccinations_gdp %>% 
    group_by(continent) %>% 
    summarise(population_sum = sum(population), 
              population_density_mean = mean(population_density, na.rm = T), 
              cumulative_cases_mean = mean(cumulative_cases, na.rm = T),
              gdp_mean = mean(gdp_per_capita, na.rm = T), 
              v_rate_mean = mean(v_rate, na.rm = T),
              median_age_mean = mean(median_age, na.rm = T), 
              life_expectancy_mean = mean(life_expectancy, na.rm = T), 
              develop_index_mean = mean(human_development_index, na.rm = T)) %>% 
    arrange(population_sum)

print_plot <- function(variable, title){
  v_gap_stat %>% 
    ggplot(aes(x = reorder(continent, variable), y = variable)) +
    theme_bw() + geom_col() + xlab("continent") +
    ggtitle(title) + 
    theme(plot.title = element_text(hjust=0.5))
}

print_plot(v_gap_stat$population_sum, 'population_sum')
print_plot(v_gap_stat$population_density_mean, 'population_density_mean')
print_plot(v_gap_stat$cumulative_cases_mean, 'cumulative_cases_mean')
print_plot(v_gap_stat$gdp_mean, 'gdp_mean')
print_plot(v_gap_stat$v_rate_mean, 'v_rate_mean')
print_plot(v_gap_stat$median_age_mean, 'median_age_mean')
print_plot(v_gap_stat$life_expectancy_mean, 'life_expectancy_mean')
print_plot(v_gap_stat$develop_index_mean, 'develop_index_mean')
  
### 8. Stringency_index vs. Reproduction_rate
df <- owid_countries %>% 
        filter(reproduction_rate >= 0) %>% 
        select(location, date, stringency_index, reproduction_rate) %>% 
        drop_na()

cor(df$stringency_index, df$reproduction_rate)

plot(reproduction_rate ~ stringency_index, data = df)
abline(lm(reproduction_rate ~ stringency_index, data = df), col = 'red')

df %>% 
  mutate(stringency_index_group = cut(stringency_index, breaks = c(-Inf, seq(10, 90, 10), Inf), 
                                      labels = c('0~10', '10~20', '20~30', '30~40', '40~50', 
                                                 '50~60', '60~70', '70~80', '80~90', '90~100'))) %>%
  group_by(stringency_index_group) %>% 
  summarise(mean(reproduction_rate))