'''
2021-2 - Data Mining Practicum
Prof. Hyebong Choi

Ch 2. Data Visualization Basics
'''

install.packages('dplyr')
install.packages('tidyr')
install.packages('ggplot2')
library(dplyr)
library(tidyr)
library(ggplot2)

## tidyr's Review - gather, spread, separate, unite
wide_df <- data.frame(col = c('X', 'Y'), A = c(1, 4), B = c(2, 5), C = c(3, 6))
gather(wide_df, my_key, my_val, -col)

long_df <- gather(wide_df, my_key, my_val, -col)
spread(long_df, my_key, my_val)

treatments <- data.frame(patient = rep(c('X', 'Y'), 3), 
                         treatment = rep(c('A', 'B'), each = 3), 
                         year_mo = rep(c('2010-10', '2012-08', '2014-12'), each = 2), 
                         response = c(1, 4, 2, 5, 3, 6))
separate(treatments, year_mo, c('year', 'month'))

treatments2 <- separate(treatments, year_mo, c('year', 'month'))
unite(treatments2, year_mo, year, month)

## Exercise - 월별 min, max, mean.temperature의 평균을 tidyr과 dplyr로 구해보기
load(url('http://github.com/hbchoi/SampleData/raw/master/weather.RData'))
weather2 %>% 
  gather(date, value, 4:34) %>% 
  spread(measure, value) %>% 
  group_by(month) %>% 
  summarise(T_min_mean = mean(as.double(Min.TemperatureF), na.rm = T), 
            T_max_mean = mean(as.double(Max.TemperatureF), na.rm = T), 
            T_mean_mean = mean(as.double(Mean.TemperatureF), na.rm = T))

## Example - First trial of ggplot
ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point()

## ggplot2 layers - Aesthetics
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species) +
  stat_smooth(method = 'lm', se = F, col = 'red') + 
  scale_y_continuous('Sepal Width', limits = c(2, 5), expand = c(0, 0)) +
  scale_x_continuous('Sepal Length', limits = c(4, 8), expand = c(0, 0)) +
  coord_equal() +
  theme(panel.background = element_blank(), 
        plot.background = element_blank(), 
        legend.background = element_blank(), 
        legend.key = element_blank(), 
        strip.background = element_blank(), 
        axis.text = element_text(colour = 'black'), 
        axis.ticks = element_line(colour = 'black'), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = 'black'), 
        strip.text = element_blank(), 
        panel.spacing = unit(1, 'lines'))

## Exercise - iris dataset
iris_df <- iris

iris.tidy <- iris_df %>% 
                gather(name, Value, -Species) %>% 
                separate(name, c('Part', 'Measure'))

ggplot(iris.tidy, aes(x = Species, y = Value)) + 
  geom_jitter(aes(color=Part)) + 
  facet_grid(. ~ Measure) 

iris.tidy$no <- rep(1:150, 4)

iris.wide <- iris.tidy %>% 
                spread(Measure, Value) %>% 
                select(Species, Part, Length, Width)

ggplot(iris.wide, aes(x = Length, y = Width)) +
  geom_jitter(aes(color=Part)) +
  facet_grid(. ~ Species)