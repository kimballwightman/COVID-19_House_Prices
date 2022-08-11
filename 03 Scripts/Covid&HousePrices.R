#import the covid-19 mortality rate data
library(tidyverse)
getwd()
setwd('/Users/kimballwightman/Desktop/Projects/Covid & House Prices Analysis/02 Data/Original Data/')
df_covid <- read_csv('us-counties.csv')
#only work with covid deaths in 2020
df_covid <- df_covid %>%
  separate(date, c('Year','Month','Day'), sep='-')
df_covid <- transform(df_covid,Year = as.numeric(Year))
str(df_covid)
df_covid_2020 <- subset(df_covid, Year == 2020)
#aggregate observations for deaths by county
df_covid_sum_deaths <- setNames(aggregate(df_covid_2020$deaths, by=list(df_covid_2020$county, 
                                                         df_covid_2020$state, 
                                                         df_covid_2020$fips, 
                                                         df_covid_2020$Year), 
                                           FUN=sum),
                                 c('County','State','Fips','Year','Deaths'))
df_covid_sum_deaths <- rename(df_covid_sum_deaths, 'fips' = 'Fips')
path <- '/Users/kimballwightman/Desktop/Projects/Covid & House Prices Analysis/02 Data/Prepared Data/'
write_csv(df_covid_sum_deaths, paste(path, 'covid_deaths_ready.csv', sep=''))


#import population data
library(readxl)
df_pop = read_xlsx('2020pop.xlsx')
str(df_pop)
#rename ID to fips
df_pop <- rename(df_pop, 'fips' = 'ID')
write_csv(df_pop, paste(path, 'population_ready.csv', sep=''))


#import housing price index data
df_hp = read_xlsx('HPI_AT_3zip.xlsx')
#keep years 2020 and 2021 for change in house prices
df_hp_sub <- subset(df_hp, Year == 2020 | Year == 2021)
df_hp_sub <- select(df_hp_sub, -c('Index Type'))
df_hp_mean_score <- setNames(aggregate(df_hp_sub$`Index (NSA)`, by=list(df_hp_sub$`Three-Digit ZIP Code`,
                                                                        df_hp_sub$Year), 
                                           FUN=mean),
                                 c('Zip_Three','Year','Mean Index Score'))
#reshape data to get price score columns for 2020 and 2021, create a score change variable
df_hp_l_to_w <- df_hp_mean_score %>% spread('Year','Mean Index Score')
df_hp_l_to_w$'index_change' <- df_hp_l_to_w$`2021` - df_hp_l_to_w$`2020`
#reshape back to long
df_hp_w_to_l <- df_hp_l_to_w %>% gather('2020','2021', key='Year', value='Mean Index Score')
df_hp_ready <- subset(df_hp_w_to_l, Year == 2021)
df_hp_ready <- select(df_hp_ready, -c('Year','Mean Index Score'))
write_csv(df_hp_ready, paste(path, 'hp_ready.csv', sep=''))


#import crosswalk data
df_xw <- read_xlsx('COUNTY_ZIP_032020.xlsx')
#get first three digits of zip code as numeric data type to merge with house price data
str(df_xw)
df_xw$'Zip_Three' = substr(df_xw$'ZIP',1,3)
df_xw <- transform(df_xw, Zip_Three=as.numeric(Zip_Three))
str(df_xw)
df_xw_ready <- select(df_xw, -c('ZIP'))
df_xw_ready <- rename(df_xw_ready, 'fips' = 'COUNTY')
#drop duplicates
df_xw_ready <- distinct(df_xw_ready)
write_csv(df_xw_ready, paste(path, 'xw_ready.csv', sep=''))


#merging covid and population data
df_merge_1 <- merge(df_covid_sum_deaths, df_pop)
write_csv(df_merge_1, paste(path, 'merge_1.csv', sep=''))

#merging house prices and crosswalk data
df_merge_2 <- merge(df_hp_ready, df_xw_ready)
write_csv(df_merge_2, paste(path, 'merge_2.csv', sep=''))

#merge the merges
df_merge_3 <- merge(df_merge_1, df_merge_2)
df_final <- select(df_merge_3, -c('County Full','badid'))
#create a state variable using the fips code's first two digits
df_fips <- transform(df_final, fips=as.character(fips))
str(df_fips)
df_fips$'state_fips' = substr(df_fips$'fips',1,2)
df_final <- transform(df_fips, state_fips=as.numeric(state_fips))
df_final <- select(df_final, -c('Year'))
df_final$'deaths_per_capita' <- (df_final$Deaths/df_final$PopTotal)*100000
write_csv(df_final, paste(path, 'merge_final.csv', sep=''))


#Regression Analysis
model_1 <- estimatr::lm_robust(index_change~deaths_per_capita, data=df_final, se_type='stata')
summary(model_1)

#controlling for State
df_state <- transform(df_final, state_fips=as.character(state_fips))
#state_fips has 51 distinct values, D.C. is included.
model_2 <- estimatr::lm_robust(index_change~deaths_per_capita + state_fips, data=df_state, se_type='stata')
summary(model_2)


#Summary Statistics
library(psych)
describe(select(df_final, c('Deaths','PopTotal','index_change','deaths_per_capita')))


#create a histogram of index_change
hist(df_final$index_change, breaks=40)



