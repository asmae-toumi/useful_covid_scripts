## definitions
# Confirmed cases include presumptive positive cases.
# Recovered cases are estimates based on local media reports, and may be substantially lower than the true number.
# Active cases = total confirmed - total recovered - total deaths.
# New Cases are Confirmed cases reported yesterday

## source for confirmed covid-19 data
# https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv

# 2019 population from https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.html 

options(scipen = 999)

pop <- read_csv("data/pop.csv")

confirmed_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

confirmed_cases <- confirmed_cases %>%
  mutate(FIPS = as.character(FIPS)) %>%
  rename(fips = FIPS)

df_5 <- confirmed_cases %>%
  filter(nchar(fips) == 5)

df_4 <- confirmed_cases %>%
  filter(nchar(fips) == 4) %>%
  mutate(fips = str_c("0", fips))

confirmed_cases <- bind_rows(df_4, df_5)

library(zoo)
library(lubridate)

confirmed_cases <-
  confirmed_cases %>%
  pivot_longer(cols=contains("/"),names_to="date",values_to="cases")

"%ni%" <- Negate("%in%")

confirmed_cases <- confirmed_cases %>%
  mutate(date = mdy(date)) %>%
  rename(state = Province_State) %>%
  filter(state %ni% c("Virgin Islands","Northern Mariana Islands","Guam","Grand Princess","Diamond Princess","American Samoa", "Puerto Rico")) %>%
  group_by(fips,date) %>%
  summarize(cases = sum(cases)) %>%
  ungroup() %>%
  group_by(fips) %>%
  arrange(fips,date) %>%
  left_join(pop, by = "fips") %>%
  mutate(new_cases = rollapply(cases,2,diff,align='right',fill=NA)) %>%
  mutate(cases_pop = cases/popestimate2019,
         new_cases_pop = new_cases/popestimate2019,
         cases_cap = cases_pop * 100000,
         new_cases_cap = new_cases_pop * 100000) %>%
  filter(fips < 8000) # removing fips codes over 8000 because they're not real

# if you want moving average and normalized moving average
#   mutate(moving_avg = rollapply(new_cases,7,mean,align='right',fill=NA)) %>%
#   mutate(moving_avg = moving_avg/population) %>%
#   mutate(normalized = scale(moving_avg))
