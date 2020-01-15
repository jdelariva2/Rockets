library(tidyverse)
library(data.table)
library(sf)
library(mapview)
library(dplyr)
library(tidyr)
library(lubridate)
setwd('C:/Users/jorge/OneDrive/Documents')
theme_set(theme_light())

url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/launches.csv'
launches <- read.csv(url)
head(launches)
unique(launches$agency_type)


#Launches by year
launches %>%
  count(launch_year, sort=TRUE) %>%
  ggplot(aes(x=launch_year,y=n))+ geom_point()

#Launches by year by agency
launches %>%
  count(launch_year,agency_type, sort=TRUE) %>%
  ggplot(aes(x=launch_year,y=n,colour = factor(agency_type)))+ geom_point(size=4)

library(countrycode)

launches_processed <- launches %>%
  filter(launch_date <= Sys.Date()) %>%
  mutate(state_code_cleaned = fct_collapse(
    state_code,
    "RU" = c("SU", "RU"),
    "FR" = "F",
    "JP" = "J",
    "IT" = "I"
  )) %>%
  mutate(state_name = countrycode(state_code_cleaned, "iso2c", "country.name"),
         state_name = fct_lump(state_name, 6)) %>%
  replace_na(list(state_name = "Other"))

launches_processed %>%
  count(launch_year, state_name) %>%
  mutate(state_name = fct_reorder(state_name, -n, sum)) %>%
  ggplot(aes(launch_year, n, color = state_name)) +
  geom_line() +
  labs(x = "Time",
       y = "Launches per year",
       color = "Responsible state",
       title = "Launches per year per country",
       subtitle = "Combines Soviet Union (pre-1990) with Russia")