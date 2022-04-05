
# STRINGR data cleaning
# STRINGI  - https://cran.r-project.org/web/packages/stringi/stringi.pdf 

library(tidyverse)
library(stringr)
library(janitor)

cars = read_csv2('kijiji_cars.csv') %>% clean_names()


glimpse(cars)
skimr::skim(cars)

summary(cars)



