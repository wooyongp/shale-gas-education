library(tidyverse)
library(ipumsr)

# Data for 20250420


ddi <- read_ipums_ddi('data/usa_00032.xml')# ACS 1yr, 2000 sample(5%)
data_2000 <- read_ipums_micro(ddi) %>% mutate(MULTYEAR = YEAR)

ddi <- read_ipums_ddi('data/usa_00033.xml') # ACS 2001, 2002, 2003, 2004, 2005-2009
data_2009 <- read_ipums_micro(ddi)
data_2009 <- data_2009 %>% mutate(MULTYEAR = coalesce(MULTYEAR, YEAR))

ddi <- read_ipums_ddi('data/usa_00028.xml') # ACS 5yr, 2014 sample
data_2014 <- read_ipums_micro(ddi)

data <- bind_rows(data_2000, data_2009, data_2014)

arrow::write_parquet(data, "data/ACS5yr_2000-2014.parquet")


# Update(Tweak: included PUMA units)

ddi <- read_ipums_ddi('data/usa_00038.xml')
data <- read_ipums_micro(ddi)
arrow::write_parquet(data, "data/ACS_2000-2014.parquet")


# shale county to shale puma
data <- arrow::read_parquet("data/ACS5yr_2000-2014.parquet")

df <- data %>% count(YEAR, STATEFIP, COUNTYFIP, PUMA) %>% select(-n)

shale_counties <- read_csv('data/shale_gas.csv')

df <- df %>% mutate(STATEFIP = as_factor(STATEFIP), COUNTYFIP = as_factor(COUNTYFIP), PUMA = as_factor(PUMA)) %>% 
  mutate(STATEFIP = as.numeric(STATEFIP), COUNTYFIP = as.numeric(COUNTYFIP), PUMA = as.numeric(PUMA)) %>% 
  filter(!is.na(COUNTYFIP), !is.na(PUMA))

df <- df %>% filter(STATEFIP %in% shale_counties$state_code & COUNTYFIP %in% shale_counties$county_code)

write_rds(df, 'data/shale_gas_puma.rds')
