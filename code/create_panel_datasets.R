# Load required libraries
library(tidyverse)
library(arrow)
library(ipumsr)
library(Hmisc)

# Source helper functions
source("code/functions.r")

# Load data
data <- read_parquet("data/ACS_2000-2014.parquet")

# Define treatment states and years
treatlist <- c(1, 5, 22, 38, 40, 42, 48, 54)
treat_time <- tibble(statefip = treatlist, 
                    treat_year = c(2008, 2006, 2008, 2007, 2006, 2008, 2005, 2008))

# Filter data according to specifications
data <- data |>
  filter(STATEFIP != 15, STATEFIP != 2, STATEFIP != 72,
         STATEFIP != 8, STATEFIP != 56,
         YEAR <= 2010,
         INCTOT > 0, INCTOT != 9999998, INCTOT != 9999999,
         EDUCD >= 62,
         !is.na(RACE), !is.na(HISPAN))

# Left join with treat_time
data <- data |> left_join(treat_time, by = c("STATEFIP" = "statefip"))

# Apply mutations
data <- data |>
  mutate(
    school_years = educD_to_schlyrs(EDUCD),
    state = as.character(ipumsr::as_factor(STATEFIP)),
    oil_and_gas_industry = as.integer(str_detect(INDNAICS, "^21")),
    treat_year = coalesce(treat_year, 3000),
    W = as.integer(YEAR >= treat_year),
    id = paste0(SERIAL, SAMPLE, collapse = '-'),
    log_INCTOT = log(INCTOT),
    college = as.integer(EDUCD >= 62),
    race_simplified = race_simplified(RACE, HISPAN)
  )

# Create income quartiles for INCTOT
inc_quartile <- data |>
  filter(INCTOT > 0, INCTOT != 9999998, INCTOT != 9999999) |>
  dplyr::group_by(YEAR, STATEFIP) |>
  dplyr::summarize(
    INC1Q = wtd.quantile(INCTOT, weights = PERWT, probs = 0.25, na.rm = TRUE),
    INC2Q = wtd.quantile(INCTOT, weights = PERWT, probs = 0.5, na.rm = TRUE),
    INC3Q = wtd.quantile(INCTOT, weights = PERWT, probs = 0.75, na.rm = TRUE),
    .groups = 'drop'
  )

data <- data |> left_join(inc_quartile, by = c("YEAR", "STATEFIP"))

data <- data |>
  mutate(incomeQ = case_when(
    INCTOT < INC1Q ~ 1,
    INCTOT < INC2Q ~ 2,
    INCTOT < INC3Q ~ 3,
    INCTOT >= INC3Q ~ 4
  ))

# Create income quartiles for FTOTINC
ftotinc_quartile <- data |>
  filter(FTOTINC > 0, FTOTINC != 9999998, FTOTINC != 9999999, RELATE == 1) |>
  group_by(YEAR, STATEFIP) |>
  dplyr::summarize(
    FINC1Q = wtd.quantile(FTOTINC, weights = HHWT, probs = 0.25, na.rm = TRUE),
    FINC2Q = wtd.quantile(FTOTINC, weights = HHWT, probs = 0.5, na.rm = TRUE),
    FINC3Q = wtd.quantile(FTOTINC, weights = HHWT, probs = 0.75, na.rm = TRUE),
    .groups = 'drop'
  )

data <- data |> left_join(ftotinc_quartile, by = c("YEAR", "STATEFIP"))

data <- data |>
  mutate(fincomeQ = case_when(
    FTOTINC < FINC1Q ~ 1,
    FTOTINC < FINC2Q ~ 2,
    FTOTINC < FINC3Q ~ 3,
    FTOTINC >= FINC3Q ~ 4
  ))

# Create the eight panel datasets

# Dataset 1: Household income by state and year
panel1 <- data |>
  filter(FTOTINC != 0, FTOTINC != 9999998, FTOTINC != 9999999, RELATE == 1) |>
  dplyr::group_by(YEAR, STATEFIP) |>
  dplyr::summarize(
    avg_income = weighted.mean(FTOTINC, w = HHWT, na.rm = TRUE),
    log_FTOTINC = weighted.mean(log(FTOTINC), w = HHWT, na.rm = TRUE),
    total_pop = sum(HHWT, na.rm = TRUE),
    treat_year = mean(treat_year, na.rm = TRUE),
    .groups = 'drop'
  )

# Dataset 2: Household income by state, year, and income quartile
panel2 <- data |>
  filter(FTOTINC != 0, FTOTINC != 9999998, FTOTINC != 9999999, RELATE == 1) |>
  dplyr::group_by(YEAR, STATEFIP, fincomeQ) |>
  dplyr::summarize(
    avg_income = weighted.mean(FTOTINC, w = HHWT, na.rm = TRUE),
    log_FTOTINC = weighted.mean(log(FTOTINC), w = HHWT, na.rm = TRUE),
    total_pop = sum(HHWT, na.rm = TRUE),
    treat_year = mean(treat_year, na.rm = TRUE),
    .groups = 'drop'
  )

# Dataset 3: College enrollment for young adults (18-24) by state and year
panel3 <- data |>
  filter(AGE >= 18, AGE < 25) |>
  dplyr::group_by(YEAR, STATEFIP) |>
  dplyr::summarize(
    avg_college = weighted.mean(college, w = PERWT, na.rm = TRUE),
    total_pop = sum(PERWT, na.rm = TRUE),
    treat_year = mean(treat_year, na.rm = TRUE),
    .groups = 'drop'
  )

# Dataset 4: College enrollment for young adults (18-24) by state, year, and income quartile
panel4 <- data |>
  filter(AGE >= 18, AGE < 25) |>
  group_by(YEAR, STATEFIP, fincomeQ) |>
  dplyr::summarize(
    avg_college = weighted.mean(college, w = PERWT, na.rm = TRUE),
    total_pop = sum(PERWT, na.rm = TRUE),
    treat_year = mean(treat_year, na.rm = TRUE),
    .groups = 'drop'
  )

# Dataset 5: High school enrollment (16-18) by state and year
panel5 <- data |>
  filter(AGE >= 16, AGE <= 18, SCHOOL != 0) |>
  dplyr::group_by(YEAR, STATEFIP) |>
  dplyr::summarize(
    enrollment_rate = weighted.mean(as.integer(SCHOOL == 2), w = PERWT, na.rm = TRUE),
    total_pop = sum(PERWT, na.rm = TRUE),
    treat_year = mean(treat_year, na.rm = TRUE),
    .groups = 'drop'
  )

# Dataset 6: High school enrollment (16-18) by state, year, and income quartile
panel6 <- data |>
  filter(AGE >= 16, AGE <= 18, SCHOOL != 0) |>
  dplyr::group_by(YEAR, STATEFIP, fincomeQ) |>
  dplyr::summarize(
    enrollment_rate = weighted.mean(as.integer(SCHOOL == 2), w = PERWT, na.rm = TRUE),
    total_pop = sum(PERWT, na.rm = TRUE),
    treat_year = mean(treat_year, na.rm = TRUE),
    .groups = 'drop'
  )

# Dataset 7: Elementary school enrollment (5-10) by state and year
panel7 <- data |>
  filter(AGE >= 5, AGE <= 10, SCHOOL != 0) |>
  dplyr::group_by(YEAR, STATEFIP) |>
  dplyr::summarize(
    enrollment_rate = weighted.mean(as.integer(SCHOOL == 2), w = PERWT, na.rm = TRUE),
    total_pop = sum(PERWT, na.rm = TRUE),
    treat_year = mean(treat_year, na.rm = TRUE),
    .groups = 'drop'
  )

# Dataset 8: Elementary school enrollment (5-10) by state, year, and income quartile
panel8 <- data |>
  filter(AGE >= 5, AGE <= 10, SCHOOL != 0) |>
  dplyr::group_by(YEAR, STATEFIP, fincomeQ) |>
  dplyr::summarize(
    enrollment_rate = weighted.mean(as.integer(SCHOOL == 2), w = PERWT, na.rm = TRUE),
    total_pop = sum(PERWT, na.rm = TRUE),
    treat_year = mean(treat_year, na.rm = TRUE),
    .groups = 'drop'
  )

# Save datasets as .rds files
saveRDS(panel1, "data/panel1_household_income.rds")
saveRDS(panel2, "data/panel2_household_income_by_quartile.rds")
saveRDS(panel3, "data/panel3_college_enrollment.rds")
saveRDS(panel4, "data/panel4_college_enrollment_by_quartile.rds")
saveRDS(panel5, "data/panel5_high_school_enrollment.rds")
saveRDS(panel6, "data/panel6_high_school_enrollment_by_quartile.rds")
saveRDS(panel7, "data/panel7_elementary_enrollment.rds")
saveRDS(panel8, "data/panel8_elementary_enrollment_by_quartile.rds")

