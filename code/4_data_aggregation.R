# libraries ----
library(DBI)
library(duckdb)
library(data.table)
library(dtplyr)
if (!("dplyr" %in% rownames(utils::installed.packages()))) {
  stop("Package 'dplyr' is required but not installed. Please install it with install.packages('dplyr').", call. = FALSE)
}

# settings ----
reattach_quartile <- TRUE # TRUE if you want to reattach the quartiles to the data
rewrite_database <- TRUE # TRUE if you want to rewrite the database

# data ----
con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = "data/ACS.duckdb")

data <- dbReadTable(con, "ACS_00_14_preprocessed")

if(reattach_quartile){
  ftotinc_quartile_state <- dbReadTable(con, "FTOTINC_QUARTILES_STATE")
  ftotinc_quartile_national <- dbReadTable(con, "FTOTINC_QUARTILES_NATIONAL")
}

# functions ----
source("code/functions.r")


## into data.table
data <- data |> as.data.table()
if(reattach_quartile){
  ftotinc_quartile_state <- ftotinc_quartile_state |> as.data.table()
  ftotinc_quartile_national <- ftotinc_quartile_national |> as.data.table()
}

## merge data ----
if(reattach_quartile){
    data <- data |> merge.data.table(ftotinc_quartile_state, by = c("YEAR", "STATEFIP"), all.x = TRUE)
    data <- data |> merge.data.table(ftotinc_quartile_national, by = c("YEAR"), all.x = TRUE)

    data[, `:=`(
        fincomeQ_state = dplyr::case_when(
        FTOTINC < FINC1Q_STATE ~ 1,
        FTOTINC < FINC2Q_STATE ~ 2,
        FTOTINC < FINC3Q_STATE ~ 3,
        FTOTINC >= FINC3Q_STATE ~ 4),
        fincomeQ_national = dplyr::case_when(
        FTOTINC < FINC1Q_NATIONAL ~ 1,
        FTOTINC < FINC2Q_NATIONAL ~ 2,
        FTOTINC < FINC3Q_NATIONAL ~ 3,
        FTOTINC >= FINC3Q_NATIONAL ~ 4))]

    data[, (c("FINC1Q_STATE", "FINC2Q_STATE", "FINC3Q_STATE", "FINC1Q_NATIONAL", "FINC2Q_NATIONAL", "FINC3Q_NATIONAL")) := NULL]

}

### write to database
if(rewrite_database){
  dbExecute(con, "DROP TABLE IF EXISTS ACS_00_14_preprocessed")
  dbWriteTable(con, "ACS_00_14_preprocessed", data)
}

## PUMA level 2000 covariates ----

covariates <- data[YEAR==2000, .(
    AGE, STATEFIP, race_simplified, college, EMPSTAT, INDNAICS, female, MARST, INCTOT, school_years, oil_and_gas_industry,
    PERWT, HHWT)]

covariates <- covariates[,
    .(
      white_ratio_2000 = weighted.mean(race_simplified == "White", w = PERWT, na.rm = TRUE),
      black_ratio_2000 = weighted.mean(race_simplified == "Black", w = PERWT, na.rm = TRUE),
      asian_ratio_2000 = weighted.mean(race_simplified == "Asian", w = PERWT, na.rm = TRUE),
      hispanic_ratio_2000 = weighted.mean(race_simplified == "Hispanic", w = PERWT, na.rm = TRUE),
      avg_age_2000 = weighted.mean(AGE, w = PERWT, na.rm = TRUE),
      college_rate_2000 = weighted.mean(college, w = PERWT, na.rm = TRUE), # At least some college education
      employment_rate_2000 = sum(as.integer(EMPSTAT == 1)*PERWT, na.rm = TRUE) / sum(as.integer(EMPSTAT %in% 1:2)*PERWT, na.rm = TRUE), # Employed or Unemployed
      industry_concentration_2000 = hhi(INDNAICS, w = PERWT),
      ratio_concentration_2000 = hhi(race_simplified, w = PERWT),
      female_ratio_2000 = weighted.mean(female, w = PERWT, na.rm = TRUE),
      married_ratio_2000 = weighted.mean(MARST == 1, w = PERWT, na.rm = TRUE),
      avg_income_2000 = weighted.mean(INCTOT, w = PERWT, na.rm = TRUE),
      avg_school_years_2000 = weighted.mean(school_years, w = PERWT, na.rm = TRUE),
      total_pop_2000 = sum(PERWT, na.rm = TRUE),
      oil_and_gas_industry_share_2000 = weighted.mean(oil_and_gas_industry, w = PERWT, na.rm = TRUE)
    ), 
    by = .(STATEFIP)]

### write to database
if(rewrite_database){
  dbExecute(con, "DROP TABLE IF EXISTS PUMA_COVARIATES_2000")
  dbWriteTable(con, "PUMA_COVARIATES_2000", covariates)
}

print("2000 state-level covariates completed")



## PUMA+FTOTINC_QUARTILE level 2000 covariates ----

covariates <- data[YEAR==2000, .(
    AGE, STATEFIP, race_simplified, college, EMPSTAT, INDNAICS, female, MARST, INCTOT, school_years, oil_and_gas_industry, fincomeQ_national, 
    PERWT, HHWT)]

covariates <- covariates[,
    .(
      white_ratio_2000 = weighted.mean(race_simplified == "White", w = PERWT, na.rm = TRUE),
      black_ratio_2000 = weighted.mean(race_simplified == "Black", w = PERWT, na.rm = TRUE),
      asian_ratio_2000 = weighted.mean(race_simplified == "Asian", w = PERWT, na.rm = TRUE),
      hispanic_ratio_2000 = weighted.mean(race_simplified == "Hispanic", w = PERWT, na.rm = TRUE),
      avg_age_2000 = weighted.mean(AGE, w = PERWT, na.rm = TRUE),
      college_rate_2000 = weighted.mean(college, w = PERWT, na.rm = TRUE), # At least some college education
      employment_rate_2000 = sum(as.integer(EMPSTAT == 1)*PERWT, na.rm = TRUE) / sum(as.integer(EMPSTAT %in% 1:2)*PERWT, na.rm = TRUE), # Employed or Unemployed
      industry_concentration_2000 = hhi(INDNAICS, w = PERWT),
      ratio_concentration_2000 = hhi(race_simplified, w = PERWT),
      female_ratio_2000 = weighted.mean(female, w = PERWT, na.rm = TRUE),
      married_ratio_2000 = weighted.mean(MARST == 1, w = PERWT, na.rm = TRUE),
      avg_income_2000 = weighted.mean(INCTOT, w = PERWT, na.rm = TRUE),
      avg_school_years_2000 = weighted.mean(school_years, w = PERWT, na.rm = TRUE),
      total_pop_2000 = sum(PERWT, na.rm = TRUE),
      oil_and_gas_industry_share_2000 = weighted.mean(oil_and_gas_industry, w = PERWT, na.rm = TRUE)
    ), 
    by = .(STATEFIP, fincomeQ_national)]

### write to database
if(rewrite_database){
  dbExecute(con, "DROP TABLE IF EXISTS PUMA_COVARIATES_2000_FTOTINC_QUARTILE")
  dbWriteTable(con, "PUMA_COVARIATES_2000_FTOTINC_QUARTILE", covariates)
}

print("2000 state-level covariates with FTOTINC_QUARTILE completed")


## Data Aggregation for Panel Data(PUMA level) ----

panel <- data[YEAR >=2005 & !is.na(FTOTINC), ]

panel <- panel[, .(
      white_ratio = weighted.mean(race_simplified == "White", w = PERWT, na.rm = TRUE),
      black_ratio = weighted.mean(race_simplified == "Black", w = PERWT, na.rm = TRUE),
      asian_ratio = weighted.mean(race_simplified == "Asian", w = PERWT, na.rm = TRUE),
      hispanic_ratio = weighted.mean(race_simplified == "Hispanic", w = PERWT, na.rm = TRUE),
      avg_age = weighted.mean(AGE, w = PERWT, na.rm = TRUE),
      college_rate = weighted.mean(college, w = PERWT, na.rm = TRUE), # At least some college education
      employment_rate = sum(as.integer(EMPSTAT == 1)*PERWT, na.rm = TRUE) / sum(as.integer(EMPSTAT %in% 1:2)*PERWT, na.rm = TRUE), # Employed or Unemployed
      industry_concentration = hhi(INDNAICS, w = PERWT),
      ratio_concentration = hhi(race_simplified, w = PERWT),
      female_ratio = weighted.mean(female, w = PERWT, na.rm = TRUE),
      married_ratio = weighted.mean(MARST == 1, w = PERWT, na.rm = TRUE),
      avg_income = weighted.mean(INCTOT, w = PERWT, na.rm = TRUE),
      avg_FTOTINC = weighted.mean(FTOTINC, w = HHWT, na.rm = TRUE),
      avg_school_years = weighted.mean(school_years, w = PERWT, na.rm = TRUE),
      total_pop = sum(PERWT, na.rm = TRUE),
      avg_nchild = weighted.mean(NCHILD, w = PERWT, na.rm = TRUE),
      oil_and_gas_industry_share = weighted.mean(oil_and_gas_industry, w = PERWT, na.rm = TRUE)
    ), by = .(YEAR, treat_year_CSDID, treated, STATEFIP, PUMA, W)]

## check for imbalanced in the data

unbal <- panel[, .(n=dplyr::n_distinct(YEAR)), by = .(PUMA)]
unbal <- unbal[n!=max(n)]

print(unbal) # 2201801(Louisiana), 2201802(Louisiana), 2202300(Louisiana), 22777777(Louisiana)

panel <- panel[!PUMA %in% unbal$PUMA]

### write to database
if(rewrite_database){
  dbExecute(con, "DROP TABLE IF EXISTS PUMA_PANEL")
  dbWriteTable(con, "PUMA_PANEL", panel)
}

print("Data Aggregation for Panel Data(PUMA level) completed")
if(length(unbal$PUMA) > 0){
  print(paste0("There are imbalanced PUMA in the data removed: ", paste0(unbal$PUMA, collapse = ", ")))
}

## Data Aggregation for Panel Data(PUMA+FTOTINC_QUARTILE level) ----

panel <- data[YEAR >=2005, ]

panel <- panel[, .(
      white_ratio = weighted.mean(race_simplified == "White", w = PERWT, na.rm = TRUE),
      black_ratio = weighted.mean(race_simplified == "Black", w = PERWT, na.rm = TRUE),
      asian_ratio = weighted.mean(race_simplified == "Asian", w = PERWT, na.rm = TRUE),
      hispanic_ratio = weighted.mean(race_simplified == "Hispanic", w = PERWT, na.rm = TRUE),
      avg_age = weighted.mean(AGE, w = PERWT, na.rm = TRUE),
      college_rate = weighted.mean(college, w = PERWT, na.rm = TRUE), # At least some college education
      employment_rate = sum(as.integer(EMPSTAT == 1)*PERWT, na.rm = TRUE) / sum(as.integer(EMPSTAT %in% 1:2)*PERWT, na.rm = TRUE), # Employed or Unemployed
      industry_concentration = hhi(INDNAICS, w = PERWT),
      ratio_concentration = hhi(race_simplified, w = PERWT),
      female_ratio = weighted.mean(female, w = PERWT, na.rm = TRUE),
      married_ratio = weighted.mean(MARST == 1, w = PERWT, na.rm = TRUE),
      avg_income = weighted.mean(INCTOT, w = PERWT, na.rm = TRUE),
      avg_FTOTINC = weighted.mean(FTOTINC, w = HHWT, na.rm = TRUE),
      avg_school_years = weighted.mean(school_years, w = PERWT, na.rm = TRUE),
      total_pop = sum(PERWT, na.rm = TRUE),
      avg_nchild = weighted.mean(NCHILD, w = PERWT, na.rm = TRUE),
      oil_and_gas_industry_share = weighted.mean(oil_and_gas_industry, w = PERWT, na.rm = TRUE)
    ), by = .(YEAR, treat_year_CSDID, treated, STATEFIP, PUMA, W, fincomeQ_national)]


## check for imbalanced in the data

unbal <- panel[, .(n=dplyr::n_distinct(YEAR)), by = .(PUMA, fincomeQ_national)]
unbal <- unbal[n!=max(n)]

print(unbal) # 2201801(Louisiana), 2201802(Louisiana), 2202300(Louisiana), 22777777(Louisiana)

panel <- panel[!(PUMA %in% unbal$PUMA & fincomeQ_national %in% unbal$fincomeQ_national)]  

### write to database
if(rewrite_database){
  dbExecute(con, "DROP TABLE IF EXISTS PUMA_PANEL_FTOTINC_QUARTILE")
  dbWriteTable(con, "PUMA_PANEL_FTOTINC_QUARTILE", panel)
}

print("Data Aggregation for Panel Data(PUMA+FTOTINC_QUARTILE level) completed")
if(length(unbal$PUMA) > 0){
  print(paste0("There are imbalanced PUMA and fincomeQ_national combinations in the data removed: ", paste0(paste0(unbal$PUMA, "_", unbal$fincomeQ_national), collapse = ", ")))
}

dbDisconnect(con, shutdown = TRUE)
