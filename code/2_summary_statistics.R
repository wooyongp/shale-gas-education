# libraries ----
library(DBI)
library(duckdb)
library(data.table)
library(stringr)
library(Hmisc)

# data ----
con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = "data/ACS.duckdb")

data <- dbReadTable(con, "ACS_00_14_preprocessed")

## disconnect database
dbDisconnect(con)

# output path ----
output_dir <- "outputs"
if(!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# functions ----
source("code/functions.r")

# summary statistics ----
## Basic summary
summary_stats_basic <- summary_stat_table(data, verbose = TRUE)
write.csv(summary_stats_basic, file.path(output_dir, "summary_stat_table.csv"), row.names = FALSE)

## Weighted summary
summary_stats_weighted <- summary_stat_table(data, weights = "PERWT", verbose = FALSE)
write.csv(summary_stats_weighted, file.path(output_dir, "summary_stat_table_weighted.csv"), row.names = FALSE)

## Grouped summary (treated)
summary_stats_grouped <- summary_stat_table(data, group = "treated", verbose = FALSE)
write.csv(summary_stats_grouped, file.path(output_dir, "summary_stat_table_by_treatment_status.csv"), row.names = FALSE)

## Grouped summary (year)
summary_stats_grouped <- summary_stat_table(data, group = "YEAR", verbose = FALSE)
write.csv(summary_stats_grouped, file.path(output_dir, "summary_stat_table_by_treatment_status.csv"), row.names = FALSE)

## Grouped summary (treated and year)
summary_stats_grouped <- summary_stat_table(data, group = c("treated", "YEAR"), verbose = FALSE)
write.csv(summary_stats_grouped, file.path(output_dir, "summary_stat_table_by_treatment_status_and_year.csv"), row.names = FALSE)