# libraries ----
library(DBI)
library(duckdb)
library(data.table)
library(stringr)

# data ----
con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = "data/ACS.duckdb")

data <- dbReadTable(con, "ACS_00_14_preprocessed")
