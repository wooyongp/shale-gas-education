# libraries ----
library(data.table)
library(arrow)
library(DBI)
library(duckdb)

# data ----
data <- read_parquet("data/ACS_2000-2014.parquet") |> as.data.table()

con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = "data/ACS.duckdb")


dbWriteTable(con, "ACS_00_14_raw", data)
