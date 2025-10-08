# libraries ----
library(DBI)
library(duckdb)
library(data.table)
library(modi)

# data ----
con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = "data/ACS.duckdb")

# read data
data <- dbReadTable(con, "ACS_00_14_preprocessed")
data <- as.data.table(data)


## data sanity check
data[, .N, is.na(FTOTINC)]

# Create FTOTINC QUARTILES by STATE
ftotinc_quartile_state <- data[!is.na(FTOTINC)][, 
    .(
        FINC1Q_STATE = weighted.quantile(FTOTINC, w = HHWT, prob = 0.25),
        FINC2Q_STATE = weighted.quantile(FTOTINC, w = HHWT, prob = 0.5),
        FINC3Q_STATE = weighted.quantile(FTOTINC, w = HHWT, prob = 0.75)
    ),
    by = .(YEAR, STATEFIP)]

head(ftotinc_quartile_state)

# Create FTOTINC QUARTILES NATIONAL
ftotinc_quartile_national <- data[!is.na(FTOTINC)][, 
    .(
        FINC1Q_NATIONAL = weighted.quantile(FTOTINC, w = HHWT, prob = 0.25),
        FINC2Q_NATIONAL = weighted.quantile(FTOTINC, w = HHWT, prob = 0.5),
        FINC3Q_NATIONAL = weighted.quantile(FTOTINC, w = HHWT, prob = 0.75)
    ),
    by = .(YEAR)]

head(ftotinc_quartile_national)

## write to database
dbExecute(con, "DROP TABLE IF EXISTS FTOTINC_QUARTILES_STATE")
dbWriteTable(con, "FTOTINC_QUARTILES_STATE", ftotinc_quartile_state)

dbExecute(con, "DROP TABLE IF EXISTS FTOTINC_QUARTILES_NATIONAL")
dbWriteTable(con, "FTOTINC_QUARTILES_NATIONAL", ftotinc_quartile_national)

print("FTOTINC QUARTILES by STATE and NATIONAL created")

dbDisconnect(con, shutdown = TRUE)
