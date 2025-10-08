# libraries ----
library(DBI)
library(duckdb)

# data ----
con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = "data/ACS.duckdb")

# Create treatment lookup table directly in DuckDB
dbExecute(con, "DROP TABLE IF EXISTS treat_time")
dbExecute(con, "
  CREATE TABLE treat_time AS 
  SELECT * FROM (VALUES 
    (1, 2008), (5, 2006), (22, 2008), (38, 2007),
    (40, 2006), (42, 2008), (48, 2005), (54, 2008)
  ) AS t(statefip, treat_year)
")

# Do all preprocessing in one SQL query
dbExecute(con, "DROP TABLE IF EXISTS ACS_00_14_preprocessed")
dbExecute(con, "
  CREATE TABLE ACS_00_14_preprocessed AS
  SELECT 
    raw.* REPLACE (
      CASE WHEN raw.HHTYPE IN (0, 9) THEN NULL ELSE raw.HHTYPE END AS HHTYPE,
      CASE WHEN raw.REGION IN (97, 99) THEN NULL ELSE raw.REGION END AS REGION,
      CASE WHEN raw.STATEFIP = 99 THEN NULL ELSE raw.STATEFIP END AS STATEFIP,
      CASE WHEN raw.COUNTYFIP = 0 THEN NULL ELSE raw.COUNTYFIP END AS COUNTYFIP,
      CASE WHEN raw.MET2013 = 0 THEN NULL ELSE raw.MET2013 END AS MET2013,
      CASE WHEN raw.MET2013ERR = 0 THEN NULL ELSE raw.MET2013ERR END AS MET2013ERR,
      CASE WHEN raw.RELATED IN (9996, 9997, 9998, 9999) THEN NULL ELSE raw.RELATED END AS RELATED,
      CASE WHEN raw.SEX = 9 THEN NULL ELSE raw.SEX END AS SEX,
      CASE WHEN raw.AGE = 9999 THEN NULL ELSE raw.AGE END AS AGE,
      CASE WHEN raw.MARST = 9 THEN NULL ELSE raw.MARST END AS MARST,
      CASE WHEN raw.BIRTHYR IN (0, 9999) THEN NULL ELSE raw.BIRTHYR END AS BIRTHYR,
      CASE WHEN raw.RACED = 997 THEN NULL ELSE raw.RACED END AS RACED,
      CASE WHEN raw.HISPAN = 9 THEN NULL ELSE raw.HISPAN END AS HISPAN,
      CASE WHEN raw.HISPAND = 900 THEN NULL ELSE raw.HISPAND END AS HISPAND,
      CASE WHEN raw.BPL IN (997, 999) THEN NULL ELSE raw.BPL END AS BPL,
      CASE WHEN raw.BPLD >= 9700 THEN NULL ELSE raw.BPLD END AS BPLD,
      CASE WHEN raw.LANGUAGE IN (96, 99) THEN NULL ELSE raw.LANGUAGE END AS LANGUAGE,
      CASE WHEN raw.LANGUAGED >=9700 THEN NULL ELSE raw.LANGUAGED END AS LANGUAGED,
      CASE WHEN raw.EDUC IN (0, 99) THEN NULL ELSE raw.EDUC END AS EDUC,
      CASE WHEN raw.EDUCD IN (0, 1, 999) THEN NULL ELSE raw.EDUCD END AS EDUCD,
      CASE WHEN raw.EMPSTAT IN (0, 9) THEN NULL ELSE raw.EMPSTAT END AS EMPSTAT,
      CASE WHEN raw.EMPSTATD IN (0, 99) THEN NULL ELSE raw.EMPSTATD END AS EMPSTATD,
      CASE WHEN raw.LABFORCE IN (0, 9) THEN NULL ELSE raw.LABFORCE END AS LABFORCE,
      CASE WHEN raw.OCC = 0 THEN NULL ELSE raw.OCC END AS OCC,    
      CASE WHEN raw.IND = 0 THEN NULL ELSE raw.IND END AS IND,
      CASE WHEN raw.INDNAICS = '0' THEN NULL ELSE raw.INDNAICS END AS INDNAICS,
      CASE WHEN raw.INCTOT IN (0, 9999998, 9999999) THEN NULL ELSE raw.INCTOT END AS INCTOT,
      CASE WHEN raw.FTOTINC IN (0, 9999998, 9999999) THEN NULL ELSE raw.FTOTINC END AS FTOTINC,
      100000*raw.STATEFIP + raw.PUMA AS PUMA
    ),
    tt.treat_year,

    -- treated indicator
    CASE WHEN raw.STATEFIP IN (1, 5, 22, 38, 40, 42, 48, 54) THEN 1 ELSE 0 END AS treated,
    
    -- school_years (from educD_to_schlyrs function)
    CASE 
      WHEN EDUCD = 1 THEN NULL
      WHEN EDUCD <= 12 THEN 0
      WHEN EDUCD IN (13, 14) THEN 1
      WHEN EDUCD = 15 THEN 2
      WHEN EDUCD = 16 THEN 3
      WHEN EDUCD = 17 THEN 4
      WHEN EDUCD BETWEEN 20 AND 22 THEN 5
      WHEN EDUCD = 23 THEN 6
      WHEN EDUCD IN (24, 25) THEN 7
      WHEN EDUCD = 26 THEN 8
      WHEN EDUCD = 30 THEN 9
      WHEN EDUCD = 40 THEN 10
      WHEN EDUCD = 50 THEN 11
      WHEN EDUCD BETWEEN 60 AND 65 THEN 12
      WHEN EDUCD IN (70, 71) THEN 13
      WHEN EDUCD BETWEEN 80 AND 83 THEN 14
      WHEN EDUCD = 90 THEN 15
      WHEN EDUCD IN (100, 101) THEN 16
      WHEN EDUCD = 110 THEN 17
      WHEN EDUCD = 111 THEN 18
      WHEN EDUCD = 112 THEN 19
      WHEN EDUCD BETWEEN 113 AND 115 THEN 20
      WHEN EDUCD = 116 THEN 24
      WHEN EDUCD = 999 THEN NULL
      ELSE NULL
    END AS school_years,
    
    -- oil_and_gas_industry (checks if INDNAICS starts with '21')
    CASE WHEN INDNAICS LIKE '21%' THEN 1 
      WHEN INDNAICS IS NOT NULL THEN 0
      WHEN INDNAICS = '0' THEN NULL
      ELSE NULL 
    END AS oil_and_gas_industry,
    
    -- treat_year_CSDID
    COALESCE(tt.treat_year, 3000) AS treat_year_CSDID,
    
    -- individual_id
    1000000*SERIAL + SAMPLE AS individual_id,
    
    -- log transformations
    CASE WHEN raw.INCTOT <= 0 THEN NULL ELSE LN(raw.INCTOT) END AS log_INCTOT,
    CASE WHEN raw.FTOTINC <= 0 THEN NULL ELSE LN(raw.FTOTINC) END AS log_FTOTINC,
    
    -- college indicator
    CASE WHEN raw.EDUCD >= 62 THEN 1 ELSE 0 END AS college,
    
    -- race_simplified
    CASE 
      WHEN raw.HISPAN > 0 THEN 'Hispanic'
      WHEN raw.RACE = 1 THEN 'White'
      WHEN raw.RACE = 2 THEN 'Black'
      WHEN raw.RACE BETWEEN 4 AND 6 THEN 'Asian'
      ELSE 'Others'
    END AS race_simplified,
    
    -- W (treatment indicator)
    CASE WHEN raw.YEAR >= COALESCE(tt.treat_year, 3000) THEN 1 ELSE 0 END AS W,

    -- female
    CASE WHEN raw.SEX = 2 THEN 1 
    WHEN raw.SEX = 1 THEN 0
      ELSE NULL END AS female
    
  FROM ACS_00_14_raw raw
  LEFT JOIN treat_time tt ON raw.STATEFIP = tt.statefip
  WHERE 
    -- Filter out Alaska, Hawaii, Colorado, Wyoming
    raw.STATEFIP NOT IN (2, 15, 8, 56)  -- Exclude Alaska, Hawaii, Colorado, Wyoming
    AND raw.YEAR IN (2000, 2005, 2006, 2007, 2008, 2009, 2010, 2011)
")

# Verify the result
result_count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM ACS_00_14_preprocessed")
print(paste("Preprocessed", result_count$n, "rows"))

# Preview
head(dbGetQuery(con, "SELECT * FROM ACS_00_14_preprocessed LIMIT 10"))
dbDisconnect(con, shutdown = TRUE)
