# libraries ----
library(DBI)
library(duckdb)
library(data.table)
library(did)
library(fixest)
library(broom)
library(stringr)
library(didimputation)
library(ggplot2)
library(dplyr)
library(DIDmultiplegt)
if (!("dplyr" %in% rownames(utils::installed.packages()))) {
  stop("Package 'dplyr' is required but not installed. Please install it with install.packages('dplyr').", call. = FALSE)
}
if (!("tibble" %in% rownames(utils::installed.packages()))) {
  stop("Package 'tibble' is required but not installed. Please install it with install.packages('tibble').", call. = FALSE)
}



# functions ----
source("code/functions.r")

# output path ----
output_dir <- "outputs"
if(!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# data ----
con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = "data/ACS.duckdb")

duckdb::dbListTables(con)

data <- dbReadTable(con, "ACS_00_14_preprocessed")
covariates_2000 <- dbReadTable(con, "PUMA_COVARIATES_2000")

dbDisconnect(con, shutdown = TRUE)

## into data.table
data <- data |> as.data.table()
data <- data |> merge(covariates_2000, by = "STATEFIP", all.x = TRUE)

data[, school_years := educD_to_schlyrs(EDUCD)]

data[ , `:=`(
  log_FTOTINC = log(FTOTINC),
  log_income = log(INCTOT),
  log_school_years = log(school_years),
  log_avg_income_2000 = log(avg_income_2000),
  log_avg_school_years_2000 = log(avg_school_years_2000),
  log_total_pop_2000 = log(total_pop_2000)
)]

# filter on young
data_young_adults <- data[AGE >= 18 & AGE <= 25, ]
data_young_adults[, treat_year:= fifelse(is.na(treat_year), 0, treat_year)]


cov <- c("white_ratio_2000", "total_pop_2000", "oil_and_gas_industry_share_2000")

# Overall ATE ----

outcomes <- c("log_school_years", "school_years", "college")

## (1) TWFE ----
results <- tibble::tibble(
  outcome = NA,
  time = NA,
  estimate = NA,
  lb = NA,
  ub = NA,
  se = NA,
  p_value = NA,
  method = NA
)

for (outcome in outcomes) {
  fml <- as.formula(paste0(outcome, " ~ i(YEAR-treat_year_CSDID, ref = -1) | PUMA + YEAR"))
  l <- feols(fml, data = data_young_adults, vcov = "hetero")
  t <- broom::tidy(l) |> 
  dplyr::transmute(
    outcome = outcome,
    time = str_remove(term, "^YEAR - treat_year_CSDID::") |> as.numeric(), 
    estimate = estimate, 
    lb = estimate - 1.96 * std.error,
    ub = estimate + 1.96 * std.error,
    se = std.error,
    p_value = p.value,
    method = "Two-way fixed effects"
  ) |> 
  dplyr::mutate(time = dplyr::if_else(time < -900, NA, time))
  results <-dplyr::bind_rows(results, t)
}

## (2) CSDID ----
# 
# for(outcome in outcomes){
#   l <- att_gt(yname = outcome, 
#   gname = "treat_year", 
#   idname = "individual_id", 
#   tname = "YEAR", 
#   data = data_young_adults, 
#   control_group = "notyettreated", 
#   panel = FALSE,
#   # xformla = as.formula(paste0("~ ", paste0(cov, collapse=" + "))),
#   bstrap = FALSE,
#   clustervars = "PUMA",
#   # biters = 1000,
#   est_method = "reg",
#   base_period = "universal"
#   ) |> aggte(type = "dynamic", na.rm = TRUE) |> broom::tidy()
#   
#   t <- l |> dplyr::transmute(
#     outcome = outcome,
#     time = event.time,
#     estimate = estimate,
#     lb = conf.low,
#     ub = conf.high,
#     se = std.error,
#     p_value = (1-pnorm(abs(estimate/se)))*2,
#     method = "Callaway and Sant'Anna(2021)"
#   )
#   results <- dplyr::bind_rows(results, t)
# }


## (3) SADID ----

for(outcome in outcomes){
  l <- feols(as.formula(paste0(outcome, " ~ sunab(treat_year_CSDID, YEAR) | PUMA + YEAR")), data = data_young_adults, vcov = "hetero") |> broom::tidy()
  
  t <- l |> dplyr::transmute(
    outcome = outcome,
    time = str_remove(term, "^YEAR::") |> as.numeric(),
    estimate = estimate,
    lb = estimate - 1.96*std.error,
    ub = estimate + 1.96*std.error,
    se = std.error,
    p_value = p.value,
    method = "Sun and Abraham(2021)"
  )
  results <- dplyr::bind_rows(results, t)
}

event_study_plot(results |>  filter(!is.na(time)), 
                 time = "time", estimate = "estimate", lb = "lb", ub = "ub", color = "method", facet = "outcome", scales_facet ="free_y")
ggsave(file.path(output_dir, "overall_ATE(cross-section).png"), width = 12, height = 8)


