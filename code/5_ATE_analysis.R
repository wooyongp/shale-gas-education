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

data <- dbReadTable(con, "PUMA_PANEL")
data_quartiles <- dbReadTable(con, "PUMA_PANEL_FTOTINC_QUARTILE")
covariates_2000 <- dbReadTable(con, "PUMA_COVARIATES_2000")
covariates_2000_quartiles <- dbReadTable(con, "PUMA_COVARIATES_2000_FTOTINC_QUARTILE")

dbDisconnect(con, shutdown = TRUE)

## into data.table
data <- data |> as.data.table()
data_quartiles <- data_quartiles |> as.data.table()
covariates_2000 <- covariates_2000 |> as.data.table()
covariates_2000_quartiles <- covariates_2000_quartiles |> as.data.table()

data <- data |> merge(covariates_2000, by = "STATEFIP", all.x = TRUE)
data_quartiles <- data_quartiles |> merge.data.table(covariates_2000_quartiles, by = c("STATEFIP", "fincomeQ_national"), all.x = TRUE)

## Mutations ----
data[ , `:=`(
  log_avg_FTOTINC = log(avg_FTOTINC),
  log_avg_income = log(avg_income),
  log_avg_school_years = log(avg_school_years),
  log_total_pop = log(total_pop),
  log_avg_income_2000 = log(avg_income_2000),
  log_avg_school_years_2000 = log(avg_school_years_2000),
  log_total_pop_2000 = log(total_pop_2000)
)]

data_quartiles[ , `:=`(
  log_avg_FTOTINC = log(avg_FTOTINC),
  log_avg_income = log(avg_income),
  log_avg_school_years = log(avg_school_years),
  log_total_pop = log(total_pop),
  log_avg_income_2000 = log(avg_income_2000),
  log_avg_school_years_2000 = log(avg_school_years_2000),
  log_total_pop_2000 = log(total_pop_2000)
)]

# Specifying covariates for HTE and propensity score analysis ----
cov <- c("white_ratio_2000", "total_pop_2000", "oil_and_gas_industry_share_2000")

# Overall ATE ----

outcomes <- c("log_avg_FTOTINC", "oil_and_gas_industry_share", "total_pop", "college_rate")

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
  l <- feols(fml, data = data, weights = ~ total_pop, vcov = "hetero")
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

for(outcome in outcomes){
  l <- att_gt(yname = outcome, gname = "treat_year_CSDID", idname = "PUMA", tname = "YEAR", data = data, 
  control_group = "notyettreated", panel = TRUE,
  # xformla = as.formula(paste0("~ ", paste0(cov, collapse=" + "))),
  bstrap = TRUE,
  biters = 1000,
  est_method = "reg",
  base_period = "universal",
  weights = "total_pop") |> aggte(type = "dynamic", na.rm = TRUE) |> broom::tidy()
  
  t <- l |> dplyr::transmute(
    outcome = outcome,
    time = event.time,
    estimate = estimate,
    lb = conf.low,
    ub = conf.high,
    se = std.error,
    p_value = (1-pnorm(abs(estimate/se)))*2,
    method = "Callaway and Sant'Anna(2021)"
  )
  results <- dplyr::bind_rows(results, t)
}


## (3) SADID ----

for(outcome in outcomes){
  l <- feols(as.formula(paste0(outcome, " ~ sunab(treat_year_CSDID, YEAR) | PUMA + YEAR")), data = data, weights = ~ total_pop, vcov = "hetero") |> broom::tidy()
  
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

## (4) Borusyak, Jaravel, and Spiess (2021) ----

for(outcome in outcomes){
  l <- didimputation::did_imputation(
    data = as.data.frame(data),
    yname = outcome,
    gname = "treat_year_CSDID",
    tname = "YEAR", 
    idname = "PUMA",
    first_stage = ~ 1 | PUMA + YEAR,
    wname = "total_pop",
    horizon = TRUE,
    pretrends = TRUE,
    cluster_var = "PUMA"
  )
  
  t <- l |> dplyr::transmute(
    outcome = outcome,
    time = as.numeric(term),
    estimate = estimate,
    lb = conf.low,
    ub = conf.high,
    se = std.error,
    p_value = (1-pnorm(abs(estimate/se)))*2,
    method = "Borusyak, Jaravel, and Spiess(2021)"
  ) |> dplyr::mutate(time = dplyr::if_else(time < -900, NA, time))
  results <- dplyr::bind_rows(results, t)
}

## (5) Chaisemartin and D'Haultfoeuille (2020) ----

for(outcome in outcomes){
  l <- DIDmultiplegt::did_multiplegt(
    mode = "dyn",
    as_tibble(data),
    outcome,
    "PUMA",
    "YEAR", 
    "W",
    effects = 5,
    placebo = 3,
    weight = "total_pop",
    less_conservative_se = TRUE
  )
  t <-  l$results$Effects |> tibble::as_tibble() |> 
    dplyr::transmute(
    outcome = outcome,
    time = 1:5,
    estimate = Estimate,
    lb = `LB CI`,
    ub = `UB CI`,
    se = SE,
    p_value = (1-pnorm(abs(estimate/se)))*2,
    method = "Chaisemartin and D'Haultfoeuille (2020)"
  ) |> 
    bind_rows(
      l$results$Placebos |> tibble::as_tibble() |> 
        dplyr::transmute(
          outcome = outcome,
          time = -2:-1,
          estimate = Estimate,
          lb = `LB CI`,
          ub = `UB CI`,
          se = SE,
          p_value = (1-pnorm(abs(estimate/se)))*2,
          method = "Chaisemartin and D'Haultfoeuille (2020)"
        )
      )
  results <- dplyr::bind_rows(results, t)
}



event_study_plot(results |> 
                   # tidyr::drop_na() |> 
                   dplyr::filter(!str_detect(method, "Borusyak"), !is.na(time)), 
                 time = "time", estimate = "estimate", lb = "lb", ub = "ub", color = "method", facet = "outcome", scales_facet ="free_y")
ggsave(file.path(output_dir, "overall_ATE.png"), width = 12, height = 8)


# ATE by quartiles ----

outcomes <- c("log_avg_FTOTINC", "oil_and_gas_industry_share", "total_pop", "college_rate")

## (1) TWFE ----
results_quartile <- tibble::tibble(
  quartile = NA,
  outcome = NA,
  time = NA,
  estimate = NA,
  lb = NA,
  ub = NA,
  se = NA,
  p_value = NA,
  method = NA
)



for(quartile in 1:4){
  dt <- data_quartiles[fincomeQ_national == quartile, ]

  for (outcome in outcomes) {
    fml <- as.formula(paste0(outcome, " ~ i(YEAR-treat_year_CSDID, ref = -1) | PUMA + YEAR"))
    l <- feols(fml, data = dt, weights = ~ total_pop, vcov = "hetero")
    t <- broom::tidy(l) |> 
      dplyr::transmute(
        quartile = quartile,
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
    results_quartile <-dplyr::bind_rows(results_quartile, t)
  }
}

## (2) CSDID ----

for(quartile in 1:4){
  dt <- data_quartiles[fincomeQ_national == quartile, ]
  
  for(outcome in outcomes){
    l <- att_gt(yname = outcome, gname = "treat_year_CSDID", idname = "PUMA", tname = "YEAR", data = dt, 
                control_group = "notyettreated", panel = TRUE,
                # xformla = as.formula(paste0("~ ", paste0(cov, collapse=" + "))),
                bstrap = TRUE,
                biters = 1000,
                est_method = "reg",
                base_period = "universal",
                weights = "total_pop") |> aggte(type = "dynamic", na.rm = TRUE) |> broom::tidy()
    
    t <- l |> dplyr::transmute(
      quartile = quartile,
      outcome = outcome,
      time = event.time,
      estimate = estimate,
      lb = conf.low,
      ub = conf.high,
      se = std.error,
      p_value = (1-pnorm(abs(estimate/se)))*2,
      method = "Callaway and Sant'Anna(2021)"
    )
    results_quartile <- dplyr::bind_rows(results_quartile, t)
  }
}

## (3) SADID ----

for(quartile in 1:4){
  dt <- data_quartiles[fincomeQ_national == quartile, ]
  for(outcome in outcomes){
    l <- feols(as.formula(paste0(outcome, " ~ sunab(treat_year_CSDID, YEAR) | PUMA + YEAR")), data = dt, weights = ~ total_pop, vcov = "hetero") |> broom::tidy()
    
    t <- l |> dplyr::transmute(
      quartile = quartile,
      outcome = outcome,
      time = str_remove(term, "^YEAR::") |> as.numeric(),
      estimate = estimate,
      lb = estimate - 1.96*std.error,
      ub = estimate + 1.96*std.error,
      se = std.error,
      p_value = p.value,
      method = "Sun and Abraham(2021)"
    )
    results_quartile <- dplyr::bind_rows(results_quartile, t)
  }
}
  
## (4) Borusyak, Jaravel, and Spiess (2021) ----

for(quartile in 1:4){
  dt <- data_quartiles[fincomeQ_national == quartile, ]
  for(outcome in outcomes){
    l <- didimputation::did_imputation(
      data = as.data.frame(dt),
      yname = outcome,
      gname = "treat_year_CSDID",
      tname = "YEAR", 
      idname = "PUMA",
      first_stage = ~ 1 | PUMA + YEAR,
      wname = "total_pop",
      horizon = TRUE,
      pretrends = TRUE,
      cluster_var = "PUMA"
    )
    
    t <- l |> dplyr::transmute(
      quartile = quartile,
      outcome = outcome,
      time = as.numeric(term),
      estimate = estimate,
      lb = conf.low,
      ub = conf.high,
      se = std.error,
      p_value = (1-pnorm(abs(estimate/se)))*2,
      method = "Borusyak, Jaravel, and Spiess(2021)"
    ) |> dplyr::mutate(time = dplyr::if_else(time < -900, NA, time))
    results_quartile <- dplyr::bind_rows(results_quartile, t)
  }
}
  
## (5) Chaisemartin and D'Haultfoeuille (2020) ----

for(quartile in 1:4){
  for(outcome in outcomes){
    dt <- data_quartiles[fincomeQ_national == quartile, ]
    l <- DIDmultiplegt::did_multiplegt(
      mode = "dyn",
      as_tibble(dt),
      outcome,
      "PUMA",
      "YEAR", 
      "W",
      effects = 5,
      placebo = 3,
      weight = "total_pop",
      less_conservative_se = TRUE
    )
    t <-  l$results$Effects |> tibble::as_tibble() |> 
      dplyr::transmute(
        quartile = quartile,
        outcome = outcome,
        time = 1:5,
        estimate = Estimate,
        lb = `LB CI`,
        ub = `UB CI`,
        se = SE,
        p_value = (1-pnorm(abs(estimate/se)))*2,
        method = "Chaisemartin and D'Haultfoeuille (2020)"
      ) |> 
      bind_rows(
        l$results$Placebos |> tibble::as_tibble() |> 
          dplyr::transmute(
            quartile = quartile,
            outcome = outcome,
            time = -2:-1,
            estimate = Estimate,
            lb = `LB CI`,
            ub = `UB CI`,
            se = SE,
            p_value = (1-pnorm(abs(estimate/se)))*2,
            method = "Chaisemartin and D'Haultfoeuille (2020)"
          )
      )
    results_quartile <- dplyr::bind_rows(results_quartile, t)
  }
}

for(i in 1:length(outcomes)){
  outcome_name <- outcomes[i]
  t <- results_quartile |> 
    dplyr::filter(!str_detect(method, "Borusyak") & !is.na(time) & outcome == outcome_name)
  p <- event_study_plot(t, 
                 time = "time", estimate = "estimate", lb = "lb", ub = "ub", color = "quartile", facet = "method", scales_facet ="free_y", title = outcome_name)
  assign(paste0("p", i), p)
}

p1
p2
p3
p4
