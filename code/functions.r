summary_stat_table <- function(data, weights = NULL, group = NULL, verbose = TRUE){

  '
  data: data.table (convert to data.table if not)
  weights: weights
  group: group (character or character vector)
  verbose: verbose
  '
  if (!"Hmisc" %in% loadedNamespaces()) {
    require(Hmisc)
  }
  if (!"data.table" %in% loadedNamespaces()) {
    require(data.table)
  }

  # Convert to data.table if not already
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Precompute numeric columns once to avoid repeated scans
  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1L))]
  if (length(numeric_cols) == 0) {
    return(data.table())
  }

  # Helper: calculate stats for a set of row indices (idx)
  calc_summary_idx <- function(idx, w_vec = NULL) {
    rows_list <- vector("list", length(numeric_cols))
    for (k in seq_along(numeric_cols)) {
      col <- numeric_cols[k]
      values <- data[[col]][idx]

      n_obs <- length(values)
      n_not_na <- sum(!is.na(values))
      n_na <- n_obs - n_not_na
      frac_na <- if (n_obs > 0) n_na / n_obs else NA_real_

      n_zeros <- sum(values == 0, na.rm = TRUE)
      frac_zeros <- if (n_not_na > 0) n_zeros / n_not_na else NA_real_

      if (n_not_na == 0) {
        if (verbose) {
          rows_list[[k]] <- data.table(
            variable = col, n_obs = n_obs, n_not_na = 0L, n_na = n_obs,
            frac_na = frac_na, n_zeros = n_zeros, frac_zeros = frac_zeros,
            mean = NA_real_, sd = NA_real_, skewness = NA_real_,
            min = NA_real_, max = NA_real_, p10 = NA_real_, p20 = NA_real_, p30 = NA_real_, p40 = NA_real_,
            p50 = NA_real_, p60 = NA_real_, p70 = NA_real_, p80 = NA_real_, p90 = NA_real_, p99 = NA_real_,
            p999 = NA_real_, p9999 = NA_real_
          )
        } else {
          rows_list[[k]] <- data.table(
            variable = col, n_obs = n_obs, frac_na = frac_na, frac_zeros = frac_zeros,
            mean = NA_real_, sd = NA_real_, min = NA_real_, max = NA_real_
          )
        }
        next
      }

      clean_values <- values[!is.na(values)]

      if (!is.null(w_vec)) {
        weights_clean <- w_vec[idx][!is.na(values)]
        weights_clean <- weights_clean[!is.na(weights_clean)]
        if (length(weights_clean) == length(clean_values)) {
          mean_val <- stats::weighted.mean(clean_values, weights_clean)
          # population-style weighted sd
          sd_val <- sqrt(sum(weights_clean * (clean_values - mean_val)^2) / sum(weights_clean))
          quantiles <- Hmisc::wtd.quantile(clean_values, weights_clean,
            probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999, 0.9999))
          names(quantiles) <- c("p10","p20","p30","p40","p50","p60","p70","p80","p90","p99","p999","p9999")
          if (length(clean_values) > 2 && sd_val > 0) {
            skewness_val <- sum(weights_clean * ((clean_values - mean_val) / sd_val)^3) / sum(weights_clean)
          } else {
            skewness_val <- NA_real_
          }
        } else {
          mean_val <- NA_real_; sd_val <- NA_real_; skewness_val <- NA_real_
          quantiles <- setNames(rep(NA_real_, 12), c("p10","p20","p30","p40","p50","p60","p70","p80","p90","p99","p999","p9999"))
        }
      } else {
        mean_val <- mean(clean_values)
        sd_val <- stats::sd(clean_values)
        quantiles <- stats::quantile(clean_values, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999, 0.9999))
        names(quantiles) <- c("p10","p20","p30","p40","p50","p60","p70","p80","p90","p99","p999","p9999")
        if (length(clean_values) > 2 && sd_val > 0) {
          skewness_val <- sum(((clean_values - mean_val) / sd_val)^3) / length(clean_values)
        } else {
          skewness_val <- NA_real_
        }
      }

      min_val <- min(clean_values)
      max_val <- max(clean_values)

      if (verbose) {
        rows_list[[k]] <- data.table(
          variable = col,
          n_obs = n_obs,
          n_not_na = n_not_na,
          n_na = n_na,
          frac_na = round(frac_na, 4),
          n_zeros = n_zeros,
          frac_zeros = round(frac_zeros, 4),
          mean = round(mean_val, 4),
          sd = round(sd_val, 4),
          skewness = round(skewness_val, 4),
          min = min_val,
          max = max_val,
          p10 = round(quantiles["p10"], 4),
          p20 = round(quantiles["p20"], 4),
          p30 = round(quantiles["p30"], 4),
          p40 = round(quantiles["p40"], 4),
          p50 = round(quantiles["p50"], 4),
          p60 = round(quantiles["p60"], 4),
          p70 = round(quantiles["p70"], 4),
          p80 = round(quantiles["p80"], 4),
          p90 = round(quantiles["p90"], 4),
          p99 = round(quantiles["p99"], 4),
          p999 = round(quantiles["p999"], 4),
          p9999 = round(quantiles["p9999"], 4)
        )
      } else {
        rows_list[[k]] <- data.table(
          variable = col,
          n_obs = n_obs,
          frac_na = round(frac_na, 4),
          frac_zeros = round(frac_zeros, 4),
          mean = round(mean_val, 4),
          sd = round(sd_val, 4),
          min = min_val,
          max = max_val
        )
      }
    }

    data.table::rbindlist(rows_list)
  }

  # If no grouping, compute for all rows
  if (is.null(group)) {
    w_vec <- if (!is.null(weights)) {
      if (!weights %in% names(data)) stop(paste("Weight variable", weights, "not found in data"))
      data[[weights]]
    } else NULL
    return(calc_summary_idx(seq_len(nrow(data)), w_vec))
  }

  # With grouping: build index lists to avoid copying data
  if (is.character(group)) {
    missing_groups <- group[!group %in% names(data)]
    if (length(missing_groups)) stop(paste("Group variables not found in data:", paste(missing_groups, collapse = ", ")))
  } else {
    stop("group must be a character vector of column names")
  }

  # Pre-extract weights vector once
  full_w <- if (!is.null(weights)) {
    if (!weights %in% names(data)) stop(paste("Weight variable", weights, "not found in data"))
    data[[weights]]
  } else NULL

  # Build index lists via data.table by= with character vector columns
  idx_dt <- data[, .(idx = list(.I)), by = c(group)]

  # Build names for each group
  grp_vals_dt <- idx_dt[, ..group]
  group_names <- vapply(seq_len(nrow(grp_vals_dt)), function(i) {
    vals <- as.list(grp_vals_dt[i])
    paste(paste(group, vapply(vals, as.character, ""), sep = "="), collapse = ", ")
  }, character(1L))

  # Compute per-group summaries
  out_list <- vector("list", nrow(idx_dt))
  for (i in seq_len(nrow(idx_dt))) {
    idx <- idx_dt$idx[[i]]
    out_list[[i]] <- calc_summary_idx(idx, full_w)
  }
  names(out_list) <- as.character(group_names)
  out_list
}


educD_to_schlyrs <- function(educD){
  case_when(
    educD == 1 ~ NA, educD <= 12 ~ 0, educD %in% 13:14 ~ 1,
    educD == 15 ~ 2, educD == 16 ~ 3, educD == 17 ~ 4,
    educD %in% 20:22 ~ 5, educD == 23 ~ 6, educD %in% 24:25 ~ 7,
    educD == 26 ~ 8, educD == 30 ~ 9, educD == 40 ~ 10,
    educD == 50 ~ 11, educD %in% 60:65 ~ 12, educD %in% 70:71 ~ 13,
    educD %in% 80:83 ~ 14, educD == 90 ~ 15, educD %in% 100:101 ~ 16,
    educD == 110 ~ 17, educD == 111 ~ 18, educD == 112 ~ 19,
    educD %in% 113:115 ~ 20, educD == 116 ~ 24, educD == 999 ~ NA, .default = NA) }


race_simplified <- function(RACE, HISPAN){
  case_when(
    HISPAN > 0 ~ "Hispanic",
    RACE == 1 ~ "White",
    RACE == 2 ~ "Black",
    RACE %in% 4:6 ~ "Asian",
    .default = "Others"
  )
}


hhi <- function(arg, wt = NULL) {
  # Support tidy evaluation for use in dplyr::summarize
  arg_quo <- rlang::enquo(arg)
  wt_quo <- rlang::enquo(wt)
  # Evaluate in the calling environment (for standalone use) or in a data-masked context (dplyr)
  arg_eval <- rlang::eval_tidy(arg_quo)
  wt_eval <- if (rlang::quo_is_null(wt_quo)) NULL else rlang::eval_tidy(wt_quo)
  if (length(arg_eval) == 0) return(NA_real_)
  if (all(is.na(arg_eval))) return(NA_real_)
  # Remove NAs in arg (and corresponding weights)
  if (!is.null(wt_eval)) {
    keep <- !is.na(arg_eval) & !is.na(wt_eval)
    arg_eval <- arg_eval[keep]
    wt_eval <- wt_eval[keep]
  } else {
    arg_eval <- arg_eval[!is.na(arg_eval)]
  }
  if (length(arg_eval) == 0) return(NA_real_)
  if (is.null(wt_eval)) {
    tab <- table(arg_eval)
    shares <- as.numeric(tab) / sum(tab)
  } else {
    shares <- tapply(wt_eval, arg_eval, sum)
    shares <- shares / sum(shares)
  }
  sum(shares^2)
}


event_study_plot <- function(data, time, estimate, lb, ub, color=NULL, facet=NULL, title = NULL, scales_facet = "fixed"){
  
  # factorize color
  data <- data |> mutate(!!sym(color) := as.factor(.data[[color]]))
  
  # Build base plot
  p <- ggplot(data, aes(x=.data[[time]], y=.data[[estimate]]))
  
  # Add color if specified
  if (!is.null(color)) {
    p <- p + aes(color=.data[[color]])
  }
  
  # Add layers
  p <- p +
    geom_hline(aes(yintercept=0), linetype="dashed")
  
  # Add error bars with conditional color and dodging
  if (!is.null(color)) {
    p <- p + geom_errorbar(aes(ymin=.data[[lb]], ymax=.data[[ub]], color=.data[[color]]), width=0.3, position=position_dodge(width=0.6))
  } else {
    p <- p + geom_errorbar(aes(ymin=.data[[lb]], ymax=.data[[ub]]), width=0.3)
  }
  
  # Add points with conditional color and dodging
  if (!is.null(color)) {
    p <- p + geom_point(aes(y=.data[[estimate]], color=.data[[color]]), position=position_dodge(width=0.6))
  } else {
    p <- p + geom_point(aes(y=.data[[estimate]]))
  }
  
  # Add scales and theme
  p <- p +
    scale_x_continuous(breaks=data[[time]], labels=data[[time]], name=NULL) +
    scale_y_continuous(name="Average Effect(std. dev.)") +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5))
  
  # Add title if specified
  if (!is.null(title)) {
    p <- p + labs(title=title)
  }
  
  # Add facet if specified
  if (!is.null(facet)) {
    p <- p + facet_wrap(~.data[[facet]], scales = scales_facet)
  }
  

  return(p)
}




# csdid_plot <- function(broom_table){
#   ggplot(broom_table |> mutate(color = as.factor(as.integer(event.time>=0))), aes(x=event.time)) +
#     geom_hline(aes(yintercept=0), linetype="dashed") +
#     geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color = color), width =0.2) +
#     geom_point(aes(y = estimate, color = color)) +
#     scale_x_continuous(breaks = broom_table$event.time, labels = broom_table$event.time, name=NULL) +
#     scale_y_continuous(name="Average Effect(std. dev.)") +
#     scale_color_discrete(name=NULL, labels=c("Pre", "Post")) +
#     theme_bw() +
#     labs(title="Event Study with Callaway and Sant'anna(2021)") +
#     theme(legend.position = "bottom", plot.title = element_text(hjust=0.5))
# }

# sadid_plot <- function(l){
#   l <- broom::tidy(l) |> bind_cols(confint(l) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))
#   l <- mutate(l, period = str_remove(term, "YEAR::") |> as.numeric()) |> 
#     filter(!is.na(period)) |> 
#     mutate(color = as.factor(as.integer(period>=0)))
#   ggplot(l, aes(x=period)) +
#     geom_hline(aes(yintercept=0), linetype="dashed") +
#     geom_errorbar(aes(ymin=lb, ymax=ub, color = color), width =0.2) +
#     geom_point(aes(y = estimate, color = color)) +
#     scale_x_continuous(breaks = l$period, labels = l$period, name=NULL) +
#     scale_y_continuous(name="Average Effect(std. dev.)") +
#     scale_color_discrete(name=NULL, labels=c("Pre", "Post")) +
#     theme_bw() +
#     labs(title="Event Study with Sun and Abraham(2021)") +
#     theme(legend.position = "bottom", plot.title = element_text(hjust=0.5))
# }


# csdid_plot_quartile <- function(out1, out2, out3, out4){
#   temp <- broom::tidy(out1) |> mutate(quartile=1) |> 
#     bind_rows(broom::tidy(out2) |> mutate(quartile=2)) |> 
#     bind_rows(broom::tidy(out3) |> mutate(quartile=3)) |> 
#     bind_rows(broom::tidy(out4) |> mutate(quartile=4))

#   temp <- temp |> mutate(push = if_else(quartile<=2, if_else(quartile==1, -0.15, -0.05), if_else(quartile==3, 0.05, 0.15)))

#   p1 <- ggplot(temp |> mutate(quartile = as.factor(quartile)), aes(x=event.time + push)) +
#     geom_hline(aes(yintercept=0), linetype="dashed") +
#     geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color = quartile), width = 0.2) +
#     geom_point(aes(y = estimate, color = quartile)) +
#     scale_x_continuous(breaks = broom::tidy(out1)$event.time, labels = broom::tidy(out1)$event.time, name=NULL) +
#     scale_y_continuous(name="Average Effect(std. dev.)") +
#     scale_color_discrete(name="income quartile", labels=c("1Q", "2Q", "3Q", "4Q")) +
#     theme_bw() +
#     labs(title="Event Study with Callaway and Sant'anna(2021)") +
#     theme(legend.position = c(0.1, 0.8), plot.title = element_text(hjust=0.5),
#           legend.background = element_rect(color="black"))
#   return(p1)
# }


# sadid_plot_quartile <- function(out1, out2, out3, out4){
#   temp <- broom::tidy(out1) |> mutate(quartile=1) |> bind_cols(confint(out1) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`)) |> 
#     bind_rows(broom::tidy(out2) |> mutate(quartile=2) |> bind_cols(confint(out2) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
#     bind_rows(broom::tidy(out3) |> mutate(quartile=3) |> bind_cols(confint(out3) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
#     bind_rows(broom::tidy(out4) |> mutate(quartile=4) |> bind_cols(confint(out4) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
#     mutate(period = str_remove(term, "YEAR::") |> as.numeric()) |> 
#     filter(!is.na(period)) |> 
#     mutate(color = as.factor(as.integer(period>=0)))

#   temp <- temp |> mutate(push = if_else(quartile<=2, if_else(quartile==1, -0.15, -0.05), if_else(quartile==3, 0.05, 0.15)))

#   p1 <- ggplot(temp |> mutate(quartile = as.factor(quartile)), aes(x=period + push)) +
#     geom_hline(aes(yintercept=0), linetype="dashed") +
#     geom_errorbar(aes(ymin=lb, ymax=ub, color = quartile), width = 0.2) +
#     geom_point(aes(y = estimate, color = quartile)) +
#     scale_x_continuous(breaks = filter(temp, quartile==1)$period, labels = filter(temp, quartile==1)$period, name=NULL) +
#     scale_y_continuous(name="Average Effect(std. dev.)") +
#     scale_color_discrete(name="income quartile", labels=c("1Q", "2Q", "3Q", "4Q")) +
#     theme_bw() +
#     labs(title="Event Study with Sun and Abraham(2021)") +
#     theme(legend.position = c(0.1, 0.8), plot.title = element_text(hjust=0.5),
#           legend.background = element_rect(color="black"))
#   return(p1)
# }

# csdid_plot_race_simplified <- function(outAsian, outBlack, outHispanic, outWhite, outOthers){
#   temp <- broom::tidy(outAsian) |> mutate(race_simplified="Asian") |> 
#     bind_rows(broom::tidy(outBlack) |> mutate(race_simplified="Black")) |> 
#     bind_rows(broom::tidy(outWhite) |> mutate(race_simplified="White")) |> 
#     bind_rows(broom::tidy(outHispanic) |> mutate(race_simplified="Hispanic")) |> 
#     bind_rows(broom::tidy(outOthers) |> mutate(race_simplified="Others"))

#   temp <- temp |> mutate(push = case_when(
#     race_simplified=="Asian" ~ -0.2,
#     race_simplified=="Black" ~ -0.1,
#     race_simplified=="Hispanic" ~ 0.1,
#     race_simplified=="Others" ~ 0.2,
#     .default = 0
#   ))

#   p1 <- ggplot(temp |> mutate(race_simplified = as.factor(race_simplified)), aes(x=event.time + push)) +
#     geom_hline(aes(yintercept=0), linetype="dashed") +
#     geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color = race_simplified), width = 0.2) +
#     geom_point(aes(y = estimate, color = race_simplified)) +
#     scale_x_continuous(breaks = broom::tidy(outAsian)$event.time, labels = broom::tidy(outAsian)$event.time, name=NULL) +
#     scale_y_continuous(name="Average Effect(std. dev.)") +
#     theme_bw() +
#     labs(title="Event Study with Callaway and Sant'anna(2021)") +
#     theme(legend.position = c(0.1, 0.8), plot.title = element_text(hjust=0.5),
#           legend.background = element_rect(color="black"))
#   return(p1)
# }

# sadid_plot_race_simplified <- function(outAsian, outBlack, outHispanic, outWhite, outOthers){
#   temp <- broom::tidy(outAsian) |> mutate(race_simplified="Asian") |> bind_cols(confint(outAsian) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`)) |> 
#           bind_rows(broom::tidy(outBlack) |> mutate(race_simplified="Black") |> bind_cols(confint(outBlack) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
#           bind_rows(broom::tidy(outWhite) |> mutate(race_simplified="White") |> bind_cols(confint(outWhite) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
#           bind_rows(broom::tidy(outHispanic) |> mutate(race_simplified="Hispanic") |> bind_cols(confint(outHispanic) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
#           bind_rows(broom::tidy(outOthers) |> mutate(race_simplified="Others") |> bind_cols(confint(outOthers) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
#     mutate(period = str_remove(term, "YEAR::") |> as.numeric()) |> 
#     filter(!is.na(period)) |> 
#     mutate(color = as.factor(as.integer(period>=0)))

#   temp <- temp |> mutate(push = case_when(
#     race_simplified=="Asian" ~ -0.2,
#     race_simplified=="Black" ~ -0.1,
#     race_simplified=="Hispanic" ~ 0.1,
#     race_simplified=="Others" ~ 0.2,
#     .default = 0
#   ))

#   p1 <- ggplot(temp |> mutate(race_simplified = as.factor(race_simplified)), aes(x=period + push)) +
#     geom_hline(aes(yintercept=0), linetype="dashed") +
#     geom_errorbar(aes(ymin=lb, ymax=ub, color = race_simplified), width = 0.2) +
#     geom_point(aes(y = estimate, color = race_simplified)) +
#     scale_x_continuous(breaks = filter(temp, race_simplified=="Asian")$period, labels = filter(temp, race_simplified=="Asian")$period, name=NULL) +
#     scale_y_continuous(name="Average Effect(std. dev.)") +
#     # scale_color_discrete(name="income quartile", labels=c("1Q", "2Q", "3Q", "4Q")) +
#     theme_bw() +
#     labs(title="Event Study with Sun and Abraham(2021)") +
#     theme(legend.position = c(0.1, 0.8), plot.title = element_text(hjust=0.5),
#           legend.background = element_rect(color="black"))
#   return(p1)
# }
