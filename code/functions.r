csdid_plot <- function(broom_table){
  ggplot(broom_table |> mutate(color = as.factor(as.integer(event.time>=0))), aes(x=event.time)) +
    geom_hline(aes(yintercept=0), linetype="dashed") +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color = color), width =0.2) +
    geom_point(aes(y = estimate, color = color)) +
    scale_x_continuous(breaks = broom_table$event.time, labels = broom_table$event.time, name=NULL) +
    scale_y_continuous(name="Average Effect(std. dev.)") +
    scale_color_discrete(name=NULL, labels=c("Pre", "Post")) +
    theme_bw() +
    labs(title="Event Study with Callaway and Sant'anna(2021)") +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5))
}

sadid_plot <- function(l){
  l <- broom::tidy(l) |> bind_cols(confint(l) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))
  l <- mutate(l, period = str_remove(term, "YEAR::") |> as.numeric()) |> 
    filter(!is.na(period)) |> 
    mutate(color = as.factor(as.integer(period>=0)))
  ggplot(l, aes(x=period)) +
    geom_hline(aes(yintercept=0), linetype="dashed") +
    geom_errorbar(aes(ymin=lb, ymax=ub, color = color), width =0.2) +
    geom_point(aes(y = estimate, color = color)) +
    scale_x_continuous(breaks = l$period, labels = l$period, name=NULL) +
    scale_y_continuous(name="Average Effect(std. dev.)") +
    scale_color_discrete(name=NULL, labels=c("Pre", "Post")) +
    theme_bw() +
    labs(title="Event Study with Sun and Abraham(2021)") +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5))
}


csdid_plot_quartile <- function(out1, out2, out3, out4){
  temp <- broom::tidy(out1) |> mutate(quartile=1) |> 
    bind_rows(broom::tidy(out2) |> mutate(quartile=2)) |> 
    bind_rows(broom::tidy(out3) |> mutate(quartile=3)) |> 
    bind_rows(broom::tidy(out4) |> mutate(quartile=4))

  temp <- temp |> mutate(push = if_else(quartile<=2, if_else(quartile==1, -0.15, -0.05), if_else(quartile==3, 0.05, 0.15)))

  p1 <- ggplot(temp |> mutate(quartile = as.factor(quartile)), aes(x=event.time + push)) +
    geom_hline(aes(yintercept=0), linetype="dashed") +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color = quartile), width = 0.2) +
    geom_point(aes(y = estimate, color = quartile)) +
    scale_x_continuous(breaks = broom::tidy(out1)$event.time, labels = broom::tidy(out1)$event.time, name=NULL) +
    scale_y_continuous(name="Average Effect(std. dev.)") +
    scale_color_discrete(name="income quartile", labels=c("1Q", "2Q", "3Q", "4Q")) +
    theme_bw() +
    labs(title="Event Study with Callaway and Sant'anna(2021)") +
    theme(legend.position = c(0.1, 0.8), plot.title = element_text(hjust=0.5),
          legend.background = element_rect(color="black"))
  return(p1)
}


sadid_plot_quartile <- function(out1, out2, out3, out4){
  temp <- broom::tidy(out1) |> mutate(quartile=1) |> bind_cols(confint(out1) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`)) |> 
    bind_rows(broom::tidy(out2) |> mutate(quartile=2) |> bind_cols(confint(out2) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
    bind_rows(broom::tidy(out3) |> mutate(quartile=3) |> bind_cols(confint(out3) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
    bind_rows(broom::tidy(out4) |> mutate(quartile=4) |> bind_cols(confint(out4) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
    mutate(period = str_remove(term, "YEAR::") |> as.numeric()) |> 
    filter(!is.na(period)) |> 
    mutate(color = as.factor(as.integer(period>=0)))

  temp <- temp |> mutate(push = if_else(quartile<=2, if_else(quartile==1, -0.15, -0.05), if_else(quartile==3, 0.05, 0.15)))

  p1 <- ggplot(temp |> mutate(quartile = as.factor(quartile)), aes(x=period + push)) +
    geom_hline(aes(yintercept=0), linetype="dashed") +
    geom_errorbar(aes(ymin=lb, ymax=ub, color = quartile), width = 0.2) +
    geom_point(aes(y = estimate, color = quartile)) +
    scale_x_continuous(breaks = filter(temp, quartile==1)$period, labels = filter(temp, quartile==1)$period, name=NULL) +
    scale_y_continuous(name="Average Effect(std. dev.)") +
    scale_color_discrete(name="income quartile", labels=c("1Q", "2Q", "3Q", "4Q")) +
    theme_bw() +
    labs(title="Event Study with Sun and Abraham(2021)") +
    theme(legend.position = c(0.1, 0.8), plot.title = element_text(hjust=0.5),
          legend.background = element_rect(color="black"))
  return(p1)
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



csdid_plot_race_simplified <- function(outAsian, outBlack, outHispanic, outWhite, outOthers){
  temp <- broom::tidy(outAsian) |> mutate(race_simplified="Asian") |> 
    bind_rows(broom::tidy(outBlack) |> mutate(race_simplified="Black")) |> 
    bind_rows(broom::tidy(outWhite) |> mutate(race_simplified="White")) |> 
    bind_rows(broom::tidy(outHispanic) |> mutate(race_simplified="Hispanic")) |> 
    bind_rows(broom::tidy(outOthers) |> mutate(race_simplified="Others"))

  temp <- temp |> mutate(push = case_when(
    race_simplified=="Asian" ~ -0.2,
    race_simplified=="Black" ~ -0.1,
    race_simplified=="Hispanic" ~ 0.1,
    race_simplified=="Others" ~ 0.2,
    .default = 0
  ))

  p1 <- ggplot(temp |> mutate(race_simplified = as.factor(race_simplified)), aes(x=event.time + push)) +
    geom_hline(aes(yintercept=0), linetype="dashed") +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color = race_simplified), width = 0.2) +
    geom_point(aes(y = estimate, color = race_simplified)) +
    scale_x_continuous(breaks = broom::tidy(outAsian)$event.time, labels = broom::tidy(outAsian)$event.time, name=NULL) +
    scale_y_continuous(name="Average Effect(std. dev.)") +
    theme_bw() +
    labs(title="Event Study with Callaway and Sant'anna(2021)") +
    theme(legend.position = c(0.1, 0.8), plot.title = element_text(hjust=0.5),
          legend.background = element_rect(color="black"))
  return(p1)
}

sadid_plot_race_simplified <- function(outAsian, outBlack, outHispanic, outWhite, outOthers){
  temp <- broom::tidy(outAsian) |> mutate(race_simplified="Asian") |> bind_cols(confint(outAsian) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`)) |> 
          bind_rows(broom::tidy(outBlack) |> mutate(race_simplified="Black") |> bind_cols(confint(outBlack) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
          bind_rows(broom::tidy(outWhite) |> mutate(race_simplified="White") |> bind_cols(confint(outWhite) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
          bind_rows(broom::tidy(outHispanic) |> mutate(race_simplified="Hispanic") |> bind_cols(confint(outHispanic) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
          bind_rows(broom::tidy(outOthers) |> mutate(race_simplified="Others") |> bind_cols(confint(outOthers) |> as_tibble() |> rename(lb = `2.5 %`, ub = `97.5 %`))) |> 
    mutate(period = str_remove(term, "YEAR::") |> as.numeric()) |> 
    filter(!is.na(period)) |> 
    mutate(color = as.factor(as.integer(period>=0)))

  temp <- temp |> mutate(push = case_when(
    race_simplified=="Asian" ~ -0.2,
    race_simplified=="Black" ~ -0.1,
    race_simplified=="Hispanic" ~ 0.1,
    race_simplified=="Others" ~ 0.2,
    .default = 0
  ))

  p1 <- ggplot(temp |> mutate(race_simplified = as.factor(race_simplified)), aes(x=period + push)) +
    geom_hline(aes(yintercept=0), linetype="dashed") +
    geom_errorbar(aes(ymin=lb, ymax=ub, color = race_simplified), width = 0.2) +
    geom_point(aes(y = estimate, color = race_simplified)) +
    scale_x_continuous(breaks = filter(temp, race_simplified=="Asian")$period, labels = filter(temp, race_simplified=="Asian")$period, name=NULL) +
    scale_y_continuous(name="Average Effect(std. dev.)") +
    # scale_color_discrete(name="income quartile", labels=c("1Q", "2Q", "3Q", "4Q")) +
    theme_bw() +
    labs(title="Event Study with Sun and Abraham(2021)") +
    theme(legend.position = c(0.1, 0.8), plot.title = element_text(hjust=0.5),
          legend.background = element_rect(color="black"))
  return(p1)
}
