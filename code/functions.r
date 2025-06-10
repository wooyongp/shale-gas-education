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

  temp <- temp |> mutate(push = if_else(quartile<=2, if_else(quartile==1, -quartile*0.2, -quartile*0.05), quartile*0.1))

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

  temp <- temp |> mutate(push = if_else(quartile<=2, if_else(quartile==1, -quartile*0.2, -quartile*0.05), quartile*0.1))

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
