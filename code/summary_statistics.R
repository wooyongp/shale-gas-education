library(tidyverse)
library(data.table)
library(haven)
library(did)
library(ipumsr)
library(modi)
library(plot.matrix)
theme_set(theme_bw())

data <- arrow::read_parquet("data/ACS_2000-2014.parquet")

# shale_gas_puma <- read_rds("data/shale_gas_puma.rds")
# rm(shale_gas_puma)

treatlist <- c(1, 5, 22, 38, 40, 42, 48, 54)
data <- data |> mutate(treated = if_else(STATEFIP %in% treatlist, 1, 0))

treat_time <- tibble(statefip = treatlist, treat_year = c(2008, 2006, 2008, 2007, 2006, 2008,2005, 2008))

# schlyrs function
educD_to_schlyrs <- function(educD){
  dplyr::case_when(
    educD == 1 ~ NA, educD <= 12 ~ 0, educD %in% 13:14 ~ 1,
    educD == 15 ~ 2, educD == 16 ~ 3, educD == 17 ~ 4,
    educD %in% 20:22 ~ 5, educD == 23 ~ 6, educD %in% 24:25 ~ 7,
    educD == 26 ~ 8, educD == 30 ~ 9, educD == 40 ~ 10,
    educD == 50 ~ 11, educD %in% 60:65 ~ 12, educD %in% 70:71 ~ 13,
    educD %in% 80:83 ~ 14, educD == 90 ~ 15, educD %in% 100:101 ~ 16,
    educD == 110 ~ 17, educD == 111 ~ 18, educD == 112 ~ 19,
    educD %in% 113:115 ~ 20, educD == 116 ~ 24, educD == 999 ~ NA, .default = NA) }

data <- data |> left_join(treat_time, by = c("STATEFIP" = "statefip"))

data <- data |> mutate(school_years = educD_to_schlyrs(EDUCD)) 

data <- data |> mutate(state = as.character(ipumsr::as_factor(STATEFIP)))

data <- data |> mutate(oil_and_gas_industry = as.integer(str_detect(INDNAICS, "^21")))

data <- data |> filter(STATEFIP!= 15, STATEFIP!=2, STATEFIP!=72)

data <- data |> filter(STATEFIP!= 8, STATEFIP!=56)

data <- data |> filter(YEAR<=2010)

data <- data |> mutate(treat_year = coalesce(treat_year, 3000))

data <- data |> mutate(W = as.integer(YEAR>=treat_year))

data <- data |> mutate(id = paste0(SERIAL, SAMPLE, collapse='-'))

data <- data |> mutate(log_INCTOT = log(INCTOT))

data <- data |> mutate(college = as.integer(EDUCD>=62))


## compute income quantile by year
inc_quartile <- data |> 
  filter(INCTOT>0, INCTOT!=9999998, INCTOT!=9999999) |> 
  group_by(YEAR, STATEFIP) |> 
  summarize(INC1Q = weighted.quantile(INCTOT, w=PERWT, prob=0.25),
            INC2Q = weighted.quantile(INCTOT, w=PERWT, prob=0.5),
            INC3Q = weighted.quantile(INCTOT, w=PERWT, prob=0.75)) |> 
  ungroup()

data <- data |> left_join(inc_quartile, by= c("YEAR", "STATEFIP"))

data <- data |> 
  mutate(incomeQ = case_when(INCTOT < INC1Q ~ 1, 
                             INCTOT < INC2Q ~ 2,
                             INCTOT < INC3Q ~ 3, INCTOT >= INC3Q ~ 4))

## with FTOTINC
ftotinc_quartile <- data |> 
  filter(FTOTINC>0, FTOTINC!=9999998, FTOTINC!=9999999, RELATE==1) |> 
  group_by(YEAR, STATEFIP) |> 
  summarize(FINC1Q = weighted.quantile(FTOTINC, w=HHWT, prob=0.25),
            FINC2Q = weighted.quantile(FTOTINC, w=HHWT, prob=0.5),
            FINC3Q = weighted.quantile(FTOTINC, w=HHWT, prob=0.75)) |> 
  ungroup()

data <- data |> left_join(ftotinc_quartile, by= c("YEAR", "STATEFIP"))

data <- data |> 
  mutate(fincomeQ = case_when(FTOTINC < FINC1Q ~ 1, 
                             FTOTINC < FINC2Q ~ 2,
                             FTOTINC < FINC3Q ~ 3, FTOTINC >= FINC3Q ~ 4))
              
              
              
data <- data |> filter(YEAR>=2002)


summary_stat <- function(filtered_data){
  table <- filtered_data |> group_by(treated) |> 
    mutate(INCTOT = if_else(INCTOT == 9999998|INCTOT==9999999|INCTOT<0, NA, INCTOT), 
           FTOTINC = if_else(FTOTINC == 9999998|FTOTINC==9999999|FTOTINC<0, NA, FTOTINC)) |> 
    summarize(
    count.income = sum(!is.na(INCTOT)),
    w.income.mean = weighted.mean(INCTOT, w=PERWT, na.rm=TRUE),
    w.income.se = sqrt(weighted.var(INCTOT, w=PERWT, na.rm=TRUE)),

    count.family.income = sum(!is.na(FTOTINC)),
    w.family.income.mean = weighted.mean(FTOTINC, w=HHWT, na.rm=TRUE),
    w.family.income.se = sqrt(weighted.var(FTOTINC, w=HHWT, na.rm=TRUE)),

    count.schooling = sum(!is.na(school_years)),
    w.schooling.mean = weighted.mean(school_years, w=PERWT, na.rm=TRUE),
    w.schooling.se = sqrt(weighted.var(school_years, w=PERWT, na.rm=TRUE)),    
    
    count.college.enroll = sum(!is.na(college)),
    w.college.enroll.mean = weighted.mean(college, w=PERWT, na.rm=TRUE),
    w.college.enroll.se = sqrt(weighted.var(college, w=PERWT, na.rm=TRUE)),    
    
    total_observation = n(),
    total_pop = sum(PERWT)
    )
    return(table)
}


# summary
summary_5_10 <- summary_stat(data |> filter(AGE>=5, AGE<=10)) |> mutate(group = "[5, 10]")
summary_16_18 <- summary_stat(data |> filter(AGE>=16, AGE<=18)) |> mutate(group = "[16, 18]")
summary_18_24 <- summary_stat(data |> filter(AGE>=18, AGE<=24)) |> mutate(group = "[18, 24]")
summary_overall <- summary_stat(data) |> mutate(group = "total")

summary <- bind_rows(summary_5_10, summary_16_18, summary_18_24, summary_overall)

summary <- summary |> 
  mutate(group = factor(group, levels = c("[5, 10]", "[16, 18]", "[18, 24]", "total"))) |> 
  arrange(treated, group)

summary1 <- summary |> transmute(`Shale Activities` = treated, `Age Group` = group, 
                    `Observation` = total_observation, `Weighted Population` = total_pop, 
                    w.income.mean, w.income.se, count.income,
                    w.family.income.mean, w.family.income.se, count.family.income)

summary2 <- summary |> transmute(`Shale Activities` = treated, `Age Group` = group, 
                    `Observation` = total_observation, `Weighted Population` = total_pop, 
                    w.schooling.mean, w.schooling.se, count.schooling, 
                    w.college.enroll.mean, w.college.enroll.se, count.college.enroll)


summary1 <- summary1 %>%
  mutate(across(where(is.numeric), ~ formatC(., format = "f", big.mark = ",", digits = 2)))

summary2 <- summary2 %>%
  mutate(across(where(is.numeric), ~ formatC(., format = "f", big.mark = ",", digits = 2)))

summary1 |> xtable::xtable(auto=TRUE, caption = "Summary Statistics", label = "tab:sum_stat", align = "lllcccccccc" ) |> print(file = "doc/table/summary_stat1.tex", floating.environment = "sidewaystable")
summary2 |> xtable::xtable(auto=TRUE, caption = "Summary Statistics", label = "tab:sum_stat", align = "lllcccccccc" ) |> print(file = "doc/table/summary_stat2.tex", floating.environment = "sidewaystable")


## Migration
ddi <- read_ipums_ddi('data/usa_00039.xml')
data <- read_ipums_micro(ddi)

treat_time <- tibble::tibble(statefip = treatlist, treat_year = c(2008, 2006, 2008, 2007, 2006, 2008,2005, 2008)) |> as.data.table()


data <- data |> left_join(treat_time, by = c("STATEFIP" = "statefip"))

data <- data |> mutate(school_years = educD_to_schlyrs(EDUCD)) 

data <- data |> mutate(state = as.character(ipumsr::as_factor(STATEFIP)))


data <- data |> filter(STATEFIP!= 15, STATEFIP!=2, STATEFIP!=72)

data <- data |> filter(STATEFIP!= 8, STATEFIP!=56)

data <- data |> filter(YEAR<=2010)

data <- data |> mutate(treat_year = coalesce(treat_year, 3000))

data <- data |> mutate(W = as.integer(YEAR>=treat_year))

data <- data |> mutate(id = paste0(SERIAL, SAMPLE, collapse='-'))

data <- data |> mutate(log_INCTOT = log(INCTOT))

data <- data |> mutate(college = as.integer(EDUCD>=62))


## compute income quantile by year
inc_quartile <- data |> 
  filter(INCTOT>0, INCTOT!=9999998, INCTOT!=9999999) |> 
  group_by(YEAR, STATEFIP) |> 
  summarize(INC1Q = weighted.quantile(INCTOT, w=PERWT, prob=0.25),
            INC2Q = weighted.quantile(INCTOT, w=PERWT, prob=0.5),
            INC3Q = weighted.quantile(INCTOT, w=PERWT, prob=0.75)) |> 
  ungroup()

data <- data |> left_join(inc_quartile, by= c("YEAR", "STATEFIP"))

data <- data |> 
  mutate(incomeQ = case_when(INCTOT < INC1Q ~ 1, 
                             INCTOT < INC2Q ~ 2,
                             INCTOT < INC3Q ~ 3, INCTOT >= INC3Q ~ 4))



data |> filter(!is.na(MIGPLAC1)) |> count(YEAR)

data <- data |> mutate(
  from_treat_to_control = if_else(!(STATEFIP %in% treatlist) & MIGPLAC1 %in% treatlist & MIGPLAC1!=0, 1, 0),
  from_control_to_treat = if_else((STATEFIP %in% treatlist) & !(MIGPLAC1 %in% treatlist) & MIGPLAC1!=0, 1, 0)
)

df <- data |> filter(STATEFIP %in% treatlist) |> 
  group_by(STATEFIP, YEAR) |> 
  summarize(avg_from_control_to_treat = weighted.mean(from_control_to_treat, w=PERWT)*100) |> 
  ungroup()

df <- df |> left_join(treat_time, by = c("STATEFIP" = "statefip"))

df <- df |> mutate(state_name = as_factor(STATEFIP))

ggplot(df |> filter(YEAR>=2002), aes(YEAR, avg_from_control_to_treat)) +
  geom_line() +
  geom_vline(aes(xintercept=treat_year), color="red", linetype = "dashed") +
  scale_y_continuous(name = "population ratio", labels = scales::label_comma(suffix="%"))+
  facet_wrap(~state_name, scales="free_y")
ggsave("doc/figures/pop_ratio_from_outside.png", width=20, height=15, unit="cm")

temp <- data |> filter(STATEFIP %in% treatlist) |> 
  group_by(incomeQ, STATEFIP, YEAR) |> 
  summarize(avg_from_control_to_treat = weighted.mean(from_control_to_treat, w=PERWT)*100) |> 
  left_join(treat_time, by = c("STATEFIP" = "statefip")) |> 
  filter(YEAR==treat_year) |> ungroup() |> 
  rename(reference_ratio = avg_from_control_to_treat) |> 
  select(-treat_year, -YEAR)

df <- data |> filter(STATEFIP %in% treatlist) |> 
  group_by(incomeQ, STATEFIP, YEAR) |> 
  summarize(avg_from_control_to_treat = weighted.mean(from_control_to_treat, w=PERWT)*100) |> 
  ungroup() |> 
  left_join(temp, by = c("incomeQ", "STATEFIP")) |> 
  arrange(STATEFIP, incomeQ, YEAR) |> 
  group_by(STATEFIP, incomeQ) |> 
  mutate(avg_from_control_to_treatN = avg_from_control_to_treat/reference_ratio) |> 
  ungroup()

df <- df |> left_join(treat_time, by = c("STATEFIP" = "statefip"))

df <- df |> mutate(state_name = as_factor(STATEFIP))



ggplot(df |> filter(YEAR>=2002, state_name %in% c("North Dakota", "Oklahoma")) |> 
  mutate(incomeQ=factor(incomeQ)), aes(YEAR, avg_from_control_to_treatN)) +
  geom_line(aes(color = incomeQ, group = incomeQ)) +
  geom_vline(aes(xintercept=treat_year), color="red", linetype = "dashed") +
  scale_y_continuous(name = "normalized population ratio") +
  scale_color_brewer(palette = "Spectral") +
  facet_wrap(~state_name, scales="free_y")
ggsave("doc/figures/pop_ratio_from_outside - by income quartile.png", width=20, height=15, unit="cm")

