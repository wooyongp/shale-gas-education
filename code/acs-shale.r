library(tidyverse)
library(data.table)
library(haven)
library(did)
library(ipumsr)
library(modi)
library(plot.matrix)
theme_set(theme_bw())
source("code/functions.r")

data <- arrow::read_parquet("data/ACS_2000-2014.parquet")

# shale_gas_puma <- read_rds("data/shale_gas_puma.rds")
# rm(shale_gas_puma)

treatlist <- c(1, 5, 22, 38, 40, 42, 48, 54)
data <- data |> mutate(treated = if_else(STATEFIP %in% treatlist, 1, 0))

treat_time <- tibble(statefip = treatlist, treat_year = c(2008, 2006, 2008, 2007, 2006, 2008,2005, 2008))

# schlyrs function
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



shale_by_states <- readxl::read_excel("data/EIA shale gas production(State).xlsx", sheet="Sheet1", skip=1)

shale_by_states <- shale_by_states |> 
  pivot_longer(cols = 2:36, names_to = "state", values_to = "amount") |> 
  mutate(YEAR = lubridate::year(YEAR))

ggplot(shale_by_states, aes(YEAR, amount)) +
  ggrepel::geom_label_repel(aes(2010, amount, label=state), data = shale_by_states |> filter(YEAR==2010) |> slice_max(order_by=amount, n=7)) +
  geom_line(aes(color=state)) +
  scale_y_continuous(name = "Amount of Production") +
  theme(legend.position = "None")
ggsave("doc/figures/EIA_shale_production.png", width=30, height=20, units="cm")


# control wrt similarity in characteristics
balance <- data |> mutate(group = if_else(treated==1, "treated", as.character(STATEFIP)))

balance <- balance |> filter(INCTOT<0, !(INCTOT %in% 9999998:9999999), !is.na(school_years), YEAR<2005) |> 
  group_by(group) |> 
  summarize(avg_edu = weighted.mean(school_years, w=PERWT), 
            avg_income = weighted.mean(INCTOT, w=PERWT),
            male_share = weighted.mean(as.numeric(SEX==1), w=PERWT),
            white_share = weighted.mean(as.numeric(RACE==1), w=PERWT),
            black_share = weighted.mean(as.numeric(RACE==2), w=PERWT),
            asian_share = weighted.mean(as.numeric(RACE %in% 4:6), w=PERWT),
            hispanic_share = weighted.mean(as.numeric(HISPAN>0), w=PERWT),
            avg_age = weighted.mean(AGE, w=PERWT)
            )


balance <- nbpMatching::gendistance(as.data.frame(balance), idcol=1)$dist 
balance <- as.data.table(balance)

b <- colnames(balance)

balance <- balance[,.(treated)]

balance[, name := b]

b <- balance[order(treated, decreasing =TRUE)] |> as_tibble() |> transmute(statefip = name, mahalanobis = treated) |> 
  head(10) |> xtable::xtable()

print(b, type = "latex", file="doc/table/mahalanobis_distance.txt")

good_comparison <- c(11, 6, 35, 33, 9)

# data <- data |> filter(treated==1|STATEFIP %in% c(11,6,35,33,9))


##  EDA----

df <- data |> 
  group_by(treated, YEAR, SEX) |> 
    summarize(avg_oil = weighted.mean(oil_and_gas_industry, w=PERWT)) |> 
      ungroup()
    
  ggplot(df |> mutate(SEX = if_else(SEX==1, "male", "female")), aes(YEAR, avg_oil, fill=factor(treated))) +
      geom_col(position="dodge") +
        scale_y_continuous(labels=scales::label_number(scale=100, suffix = "%")) +
          scale_fill_discrete(name= NULL, labels = c("non shale states", "shale states")) +
            facet_wrap(~SEX, scales = "free_y") + 
              labs(y= "Ratio of Working Population in Oil and Gas Production(%)") +
                theme(legend.position = 'bottom')
              ggsave("doc/figures/population_ratio_in_oil_production.png")
              
              
data <- data |> filter(YEAR>=2002)
            

df <- data |> filter(AGE>=18, AGE<25) |> 
  group_by(treated, YEAR, SEX) |> 
  summarize(avg_college = weighted.mean(college, w = PERWT)) |> 
  ungroup()

ggplot(df |> mutate(SEX = if_else(SEX==1, "male", "female")), aes(YEAR, avg_college, color=factor(treated))) +
  geom_line() +
  # scale_y_continuous(labels=scales::label_number(scale=100, suffix = "%")) 
  facet_wrap(~SEX, scales = "free_y") +
  scale_y_continuous(name = "Average College Enrollment") +
  scale_color_discrete(name = NULL, labels = c("non shale states", "shale states")) +
  theme(legend.position = 'bottom')
ggsave("doc/figures/average_college_shares[18, 25).png")


df <- data |> filter(AGE>=16, AGE<=18) |> 
  group_by(treated, YEAR, SEX) |> 
  summarize(attendance_rate = weighted.mean(as.integer(SCHOOL==2), w = PERWT)) |> 
  ungroup()

ggplot(df |> mutate(SEX = if_else(SEX==1, "male", "female")), aes(YEAR, attendance_rate, color=factor(treated))) +
  geom_line() +
  # scale_y_continuous(labels=scales::label_number(scale=100, suffix = "%")) 
  facet_wrap(~SEX, scales = "free_y") +
  scale_y_continuous(name = "School Enrollment Rate") +
  scale_color_discrete(name = NULL, labels = c("non shale states", "shale states")) +
  theme(legend.position = 'bottom')
ggsave("doc/figures/school_enrollment[16, 18].png")

# data_acs <- data |> left_join(shale_by_states, by = c("YEAR", "state"))
# rm(data)

## positive effect on income (Note: twfe biased) ----

df <- data |> filter(INCTOT!=0, AGE>=18, AGE<40, INCTOT!=9999998, INCTOT!=9999999) |> 
  group_by(YEAR, STATEFIP) |> 
  summarize(avg_income = weighted.mean(INCTOT, w=PERWT), 
            log_INCTOT = weighted.mean(log_INCTOT, w=PERWT, na.rm=TRUE),
            total_pop = sum(PERWT),
            treat_year = mean(treat_year)) |> ungroup()

out <- att_gt(
  yname = "log_INCTOT",
  gname = "treat_year",
  idname = "STATEFIP",
  tname = "YEAR",
  xformla = ~1,
  data = df,
  weights = "total_pop",
  control_group = "notyettreated",
  # est_method = "reg",
  cores=4
) |> aggte(type='dynamic')

csdid_plot(broom::tidy(out))
ggsave("doc/figures/did-base/csdid(logINCTOT).png", width=20, height=15, unit="cm")

df <-  data |> filter(INCTOT!=0, AGE>=18, AGE<40, INCTOT!=9999998, INCTOT!=9999999) |> 
  select(log_INCTOT, PERWT, STATEFIP, treat_year, YEAR)



l <- fixest::feols(log_INCTOT ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, df, weights=df$PERWT)

sadid_plot(l)
ggsave("doc/figures/did-base/sadid(logINCTOT).png", width=20, height=15, unit="cm")


## by income quartile


df <- data |> filter(INCTOT!=0, AGE>=18, AGE<40, INCTOT!= 9999998, INCTOT!= 9999999) |> 
  group_by(YEAR, STATEFIP, incomeQ) |> 
  summarize(avg_income = weighted.mean(INCTOT, w=PERWT), 
            log_INCTOT = weighted.mean(log_INCTOT, w=PERWT, na.rm=TRUE),
            total_pop = sum(PERWT),
            treat_year = mean(treat_year)) |> ungroup()

for(i in 1:4){
  v <- att_gt(
    yname = "log_INCTOT",
    gname = "treat_year",
    idname = "STATEFIP",
    tname = "YEAR",
    xformla = ~1,
    data = df |> filter(incomeQ==i),
    weights = "total_pop",
    control_group = "notyettreated",
    # est_method = "reg",
    cores=4
  ) |> aggte(type="dynamic")
  assign(paste0("out", i), v)
  rm(v)
}

csdid_plot_quartile(out1, out2, out3, out4)
ggsave("doc/figures/did-base/csdid(logINCTOT - by income quartile).png", width=20, height=15, unit="cm")

for(i in 1:4){
  v <- fixest::feols(log_INCTOT ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, filter(data, INCTOT!=0,INCTOT!=9999998,INCTOT!=9999999, AGE>=18, AGE<40, incomeQ==i), weights=filter(data, INCTOT!=0, INCTOT!=9999998, INCTOT!=9999999, AGE>=18, AGE<40, incomeQ==i)$PERWT)
  assign(paste0("out", i), v)
  rm(v)
}

sadid_plot_quartile(out1, out2, out3, out4)
ggsave("doc/figures/did-base/sadid(logINCTOT - by income quartile).png", width=20, height=15, units="cm")


## positive effect on FTOTINC (Note: twfe biased) ----

df <- data |> filter(FTOTINC!=0, FTOTINC!=9999998, FTOTINC!=9999999, RELATE==1) |> 
  group_by(YEAR, STATEFIP) |> 
  summarize(avg_income = weighted.mean(FTOTINC, w=HHWT), 
            log_FTOTINC = weighted.mean(log(FTOTINC), w=HHWT, na.rm=TRUE),
            total_pop = sum(HHWT),
            treat_year = mean(treat_year)) |> ungroup()

out <- att_gt(
  yname = "log_FTOTINC",
  gname = "treat_year",
  idname = "STATEFIP",
  tname = "YEAR",
  # xformla = ~1,
  data = df,
  weights = "total_pop",
  control_group = "notyettreated",
  # est_method = "reg",
  cores=4
) |> aggte(type='dynamic')

csdid_plot(broom::tidy(out))
ggsave("doc/figures/did-base/csdid(logFTOTINC).png", width=20, height=15, unit="cm")

df <-  data |> filter(FTOTINC!=0, FTOTINC!=9999998, FTOTINC!=9999999, RELATE==1) |> 
  mutate(log_FTOTINC = log(FTOTINC))

l <- fixest::feols(log_FTOTINC ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, df, weights=df$HHWT)

sadid_plot(l)
ggsave("doc/figures/did-base/sadid(logFTOTINC).png", width=20, height=15, unit="cm")

df <- filter(df, oil_and_gas_industry==1)
l <- fixest::feols(log_FTOTINC ~ sunab(treat_year, YEAR) + school_years| (HISPAN>0) + RACE + STATEFIP + YEAR, df, weights=df$HHWT)

l <- fixest::feols(log_FTOTINC ~ as.numeric(YEAR>=treat_year) + school_years|treated, df, weights = df$HHWT)
fixest::fitstat(l, 'f')

## by income quartile


df <- data |> filter(FTOTINC!=0, FTOTINC!=9999998, FTOTINC!=9999999, RELATE==1) |> 
  mutate(log_FTOTINC = log(FTOTINC)) |> 
  group_by(YEAR, STATEFIP, fincomeQ) |> 
  summarize(log_FTOTINC = weighted.mean(log_FTOTINC, w=HHWT, na.rm=TRUE),
            total_pop = sum(HHWT),
            treat_year = mean(treat_year)) |> ungroup()

for(i in 1:4){
  v <- att_gt(
    yname = "log_FTOTINC",
    gname = "treat_year",
    idname = "STATEFIP",
    tname = "YEAR",
    xformla = ~1,
    data = df |> filter(fincomeQ==i),
    weights = "total_pop",
    control_group = "notyettreated",
    # est_method = "reg",
    cores=4
  ) |> aggte(type="dynamic")
  assign(paste0("out", i), v)
  rm(v)
}

csdid_plot_quartile(out1, out2, out3, out4)
ggsave("doc/figures/did-base/csdid(logFTOTINC - by income quartile).png", width=20, height=15, unit="cm")

for(i in 1:4){
  v <- fixest::feols(log_FTOTINC ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, filter(data |> mutate(log_FTOTINC=log(FTOTINC)), fincomeQ==i, FTOTINC!=0, FTOTINC!=9999998, FTOTINC!=9999999, RELATE==1), weights=filter(data |> mutate(log_FTOTINC=log(FTOTINC)), fincomeQ==i, FTOTINC!=0, FTOTINC!=9999998, FTOTINC!=9999999, RELATE==1)$HHWT)
  assign(paste0("out", i), v)
  rm(v)
}

sadid_plot_quartile(out1, out2, out3, out4)
ggsave("doc/figures/did-base/sadid(logFTOTINC - by income quartile).png", width=20, height=15, units="cm")



## college share----

df <- data |> filter(AGE>=18, AGE<25) |> 
  group_by(YEAR, STATEFIP) |> 
  summarize(avg_college = weighted.mean(college, w=PERWT), 
            # log_INCTOT = weighted.mean(log_INCTOT, w=PERWT, na.rm=TRUE),
            total_pop = sum(PERWT),
            treat_year = mean(treat_year)) |> ungroup()

out <- att_gt(
  yname = "avg_college",
  gname = "treat_year",
  idname = "STATEFIP",
  tname = "YEAR",
  xformla = ~1,
  data = df,
  weights = "total_pop",
  control_group = "notyettreated",
  # est_method = "reg",
  cores=4
)

event_study <- aggte(out, type = "dynamic")
broom::tidy(event_study) |> csdid_plot()
ggsave("doc/figures/did-base/csdid(Average College Share).png", width=20, height=15, unit="cm")

df <-  data |> filter(AGE>=18, AGE<25) |> 
  select(school_years, PERWT, STATEFIP, treat_year, YEAR)


# temp <- df |> slice_sample(n=60, by=c("STATEFIP", "YEAR"))

l <- fixest::feols(school_years ~ sunab(treat_year, YEAR)| STATEFIP + YEAR, df, weights=df$PERWT)
sadid_plot(l)
ggsave("doc/figures/did-base/sadid(college enrollment).png", width=20, height=15, unit="cm")

l <- fixest::feols(school_years ~ sunab(treat_year, YEAR) + FTOTINC + i(RACE) + i(SEX)| STATEFIP + YEAR, filter(data, AGE >=18, AGE<25, FTOTINC>=0, FTOTINC <9999998), weights=filter(data, AGE >=18, AGE<25, FTOTINC>=0, FTOTINC <9999998)$PERWT)

sadid_plot(l)
ggsave("doc/figures/did-base/sadid(college enrollment - controls).png", width=20, height=15, unit="cm")

## by income_quartile
df <- data |> filter(AGE>=18, AGE<25) |> 
  group_by(YEAR, STATEFIP, fincomeQ) |> 
  summarize(avg_college_share = weighted.mean(college, w=PERWT), 
            # log_INCTOT = weighted.mean(log_INCTOT, w=PERWT, na.rm=TRUE),
            total_pop = sum(PERWT),
            treat_year = mean(treat_year)) |> ungroup()

for(i in 1:4){
  v <- att_gt(
    yname = "avg_college_share",
    gname = "treat_year",
    idname = "STATEFIP",
    tname = "YEAR",
    xformla = ~1,
    data = df |> filter(fincomeQ==i),
    weights = "total_pop",
    control_group = "notyettreated",
    # est_method = "reg",
    cores=4
  ) |> aggte(type="dynamic")
  assign(paste0("out", i), v)
  rm(v)
}

csdid_plot_quartile(out1, out2, out3, out4)
ggsave("doc/figures/did-base/csdid(college_share- by income quartile).png", width=20, height=15, unit="cm")


for(i in 1:4){
  v <- fixest::feols(college ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, filter(data,AGE>=18, AGE<25, fincomeQ==i), weights=filter(data,AGE>=18, AGE<25, fincomeQ==i)$PERWT)
  assign(paste0("out", i), v)
  rm(v)
}

sadid_plot_quartile(out1, out2, out3, out4)
ggsave("doc/figures/did-base/sadid(college share - by income quartile).png", width=20, height=15, unit="cm")

# negative effect on old enrollment
df <- data |> filter(AGE>=16, AGE<=18, SCHOOL!=0) |> 
  group_by(YEAR, STATEFIP) |> 
  summarize(enrollment_rate = weighted.mean(as.integer(SCHOOL==2), w=PERWT), 
            # log_INCTOT = weighted.mean(log_INCTOT, w=PERWT, na.rm=TRUE),
            total_pop = sum(PERWT),
            treat_year = mean(treat_year)) |> ungroup()

out <- att_gt(
  yname = "enrollment_rate",
  gname = "treat_year",
  idname = "STATEFIP",
  tname = "YEAR",
  xformla = ~1,
  data = df,
  weights = "total_pop",
  control_group = "notyettreated",
  # est_method = "reg",
  cores=4
)

event_study <- aggte(out, type = "dynamic")
csdid_plot(event_study |> broom::tidy())
ggsave("doc/figures/did-base/sadid(highschool enrollment).png", width=20, height=15, unit="cm")

df <- data |> filter(AGE>=16, AGE<=18, SCHOOL!=0) |> 
  group_by(YEAR, STATEFIP, fincomeQ) |> 
  summarize(enrollment_rate = weighted.mean(as.integer(SCHOOL==2), w=HHWT), 
            # log_INCTOT = weighted.mean(log_INCTOT, w=PERWT, na.rm=TRUE),
            total_pop = sum(HHWT),
            treat_year = mean(treat_year)) |> 
  ngroup()

for(i in 1:4){
  v <- att_gt(
    yname = "enrollment_rate",
    gname = "treat_year",
    idname = "STATEFIP",
    tname = "YEAR",
    xformla = ~1,
    data = df |> filter(fincomeQ==i),
    weights = "total_pop",
    control_group = "notyettreated",
    # est_method = "reg",
    cores=4
  ) |> aggte(type="dynamic")
  assign(paste0("out", i), v)
  rm(v)
}

csdid_plot_quartile(out1, out2, out3, out4)
ggsave("doc/figures/did-base/sadid(highschool enrollment - by income_quartile).png", width=20, height=15, unit="cm")


# sadid
df <-  data |> filter(AGE>=16, AGE<=18) |> mutate(SCHOOL = if_else(SCHOOL==2, 1, if_else(SCHOOL==1, 0, NA)))


# temp <- df |> slice_sample(n=60, by=c("STATEFIP", "YEAR"))
l <- fixest::feols(SCHOOL ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, df, weights=df$PERWT)

sadid_plot(l)
ggsave("doc/figures/did-base/sadid(high school attendance).png", width=20, height=15, unit="cm")

for(i in 1:4){
  v <- fixest::feols(school_years ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, filter(df, AGE>=16, AGE<=18, fincomeQ==i), weights=filter(df, AGE>=16, AGE<=18, fincomeQ==i)$HHWT)
  assign(paste0("out", i), v)
  rm(v)
}

sadid_plot_quartile(out1, out2, out3, out4)
ggsave("doc/figures/did-base/sadid(high school attendance- by income quartile).png", width=20, height=15, unit="cm")




## By sex

# df <- data |> filter(AGE>=16, AGE<=18, SCHOOL!=0) |> 
#   group_by(YEAR, STATEFIP, SEX) |> 
#   summarize(enrollment_rate = weighted.mean(as.integer(SCHOOL==2), w=PERWT), 
#             # log_INCTOT = weighted.mean(log_INCTOT, w=PERWT, na.rm=TRUE),
#             total_pop = sum(PERWT),
#             treat_year = mean(treat_year)) |> ungroup()

# out <- att_gt(
#   yname = "enrollment_rate",
#   gname = "treat_year",
#   idname = "STATEFIP",
#   tname = "YEAR",
#   xformla = ~1,
#   data = df |> filter(SEX==1),
#   weights = "total_pop",
#   control_group = "notyettreated",
#   # est_method = "reg",
#   cores=4
# )

# aggte(out, type = "dynamic") |> ggdid()
# event_study <- aggte(out, type = "dynamic")

# out <- att_gt(
#   yname = "enrollment_rate",
#   gname = "treat_year",
#   idname = "STATEFIP",
#   tname = "YEAR",
#   xformla = ~1,
#   data = df |> filter(SEX==2),
#   weights = "total_pop",
#   control_group = "notyettreated",
#   # est_method = "reg",
#   cores=4
# )

# aggte(out, type = "dynamic") |> ggdid()
# event_study <- aggte(out, type = "dynamic")




# young enrollment
df <- data |> filter( AGE>=5, AGE<=10, SCHOOL!=0) |> 
  group_by(YEAR, STATEFIP) |> 
  summarize(enrollment_rate = weighted.mean(as.integer(SCHOOL==2), w=PERWT), 
            # log_INCTOT = weighted.mean(log_INCTOT, w=PERWT, na.rm=TRUE),
            total_pop = sum(PERWT),
            treat_year = mean(treat_year)) |> ungroup()

out <- att_gt(
  yname = "enrollment_rate",
  gname = "treat_year",
  idname = "STATEFIP",
  tname = "YEAR",
  xformla = ~1,
  data = df,
  weights = "total_pop",
  control_group = "notyettreated",
  # est_method = "reg",
  cores=4
)

csdid_plot(broom::tidy(out |> aggte(type='dynamic')))
ggsave("doc/figures/did-base/csdid(young school attendance).png", width=20, height=15, unit="cm")

 # sadid
df <-  data |> filter(AGE>=5, AGE<=10) |> mutate(SCHOOL = if_else(SCHOOL==2, 1, if_else(SCHOOL==1, 0, NA)))


# temp <- df |> slice_sample(n=60, by=c("STATEFIP", "YEAR"))
l <- fixest::feols(SCHOOL ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, df, weights=df$PERWT)

sadid_plot(l)
ggsave("doc/figures/did-base/sadid(young school attendance).png", width=20, height=15, unit="cm")

for(i in 1:4){
  v <- fixest::feols(SCHOOL ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, filter(df, fincomeQ==i), weights=filter(df, fincomeQ==i)$PERWT)
  assign(paste0("out", i), v)
  rm(v)
}

sadid_plot_quartile(out1, out2, out3, out4)
ggsave("doc/figures/did-base/sadid(young school attendance - by income quartile).png", width=20, height=15, unit="cm")


## ----child's education----


#  # sadid
# df <-  data |> filter(AGE>=6, AGE<12) |> mutate(SCHOOL = if_else(SCHOOL==2, 1, if_else(SCHOOL==1, 0, NA)))


# # temp <- df |> slice_sample(n=60, by=c("STATEFIP", "YEAR"))
# l <- fixest::feols(SCHOOL ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, df, weights=df$PERWT)

# sadid_plot(l)
# ggsave("doc/figures/did-base/sadid(school attendance young).png", width=20, height=15, unit="cm")

# for(i in 1:4){
#   v <- fixest::feols(school_years ~ sunab(treat_year, YEAR) | STATEFIP + YEAR, filter(data, AGE>=6, AGE<12, fincomeQ==i), weights=filter(data, AGE>=6, AGE<12, fincomeQ==i)$HHWT)
#   assign(paste0("out", i), v)
#   rm(v)
# }

# sadid_plot_quartile(out1, out2, out3, out4)
# ggsave("doc/figures/did-base/sadid(school attendance young - by income quartile).png", width=20, height=15, unit="cm")



# df <- data |> filter( AGE>=6, AGE<12, SCHOOL!=0) |> 
#   group_by(YEAR, STATEFIP) |> 
#   summarize(enrollment_rate = weighted.mean(as.integer(SCHOOL==2), w=PERWT), 
#             # log_INCTOT = weighted.mean(log_INCTOT, w=PERWT, na.rm=TRUE),
#             total_pop = sum(PERWT),
#             treat_year = mean(treat_year)) |> ungroup()

# out <- att_gt(
#   yname = "enrollment_rate",
#   gname = "treat_year",
#   idname = "STATEFIP",
#   tname = "YEAR",
#   xformla = ~1,
#   data = df,
#   weights = "total_pop",
#   control_group = "notyettreated",
#   # est_method = "reg",
#   cores=4
# )

# event_study <- aggte(out, type = "dynamic")

# event_study |> broom::tidy() |> csdid_plot()
# ggsave("doc/figures/did-base/csdid(young enrollment).png")


# df <- data |> filter(AGE>=6, AGE<12) |> 
#   mutate(SCHOOL = if_else(SCHOOL==2, 1, if_else(SCHOOL==1, 0, NA))) |> 
#   group_by(YEAR, STATEFIP, fincomeQ) |> 
#   summarize(avg_enrollment = weighted.mean(SCHOOL, w=HHWT), 
#             # log_INCTOT = weighted.mean(log_INCTOT, w=PERWT, na.rm=TRUE),
#             total_pop = sum(HHWT),
#             treat_year = mean(treat_year)) |> ungroup()

# for(i in 1:4){
#   v <- att_gt(
#     yname = "avg_enrollment",
#     gname = "treat_year",
#     idname = "STATEFIP",
#     tname = "YEAR",
#     xformla = ~1,
#     data = df |> filter(fincomeQ==i),
#     weights = "total_pop",
#     control_group = "notyettreated",
#     # est_method = "reg",
#     cores=4
#   ) |> aggte(type="dynamic")
#   assign(paste0("out", i), v)
#   rm(v)
# }

# csdid_plot_quartile(out1, out2, out3, out4)
# ggsave("doc/figures/did-base/csdid(young enrollment - by income quartile).png")
