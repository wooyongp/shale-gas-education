library(tidyverse)
library(data.table)
library(haven)
library(Synth)
library(SCtools)
library(ipumsr)
# library(causalTree)
# library(grf)

# load data
data <- arrow::read_parquet("data/ACS_2000-2014.parquet")
shale_gas_puma <- read_rds("data/shale_gas_puma.rds")

# filter out adjacent shale states
shale_gas_puma |> distinct(STATEFIP)
nrow(data)
data <- data |>
    # Calibrate comparison
    filter((STATEFIP<60), STATEFIP!=15, STATEFIP!=2) 
nrow(data)



# check if filter done correctly
data |> filter(STATEFIP ==48) |> select(YEAR, SAMPLE, STATEFIP)
data |> filter(STATEFIP ==2) |> select(YEAR, SAMPLE, STATEFIP)

# donor pool
donor <-c(4, 6, 9, 10, 11, 12, 13, 16, 17, 18, 19, 20, 21, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 39, 41, 44, 45, 46, 47, 49, 50, 51, 53, 55)


# education to school years
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


# Age composition by State and PUMA
age_comp <- data |> 
    mutate(age_group = case_when(AGE <18 ~ 1, AGE %in% 18:39 ~ 2, AGE>=40 ~3)) |> 
    group_by(YEAR, STATEFIP, PUMA, age_group, SEX) |> 
    summarize(pop = sum(PERWT)) |> ungroup()

# filter out for age
# data <- data |> filter(AGE >=18, AGE < 25) 

data |> summary()

data <- data |> mutate(INCTOT = if_else(INCTOT <0| INCTOT %in% 9999998:9999999, NA, INCTOT),
                        EMPSTAT = if_else(EMPSTAT==9, NA, EMPSTAT),
                        HISPAN = if_else(HISPAN==9, NA, HISPAN)
                        ) |> 
                 mutate(employed = if_else(EMPSTAT==1, 1, 0), 
                        LF = if_else(EMPSTAT !=3, 1, 0), 
                        white = if_else(RACE==1, 1, 0),
                        asian = if_else(RACE %in% 4:6, 1, 0),
                        hispanic = if_else(HISPAN>0, 1, 0),
                        black = if_else(RACE==2, 1, 0),
                        college_student = if_else(AGE>=18 & AGE <25, 1, 0))

data <- data |> mutate(college = as.integer(EDUCD>=62))

age_ratio <- age_comp |> group_by(YEAR, STATEFIP, age_group) |> summarize(pop = sum(pop)) |> 
    arrange(YEAR, STATEFIP, age_group) |> 
    summarize(youth_ratio = nth(pop, 1)/ sum(pop), elderly_ratio = nth(pop,3)/ sum(pop)) |> ungroup()

df <- data |> mutate(school_years = educD_to_schlyrs(EDUCD)) |>
    group_by(SEX,YEAR, STATEFIP)  |> 
    summarize(avg_college_rate = weighted.mean(college_student*college, w=PERWT, na.rm=TRUE),
              pop = sum(PERWT),
              avg_income = weighted.mean(INCTOT, w = PERWT, na.rm = TRUE),   # This na.rm only excludes negative income from computing avg
              avg_employment = weighted.mean(employed, w = PERWT),
              labor_force = weighted.mean(LF, w=PERWT),
              white_ratio = weighted.mean(white, w=PERWT),
              black_ratio = weighted.mean(black, w=PERWT),
              asian_ratio = weighted.mean(asian, w=PERWT),
              hispanic_ratio = weighted.mean(hispanic, w=PERWT),
              ) |> ungroup() |> 
    left_join(age_ratio, by=c("STATEFIP", "YEAR"))


  df_male <- df |> filter(SEX==1)
  df_female <- df |> filter(SEX==2)

## Education related ----


dataprep_out <- Synth::dataprep(data.frame(df_male),
                predictors = c("white_ratio", "black_ratio", "asian_ratio", "hispanic_ratio", "youth_ratio", "pop"),
                unit.variable = "STATEFIP", time.variable = "YEAR",
                time.predictors.prior = 2000:2004,
                dependent = "avg_college_rate",
                treatment.identifier = 48,
                controls.identifier = donor,
                time.optimize.ssr = 2000:2004,
                time.plot = 2000:2010)

synth_out <- synth(data.prep.obj = dataprep_out)

png("doc/figures/scm/fig3_v4.png", width = 600, height = 800)
path.plot(synth_out, dataprep_out, Main = "Male Education on Texas Shale Gas Shock")
dev.off()

tictoc::tic()
placebos <- generate.placebos(dataprep_out, synth_out)
tictoc::toc()

png("doc/figures/scm/fig3-placebo_v4.png", width = 600, height = 800)
plot_placebos(placebos)
dev.off()

png("doc/figures/scm/fig3-mspe_v4.png", width = 600, height = 800)
mspe.plot(placebos, plot.hist = TRUE)
dev.off()


dataprep_out <- Synth::dataprep(data.frame(df_female),
                predictors = c("white_ratio", "black_ratio", "asian_ratio", "hispanic_ratio", "youth_ratio","pop"),
                unit.variable = "STATEFIP", time.variable = "YEAR",
                time.predictors.prior = 2000:2004,
                dependent = "avg_college_rate",
                treatment.identifier = 48,
                controls.identifier = donor,
                time.optimize.ssr = 2000:2004,
                time.plot = 2000:2010)

synth_out <- synth(data.prep.obj = dataprep_out)

png("doc/figures/scm/fig4_v4.png", width = 600, height = 800)
path.plot(synth_out, dataprep_out, Main = "Female Education on Texas Shale Gas Shock")
dev.off()

tictoc::tic()
placebos <- generate.placebos(dataprep_out, synth_out)
tictoc::toc()

png("doc/figures/scm/fig4-placebo_v4.png", width = 600, height = 800)
plot_placebos(placebos)
dev.off()

png("doc/figures/scm/fig4-mspe_v4.png", width = 600, height = 800)
mspe.plot(placebos, plot.hist = TRUE)
dev.off()


