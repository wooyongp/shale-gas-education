## Gini Coefficient by state and year
library(tidyverse)
library(data.table)
library(haven)
library(ipumsr)
library(DescTools)

data <- arrow::read_parquet("data/ACS_2000-2014.parquet") |> as.data.table()

data <- data[INCTOT>=0 & INCTOT !=9999998 & INCTOT !=9999999]

gini_by_state_year <- data[, .(gini = DescTools::Gini(INCTOT, weights = PERWT), pop = sum(PERWT)), .(STATEFIP, YEAR)]

gini_by_state_year <- gini_by_state_year[!(STATEFIP %in% c(2, 15, 72))]

shale_gas_puma <- read_rds("data/shale_gas_puma.rds")
treatlist <- distinct(shale_gas_puma, STATEFIP)$STATEFIP
rm(shale_gas_puma)

gini_by_state_year[, treated := if_else(STATEFIP %in% treatlist, 1, 0)]

gini_by_shale_year <- gini_by_state_year[, .(avg_gini = weighted.mean(gini, pop)), by=.(treated, YEAR)]

gini_by_shale_year[order(YEAR), `:=`(avg_giniN = avg_gini/nth(avg_gini, 6)), .(treated)]

gini_by_shale_year |> 
  ggplot(aes(YEAR, avg_giniN, color=factor(treated))) +
  geom_hline(aes(yintercept=1), color = 'blue', linetype='dashed') +
  geom_vline(aes(xintercept=2005), color = 'blue', linetype='dashed') +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = NULL) +
  scale_color_discrete(name = NULL, labels=c("non-shale states", "shale states")) +
  labs(y = "Gini Coefficient(normalized to 2005)") +
  theme(legend.position = c(0.85,0.15), legend.background = element_rect(color='black'))
ggsave("doc/figures/gini.png", width=16, height=9, unit="cm")

ggplot(gini_by_state_year, aes(x=YEAR, y=gini, color = factor(treated), group =STATEFIP)) +
  geom_line()
