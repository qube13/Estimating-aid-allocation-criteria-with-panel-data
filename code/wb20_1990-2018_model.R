# | Load Packages  ---------------------------------------------------------------------------------
library(svglite)

library(wbstats)
library(readxl)
library(democracyData)

library(tidyverse)
library(plm)
library(stargazer)
library(reshape)
library(ggplot2)
library(plotly)
library(gridExtra)
library(lfe) #maybe install first

# | Load Data  -------------------------------------------------------------------------------------

# default language is english
new_cache_wb <- wbcache()

# Population, total
start_year <- 1982
end_year <- 2018

# GNI Atlas Method
wb_data_current <- wb(indicator = c("DT.ODA.ODAT.PC.ZS", "SP.DYN.IMRT.IN", "NY.GNP.PCAP.CD", "SP.POP.TOTL"), startdate = start_year, enddate = end_year, country = "countries_only", return_wide = TRUE)


#load Civil rights data 
freehouse_data_original <- download_fh(url = "https://freedomhouse.org/sites/default/files/2020-02/2020_Country_and_Territory_Ratings_and_Statuses_FIW1973-2020.xlsx", 
                              include_territories = TRUE, verbose = FALSE)

#| Data transformation  ---------------------------------------------------------------------------

# filter the relavant years of the freehouse data 
freehouse_data <- freehouse_data_original %>% filter(freehouse_data_original$year >=start_year)

# Check which countries are not in freehouse data and rename them
ordered(setdiff(unique(wb_data_current$country),unique(freehouse_data$fh_country)))

ordered(setdiff(unique(freehouse_data$fh_country),unique(wb_data_current$country)))

freehouse_data$fh_country[freehouse_data$fh_country == "Bahamas"] <- "Bahamas, The"
freehouse_data$fh_country[freehouse_data$fh_country == "Brunei"] <- "Brunei Darussalam"
freehouse_data$fh_country[freehouse_data$fh_country == "Congo (Brazzaville)"] <- "Congo, Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "Congo (Kinshasa)"] <- "Congo, Dem. Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "Egypt"] <- "Egypt, Arab Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "Iran"] <- "Iran, Islamic Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "South Korea"] <- "Korea, Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "Egypt"] <- "Egypt, Arab Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "Kyrgyzstan"] <- "Kyrgyz Republic"
freehouse_data$fh_country[freehouse_data$fh_country == "Laos"] <- "Lao PDR"
freehouse_data$fh_country[freehouse_data$fh_country == "Macao"] <- "Macao SAR, China"
freehouse_data$fh_country[freehouse_data$fh_country == "West Bank and Gaza Strip"] <- "West Bank and Gaza"
freehouse_data$fh_country[freehouse_data$fh_country == "Syria"] <- "Syrian Arab Republic"
freehouse_data$fh_country[freehouse_data$fh_country == "Venezuela"] <- "Venezuela, RB"
freehouse_data$fh_country[freehouse_data$fh_country == "Hong Kong"] <- "Hong Kong SAR, China"
freehouse_data$fh_country[freehouse_data$fh_country == "Russia"] <- "Russian Federation"
freehouse_data$fh_country[freehouse_data$fh_country == "Yemen"] <- "Yemen, Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "The Gambia"] <- "Gambia, The"

#2 different interpretations of civil rights data transformation in the paper
freehouse_data$fh_inverse = 1 / freehouse_data$fh_total
# +1 because otherwise log(0)=-Inf in a later transformation
freehouse_data$fh_total_reversed = freehouse_data$fh_total_reversed + 1

# Check if all renames worked
ordered(setdiff(unique(wb_data_current$country),unique(freehouse_data$fh_country)))

total_data_current <- merge(wb_data_current, freehouse_data[,c("fh_country","year", "fh_total_reversed", "fh_inverse")], by.x = c("country","date"), by.y = c("fh_country","year"))

total_data_current$iso2c <- NULL
total_data_current$iso3c <- NULL
colnames(total_data_current) <- c("country", "year", "NET_ODA", "GNP_PCAP", "INF_MORT", "POP", "RIGHTS_REV", "RIGHTS_INV")

summary(total_data_current)

#  Write Raw data to CSV  ===============================================================================

#write_csv(total_data_current, "data/wb20_1990-2018/raw_data.csv")

#  Start Preprocessing  ===============================================================================

calc_start <- 1988
calc_end <- 2018

calc_data_current <- total_data_current %>% filter(total_data_current$year >=calc_start & total_data_current$year <=calc_end)

calc_data_current$NET_ODA[calc_data_current$year == calc_start] = 1
calc_data_current$NET_ODA[calc_data_current$year == calc_start+1] = 1

countries_without_neg_vals <- setdiff(unique(calc_data_current$country), unique(calc_data_current[calc_data_current$NET_ODA < 0, ]$country))
length(countries_without_neg_vals)

# remove countries with NA values
countries_without_NA <- setdiff(unique(calc_data_current$country), unique(calc_data_current[rowSums(is.na(calc_data_current)) > 0,]$country))
length(countries_without_NA)

countries_with_all_entries <- calc_data_current %>% group_by(country) %>% count() %>% data.frame() %>% .[.$n == calc_end - calc_start + 1,] %>% .$country
length(countries_with_all_entries)



calc_data_current <- calc_data_current[calc_data_current$country %in%  countries_without_NA, ]
calc_data_current <- calc_data_current[calc_data_current$country %in%  countries_with_all_entries, ]
calc_data_current <- calc_data_current[calc_data_current$country %in%  countries_without_neg_vals, ]

length(unique(calc_data_current$country))

print(unique(calc_data_current$country))

summary(calc_data_current)

# Change data typ to panel data
# Calculate Lags and normalize

panel_data_current <- calc_data_current  %>% pdata.frame(index = c("country", "year"), drop.index = FALSE, row.names = TRUE)

panel_data_current$GNP_AVG_T2 <- panel_data_current$GNP_PCAP %>% lag(0:2) %>% data.frame() %>%  transmute((X1 + X2) / 2) %>% pull()
panel_data_current$INF_MORT_T2 <- panel_data_current$INF_MORT %>% lag(0:2) %>% data.frame() %>%  transmute((X1 + X2) / 2) %>% pull()

normalize_base_current <- panel_data_current %>%
  group_by(year) %>%
  mutate_at(vars(NET_ODA, POP, GNP_AVG_T2, INF_MORT_T2, RIGHTS_REV, RIGHTS_INV), funs(as.vector(mean(.)))) %>% pdata.frame(index = c("country", "year"), drop.index = FALSE, row.names = TRUE)


final_data_wb_current <- (panel_data_current / normalize_base_current) %>% .[c("NET_ODA", "POP", "GNP_AVG_T2", "INF_MORT_T2", "RIGHTS_REV", "RIGHTS_INV")] %>% .[complete.cases(.),] %>% log()

final_data_wb_current$country <- panel_data_current %>% .[complete.cases(.),] %>% .$country
final_data_wb_current$year <- panel_data_current %>% .[complete.cases(.),] %>% .$year

colnames(final_data_wb_current) <- c("NET_ODA", "POP", "GNP_PCAP", "INF_MORT", "RIGHTS_REV", "RIGHTS_INV", "country", "year")
final_data_wb_current <- final_data_wb_current %>% pdata.frame(index = c("country", "year"), drop.index = FALSE, row.names = TRUE)

#  Write Final data to CSV  ===============================================================================

#write_csv(final_data_wb_current, "data/wb20_1990-2018/final_data.csv")

#  Model Estimation  ===============================================================================

# | One-Way Fixed Effect Estimation  ---------------------------------------------------------------

model_time_current <- felm(NET_ODA ~ GNP_PCAP + INF_MORT + RIGHTS_INV + POP | year, data = final_data_wb_current)
summary(model_time_current)

# | Two-Way Fixed Effect Estimation  ---------------------------------------------------------------

model_two_ways_current <- felm(NET_ODA ~ GNP_PCAP + INF_MORT + RIGHTS_INV + POP | country + year, data = final_data_wb_current)
summary(model_two_ways_current)


#  Create Figures using Stargazer ==================================================================

all.results = list(model_time_current, model_two_ways_current)
stargazer(all.results, type = "html", out="results/wb20_1990-2018_model.html",
          column.labels     = c("One-way FE (time)","Two-way FE"),
          title             = "Table 2.1: Results",
          digits            = 3 , digits.extra = 1)


