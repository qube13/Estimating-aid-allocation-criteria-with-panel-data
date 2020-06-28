# | Load Packages  ---------------------------------------------------------------------------------

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
library(lfe)

# | Load Data  -------------------------------------------------------------------------------------

# default language is english
new_cache <- wbcache()

# Population, total
start_date <- 1982
end_date <- 2020

# GNI Atlas Method
wb_data <- wb(indicator = c("DT.ODA.ODAT.PC.ZS", "SP.DYN.IMRT.IN", "NY.GNP.PCAP.CD", "SP.POP.TOTL"), startdate = start_date, enddate = end_date, country = "countries_only", return_wide = TRUE)

wdr_1991 <- read_excel("data/wdr91_and_wt91_data.xlsx")

#load Civil rights data 
freehouse_data <- download_fh(url = "https://freedomhouse.org/sites/default/files/2020-02/2020_Country_and_Territory_Ratings_and_Statuses_FIW1973-2020.xlsx", 
                              include_territories = TRUE, verbose = FALSE)

#| Data transformation  ---------------------------------------------------------------------------

#data reconstruction from paper (1984-1989, listed in at least 2 years of WDR report)
wdr_1991$`1983` <- NULL
wdr_1991_clean <- wdr_1991[rowSums(!is.na(wdr_1991)) > 1, ]

# reduce wb_data to only include countries from World Development Report 1991
wb_data <- wb_data[wb_data$country %in%  wdr_1991_clean$Country, ]

# Check if countries of WDR are not in WB Data
setdiff(wdr_1991_clean$Country, unique(wb_data$country))

# Check which countries are not in freehouse data and rename them
ordered(setdiff(unique(wb_data$country),unique(freehouse_data$fh_country)))

ordered(setdiff(unique(freehouse_data$fh_country),unique(wb_data$country)))

freehouse_data$fh_country[freehouse_data$fh_country == "Bahamas"] <- "Bahamas, The"
freehouse_data$fh_country[freehouse_data$fh_country == "Brunei"] <- "Brunei Darussalam"
freehouse_data$fh_country[freehouse_data$fh_country == "Congo (Brazzaville)"] <- "Congo, Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "Congo (Kinshasa)"] <- "Congo, Dem. Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "Egypt"] <- "Egypt, Arab Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "Iran"] <- "Iran, Islamic Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "South Korea"] <- "Korea, Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "Egypt"] <- "Egypt, Arab Rep."
freehouse_data$fh_country[freehouse_data$fh_country == "Laos"] <- "Lao PDR"
freehouse_data$fh_country[freehouse_data$fh_country == "Syria"] <- "Syrian Arab Republic"
freehouse_data$fh_country[freehouse_data$fh_country == "Venezuela"] <- "Venezuela, RB"
freehouse_data$fh_country[freehouse_data$fh_country == "Hong Kong"] <- "Hong Kong SAR, China"

#2 different interpretations of civil rights data transformation in the paper
freehouse_data$fh_inverse = 1 / freehouse_data$fh_total
# +1 because otherwise log(0)=-Inf in a later transformation
freehouse_data$fh_total_reversed = freehouse_data$fh_total_reversed + 1

# Check if all renames worked
ordered(setdiff(unique(wb_data$country),unique(freehouse_data$fh_country)))

total_data <- merge(wb_data, freehouse_data[,c("fh_country","year", "fh_total_reversed", "fh_inverse")], by.x = c("country","date"), by.y = c("fh_country","year"))

total_data$iso2c <- NULL
total_data$iso3c <- NULL
colnames(total_data) <- c("country", "year", "NET_ODA", "GNP_PCAP", "INF_MORT", "POP", "RIGHTS_REV", "RIGHTS_INV")

summary(total_data)

#write_csv(total_data, "data/wb20_1984-1989/raw_data.csv")

# Filter out relevant years (Start =1982, cause for data trans needed)
calc_start <- 1982
calc_end <- 1989

calc_years <- c(calc_start:calc_end)

calc_data <- total_data[total_data$year %in%  calc_years, ]

# to not throw out countries that have neg or NAn in these years since not important for model
calc_data$NET_ODA[calc_data$year == calc_start] = 1
calc_data$NET_ODA[calc_data$year == calc_start+1] = 1

# remove countries with NA values
countries_without_NA <- setdiff(unique(calc_data$country), unique(calc_data[rowSums(is.na(calc_data)) > 0,]$country))
length(countries_without_NA)

countries_with_all_entries <- calc_data %>% group_by(country) %>% count() %>% data.frame() %>% .[.$n == calc_end - calc_start + 1,] %>% .$country
length(countries_with_all_entries)

countries_without_neg_vals <- setdiff(unique(calc_data$country), unique(calc_data[calc_data$NET_ODA < 0, ]$country))
length(countries_without_neg_vals)

calc_data <- calc_data[calc_data$country %in%  countries_without_NA, ]
calc_data <- calc_data[calc_data$country %in%  countries_with_all_entries, ]
calc_data <- calc_data[calc_data$country %in%  countries_without_neg_vals, ]

length(unique(calc_data$country))

print(unique(calc_data$country))

summary(calc_data)
 
# Change data typ to panel data
# Calculate Lags and normalize

panel_data <- calc_data  %>% pdata.frame(index = c("country", "year"), drop.index = FALSE, row.names = TRUE)

panel_data$GNP_AVG_T2 <- panel_data$GNP_PCAP %>% lag(0:2) %>% data.frame() %>%  transmute((X1 + X2) / 2) %>% pull()
panel_data$INF_MORT_T2 <- panel_data$INF_MORT %>% lag(0:2) %>% data.frame() %>%  transmute((X1 + X2) / 2) %>% pull()

normalize_base <- panel_data %>%
  group_by(year) %>%
  mutate_at(vars(NET_ODA, POP, GNP_AVG_T2, INF_MORT_T2, RIGHTS_REV, RIGHTS_INV), funs(as.vector(mean(.)))) %>% pdata.frame(index = c("country", "year"), drop.index = FALSE, row.names = TRUE)


final_data <- (panel_data / normalize_base) %>% .[c("NET_ODA", "POP", "GNP_AVG_T2", "INF_MORT_T2", "RIGHTS_REV", "RIGHTS_INV")] %>% .[complete.cases(.),] %>% log()

final_data$country <- panel_data %>% .[complete.cases(.),] %>% .$country
final_data$year <- panel_data %>% .[complete.cases(.),] %>% .$year

final_data <- final_data %>% pdata.frame(index = c("country", "year"), drop.index = FALSE, row.names = TRUE)

colnames(final_data) <- c("NET_ODA", "POP","GNP_PCAP", "INF_MORT","RIGHTS_REV","RIGHTS_INV","country","year")

# export csv for use in other scripts
#write_csv(final_data, "data/wb20_1984-1989/final_data.csv")

#  Model Estimation  ===============================================================================

# | One-Way Fixed Effect Estimation  ---------------------------------------------------------------
model_time_effect <- felm(NET_ODA ~ GNP_PCAP + INF_MORT + RIGHTS_INV + POP | year, data = final_data)
summary(model_time_effect)


# | Two-Way Fixed Effect Estimation  ---------------------------------------------------------------

model_two_ways <- felm(NET_ODA ~ GNP_PCAP + INF_MORT + RIGHTS_INV + POP | country + year, data = final_data)
summary(model_two_ways)


#  Create Figures using Stargazer ==================================================================

all.results = list(model_time_effect, model_two_ways)
stargazer(all.results, type = "html", out="results/wb20_1984-1989_model.html",
          column.labels     = c("One-way FE (time)", "Two-way FE"),
          title             = "Results from world bank data 2020",
          digits            = 3 , digits.extra = 1)

