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
library(lfe) 

# | Load Data  -------------------------------------------------------------------------------------

# default language is english
new_cache_wb <- wbcache()

# Population, total
start_year <- 1982
end_year <- 2018

# GNI Atlas Method
wb_data_current <- wb(indicator = c("DT.ODA.ODAT.PC.ZS", "SP.DYN.IMRT.IN", "NY.GNP.PCAP.CD", "SP.POP.TOTL"), startdate = start_year, enddate = end_year, return_wide = TRUE)


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

write_csv(total_data_current, "data/time_sensitivity_raw_data.csv")

time_coeffs_df <- data.frame()
two_ways_coeffs_df <- data.frame()

time_range <- 7
time_frame <- end_year - start_year - time_range +1

for (i in 1:time_frame){
  calc_start <- start_year + i-1
  calc_end <- calc_start + time_range 
  
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
  
  # if (i == 1){
  #   countries <- unique(calc_data_current$country)
  # }
  # else {
  #   countries <-  intersect(unique(calc_data_current$country), countries)
  # }
  # 
  # calc_data_current <- calc_data_current[calc_data_current$country %in%  all_countries, ]
  # 
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
  
  #  Model Estimation  ===============================================================================
  
  # | One-Way Fixed Effect Estimation  ---------------------------------------------------------------
  
  model_time_current <- felm(NET_ODA ~ GNP_PCAP + INF_MORT + RIGHTS_INV + POP | year, data = final_data_wb_current)
  model_time_current.coeffs <- summary(model_time_current)[["coefficients"]]
  
  model_time_current.coeffs <- rbind(data.frame(),model_time_current.coeffs)
  model_time_current.coeffs["index_name"] <- rownames(model_time_current.coeffs)
  model_time_current.coeffs["end_year"] <- calc_end
  time_coeffs_df <- rbind(time_coeffs_df,model_time_current.coeffs)
  
  
  # | Two-Way Fixed Effect Estimation  ---------------------------------------------------------------
  
  model_two_ways_current <- felm(NET_ODA ~ GNP_PCAP + INF_MORT + RIGHTS_INV + POP | country + year, data = final_data_wb_current)
  model_two_ways_current.coeffs <- summary(model_two_ways_current)[["coefficients"]]
  
  model_two_ways_current.coeffs <- rbind(data.frame(),model_two_ways_current.coeffs)
  model_two_ways_current.coeffs["index_name"] <- rownames(model_two_ways_current.coeffs)
  model_two_ways_current.coeffs["end_year"] <- calc_end
  two_ways_coeffs_df <- rbind(two_ways_coeffs_df,model_two_ways_current.coeffs)
}

trumbull_model <- read_delim("data/trumbull_wall_model.csv", delim=";")

colnames(time_coeffs_df) <- c("Estimate", "StdError", "tValue", "PR", "index_name", "year")
colnames(two_ways_coeffs_df) <- c("Estimate", "StdError", "tValue", "PR", "index_name", "year")


time_coeffs_df$Significance[time_coeffs_df$PR > 0.1] = 0
time_coeffs_df$Significance[time_coeffs_df$PR <= 0.1 & time_coeffs_df$PR > 0.05 ] = 1
time_coeffs_df$Significance[time_coeffs_df$PR <= 0.05 & time_coeffs_df$PR > 0.01 ] = 2
time_coeffs_df$Significance[time_coeffs_df$PR <= 0.01 ] = 3


two_ways_coeffs_df$Significance[two_ways_coeffs_df$PR > 0.1] = 0
two_ways_coeffs_df$Significance[two_ways_coeffs_df$PR <= 0.1 & two_ways_coeffs_df$PR > 0.05 ] = 1
two_ways_coeffs_df$Significance[two_ways_coeffs_df$PR <= 0.05 & two_ways_coeffs_df$PR > 0.01 ] = 2
two_ways_coeffs_df$Significance[two_ways_coeffs_df$PR <= 0.01 ] = 3

two_ways_coeffs_df$Model <- "TwoWay"
time_coeffs_df$Model <- "OneWay"

plot_df <- rbind(two_ways_coeffs_df, time_coeffs_df)

plot <- ggplot(data=plot_df, aes(x=year, y=Estimate, size=Significance, color=Model)) +
  geom_point(alpha=0.6)+
  geom_hline(data=trumbull_model, aes(yintercept=Time),linetype="dashed", color="#009682")+
  geom_hline(data=trumbull_model, aes(yintercept=TwoWays),linetype="dashed", color="#4664AA")+
  facet_wrap(~index_name,nrow = 4,scales = "free_y", strip.position="right")+
  scale_colour_manual(values=c("#009682","#4664AA"))
  

ggsave("results/year_sensitivity_analysis.jpg",plot=plot, width=8, height=5, dpi=400)

