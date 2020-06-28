library(gtools)
library(tidyverse)
library(plm)
library(stargazer)
library(reshape)
library(ggplot2)
library(plotly)
library(gridExtra)

final_data <- read_csv("data/wt91_1984-1989/final_data.csv")

final_data <- final_data %>% pdata.frame(index = c("country", "year"), drop.index = FALSE, row.names = TRUE)

colnames(final_data)<- c("NET_ODA","POP","GNP_PCAP","INF_MORT","RIGHTS_REV","RIGHTS_INV","country","year" )

all_countries <- as.character(unique(final_data$country))

iterations <- 300
left_outs <- 5

time_coeffs_df <- data.frame()
two_ways_coeffs_df <- data.frame()

for (i in 1:iterations){

# random X countries out 
countries_left <- sort(sample(all_countries, length(all_countries)-left_outs))

calc_data <- final_data[final_data$country %in%  countries_left, ]

#  Model Estimation  ===============================================================================

# | One-Way Fixed Effect Estimation  ---------------------------------------------------------------

model_time_effect <- plm(NET_ODA ~ GNP_PCAP + INF_MORT + RIGHTS_INV + POP, data = calc_data, model = "within", effect = "time" )
model_time_effect.coeffs <- summary(model_time_effect)[["coefficients"]]

model_time_effect.coeffs_df <- rbind(data.frame(),model_time_effect.coeffs)
model_time_effect.coeffs_df["index_name"] <- rownames(model_time_effect.coeffs_df)
time_coeffs_df <- rbind(time_coeffs_df,model_time_effect.coeffs_df)

# | Two-Way Fixed Effect Estimation  ---------------------------------------------------------------

model_two_ways <- plm(NET_ODA ~ GNP_PCAP + INF_MORT + RIGHTS_INV + POP, data = calc_data, model = "within", effect = "twoways",index = c('country', 'year') )
model_two_ways.coeffs <- summary(model_two_ways)[["coefficients"]]

model_two_ways.coeffs_df <- rbind(data.frame(),model_two_ways.coeffs)
model_two_ways.coeffs_df["index_name"] <- rownames(model_two_ways.coeffs_df)
two_ways_coeffs_df <- rbind(two_ways_coeffs_df,model_two_ways.coeffs_df)

}

trumbull_model <- read_delim("data/trumbull_wall_model.csv", delim=";")

colnames(time_coeffs_df) <- c("Estimate", "StdError", "tValue", "PR", "index_name")
colnames(two_ways_coeffs_df) <- c("Estimate", "StdError", "tValue", "PR", "index_name")


time_coeffs_df$SIGN[time_coeffs_df$PR > 0.1] = 0
time_coeffs_df$SIGN[time_coeffs_df$PR <= 0.1 & time_coeffs_df$PR > 0.05 ] = 1
time_coeffs_df$SIGN[time_coeffs_df$PR <= 0.05 & time_coeffs_df$PR > 0.01 ] = 2
time_coeffs_df$SIGN[time_coeffs_df$PR <= 0.01 ] = 3


two_ways_coeffs_df$SIGN[two_ways_coeffs_df$PR > 0.1] = 0
two_ways_coeffs_df$SIGN[two_ways_coeffs_df$PR <= 0.1 & two_ways_coeffs_df$PR > 0.05 ] = 1
two_ways_coeffs_df$SIGN[two_ways_coeffs_df$PR <= 0.05 & two_ways_coeffs_df$PR > 0.01 ] = 2
two_ways_coeffs_df$SIGN[two_ways_coeffs_df$PR <= 0.01 ] = 3

two_ways_coeffs_df$Model <- "TwoWay"
time_coeffs_df$Model <- "OneWay"

plot_df <- rbind(two_ways_coeffs_df, time_coeffs_df)
plot <- ggplot(data=plot_df, aes(x=Estimate, color=Model, fill=Model)) +
    geom_histogram(alpha=0.7, bins = 50)+
    scale_colour_manual(values=c("#009682","#4664AA"))+
    scale_fill_manual(values=c("#009682","#4664AA"))+
    geom_vline(aes(xintercept=0))+
    geom_vline(data=trumbull_model, aes(xintercept=Time),linetype="dashed", color="#009682")+
    geom_vline(data=trumbull_model, aes(xintercept=TwoWays),linetype="dashed", color="#4664AA")+
    facet_wrap(~index_name,nrow = 4,scales = "free", strip.position="right")
    
ggsave("results/country_sensitivity_analysis.jpg",plot=plot, width=8, height=5, dpi=400)
