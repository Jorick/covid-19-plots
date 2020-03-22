#--------------------------
# COVID-19 cases-plot
# author = Jorick Roels
#--------------------------

# Setup
library(ggplot2)
library(tidyverse)
library(patchwork)
library(modelr)

# source data loader 
source("data-loader.R")
# read data
cov_data <- read_csv("procdata/data.csv")

#------------------------
# Data transformation

# Cases starting from 100 confirmed cases
cov_data_100 <- cov_data %>% 
  filter(confirmed >= 100)

# Start date of eacht country for 100 confirmed cases
start_dates_all <- cov_data_100 %>% 
  group_by(Country) %>% 
  summarise(start_date = min(date))

# Add days column to each country starting the count from each start date
cov_data_100 <- cov_data_100 %>% 
  left_join(start_dates_all) %>% 
  mutate(day = as.numeric(date - (start_date - 1))) %>% 
  select(-start_date)

# -----------------------------
# Make the plots
log_confirmed <- cov_data_100 %>% 
  ggplot(mapping = aes(day, confirmed)) +
  geom_line(mapping = aes(color = Country)) +
  geom_point(mapping = aes(color = Country)) +
  scale_y_log10() +
  ggtitle("Number of confirmed cases on logarithmic scale") +
  ylab("log(confirmed cases)")

linear_deaths <- cov_data_100 %>% 
  ggplot(mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = Country)) +
  geom_point(mapping = aes(color = Country)) +
  ggtitle("Number of deaths in 8 countries")

log_deaths <- cov_data_100 %>% 
  ggplot(mapping = aes(day, deaths)) +
  geom_line(mapping = aes(color = Country)) +
  geom_point(mapping = aes(color = Country)) +
  scale_y_log10() +
  ggtitle("Number of deaths in 8 countries") +
  ylab("Log(deaths)")

#------------------
# Simple model for Belgium
data_belgium <- cov_data_100 %>%
  filter(Country == "Belgium")

data_belgium <- data_belgium %>% 
  mutate(lconfirmed = log(confirmed))

model <- lm(lconfirmed ~ day, data = data_belgium)

belgium_pred <- data_belgium %>% 
  add_predictions(model) %>% 
  mutate(confirmed_pred = exp(pred))

belgium_plot <- belgium_pred %>% 
  ggplot(mapping = aes(date)) +
    geom_line(mapping = aes(y = confirmed),color = "blue") +
    geom_point(mapping = aes(y = confirmed), color = "blue") +
    geom_line(mapping = aes(y = confirmed_pred), color = "red") +
    scale_y_log10() +
    ggtitle("Number of confirmed cases in Belgium and predictive model on logarithmic scale") +
    ylab("log(cases)")

# -----------------
# Plot everything

(belgium_plot) /
  (log_confirmed | log_deaths) +
  plot_layout(guides = 'collect') +
  plot_annotation(caption = paste("Data obtained from Johns Hopkins CSSE:",
                                  "https://github.com/CSSEGISandData/COVID-19")) &
  theme_light() +
  theme(plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))

# Save the plot
if(!dir.exists("Fig")) dir.create("Fig")
fname <- paste0("covid_plots",Sys.Date(),".png")
ggsave(file.path("Fig",fname), width = 12, height = 8)

