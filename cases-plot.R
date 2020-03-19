#--------------------------
# COVID-19 cases-plot
# author = Jorick Roels
#--------------------------

# Setup
library(ggplot2)
library(tidyverse)
library(patchwork)
library(modelr)

# read data
cov_data <- read_csv("procdata/data.csv")

# add log transformation
#confirmed <- confirmed %>% 
#  mutate(log_cases = if_else(cases > 0,log10(cases), 0))

# Make plots

linear_confirmed <- cov_data %>% 
  ggplot(mapping = aes(date, confirmed)) +
    geom_line(mapping = aes(color = Country_region))

linear_confirmed_log <- cov_data %>% 
  ggplot(mapping = aes(date, confirmed)) +
  geom_line(mapping = aes(color = Country_region)) +
  scale_y_log10()

linear_deaths<- cov_data %>% 
  ggplot(mapping = aes(date, deaths)) +
    geom_line(mapping = aes(color = Country_region))

linear_deaths_log <- cov_data %>% 
  ggplot(mapping = aes(date, deaths)) +
  geom_line(mapping = aes(color = Country_region)) +
  scale_y_log10()

(linear_confirmed | linear_confirmed_log) /
  (linear_deaths | linear_deaths_log)

# Cases starting from 100 confirmed cases
cov_data_100 <- cov_data %>% 
  filter(confirmed >= 100)

linear_confirmed_100 <- cov_data_100 %>% 
  ggplot(mapping = aes(date, confirmed)) +
  geom_line(mapping = aes(color = Country_region))

linear_confirmed_log_100 <- cov_data_100 %>% 
  ggplot(mapping = aes(date, confirmed)) +
  geom_line(mapping = aes(color = Country_region)) +
  scale_y_log10()

linear_deaths_100 <- cov_data_100 %>% 
  ggplot(mapping = aes(date, deaths)) +
  geom_line(mapping = aes(color = Country_region))

linear_deaths_log_100 <- cov_data_100 %>% 
  ggplot(mapping = aes(date, deaths)) +
  geom_line(mapping = aes(color = Country_region)) +
  scale_y_log10()

(linear_confirmed_100 | linear_confirmed_log_100) /
  (linear_deaths_100 | linear_deaths_log_100) +
  theme_light() +
  ggtitle("Plots of confirmed cases and deaths starting from 100 confirmed cases") +
  plot_layout(guides = 'collect',
              heights = c(2,2,1)) +
  plot_annotation(title = "Plot of linear and logarithmic confirmed cases and deaths",
                  caption = paste("Data obtained from Johns Hopkins CSSE:",
                                  "https://github.com/CSSEGISandData/COVID-19"))

# Save the plot
if(!dir.exists("Fig")) dir.create("Fig")
fname <- paste0("covid_plots",Sys.Date(),".png")
ggsave(file.path("Fig",fname), width = 8, height = 8)

#------------------
# Modeling only Belgium
belgium <- confirmed %>% 
  filter(Country_region == "Belgium") %>% 
  filter(cases > 0)

belgium_plot <- belgium %>% 
  ggplot(mapping = aes(date, log_cases)) +
    geom_line()
  
belgium_plot


model <- lm(log(cases) ~ log(date), data = belgium)

belgium_pred <- belgium %>% 
  add_predictions(model)

belgium_pred %>% 
  ggplot(mapping = aes(date)) +
    geom_line(mapping = aes(y = cases)) +
    geom_line(mapping = aes(y = pred), color = "red")
