#--------------------------
# COVID-19 cases-plot
# author = Jorick Roels
#--------------------------

# Setup
library(ggplot2)
library(tidyverse)
library(patchwork)

# read data
confirmed <- read_csv("procdata/confirmed.csv")

# add log transformation
confirmed <- confirmed %>% 
  mutate(log_cases = if_else(cases > 0,log10(cases), 0))

# Make plots
heat_map <- confirmed %>% 
  ggplot(mapping = aes(date, Country_region)) +
    geom_tile(mapping = aes(fill = cases)) +
    scale_fill_viridis_c(option = "plasma")

heat_map_log <- confirmed %>% 
  ggplot(mapping = aes(date, Country_region)) +
  geom_tile(mapping = aes(fill = log_cases)) +
  scale_fill_viridis_c(option = "plasma")

linear <- confirmed %>% 
  ggplot(mapping = aes(date, cases)) +
    geom_line(mapping = aes(color = Country_region))

linear_log2 <- confirmed %>% 
  ggplot(mapping = aes(date, cases)) +
  geom_line(mapping = aes(color = Country_region)) +
  scale_y_log10()

(heat_map | heat_map_log) /
  (linear | linear_log)