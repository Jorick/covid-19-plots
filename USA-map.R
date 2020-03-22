#-------------------------
# A map of USA with al cases by state
# Author = Joric Roels
#-------------------------

# Setup
library(tidyverse)
library(lubridate)
library(sf)
library(spData)
library(tmap)

#------------------------
# Read the rax data
files <- c("Confirmed", "Deaths", "Recovered")
fnames <- paste0("time_series_19-covid-", files, ".csv")
db_confirmed <- read_csv(file.path("rawdata", fnames[1]))

db_deaths <- read_csv(file.path("rawdata", fnames[2]))

db_recovered <- read_csv(file.path("rawdata", fnames[3]))

# Filter out data for USA
us_confirmed <- db_confirmed %>% 
  rename(NAME = `Province/State`, Country = `Country/Region`) %>% 
  filter(Country == "US") %>% 
  select(-(Lat:Long)) %>% 
  pivot_longer(ends_with("20"), names_to = "date", values_to = "confirmed") %>% 
  mutate(date = mdy(date))

us_deaths <- db_deaths %>% 
  rename(NAME = `Province/State`, Country = `Country/Region`) %>% 
  filter(Country == "US") %>% 
  select(-(Lat:Long)) %>% 
  pivot_longer(ends_with("20"), names_to = "date", values_to = "deaths") %>% 
  mutate(date = mdy(date))

us_recovered <- db_recovered %>% 
  rename(NAME = `Province/State`, Country = `Country/Region`) %>% 
  filter(Country == "US") %>% 
  select(-(Lat:Long)) %>% 
  pivot_longer(ends_with("20"), names_to = "date", values_to = "recovered") %>% 
  mutate(date = mdy(date))

us_all <- us_confirmed %>%
  left_join(us_deaths) %>% 
  left_join(us_recovered) %>% 
  mutate(lconfirmed = if_else(confirmed == 0, 0, log(confirmed)))

# Combine data with us map data
us_map <- us_states %>% 
  left_join(us_all, by = "NAME")

# make plot
us_plot <- tm_shape(us_map) +
  tm_polygons(col = "lconfirmed") +
  tm_facets(along = "date", free.coords = FALSE)

# save plot as .gif
fname <- paste0("us_map_", Sys.Date(), ".gif")
tmap_animation(us_plot,
               filename = file.path("Fig", fname),
               delay = 30,
               width = 1500,
               height = 1500)
