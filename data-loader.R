#--------------------------
# COVID-19 cases 
# author = Jorick Roels
#--------------------------
library(tidyverse)
library(lubridate)


# Check if neccesary dirs exist and create them when needed.
if(!dir.exists("rawdata")) dir.create("rawdata")

if(!dir.exists("procdata")) dir.create("procdata")

#--------------------------
# Setup file names
files <- c("Confirmed", "Deaths", "Recovered")
fnames <- paste0("time_series_19-covid-", files, ".csv")
#--------------------------
# Download Data
master_url <- paste("https://raw.githubusercontent.com",
                    "CSSEGISandData/COVID-19/master",
                    "csse_covid_19_data",
                    "csse_covid_19_time_series", sep = "/")
for(fn in fnames){
  thefile <- file.path("rawdata", fn)
  download.file(file.path(master_url, fn), destfile = thefile)
}

#------------------------
# Read the data
db_confirmed <- read_csv(file.path("rawdata", fnames[1]))

db_deaths <- read_csv(file.path("rawdata", fnames[2]))

db_recovered <- read_csv(file.path("rawdata", fnames[3]))

#------------------------
# Process data
# Select the countries we want ot see
countries <- c("Japan", "Belgium", "Italy", "Sweden", "Spain", "Netherlands",
               "Germany", "Korea, South", "UK")

# Dataset formatting
  db_confirmed <- db_confirmed %>% 
    rename(Province_state = `Province/State`, Country = `Country/Region`) %>% 
    filter(Country %in% countries) %>% 
    select(-Province_state, -(Lat:Long)) %>% 
    pivot_longer(ends_with("20"), names_to = "date", values_to = "confirmed") %>% 
    mutate(date = mdy(date))
  
  db_deaths <- db_deaths %>% 
    rename(Province_state = `Province/State`, Country = `Country/Region`) %>% 
    filter(Country %in% countries) %>% 
    select(-Province_state, -(Lat:Long)) %>% 
    pivot_longer(ends_with("20"), names_to = "date", values_to = "deaths") %>% 
    mutate(date = mdy(date))
  
  db_recovered <- db_recovered %>% 
    rename(Province_state = `Province/State`, Country = `Country/Region`) %>% 
    filter(Country %in% countries) %>% 
    select(-Province_state, -(Lat:Long)) %>% 
    pivot_longer(ends_with("20"), names_to = "date", values_to = "recovered") %>% 
    mutate(date = mdy(date))

db_all <- db_confirmed %>% 
  left_join(db_deaths) %>% 
  left_join(db_recovered)

#-------------------------
# Save data

write_csv(db_all, 
            file.path("procdata", "data.csv"))

rm(list = ls())