library(shiny)
library(shinydashboard)
library(sf)
library(tidyverse)

# load the in the dashboard data files
input_directory = "C:/Users/atabascio/OneDrive - CUI/Documents/construction-mitigation/Interim/"

ff_monthly = read_csv(paste0(input_directory, "ff_monthly_meta.csv")) %>%
  select(-...1)
ff_quarter = read_csv(paste0(input_directory, "ff_table_meta.csv")) %>%
  select(-...1)
ff_day_of_week = read_csv(paste0(input_directory, "ff_day_of_week_meta.csv")) %>%
  select(-...1)
ff_time_of_day = read_csv(paste0(input_directory, "ff_time_of_day_meta.csv")) %>%
  select(-...1)
ff_type = read_csv(paste0(input_directory, "ff_type_meta.csv")) %>%
  select(-...1)
traveltime = read_csv(paste0(input_directory, "tt_grouped_meta.csv")) %>%
  select(-...1)
marketrent = read_csv(paste0(input_directory, "marketrent_meta.csv")) %>%
  select(-...1)
vacancyrate = read_csv(paste0(input_directory, "vacancyrate_meta.csv")) %>%
  select(-...1)
