library(shiny)
library(shinydashboard)
library(sf)
library(plotly)
library(tidyverse)
library(lubridate)
library(scales)
library(leaflet)

# load the in the dashboard data files

input_directory = "../../Interim/"

ff_monthly = read_csv(paste0(input_directory, "ff_monthly_meta.csv")) %>%
  select(-...1) %>% rename("Date" = date)
ff_quarter = read_csv(paste0(input_directory, "ff_table_meta.csv")) %>%
  select(-...1)
ff_day_of_week = read_csv(paste0(input_directory, "ff_day_of_week_meta.csv")) %>%
  select(-...1)
ff_time_of_day = read_csv(paste0(input_directory, "ff_time_of_day_meta.csv")) %>%
  select(-...1)
ff_type = read_csv(paste0(input_directory, "ff_type_meta.csv")) %>%
  select(-...1) %>% rename("Visits" = Count)
traveltime = read_csv(paste0(input_directory, "tt_grouped_meta.csv")) %>%
  select(-...1)
marketrent = read_csv(paste0(input_directory, "marketrent_meta.csv")) %>%
  select(-...1)
vacancyrate = read_csv(paste0(input_directory, "vacancyrate_meta.csv")) %>%
  select(-...1)

# load in the area shapefiles
BIAs_shp = st_read(paste0(input_directory, "BIAs/All.shp")) %>%
  st_transform(crs = 4326)

# load in the main street point file
Business_shp = st_read(paste0(input_directory, "ms_businesses.geojson"))

# get the centroid coordinates for the zoom and fly around
BIA_centroids = st_centroid(BIAs_shp)

BIA_centroids = as.data.frame(st_coordinates(BIA_centroids))

BIA_centroids = BIA_centroids %>%
  rename("Longitude" = X, "Latitude" = Y)

# join back to the shapefile
BIAs_shp = BIAs_shp %>%
  bind_cols(BIA_centroids)

