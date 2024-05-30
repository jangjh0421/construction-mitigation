#######################################################################
# Title: Construction Mitigation Data Refinement
# Creator: Alex Tabascio
# Date Created: 2024-05-29
#######################################################################


#### INTRODUCTION ####

# This script will be used to create the backend data for the Toronto Construction
# Mitigation Project which will then be visualized in an r shiny dashboard. The
# dashboard contains four main sections in order to provide a BIA with information regarding
#     Visitor Levels
#     Commercial Real Estate
#     Traffic
#     Spending Patterns
# 
# Data for the project was taken from a combination of private and open sources and aggregated in
# in such a way that allows the user to filter the dashboard based on Area, Year and Quarter



#### LOADING THE NECCESSARY PACKAGES ####

library(tidyverse)
library(sf)
library(lubridate)
options(scipen = 999)



#### WORKFLOW FUNCTIONS ####

# FILTER QUARTER
#   This function will filter the mobilscapes data based on the desired quarter
#   Takes a large mobilescapes output and filters the rows based on the desired quarter
#   returning the resulting data frame

filter_quarter = function(mobilescapes_output, desired_quarter, common_location){
  # filter the rows used based on the desired quarter
  if(desired_quarter == "Q1"){
    if(common_location == "evening"){
      mobilescapes_filtered = mobilescapes_output %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, January, February, March, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(January > 0 | February > 0 | March > 0) %>%
        mutate(Quarter_Visits = January + February + March)
    } else {
      mobilescapes_filtered = mobilescapes_output %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, January, February, March, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(January > 0 | February > 0 | March > 0) %>%
        mutate(Quarter_Visits = January + February + March)
    }
  } else if (desired_quarter == "Q2"){
    if(common_location == "evening"){
      mobilescapes_filtered = mobilescapes_output %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, April, May, June, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(April > 0 | May > 0 | June > 0) %>%
        mutate(Quarter_Visits = April + May + June)
    } else {
      mobilescapes_filtered = mobilescapes_output %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, April, May, June, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(April > 0 | May > 0 | June > 0) %>%
        mutate(Quarter_Visits = April + May + June)
    }
  } else if (desired_quarter == "Q3"){
    if(common_location == "evening"){
      mobilescapes_filtered = mobilescapes_output %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, July, August, September, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(July > 0 | August > 0 | September > 0) %>%
        mutate(Quarter_Visits = July + August + September)
    } else {
      mobilescapes_filtered = mobilescapes_output %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, July, August, September, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(July > 0 | August > 0 | September > 0) %>%
        mutate(Quarter_Visits = July + August + September)
    }
  } else {
    if(common_location == "evening"){
      mobilescapes_filtered = mobilescapes_output %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, October, November, December, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(October > 0 | November > 0 | December > 0) %>%
        mutate(Quarter_Visits = October + November + December)
    } else {
      mobilescapes_filtered = mobilescapes_output %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, October, November, December, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(October > 0 | November > 0 | December > 0) %>%
        mutate(Quarter_Visits = October + November + December)
    }
  }
  
  # return the filtered mobilescapes data
  return(mobilescapes_filtered)
}


# APPEND RESULTS
append_results = function(input_table, appending_table = NULL){
  if(is.null(appending_table)){
    resulting_table = input_table
  } else {
    resulting_table = bind_rows(input_table, appending_table)
  }
  
  return(resulting_table)
}


#### CHART FUNCTIONS ####


# FOOT TRAFFIC CHANGE
#   Calculates the percentage difference of the current quarter to four temporal filters
#   Same Quarter of the previous year
#   Same Quarter of the construction start year
#   Same Quarter of the prepandemic year
#   Previous Quarter of the same year

foottraffic_change = function(ff_target, ff_control, name){
  # get the foot traffic total for each month
  get_monthly_total = function(ff_quarterly){
    # get the row-wise total from columns 4:6
    ff_quarterly_sum = rowSums(ff_quarterly[,4:6])
    return(ff_quarterly_sum)
  }
  # get the target and control quarterly totals using the get_monthly_total function
  ff_yoy_target_sum = sum(get_monthly_total(target_ff))
  ff_control_sum = sum(get_monthly_total(control_ff))
  
  # create a custom tibble
  ff_change = tibble("Name" = c(name), "Target Count" = c(ff_yoy_target_sum), "Control Count" = c(ff_control_sum),
                     "Change" = c(((ff_yoy_target_sum - ff_control_sum) / ff_control_sum) * 100)) 
  return(ff_change)
}


# DAY OF THE WEEK
#   calculate the quarterly total of visits per day of the week

day_of_week_count = function(target_ff){
  # get the quarterly sum of foot traffic for the target quarter
  target_day_count = tibble(Day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                            Visits = c(sum(target_ff$Sunday), sum(target_ff$Monday), sum(target_ff$Tuesday), sum(target_ff$Wednesday), sum(target_ff$Thursday),
                                       sum(target_ff$Friday), sum(target_ff$Saturday)))
  target_day_count = target_day_count %>%
    mutate(Percentage = Visits / sum(Visits) * 100)
  
  return(target_day_count)
}


# TIME OF THE DAY
#   get the quarterly total of visits per time of the day

time_of_day_count = function(target_ff){
  # count the daily total
  target_time_count = tibble(Time = c("Early Morning: 12am - 6am", "Morning: 6am - 12pm", "Afternoon: 12pm - 6pm", "Evening: 6pm - 12am"),
                             Visits = c(sum(target_ff$EarlyMorning), sum(target_ff$MorningCommute) + sum(target_ff$LateMorning), sum(target_ff$Midday) + sum(target_ff$EveningCommute),
                                        sum(target_ff$Evening) + sum(target_ff$LateEvening)))
  target_time_count = target_time_count %>%
    mutate(Percentage = Visits / sum(Visits) * 100)
  
  return(target_time_count)
}


# VISITOR TYPE

# new function to calculate the visitor type due to the new privacy standards
visitor_type = function(foottraffic_CEL, foottraffic_CDL, StudyArea_Buff){
  
  # get the total number of visits
  Quarter_Visits = sum(foottraffic_CEL$Quarter_Visits)
  
  # get the number of workers using the common daytime location
  # plot the CDL coordinates
  foottraffic_workers = foottraffic_CDL %>% filter(!is.na(CDL_LATITUDE)) 
  foottraffic_workers = st_as_sf(x = foottraffic_workers,
                                 coords = c("CDL_LONGITUDE", "CDL_LATITUDE"),
                                 crs = 4326)
  foottraffic_workers = foottraffic_workers %>% st_transform(crs = 3347)
  
  # get the intersection of workers inside the study area
  workers_int = st_intersection(foottraffic_workers, StudyArea_Buff)
  
  # get the count of Daily Visits
  foottraffic_workers_count = sum(workers_int$Quarter_Visits)
  
  # get the number of residents using the Common Evening Location
  # plot the CEL coordinates
  foottraffic_residents = foottraffic_CEL %>% filter(!is.na(CEL_LATITUDE)) 
  foottraffic_residents = st_as_sf(foottraffic_residents,
                                   coords = c("CEL_LONGITUDE", "CEL_LATITUDE"),
                                   crs = 4326)
  foottraffic_residents = foottraffic_residents %>% st_transform(crs = 3347)
  
  # get the intersection of residents inside the study area
  residents_int = st_intersection(foottraffic_residents, StudyArea_Buff)
  
  # get the count of Daily Visits
  foottraffic_residents_count = sum(residents_int$Quarter_Visits)
  
  # Create a final tibble and export
  foottraffic_summary = tibble(
    Type = c("Resident", "Recurring Visitor", "Infrequent Visitor"),
    Count = c(foottraffic_residents_count, foottraffic_workers_count, Quarter_Visits - (foottraffic_residents_count + foottraffic_workers_count)))
  
  # Add percentage
  foottraffic_summary = foottraffic_summary %>% mutate(Percentage = Count / Quarter_Visits * 100)
  return(foottraffic_summary)
}


# MONTHLY FOOT TRAFFIC SUMMARY

monthly_foottraffic = function(ff_current, ff_pandemic){
  
  # summarize the quarterly totals for each month
  ff_current = ff_current %>%
    summarize(across(c(4,5,6), sum, .names = "{col}")) %>%
    pivot_longer(cols = everything(), names_to = "Month", values_to = "Count_current")
  
  ff_pandemic = ff_pandemic %>%
    summarize(across(c(4,5,6), sum, .names = "{col}")) %>%
    pivot_longer(cols = everything(), names_to = "Month", values_to = "Count_pandemic") %>%
    select(-Month)
  
    # bind columns
  ff_monthly_change = bind_cols(ff_current, ff_pandemic)
  
  # get the relative percentage to 2019 and convert date
  ff_monthly_change = ff_monthly_change %>%
    mutate(Percentage = Count_current / Count_pandemic * 100,
           date = case_when(
             Month == "January" ~ ymd(paste0(study_year, "01", "01")),
             Month == "February" ~ ymd(paste0(study_year, "02", "01")),
             Month == "March" ~ ymd(paste0(study_year, "03", "01")),
             Month == "April" ~ ymd(paste0(study_year, "04", "01")),
             Month == "May" ~ ymd(paste0(study_year, "05", "01")),
             Month == "June" ~ ymd(paste0(study_year, "06", "01")),
             Month == "July" ~ ymd(paste0(study_year, "07", "01")),
             Month == "August" ~ ymd(paste0(study_year, "08", "01")),
             Month == "September" ~ ymd(paste0(study_year, "09", "01")),
             Month == "October" ~ ymd(paste0(study_year, "10", "01")),
             Month == "November" ~ ymd(paste0(study_year, "11", "01")),
             Month == "December" ~ ymd(paste0(study_year, "12", "01"))
           )) %>%
    select(date, Percentage, Count_current) %>%
    rename("Count" = Count_current)
  
  # return
  return(ff_monthly_change)
  
}



#### LOAD SCRIPT-SPECIFIC VARIABLES ####

# Temporal variables
year = 2024
data_year = 24
quarter = "Q1"

# BIA list
BIAs = c("DowntownWest", "DowntownYonge", "FinancialDistrict", "Greektown", "Leslieville", "LibertyVillage",
         "PapeVillage", "QueenStreet", "Riverside", "StLawrenceMarket", "WestQueenWest")



#### Begin the Data Cleaning ####

## FOOT TRAFFIC TABLE ##

for(i in 1:length(BIAs)){
  
  file_directory = "./Data/EnvironicsData_DA/"
  
  # Load in the four key data sets and filter based on the quarter
  
    # current year
  file_name = paste0(file_directory, BIAs[i], data_year, "CEL.csv")
  ff_current_year = read_csv(file_name)
  ff_current_year = filter_quarter(ff_current_year, quarter, "evening")
  
    # previous year
  file_name = paste0(file_directory, BIAs[i], (data_year - 1), "CEL.csv")
  ff_previous_year = read_csv(file_name)
  ff_previous_year = filter_quarter(ff_previous_year, quarter, "evening")
  
    # pre-construction
  file_name = paste0(file_directory, BIAs[i], "22")
  ff_construction_year = read_csv(file_name)
  ff_construction_year = filter_quarter(ff_construction_year, quarter, "evening")
  
    # pre-pandemic
  file_name = paste0(file_directory, BIAs[i], "19")
  ff_pandemic_year = read_csv(file_name)
  ff_pandemic_year = filter_quarter(ff_pandemic_year, quarter, "evening")
  
    # last quarter
  if(quarter == "Q1"){
    ff_last_quarter = ff_previous_year
    ff_last_quarter = filter_quarter(ff_last_quarter, "Q4", "evening")
  } else {
    ff_last_quarter = ff_current_year
    if(quarter == "Q2"){
      ff_last_quarter = filter_quarter(ff_last_quarter, "Q1", "evening")
    } else if (quarter == "Q3"){
      ff_last_quarter = filter_quarter(ff_last_quarter, "Q2", "evening")
    } else {
      ff_last_quarter = filter_quarter(ff_last_quarter, "Q3", "evening")
    }
  }
  
  # Calculate the percentage difference in foot traffic
  
    # current to previous year change
  ff_yoy_change = foottraffic_change(ff_current_year, ff_previous_year, BIAs[i])
  ff_yoy_change = ff_yoy_change %>%
    mutate(time_window = "Year Over Year")
  
    # current to construction start date
  ff_preconst_change = foottraffic_change(ff_current_year, ff_construction_year, BIAs[i])
  ff_preconst_change = ff_preconst_change %>%
    mutate(time_window = "Construction Start")
  
    # current to pandemic change
  ff_pandemic_change = foottraffic_change(ff_current_year, ff_pandemic_year, BIAs[i])
  ff_pandemic_change = ff_pandemic_change %>%
    mutate(time_window = "Pre Pandemic")
  
    # current to last quarter change
  ff_lastquarter_change = foottraffic_change(ff_current_year, ff_last_quarter, BIAs[i])
  ff_lastquarter_change = ff_lastquarter_change %>%
    mutate(time_window = "Last Quarter")
  
  
  # Create a large data set for all study areas within the Database
  ff_change = bind_rows(ff_yoy_change, ff_preconst_change, ff_pandemic_change, ff_lastquarter_change)
  ff_change = ff_change %>%
    mutate(Year = year,
           Quarter = quarter) %>%
    select(Year, Quarter, Name, everything())
    
  # append the table based on the iteration
  if(i == 1){
    ff_change_all = append_results(ff_change)
  } else {
    ff_change_all = append_results(ff_change_all, ff_change)
  }
  
}


## VISITOR TYPE CHART ##

for(i in 1:length(BIAs)){
  
  file_directory = "./Data/EnvironicsData_DA/"
  
  # Load in the evening data for the current quarter
  file_name = paste0(file_directory, BIAs[i], data_year, "CEL.csv")
  ff_visit_type_evening = read_csv(file_name)
  ff_visit_type_evening = filter_quarter(ff_visit_type, quarter, "evening")
  
  # Load in the day data for the current quarter
  file_name = paste0(file_directory, BIAs[i], data_year, "CDL.csv")
  ff_visit_type_day = read_csv(file_name)
  ff_visit_type_day = filter_quarter(ff_visit_type, quarter, "day")
  
  # Load in the BIA shapefile
  file_directory_area = "./Data/BIAs/"
  study_area = st_read(paste0(file_directory_area, BIAs[i]))
  # get the 1 kilometer buffer for the BIA
  study_area = st_buffer(study_area, 1000)
  
  # call the visitor types function for the current quarter
  ff_visit_type = visitor_type(ff_visit_type_evening, ff_visit_type_day, study_area)
  
  ff_visit_type = ff_visit_type %>%
    mutate(Name = BIAs[i],
           Quarter = quarter,
           Year = year) %>%
    select(Year, Quarter, Name, everything())
  
  # append the table based on the iteration
  if(i == 1){
    ff_visit_type_all = append_results(ff_visit_type)
  } else {
    ff_visit_type_all = append_results(ff_visit_type_all, ff_visit_type)
  }
  
}


## DAY OF THE WEEK ##

for(i in 1:length(BIAs)){
  
  file_directory = "./Data/EnvironicsData_DA/"
  
  # Load in the evening data for the current quarter
  file_name = paste0(file_directory, BIAs[i], data_year, "CEL.csv")
  ff_day_of_week = read_csv(file_name)
  ff_day_of_week = filter_quarter(ff_day_of_week, quarter, "evening")
  
  # get the day of week count for the current quarter
  ff_day_of_week = day_of_week_count(ff_day_of_week)
  
  ff_day_of_week = ff_day_of_week %>%
    mutate(Name = BIAs[i],
           Quarter = quarter,
           Year = year) %>%
    select(Year, Quarter, Name, everything())
  
  # append the table based on the iteration
  if(i == 1){
    ff_day_of_week_all = append_results(ff_day_of_week)
  } else {
    ff_day_of_week_all = append_results(ff_day_of_week_all, ff_day_of_week)
  }
  
}


## TIME OF THE DAY ##

for(i in 1:length(BIAs)){
  
  file_directory = "./Data/EnvironicsData_DA/"
  
  # Load in the evening data for the current quarter
  file_name = paste0(file_directory, BIAs[i], data_year, "CEL.csv")
  ff_time_of_day = read_csv(file_name)
  ff_time_of_day = filter_quarter(ff_time_of_day, quarter, "evening")
  
  # get the time of day count for the current quarter
  ff_time_of_day = time_of_day_count(ff_time_of_day)
  
  ff_time_of_day = ff_time_of_day %>%
    mutate(Name = BIAs[i],
           Quarter = quarter,
           Year = year) %>%
    select(Year, Quarter, Name, everything())
  
  # append the table based on the iteration
  if(i == 1){
    ff_time_of_day_all = append_results(ff_time_of_day)
  } else {
    ff_time_of_day_all = append_results(ff_time_of_day_all, ff_time_of_day)
  }
  
}


## MONTHLY FOOTTRAFFIC CHART ##

for(i in 1:length(BIAs)){
  
  file_directory = "./Data/EnvironicsData_DA/"
  
  # Load in the evening data for the current quarter
  file_name = paste0(file_directory, BIAs[i], data_year, "CEL.csv")
  ff_current_year = read_csv(file_name)
  ff_current_year = filter_quarter(ff_monthly_current, quarter, "evening")
  
  # Load in the pre-pandemic data for the current quarter
  file_name = paste0(file_directory, BIAs[i], "19")
  ff_pandemic_year = read_csv(file_name)
  ff_pandemic_year = filter_quarter(ff_pandemic_year, quarter, "evening")
  
  # get the relative change in foot traffic 
  # compared to 2019 to the current quarter
  ff_monthly_change = monthly_foottraffic(ff_current_year, ff_pandemic_year)
  
  ff_monthly_change = ff_monthly_change %>%
    mutate(Name = BIAs[i]) %>%
    select(Name, everything())
  
  # append the table based on the iteration
  if(i == 1){
    ff_monthly_change_all = append_results(ff_monthly_change)
  } else {
    ff_monthly_change_all = append_results(ff_monthly_change_all, ff_monthly_change)
  }
  
}








































































