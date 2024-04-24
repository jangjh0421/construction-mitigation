# This script will be used to create quarterly dashboards using a number of
# different specialty data sets to asses the impact and mitigation techniques
# on main streets businesses and BIAs in relation to the construction of the
# Ontario Line.


# This script will produce an automated quarterly dashboard for each of the 12 BIAs
# analyzing foot traffic, location of visitors, traffic counts and overall spend data
# compared to the same quarter from the previous year in order to account for changes
# in seasonality.


# The initial output will be in pdf format but an overall goal could by the produce an
# updating online dashboard using the shiny r package

# LOAD THE NECCESSARY PACKAGES-------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)
setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters/ConstructionMitigation")
options(scipen = 999)


# Temporal variables
year = 2023
data_year = 23
quarter = "Q4"

# BIA list
BIAs = c("DowntownWest", "DowntownYonge", "FinancialDistrict", "GerrardIndiaBaz", "Greektown", "Leslieville", "LibertyVillage",
         "PapeVillage", "QueenStreet", "Riverside", "StLawrenceMarket", "WestQueenWest")

# CREATE PROJECT FUNCTIONS ----------------------------------------------------

# Get the year-over-year change for the quarter
generate_foottraffic_change = function(target_ff, control_ff, name){
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


# Get the quarterly sum of Percentage for day of the week
generate_day_of_week_count = function(target_ff, control_ff, study_year){
  # get the quarterly sum of foot traffic for the target quarter
  target_day_count = tibble(Day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                            Visits = c(sum(target_ff$Sunday), sum(target_ff$Monday), sum(target_ff$Tuesday), sum(target_ff$Wednesday), sum(target_ff$Thursday),
                                       sum(target_ff$Friday), sum(target_ff$Saturday)),
                            Year = c(as.character(study_year)),
                            Quarter = c(quarter))
  target_day_count = target_day_count %>%
    mutate(Percentage = Visits / sum(Visits) * 100)
  # Do the same for the control year
  control_day_count = tibble(Day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                             Visits = c(sum(control_ff$Sunday), sum(control_ff$Monday), sum(control_ff$Tuesday), sum(control_ff$Wednesday), sum(control_ff$Thursday),
                                        sum(control_ff$Friday), sum(control_ff$Saturday)),
                             Year = c(as.character(study_year - 1)),
                             Quarter = c(quarter))
  control_day_count = control_day_count %>%
    mutate(Percentage = Visits / sum(Visits) * 100)
  # bind the rows and return tibble
  ff_day_of_week_count = bind_rows(target_day_count, control_day_count)
  return(ff_day_of_week_count)
}


# Get the quarterly sum of Percentage for time of the day
generate_time_of_day_count = function(target_ff, control_ff, study_year){
  # get the quarterly sum of foot traffic for the target quarter
  target_time_count = tibble(Time = c("Early Morning: 12am - 6am", "Morning: 6am - 12pm", "Afternoon: 12pm - 6pm", "Evening: 6pm - 12am"),
                             Visits = c(sum(target_ff$EarlyMorning), sum(target_ff$MorningCommute) + sum(target_ff$LateMorning), sum(target_ff$Midday) + sum(target_ff$EveningCommute),
                                        sum(target_ff$Evening) + sum(target_ff$LateEvening)),
                             Year = c(as.character(study_year)),
                             Quarter = c(quarter))
  target_time_count = target_time_count %>%
    mutate(Percentage = Visits / sum(Visits) * 100)
  # Do the same for the control year
  control_time_count = tibble(Time = c("Early Morning: 12am - 6am", "Morning: 6am - 12pm", "Afternoon: 12pm - 6pm", "Evening: 6pm - 12am"),
                              Visits = c(sum(control_ff$EarlyMorning), sum(control_ff$MorningCommute) + sum(control_ff$LateMorning), sum(control_ff$Midday) + sum(control_ff$EveningCommute),
                                         sum(control_ff$Evening) + sum(control_ff$LateEvening)),
                              Year = c(as.character(study_year - 1)),
                              Quarter = c(quarter))
  control_time_count = control_time_count %>%
    mutate(Percentage = Visits / sum(Visits) * 100)
  # bind the rows and return tibble
  ff_time_of_day_count = bind_rows(target_time_count, control_time_count)
  return(ff_time_of_day_count)
}


# Get the new monthly foot traffic numbers relative to 2019
generate_relative_foottraffic = function(target_ff, ff_baseline, study_year){
  # pivot wider
  ff_baseline = ff_baseline %>%
    pivot_longer(!Name, names_to = "Month", values_to = "Count") %>%
    select(-Name)
  target_ff = target_ff %>%
    pivot_longer(!Name, names_to = "Month", values_to = "Count_current")
  # join together
  target_ff = target_ff %>%
    left_join(ff_baseline, by = "Month")
  # get the relative count to 2019
  target_ff = target_ff %>%
    mutate(Percentage = Count_current / Count * 100) %>%
    select(-Count, -Name) %>%
    rename("Count" = Count_current)
  # change the Month to a y-m-d and return
  target_ff = target_ff %>%
    mutate(date = case_when(
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
      Month == "December" ~ ymd(paste0(study_year, "12", "01")))) %>%
    select(date, Percentage, everything()) %>%
    select(-Month)
  # return
  return(target_ff)
}


# new function to calculate the visitor type due to the new privacy standards
generate_visitor_type = function(foottraffic_CEL, foottraffic_CDL, year_date, StudyArea_Buff){
  
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
    Year = c(year_date, year_date, year_date),
    Count = c(foottraffic_residents_count, foottraffic_workers_count, Quarter_Visits - (foottraffic_residents_count + foottraffic_workers_count)))
  
  # Add percentage
  foottraffic_summary = foottraffic_summary %>% mutate(Percentage = Count / Quarter_Visits * 100)
  return(foottraffic_summary)
}


# PART 1 OUTPUT FOR INDIVIDUAL BIAs -------------------------------------------

# LOAD THE CONSTANT DATA FOR THIS SECTION

# commercial real estate data
marketrent = read_csv("./Data/CoStar/MarketRent.csv")
vacancyrate = read_csv("./Data/CoStar/VacancyRate.csv") %>%
  mutate(VacancyRate = VacancyRate * 100)

# get the date ranges based on the quarters
if (quarter == "Q1"){
  # get the filters for the target quarter
  target_start_date = ymd(paste0(year,"01","01"))
  target_end_date = ymd(paste0(year, "03", "31"))
  # get the filters for the control quarter
  control_start_date = ymd(paste0(year-1, "01","01"))
  control_end_date = ymd(paste0(year-1, "03","31"))
} else if (quarter == "Q2"){
  # get the filters for the target quarter
  target_start_date = ymd(paste0(year,"04","01"))
  target_end_date = ymd(paste0(year, "06", "30"))
  # get the filters for the control quarter
  control_start_date = ymd(paste0(year-1, "04","01"))
  control_end_date = ymd(paste0(year-1, "06","30"))
} else if (quarter == "Q3"){
  # get the filters for the target quarter
  target_start_date = ymd(paste0(year,"07","01"))
  target_end_date = ymd(paste0(year, "09", "30"))
  # get the filters for the control quarter
  control_start_date = ymd(paste0(year-1, "07","01"))
  control_end_date = ymd(paste0(year-1, "09","30"))
} else{
  # get the filters for the target quarter
  target_start_date = ymd(paste0(year,"10","01"))
  target_end_date = ymd(paste0(year, "12", "31"))
  # get the filters for the control quarter
  control_start_date = ymd(paste0(year-1, "10","01"))
  control_end_date = ymd(paste0(year-1, "12","31"))
}

# Load the Traffic Time Data
travel_time = read_csv("./Data/TravelTime/i0539_tt_ontario_line.csv") %>%
  mutate(study_area = str_replace_all(study_area, "BIA|\\s", ""))

# INITIATE FOR LOOP

for (i in 1:length(BIAs)){
  
  ## YEAR OVER YEAR CHANGE ----------------------------------------------------
  ### 2023 --------------------------------------------------------------------
  if (year == 2023){
    #### YEAR OVER YEAR CHANGE
    # CURRENT YEAR
    # get the file directory for the target data set
    file_directory = "./Data/Quarterly_Foottraffic/2023/"
    filename_yoy_target = paste0(BIAs[i], "23CEL.csv", sep = "")
    filename_yoy_target = paste0(file_directory, filename_yoy_target, sep = "")
    # load the data files
    ff_yoy_target = read_csv(filename_yoy_target)
    # filter the months used based on the Quarter
    if (quarter == "Q1"){
      ff_yoy_target = ff_yoy_target %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, January, February, March, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(January > 0 | February > 0 | March > 0) %>%
        mutate(Quarter_Visits = January + February + March)
      
    } else if (quarter == "Q2"){
      ff_yoy_target = ff_yoy_target %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, April, May, June, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(April > 0 | May > 0 | June > 0) %>%
        mutate(Quarter_Visits = April + May + June)
      
    } else if (quarter == "Q3"){
      ff_yoy_target = ff_yoy_target %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, July, August, September, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(July > 0 | August > 0 | September > 0) %>%
        mutate(Quarter_Visits = July + August + September)
    } else {
      ff_yoy_target = ff_yoy_target %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, October, November, December, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(October > 0 | November > 0 | December > 0) %>%
        mutate(Quarter_Visits = October + November + December)
    }
    # PREVIOUS YEAR
    # get the file directory for the target data set control (same quarter of the previous year)
    file_directory = paste0("./Data/Quarterly_Foottraffic/", as.character(year - 1), "/", sep = "")
    filename_yoy_control = paste0(BIAs[i], as.character(data_year - 1),"CEL.csv", sep = "")
    filename_yoy_control = paste0(file_directory, filename_yoy_control, sep = "")
    # load the data file
    ff_yoy_control = read_csv(filename_yoy_control)
    # filter the months used based on the Quarter
    if (quarter == "Q1"){
      ff_yoy_control = ff_yoy_control %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, January, February, March, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(January > 0 | February > 0 | March > 0) %>%
        mutate(Quarter_Visits = January + February + March)
      
    } else if (quarter == "Q2"){
      ff_yoy_control = ff_yoy_control %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, April, May, June, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(April > 0 | May > 0 | June > 0) %>%
        mutate(Quarter_Visits = April + May + June)
      
    } else if (quarter == "Q3"){
      ff_yoy_control = ff_yoy_control %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, July, August, September, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(July > 0 | August > 0 | September > 0) %>%
        mutate(Quarter_Visits = July + August + September)
    } else {
      ff_yoy_control = ff_yoy_control %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, October, November, December, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(October > 0 | November > 0 | December > 0) %>%
        mutate(Quarter_Visits = October + November + December)
    }
    
    
    #### LAST QUARTER
    if (quarter == "Q1"){
      # Quarter 4 files
      file_directory = "./Data/Quarterly_Foottraffic/"
      file_directory = paste0(file_directory, as.character(year - 1), "/", sep = "")
      filename_quarter_control = paste0(BIAs[i], as.character(data_year - 1), "CEL.csv", sep = "")
      filename_quarter_control = paste0(file_directory, filename_quarter_control, sep = "")
      # load in the data files
      ff_last_quarter = read.csv(filename_quarter_control) %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, October, November, December) %>%
        filter(October > 0 | November > 0 | December > 0)
      
    } else if(quarter == "Q2"){
      # Quarter 1 files
      file_directory = "./Data/Quarterly_Foottraffic/"
      file_directory = paste0(file_directory, as.character(year), "/",sep = "")
      filename_quarter_control = paste0(BIAs[i], as.character(data_year), "CEL.csv", sep = "")
      filename_quarter_control = paste0(file_directory, filename_quarter_control, sep = "")
      # load in the data files
      ff_last_quarter = read.csv(filename_quarter_control) %>% 
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, January, February, March) %>%
        filter(January > 0 | February > 0 | March > 0)
      
    } else if (quarter == "Q3"){
      # Quarter 2 files
      file_directory = "./Data/Quarterly_Foottraffic/"
      file_directory = paste0(file_directory, as.character(year), "/",sep = "")
      filename_quarter_control = paste0(BIAs[i], as.character(data_year), "CEL.csv", sep = "")
      filename_quarter_control = paste0(file_directory, filename_quarter_control, sep = "")
      # load in the data files
      ff_last_quarter = read.csv(filename_quarter_control) %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, April, May, June) %>%
        filter(April > 0 | May > 0 | June > 0)
      
    } else{
      # Quarter 3 files
      file_directory = "./Data/Quarterly_Foottraffic/"
      file_directory = paste0(file_directory, as.character(year), "/",sep = "")
      filename_quarter_control = paste0(BIAs[i], as.character(data_year), "CEL.csv", sep = "")
      filename_quarter_control = paste0(file_directory, filename_quarter_control, sep = "")
      # load in the data files
      ff_last_quarter = read.csv(filename_quarter_control) %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, July, August, September) %>%
        filter(July > 0 | August > 0 | September > 0)
    }
    
    
  } else {
    ### 2024 and Later --------------------------------------------------------
    #### YEAR OVER YEAR CHANGE
    # Current Year
    file_directory = "./Data/Quarterly_Foottraffic/"
    file_directory = paste0(file_directory, quarter, "/", sep = "")
    filename_yoy_target = paste0(BIAs, as.character(data_year), "CEL.csv", sep = "")
    filename_yoy_target = paste0(file_directory, filename_yoy_target, sep = "")
    # load the data files
    ff_yoy_target = read_csv(filename_yoy_target)
    # filter the months used based on the Quarter
    if (quarter == "Q1"){
      ff_yoy_target = ff_yoy_target %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, January, February, March, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(January > 0 | February > 0 | March > 0) %>%
        mutate(Quarter_Visits = January + February + March)
      
    } else if (quarter == "Q2"){
      ff_yoy_target = ff_yoy_target %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, April, May, June, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(April > 0 | May > 0 | June > 0) %>%
        mutate(Quarter_Visits = April + May + June)
      
    } else if (quarter == "Q3"){
      ff_yoy_target = ff_yoy_target %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, July, August, September, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(July > 0 | August > 0 | September > 0) %>%
        mutate(Quarter_Visits = July + August + September)
      
    } else {
      ff_yoy_target = ff_yoy_target %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, October, November, December, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(October > 0 | November > 0 | December > 0) %>%
        mutate(Quarter_Visits = October + November + December)
    }
    
    # PREVIOUS YEAR (2023) will need to be changed in 2025
    # get the file directory for the target data set control (same quarter of the previous year)
    file_directory = paste0("./Data/Quarterly_Foottraffic/", as.character(year - 1), "/", sep = "")
    filename_yoy_control = paste0(BIAs[i], as.character(data_year - 1),"CEL.csv", sep = "")
    filename_yoy_control = paste0(file_directory, filename_yoy_control, sep = "")
    # load the data file
    ff_yoy_control = read_csv(filename_yoy_control)
    # filter the months used based on the Quarter
    if (quarter == "Q1"){
      ff_yoy_control = ff_yoy_control %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, January, February, March, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(January > 0 | February > 0 | March > 0) %>%
        mutate(Quarter_Visits = January + February + March)
      
    } else if (quarter == "Q2"){
      ff_yoy_control = ff_yoy_control %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, April, May, June, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(April > 0 | May > 0 | June > 0) %>%
        mutate(Quarter_Visits = April + May + June)
      
    } else if (quarter == "Q3"){
      ff_yoy_control = ff_yoy_control %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, July, August, September, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(July > 0 | August > 0 | September > 0) %>%
        mutate(Quarter_Visits = July + August + September)
      
    } else {
      ff_yoy_control = ff_yoy_control %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, October, November, December, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(October > 0 | November > 0 | December > 0) %>%
        mutate(Quarter_Visits = October + November + December)
    }
    
    
    #### LAST QUARTER
    if (quarter == "Q1"){
      # Quarter 4 files
      file_directory = "./Data/Quarterly_Foottraffic/"
      file_directory = paste0(file_directory, as.character(year - 1), "/", sep = "")
      filename_quarter_control = paste0(BIAs[i], as.character(data_year - 1), "CEL.csv", sep = "")
      filename_quarter_control = paste0(file_directory, filename_quarter_control, sep = "")
      # load in the data files
      ff_last_quarter = read.csv(filename_quarter_control) %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, October, November, December) %>%
        filter(October > 0 | November > 0 | December > 0)
      
    } else if(quarter == "Q2"){
      # Quarter 1 files
      file_directory = "./Data/Quarterly_Foottraffic/"
      file_directory = paste0(file_directory, "Q1", "/", sep = "")
      filename_quarter_control = paste0(BIAs, as.character(data_year), "CEL.csv", sep = "")
      filename_quarter_control = paste0(file_directory, filename_yoy_target, sep = "")
      # load in the data files
      ff_last_quarter = read.csv(filename_quarter_control) %>% 
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, January, February, March) %>%
        filter(January > 0 | February > 0 | March > 0)
      
    } else if (quarter == "Q3"){
      # Quarter 2 files
      file_directory = "./Data/Quarterly_Foottraffic/"
      file_directory = paste0(file_directory, "Q2", "/", sep = "")
      filename_quarter_control = paste0(BIAs, as.character(data_year), "CEL.csv", sep = "")
      filename_quarter_control = paste0(file_directory, filename_yoy_target, sep = "")
      # load in the data files
      ff_last_quarter = read.csv(filename_quarter_control) %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, April, May, June) %>%
        filter(April > 0 | May > 0 | June > 0)
      
    } else{
      # Quarter 3 files
      file_directory = "./Data/Quarterly_Foottraffic/"
      file_directory = paste0(file_directory, "Q3", "/", sep = "")
      filename_quarter_control = paste0(BIAs, as.character(data_year), "CEL.csv", sep = "")
      filename_quarter_control = paste0(file_directory, filename_yoy_target, sep = "")
      # load in the data files
      ff_last_quarter = read.csv(filename_quarter_control) %>%
        select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, July, August, September) %>%
        filter(July > 0 | August > 0 | September > 0)
    }
    
  }
  
  ### PRE CONSTRUCTION (2022) -------------------------------------------------
  # Load the mobile data for 2022
  file_directory = "./Data/Quarterly_Foottraffic/2022/"
  filename_construction = paste0(BIAs[i], "22CEL.csv", sep = "")
  filename_construction = paste0(file_directory, filename_construction, sep = "")
  ff_construction = read.csv(filename_construction)
  
  # filter the data based on the quarter
  if (quarter == "Q1"){
    ff_construction = ff_construction %>%
      select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, January, February, March) %>%
      filter(January > 0 | February > 0 | March > 0)
    
  } else if (quarter == "Q2"){
    ff_construction = ff_construction %>%
      select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, April, May, June) %>%
      filter(April > 0 | May > 0 | June > 0)
    
  } else if (quarter == "Q3"){
    ff_construction = ff_construction %>%
      select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, July, August, September) %>%
      filter(July > 0 | August > 0 | September > 0)
    
  } else {
    ff_construction = ff_construction %>%
      select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, October, November, December) %>%
      filter(October > 0 | November > 0 | December > 0)
  }    
  
  ### PRE PANDEMIC ------------------------------------------------------------
  # Load the mobile data for 2019
  file_directory = "./Data/EnvironicsData_DA/"
  filename_pandemic = paste0(BIAs[i], "19.csv", sep = "")
  filename_pandemic = paste0(file_directory, filename_pandemic, sep = "")
  ff_pandemic = read.csv(filename_pandemic)
  
  # filter the data based on the quarter
  if (quarter == "Q1"){
    ff_pandemic = ff_pandemic %>%
      select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, January, February, March) %>%
      filter(January > 0 | February > 0 | March > 0)
    
  } else if (quarter == "Q2"){
    ff_pandemic = ff_pandemic %>%
      select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, April, May, June) %>%
      filter(April > 0 | May > 0 | June > 0)
    
  } else if (quarter == "Q3"){
    ff_pandemic = ff_pandemic %>%
      select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, July, August, September) %>%
      filter(July > 0 | August > 0 | September > 0)
    
  } else {
    ff_pandemic = ff_pandemic %>%
      select(GeofenceName, CEL_LATITUDE, CEL_LONGITUDE, October, November, December) %>%
      filter(October > 0 | November > 0 | December > 0)
  }
  
  # summarise the total foot traffic from pre pandemic and current year
  if (quarter == "Q1"){
   ff_monthly_2019 = ff_pandemic %>%
     summarise(
       January = sum(January, na.rm = TRUE),
       February = sum(February, na.rm = TRUE),
       March = sum(March, na.rm = TRUE)) %>%
     mutate(Name = BIAs[i])
  
   ff_monthly_target = ff_yoy_target %>%
     summarise(
       January = sum(January, na.rm = TRUE),
       February = sum(February, na.rm = TRUE),
       March = sum(March, na.rm = TRUE)) %>%
     mutate(Name = BIAs[i])
  
  } else if (quarter == "Q2"){
   ff_monthly_2019 = ff_pandemic %>%
     summarise(
       April = sum(April),
       May = sum(May),
       June = sum(June)) %>%
     mutate(Name = BIAs[i])
  
   ff_monthly_target = ff_yoy_target %>%
     summarise(
       April = sum(April),
       May = sum(May),
       June = sum(June)) %>%
     mutate(Name = BIAs[i])
  
  } else if (quarter == "Q3"){
   ff_monthly_2019 = ff_pandemic %>%
     summarise(
       July = sum(July),
       August = sum(August),
       September = sum(September)) %>%
     mutate(Name = BIAs[i])
  
   ff_monthly_target = ff_yoy_target %>%
     summarise(
       July = sum(July),
       August = sum(August),
       September = sum(September)) %>%
     mutate(Name = BIAs[i])
  
  } else {
   ff_monthly_2019 = ff_pandemic %>%
     summarise(
       October = sum(October),
       November = sum(November),
       December = sum(December)) %>%
     mutate(Name = BIAs[i])
  
   ff_monthly_target = ff_yoy_target %>%
     summarise(
       October = sum(October),
       November = sum(November),
       December = sum(December)) %>%
     mutate(Name = BIAs[i])
  }

  ### VISIT TYPES TABLE -------------------------------------------------------
  # Get the Daytime location data in a similar process to the evening location data
  
  if (year == 2023){
    #### YEAR OVER YEAR CHANGE
    # CURRENT YEAR
    # get the file directory for the target data set
    file_directory = "./Data/Quarterly_Foottraffic/2023/"
    filename_yoy_target_cdl = paste0(BIAs[i], "23CDL.csv", sep = "")
    filename_yoy_target_cdl = paste0(file_directory, filename_yoy_target_cdl, sep = "")
    # load the data files
    ff_yoy_target_cdl = read_csv(filename_yoy_target_cdl)
    # filter the months used based on the Quarter
    if (quarter == "Q1"){
      ff_yoy_target_cdl = ff_yoy_target_cdl %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, January, February, March, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(January > 0 | February > 0 | March > 0) %>%
        mutate(Quarter_Visits = January + February + March)
      
    } else if (quarter == "Q2"){
      ff_yoy_target_cdl = ff_yoy_target_cdl %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, April, May, June, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(April > 0 | May > 0 | June > 0) %>%
        mutate(Quarter_Visits = April + May + June)
      
    } else if (quarter == "Q3"){
      ff_yoy_target_cdl = ff_yoy_target_cdl %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, July, August, September, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(July > 0 | August > 0 | September > 0) %>%
        mutate(Quarter_Visits = July + August + September)
        
    } else {
      ff_yoy_target_cdl = ff_yoy_target_cdl %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, October, November, December, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(October > 0 | November > 0 | December > 0) %>%
        mutate(Quarter_Visits = October + November + December)
    }
    # PREVIOUS YEAR
    # get the file directory for the target data set control (same quarter of the previous year)
    file_directory = paste0("./Data/Quarterly_Foottraffic/", as.character(year - 1), "/", sep = "")
    filename_yoy_control_cdl = paste0(BIAs[i], as.character(data_year - 1),"CDL.csv", sep = "")
    filename_yoy_control_cdl = paste0(file_directory, filename_yoy_control_cdl, sep = "")
    # load the data file
    ff_yoy_control_cdl = read_csv(filename_yoy_control_cdl)
    # filter the months used based on the Quarter
    if (quarter == "Q1"){
      ff_yoy_control_cdl = ff_yoy_control_cdl %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, January, February, March, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(January > 0 | February > 0 | March > 0) %>%
        mutate(Quarter_Visits = January + February + March)
      
    } else if (quarter == "Q2"){
      ff_yoy_control_cdl = ff_yoy_control_cdl %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, April, May, June, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(April > 0 | May > 0 | June > 0) %>%
        mutate(Quarter_Visits = April + May + June)
      
    } else if (quarter == "Q3"){
      ff_yoy_control_cdl = ff_yoy_control_cdl %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, July, August, September, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(July > 0 | August > 0 | September > 0) %>%
        mutate(Quarter_Visits = July + August + September)
      
    } else {
      ff_yoy_control_cdl = ff_yoy_control_cdl %>%
        select(GeofenceName, CDL_LATITUDE, CDL_LONGITUDE, October, November, December, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
               EarlyMorning, MorningCommute, LateMorning, Midday, EveningCommute, Evening, LateEvening) %>%
        filter(October > 0 | November > 0 | December > 0) %>%
        mutate(Quarter_Visits = October + November + December)
    }
  }
  
  # load the BIA shapefile for the BIA
  file_directory_area = paste("./Data/BIAs", BIAs[i], sep = "/")
  study_area = st_read(file_directory_area)
  study_area = st_buffer(study_area, 1000)
  
  
  # use the generate_visit_types function to calculate the types of visits taking place
  ff_type_target = generate_visitor_type(ff_yoy_target, ff_yoy_target_cdl, year, study_area)
  ff_type_control = generate_visitor_type(ff_yoy_control, ff_yoy_control_cdl, (year - 1), study_area)
  

  ### EXPORT THE CHARTS -------------------------------------------------------

  #### Foot Traffic -----------------------------------------------------
  
  # year-over-year change
  ff_year_over_year = generate_foottraffic_change(ff_yoy_target, ff_yoy_control, BIAs[i])
  ff_year_over_year = ff_year_over_year %>%
    mutate(time_window = "Year Over Year")
  # pre constructions
  ff_preconstruction = generate_foottraffic_change(ff_yoy_target, ff_construction, BIAs[i])
  ff_preconstruction = ff_preconstruction %>%
    mutate(time_window = "Construction Start")
  # pre pandemic
  ff_prepandemic = generate_foottraffic_change(ff_yoy_target, ff_pandemic, BIAs[i])
  ff_prepandemic = ff_prepandemic %>%
    mutate(time_window = "Pre Pandemic")
  # last quarter
  ff_lastquarter = generate_foottraffic_change(ff_yoy_target, ff_last_quarter, BIAs[i])
  ff_lastquarter = ff_lastquarter %>%
     mutate(time_window = "Last Quarter")
  # combine and export
  ff_table = bind_rows(ff_year_over_year, ff_preconstruction, ff_prepandemic, ff_lastquarter)
  
  # CREATE A LARGE SHEET WITH EVERY STUDY AREA FOR A WEB APPLICATION
  # Add the year and quarter to the study area
  ff_table = ff_table %>%
   mutate(Year = year,
          Quarter = quarter)
  # create new data frame or append based on iteration
  if (i == 1){
   ff_table_all = ff_table
  } else {
   ff_table_all = bind_rows(ff_table_all, ff_table)
  }
  
  
  #### Visit Type -----------------------------------------------------
  
  ff_type = bind_rows(ff_type_target, ff_type_control)
  # Add a the and quarter and name
  ff_type = ff_type %>%
    mutate(Name = BIAs[i],
           Quarter = quarter)
  
  # create a new data frame or append based on the iteration
  if (i == 1){
    ff_type_all = ff_type
  } else {
    ff_type_all = bind_rows(ff_type_all, ff_type)
  }
  
  # plot the chart
  output_directory = paste0("Output/QuarterlyDashboard/", BIAs[i], "/ff_visittype", quarter, as.character(data_year), ".jpg", sep = "")
  ggplot(ff_type_all, aes(x = as.character(Year),
                           y = Count, fill = factor(Type, levels = c("Infrequent Visitor", "Recurring Visitor", "Resident")))) +
    geom_bar(position = "stack",  stat = "identity", width = 0.7) +
    scale_color_manual(values = c("#CC2936", "#002A41", "#00AEF3"), aesthetics = c("colour", "fill")) +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
    labs(title = paste0("Visit Count by Type of Visitor: ", quarter), x = "Year", y = "Visits (Thousands)") +
    theme(panel.background = element_rect(fill = 'transparent', colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = 'gray80'),
          plot.background = element_rect(fill = 'transparent', colour = NA),
          plot.title = element_text(size = 9),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 7),
          axis.text = element_text(size = 7),
          legend.text = element_text(size = 7),
          legend.key.size = unit(0.5, "cm"),
          legend.title = element_blank(),
          legend.margin = margin(c(0,0,0,0)),
          legend.position = "bottom")
  ggsave(output_directory, dpi = 500, height = 8.4, width = 10.9, units = "cm")

  #### Day of the Week --------------------------------------------------------
  ff_day_of_week = generate_day_of_week_count(ff_yoy_target, ff_yoy_control, year)
  ff_day_of_week = ff_day_of_week %>%
    mutate(Name = BIAs[i])
  
  # create new data frame or append based on iteration
  if (i == 1){
    ff_day_of_week_all = ff_day_of_week
  } else {
    ff_day_of_week_all = bind_rows(ff_day_of_week_all, ff_day_of_week)
  }
  
  # plot the chart
  output_directory = paste0("Output/QuarterlyDashboard/", BIAs[i], "/ff_dayofweek", quarter, as.character(data_year), ".jpg", sep = "")
  ggplot(ff_day_of_week, aes(x = factor(Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                             y = Visits, fill = Year)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.7) +
    scale_color_manual(values = c("#002A41", "#00AEF3"), aesthetics = c("fill", "color")) +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
    labs(title = paste0("Visits by Day of Week: ", quarter), x = "Day of the Week", y = "Visits (Thousands)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 9),
      axis.title.y = element_text(size = 7),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 7),
      legend.text = element_text(size = 7),
      legend.title = element_blank(),
      legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(c(0,0,0,0)),
      legend.position = "bottom")
  ggsave(output_directory, dpi = 500, height = 3.9, width = 10.9, units = 'cm')

  
  #### Time of the Day --------------------------------------------------------
  ff_time_of_day = generate_time_of_day_count(ff_yoy_target, ff_yoy_control, year)
  ff_time_of_day = ff_time_of_day %>%
    mutate(Name = BIAs[i])
  
  # create new data frame or append based on iteration
  if (i == 1){
    ff_time_of_day_all = ff_time_of_day
  } else {
    ff_time_of_day_all = bind_rows(ff_time_of_day_all, ff_time_of_day)
  }
    
  # plot the chart
  output_directory = paste0("Output/QuarterlyDashboard/", BIAs[i], "/ff_timeofday", quarter, as.character(data_year), ".jpg", sep = "")
  ggplot(ff_time_of_day, aes(x = factor(Time, levels = c("Early Morning: 12am - 6am", "Morning: 6am - 12pm", "Afternoon: 12pm - 6pm", "Evening: 6pm - 12am")),
                             y = Visits, fill = Year)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.9) +
    scale_color_manual(values = c("#002A41", "#00AEF3"), aesthetics = c("fill", "color")) +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
    labs(title = paste0("Visits by Time of Day: ", quarter), x = "Time of Day", y = "Visits (Thousands)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 9),
      axis.title.y = element_text(size = 7),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(size = 7, angle = 7),
      legend.position = "none")
  ggsave(output_directory, dpi = 500, height = 3.9, width = 10.9, units = 'cm')


  #### Monthly visit level ----------------------------------------------------

  # get the monthly relative change compared to 2019
  ff_monthly_change = generate_relative_foottraffic(ff_monthly_target, ff_monthly_2019, year)

  # update the foot traffic data set and re-export
  # retrieve the last ff_monthly sheet from the output folder
  file_directory = paste0("Output/QuarterlyDashboard/", BIAs[i], "/", "ff_monthly.csv", sep = "")
  ff_monthly = read.csv(file_directory) %>% select(-X) %>% mutate(date = as.Date(date))
  ff_monthly = bind_rows(ff_monthly, ff_monthly_change)

  # export the updated ff_monthly data
  output_directory = paste0("./Output/QuarterlyDashboard/", BIAs[i], "/", "ff_monthly.csv", sep = "")
  write.csv(ff_monthly, file = output_directory)
  
  start_date = as.Date("2020-01-01")

  # plot the chart
  output_directory = paste0("Output/QuarterlyDashboard/", BIAs[i], "/ff_monthly", quarter, as.character(data_year), ".jpg", sep = "")
  ggplot(ff_monthly, aes(x = date, y = Percentage)) +
    geom_hline(yintercept = 100, color = "#000000") +
    geom_line(size = 1.5, color = "#00AEF3") +
    ylim(0, 180) +
    labs(title = "Visitor Levels (%) Relative to 2019", x = "Month", y = "Percentage (%)") +
    scale_x_date(limits = c(start_date, max(ff_monthly$date)), date_breaks = "3 month", date_labels = "%b %Y") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 9),
      axis.title.y = element_text(size = 7),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 7, angle = 30),
      axis.text.y = element_text(size = 7),
      legend.position = "none")
  ggsave(output_directory, dpi = 500, height = 8.1, width = 10.9, units = 'cm')

  # CREATE A LARGE SHEET WITH EVERY STUDY AREA FOR A WEB APPLICATION

  # add the month and quarter for filtering
  ff_monthly = ff_monthly %>%
    mutate(Area = BIAs[i])

  if (i == 1){
    ff_monthly_all = ff_monthly
  } else {
    ff_monthly_all = bind_rows(ff_monthly_all, ff_monthly)
  }


  ## COMMERCIAL REAL ESTATE ---------------------------------------------------
  variable_names = c("Area")

  #### Market Rent ------------------------------------------------------------

  # filter out the that selected BIA
  marketrent_filtered = marketrent %>%
    filter(!!sym(variable_names[1]) == BIAs[i])

  # filter the target and control months
  marketrent_target = marketrent_filtered %>%
    filter(Period >= target_start_date & Period <= target_end_date) %>%
    rename("targetrent" = MarketRent)

  marketrent_control = marketrent_filtered %>%
    filter(Period >= control_start_date & Period <= control_end_date) %>%
    rename("controlrent" = MarketRent) %>%
    select(-Area, -Period)

  # bind the columns and take the percentage difference
  marketrent_target = marketrent_target %>%
    bind_cols(., marketrent_control) %>%
    mutate(yoy_growth = ((targetrent - controlrent) / controlrent) * 100)



  #### Vacancy Rate -----------------------------------------------------------

  # filter out the that selected BIA
  vacancyrate_filtered = vacancyrate %>%
    filter(!!sym(variable_names[1]) == BIAs[i])

  # filter the target and control months
  vacancyrate_target = vacancyrate_filtered %>%
    filter(Period >= target_start_date & Period <= target_end_date) %>%
    rename("targetvacancy" = VacancyRate)

  vacancyrate_control = vacancyrate_filtered %>%
    filter(Period >= control_start_date & Period <= control_end_date) %>%
    rename("controlvacancy" = VacancyRate) %>%
    select(-Area, -Period)

  # bind the columns and take the percentage difference
  vacancyrate_target = vacancyrate_target %>%
    bind_cols(., vacancyrate_control) %>%
    mutate(yoy_growth = ((targetvacancy - controlvacancy) / controlvacancy) * 100)


  #### Export the Data --------------------------------------------------------
  # CREATE A LARGE SHEET WITH EVERY STUDY AREA FOR A WEB APPLICATION
    # add the month and quarter for filtering
  vacancyrate_target = vacancyrate_target %>%
    mutate(Year = year,
           Quarter = quarter)

  if (i == 1){
    vacancyrate_all = vacancyrate_target
  } else {
    vacancyrate_all = bind_rows(vacancyrate_all, vacancyrate_target)
  }

  # add the month and quarter for filtering
  marketrent_target = marketrent_target %>%
    mutate(Year = year,
           Quarter = quarter)

  if (i == 1){
    marketrent_all = marketrent_target
  } else {
    marketrent_all = bind_rows(marketrent_all, marketrent_target)
  }



  ## TRAVEL TIME DATA ---------------------------------------------------------

  # filter the data based on the given years, and BIA
  BIA = BIAs[i]
  tt_filtered = travel_time %>%
    filter((year_ == year | year_ == year - 1) & study_area == BIA)
  # filter based on the quarter
  if (quarter == "Q1"){
    tt_filtered = tt_filtered %>%
      filter(month_ < 4)
  } else if (quarter == "Q2"){
    tt_filtered = tt_filtered %>%
      filter(month_ >= 4 & month_ < 7)
  } else if (quarter == "Q3") {
    tt_filtered = tt_filtered %>%
      filter(month_ >= 7 & month_ < 10)
  } else {
    tt_filtered = tt_filtered %>% filter(month_ >=  10)
  }

  # group by the travel direction, year and time of day then take the average travel time
  # make sure you update the
  tt_grouped = tt_filtered %>% group_by(direction, year_, period_name) %>%
    summarise(avg_tt_s = mean(avg_tt_s, na.rm = TRUE)) %>%
    pivot_wider(names_from = c(year_, period_name), values_from = avg_tt_s) %>%
    rename("AM_Control" = `2022_AM Peak Hour`, "PM_Control" = `2022_PM Peak Hour`, "AM_Target" = `2023_AM Peak Hour`, "PM_Target" = `2023_PM Peak Hour`) %>%
    mutate(AM_Diff = AM_Target - AM_Control,
           PM_Diff = PM_Target - PM_Control)
  
  # output the excel file

    # CREATE A LARGE SHEET WITH EVERY STUDY AREA FOR A WEB APPLICATION
    # add the month and quarter for filtering
  tt_grouped = tt_grouped %>%
    mutate(Area = BIAs[i],
           Year = year,
           Quarter = quarter)
  if (i == 1){
    tt_grouped_all = tt_grouped
  } else {
    tt_grouped_all = bind_rows(tt_grouped_all, tt_grouped)
  }

  
}


# EXPORT DASHBOARD FILES -----------------------------------------------------

# Load in the previous data files
ff_table_meta = read_csv("./Output/OutputData/ff_table_meta.csv") %>%
  select(-...1)
marketrent_meta = read_csv("./Output/OutputData/marketrent_meta.csv") %>%
  select(-...1)
vacancyrate_meta = read_csv("./Output/OutputData/vacancyrate_meta.csv") %>%
  select(-...1)
tt_grouped_meta = read_csv("./Output/OutputData/tt_grouped_meta.csv") %>%
  select(-...1)
ff_day_of_week_meta = read_csv("./Output/OutputData/ff_day_of_week_meta.csv") %>%
  select(-...1) %>%
  mutate(Year = as.character(Year))
ff_time_of_day_meta = read_csv("./Output/OutputData/ff_time_of_day_meta.csv") %>%
  select(-...1) %>%
  mutate(Year = as.character(Year))
ff_type_meta = read_csv("./Output/OutputData/ff_type_meta.csv") %>%
  select(-...1)
# combine the previous and current data
ff_table_meta = bind_rows(ff_table_meta, ff_table_all)
marketrent_meta = bind_rows(marketrent_meta, marketrent_all)
vacancyrate_meta = bind_rows(vacancyrate_meta, vacancyrate_all)
tt_grouped_meta = bind_rows(tt_grouped_meta, tt_grouped_all)
ff_day_of_week_meta = bind_rows(ff_day_of_week_meta, ff_day_of_week_all)
ff_time_of_day_meta = bind_rows(ff_time_of_day_meta, ff_time_of_day_all)
ff_type_meta = bind_rows(ff_type_meta, ff_type_all)

# export the new data files
write.csv(ff_monthly_all, "./Output/OutputData/ff_monthly_meta.csv")
write.csv(ff_table_meta, "./Output/OutputData/ff_table_meta.csv")
write.csv(marketrent_meta, "./Output/OutputData/marketrent_meta.csv")
write.csv(vacancyrate_meta, "./Output/OutputData/vacancyrate_meta.csv")
write.csv(tt_grouped_meta, "./Output/OutputData/tt_grouped_meta.csv")
write.csv(ff_day_of_week_meta, "./Output/OutputData/ff_day_of_week_meta.csv")
write.csv(ff_time_of_day_meta, "./Output/OutputData/ff_time_of_day_meta.csv")
write.csv(ff_type_meta, "./Output/OutputData/ff_type_meta.csv")


# PART 2: ALL BIA SPEND METRICS -----------------------------------------------

## Moneris Spend Data ---------------------------------------------------------
moneris_file_date = paste0("Moneris", as.character(year), ".csv", sep = "")
moneris_file_directory = "./Data/MonerisSpend/"
moneris_file_directory = paste0(moneris_file_directory, moneris_file_date, sep = "")
spend_data = read.csv(moneris_file_directory)

# convert the week_no to a date that can be used to visualize the data
spend_data = spend_data %>%
  mutate(date = case_when(
    week_no == 1 ~ ymd(paste0(year, "01", "30")), week_no == 2 ~ ymd(paste0(year, "02", "06")),
    week_no == 3 ~ ymd(paste0(year, "02", "13")), week_no == 4 ~ ymd(paste0(year, "02", "20")),
    week_no == 5 ~ ymd(paste0(year, "02", "27")), week_no == 6 ~ ymd(paste0(year, "03", "06")),
    week_no == 7 ~ ymd(paste0(year, "03", "13")), week_no == 8 ~ ymd(paste0(year, "03", "20")),
    week_no == 9 ~ ymd(paste0(year, "03", "27")), week_no == 10 ~ ymd(paste0(year, "04", "03")),
    week_no == 11 ~ ymd(paste0(year, "04", "10")), week_no == 12 ~ ymd(paste0(year, "04", "17")),
    week_no == 13 ~ ymd(paste0(year, "04", "24")), week_no == 14 ~ ymd(paste0(year, "05", "01")),
    week_no == 15 ~ ymd(paste0(year, "05", "08")), week_no == 16 ~ ymd(paste0(year, "05", "15")),
    week_no == 17 ~ ymd(paste0(year, "05", "22")), week_no == 18 ~ ymd(paste0(year, "05", "29")),
    week_no == 19 ~ ymd(paste0(year, "06", "05")), week_no == 20 ~ ymd(paste0(year, "06", "12")),
    week_no == 21 ~ ymd(paste0(year, "06", "19")), week_no == 22 ~ ymd(paste0(year, "06", "26")),
    week_no == 23 ~ ymd(paste0(year, "07", "03")), week_no == 24 ~ ymd(paste0(year, "07", "10")),
    week_no == 25 ~ ymd(paste0(year, "07", "17")), week_no == 26 ~ ymd(paste0(year, "07", "24")),
    week_no == 27 ~ ymd(paste0(year, "07", "31")), week_no == 28 ~ ymd(paste0(year, "08", "07")),
    week_no == 29 ~ ymd(paste0(year, "08", "14")), week_no == 30 ~ ymd(paste0(year, "08", "21")),
    week_no == 31 ~ ymd(paste0(year, "08", "28")), week_no == 32 ~ ymd(paste0(year, "09", "04")),
    week_no == 33 ~ ymd(paste0(year, "09", "11")), week_no == 34 ~ ymd(paste0(year, "09", "18")),
    week_no == 35 ~ ymd(paste0(year, "09", "25")), week_no == 36 ~ ymd(paste0(year, "10", "02")),
    week_no == 37 ~ ymd(paste0(year, "10", "09")), week_no == 38 ~ ymd(paste0(year, "10", "16")),
    week_no == 39 ~ ymd(paste0(year, "10", "23")), week_no == 40 ~ ymd(paste0(year, "10", "30")),
    week_no == 41 ~ ymd(paste0(year, "11", "06")), week_no == 42 ~ ymd(paste0(year, "11", "13")),
    week_no == 43 ~ ymd(paste0(year, "11", "20")), week_no == 44 ~ ymd(paste0(year, "11", "27")),
    week_no == 45 ~ ymd(paste0(year, "12", "04")), week_no == 46 ~ ymd(paste0(year, "12", "11")),
    week_no == 47 ~ ymd(paste0(year, "12", "18")), week_no == 48 ~ ymd(paste0(year, "12", "25")),
    week_no == 49 ~ ymd(paste0(year, "01", "02")), week_no == 50 ~ ymd(paste0(year, "01", "09")),
    week_no == 51 ~ ymd(paste0(year, "01", "16")), week_no == 52 ~ ymd(paste0(year, "01", "23"))
  ))

# convert the yoy_vol_growth into a percentage out of 100
spend_data = spend_data %>%
  mutate(yoy_vol_Growth = yoy_vol_Growth * 100,
         yoy_txn_Growth = yoy_txn_Growth * 100)

# FILTER THE SPEND DATA FOR THE TARGET QUARTER
if (quarter == "Q1"){
  spend_target = spend_data %>%
    filter(week_no <= 9 | week_no >= 49) %>%
    select(date, yoy_vol_Growth, yoy_txn_Growth)
} else if (quarter == "Q2"){
  spend_target = spend_data %>%
    filter(week_no >= 10 & week_no <= 22) %>%
    select(date, yoy_vol_Growth, yoy_txn_Growth)
} else if (quarter == "Q3"){
  spend_target = spend_data %>%
    filter(week_no >= 23 & week_no <= 35) %>%
    select(date, yoy_vol_Growth, yoy_txn_Growth)
} else{
  spend_target = spend_data %>%
    filter(week_no >= 36 & week_no <= 48) %>%
    select(date, yoy_vol_Growth, yoy_txn_Growth)
}

output_directory = paste0("Output/QuarterlyDashboard/weeklyspend", quarter, as.character(data_year), ".jpg", sep = "")
# PLOT THE WEEK OVER WEEK TIME SERIES SPEND DATA
ggplot(spend_target, aes(x = date, y = yoy_vol_Growth)) +
  geom_hline(yintercept = 0, color = "#000000") +
  geom_line(color = "#002A41") +
  geom_point(color = "#00AEF3", fill = "#00AEF3", size = 2) +
  labs(title = paste0("Weekly Year-over-Year change in Sales Volume Growth: ", quarter), x = "Week", y = "Percentage (%)") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  theme(
    panel.background = element_rect(fill = 'transparent', colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = 'gray80'),
    plot.background = element_rect(fill = 'transparent', colour = NA),
    plot.title = element_text(size = 9),
    axis.title.y = element_text(size = 7),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 7, angle = 17),
    axis.text.y = element_text(size = 7),
    legend.position = "none")
ggsave(output_directory, dpi = 500, height = 8.1, width = 10.9, units = 'cm')


# Calculate the Average Yoy_Growth for that quarter
spend_target_avg = spend_target %>%
  summarise(
    vol_growth_avg = mean(yoy_vol_Growth),
    txn_growth_avg = mean(yoy_txn_Growth))
# Add the study area at the start of the table

spend_target_avg = spend_target_avg %>%
  mutate(Name = "All BIAs") %>%
  select(Name, everything())

## Toronto CMA Spend Data -----------------------------------------------------
spend_data_CoT = read_csv("./Data/Retail_Sales.csv")

# filter that start and end dates for the regional spend
regional_spend_target = spend_data_CoT %>%
  filter(date >= target_start_date & date <= target_end_date) %>%
  rename("sales_target" = sales) %>%
  summarise(
    sales_target = mean(sales_target)
  )
regional_spend_control = spend_data_CoT %>%
  filter(date >= control_start_date & date <= control_end_date) %>%
  rename("sales_control" = sales) %>%
  summarise(
    sales_control = mean(sales_control) 
  )

# combine the columns and take the year over year change
regional_spend = bind_cols(regional_spend_target, regional_spend_control)

# get a number for the yoy_vol_growth
regional_spend = regional_spend %>%
  mutate(Name = "Toronto CMA",
         vol_growth_avg = ((sales_target - sales_control) / sales_control) * 100) %>%
  select(Name, vol_growth_avg)

spend_report = bind_rows(spend_target_avg, regional_spend)

# output the excel chart
output_directory = paste0("Output/QuarterlyDashboard/quarterlyspend", quarter, as.character(data_year), ".csv", sep = "")
write.csv(spend_report, file = output_directory)


## StatsCan RTLBCI ------------------------------------------------------------

RTLBCI = read_csv("./Data/RTLBCI/RTLBCI.csv")

# filter the start and end dates
RTLBCI_target = RTLBCI %>%
  filter(Date >= target_start_date & Date <= target_end_date) %>%
  summarise(
    index_target = mean(RTLBCI)
  )

RTLBCI_control = RTLBCI %>%
  filter(Date >= control_start_date & Date <= control_end_date) %>%
  summarise(
    index_control = mean(RTLBCI)
  )

RTLBCI_report = bind_cols(RTLBCI_target, RTLBCI_control)
RTLBCI_report = RTLBCI_report %>%
  mutate(Name = "Toronto CMA",
         index_avg = ((index_target - index_control) / index_control) * 100) %>%
  select(Name, index_avg)

# output the excel chart
output_directory = paste0("Output/QuarterlyDashboard/RTLBCI", quarter, as.character(data_year), ".csv", sep = "")
write.csv(RTLBCI_report, file = output_directory)

