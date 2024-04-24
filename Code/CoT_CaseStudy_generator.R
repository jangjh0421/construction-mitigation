# The goal of this script will be to further refine and automate the metrics, internal maps and
# formatted data sets needed to run the cases studies for all of the study regions for Canada. Instead of going section
# by section like the first iteration of the rmd script functions will be made to define catch ment areas
# create metrics and charts, create internal facing maps and lists for field research, and format datasets for the web platform

# first we will create all of the necessary functions needed then add the code to run a re-producable case study based on input


## PART 1: LOAD PACKAGES ------------------------------------------------------

library(tidyverse)
library(cancensus)
library(ggrepel)
library(tibbletime)
library(units)
library(sf)
library(tmap)
library(tiff)
library(jsonlite)
options(scipen = 999)

## PART 2: WORKFLOW FUNCTIONS -------------------------------------------------

# These functions will be used in multiple sections within the case study generation script

#### Define Catchment Area
# Get the demographic buffer for the Area and return a list of DAs or Postal Codes to be used with the EA dataset
define_catchment_area = function(spatial_boundary, study_boundary, spatial_type){
  # preform an intersection between the desired sptial and study boundary
  catchment_int = st_intersection(spatial_boundary, study_boundary)
  # get a list of DAs or postal codes based on the spatial_type
  if(spatial_type == "PC"){
    catchment_list = catchment_int$POSTALCODE
  }
  else{
    catchment_list = catchment_int$DAUID
  }
  # return the list
  return(catchment_list)
}


#### Create Infrastructure Density
# Get the density of business and civic infrastructure
create_infrastructure_density = function(area_count, boundary_area){
  # divide the count by the area
  inf_density = sum(area_count$n) / boundary_area
  return(inf_density)
}


#### generate infrastructure composition
# get the composition of civic and business types for the study area, walkshed and region
generate_infrastructure_composition = function(inf_locations, inf_list, inf_type, study_area, walkshed, CSD, study_area_name, region_name, if_first){
  # preform an intersection between the inf_locations and the study area
  inf_intersection = function(location_shp, spatial_area){
    location_int = st_intersection(location_shp, spatial_area) %>% select(Unique.Identifier, Business.Name, Street.address, City, Province, Postal.Code, EmpSizeNum, Industry_Code, Description, Group)
    return(location_int)
  }
  
  # get the inf type and filter according to the NAICS code used
  if (inf_type == "civic"){
    CivInf_list = inf_list$NAICS_6
    inf_locations = inf_locations %>% subset(NAICS_6 %in% CivInf_list)
    inf_locations = inf_locations %>% left_join(inf_list, by = "NAICS_6")
    inf_locations = inf_locations %>% rename("Industry_Code" = NAICS_6)
  } else{
    BusInf_list = inf_list$NAICS_4
    inf_locations = inf_locations %>% subset(NAICS_4 %in% BusInf_list)
    inf_locations = inf_locations %>% left_join(inf_list, by = "NAICS_4")
    inf_locations = inf_locations %>% rename("Industry_Code" = NAICS_4)
    # filter out large institutions based on employment count
    filter_condition = inf_locations %>% filter(Industry_Code == 5221 & EmpSizeNum > 50 | Industry_Code == 5412 & EmpSizeNum > 50 | Industry_Code == 5411 & EmpSizeNum > 50 | Industry_Code == 5312 & EmpSizeNum > 50 |
                                                  Industry_Code == 5173 & EmpSizeNum > 50 )
    inf_locations = inf_locations %>% anti_join(filter_condition, by = "Unique.Identifier")
  }
  
  # turn the inf_locations into a spatial structure and join the infrastructure types
  inf_locations = st_as_sf(inf_locations, coords = c("Longitude.X.coordinate", "Latitude.Y.coordinate"), crs = 4326)
  inf_project = inf_locations %>% st_transform(crs = 3347) %>% select(Unique.Identifier, Business.Name, Street.address, City, Province, Postal.Code, EmpSizeNum, Industry_Code, Description, Group)
  
  # preform intersections with the three different spatial scales
  inf_region_int = inf_intersection(inf_project, CSD)
  inf_walkshed_int = inf_intersection(inf_project, walkshed)
  inf_studyarea_int = inf_intersection(inf_project, study_area)

  # get the count and composition for each scale
  inf_region_count = inf_region_int %>% count(Group) %>% mutate(Percentage = n / sum(n) * 100, Area = region_name)
  inf_walkshed_count = inf_walkshed_int %>% count(Group) %>% mutate(Percentage = n / sum(n) * 100, Area = "Ten Minute Walk")
  inf_studyarea_count = inf_studyarea_int %>% count(Group) %>% mutate(Percentage = n / sum(n) * 100, Area = study_area_name)
  
  # bind rows and return
  inf_join = bind_rows(inf_region_count, inf_walkshed_count, inf_studyarea_count) %>% st_drop_geometry()
  
  # return the joined list and the study area intersection for an internal map
  return(list(inf_join = inf_join, inf_studyarea_int = inf_studyarea_int))
}


## PART 3: SECTION FUNCTIONS --------------------------------------------------

# These functions will be called for each specific section of the case study generation


### SECTION 1: SUMMARY --------------------------------------------------------
# generates statistics on population, households, Age, Income and Commuting Mode Share
generate_summary = function(demo_summary, catchment_list, summary_area_demo){
  # generate the demographic overview for the study area
  Demos_studyarea = demo_summary %>% subset(Key %in% catchment_list)
  summary_tibble = tibble(Variables = c("Population", "Households", "Density" ,"Unit Density", "Average Age", "Average Income"),
                          Stat = c(sum(Demos_studyarea$ECYBASPOP), sum(Demos_studyarea$ECYBASHHD), sum(Demos_studyarea$ECYBASPOP / summary_area_demo), sum(Demos_studyarea$ECYBASHHD / summary_area_demo),
                                   mean(Demos_studyarea$ECYPTAAVG), mean(Demos_studyarea$ECYPNIAVG)))
  
  
  # generate the commuting mode overview for the study area
  commute_studyarea = Demos_studyarea %>% select(Key, ECYTRADRIV, ECYTRAPSGR, ECYTRAPUBL, ECYTRABIKE, ECYTRAWALK)
  
  commute_studyarea = commute_studyarea %>% mutate(Pop_Commute = sum(across(all_of(c(2:6)))),
                                                   across(all_of(c(2:6)), ~sum(.)),
                                                   across(all_of(c(2:6)), ~(. / Pop_Commute * 100))) %>% distinct(ECYTRADRIV, .keep_all = TRUE)
  commute_tibble = tibble(Variables = c("Driver", "Public Transit", "Bike", "Walk"),
                          Stat = c(commute_studyarea$ECYTRADRIV + commute_studyarea$ECYTRAPSGR, commute_studyarea$ECYTRAPUBL, commute_studyarea$ECYTRABIKE, commute_studyarea$ECYTRAWALK))
  
  
  # column-bind rows and export
  return(list(studyarea_summary = summary_tibble, commute_summary = commute_tibble))
}


### SECTION 3: CIVIC INFRASTRUCTURE -------------------------------------------

# generates a table and plot of civic infrastructure composition for the study area
generate_civic_infrastrcuture = function(civic_locations, civic_list, study_area, walkshed, CSD, study_area_name, region_name, if_first, study_area_area){
  
  # call the generate infrastructure composition composition
  print('Generating Civic Composition....')
  Civic_data = generate_infrastructure_composition(civic_locations, civic_list, "civic", study_area, walkshed, CSD, study_area_name, region_name, if_first)
  print('Composition Generated!')
  
  # Extract the items from the generate infrastructure composition function
  Civic_join = Civic_data$inf_join
  Civic_studyarea_shp = Civic_data$inf_studyarea_int
  print('Creating Civic Composition Chart....')
  
  # create the chart for the section
  ggplot(Civic_join, aes(x = factor(Area, levels = c(study_area_name, "Ten Minute Walk", region_name)),
                         y = Percentage, fill = factor(Group, levels = c("Arts and Culture", "Education", "Health and Care Facilities", "Government and Community Services","Recreation Facilities")))) +
    geom_bar(position = "stack", stat = "identity", width = 0.6) +
    geom_text(aes(label = paste(round(Percentage, 1), " %")), position = "stack", vjust = 1.25, color = "#000000", size = 3) +
    scale_color_manual(values = c("#DB3069", "#F45D01", "#33AED7", "#8A4285", "#43B171"), aesthetics = c("colour", "fill")) +
    labs(title = "Share of Civic Infrastructure", x = "Group Name", y = "Percentage %") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 6),
      title = element_blank(),
      legend.position = "none",
      legend.title = element_blank())
  ggsave('./charts/civinf.jpg', dpi = 500, height = 9.0, width = 14.53, units = 'cm')
  print('Chart Exported!')
  
  # generate the infrastructure density
  civ_density = create_infrastructure_density(Civic_join %>% filter(Area == study_area_name), study_area_area)
  
  # return the summary
  return(list(civ_summary = Civic_join, civ_density = civ_density))
}




### SECTION 4: HOUSING --------------------------------------------------------

# generates the data and internal visuals for Housing Data and Metrics
generate_housing = function(DemosHousing, Catchment_DAs_list, Region_DAs_list, studyarea_name, region_name, CSD_id){
  
  
  #### SECTION SPECIFIC FUNCTIONS ---------------------------------------------
  
  
  # calculate the housing tenure for the region and study area
  calculate_housing_tenure = function(DemosHousing, Catchment_list, area_name){
    
    # pull housing data for the study area
    housing_tenure = DemosHousing %>% select(Key, ECYTENOWN, ECYTENRENT) %>% rename(Owner = ECYTENOWN, Renter = ECYTENRENT)
    housing_tenure = housing_tenure %>% subset(Key %in% Catchment_list)
    
    # calculate study area percentage
    housing_tenure = housing_tenure %>% mutate(DA_Pop = select(., 2:3) %>% rowSums(na.rm = TRUE),
                                               pop_per = DA_Pop / sum(DA_Pop) * 100,
                                               across(all_of(c(2:3)), ~(. / DA_Pop * 100)))
    housing_tenure = housing_tenure %>% mutate(across(all_of(c(2:3)), ~weighted.mean(., w = pop_per, na.rm = TRUE))) %>% distinct(Owner, .keep_all = TRUE)
    
    # format the rows
    housing_tenure = housing_tenure %>% select(-Key, -DA_Pop, -pop_per) %>% mutate(Area = area_name)
    housing_tenure = housing_tenure %>% pivot_longer(!Area, names_to = "Tenure", values_to = "Percentage")
    return(housing_tenure)
  }
  
  
  
  # calculate the dwelling construction year for the region and study area
  calculate_housing_year = function(DemosHousing, Catchment_list, area_name){
    
    # pull housing data for the study area
    housing_year = DemosHousing %>% select(Key, ECYPOCP60, ECYPOC6180, ECYPOC8190, ECYPOC9100, ECYPOC0105, ECYPOC0610, ECYPOC1115, ECYPOC1621, ECYPOC22P) %>% 
      rename("Pre 1960" = ECYPOCP60, "1961-1980" = ECYPOC6180, "1981-1990" = ECYPOC8190, "1991-2000" = ECYPOC9100, "2001-2005" = ECYPOC0105, "2006-2010" = ECYPOC0610,
             "2011-2015" = ECYPOC1115, "2016-2021" = ECYPOC1621, "After 2021" = ECYPOC22P)
    housing_year = housing_year %>% subset(Key %in% Catchment_list)
    
    # calculate study area percentage
    housing_year = housing_year %>% mutate(DA_Pop = select(., 2:10) %>% rowSums(na.rm = TRUE),
                                           pop_per = DA_Pop / sum(DA_Pop) * 100,
                                           across(all_of(c(2:10)), ~(. / DA_Pop * 100)))
    housing_year = housing_year %>% mutate(across(all_of(c(2:10)), ~weighted.mean(., w = pop_per, na.rm = TRUE))) %>% distinct(`Pre 1960`, .keep_all = TRUE)
    
    # format the rows
    housing_year = housing_year %>% select(-Key, -DA_Pop, -pop_per) %>% mutate(Area = area_name)
    housing_year = housing_year %>% pivot_longer(!Area, names_to = "Construction Year", values_to = "Percentage")
    return(housing_year)
  }
  
  
  
  # calculate the housing structure
  calculate_housing_structure = function(DemosHousing, Catchment_list, area_name){
    
    # pull housing data for the study area
    housing_structure = DemosHousing %>% select(Key, ECYSTYSING, ECYSTYSEMI, ECYSTYROW, ECYSTYAP5P, ECYSTYAPU5, ECYSTYDUPL) %>% 
      rename("Single-Detached" = ECYSTYSING, "Semi-Detached" = ECYSTYSEMI, "Row" = ECYSTYROW, "High-rise Apartment" = ECYSTYAP5P, "Low-rise Apartment" = ECYSTYAPU5, "Duplex" = ECYSTYDUPL)
    housing_structure = housing_structure %>% subset(Key %in% Catchment_list)
    
    # calculate study area percentage
    housing_structure = housing_structure %>% mutate(DA_Pop = select(., 2:7) %>% rowSums(na.rm = TRUE),
                                                     pop_per = DA_Pop / sum(DA_Pop) * 100,
                                                     across(all_of(c(2:7)), ~(. / DA_Pop * 100)))
    housing_structure = housing_structure %>% mutate(across(all_of(c(2:7)), ~weighted.mean(., w = pop_per, na.rm = TRUE))) %>% distinct(`Row`, .keep_all = TRUE)
    
    # format the rows
    housing_structure = housing_structure %>% select(-Key, -DA_Pop, -pop_per) %>% mutate(Area = area_name)
    housing_structure = housing_structure %>% pivot_longer(!Area, names_to = "Housing Type", values_to = "Percentage")
    return(housing_structure)
  }
  
  
  
  # calculate the housing value by pulling the data from the 2021 census
  calculate_housing_value = function(census_level, Catchment_list, area_name, CSD_id){
    
    # pull the dwelling value data from census mapper api
    housing_value = get_census(dataset = 'CA21', regions = list(CSD = CSD_id),
                               vectors = c("v_CA21_4312", "v_CA21_4318"),
                               level = census_level, use_cache = FALSE, geo_format = NA)
    
    # format the CSD and study area and calculate value
    if(census_level == 'CSD'){
      housing_value = housing_value %>% rename("Dwelling Value" = `v_CA21_4312: Average value of dwellings ($) (60)`,
                                               "Monthly Rent" = `v_CA21_4318: Average monthly shelter costs for rented dwellings ($) (59)`)
      housing_value = housing_value %>% select(-GeoUID, -Type, -`Region Name`, -`Area (sq km)`, -Population, -Dwellings, -Households,
                                               -CD_UID, -PR_UID, -CMA_UID) %>%
        mutate(Area = area_name)
    }
    else{
      housing_value = housing_value %>% subset(GeoUID %in% Catchment_DAs_list)
      housing_value = housing_value %>% rename("Dwelling Value" = `v_CA21_4312: Average value of dwellings ($) (60)`,
                                               "Monthly Rent" = `v_CA21_4318: Average monthly shelter costs for rented dwellings ($) (59)`)
      housing_value = housing_value %>% mutate(Household_per = Households / sum(Households), Dwelling_per = Dwellings / sum(Dwellings))
      housing_value = housing_value %>% mutate(`Dwelling Value` = weighted.mean(`Dwelling Value`, w = Household_per, na.rm = TRUE),
                                               `Monthly Rent` = weighted.mean(`Monthly Rent`, w = Dwelling_per, na.rm = TRUE)) %>% distinct(`Dwelling Value`, .keep_all = TRUE)
      housing_value = housing_value %>% select(-GeoUID, -Type, -`Region Name`, -`Area (sq km)`, -Population, -Dwellings, -Households,
                                               -CSD_UID, -CD_UID, -CT_UID, -CMA_UID, -Household_per, -Dwelling_per) %>% mutate(Area = area_name)
    }
    
    # return the housing value data
    return(housing_value)
  }
  
  
  #### SECTION DATA VIZ -------------------------------------------------------
  
  
  
  # Part 1: Housing Tenure
  # calculate housing tenure
  housing_tenure_studyarea = calculate_housing_tenure(DemosHousing, Catchment_DAs_list, studyarea_name)
  housing_tenure_region = calculate_housing_tenure(DemosHousing, Region_DAs_list, region_name)
  
  # bind rows
  housing_tenure_join = bind_rows(housing_tenure_studyarea, housing_tenure_region)
  
  # plot chart
  ggplot(housing_tenure_join, aes(x = factor(Area, levels = c(studyarea_name, region_name)), y = Percentage, fill = Tenure)) +
    geom_bar(position = "stack",  stat = "identity", width = 0.8) +
    geom_text(aes(label = round(Percentage, 1)), position = "stack", vjust = 2, color = "#f1f1f1", size = 3) +
    scale_color_manual(values = c("#00AEF3", "#002A41"), aesthetics = c("colour", "fill")) +
    labs(title = "Housing Tenure", x = "Area", y = "Percentage (%)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5),
      axis.text.y= element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.key.size = unit(0.5, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom")
  ggsave('./charts/housingtenure.jpg', dpi = 500, height = 7.53, width = 16.25, units = 'cm')
  print('Housing Tenure Complete!')
  
  
  
  # Part 2: Housing Construction Year
  # calculate housing year
  housing_year_studyarea = calculate_housing_year(DemosHousing, Catchment_DAs_list, studyarea_name)
  housing_year_region = calculate_housing_year(DemosHousing, Region_DAs_list, region_name)
  
  # bind rows
  housing_year_join = bind_rows(housing_year_studyarea, housing_year_region)
  
  # plot chart
  ggplot(housing_year_join, aes(x = factor(`Construction Year`, levels = c("Pre 1960", "1961-1980", "1981-1990", "1991-2000", "2001-2005", "2006-2010", "2011-2015", "2016-2021", "After 2021")),
                                y = Percentage, fill = factor(Area, levels = c(region_name, studyarea_name)))) +
    geom_bar(position = "dodge",  stat = "identity", width = 0.7) +
    scale_color_manual(values = c("#CC2936", "#00AEF3"), aesthetics = c("colour", "fill")) +
    labs(title = "Housing Construction Year", x = "Construction Year", y = "Percentage (%)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5, angle = 10),
      axis.text.y= element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.key.size = unit(0.5, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom")
  ggsave('./charts/housingyear.jpg', dpi = 500, height = 7.53, width = 16.25, units = 'cm')
  print('Housing Year Complete!')
  
  
  
  # Part 3: Housing Structure
  # calculate housing structure
  housing_structure_studyarea = calculate_housing_structure(DemosHousing, Catchment_DAs_list, studyarea_name)
  housing_structure_region = calculate_housing_structure(DemosHousing, Region_DAs_list, region_name)
  
  # bind rows and remove census codes
  housing_structure_join = bind_rows(housing_structure_studyarea, housing_structure_region)
  
  # plot chart
  ggplot(housing_structure_join, aes(x = factor(`Housing Type`, levels = c("Single-Detached", "Semi-Detached", "Row", "High-rise Apartment", "Low-rise Apartment", "Duplex")),
                                     y = Percentage, fill = factor(Area, levels = c(region_name, studyarea_name)))) +
    geom_bar(position = "dodge",  stat = "identity", width = 0.7) +
    scale_color_manual(values = c("#CC2936", "#00AEF3"), aesthetics = c("colour", "fill")) +
    labs(title = "Housing Structure", x = "Housing Structure", y = "Percentage %") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5, angle = 10),
      axis.text.y= element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.key.size = unit(0.5, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom")
  ggsave('./charts/housingstructure.jpg', dpi = 500, height = 7.53, width = 16.25, units = 'cm')
  print('Housing Structure Complete!')
  
  
  
  # Part 4: Housing Value
  # calculate housing value
  housing_value_studyarea = calculate_housing_value('DA', Catchment_DAs_list, study_area_name, CSD_id)
  housing_value_region = calculate_housing_value('CSD', Region_DAs_list, region_name, CSD_id)
  
  # bind rows
  housing_value_join = bind_rows(housing_value_studyarea, housing_value_region)
  housing_value_join = housing_value_join %>% pivot_longer(!Area, names_to = "Type", values_to = "Cost")
  
  # plot chart
  ggplot(housing_value_join, aes(x = Area, y = Cost, fill = factor(Area, levels = c(region_name, studyarea_name)))) +
    geom_bar(position = "dodge",  stat = "identity", width = 0.9) +
    geom_text(aes(label = paste("$ ", round(Cost, 0))), vjust = 2.5, position = position_dodge(width = 0.4), color = "#f1f1f1", size = 3) +
    scale_color_manual(values = c("#CC2936", "#00AEF3"), aesthetics = c("colour", "fill")) +
    facet_wrap(~Type, scales = "free") +
    labs(title = "Housing Value", x = "Area", y = "") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5, angle = 10),
      axis.text.y= element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.key.size = unit(0.5, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom")
  ggsave('./charts/housingvalue.jpg', dpi = 500, height = 7.53, width = 16.25, units = 'cm')
  print('Housing Value Complete!')
  
  
  
  # return the list of data for each element for web mapping
  return(list(housing_tenure = housing_tenure_join, housing_year = housing_year_join, housing_structure = housing_structure_join, housing_value = housing_value_join))
}


### SECTION 5: DEMOGRAPHIC PROFILE --------------------------------------------

# generate demographic profiles for the study area in comparison to the study region
generate_demographic_profile = function(DemoOutlook, Catchment_DAs_list, Region_DAs_list, studyarea_name, region_name, CSD_id){
  
  
  #### SECTION SPECIFIC FUNCTIONS ---------------------------------------------
  
  
  # calculate the age distribution of males and females for the study area and region
  calculate_age_distribution = function(DemoOutlook, Catchment_list, area_name, gender){
    
    # pull the data dependent on the gender needed
    if(gender == "male"){
      Demo_age = DemoOutlook %>% select(Key, ECYPMA_0_4, ECYPMA_5_9, ECYPMA1014, ECYPMA1519, ECYPMA2024, ECYPMA2529, ECYPMA3034, ECYPMA3539, ECYPMA4044, ECYPMA4549,
                                        ECYPMA5054, ECYPMA5559, ECYPMA6064, ECYPMA6569, ECYPMA7074, ECYPMA7579, ECYPMA8084, ECYPMA85P) %>%
        rename("0 to 4" = ECYPMA_0_4, "5 to 9" = ECYPMA_5_9, "10 to 14" = ECYPMA1014, "15 to 19" = ECYPMA1519, "20 to 24" = ECYPMA2024, "25 to 29" = ECYPMA2529, "30 to 34" = ECYPMA3034,
               "35 to 39" = ECYPMA3539, "40 to 44" = ECYPMA4044, "45 to 49" = ECYPMA4549, "50 to 54" = ECYPMA5054, "55 to 59" = ECYPMA5559, "60 to 64" = ECYPMA6064, "65 to 69" = ECYPMA6569,
               "70 to 74" = ECYPMA7074, "75 to 79" = ECYPMA7579, "80 to 84" = ECYPMA8084, "85 and Over" = ECYPMA85P)
      
      # subset based on catchment area
      Demo_age = Demo_age %>% subset(Key %in% Catchment_list)
      
      # calculate study area percentage
      Demo_age = Demo_age %>% mutate(DA_Pop = select(., 2:19) %>% rowSums(na.rm = TRUE),
                                     pop_per = DA_Pop / sum(DA_Pop) * 100,
                                     across(all_of(c(2:19)), ~(. / DA_Pop * 100)))
      Demo_age = Demo_age %>% mutate(across(all_of(c(2:19)), ~weighted.mean(., w = pop_per, na.rm = TRUE))) %>% distinct(`0 to 4`, .keep_all = TRUE)
      
      # format the rows
      Demo_age = Demo_age %>% select(-Key, -DA_Pop, -pop_per) %>% mutate(Gender = "Male")
      Demo_age = Demo_age %>% pivot_longer(!Gender, names_to = "Age", values_to = "Percentage") %>% mutate(Area = area_name)
    } else{
      
      # pull female age data for the study area
      Demo_age = DemoOutlook %>% select(Key, ECYPFA_0_4, ECYPFA_5_9, ECYPFA1014, ECYPFA1519, ECYPFA2024, ECYPFA2529, ECYPFA3034, ECYPFA3539, ECYPFA4044, ECYPFA4549,
                                        ECYPFA5054, ECYPFA5559, ECYPFA6064, ECYPFA6569, ECYPFA7074, ECYPFA7579, ECYPFA8084, ECYPFA85P) %>% 
        rename("0 to 4" = ECYPFA_0_4, "5 to 9" = ECYPFA_5_9, "10 to 14" = ECYPFA1014, "15 to 19" = ECYPFA1519, "20 to 24" = ECYPFA2024, "25 to 29" = ECYPFA2529, "30 to 34" = ECYPFA3034,
               "35 to 39" = ECYPFA3539, "40 to 44" = ECYPFA4044, "45 to 49" = ECYPFA4549, "50 to 54" = ECYPFA5054, "55 to 59" = ECYPFA5559, "60 to 64" = ECYPFA6064, "65 to 69" = ECYPFA6569,
               "70 to 74" = ECYPFA7074, "75 to 79" = ECYPFA7579, "80 to 84" = ECYPFA8084, "85 and Over" = ECYPFA85P)
      Demo_age = Demo_age %>% subset(Key %in% Catchment_list)
      
      # calculate study area percentage
      Demo_age = Demo_age %>% mutate(DA_Pop = select(., 2:19) %>% rowSums(na.rm = TRUE),
                                     pop_per = DA_Pop / sum(DA_Pop) * 100,
                                     across(all_of(c(2:19)), ~(. / DA_Pop * 100)))
      Demo_age = Demo_age %>% mutate(across(all_of(c(2:19)), ~weighted.mean(., w = pop_per, na.rm = TRUE))) %>% distinct(`0 to 4`, .keep_all = TRUE)
      
      # format the rows
      Demo_age = Demo_age %>% select(-Key, -DA_Pop, -pop_per) %>% mutate(Gender = "Female")
      Demo_age = Demo_age %>% pivot_longer(!Gender, names_to = "Age", values_to = "Percentage") %>% mutate(Area = area_name)
    }
    return(Demo_age)
  }
  
  
  
  # calculate the income distribution for the study area compared to the region
  calculate_income_distribution = function(census_level, Catchment_list, area_name, CSD_id){
    
    # pull the income distribution data from statscan
    Income_level = get_census(dataset = 'CA21', regions = list(CSD = CSD_id),
                              vectors = c("v_CA21_1106","v_CA21_1109","v_CA21_1112","v_CA21_1115",
                                          "v_CA21_1118","v_CA21_1124","v_CA21_1127","v_CA21_1130",
                                          "v_CA21_1133","v_CA21_1136"),
                              level = census_level, use_cache = FALSE, geo_format = NA)
    if(census_level == 'CSD'){
      
      # calculate the income deciles for the CSD
      pop_control = Income_level$Population
      Income_level = Income_level %>% select(-GeoUID, -Type, -`Region Name`, -`Area (sq km)`, -Population, -Dwellings, -Households,
                                             -CD_UID, -PR_UID, -CMA_UID) %>% 
        mutate(Area = "City of Toronto") %>%
        pivot_longer(!Area, names_to = "Income Decile", values_to = "Percentage")
      Income_level = Income_level %>% mutate(Percentage = Percentage / pop_control * 100)
    } else {
      
      # get the Catchment for the Study area
      Income_level = Income_level %>% subset(GeoUID %in% Catchment_list)
      
      # take the population percentage, percentage of each income decile and weighted average and Collapse to one row
      Income_level = Income_level %>% mutate(Total_Pop = select(., 12:21) %>% rowSums(na.rm = TRUE))
      Income_level = Income_level %>% mutate(pop_per = Total_Pop / sum(Total_Pop) * 100)
      Income_level = Income_level %>% mutate(across(all_of(c(12:21)), ~(. / Total_Pop * 100)))
      Income_level = Income_level %>% mutate(across(all_of(c(12:21)), ~weighted.mean(., w = pop_per, na.rm = TRUE))) %>% distinct(`v_CA21_1106: In bottom decile`, .keep_all = TRUE)
      Income_level = Income_level %>% select(-GeoUID, -Type, -`Region Name`, -`Area (sq km)`, -Population, -Dwellings, -Households,
                                             -CSD_UID, -CD_UID, -CT_UID, -CMA_UID, -Total_Pop, -pop_per) %>% mutate(Area = area_name)
      Income_level = Income_level %>%
        pivot_longer(!Area, names_to = "Income Decile", values_to = "Percentage")
    }
    return(Income_level)
  }
  
  
  
  # calculate_educational_attainment
  calculate_education = function(DemoOutlook, Catchment_list, area_name){
    
    # pull the variables for calculation
    Demo_edu_atten = DemoOutlook %>% select(Key, ECYEDANCDD, ECYEDAHSCE, ECYEDAATCD, ECYEDACOLL, ECYEDAUDBB, ECYEDAUD) %>%
      mutate(ECYEDAUDBB = ECYEDAATCD + ECYEDACOLL + ECYEDAUDBB) %>% select(-ECYEDAATCD, -ECYEDACOLL) %>%
      rename("No certificate" = ECYEDANCDD, "High school diploma" = ECYEDAHSCE, "Postsecondary certificate" = ECYEDAUDBB, "Bachelors degree or higher" = ECYEDAUD)
    Demo_edu_atten = Demo_edu_atten %>% subset(Key %in% Catchment_list)
    
    # calculate percentage
    Demo_edu_atten = Demo_edu_atten %>% mutate(DA_Pop = select(., 2:5) %>% rowSums(na.rm = TRUE),
                                               pop_per = DA_Pop / sum(DA_Pop) * 100,
                                               across(all_of(c(2:5)), ~(. / DA_Pop * 100)))
    Demo_edu_atten = Demo_edu_atten %>% mutate(across(all_of(c(2:5)), ~weighted.mean(., w = pop_per, na.rm = TRUE))) %>% distinct(`High school diploma`, .keep_all = TRUE)
    
    # format the rows
    Demo_edu_atten = Demo_edu_atten %>% select(-Key, -DA_Pop, -pop_per) %>% mutate(Area = area_name)
    Demo_edu_atten = Demo_edu_atten %>% pivot_longer(!Area, names_to = "Degree Attained", values_to = "Percentage")
    return(Demo_edu_atten)
  }
  
  
  
  # calculate_generational_status
  calculate_generational_status = function(DemoOutlook, Catchment_list, area_name){
    
    # pull the generational status data for the study area
    Demo_gen = DemoOutlook %>% select(Key, ECYGEN1GEN, ECYGEN2GEN, ECYGEN3GEN) %>%
      rename("First" = ECYGEN1GEN, "Second" = ECYGEN2GEN, "Third or more" = ECYGEN3GEN)
    Demo_gen = Demo_gen %>% subset(Key %in% Catchment_list)
    
    # calculate study area percentage
    Demo_gen = Demo_gen %>% mutate(DA_Pop = select(., 2:4) %>% rowSums(na.rm = TRUE),
                                   pop_per = DA_Pop / sum(DA_Pop) * 100,
                                   across(all_of(c(2:4)), ~(. / DA_Pop * 100)))
    Demo_gen = Demo_gen %>% mutate(across(all_of(c(2:4)), ~weighted.mean(., w = pop_per, na.rm = TRUE))) %>% distinct(`First`, .keep_all = TRUE)
    
    # format the rows
    Demo_gen = Demo_gen %>% select(-Key, -DA_Pop, -pop_per) %>% mutate(Area = area_name)
    Demo_gen = Demo_gen %>% pivot_longer(!Area, names_to = "Generation Status", values_to = "Percentage")
    return(Demo_gen)
  }
  
  
  #### SECTION DATA VIZ -------------------------------------------------------
  
  
  # Part 1: Population Pyramid
  print('Generating Population Pyramid...')
  
  # calculate the age distribution for the each gender and area
  Demo_age_male = calculate_age_distribution(DemoOutlook, Catchment_DAs_list, studyarea_name, "male")
  Demo_age_male_region = calculate_age_distribution(DemoOutlook, Region_DAs_list, region_name, "male")
  Demo_age_female = calculate_age_distribution(DemoOutlook, Catchment_DAs_list, studyarea_name, "female")
  Demo_age_female_region = calculate_age_distribution(DemoOutlook, Region_DAs_list, region_name, "female")
  
  # bind rows
  Age_Dist = bind_rows(Demo_age_male, Demo_age_male_region, Demo_age_female, Demo_age_female_region)
  
  # Plot the Population Pyramid
  ggplot(Age_Dist, aes(x = factor(Age, levels = c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39",
                                                  "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79",
                                                  "80 to 84", "85 and Over")), y = Percentage, fill = factor(Area, levels = c(region_name, studyarea_name)))) +
    # bars for the Region
    geom_bar(data = Age_Dist %>% filter(Gender == "Female", Area == region_name) ,stat = "identity", position = "identity", alpha = 0.66, fill = "#CC2936") +
    geom_bar(data = Age_Dist %>% filter(Gender == "Male", Area == region_name), stat = "identity", position = "identity", mapping = aes(y = -Percentage), alpha = 0.66, fill = "#CC2936") +
    # bars for the study area
    geom_bar(data = Age_Dist %>% filter(Gender == "Female", Area == studyarea_name) ,stat = "identity", position = "identity", alpha = 0.66, fill = "#00AEF3") +
    geom_bar(data = Age_Dist %>% filter(Gender == "Male", Area == studyarea_name), stat = "identity", position = "identity", mapping = aes(y = -Percentage), alpha = 0.66, fill = "#00AEF3") +
    # styling for the chart
    scale_y_continuous(labels = abs, limits = max(Age_Dist$Percentage) * c(-1,1)) +
    labs(title = "Population Pyramid", x = "Age Range", y = "Percentage (%)") +
    ylim(-30,30) +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 6),
      axis.text.x = element_text(size = 5,),
      axis.text.y= element_text(size = 5),
      legend.title = element_blank()) +
    coord_flip()
  ggsave('./charts/agepyramid.jpg', dpi = 500, height = 7.53, width = 10.78, units = 'cm')
  print('Population Pyramid Complete!')
  
  
  
  # Part 2: Income Deciles
  print('Generating Income Deciles...')
  
  # calculate the Income Deciles for the region and studyarea
  Income_Region = calculate_income_distribution('CSD', Region_DAs_list, region_name, CSD_id)
  Income_Studyarea = calculate_income_distribution('DA', Catchment_DAs_list, studyarea_name, CSD_id)
  
  # combine the data frame and calculate the percentage difference
  Income_join = bind_rows(Income_Studyarea, Income_Region)
  Income_join = Income_join %>% mutate(`Income Decile` = str_remove(`Income Decile`, ".*: "))
  
  # Plot the Income Chart compared to the National Average
  ggplot(Income_join, aes(x = factor(`Income Decile`, levels = c("In bottom decile", "In second decile", "In third decile",
                                                                 "In forth decile", "In fifth decile", "In sixth decile",
                                                                 "In seventh decile", "In eighth decile", "In ninth decile", "In top decile")),
                          y = Percentage, fill = factor(Area, levels = c(region_name, studyarea_name)))) +
    geom_bar(position = "dodge", stat = "identity", width = 0.7) +
    scale_color_manual(values = c("#CC2936", "#00AEF3"), aesthetics = c("colour", "fill")) +
    labs(title = "Income Decile", x = "Income Decile",
         y = "Percentage (%)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5, angle = 10),
      axis.text.y= element_text(size = 5),
      legend.text = element_text(size = 4),
      legend.key.size = unit(0.25, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom")
  ggsave('./charts/incomedecile.jpg', dpi = 500, height = 7.53, width = 10.78, units = 'cm')
  print('Income Deciles Completed!')
  
  
  
  # Part 3: Educational Attainment
  print('Generating Educational Attainment...')
  
  # calculate educational attainment for the studyarea and region
  Edu_Studyarea = calculate_education(DemoOutlook, Catchment_DAs_list, studyarea_name)
  Edu_Region = calculate_education(DemoOutlook, Region_DAs_list, region_name)
  
  # bind rows
  Demo_edu = bind_rows(Edu_Studyarea, Edu_Region)
  
  # Plot Chart
  ggplot(Demo_edu, aes(x = factor(`Degree Attained`, levels = c("No certificate", "High school diploma", "Postsecondary certificate", "Bachelors degree or higher")),
                       y = Percentage, fill = factor(Area, levels = c(region_name, studyarea_name))))+
    geom_bar(position = "dodge", stat = "identity", width = 0.9) +
    scale_color_manual(values = c("#CC2936", "#00AEF3"), aesthetics = c("colour", "fill")) +
    labs(title = "Highest Degree Attained for the Population", x = "Degree Attained",
         y = "Percentage (%)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5, angle = 10),
      axis.text.y= element_text(size = 5),
      legend.text = element_text(size = 4),
      legend.key.size = unit(0.25, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom")
  ggsave('./charts/education.jpg', dpi = 500, height = 7.53, width = 10.78, units = 'cm')
  print('Educational Attainment Complete!')
  
  
  
  # Part 4: Generation Status
  print('Generating Generational Status...')
  
  # calculate the generational status for the area and region
  Demo_studyarea = calculate_generational_status(DemoOutlook, Catchment_DAs_list, studyarea_name)
  Demo_region = calculate_generational_status(DemoOutlook, Region_DAs_list, region_name)
  
  # bind rows
  Demo_gen = bind_rows(Demo_studyarea, Demo_region)
  
  # plot chart
  ggplot(Demo_gen, aes(x = factor(`Generation Status`, levels = c("First", "Second","Third or more")), y = Percentage, fill = factor(Area, levels = c(region_name, studyarea_name))))+
    geom_bar(position = "dodge", stat = "identity", width = 0.9) +
    scale_color_manual(values = c("#CC2936", "#00AEF3"), aesthetics = c("colour", "fill")) +
    labs(title = "Canadian Generational Status", x = "Generation Status",
         y = "Percentage (%)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5, angle = 10),
      axis.text.y= element_text(size = 5),
      legend.text = element_text(size = 4),
      legend.key.size = unit(0.25, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom") +
    coord_flip()
  ggsave('./charts/genstatus.jpg', dpi = 500, height = 7.53, width = 10.78, units = 'cm')
  print('Generational Status Complete!')
  
  
  
  # return the files for the webmap
  return(list(Pop_pyramid = Age_Dist, Inc_Dec = Income_join, Edu_Attn = Demo_edu, Gen_stat = Demo_gen))
}



### SECTION 6: LOCAL BUSINESS PROFILE -----------------------------------------

generate_business_profile = function(business_locations, business_list, study_area, walkshed, CSD, studyarea_name, region_name, if_first, study_area_area){
  
  # call the generate infrastructure composition function
  print('Generating Business Composition...')
  Business_data = generate_infrastructure_composition(business_locations, business_list, "business", study_area, walkshed, CSD, studyarea_name, region_name, if_first)
  print('Composition Generated!')
  
  # Extract the items from the inf composition function
  Bus_join = Business_data$inf_join
  Business_studyarea_shp = Business_data$inf_studyarea_int
  print('Creating Business Composition Chart...')
  
  # create the chart for the section
  ggplot(Bus_join, aes(x = factor(Area, levels = c(studyarea_name, "Ten Minute Walk", region_name)),
                       y = Percentage, fill = factor(Group, levels = c("Services and Other", "Retail", "Food and Drink")))) +
    geom_bar(position = "stack", stat = "identity", width = 0.6) +
    geom_text(aes(label = paste(round(Percentage, 1), " %")), position = "stack", vjust = 2, color = "#000000", size = 4) +
    scale_color_manual(values = c("#2A5CAC", "#F13737", "#43B171"), aesthetics = c("colour", "fill")) +
    labs(title = "Percentage of Business Types", x = "Area", y = "Percentage") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 7),
      title = element_blank(),
      legend.position = "none",
      legend.title = element_blank())
  ggsave('./charts/businessinf.jpg', dpi = 500, height = 15.87, width = 13.54, units = 'cm')
  print('Chart Exported!')
  
  # generate the infrastructure density
  bus_density = create_infrastructure_density(Bus_join %>% filter(Area == studyarea_name), study_area_area)
  
  # return the summary
  return(list(bus_summary = Bus_join, bus_density = bus_density))
}



### SECTION 8: FOOT TRAFFIC AND VISITOR TYPES ---------------------------------

generate_visitor_foottraffic = function(ff_2019, ff_2020, ff_2021, ff_2022, ff_2023){
  
  
  #### SECTION SPECIFIC FUNCTIONS ---------------------------------------------
  
  
  # get visitor coordinates for a given study area and year
  get_visitor_coordinates = function(ff_traffic, study_year){
    
    # get the variable name
    year_name = paste("Daily_Visits_", study_year, sep = "")
    
    # round the coordinate and aggregate the points for each year
    ff_coords = ff_traffic %>% select(CEL_LATITUDE, CEL_LONGITUDE, Daily_Visits) %>% mutate(CEL_LATITUDE = round(CEL_LATITUDE, 4), CEL_LONGITUDE = round(CEL_LONGITUDE,4))
    ff_coords = ff_coords %>% group_by(CEL_LATITUDE, CEL_LONGITUDE) %>% summarise(Daily_Visits = sum(Daily_Visits))
    ff_coords = ff_coords %>% rename(!!year_name := Daily_Visits)
    
    # return the coordinates
    return(ff_coords)
  }
  
  
  
  # calculate the visitor type for each year
  calculate_visitor_type = function(foottraffic, year_date){
    
    # get the total number of visits
    Daily_Visits = sum(foottraffic$Daily_Visits)
    
    # get the number of workers using the Common Day Location
    # plot the CDL coordinates
    foottraffic_workers = foottraffic %>% filter(!is.na(CDL_LATITUDE)) 
    foottraffic_workers = st_as_sf(x = foottraffic_workers,
                                   coords = c("CDL_LONGITUDE", "CDL_LATITUDE"),
                                   crs = 4326)
    foottraffic_workers = foottraffic_workers %>% st_transform(crs = 3347)
    
    # remove workers for DAs where they would be defined as residents
    foottraffic_workers = foottraffic_workers %>% subset(!(CEL_PRCDDA %in% Study_area_DAs))
    
    # get the intersection of workers inside the study area
    workers_int = st_intersection(foottraffic_workers, StudyArea_Buff)
    
    # get the count of Daily Visits
    foottraffic_workers_count = sum(workers_int$Daily_Visits)
    
    # get the number of residents using the Common Evening Location
    # plot the CEL coordinates
    foottraffic_residents = foottraffic %>% filter(!is.na(CEL_LATITUDE)) 
    foottraffic_residents = st_as_sf(foottraffic_residents,
                                     coords = c("CEL_LONGITUDE", "CEL_LATITUDE"),
                                     crs = 4326)
    foottraffic_residents = foottraffic_residents %>% st_transform(crs = 3347)
    
    # get the intersection of residents inside the study area
    residents_int = st_intersection(foottraffic_residents, StudyArea_Buff)
    
    # get the count of Daily Visits
    foottraffic_residents_count = sum(residents_int$Daily_Visits)
    
    # Create a final tibble and export
    foottraffic_summary = tibble(
      Type = c("Resident", "Recurring Visitor", "Infrequent Visitor"),
      Year = c(year_date, year_date, year_date),
      Count = c(foottraffic_residents_count, foottraffic_workers_count, Daily_Visits - (foottraffic_residents_count + foottraffic_workers_count)))
    
    # Add percentage
    foottraffic_summary = foottraffic_summary %>% mutate(Percentage = Count / Daily_Visits * 100)
    return(foottraffic_summary)
  }
  
  
  
  # generate the monthly change in foottraffic for a given year
  generate_monthly_change = function(foot_traffic_baseline, foot_traffic_year, year_date){
    
    # get the foot traffic total for each month
    generate_montly_total = function(foot_traffic_total){
      
      # get the baseline total
      foot_traffic_total = foot_traffic_total %>% select(January, February, March, April, May, June, July, August, September, October, November, December)
      foot_traffic_total = colSums(foot_traffic_total)
      
      # create a baseline data
      foot_traffic_monthly_total = tibble(
        January = foot_traffic_total[1], February = foot_traffic_total[2], March = foot_traffic_total[3], April = foot_traffic_total[4], May = foot_traffic_total[5],
        June = foot_traffic_total[6], July = foot_traffic_total[7], August = foot_traffic_total[8], September = foot_traffic_total[9], October = foot_traffic_total[10],
        November = foot_traffic_total[11], December = foot_traffic_total[12])
      return(foot_traffic_monthly_total)
    }
    
    
    
    # get the monthly totals for the baseline and yearly data
    foot_traffic_baseline = generate_montly_total(foot_traffic_baseline)
    foot_traffic_year = generate_montly_total(foot_traffic_year)
    
    # divide the year by the baseline data
    ff_percentage = create_series(year_date ~ year_date, 'monthly', class = "Date") %>%
      mutate("Percentage" = c(foot_traffic_year$January / foot_traffic_baseline$January * 100, foot_traffic_year$February / foot_traffic_baseline$February * 100,
                              foot_traffic_year$March / foot_traffic_baseline$March * 100, foot_traffic_year$April / foot_traffic_baseline$April * 100,
                              foot_traffic_year$May / foot_traffic_baseline$May * 100, foot_traffic_year$June / foot_traffic_baseline$June * 100,
                              foot_traffic_year$July / foot_traffic_baseline$July * 100, foot_traffic_year$August / foot_traffic_baseline$August * 100,
                              foot_traffic_year$September / foot_traffic_baseline$September * 100, foot_traffic_year$October / foot_traffic_baseline$October * 100,
                              foot_traffic_year$November / foot_traffic_baseline$November * 100, foot_traffic_year$December / foot_traffic_baseline$December * 100),
             "Count" = c(foot_traffic_year$January, foot_traffic_year$February, foot_traffic_year$March, foot_traffic_year$April, foot_traffic_year$May, 
                         foot_traffic_year$June, foot_traffic_year$July, foot_traffic_year$August, foot_traffic_year$September, foot_traffic_year$October,
                         foot_traffic_year$November, foot_traffic_year$December))
    return(ff_percentage)
  }
  
  
  #### SECTION DATA VIZ -------------------------------------------------------
  
  
  # pre-step for web mapping: get the count of visits based on home coordinates
  ff_2019_coords = get_visitor_coordinates(ff_2019, "2019")
  ff_2020_coords = get_visitor_coordinates(ff_2020, "2020")
  ff_2021_coords = get_visitor_coordinates(ff_2021, "2021")
  ff_2022_coords = get_visitor_coordinates(ff_2022, "2022")
  
  # merge the data sets across the years
  ff_coords_merged = merge(ff_2019_coords, ff_2020_coords, by = c("CEL_LATITUDE", "CEL_LONGITUDE"), all = TRUE)
  ff_coords_merged = merge(ff_coords_merged, ff_2021_coords, by = c("CEL_LATITUDE", "CEL_LONGITUDE"), all = TRUE)
  ff_coords_merged = merge(ff_coords_merged, ff_2022_coords, by = c("CEL_LATITUDE", "CEL_LONGITUDE"), all = TRUE)
  
  # replace NAs with 0 and export the file
  ff_coords_merged = ff_coords_merged %>% replace(is.na(.), 0)
  
  
  
  # Part 1: Create Visitor Type Chart and EXport Data in csv
  ff_type_19 = calculate_visitor_type(ff_2019, "2019")
  ff_type_20 = calculate_visitor_type(ff_2020, "2020")
  ff_type_21 = calculate_visitor_type(ff_2021, "2021")
  ff_type_22 = calculate_visitor_type(ff_2022, "2022")
  
  # bind rows together and visualize
  ff_type_join = bind_rows(ff_type_19, ff_type_20, ff_type_21, ff_type_22)
  
  ggplot(ff_type_join, aes(x = factor(Year, levels = c("2019", "2020", "2021", "2022")),
                           y = Count, fill = factor(Type, levels = c("Infrequent Visitor", "Recurring Visitor", "Resident")))) +
    geom_bar(position = "stack",  stat = "identity", width = 0.7) +
    geom_text(aes(label = round(Percentage, 1)), position = "stack", vjust = 2, size = 3, color = "#f1f1f1") +
    scale_color_manual(values = c("#CC2936", "#002A41", "#00AEF3"), aesthetics = c("colour", "fill")) +
    labs(title = "Visit Count by Type of Visitor", x = "Year", y = "Visits") +
    theme(panel.background = element_rect(fill = 'transparent', colour = NA),
          panel.grid.minor = element_line(color = 'gray80'),
          panel.grid.major = element_line(color = 'gray80'),
          plot.background = element_rect(fill = 'transparent', colour = NA),
          plot.title = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 7),
          axis.text = element_text(size = 5),
          legend.text = element_text(size = 5),
          legend.key.size = unit(0.5, "cm"),
          legend.title = element_blank(),
          legend.position = "bottom")
  ggsave('./charts/visittypology.jpg', dpi = 500, height = 15.95, width = 14.74, units = 'cm')
  
  
  # Part 2: Generate Monthly Change in Foot traffic
  # generate the relative foot traffic for each year
  ff_percentage_20 = generate_monthly_change(ff_2019, ff_2020, "2020")
  ff_percentage_21 = generate_monthly_change(ff_2019, ff_2021, "2021")
  ff_percentage_22 = generate_monthly_change(ff_2019, ff_2022, "2022")
  
  # bind rows
  ff_percentage = rbind(ff_percentage_20, ff_percentage_21)
  ff_percentage = rbind(ff_percentage, ff_percentage_22)
  
  # visualize plot
  ggplot(ff_percentage, aes(x = date, y = Percentage)) +
    geom_line(size = 1.5, color = "#002A41") +
    ylim(0, 160) +
    labs(title = "Foot Traffic Relative to 2019", x = "Month", y = "Percentage (%)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 6),
      axis.text.y= element_text(size = 6),
      legend.title = element_blank())
  ggsave('./charts/monthlychange.jpg', dpi = 500, height = 7.53, width = 16.25, units = 'cm')
  
  
  
  # Part 3: Get the foot traffic year over year
  # get the relative yearly summary
  ff_summary = tibble("date" = c("2020", "2021", "2022"),
                      "Percentage" = c((sum(ff_2020$Daily_Visits) / sum(ff_2019$Daily_Visits)) * 100,
                                       (sum(ff_2021$Daily_Visits) / sum(ff_2019$Daily_Visits)) * 100,
                                       (sum(ff_2022$Daily_Visits) / sum(ff_2019$Daily_Visits)) * 100))
  
  # plot the chart
  ggplot(ff_summary, aes(x = date, y = Percentage)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.6, fill = "#002a41") +
    geom_text(aes(label = round(Percentage, 1)), position = "stack", vjust = 2, color = "#f1f1f1", size = 3) +
    labs(title = "Yearly Foot Traffic (Relative to 2019)", x = "Year", y = "Percentage (%)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 5),
      axis.text = element_text(size = 5),
      legend.position = "none",
      legend.title = element_blank())
  ggsave('./charts/yearlychange.jpg', dpi = 500, height = 5.7, width = 14.06, units = 'cm')
  
  # return all the data frames 
  return(list(ff_coordinate_heatmap = ff_coords_merged, ff_visitor_types = ff_type_join, ff_monthly = ff_percentage, ff_yearly = ff_summary))
}




### SECTION 9: GENERATE VISITOR CHARACTERISTICS -------------------------------

generate_visitor_characteristics = function(ff_2019, ff_2020, ff_2021, ff_2022, StudyArea_shp){
  
  
  #### SECTION SPECIFIC FUNCTIONS ------------------------------------------------
  
  
  # generate the visitor distance for each year
  generate_visitor_distance = function(ff_year, Centroid_point, study_year){
    # plot the CEL coordinates for the given year
    st_vis = ff_year %>% filter(!is.na(CEL_LATITUDE)) 
    st_vis = st_as_sf(x = st_vis,
                      coords = c("CEL_LONGITUDE", "CEL_LATITUDE"),
                      crs = 4326)
    
    # calculate the distance
    st_vis = st_vis %>% mutate(
      dist = st_distance(geometry, Centroid_point),
      distance = as.numeric(dist))
    
    # group visitors in distance bands
    st_vis = st_vis %>% mutate(dis_band = case_when(
      distance < 1000 ~ "Under 1km",
      distance >= 1000 & distance < 5000 ~ "1 - 5km",
      distance >= 5000 & distance < 10000 ~ "5 - 10km",
      distance >= 10000 & distance < 25000 ~ "10 - 25km",
      distance >= 25000 & distance < 50000 ~ "25 - 50km",
      distance >= 50000 ~ "Over 50km"))
    
    # get the weighted count of visits based on distance
    st_vis_count = st_vis %>% group_by(dis_band) %>% summarise(visits = sum(Daily_Visits),
                                                               visitors = sum(Unique_Visitors))
    st_vis_count = st_vis_count %>% mutate(Percentage_visits = visits / sum(visits) * 100,
                                           Percentage_visitors = visitors / sum(visitors) * 100,
                                           Year = study_year) %>% st_drop_geometry()
    
    # return visitor distance
    return(st_vis_count)
  }
  
  
  
  # generate the monthly change in median visit distance for each case study
  generate_average_visit_distance = function(foot_traffic_year, Centroid_point, study_year){
    # get a list of months
    month = c("January", "February", "March", "April", "May", "June", "July", "August",
               "September", "October", "November", "December")
    
    # create a loop that calculates the visitor distance
    for (i in 1: length(month)){
      # filter visitors for the given month
      foot_traffic_total_month = foot_traffic_year %>% filter(!!sym(month[i]) > 0)
      
      # plot the CEL coordinates for the given year
      st_vis = foot_traffic_total_month %>% filter(!is.na(CEL_LATITUDE)) 
      st_vis = st_as_sf(x = st_vis,
                        coords = c("CEL_LONGITUDE", "CEL_LATITUDE"),
                        crs = 4326)
      
      # calculate the distance
      st_vis = st_vis %>% mutate(
        dist = st_distance(geometry, Centroid_point),
        distance = as.numeric(dist))
      
      # get the average and return
      avg_distance = weighted.mean(st_vis$distance, st_vis$Daily_Visits)
        
      # create a tibble to hold all values for the year
      if (i == 1){
        visit_dist = tibble("Month" = c(month[i]), "average_dist" = avg_distance)
      } else{
        temp = tibble("Month" = c(month[i]), "average_dist" = avg_distance)
        visit_dist = bind_rows(visit_dist, temp)
      }
    }
    
    # Add the year
    temp = create_series(study_year ~ study_year, 'monthly', class = "Date")
    visit_dist = bind_cols(temp, visit_dist) %>% select(-Month)
    
    # return the average visit distance for the year
    return(visit_dist)
  }
  
  
  #### SECTION DATA VIZ -------------------------------------------------------
  
  
  # Part 1: Calculate the visitor distance from 2019 and 2022
  # convert the study area to degrees
  StudyArea_shp = StudyArea_shp %>% st_transform(crs = 4326)
  print('shape transformed')
  
  # get the centroid of the study area
  sf_use_s2(FALSE)
  StudyArea_centroid = st_centroid(StudyArea_shp)
  StudyArea_centroid = StudyArea_centroid %>% extract(geometry, c('Cen_Latitude', 'Cen_Longitude'), '\\((.*), (.*)\\)', convert = TRUE)
  centroid_lat = StudyArea_centroid$Cen_Latitude
  centroid_lon = StudyArea_centroid$Cen_Longitude
  Centroid_point = st_sfc(st_point(c(centroid_lat, centroid_lon))) %>%
    st_set_crs(4326)
  print('centroid found')
  
  # get the visitor distance for the corresponding years
  vis_count_19 = generate_visitor_distance(ff_2019, Centroid_point, "2019")
  vis_count_22 = generate_visitor_distance(ff_2022, Centroid_point, "2022")
  
  # join rows
  vis_count_join = bind_rows(vis_count_19, vis_count_22)
  
  # plot chart
  ggplot(vis_count_join, aes(x = factor(dis_band, levels = c("Under 1km", "1 - 5km", "5 - 10km", "10 - 25km", "25 - 50km", "Over 50km")),
                             y = Percentage_visits, fill = Year)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.7) +
    geom_point(aes(x = factor(dis_band, levels = c("Under 1km", "1 - 5km", "5 - 10km", "10 - 25km", "25 - 50km", "Over 50km")),
                   y = Percentage_visitors, fill = Year, shape = Year), color = 'black', size = 3) +
    scale_color_manual(values = c("#002A41", "#00AEF3"), aesthetics = c("fill", "color")) +
    scale_shape_manual(values=21:22) +
    labs(title = "Percentage of Visits (Bars) compared to the Percentage of Unique Visitors (Dots)", x = "Distance Band", y = "Percentage (%)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5),
      axis.text.y= element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.key.size = unit(0.5, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom")
  ggsave('./charts/visitordistance.jpg', dpi = 500, height = 7.53, width = 16.25, units = 'cm')
  print('distance generated')
  
  
  
  # Part 2: Calculate the average visit distance
  
  # get the average overall distance for each given year
  vis_avg_dist_19 = generate_average_visit_distance(ff_2019, Centroid_point = Centroid_point, "2019")
  vis_avg_dist_20 = generate_average_visit_distance(ff_2020, Centroid_point = Centroid_point, "2020")
  vis_avg_dist_21 = generate_average_visit_distance(ff_2021, Centroid_point = Centroid_point, "2021")
  vis_avg_dist_22 = generate_average_visit_distance(ff_2022, Centroid_point = Centroid_point, "2022")
  
  # join rows
  vis_avg = bind_rows(vis_avg_dist_19, vis_avg_dist_20, vis_avg_dist_21, vis_avg_dist_22)
  
  # plot the overall graph
  # visualize plot
  ggplot(vis_avg, aes(x = date, y = average_dist)) +
    geom_line(size = 1.5, color = "#00709C") +
    labs(title = "Average Visit Distance", x = "Month", y = "Percentage") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      axis.title.x = element_blank(),
      legend.title = element_blank())
  ggsave('./charts/averagedistance.jpg', dpi = 500, height = 7.53, width = 16.25, units = 'cm')
  
  
  
  # Part 3: Count the percentage of visits per day of the week
  vis_day_count = tibble(Day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                         Visits = c(sum(ff_2022$Sunday), sum(ff_2022$Monday), sum(ff_2022$Tuesday), sum(ff_2022$Wednesday), sum(ff_2022$Thursday),
                                    sum(ff_2022$Friday), sum(ff_2022$Saturday)))
  
  # get the percentage
  vis_day_count = vis_day_count %>% mutate(Percentage = Visits / sum(Visits) * 100)
  
  # plot the chart
  ggplot(vis_day_count, aes(x = factor(Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                            y = Percentage)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.7, fill = "#002A41") +
    labs(title = "Visits (%) by Day of Week", x = "Day of the Week", y = "Percentage (%)") +
    ylim(0, 25) +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5),
      axis.text.y= element_text(size = 5),
      legend.position = "none",
      legend.title = element_blank())
  ggsave('./charts/dayofweek.jpg', dpi = 500, height = 7.53, width = 16.25, units = 'cm')
  
  
  # Part 4: Count the percentage of visits per time of day
  # create a tibble of visits for each day of the week
  vis_time_count = tibble(Time = c("12am - 6am", "6am - 9am", "9am - 12pm", "12pm - 3pm", "3pm - 6pm", "6pm - 9pm", "9pm - 12am"),
                          Visits = c(sum(ff_2022$EarlyMorning), sum(ff_2022$MorningCommute), sum(ff_2022$LateMorning), sum(ff_2022$Midday), sum(ff_2022$EveningCommute),
                                     sum(ff_2022$Evening), sum(ff_2022$LateEvening)))
  
  # get the percentage
  vis_time_count = vis_time_count %>% mutate(Percentage = Visits / sum(Visits) * 100)
  
  # plot the chart
  ggplot(vis_time_count, aes(x = factor(Time, levels = c("12am - 6am", "6am - 9am", "9am - 12pm", "12pm - 3pm", "3pm - 6pm", "6pm - 9pm", "9pm - 12am")),
                             y = Percentage)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.7, fill = "#00AEF3") +
    labs(title = "Visits (%) by Time of Day", x = "Time of Day", y = "Percentage (%)") +
    ylim(0, 25) +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5),
      axis.text.y= element_text(size = 5),
      legend.position = "none",
      legend.title = element_blank())
  ggsave('./charts/timeofday.jpg', dpi = 500, height = 7.53, width = 16.25, units = 'cm')
  
  # return the data frame for charting in the webmap
  return(list(vis_distance = vis_count_join, vis_avg_dist = vis_avg, vis_dayofweek = vis_day_count, vis_timeofday = vis_time_count))
}




### SECTION 10: GREENSPACE ----------------------------------------------------

generate_greenspace_analysis = function(parks, study_area, walkshed, CSD, study_area_name, region_name){
  
  
  #### SECTION SPECIFIC FUNCTIONS ---------------------------------------------
  
  
  # create a function to preform an intersection between the parks and the spatial area
  inf_intersection = function(location_shp, spatial_area){
    location_int = st_intersection(location_shp, spatial_area) %>% select(osm_id, leisure)
    return(location_int)
  }
  
  
  
  # create a function that calculates the percentage of an area covered by green space
  park_area_percentage = function (parks_int, spatial_area){
    # get the total area of the parks
    parks_int = parks_int %>% mutate(area = st_area(parks_int))
    # get area sum and drop units
    parks_total_area = drop_units(sum(parks_int$area))
    # calculate the boundary area
    boundary_total_area = drop_units(st_area(spatial_area))
    # calculate the area percentage
    parks_percentage = parks_total_area / boundary_total_area * 100
    return(list(parks_area = parks_total_area, parks_percentage = parks_percentage))
  }
  
  
  #### SECTION DATA VIZ -------------------------------------------------------
  
  
  # preform the intersections with the two different scales, as the parks shapefile is already just for the intended region
  park_walkshed_int = inf_intersection(parks, walkshed)
  park_studyarea_int = inf_intersection(parks, study_area)
  parks_region_int = inf_intersection(parks, CSD)
  
  # get the percentage of the desired area covered by green space
  park_studyarea = park_area_percentage(park_studyarea_int, study_area)
  park_walkshed = park_area_percentage(park_walkshed_int, walkshed)
  park_region = park_area_percentage(parks_region_int, CSD)
  
  # create a final data frame including the study area name, total park area, and percentage covered
  parks_final = tibble(Area = c(study_area_name, "Ten Minute Walk", region_name),
                       Park_Area = c(park_studyarea$parks_area, park_walkshed$parks_area, park_region$parks_area),
                       Park_Percentage = c(park_studyarea$parks_percentage, park_walkshed$parks_percentage, park_region$parks_percentage))
  
  # plot the chart
  ggplot(parks_final, aes(x = factor(Area, levels = c(study_area_name, "Ten Minute Walk", region_name)), y = Park_Percentage)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.6, fill = "#2CB80A") +
    geom_text(aes(label = paste(round(Park_Percentage, 1), " %")), position = "stack", vjust = 1.75, color = "#ffffff", size = 3) +
    labs(title = "Percentage of Area Covered by Parks or Greenspace", x = "Area", y = "Percentage (%)") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.text = element_text(size = 5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 6),
      legend.title = element_blank())
  ggsave('./charts/greenspace.jpg', dpi = 500, height = 6.54, width = 14.53, units = 'cm')
  
  return(parks_final)
}


### SECTION 11: FAMILY STRCUTURE ----------------------------------------------

# generate the household structure for the study area in comparison to the study region
generate_family_structure = function(DemosFamilyStrcuture, Catchment_DAs_list, Region_DAs_list, studyarea_name, region_name){
  
  
  #### SECTION SPECIFIC FUNCTIONS ---------------------------------------------
  
  
  # calculate the family structure for the study area in comparison to the region
  calculate_family_structure = function(FamilyStrcuture, Catchment_list, area_name){
    
    # pull the family structure data
    Demos_family = DemosFamilyStrcuture %>% rename("Not Married Total" = ECYMARNMCL, "Single" = ECYMARSING, "Census Families" = ECYCFSCF,"Couple Family Total" = ECYCFSC,
                                                   "Couple without Children" = ECYCFSCNC, "Couple with Children" = ECYCFSCWC, "Lone-Parent Families" = ECYCFSLP)
    Demos_family = Demos_family %>% subset(Key %in% Catchment_list)
    
    # calculate the DA-level percentages
    Demos_tibble = tibble ("Family Structure" = c("Single", "Couple without Children", "Couple with Children", "Lone-Parent Families"),
                           "Percentage" = c(sum(Demos_family$Single) / sum(Demos_family$`Not Married Total`) * 100,
                            sum(Demos_family$`Couple without Children`) / sum(Demos_family$`Couple Family Total`) * 100,
                            sum(Demos_family$`Couple with Children`) / sum(Demos_family$`Census Families`) * 100,
                            sum(Demos_family$`Lone-Parent Families`) / sum(Demos_family$`Census Families`) * 100),
                           "Area" = c(area_name))
    
    return(Demos_tibble)
  }
  
  
  #### SECTION DATA VIZ -------------------------------------------------------
  
  # calculate family structure percentage
  family_structure_studyarea = calculate_family_structure(DemosFamilyStrcuture, Catchment_DAs_list, studyarea_name)
  family_structure_region = calculate_family_structure(DemosFamilyStrcuture, Region_DAs_list, region_name)
  
  # bind rows
  family_structure_join  = bind_rows(family_structure_studyarea, family_structure_region)
  
  # plot chart
  ggplot(family_structure_join, aes(x = factor(`Family Structure`, levels = c("Single", "Couple without Children", "Couple with Children", "Lone-Parent Families")),
                                    y = Percentage, fill = factor(Area, levels = c(region_name, studyarea_name)))) +
    geom_bar(position = "dodge",  stat = "identity", width = 0.7) +
    scale_color_manual(values = c("#CC2936", "#00AEF3"), aesthetics = c("colour", "fill")) +
    labs(title = "Family Structure", x = "Family Structure", y = "Percentage %") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5, angle = 10),
      axis.text.y= element_text(size = 5),
      legend.text = element_text(size = 4),
      legend.key.size = unit(0.25, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom")
  ggsave('./charts/familystructure.jpg', dpi = 500, height = 7.53, width = 10.78, units = 'cm')
  print('Family Structure Complete!')
  
  return(family_structure_join)
  
}

### SECTION 12: EMPLOYMENT STATUS ---------------------------------------------

# generate the employment status breakdown for the study area
generate_employment_breakdown = function(DemosEmploymentStatus, Catchment_DAs_list, Region_DAs_list, studyarea_name, region_name){
  
  
  #### SECTION SPECIFIC FUNCTIONS ---------------------------------------------
  
  
  # calculate the employment status for the study area in comparison to the region
  calculate_employment_status = function(EmploymentStatus, Catchment_list, area_name){
  
    # pull the employment status
    Demos_emp = EmploymentStatus %>% rename("Total Labour Force" = ECYACTHPL, "In Labour Force" = ECYACTINLF, "Employed" = ECYACTEMP, "Unemployed" = ECYACTUEMP,
                                                 "Not in Labour Force" = ECYACTNOLF) %>% subset(Key %in% Catchment_list)
    
    # calculate the DA-level percentages
    Emp_tibble = tibble("Employment Status" = c("Employed", "Unemployed", "Not in Labour Force"),
                        "Percentage" = c(sum(Demos_emp$Employed) / sum(Demos_emp$`In Labour Force`) * 100,
                                         sum(Demos_emp$Unemployed) / sum(Demos_emp$`In Labour Force`) * 100,
                                         sum(Demos_emp$`Not in Labour Force`) / sum(Demos_emp$`Total Labour Force`) * 100),
                        "Area" = c(area_name))
    
    return(Emp_tibble)
  }
  
  
  #### SECTION DATA VIZ -------------------------------------------------------
  
  # calculate the employment structure
  employment_structure_studyarea = calculate_employment_status(DemosEmploymentStatus, Catchment_DAs_list, studyarea_name)
  employment_structure_region = calculate_employment_status(DemosEmploymentStatus, Region_DAs_list, region_name)
  
  # bind rows
  employment_structure_join = bind_rows(employment_structure_studyarea, employment_structure_region)
  
  # plot chart
  ggplot(employment_structure_join, aes(x = factor(`Employment Status`, levels = c("Employed", "Unemployed", "Not in Labour Force")),
                                    y = Percentage, fill = factor(Area, levels = c(region_name, studyarea_name)))) +
    geom_bar(position = "dodge",  stat = "identity", width = 0.7) +
    scale_color_manual(values = c("#CC2936", "#00AEF3"), aesthetics = c("colour", "fill")) +
    labs(title = "Employment Status", x = "Employment Status", y = "Percentage %") +
    theme(
      panel.background = element_rect(fill = 'transparent', colour = NA),
      panel.grid.minor = element_line(color = 'gray80'),
      panel.grid.major = element_line(color = 'gray80'),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7),
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 5, angle = 10),
      axis.text.y= element_text(size = 5),
      legend.text = element_text(size = 4),
      legend.key.size = unit(0.25, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom")
  ggsave('./charts/employmentstatus.jpg', dpi = 500, height = 7.53, width = 10.78, units = 'cm')
  print('Employment Structure Complete!')
  
  return(employment_structure_join)
}



## PART 4: RUNNING THE SCRIPT -------------------------------------------------

# Set the Working Directory and censusmapper api
setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters")
options(cancensus.api_key = "CensusMapper_3326bf59a08fe00964d27dc30aa685ec")



# Load Script Parameters
prov_code = "35"
prov_name = "Ontario"
CSD_id = "3520005"
region_name = "City of Toronto"
first_time = "no"
study_area_name = "West Queen West"



# Load the Spatial Data
# National DA Shapefile
DA_shp = st_read("./Data/lda_000a21a_e") %>% filter(PRUID == prov_code) %>% st_transform(crs = 3347)
# CSD Shapefile
CSD_shp = st_read("./Data/lcsd000a21a_e") %>% filter(CSDUID == CSD_id) %>% st_transform(crs = 3347)
# Parks Data
Parks_shp = st_read("./TorontoCMA/Data/Parks") %>% st_transform(crs = 3347) %>% select(osm_id, leisure)
# Study Area Shapefile
StudyArea_shp = st_read("./ConstructionMitigation/Data/BIAs/WestQueenWest") %>% st_transform(crs = 3347)
# Study Area Walking Buffer Shapefile
StudyArea_Walk = st_read("./ConstructionMitigation/Data/Walking_Buffers/WestQueenWest") %>% st_transform(crs = 3347)
# Study Area Demographic Buffer Shapefile
StudyArea_Buff = st_buffer(StudyArea_shp, 1000)



# Load the CSV Files Needs
Businesses = read.csv("./Data/EA_Data_Export/Businesses_Canada.csv") %>% rename("NAICS_6" = NAICS.Code...6.Digit, "NAICS_4" = NAICS.Code...4.Digit, "Employee.Size" = English.description.of.Employee.Size.Code..INFO_EMP.)
DemosSummary = read.csv("./Data/EA_Data_Export/DemosSummary.csv") %>% mutate(Key = as.character(Key))
DemosHousing = read.csv("./Data/EA_Data_Export/DemosHousing.csv") %>% mutate(Key = as.character(Key))
DemoOutlook = read.csv("./Data/EA_Data_Export/DemographicOutlook.csv") %>% mutate(Key = as.character(Key))
DemosFamily = read.csv("./Data/EA_Data_Export/DemosFamilyStructure.csv") %>% mutate(Key = as.character(Key))
DemosEmployment = read.csv("./Data/EA_Data_Export/DemosEmployment.csv") %>% mutate(Key = as.character(Key))
CommHealth = read.csv("./Data/EA_Data_Export/CommunityHealth.csv") %>% mutate(Key = as.character(Key))
CommLife = read.csv("./Data/EA_Data_Export/CommunityLife.csv") %>% mutate(Key = as.character(Key))
Civ_list = read.csv("./Data/CivInf_NAICS.csv")
Bus_list = read.csv("./Data/BusInf_NAICS.csv")

# Add in the Average Employee Size Number
Businesses = Businesses %>% mutate(EmpSizeNum = case_when(
  Number.of.Employees.Size.Code == "A" ~ 2,
  Number.of.Employees.Size.Code == "B" ~ 7,
  Number.of.Employees.Size.Code == "C" ~ 14.5,
  Number.of.Employees.Size.Code == "D" ~ 34.5,
  Number.of.Employees.Size.Code == "E" ~ 75,
  Number.of.Employees.Size.Code == "F" ~ 175,
  Number.of.Employees.Size.Code == "G" ~ 375,
  Number.of.Employees.Size.Code == "H" ~ 750,
  Number.of.Employees.Size.Code == "I" ~ 3000,
  Number.of.Employees.Size.Code == "J" ~ 7500,
  Number.of.Employees.Size.Code == "K" ~ 10001))


# Load the EA Foot traffic Data
Directory = "./ConstructionMitigation/Data/EnvironicsData_DA"
study_area_filename = "WestQueenWest"
filename19 = paste(study_area_filename, "19.csv", sep = "")
filename19 = paste(Directory, filename19, sep = "/")
filename20 = paste(study_area_filename, "20.csv", sep = "")
filename20 = paste(Directory, filename20, sep = "/")
filename21 = paste(study_area_filename, "21.csv", sep = "")
filename21 = paste(Directory, filename21, sep = "/")
filename22 = paste(study_area_filename, "22.csv", sep = "")
filename22 = paste(Directory, filename22, sep = "/")
ff_2019 = read.csv(filename19)
ff_2020 = read.csv(filename20)
ff_2021 = read.csv(filename21)
ff_2022 = read.csv(filename22)



# Define our catchment areas for the region and study area
Study_area_DAs = define_catchment_area(DA_shp, StudyArea_shp, "DA")
Region_DAs = define_catchment_area(DA_shp, CSD_shp, "DA")
# get the areas for the buffer and boundary
summary_area_boundary = drop_units(st_area(StudyArea_shp) / 1000000)
summary_area_demo = drop_units(st_area(StudyArea_Buff) / 1000000)



# set the output working directory
setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters/ConstructionMitigation/Output/CaseStudies/WestQueenWest")



### generate summary statistics for the section 1 -----------------------------
section_1_output = generate_summary(DemosSummary, Study_area_DAs, summary_area_demo)
studyarea_summary = section_1_output$studyarea_summary
commute_summary = section_1_output$commute_summary
# export the section 1 data
write_json(studyarea_summary, "./data/studyarea.json")
write_json(commute_summary, "./data/commute_summary.json")


### generate the civic infrastructure summary for section 3 -------------------
section_3_output = generate_civic_infrastrcuture(Businesses, Civ_list, StudyArea_shp, StudyArea_Walk, CSD_shp, study_area_name, region_name, first_time, summary_area_boundary)
civ_summary = section_3_output$civ_summary
civ_density = section_3_output$civ_density
# export the section 3 data
write_json(civ_summary, "./data/civ_summary.json")


### generate the housing charts and data for section 4 ------------------------
section_4_output = generate_housing(DemosHousing, Study_area_DAs, Region_DAs, study_area_name, region_name, CSD_id)
housing_tenure = section_4_output$housing_tenure
housing_const_year = section_4_output$housing_year
housing_type = section_4_output$housing_structure
housing_value = section_4_output$housing_value
# export section 4 data
write_json(housing_tenure, "./data/housing_tenure.json")
write_json(housing_const_year, "./data/housing_year.json")
write_json(housing_type, "./data/housing_type.json")
write_json(housing_value, "./data/housing_value.json")


### generate the demographic profile for section 5 ----------------------------
section_5_output = generate_demographic_profile(DemoOutlook, Study_area_DAs, Region_DAs, study_area_name, region_name, CSD_id)
pop_pyramid = section_5_output$Pop_pyramid
Inc_Decile = section_5_output$Inc_Dec
Edu_Attn = section_5_output$Edu_Attn
Gen_Status = section_5_output$Gen_stat
# export the section 5 data
write_json(pop_pyramid, "./data/pop_pyramid.json")
write_json(Inc_Decile, "./data/Inc_Decile.json")
write_json(Edu_Attn, "./data/Edu_Attn.json")
write_json(Gen_Status, "./data/Gen_Status.json")


### generate the business summary for section 6 -------------------------------
section_6_output = generate_business_profile(Businesses, Bus_list, StudyArea_shp, StudyArea_Walk, CSD_shp, study_area_name, region_name, first_time, summary_area_boundary)
bus_summary = section_6_output$bus_summary
bus_density = section_6_output$bus_density
# export the section 6 data
write_json(bus_summary, "./data/bus_summary.json")


### generate the employment data for section 7 --------------------------------
section_7_output = generate_employment_profile(Businesses, Bus_list, Civ_list, first_time)

### generate the visitor foot traffic data for section 8 ----------------------
section_8_output = generate_visitor_foottraffic(ff_2019, ff_2020, ff_2021, ff_2022)
ff_coordinate_heatmap = section_8_output$ff_coordinate_heatmap
ff_visitor_types = section_8_output$ff_visitor_types
ff_monthly = section_8_output$ff_monthly
ff_yearly = section_8_output$ff_yearly
# export the section 8 data
write_json(ff_coordinate_heatmap, "./data/coordinate_heatmap.json")
write_json(ff_visitor_types, "./data/visitortypes.json")
write_json(ff_monthly, "./data/ff_monthly.json")
write_json(ff_yearly, "./data/ff_yearly.json")
write.csv(ff_visitor_types, "./data/visitortypes.csv")
write.csv(ff_monthly, "./data/ff_monthly.csv")

### generate the visitor characteristics for section 9 ------------------------
section_9_output = generate_visitor_characteristics(ff_2019, ff_2020, ff_2021, ff_2022, StudyArea_shp)
vis_distance = section_9_output$vis_distance
vis_avg_dist = section_9_output$vis_avg_dist
vis_dayofweek = section_9_output$vis_dayofweek
vis_timeofday = section_9_output$vis_timeofday
# export the section 9 data
write_json(vis_distance, "./data/visitordistance.json")
write_json(vis_dayofweek, "./data/vis_dayofweek.json")
write_json(vis_timeofday, "./data/vis_timeofday.json")
write_json(vis_avg_dist, "./data/vis_avg_dist.json")
write.csv(vis_avg_dist, "./data/vis_avg_dist.csv")

### generate the green space analysis for section 10 --------------------------
section_10_output = generate_greenspace_analysis(Parks_shp, StudyArea_shp, StudyArea_Walk, CSD_shp, study_area_name, region_name)
# export the section 10 data
write_json(section_10_output, "./data/greenspace_per.json")

#### generate family structure for section 11 (project specific) --------------
section_11_output = generate_family_structure(DemosFamily, Study_area_DAs, Region_DAs, study_area_name, region_name)

#### generate employment status for section 12 (project specific) -------------
section_12_output = generate_employment_breakdown(DemosEmployment, Study_area_DAs, Region_DAs, study_area_name, region_name)
write.csv(section_12_output, "./data/empstatus.csv")

## PART 5: PRODUCE THE COMPOSITION CHARTS -------------------------------------

# export the business and civic densities
bus_civ_densities = tibble("Type" = c("Business", "Civic"),
                           "Density" = c(bus_density, civ_density))
write.csv(bus_civ_densities, "./data/bus_civ_density.csv")

# civic composition
civ_comp = civ_summary %>% spread(Area, Percentage) %>% group_by(Group) %>% st_drop_geometry() %>%
  fill(everything(), .direction = "downup") %>%
  slice(1)
civ_comp = civ_comp %>% replace(is.na(.), 0)
civ_comp = civ_comp %>% select(Group, region_name, study_area_name) %>% mutate(Percent_Diff = ((`West Queen West` - `City of Toronto`)/((`West Queen West` + `City of Toronto`)/2))*100)

# business composition
bus_comp = bus_summary %>% replace(is.na(.), 0) %>% spread(Area, Percentage) %>% group_by(Group) %>% st_drop_geometry() %>%
  fill(everything(), .direction = "downup") %>%
  slice(1)
bus_comp = bus_comp %>% replace(is.na(.), 0)
bus_comp = bus_comp %>% select(Group, region_name, study_area_name) %>% mutate(Percent_Diff = ((`West Queen West` - `City of Toronto`)/((`West Queen West` + `City of Toronto`)/2))*100)

# bind the summary rows
summary_bind = bind_rows(bus_comp, civ_comp)

# plot the business summary chart
ggplot(summary_bind, aes(x = factor(Group, levels = c("Recreation Facilities", "Government and Community Services","Health and Care Facilities", "Education", "Arts and Culture",
                                                      "Services and Other", "Food and Drink", "Retail")), y = Percent_Diff, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_color_manual(values = c("#002A41","#002A41", "#00AEF3","#002A41", "#002A41", "#002A41", "#00AEF3", "#00AEF3"), aesthetics = c("colour", "fill")) +
  labs(title = "Percentage Difference of Area Composition relative to\nthe City of Toronto", x = "Group", y = "Percentage Difference (%)") +
  theme(
    panel.background = element_rect(fill = 'transparent', colour = NA),
    panel.grid.minor = element_line(color = 'gray80'),
    panel.grid.major = element_line(color = 'gray80'),
    plot.background = element_rect(fill = 'transparent', colour = NA),
    plot.title = element_text(size = 7),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 5),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position = "none") +
  coord_flip()
ggsave('./charts/composition.jpg', dpi = 500, height = 5.7, width = 14.06, units = 'cm')

# export csv
write_json(summary_bind, "./data/composition.json")



