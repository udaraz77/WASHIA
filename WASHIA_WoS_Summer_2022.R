# Author: Umar Daraz udaraz@unicef.org/umar.daraz@gmail.com
# Title: WoS WASH Household Assessment Indicators Calculations for WASH Insecurity Analysis
# Data Set: This version of script shall process Summer 2022 Data sets of WoS WASH Household Assessment Data. Each Round of Data set has slight variation
# Last Modified Date:19-May-2023
# Sat May 20 23:12:17 2023 ------------------------------


# Loading libraries
{
  library(openxlsx)
  library(tidyverse)
}

# Step1: Loading Raw Data
{
# The following code will open Summar 2022 Data sets of WoS WASH Household Assessment Data
# @variable WoS_WASH_2022_Raw
options(java.parameters = "- Xmx2048m") # increase memory for large file size
WoS_WASH_2022_Raw <- readWorkbook( file.choose(), 
                                     sheet=1, 
                                     startRow=1, 
                                     colNames=TRUE, 
                                     rowNames=FALSE, 
                                     detectDates=TRUE, 
                                     skipEmptyRows=TRUE, 
                                     skipEmptyCols=TRUE, 
                                     rows=NULL, 
                                     cols=NULL, 
                                     check.names=FALSE, 
                                     sep.names=".", 
                                     namedRegion=NULL, 
                                     na.strings="NA", 
                                     fillMergedCells=FALSE)
}

# Step1: Calculation Improved Water Source JMP Indicator
{
  # References:
  # 1: https://washdata.org/monitoring/methods/estimation-methods
  # 2: https://www.who.int/data/nutrition/nlis/info/improved-sanitation-facilities-and-drinking-water-sources
  
  # Indicator Definition:
  # Households using improved drinking water sources which are located on premises, with water available when needed, and free from contamination*, are classified as 
  # having safely managed services. Households not meeting all of these criteria, but using an improved source with water collection times of no more than 30 minutes per round 
  # trip are classified as having basic services, and those using improved sources with water collection times exceeding 30 minutes are classified as limited services.
    
  # *For global monitoring purposes, the priority water quality parameter will be the absence of faecal indicator bacteria (E. coli or thermotolerant coliforms).
  # Data on arsenic and fluoride will also be used where available
  
  # UD Notes: In the Syrian context water is delivered at the household through networks, water trucks (private markete), or water bottels are bouhgt from commercial markete.
  # Therefore water fetching time is not relevant in communities however, IDP sites in some cases fetch water from a collection point.
  # Other Reference: 
  # Improved water sources: 
  
  # In Syria context following water sources are considered improved water sources:
  # 1: Network Water 
  # 2: Bottled Water
  # 3: Closed Well (Networked)
  
  # Data Processing:
  
  # Input Variables
  # @var 1: W1. What water source did your household use the most in the last 30 days? [col:48]
  #         Options [1: "Network",  2: "Water_trucking", 3: "Closed_well_individual", 4: "Closed_well_network", 5: "Open_well", 6:"Bottle", 7: "Springs" 8: "Other"]
  
  # @var 2: Do you receive water in your household? (The household member doesn't need to fetch water from a source outside the household.) [col:46]
  #         Options [1: "Yes",  2: "No"]
  
  # @var 3: If "Yes" is selected in @var 2, who is responsible for fetching the water?" [col:47]
  
  # @var 4: what's the location type? [col:15]
  #       : Options [1: "Community", 2: "Neighborhood", 3: "Camp"]
  
  # @var 5: Household Type? [col:27]
  #       : Options [1: "Host-population", 2: "IDPs", 3: "Returnee (returned in 2021/2022)"]
  
  # @var 6: Sub district codes [col:12]
  
  # @var 7: Community PCode [col:14]
  
  # @var 8: Weight (household weight) [col:1]
  
  # Results variables
  # @var improved_water_Source_raw  [To store the extracted data from the main data source]
  # @var improved_water_Source [To store final results at ]
  
  # Data Extraction
  improved_water_Source_raw <- WoS_WASH_2022_Raw[,c(12,14,15,27,48,49,46,47,1)]
  colnames(improved_water_Source_raw) <- c("SD_Pcode","CommunityPcode","LocationType","HH_Type","WaterSource","WaterSourceOther","WaterReceivedAtHH","WhoFetch","HH_Weight")
  
  # Data cleaning
  # Remove first two rows 
  improved_water_Source_raw <- improved_water_Source_raw %>%  filter(!row_number() %in% c(1, 2))
  
  # Type conversion 
  improved_water_Source_raw$HH_Weight <- as.numeric(improved_water_Source_raw$HH_Weight)
  
  # Fix spelling errors
  improved_water_Source_raw <- improved_water_Source_raw %>% 
    mutate(WaterSource = str_replace(WaterSource,"Closed_well_indivisual","Closed_well_individual")) %>% 
    mutate(WaterSource = str_replace(WaterSource,"O","Other")) %>% 
    mutate(WaterSource = str_replace(WaterSource,"Otherpen_well","Open_well"))
  
  # Adding JMP standard Variables
  ## Urban Rural and camps
  improved_water_Source_raw$UrbanRural <- ifelse(str_sub(improved_water_Source_raw$CommunityPcode,1,1)=='N',"Urban",
                                                     ifelse(str_sub(improved_water_Source_raw$CommunityPcode,1,2)=='CP',"Camp","Rural"))
  ## Water Source Categories
  
  improved_water_Source_raw$Water_safely <- FALSE
  improved_water_Source_raw$water_basic <- ifelse(  (improved_water_Source_raw$WaterSource == "Network" | 
                                                    improved_water_Source_raw$WaterSource == "Closed_well_network" |
                                                    improved_water_Source_raw$WaterSource == "Bottle") &
                                                      improved_water_Source_raw$WaterReceivedAtHH == "Yes",TRUE,FALSE
                                                    )
  improved_water_Source_raw$water_limited <- ifelse((improved_water_Source_raw$WaterSource == "Network" | 
                                                       improved_water_Source_raw$WaterSource == "Closed_well_network" |
                                                       improved_water_Source_raw$WaterSource == "Bottle") &
                                                      improved_water_Source_raw$WaterReceivedAtHH == "No",TRUE,FALSE
                                                      
  )
  improved_water_Source_raw$water_unimproved  <- ifelse((improved_water_Source_raw$WaterSource == "Closed_well_individual" | 
                                                           improved_water_Source_raw$WaterSource == "Water_trucking" |
                                                           improved_water_Source_raw$WaterSource == "Open_well" |
                                                           improved_water_Source_raw$WaterSource == "Springs" | 
                                                           improved_water_Source_raw$WaterSource == "Other"),TRUE,FALSE 
  )
  improved_water_Source_raw$water_surface  <- ifelse((improved_water_Source_raw$WaterSource == "River" |
                                                        improved_water_Source_raw$WaterSource == "Lake"),TRUE,FALSE
    
  )
  improved_water_Source_raw$check <- ifelse(improved_water_Source_raw$water_basic | 
                                              improved_water_Source_raw$water_limited | 
                                              improved_water_Source_raw$water_unimproved | 
                                              improved_water_Source_raw$water_surface,TRUE,FALSE
                                              )
  # Replace True with values 
  improved_water_Source_raw$Water_safely = ifelse(improved_water_Source_raw$Water_safely,improved_water_Source_raw$HH_Weight,0)
  improved_water_Source_raw$water_basic = ifelse(improved_water_Source_raw$water_basic,improved_water_Source_raw$HH_Weight,0)
  improved_water_Source_raw$water_limited = ifelse(improved_water_Source_raw$water_limited,improved_water_Source_raw$HH_Weight,0)
  improved_water_Source_raw$water_unimproved = ifelse(improved_water_Source_raw$water_unimproved,improved_water_Source_raw$HH_Weight,0)
  improved_water_Source_raw$water_surface = ifelse(improved_water_Source_raw$water_surface,improved_water_Source_raw$HH_Weight,0)
  
  
   # Calculate Indicator Values %
  
  improved_water_Source <- improved_water_Source_raw %>% 
    group_by(SD_Pcode) %>% 
    summarise(Water_safely = sum(Water_safely)
             ,water_basic = sum(water_basic)
             ,water_limited = sum(water_limited)
             ,water_unimproved = sum(water_unimproved)
             ,water_surface = sum(water_surface)
             ,TRec = sum(HH_Weight)
             ,nrec = n())
  
  improved_water_Source_per <- improved_water_Source %>% 
    group_by(SD_Pcode) %>% 
    summarise(Water_safely = round((Water_safely/TRec)*100,2)
              ,water_basic = round((water_basic/TRec)*100,2)
              ,water_limited = round((water_limited/TRec)*100,2)
              ,water_unimproved = round((water_unimproved/TRec)*100,2)
              ,water_surface = round((water_surface/TRec)*100,2)
              ,TRec = TRec
              ,nrec = nrec)
  
  improved_water_Source_PiN <- improved_water_Source_per %>% 
    group_by(SD_Pcode) %>% 
    summarise(Water_safely = Water_safely
              ,water_basic = water_basic
              ,water_limited = water_limited
              ,water_unimproved = water_unimproved
              ,water_surface = water_surface
              ,TRec = TRec
              ,nrec = nrec
              ,PiN_Multiplier = water_limited+water_unimproved+water_surface)

  
}
