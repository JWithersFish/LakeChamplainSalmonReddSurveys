# This script is intended to convert Access database data to ArcGIS Online for Atlantic salmon redd surveys in Lake Champlain basin.
# Created by Jonah Withers on 2021-11-05



# Clean house! ----
rm(list = ls()) # Clear environment
gc() # Clear ram




# Libraries ----
requiredPackages <- c("dplyr", "lubridate","sf", "sp", "rgdal",
                      "RODBC", "ggplot2", "arcgisbinding")

# Function to install any packages not installed
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Load packages
ipak(requiredPackages)
arc.check_product()





# Import and process data ----
# > Connect to access database ####

# identify channel to connect to database
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=./AtlanticSalmonReddSurveyLakeChamplainTrib.accdb")

# Look at tables and queries within database
sqlTables(channel)

Loc.0 <- as.data.frame(sqlFetch(channel, "tbl_Location"))
Trip.0 <- as.data.frame(sqlFetch(channel, "tbl_Trip"))
Redds.0 <- as.data.frame(sqlFetch(channel, "tbl_Redd"))
Fish.0 <- as.data.frame(sqlFetch(channel, "tbl_Fish"))
GPS.0 <- as.data.frame(sqlFetch(channel, "tbl_GPSUPload"))

# Close connection to databse
odbcClose(channel)





# Reformat ----
Loc.1 <- Loc.0 %>% 
  mutate(Tributary = as.character(Tributary),
         Site_ID = as.character(Site_ID))

Trip.1 <- Trip.0 %>% 
  mutate(Site_ID = as.character(Site_ID),
         Crew = as.character(Crew),
         Weather = as.character(Weather),
         TripComments = as.character(TripComments))

Redds.1 <- Redds.0 %>% 
  mutate(GPS_ID = as.character(GPS_ID),
         Species = as.character(Species),
         ReddComments = as.character(ReddComments))


Fish.1 <- Fish.0 %>% 
  mutate(Species = as.character(Species),
         Sex = as.character(Sex),
         GPS_ID = as.character(GPS_ID),
         FloyTag = as.character(FloyTag),
         FishComments = as.character(FishComments))

GPS.1 <- GPS.0 %>% 
  mutate(GPS_ID = as.character(GPS_ID))





# Join tables ----
# > Redds ####
Redds.2 <- Redds.1 %>%
  left_join(GPS.1, by = "GPS_ID") %>%
  left_join(Trip.1, by = "Trip_ID") %>%
  left_join(Loc.1 %>%
               mutate(Tributary = as.character(Tributary)) %>%
               dplyr::select(Tributary, Site_ID), by = "Site_ID") %>% 
  mutate(SampYear = year(TripDate))
  
# Parse by tributary
Win.Redds <- Redds.2 %>% 
  filter(Tributary == "Winooski River")

Boq.Redds <- Redds.2 %>% 
  filter(Tributary == "Boquet River")



# > Fish ####
Fish.2 <- Fish.1 %>%
  left_join(GPS.1, by = "GPS_ID") %>%
  left_join(Trip.1, by = "Trip_ID") %>%
  left_join(Loc.1 %>%
              mutate(Tributary = as.character(Tributary)) %>%
              dplyr::select(Tributary, Site_ID), by = "Site_ID") %>% 
  mutate(SampYear = year(TripDate))

# Parse by tributary
Win.Fish <- Fish.2 %>% 
  filter(Tributary == "Winooski River")

Boq.Fish <- Fish.2 %>% 
  filter(Tributary == "Boquet River")





# Write tables to .gdb ----
arc.write("./ArcGIS/LakeChamplainLandlockedAtlanticSalmonReddSurveys/LakeChamplainLandlockedAtlanticSalmonReddSurveys.gdb/Tributaries",
          data = Loc.1,
          overwrite = TRUE)

arc.write("./ArcGIS/LakeChamplainLandlockedAtlanticSalmonReddSurveys/LakeChamplainLandlockedAtlanticSalmonReddSurveys.gdb/Trips",
          data = Trip.1,
          overwrite = TRUE)

arc.write("./ArcGIS/LakeChamplainLandlockedAtlanticSalmonReddSurveys/LakeChamplainLandlockedAtlanticSalmonReddSurveys.gdb/Winooski_Redds",
          coords = c("Longitude", "Latitude"),
          shape_info = list(type = 'Point', hasZ = FALSE, WKID = 4326),
          data = Win.Redds,
          overwrite = TRUE)

arc.write("./ArcGIS/LakeChamplainLandlockedAtlanticSalmonReddSurveys/LakeChamplainLandlockedAtlanticSalmonReddSurveys.gdb/Boquet_Redds",
          coords = c("Longitude", "Latitude"),
          shape_info = list(type = 'Point', hasZ = FALSE, WKID = 4326),
          data = Boq.Redds,
          overwrite = TRUE)

arc.write("./ArcGIS/LakeChamplainLandlockedAtlanticSalmonReddSurveys/LakeChamplainLandlockedAtlanticSalmonReddSurveys.gdb/Winooski_Fish",
          coords = c("Longitude", "Latitude"),
          shape_info = list(type = 'Point', hasZ = FALSE, WKID = 4326),
          data = Win.Fish,
          overwrite = TRUE)

arc.write("./ArcGIS/LakeChamplainLandlockedAtlanticSalmonReddSurveys/LakeChamplainLandlockedAtlanticSalmonReddSurveys.gdb/Boquet_Fish",
          coords = c("Longitude", "Latitude"),
          shape_info = list(type = 'Point', hasZ = FALSE, WKID = 4326),
          data = Boq.Fish,
          overwrite = TRUE)