#
# Checking what chemistry data (both biota and sediment) we have in NIVAbasen
#
# Conclusions:
# - Biota: only 2016 and 2018
# - Sediment: 


#
# 1. Getting ready ----
#

library(dplyr)
library(purrr)
library(niRvana)
library(lubridate)
library(leaflet)

# username + password
set_credentials()

#
# 2. Get stations from projects ----
# 

# Get the list of projects
df_projects <- get_projects()   # we call it 'df_projects' (the default name used by 'get_stations_from_project')

df_projects %>%
  filter(PROJECT_ID %in% c(15009, 16009))  # Zero. I think these are project numbers, not project_ID 

df_projects %>%
  filter(PROJECT_ID %in% c(15404))

# Name "Viktilt" gotten from Aquamonitor
df_proj <- df_projects %>% filter(grepl("Vikkilen", PROJECT_NAME) | grepl("Viktilt", PROJECT_NAME))
df_proj$PROJECT_ID     # 118, 9246, 12386
df_proj$PROJECT_NAME   # "Vikkilen", "Viktilt 2022", "Overvåking av Vikkilen i 2018"

# Get a list of the stations associated with those 3 projects
df_stations <- df_proj$PROJECT_NAME %>% map_df(~get_stations_from_project(., exact = TRUE))
nrow(df_stations)  # 28

#
# Do some more searching after stations
#
df_stations_extra <- get_nivabase_selection("*", 
                                         "PROJECTS_STATIONS", 
                                         "STATION_NAME", c("Vikkilen", "Nymo", "Kjellviga", "Bie"),
                                         values_are_text = TRUE)
# Same stations as above - no new stations
xtabs(~PROJECT_ID, df_stations)
xtabs(~PROJECT_ID, df_stations_extra)

#
# 3a. Search by latitude/longitude ----
# Coordinates from kart.finn.no (put markers, then click markers, then click GPS-koordinater)
#
# Area is shown in part 3b
#
# get_nivabase_data("select OWNER,TABLE_NAME from ALL_TAB_COLUMNS where column_name = 'SAMPLE_POINT_ID'")
# get_nivabase_data("select * from ALL_TAB_COLUMNS where OWNER = 'NIVA_GEOMETRY' and table_name = 'SAMPLE_POINTS'")  
#

# Sample points in box 1, covering southern part of the area
df_sample_points1 <- get_nivabase_data("select SAMPLE_POINT_ID, LONGITUDE, LATITUDE 
                                      from NIVA_GEOMETRY.SAMPLE_POINTS where 
                                      LATITUDE > 58.33489 and LATITUDE < 58.36696 and 
                                      LONGITUDE > 8.59503 and LONGITUDE < 8.63022")   
nrow(df_sample_points1)  # 119

# Sample points in box 2, covering northern part of the area
df_sample_points2 <- get_nivabase_data("select SAMPLE_POINT_ID, LONGITUDE, LATITUDE
                                       from NIVA_GEOMETRY.SAMPLE_POINTS where 
                                       LATITUDE > 58.29441 and LATITUDE < 58.33489 and 
                                       LONGITUDE > 8.57392 and LONGITUDE < 8.60155")   
nrow(df_sample_points2)  # 98

# Combine sample points
df_sample_points <- bind_rows(df_sample_points1, df_sample_points2)

# Get stations
df_sample_points_stations <- get_nivabase_selection(
  "STATION_ID, GEOM_REF_ID", 
  "STATIONS", 
  "GEOM_REF_ID", 
  unique(df_sample_points$SAMPLE_POINT_ID))
nrow(df_sample_points_stations)  # 67

# Get PROJECTS_STATIONS
df_sample_points_projstations <- get_nivabase_selection(
  "PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME", 
  "PROJECTS_STATIONS", 
  "STATION_ID", 
  unique(df_sample_points_stations$STATION_ID))

# Get projects
df_sample_points_projects <- get_nivabase_selection(
  "PROJECT_ID, PROJECT_NAME", 
  "PROJECTS", 
  "PROJECT_ID", 
  unique(df_sample_points_projstations$PROJECT_ID))
nrow(df_sample_points_projects)  # 18

# Add extra columns to 'df_sample_points'
df_sample_points <- df_sample_points %>%
  left_join(df_sample_points_stations, by = c("SAMPLE_POINT_ID" = "GEOM_REF_ID")) %>%
  left_join(df_sample_points_projstations) %>%            # add PROJECT_ID
  left_join(df_sample_points_projects) %>%                # add PROJECT_NAME
  mutate(labeltext = ifelse(                              # add label text for plot
    !is.na(STATION_NAME),
    paste0(STATION_NAME, " (StasjonsID = ", STATION_ID, "),  prosjekt: ", PROJECT_NAME),
    NA))

openxlsx::write.xlsx(df_sample_points, "Data/03_Stasjoner_Vikkilen.xlsx")

#
# 3b. Show sample points and on map ----
#

# Map 1 - just markers
df_sample_points %>%
  leaflet() %>% addTiles() %>% 
  addRectangles(8.63022, 58.36696, 8.59503, 58.33489, fillOpacity = 0.15) %>%
  addRectangles(8.57392, 58.33489, 8.60155, 58.29441, fillOpacity = 0.15) %>%
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, label = ~labeltext)

# Map 2 - markers with station names are coloured
getColor <- function()                                          # Function 1
  ifelse(is.na(df_sample_points$labeltext), "green", "blue") # "hard-coded" for data set

icons <- awesomeIcons(       # Function 2 is a "meta-function" 
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor()   # Refer to function 1 here
)

leaf <- df_sample_points %>%
  leaflet() %>% addTiles() %>% 
  addAwesomeMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, icon = icons, label = ~labeltext)
leaf

# Save
htmlwidgets::saveWidget(leaf, file = "Temp.html")
file.copy(from = "Temp.html", to = "Data/03_Stasjoner_Vikkilen.html")
file.remove("Temp.html")

# sOME MORE LOOKING 
# get_nivabase_data("select OWNER,TABLE_NAME from ALL_TAB_COLUMNS where column_name = 'GEOM_REF_ID'")
# get_nivabase_data("select OWNER,TABLE_NAME from ALL_TAB_COLUMNS where column_name = 'LONGITUDE'")
# get_nivabase_data("select OWNER,TABLE_NAME from ALL_TAB_COLUMNS where OWNER = 'NIVA_GEOMETRY'")
# get_nivabase_data("select * from NIVADATABASE.STATION_ATTRIBUTE_DEFINITIONS")

#
# 4. Chemicals in BIOTA ----
#
# Find only data from 2016 + 2018 
#
# 
# a. Get all specimens collected at these stations (20 seconds or so)
df_specimens1 <- get_specimens_from_stationdata(df_stations)
nrow(df_specimens1)  # 14
xtabs(~DATE_CAUGHT, df_specimens1)  # 2016 + 2018

# b. From df_sample_points above (selected by lat/long), coupled using SAMPLE_POINT_ID
df_specimens2 <- get_nivabase_selection("*", "BIOTA_SINGLE_SPECIMENS", 
                                       "SAMPLE_POINT_ID", df_sample_points$SAMPLE_POINT_ID)
nrow(df_specimens2)  # 0

# From df_sample_points above (selected by lat/long), coupled using STATION_ID
df_specimens2 <- get_nivabase_selection("*", "BIOTA_SINGLE_SPECIMENS", 
                                        "STATION_ID", df_sample_points$SAMPLE_POINT_ID)
nrow(df_specimens2)  # 121
sum(df_specimens2$SPECIMEN_ID %in% df_specimens1$SPECIMEN_ID) # 0, so all are new

# c. Combine data from a + b
df_specimens <- bind_rows(df_specimens1, df_specimens2)  # 74
nrow(df_specimens)  # 135

# Get method definition table (for the chemical methods)
df_methods <- get_nivabase_data("select METHOD_ID, NAME, UNIT, BASIS_ID from NIVADATABASE.METHOD_DEFINITIONS")

# Get data frame of chemical data for all samples from the measurement year 2016 (30 seconds or so)
df_biota <- get_biota_chemistry(
  years = 1990:2018, 
  specimendata = df_specimens, 
  stationdata = df_stations,
  report_samples = TRUE)

nrow(df_biota) # 31009

# Add latitude, longitude
df_biota <- df_biota %>% 
  left_join(df_sample_points %>% select(SAMPLE_POINT_ID, LONGITUDE, LATITUDE), 
            by = c("STATION_ID" = "SAMPLE_POINT_ID"))

nrow(df_biota) # 31009

tab <- xtabs(~NAME + year(SAMPLE_DATE), df_biota)  
sel1 <- grepl("tin", rownames(tab), ignore.case = TRUE)
sel2 <- grepl("BT", rownames(tab), ignore.case = FALSE)
sel <- sel1 | sel2
sum(sel)
tab[sel,]
# NAME                           2016 2018
# Dibutyltinn-Sn (DBT-Sn)         0   16
# Dibutyltinn (DBT)               0   16
# Dioktyltinn-Sn (DOT-Sn)         0   16
# Dioktyltinn (DOT)               0   16
# Fettinnhold                     3    8
# Monobutyltinn (MBT)             0   16
# Monobutyltinn (MBT)-Sn          0   16
# Monooktyltinn (MOT)             0   16
# Monooktyltinn (MOT)-Sn          0   16
# Tetrabutyltinn (TetraBT)        0   16
# Tetrabutyltinn (TTBT)-Sn        0   16
# Tributyltinn                   11    0
# Tributyltinn (TBT)              0   16
# Tributyltinn (TBT)-Sn           0   16
# Trifenyltinn (TPhT)             0   16
# Trifenyltinn (TPhT)-Sn          0   16
# Trisykloheksyltinn (TCHT)       0   16
# Trisykloheksyltinn (TCHT)-Sn    0   16

df_biota %>% 
  filter(NAME %in% c("Tributyltinn", "Tributyltinn (TBT)")) %>%
  count(LATIN_NAME, STATION_CODE, STATION_NAME, year(SAMPLE_DATE), NAME)

#    LATIN_NAME         STATION_CODE STATION_NAME                 `year(SAMPLE_DATE)` NAME                   n
#    <chr>              <chr>        <chr>                                      <dbl> <chr>              <int>
#  1 NA                 BL3          Biodden                                     2018 Tributyltinn (TBT)     2
#  2 NA                 BL4          Kjellviga                                   2018 Tributyltinn (TBT)     2
#  3 NA                 BL5          Bie                                         2018 Tributyltinn (TBT)     2
#  4 NA                 BL6          Nymo                                        2018 Tributyltinn (TBT)     2
#  5 NA                 Lit1         Håøya - strandsnegl                         2018 Tributyltinn (TBT)     2
#  6 NA                 Lit4         Hasseldalen strandsnegl                     2018 Tributyltinn (TBT)     2
#  7 NA                 Lit6         AS Nymo strandsnegl                         2018 Tributyltinn (TBT)     2
#  8 NA                 Lit7         Vikkilen innerst strandsnegl                2018 Tributyltinn (TBT)     2
#  9 Cancer pagurus     Vikk_krabbe  Vikkilen krabbe                             2016 Tributyltinn           1
# 10 Gadus morhua       Vikk_torsk   Vikkilen Torsk                              2016 Tributyltinn           2
# 11 Littorina littorea Lit1         Håøya - strandsnegl                         2016 Tributyltinn           2
# 12 Littorina littorea Lit4         Hasseldalen strandsnegl                     2016 Tributyltinn           2
# 13 Littorina littorea Lit6         AS Nymo strandsnegl                         2016 Tributyltinn           2
# 14 Littorina littorea Lit7         Vikkilen innerst strandsnegl                2016 Tributyltinn           2  



#
# 5. Chemical data in SEDIMENT ----
#

#
# Sediment samples
#
# a. From stations IDs
df_sediment_samples1 <- get_nivabase_selection("*", "SEDIMENT_SAMPLES", "STATION_ID", df_stations$STATION_ID)
nrow(df_sediment_samples1) # 33
# b. From df_sample_points above (selected by lat/long)
df_sediment_samples2 <- get_nivabase_selection("*", "SEDIMENT_SAMPLES", "SAMPLE_POINT_ID", df_sample_points$SAMPLE_POINT_ID)
nrow(df_sediment_samples2) # 12
sum(df_sediment_samples2$SAMPLE_ID %in% df_sediment_samples1$SAMPLE_ID)  # 0, i.e. only new samples
sum(df_sediment_samples2$STATION_ID %in% df_sediment_samples1$STATION_ID)  # 0, i.e. only new samples
# c. Combine data from a + b
df_sediment_samples <- bind_rows(df_sediment_samples1, df_sediment_samples2)


#
# Get chemistry results (from df_sediment_samples)
#
station_ids <- unique(c(df_sediment_samples$STATION_ID, df_stations$STATION_ID))
df_sediment_chemst <- get_nivabase_selection("*", "SED_CHEMISTRY_STAT", "STATION_ID", station_ids)
nrow(df_sediment_chemst) # 229

# Add long, lat
df_sediment_chemst <- left_join(df_sediment_chemst, df_sample_points, by = c("STATION_ID" = "SAMPLE_POINT_ID"))

# Try to get better chemical names, but not much success
df_sediment_chemdef <- get_nivabase_selection("*", "SED_PARAMETER_DEFINITIONS", "NAME", 
                                              unique(df_sediment_chemst$NAME), values_are_text = TRUE)
nrow(df_sediment_chemdef) # 49 - but mostly NA values for  DESCR, SORT_NAME etc.

#
# Statistics
#
xtabs(~NAME, df_sediment_chemst)    # No TBT! (but DBT)
xtabs(~FIRST, df_sediment_chemst)   # 1993, 1997, 1998, 2004 - 2005
station_ids_sed <- xtabs(~STATION_ID, df_sediment_chemst) %>% names()   # 11 stations with data
get_nivabase_selection("PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME", "PROJECTS_STATIONS", 
                       "STATION_ID", station_ids_sed)  

# PROJECT_ID STATION_ID STATION_CODE                   STATION_NAME
#          1         118      10149         VIK1                       Vikkilen
#          2        2211      34860         HaG1                       Grimstad
#          3        2211      34861         HaG2                       Grimstad
#          4        2211      34862         HaG3                       Grimstad
#          5        2348      36104        GRI1A                   Grimstadhavn
#          6        2348      36105        GRI2A                   Grimstadhavn
#          7        2373      36431        ARE01 Arendal, Grimstad, Tvedestrand
#          8        2373      36432        ARE02 Arendal, Grimstad, Tvedestrand
#          9        2373      36440        ARE12 Arendal, Grimstad, Tvedestrand
#          10       2373      36441        ARE13 Arendal, Grimstad, Tvedestrand
#          11       2373      36442        ARE14 Arendal, Grimstad, Tvedestrand


#
# 6. Find ALL tbt in sediment ----
#
df_sediment_chemst_tbt <- 
  get_nivabase_selection("*", "SED_CHEMISTRY_STAT", "NAME", "TBT", values_are_text = TRUE)
nrow(df_sediment_chemst_tbt) # 207

xtabs(~year(FIRST), df_sediment_chemst_tbt)   # 2006 - 2015
xtabs(~STATION_ID, df_sediment_chemst_tbt)   # station_id = 10149

df_sample_points_tbt <- get_nivabase_selection(
  "SAMPLE_POINT_ID, LONGITUDE, LATITUDE",
  "SAMPLE_POINTS",
  "SAMPLE_POINT_ID",
  unique(df_sediment_chemst_tbt$STATION_ID),
  owner = "NIVA_GEOMETRY")
nrow(df_sample_points_tbt) # 31

#
# Check where these TBT data are - none in Grimstad
#
df_sample_points_tbt %>%
  select(LONGITUDE, LATITUDE) %>%
  leaflet(data = .) %>% addTiles() %>% addMarkers()



#
#  Appendix (used when maming script) ----
#

# Looking for tables
# get_nivabase_data("select TABLE_NAME from ALL_TAB_COLUMNS where OWNER = 'NIVADATABASE' and column_name = 'STATION_ID'")  
# get_nivabase_data("select TABLE_NAME from ALL_TAB_COLUMNS where OWNER = 'NIVADATABASE' and column_name = 'STA_ID'")  
# get_nivabase_data("select TABLE_NAME from ALL_TAB_COLUMNS where OWNER = 'NIVADATABASE' and table_name LIKE '%SED%'")  
# get_nivabase_data("select TABLE_NAME from ALL_TAB_COLUMNS where OWNER = 'NIVADATABASE' and table_name LIKE '%SED%' AND column_name = 'SAMPLE_POINT_ID'")  
# BB_GRAB_SAMPL
# HB_OBSERVATION_SITES
# HB_PARAMETER_VALUES
# SEDIMENT_SAMPLES
# SED_CHEMISTRY_STAT

# Some looking at tables
# get_nivabase_data("select * from NIVADATABASE.SEDIMENT_SAMPLES where rownum < 4")   
# get_nivabase_data("select * from NIVADATABASE.SED_CHEMISTRY_STAT where rownum < 4")   
# get_nivabase_data("select * from NIVADATABASE.SED_PARAM_STAT where rownum < 4")   # CHECK SED_PARAMETER_DEFINITIONS, SED_PARAMETERs_METHODS 
# get_nivabase_data("select * from NIVADATABASE.SED_PARAMETER_DEFINITIONS where rownum < 4")   

df_par_met <- get_nivabase_data("select PARAMETER_ID, METHOD_ID, CONVERSION_FACTOR from NIVADATABASE.WC_PARAMETERS_METHODS")
df_par_def <- get_nivabase_data("select PARAMETER_ID, NAME, UNIT, CATEGORY, SORT_NAME, SIMPLE_NAME from NIVADATABASE.WC_PARAMETER_DEFINITIONS")
df_par_met <- get_nivabase_selection("*", "WC_PARAMETERS_METHODS", "METHOD_ID", df_all$METHOD_ID)
df_par_met <- get_nivabase_selection("*", "WC_PARAMETERS_METHODS", "METHOD_ID", df_all$METHOD_ID)

