
#
# Checking to see if we have gotten TBT data for 2021  
# See 

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

add_latin_name <- function(data){
  data %>%
    left_join(
      get_nivabase_selection("TAXONOMY_CODE_ID, LATIN_NAME", 
                             "NIVADATABASE.TAXONOMY_CODES left join NIVADATABASE.TAXONOMY on NIVADATABASE.TAXONOMY_CODES.NIVA_TAXON_ID = NIVADATABASE.TAXONOMY.NIVA_TAXON_ID", 
                             "TAXONOMY_CODE_ID", 
                             unique(data$TAXONOMY_CODE_ID), table_literal = TRUE), 
      by = "TAXONOMY_CODE_ID"
    )
}

add_o_number <- function(data){
  data %>%
    left_join(
      get_nivabase_selection("PROJECT_ID, O_NUMBER", 
                             "NIVADATABASE.PROJECTS_O_NUMBERS", 
                             "PROJECT_ID", 
                             unique(data$PROJECT_ID), table_literal = TRUE), 
      by = "PROJECT_ID"
    )
}

  
  
#  get_nivabase_data("select PROJECTS_O_NUMBERS_ID, PROJECT_ID, O_NUMBER from NIVADATABASE.PROJECTS_O_NUMBERS")  



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
df_proj$PROJECT_ID     # 118, 9246, 12386, 12578
df_proj$PROJECT_NAME  

df_proj %>%
  count(PROJECT_ID, PROJECT_NAME, PROJECT_DESCRIPTION)
# PROJECT_ID                         PROJECT_NAME                           PROJECT_DESCRIPTION n
# 1        118                             Vikkilen                                Kristoffer Næs 1
# 2       9246                         Viktilt 2022                Overvåking i Vikkilen for Nymo 1
# 3      12386        Overvåking av Vikkilen i 2018                        Blåskjell, Strandsnegl 1
# 4      12578 Miljøovervåking i Vikkilen 2015-2022 Sedimenter, blåskjell, strandsnegl, bunnfauna 1



#
# Get a list of the stations associated with those 3 projects
#
df_stations <- df_proj$PROJECT_NAME %>% map_df(~get_stations_from_project(., exact = TRUE)) %>%
  add_o_number()
nrow(df_stations)  # 41

xtabs(~PROJECT_ID)

df_stations_unique <- df_stations %>%
  distinct(STATION_ID, STATION_CODE, O_NUMBER)
# O number = 15404 for all 


#
# 3. Chemicals in BIOTA ----
#
# Finding only data from 2016 + 2018 
#

df_specimens1 <- get_specimens_from_stationdata(df_stations)
nrow(df_specimens1)  # 14
df_specimens1 <- df_specimens1 %>% 
  add_latin_name() %>%
  left_join(df_stations_unique)
nrow(df_specimens1)  # 14
tab <- xtabs(~DATE_CAUGHT + STATION_CODE + LATIN_NAME, df_specimens1)  # 2016 + 2018
tab
names(tab)

df_stations %>%
  filter(STATION_ID %in% as.numeric(colnames(tab))) %>%
  arrange(STATION_ID) 
 
#
# 4. Check in labware tables
#

#
# NOT POSSIBLE AT THE MOMENT
#
get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLES where rownum < 3")
# Error: nanodbc/nanodbc.cpp:1617: 42S02: [Oracle][ODBC][Ora]ORA-00942: table or view does not exist
# 
# <SQL> 'select * from NIVADATABASE.LABWARE_CHECK_SAMPLES where rownum < 3'
# Execution halted

get_nivabase_data("select * from LABWARE.V_SAMPLES_ONGOING where rownum < 3")    # fails
get_nivabase_data("select * from LABWARE.V_NIVA_TISSUE_TYPES where rownum < 3")  # OK
get_nivabase_data("select * from LABWARE.V_RPT_BESTILLINGX where rownum < 3")    # fails

get_nivabase_data("select OWNER,TABLE_NAME from ALL_TAB_COLUMNS where column_name = 'TISSUE_ID'")  
get_nivabase_data("select OWNER,TABLE_NAME from ALL_TAB_COLUMNS where column_name = 'TISSUE'")  

get_nivabase_data("select OWNER,TABLE_NAME,COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'LABWARE_CHECK_SAMPLE'")  

#
# Search by project
#

df1 <- get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where rownum < 6")


df1 <- get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where PROSJEKT LIKE '%15404%'")
nrow(df1) # 197
head(df1)
df1 %>% filter(grepl("2019", TEXT_ID)) %>% arrange(DESCRIPTION) %>% View()

get_nivabase_data("select OWNER,TABLE_NAME from ALL_TAB_COLUMNS where column_name = 'TEXT_ID'")  


#
# Project
#

# get_nivabase_data("select OWNER,TABLE_NAME from ALL_TAB_COLUMNS where column_name = 'O_NUMBER'")  
df_o <- get_nivabase_data("select PROJECTS_O_NUMBERS_ID, PROJECT_ID, O_NUMBER from NIVADATABASE.PROJECTS_O_NUMBERS")  
sel <- grepl("15404", df_o$O_NUMBER)
df_o[sel,]
projid <- df_o$PROJECT_ID[sel]   # 12456
df_proj %>%
  filter(PROJECT_ID == projid)

#
# Get samples
#

get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where PROSJEKT = 'Havforsuring Kyst'")

df1 <- get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where PROSJEKT = 'Havforsuring Kyst'")
nrow(df1) # 0


