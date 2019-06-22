
## 0a. Libraries ----
library(dplyr)
library(purrr)
library(ggplot2)
library(stringr)    # str_extract
library(readxl)     # read_excel
library(lubridate)  # year, dmy_hms
library(janitor)    # compare_df_cols

source("02_Read_data_functions.R")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## 0b. Where the data is ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

folder_data <- "K:/Avdeling/Mar/Msc/Artikkel MSC Littorina/Data/TBT"  # original
folder_data <- "Input_data/TBT"                                                           # above folder, copied
# folder_data_upper <- "Input_data"
files <- dir(folder_data, pattern = ".xlsx")
files <- files[!grepl("^~", files)]           # Exclude temporary files (that are)when a file is opened)
files


#
# 1. TBT in snails ----
#

#
# File 1: 2005-2011 (snails) ----
#
# Already in format easy to use
#
fn <- "TBT i snegler 2005 til 2011.xlsx"
fn_full <- paste0(folder_data, "/", fn)
# sheets <- readxl::excel_sheets(fn_full)
# sheets
df1 <- read_excel(fn_full, sheet = "bearb_data", range = "A2:P30") 
# TBT already in numeric form
names(df1)[1:5] <- c("EUNOMO", "Sample", "Station", "Year", "Species")
# ¤df1$Species

df1 <- df1 %>%
  mutate(Species = ifelse(Species == "Hinia", "Nassarius", Species))

# Test plot
ggplot(df1, aes(Year, `Tributyltinn (TBT) (µg/kg)`, color = Species)) +
  geom_line() + geom_point() +
  facet_wrap(~Station)

#
# File 2: 2013 (snails + sediment) ----
#
# Eurofins file
#
fn <- "TBT i snegler og sediment 2013.xlsx"
fn_full <- paste0(folder_data, "/", fn)
df2_headers <- read_excel(fn_full, sheet = 1, range = "A6:O7") 
df2 <- read_excel(fn_full, sheet = 1, range = "A9:O24") %>%
  as.data.frame()
names(df2)[6:15] <- names(df2_headers)[6:15]
df2  # Check that TBT looks ok

# Set year, station, species
df2$Year <- 2013
df2$Station <- ""
df2$Species <- ""

sel <- grepl("Hinia", df2$Merking)
df2$Station[sel] <- sub("Hinia +", "", df2$Merking[sel])
df2$Species[sel] <- "Nassarius"

sel <- grepl("Kongsnegl", df2$Merking)
df2$Station[sel] <- sub("Kongsnegl", "", df2$Merking[sel])
df2$Species[sel] <- "Buccinum"

sel <- grepl("LITLI", df2$Merking)
df2$Station[sel] <- sub("LITLI +", "", df2$Merking[sel])
df2$Species[sel] <- "Littorina"

sel <- grepl("Sediment", df2$Merking)
df2$Station[sel] <- sub("Sediment +", "", df2$Merking[sel])
df2$Species[sel] <- "Sediment"

df2 %>% select(Merking, Station, Species)

df2 %>% pull(`TBT-B EF`)

#
# File 3 - 2014 (snails) ----
#
# Eurofins file
#

fn <- "TBT i snegler 2014.xlsx"
fn_full <- paste0(folder_data, "/", fn)
df3_headers <- read_excel(fn_full, sheet = 1, range = "A6:O7") 
df3 <- read_excel(fn_full, sheet = 1, range = "A9:O19") %>%
  as.data.frame()
names(df3)[6:15] <- names(df3_headers)[6:15]
names(df3)[6] <- "Testno"
# df3

# Set year, station, species
df3$Year <- 2014
df3$Station <- ""
df3$Species <- ""

sel <- grepl("Nasarius", df3$Merking)
df3$Station[sel] <- sub(" +Nasarius", "", df3$Merking[sel])
df3$Species[sel] <- "Nassarius"

sel <- grepl("Kongsnegl", df3$Merking)
df3$Station[sel] <- sub(" +Kongsnegl", "", df3$Merking[sel])
df3$Species[sel] <- "Buccinum"

sel <- grepl("LITLI", df3$Merking)
df3$Station[sel] <- sub(" +LITLI", "", df3$Merking[sel])
df3$Species[sel] <- "Littorina"

# Remove "St"
df3$Station <- sub("St", "", df3$Station, ignore.case = TRUE)

# Check
df3 %>% select(Merking, Station, Species)

#
# File 4 - 2016 og 2018 (snails + blue mussel) ----
#


fn <- "TBT i blåskjell og strandsnegl 2016 og 2018 fra Aquamonitor.xlsx"
fn_full <- paste0(folder_data, "/", fn)
df4_headers <- read_excel(fn_full, sheet = "BiotaChemistry", range = "A1:BG2") 

df4 <- read_excel(fn_full, sheet = "BiotaChemistry", range = "B2:BG28")

names(df4)[12:58] <- names(df4_headers)[13:59]  # Note different indices

df4 <- df4 %>%
  mutate(
    Station = stringr::str_extract(StationCode, "[:digit:]+"),
    Species = case_when(TaxonName %in% "Blåskjell" ~ "Mytilus",
                        TaxonName %in% "Storstrandsnegl" ~ "Littorina"),
    Year = year(dmy_hms(df4$SampleDate))
  )

df4 %>% 
  count(StationCode, Station, Species)

df4$TBT

#
# Combine file 1-4 , checking----
#

# Variable names (check)
colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)

# "Tributyltinn (TBT) (µg/kg)"
# "TBT-B EF"
# "TBT-B EF"
# "TBT"

# Classes (check)
# 2 and 3 are character (text variables)
df1 %>% 
  rename(TBT = `Tributyltinn (TBT) (µg/kg)`) %>% 
  pull(TBT) %>% class()
df2 %>% 
  rename(TBT = `TBT-B EF`) %>% 
  pull(TBT) %>% class()
df3 %>% 
  rename(TBT = `TBT-B EF`) %>% 
  pull(TBT) %>% class()
df4 %>% 
  pull(TBT) %>% class()

# For those with character values (test)
df2 %>% 
  rename(X = `TBT-B EF`) %>% 
  mutate(TBT = str_extract(X, "[0-9,.]+") %>% as.numeric(),
         TBT_flag = ifelse(str_detect(X, "<"), "<", NA)) %>%
  select(X, TBT, TBT_flag)

#
# Combine file 1-4 , actual ----
#
dat <- bind_rows(
  df1 %>% 
    rename(TBT = `Tributyltinn (TBT) (µg/kg)`) %>%
    mutate(TBT_flag = as.character(NA)) %>%
    select(Station, Year, Species, TBT, TBT_flag),  
  df2 %>% 
    rename(X = `TBT-B EF`) %>% 
    mutate(TBT = str_extract(X, "[0-9,.]+") %>% as.numeric(),
           TBT_flag = ifelse(str_detect(X, "<"), "<", NA)) %>%
    select(Station, Year, Species, TBT, TBT_flag),  
  df3 %>% 
    rename(X = `TBT-B EF`) %>% 
    mutate(TBT = str_extract(X, "[0-9,.]+") %>% as.numeric(),
           TBT_flag = ifelse(str_detect(X, "<"), "<", NA)) %>%
    select(Station, Year, Species, TBT, TBT_flag),  
  df4 %>% 
    mutate(TBT_flag = as.character(NA)) %>%
    select(Station, Year, Species, TBT, TBT_flag)
)

xtabs(~Year + Station + Species, dat)
# Test plot
ggplot(dat, aes(Year, TBT, color = Species)) +
  geom_line() + geom_point() +
  facet_wrap(~paste(Species, Station))

#
# Save data ----
#
# We call the files "snegl' even though there is some blue mussel and sediment inside too
#
#
saveRDS(dat, file = "Data/Snegl_tbt_2005_2018.RData")
openxlsx::write.xlsx(dat, file = "Data/Snegl_tbt_2005_2018.xlsx")


#
# File 5 - 2004-2005 (blue mussel + sediment) ----
#
# Sheet 1
# Eurofins file
#
fn <- "TBT i blåskjell i 2005 og sediment i 2004 og 2005.xlsx"
fn_full <- paste0(folder_data, "/", fn)
df_headers <- read_excel(fn_full, sheet = 1, range = "A3:BB4") 
df <- read_excel(fn_full, sheet = 1, range = "A2:BB48", col_types = "text") %>%
  as.data.frame()
names(df)[5:11] <- names(df_headers)[5:11]

# df  # Check that TBT looks ok

# Set year, station, species
df5a <- df %>%
  filter(!is.na(Prøvenr)) %>%
  mutate(Year = substr(Prøvenr, 1, 4),           # we pick year from prøvenummer, not sample date
         Station = sub("St. +", "", Merket),     # Remove "St." and "St" 
         Station = sub("St +", "", Station),
         Species = "Sediment",
         X = sub(",", ".", `TBT-Sm`, fixed = TRUE),
         X = sub("s", "", X, fixed = TRUE),
         TBT = as.numeric(X))
# df5a %>% select(Tatt, Merket, Year, Station, Species, TBT)
df5a %>% count(Merket, Year, Station, Species)

#
# Sheet 2
#
sheet <- "Blåskjell"
df_headers <- read_excel(fn_full, sheet = sheet, range = "A2:K3") 
df <- read_excel(fn_full, sheet = sheet, range = "A1:K11", col_types = "text") %>%
  as.data.frame()
df <- df[-(1:2),]
names(df)[1:2] <- names(df_headers)[1:2]

df5a <- df %>%
  mutate(Year = substr(Tatt, 1, 4),           # we pick year from prøvenummer, not sample date
         Station = str_extract(Merket, "[0-9]+"),     # Remove "St." and "St" 
         Species = "Mytilus",
         X = sub(",", ".", `TBT-B`, fixed = TRUE),
         X = sub("s", "", X, fixed = TRUE),
         TBT = as.numeric(X))

head(df5a)


#
# File 5 - 2004-2005 (blue mussel + sediment) ----
#
# Sheet 1
# Eurofins file
#
fn <- "TBT i blåskjell i 2005 og sediment i 2004 og 2005.xlsx"
fn_full <- paste0(folder_data, "/", fn)
df_headers <- read_excel(fn_full, sheet = 1, range = "A3:BB4") 
df <- read_excel(fn_full, sheet = 1, range = "A2:BB48", col_types = "text") %>%
  as.data.frame()
names(df)[5:11] <- names(df_headers)[5:11]

# df  # Check that TBT looks ok

# Set year, station, species
df5a <- df %>%
  filter(!is.na(Prøvenr)) %>%
  mutate(Year = substr(Prøvenr, 1, 4),           # we pick year from prøvenummer, not sample date
         Station = sub("St. +", "", Merket),     # Remove "St." and "St" 
         Station = sub("St +", "", Station),
         Species = "Sediment",
         X = sub(",", ".", `TBT-Sm`, fixed = TRUE),
         X = sub("s", "", X, fixed = TRUE),
         TBT = as.numeric(X))
# df5a %>% select(Tatt, Merket, Year, Station, Species, TBT)
df5a %>% count(Merket, Year, Station, Species)


# GOTTEN THIS FAR ----


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## 1. Strandsnegl (common periwinkle) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## . a 1997 data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

fn <- "Diverse strandsnegl intersex 1997.xls"        # File of type 1
fn_full <- paste0(folder_data[1], "/", fn)
sheets <- readxl::excel_sheets(fn_full)
sheets







#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# OLD STUFF ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# df4
# unique(df4$Merking) %>% dput()
fn <- "TBT i blåskjell og strandsnegl 2016 og 2018 bearbeidede data.xlsx"
fn_full <- paste0(folder_data, "/", fn)
df4 <- read_excel(fn_full, sheet = "2016 og 2018", range = "A2:K33")  %>%
  tidyr::gather(key = "Merking", value = "Verdi", -(Parameter:Grenseverdi))

# df4
# unique(df4$Merking) %>% dput()

df_station_year <- tibble(
  Merking = c("Bl6 Nymo mars 2016", "Bl6 Nymo 2018", "Bl5 Bie mars 2016", 
              "Bl5 Bie 2018", "Bl4 Kjellviga mars 2016", "Bl4 Kjellviga 2018", 
              "Bl3 Biodden mars 2016", "Bl3 Biodden 2018"), 
  Station = c("6", "6", "5",
              "5", "4", "4",
              "3", 3),
  Year = c(2016, 2018, 2016,
           2018, 2016, 2018,
           2016, 2018)
)

df4 <- df4 %>%
  left_join(df_station_year) %>%
  filter(Parameter %in% "Tributyltinn") %>%
  mutate(Verdi = as.numeric(Verdi))
