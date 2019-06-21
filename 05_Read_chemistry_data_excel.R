
## 0a. Libraries ----
library(dplyr)
library(purrr)
library(ggplot2)
library(readxl)
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
# File 1: 2005-2011
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

ggplot(df1, aes(Year, `Tributyltinn (TBT) (µg/kg)`, color = Species)) +
  geom_line() + geom_point() +
  facet_wrap(~Station)

#
# File 2: 2013
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

#
# File 3 - 2014 
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
df3$Station[sel] <- sub(" +LITLI", "", df3$Merking[sel], fixed = TRUE)
df3$Species[sel] <- "Littorina"

# Check
df3 %>% select(Merking, Station, Species)

#
# File 4 - 2016 og 2018
#


fn <- "TBT i blåskjell og strandsnegl 2016 og 2018 fra Aquamonitor.xlsx"
fn_full <- paste0(folder_data, "/", fn)
df4_headers <- read_excel(fn_full, sheet = "BiotaChemistry", range = "A1:BG2") 
df4 <- read_excel(fn_full, sheet = "BiotaChemistry", range = "B2:BG28") %>%
  filter(TaxonName %in% "Storstrandsnegl")
# i <- which(names(df4_headers) == "TBT")
# df4[,i-1]
names(df4)[12:58] <- names(df4_headers)[13:59]  # Note different indices
# df4$TBT

df4$Station <- stringr::str_extract( df4$StationCode, "[:digit:]+")
df4$Species <- "Littorina"
df4$Year <- year(dmy_hms(df4$SampleDate))


#
#
#
# GOTTEN THIS FAR ----
#
#
#




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
