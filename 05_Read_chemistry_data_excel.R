
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
  mutate(Species = ifelse(Species == "Hinia", "Nassarius", Species),
         Merket = as.character(NA))

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

df2 <- df2 %>%
  rename(Merket = Merking)  %>%   # just to keep with other files such as 5 and 6 below
  mutate(Prøvedyp = as.character(NA))

# Set year, station, species
df2$Year <- 2013
df2$Station <- ""
df2$Species <- ""

sel <- grepl("Hinia", df2$Merket)
df2$Station[sel] <- sub("Hinia +", "", df2$Merket[sel])
df2$Species[sel] <- "Nassarius"

sel <- grepl("Kongsnegl", df2$Merket)
df2$Station[sel] <- sub("Kongsnegl", "", df2$Merket[sel])
df2$Species[sel] <- "Buccinum"

sel <- grepl("LITLI", df2$Merket)
df2$Station[sel] <- sub("LITLI +", "", df2$Merket[sel])
df2$Species[sel] <- "Littorina"

sel <- grepl("Sediment", df2$Merket)
df2$Station[sel] <- sub("Sediment +", "", df2$Merket[sel])
df2$Species[sel] <- "Sediment"

df2 %>% select(Merket, Station, Species)

df2 %>% pull(`TBT-B EF`)

df2 %>% count(Year, Station, Prøvedyp)

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
df3 <- rename(df3, Merket = Merking)
# df3

df3 <- df3 %>%
  rename(Merket = Merking)    # just to keep with other files such as 5 and 6 below

# Set year, station, species
df3$Year <- 2014
df3$Station <- ""
df3$Species <- ""

sel <- grepl("Nasarius", df3$Merket)
df3$Station[sel] <- sub(" +Nasarius", "", df3$Merket[sel])
df3$Species[sel] <- "Nassarius"

sel <- grepl("Kongsnegl", df3$Merket)
df3$Station[sel] <- sub(" +Kongsnegl", "", df3$Merket[sel])
df3$Species[sel] <- "Buccinum"

sel <- grepl("LITLI", df3$Merket)
df3$Station[sel] <- sub(" +LITLI", "", df3$Merket[sel])
df3$Species[sel] <- "Littorina"

# Remove "St"
df3$Station <- sub("St", "", df3$Station, ignore.case = TRUE)

# Check
df3 %>% select(Merket, Station, Species)

df3 %>% count(Species, Station)

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
    Year = year(dmy_hms(df4$SampleDate)),
    Merket = paste(StationCode, StationName)
  )

# All samples are duplicated
df4 %>% 
  count(StationCode, Station, Species, Year, SampleId)
# df4 %>% 
#   arrange(StationCode, Station, Species, Year, SampleId) %>%
#   View()

# Pick first observation for each sample
df4 <- df4 %>% 
  select(-ProjectName) %>%
  group_by(StationCode, Station, Species, Year, SampleId) %>%
  summarise_all(first)

# df4 %>% View()


df4 %>% 
  count(StationCode, Station, Species)

# df4$TBT

#
# File 5 - 2004-2005 (blue mussel + sediment) ----
#

#
# . 5a (sediment) ----
# Sheet 1
# Eurofins file
#
fn <- "TBT i blåskjell i 2005 og sediment i 2004 og 2005.xlsx"
fn_full <- paste0(folder_data, "/", fn)
sheet <- "Analyseresultater"
df_headers <- read_excel(fn_full, sheet = sheet, range = "A3:BB4") 
df <- read_excel(fn_full, sheet = sheet, range = "A2:BB48", col_types = "text") %>%
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
df5a %>% count(Year, Station, Prøvedyp)

#
# . 5b (blue mussel) ----
#
df_headers <- read_excel(fn_full, sheet = "Blåskjell", range = "A1:K2", col_types = "text")
df <- read_excel(fn_full, sheet = "Blåskjell", range = "A2:K11")
names(df)[-(1:2)] <- names(df_headers)[-(1:2)] 

df5b <- df %>%
  filter(!is.na(Tatt)) %>%
  mutate(Year = substr(Tatt, 1, 4),           # we pick year from prøvenummer, not sample date
         Station = str_extract(Merket, "[0-9]+"),     # Remove "St." and "St" 
         Station = sub("St +", "", Station),
         Species = "Mytilus",
         X = sub(",", ".", `TBT-B`, fixed = TRUE),
         X = sub("<", "", X, fixed = TRUE),
         TBT = as.numeric(X))

df5b

df5b %>%
  count(Species, Station)

#
# File 6 - 2008 (blue mussel + sediment) ----
#

#
# . 6a (sediment) ----
#

# Sheet 1
# Eurofins file
#
fn <- "TBT i blåskjell og sediment 2008.xlsx"
fn_full <- paste0(folder_data, "/", fn)
sheet <- "Sediment"
df_headers <- read_excel(fn_full, sheet = sheet, range = "A2:BC3") 
df <- read_excel(fn_full, sheet = sheet, range = "A3:BC20", col_types = "text")
names(df)[-(1:8)] <- names(df_headers)[-(1:8)]
head(df)

# df  # Check that TBT looks ok

# Set year, station, species
df6a <- df %>%
  filter(!is.na(Tatt)) %>%
  mutate(Year = substr(Tatt, 1, 4),           # we pick year from prøvenummer, not sample date
         Station = sub("St. +", "", Merket),     # Remove "St." and "St" 
         Station = sub("St +", "", Station),
         Species = "Sediment",
         X = sub(",", ".", `TBT-Sm`, fixed = TRUE),
         X = sub("s", "", X, fixed = TRUE),
         TBT = as.numeric(X))

# PReliminary check
df6a %>% count(Merket, Year, Station, Species)

df6a <- df6a %>%
  mutate(Station = sub(" 0-5 cm", "", Station, fixed = TRUE)) %>%
  mutate(Station = str_extract(Station, "[0-9]+"), 
         Prøvedyp = ifelse(grepl("0-5", Merket), "0-5", as.character(NA))     # NOTE: HARD-CODED (all are 0-5 cm)
  )


# df6a %>% select(Tatt, Merket, Year, Station, Species, TBT)
df6a %>% count(Merket, Year, Station, Species)
df6a %>% count(Year, Station, Prøvedyp)

#
# . 6b (blue mussel) ----
#
sheet <- "Blåskjell"
df_headers <- read_excel(fn_full, sheet = sheet, range = "A2:DN3", col_types = "text")
df <- read_excel(fn_full, sheet = sheet, range = "A3:DN11")
names(df)[-(1:10)] <- names(df_headers)[-(1:10)] 

df6b <- df %>%
  filter(!is.na(Tatt)) %>%
  mutate(Year = substr(Tatt, 1, 4),           # we pick year from prøvenummer, not sample date
         Station = str_extract(Merket, "[0-9]+"),     # Remove "St." and "St" 
         Station = sub("St +", "", Station),
         Species = "Mytilus",
         X = sub(",", ".", `TBT-B`, fixed = TRUE),
         X = sub("<", "", X, fixed = TRUE),
         TBT = as.numeric(X))

# df6b %>% select(Tatt, Merket, Year, Station, Species, TBT)
df6b %>% count(Merket, Year, Station, Species)
df6b


#
# File 7 - 2007 (blue mussel) ----
#
# Sheet 1
# Eurofins file
#
fn <- "TBT i blåskjell 2007.xlsx"
fn_full <- paste0(folder_data, "/", fn)
sheet <- "Sheet1"


df_headers <- read_excel(fn_full, sheet = sheet, range = "C6:Y7") 
df <- read_excel(fn_full, sheet = sheet, range = "C8:Y22", col_types = "text")
names(df)[-(1:4)] <- names(df_headers)[-(1:4)]
names(df)[1:2] <- c("Tatt", "Merket")
head(df)

# df  # Check that TBT looks ok

# Set year, station, species
df7 <- df %>%
  filter(!is.na(TESTNO)) %>%
  mutate(Year = substr(Tatt, 1, 4),           # we pick year from prøvenummer, not sample date
         Station = sub("St. +", "", Merket),     # Remove "St." and "St" 
         Station = sub("St +", "", Station),
         Species = "Mytilus",
         X = sub(",", ".", `TBT-B`, fixed = TRUE),
         X = sub("s", "", X, fixed = TRUE),
         TBT = as.numeric(X))
# df7 %>% select(Tatt, Merket, Year, Station, Species, TBT)
df7 %>% count(Merket, Year, Station, Species)

df7 <- df7 %>%
  mutate(Station = substr(Station, 1, 1))
df7 %>% count(Merket, Year, Station, Species)

         
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Combine file 1-7 - preparations ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# . a Make list of datasets ----
datasetnames <- strsplit("df1, df2, df3, df4, df5a, df5b, df6a, df6b, df7", ", ")[[1]]
files <- list(df1, df2, df3, df4, df5a, df5b, df6a, df6b, df7)
names(files) <- datasetnames

# backup
# files_back <- files
# revert to backup
# files <- files_back

# Check column names
files %>% map(~grep("TBT", names(.), value = TRUE))


# . b Sediment depth - must be in all datasets ----

# Files with Species = Sediment
check1 <- files %>% map_lgl(~"Sediment" %in% unique(.$Species))
# Files containing variable Prøvedyp
check2 <- files %>% map_lgl(~"Prøvedyp" %in% names(.))

check1 == check2  # should be all TRUE

# Add Prøvedyp for files lacking it
for (i in which(!check2)){
  files[[i]] <- files[[i]] %>% mutate(Prøvedyp = as.character(NA))
}


# . c Pick variables and set name of TBT variable to "TBT"----

# Set TBT column
name_tbt <- c(
  "Tributyltinn (TBT) (µg/kg)",
  "TBT-B EF",
  "TBT-B EF",
  "TBT",
  "TBT-Sm", "TBT-B",
  "TBT-Sm", "TBT-B",
  "TBT-B"
)
names(name_tbt) <- datasetnames

# Check if "Merket" is available
files %>% map_lgl(~"Merket" %in% names(.))

# Pick variables and set name of TBT variable to "TBT" 
for (datasetname in datasetnames){
  variables <- c("Station", "Year", "Species", "Prøvedyp", 
                 "Merket", name_tbt[datasetname])
  files[[datasetname]] <- files[[datasetname]][variables]
  names(files[[datasetname]])[6] <- "TBT"    # note: "4" is hard-coded - depends of number of variables in select()
}

#
# . d Fix concentrations/flags ----
#

# Check class of TBT
files %>% map_chr(~class(.$TBT))

# ... or check class of all variables
compare_df_cols(files, return = "mismatch")

# Check whether TBT data contains flag "s" or "<"
check_flag <- function(datasetname){
  df <- files[[datasetname]]
  tibble(datasetname = datasetname,
    `Flag <`= sum(grepl("<", df$TBT, fixed = TRUE)),
    `Flag s`= sum(grepl("s", df$TBT, fixed = TRUE)),
    `Flag S`= sum(grepl("S", df$TBT, fixed = TRUE))
  )
}  
# check_flag("df2")
flags <- datasetnames %>% map_df(check_flag)
# flags


# When concentration is a text variable, extract the number and and less-than flag
#   where a "<" is found
# NOTE: The "s" flag (for "larger than" is simply ignored!
fix_concentrations <- function(df){
  # cat(names(df))
  if (class(df$TBT) == "character"){
    df <- df %>%
      mutate(X = TBT) %>%
      mutate(TBT = str_extract(X, "[0-9,.]+") %>% as.numeric(),
             TBT_flag = ifelse(str_detect(X, "<"), "<", as.character(NA))) %>% 
      select(-X)
  } else {
    df <- df %>%
      mutate(
        TBT_flag = as.character(NA)
             )
  }
  df
}

# Test
# fix_concentrations(files[[1]]) %>% str()
# fix_concentrations(files[[2]]) %>% str()
# fix_concentrations(files[[5]]) %>% str()

# Fix all files
files <- files %>% map(fix_concentrations)

# Check variable classes
compare_df_cols(files, return = "mismatch")

# Fix character variables (here, year)
change_character_to_numeric <- function(df, varname){
  df[[varname]] = as.numeric(df[[varname]])
  df
}
# test <- files[[5]] %>% change_character_to_numeric("Year")
files <- files %>% map(change_character_to_numeric, "Year")

#
# Combine data ----
#

# Add name of dataset
files <- datasetnames %>% map(~ mutate(files[[.]], df_name = .))
  
# Bind together
dat <- bind_rows(files)

#
# Save data ----
#
# We call the files "snegl' even though there is some blue mussel and sediment inside too
#
#
saveRDS(dat, file = "Data/05_TBT.RData")
openxlsx::write.xlsx(dat, file = "Data/05_TBT.xlsx")

#
# Tables etc ----
#

#
# Species
#
xtabs(~Species, dat)

# Stations and datasets
xtabs(~Station + df_name, dat %>% filter(Species %in% "Littorina"))
xtabs(~Station + df_name, dat %>% filter(Species %in% "Mytilus"))
xtabs(~Station + df_name, dat %>% filter(Species %in% "Sediment"))

xtabs(~Station + Year, dat %>% filter(Species %in% "Littorina"))
xtabs(~Station + Year, dat %>% filter(Species %in% "Mytilus"))
xtabs(~Station + Year, dat %>% filter(Species %in% "Sediment"))

xtabs(~addNA(Station) + df_name, dat)

# Test plot
dat %>%
  filter(Species %in% "Littorina") %>%
  ggplot(aes(Year, TBT, color = Species)) +
  geom_line() + geom_point() +
  facet_wrap(~paste(Species, Station))

dat %>%
  filter(Species %in% "Mytilus") %>%
  ggplot(aes(Year, TBT, color = Species)) +
  #geom_smooth() + 
  geom_point() +
  facet_wrap(~paste(Species, Station))












