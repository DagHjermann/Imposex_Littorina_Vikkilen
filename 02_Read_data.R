
#
# Reading excel files of intersex, 4 species (see file names below)
# The data of each species is in one folder and one file, ut with several sheets
# The sheet names has station and year info (e.g. "ST6_05" = Station 6, 2005) with the exception of purpursnegl
# 
# The excel format used is one of two types, type 1 (kongsnegl, nettsnegl, purpursnegl) or type 2 (strandsnegl)
# 
# Files made (folder 'data'):
#
# Strandsnegl_intersex_2005_2018.RData  # common periwinkle (Littorina littorea)
# Kongsnegl_intersex_2013_2014.RData    # common whelk (Buccinum undatum) 
# Nettsnegl_intersex_2007_2014.RData    # netted dog whelk (Nassarius reticulatus = Hinia reticulata)
# Purpursnegl_intersex_2007_2014.RData  # dog whelk (Nucella lapillus)
#
# and corresponding excel files
#

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

folder_data_upper <- "K:/Avdeling/Mar/Msc/Artikkel MSC Littorina/Data/Intersex og imposex"  # original
folder_data_upper <- "Input_data"                                                           # above folder, copied
# folder_data_upper <- "Input_data"
dir(folder_data_upper)

folder_data <- paste0(folder_data_upper, "/", 
                      c("Strandsnegl-Littorina/Skjema som er punchet eller brukt", 
                        "Kongsnegl-Buccinum", "Nettsnegl-Nassarius-Hinia", "Purpursnegl - Nucella")
)

dir(folder_data[1])

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
# Read data
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

data_1997 <-read_excel(fn_full, sheet = sheets[1])

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## . b 2005-2018 ----
# Type 2 file
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

fn <- "Strandsnegl intersex 2005 til 2018_lis.xls" # type 2
fn <- "Strandsnegl intersex 2005 til 2018.xls" # type 2
fn_full <- paste0(folder_data[1], "/", fn)
sheets <- readxl::excel_sheets(fn_full)
sheets

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . b1 Read data using function ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# debugonce(read_intersex_type2)
data_list_onefile <- sheets %>% purrr::map(~read_intersex_type2(fn_full, ., headerline = 3))
names(data_list_onefile) <- sheets

# Hard check 1
# check column names - must be alike
# transpose(data_list_onefile)$colnames

# Check 2, easier but incomplete
# Checks column names - more stringent - all should be 0
# check <- transpose(data_list_onefile)$colnames %>% bind_rows()
# for (i in 1:ncol(check)){
#   cat(i, sum(check[,i] != check[,1]), "\n")
# }

# Best check 
library(janitor)
transpose(data_list_onefile)$data %>% compare_df_cols(return = "mismatch")  # should be zero
# OR check this:
# transpose(data_list_onefile)$data %>% compare_df_cols()

# Combine data
data_onefile <- transpose(data_list_onefile)$data %>% bind_rows()

# Column name "F" also means False - let us change it
data_onefile <- data_onefile %>%
  rename(Male = `M`, Female = `F`) %>%
  mutate(Sex = tolower(Sex))

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . b2 Fix columns Sex, Female, Male ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Check no 1
data_onefile %>%
  count(Sex, Female, Male) %>%
  arrange(desc(n))

#  Male is NA, seems to be male
sel <- with(data_onefile, is.na(Male) & !(Female %in% 1)  & !(Sex %in% "f"))
data_onefile$Male[sel] <- 1
data_onefile$Female[sel] <- 0
data_onefile$Sex[sel] <- "m"

#  Male is NA, seems to be female
sel <- with(data_onefile, is.na(Male) & !(Female %in% 0)  & !(Sex %in% "m"))
data_onefile$Male[sel] <- 0
data_onefile$Female[sel] <- 1
data_onefile$Sex[sel] <- "f"

#  Female is NA, seems to be male
sel <- with(data_onefile, is.na(Female) & !(Male %in% 0)  & !(Sex %in% "f"))
data_onefile$Male[sel] <- 1
data_onefile$Female[sel] <- 0
data_onefile$Sex[sel] <- "m"

#  Female is NA, seems to be female
sel <- with(data_onefile, is.na(Female) & !(Male %in% 1)  & !(Sex %in% "m"))
data_onefile$Male[sel] <- 0
data_onefile$Female[sel] <- 1
data_onefile$Sex[sel] <- "f"

# Check no 2
data_onefile %>%
  count(Sex, Female, Male) %>%
  arrange(desc(n))

# Sex and Female disagrees
sel1 <- with(data_onefile, Sex %in% "m" & Female %in% 1)
# Probably female
sel2 <- sel1 & with(data_onefile, is.na(N_penisglands) & !is.na(ISI))
data_onefile$Male[sel2] <- 0
data_onefile$Female[sel2] <- 1
data_onefile$Sex[sel2] <- "f"

# Probably male
sel2 <- sel1 & with(data_onefile, !is.na(N_penisglands) & is.na(ISI))
data_onefile$Male[sel2] <- 1
data_onefile$Female[sel2] <- 0
data_onefile$Sex[sel2] <- "m"

# Sex and Male disagrees
sel1 <- with(data_onefile, Sex %in% "f" & Male %in% 1)
# Probably female
sel2 <- sel1 & with(data_onefile, is.na(N_penisglands) & !is.na(ISI))
data_onefile$Male[sel2] <- 0
data_onefile$Female[sel2] <- 1
data_onefile$Sex[sel2] <- "f"

# Probably male
sel2 <- sel1 & with(data_onefile, !is.na(N_penisglands) & is.na(ISI))
data_onefile$Male[sel2] <- 1
data_onefile$Female[sel2] <- 0
data_onefile$Sex[sel2] <- "m"

# Check no 3
data_onefile %>%
  count(Sex, Female, Male) %>%
  arrange(desc(n))

# For the last 2 - no info except shell height 
sel <- with(data_onefile, is.na(Sex) & Male %in% 1)
sum(sel)  # 2
data_onefile[sel,]

# Delete the last 2
data_onefile <- data_onefile %>%
  filter(!(is.na(Sex) & Male %in% 1))

# Check no 4
data_onefile %>%
  count(Sex, Female, Male) %>%
  arrange(desc(n))
# Sex   Female  Male     n
#   1 f          1     0   982
#   2 m          0     1   916


# Check that Sex columns are consistent
data_onefile %>%
  count(Sex, Female, Male, is.na(N_penisglands), is.na(ISI))


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . b3 Checking ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

head(data_onefile)
xtabs(~Year + Station, data_onefile)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . b4 Save  data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

saveRDS(data_onefile, file = "Data/02_Strandsnegl_intersex_2005_2018.RData")
openxlsx::write.xlsx(data_onefile, file = "Data/02_Strandsnegl_intersex_2005_2018.xlsx")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## 2. Kongsnegl, common whelk (Buccinum undatum) ----
# Type 1 file
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

folderno <- 2                          # Kongssnegl folder
dir(folder_data[folderno])
fn <- dir(folder_data[folderno])[1]    # File no. 1 (there is just one file)
fn_full <- paste0(folder_data[folderno], "/", fn)
sheets <- readxl::excel_sheets(fn_full)
sheets <- sheets[!sheets %in% "Sheet2"]  # empty sheet removed from list
sheets

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## . a Read using function ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Test
# res <- read_intersex_type1(fn_full, 1, headerline = 6)
# res$data

# debugonce(read_intersex_type2)
data_list_onefile <- sheets %>% purrr::map(~read_intersex_type1(fn_full, ., headerline = 6))
names(data_list_onefile) <- sheets

# Check 
transpose(data_list_onefile)$data %>% compare_df_cols(return = "mismatch")  # should be zero
# OR check this:
# transpose(data_list_onefile)$data %>% compare_df_cols()

# Combine data
data_onefile <- transpose(data_list_onefile)$data %>% bind_rows()


#
# Checks
#

head(data_onefile)
xtabs(~Year + Station, data_onefile)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . b Save data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

saveRDS(data_onefile, file = "Data/02_Kongsnegl_intersex_2013_2014.RData")
openxlsx::write.xlsx(data_onefile, file = "Data/02_Kongsnegl_intersex_2013_2014.xlsx")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## 3. Nettsnegl, netted dog whelk (Nassarius reticulatus) ----
# AKA Buccinum reticulatum (Linnaeus) and Hinia reticulata
# Type 1 file
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

folderno <- 3                          # Kongssnegl folder
dir(folder_data[folderno])
fn <- dir(folder_data[folderno])[1]    # File no. 1 (there is just one file)
fn_full <- paste0(folder_data[folderno], "/", fn)
sheets <- readxl::excel_sheets(fn_full)
sheets <- sheets[!sheets %in% "Sheet2"]  # empty sheet removed from list
sheets

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## . a Read using function ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Test
# res <- read_intersex_type1(fn_full, 1, headerline = 6)
# res$data

# debugonce(read_intersex_type2)
data_list_onefile <- sheets %>% purrr::map(~read_intersex_type1(fn_full, ., headerline = 6))
names(data_list_onefile) <- sheets

# Check 
transpose(data_list_onefile)$data %>% compare_df_cols(return = "mismatch")  # should be zero
# OR check this:
# transpose(data_list_onefile)$data %>% compare_df_cols()

# Combine data
data_onefile <- transpose(data_list_onefile)$data %>% bind_rows()


#
# Checks
#

head(data_onefile)
xtabs(~Year + Station, data_onefile)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . b Save data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

saveRDS(data_onefile, file = "Data/02_Nettsnegl_intersex_2007_2014.RData")
openxlsx::write.xlsx(data_onefile, file = "Data/02_Nettsnegl_intersex_2007_2014.xlsx")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## 4. Purpursnegl - dog whelk (Nucella lapillus) ----
# Type 1 file
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

folderno <- 4                          # folder
dir(folder_data[folderno])
fn <- dir(folder_data[folderno])[1]    # File no. 1 (there is just one file)
fn_full <- paste0(folder_data[folderno], "/", fn)
sheets <- readxl::excel_sheets(fn_full)
sheets <- sheets[!sheets %in% "Sheet2"]  # empty sheet removed from list
sheets
# "19G"                "VIKKILEN2005_HÅØYA" "2011_nucla" 

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## . a Read using function ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Test
# res <- read_intersex_type1(fn_full, 1, headerline = 6)
# res$data

# debugonce(read_intersex_type2)
data_list_onefile <- sheets %>% 
  purrr::map(~read_intersex_type1(fn_full, ., headerline = 6, 
                                  stations_from_sheetnames = FALSE))  # sheetnames is not on the form station_year 
names(data_list_onefile) <- sheets

# Check metadata
transpose(data_list_onefile)$metadata

# Manual stations/years
stations <- c("19G Harstad-Trondenes", "St. 1 Rivingen-Grimstad", "St. 1 Håøya")
years <- c(2006, 2005, 2011)
for (i in 1:3){ 
  data_list_onefile[[i]]$data$Station <- stations[i]
  data_list_onefile[[i]]$data$Year <- years[i]
}

# Check 
transpose(data_list_onefile)$data %>% compare_df_cols(return = "mismatch")  # should be zero
# OR check this:
# transpose(data_list_onefile)$data %>% compare_df_cols()

# Combine data
data_onefile <- transpose(data_list_onefile)$data %>% bind_rows()


#
# Checks
#

head(data_onefile)
xtabs(~Year + Station, data_onefile)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . b Save data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

saveRDS(data_onefile, file = "Data/02_Purpursnegl_intersex_2007_2014.RData")
openxlsx::write.xlsx(data_onefile, file = "Data/02_Purpursnegl_intersex_2007_2014.xlsx")

