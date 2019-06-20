
# "Strandsnegl intersex 1997.xls"                # File of type 1
# "Strandsnegl intersex 2005 til 2018_lis.xls"

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

folder_data_upper <- "K:/Avdeling/Mar/Msc/Artikkel MSC Littorina/Data/Intersex og imposex"
# folder_data_upper <- "Input_data"
dir(folder_data_upper)
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


#
# Checks
#

head(data_onefile)
xtabs(~Year + Station, data_onefile)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . b2 Save strandsnegl data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

saveRDS(data_onefile, file = "Data/Strandsnegl_intersex_2005_2018.RData")
openxlsx::write.xlsx(data_onefile, file = "Data/Strandsnegl_intersex_2005_2018.xlsx")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## 2. Kongsnegl, common whelk (Buccinum undatum) ----
# Type 2 file
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
# . b Save common whelk data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

saveRDS(data_onefile, file = "Data/Strandsnegl_intersex_2005_2018.RData")
openxlsx::write.xlsx(data_onefile, file = "Data/Strandsnegl_intersex_2005_2018.xlsx")

# Purpursnegl - dog whelk (Nucella lapillus)
# Nettsnegl - netted dog whelk (Nassarius reticulatus)


