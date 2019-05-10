
## Libraries ----
library(dplyr)
library(purrr)
library(ggplot2)
library(readxl)

source("02_Read_data_functions.R")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Where the data is ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

folder_data_upper <- "K:/Avdeling/Mar/Msc/Artikkel MSC Littorina/Data/Intersex og imposex"
dir(folder_data_upper)

folder_data <- paste0(folder_data_upper, "/", 
                      c("Strandsnegl-Littorina", "Kongsnegl-Buccinum", "Nettsnegl-Nassarius-Hinia", "Purpursnegl - Nucella")
)

dir(folder_data[1])

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Type 1 file ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

fn <- "Strandsnegl intersex 1997.xls"        # File of type 1
fn_full <- paste0(folder_data[1], "/", fn)
sheets <- readxl::excel_sheets(fn_full)
sheets

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Read data using function ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

data_list_onefile <- sheets %>% purrr::map(~read_intersex_type1(fn_full, ., headerline = 6))
names(data_list_onefile) <- sheets
# Check if part 1 always is HANNER and part 2 always is HUNNER

# check column names - must be alike
transpose(data_list_onefile)$colnames

# check column names - more stringent - all should be 0
check <- transpose(data_list_onefile)$colnames %>% bind_rows()
for (i in 1:ncol(check)){
  cat(i, sum(check[,i] != check[,1]), "\n")
}

# Combine data
data_onefile <- transpose(data_list_onefile)$data %>% bind_rows()


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Type 2 file ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

fn <- "Strandsnegl intersex 2005 til 2018_lis.xls" # type 2
fn_full <- paste0(folder_data[1], "/", fn)
sheets <- readxl::excel_sheets(fn_full)
sheets

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Read data using function ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

data_list_onefile <- sheets %>% purrr::map(~read_intersex_type2(fn_full, ., headerline = 3))
names(data_list_onefile) <- sheets

# check column names - must be alike
transpose(data_list_onefile)$colnames

# check column names - more stringent - all should be 0
check <- transpose(data_list_onefile)$colnames %>% bind_rows()
for (i in 1:ncol(check)){
  cat(i, sum(check[,i] != check[,1]), "\n")
}

# Combine data
data_onefile <- transpose(data_list_onefile)$data %>% bind_rows()



