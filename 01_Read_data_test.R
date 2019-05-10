
## Libraries ----
library(dplyr)
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

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Type 1: Pick file ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

dir(folder_data[1])

fn <- "Strandsnegl intersex 1997.xls"
fn_full <- paste0(folder_data[1], "/", fn)
sheets <- readxl::excel_sheets(fn_full)
sheets

# Sheet
i <- 1

# Metadata
info <- read_excel(fn_full, sheet = i, col_names = FALSE, range = "A1:A3")

#head <- read_excel(fn_full, sheet = i, skip = 5, n_max = 1) 
# dat <- read_excel(fn_full, sheet = i, skip = 7)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Type 1: Read data of this file ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Initial read of all data
headerline <- 6
range <- sprintf("A%i:Y150", headerline)
dat_init <- read_excel(fn_full, sheet = i, range = range)
colnames(dat_init)[1] <- "Nr"
# Pe	P	Gen.	other
# gl	P	excr.	excr.
colnames_original <- colnames(dat_init)
colnames(dat_init)[9:12]
colnames(dat_init)[9:12] <- c("Pe_gl", "P_P", "Gen_excr", "Other_excr")
dat_init
# Pick "cuts" in data
sel <- substr(dat_init$Nr, 1, 4) %in% "----"
cut <- which(sel)
cut
# Read part 1 (males)
range <- sprintf("A%i:Y%i", headerline + cut[1] + 1, headerline + cut[2] - 1); range
dat_part1 <- read_excel(fn_full, sheet = i, col_names = FALSE, range = range)
colnames(dat_part1) <- colnames(dat_init)
# Read part 2 (females)
range <- sprintf("A%i:Y%i", headerline + cut[2] + 1, headerline + cut[3] - 1); range
dat_part2 <- read_excel(fn_full, sheet = i, col_names = FALSE, range = range)
colnames(dat_part2) <- colnames(dat_init)

# Check
tail(dat_part1)
tail(dat_part2)

# Sex for each part
dat_init[cut[1],] %>% pull(kommentar)
dat_init[cut[2],] %>% pull(kommentar)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Type 1: test function ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

res <- read_intersex_type1(fn_full, 1, headerline = 6)
res$data

# debugonce(read_intersex_type1)
res <- read_intersex_type1(fn_full, 5, headerline = 6)

res$data



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Type 2: Pick file ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

fn <- "Strandsnegl intersex 2005 til 2018_lis.xls"
fn_full <- paste0(folder_data[1], "/", fn)
sheets <- readxl::excel_sheets(fn_full)
sheets

# Sheet
i <- 1

# Metadata
info <- read_excel(fn_full, sheet = i, col_names = FALSE, range = "A1:A3")

#head <- read_excel(fn_full, sheet = i, skip = 5, n_max = 1) 
# dat <- read_excel(fn_full, sheet = i, skip = 7)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Type 2: Read data of this file ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Initial read of all data
headerline <- 3
range <- sprintf("A%i:K150", headerline)
dat_init <- read_excel(fn_full, sheet = i, range = range, guess_max = 15)
colnames(dat_init) <- c("Nr","Shellheight","Sex","F","M","Mature", "ISI", "PRL", "Trematodes", "N_penisglands", "Comments")
sel <- substr(dat_init$Sex, 1, 1) %in% c("F","M") | dat_init$F == 1 | dat_init$M == 1
pick_rows <- sel & !is.na(sel)
data <- dat_init[pick_rows,]

# Pick "cuts" in data
sel <- substr(dat_init$Nr, 1, 4) %in% "----"
cut <- which(sel)
cut
# Read part 1 (males)
range <- sprintf("A%i:Y%i", headerline + cut[1] + 1, headerline + cut[2] - 1); range
dat_part1 <- read_excel(fn_full, sheet = i, col_names = FALSE, range = range)
colnames(dat_part1) <- colnames(dat_init)
# Read part 2 (females)
range <- sprintf("A%i:Y%i", headerline + cut[2] + 1, headerline + cut[3] - 1); range
dat_part2 <- read_excel(fn_full, sheet = i, col_names = FALSE, range = range)
colnames(dat_part2) <- colnames(dat_init)

# Check
tail(dat_part1)
tail(dat_part2)

# Sex for each part
dat_init[cut[1],] %>% pull(kommentar)
dat_init[cut[2],] %>% pull(kommentar)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Type 1: test function ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

res <- read_intersex_type2(fn_full, 1, headerline = 3)
res

# debugonce(read_intersex_type2)
res <- read_intersex_type2(fn_full, 5, headerline = 6)
