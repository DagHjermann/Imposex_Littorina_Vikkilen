
read_intersex_type1 <- function(filename, sheetname, headerline,
                                stations_from_sheetnames = TRUE){
  
  cat("Reading", filename, "\n")
  cat("Sheet", sheetname, "\n")
  
  metadata <- read_excel(filename, sheet = sheetname, col_names = FALSE, range = "A1:A3") %>%
    as.data.frame()
  colnames(metadata)[1] <- "Data"
  
  # The next line ('columntypes') was constructed from the output of 
  # example_of_dataframe %>% map_chr(class) %>% dput()
  # in this case, specifically:
  # data_list_onefile[[1]]$data %>% map_chr(class) %>% dput()
  columntypes <- c(Nr = "numeric", Code = "text", 
                   SH = "numeric", SB = "numeric", yr = "numeric", VS = "text", 
                   Pr = "text", Vd = "text", Pe_gl = "numeric", P_P = "numeric", 
                   Gen_excr = "numeric", Other_excr = "numeric", O = "text", 
                   Od = "text", Ag = "text", Ig = "text", Cg = "text", 
                   Vag = "text", in.Od = "text", stage = "numeric", type = "text", 
                   male = "numeric", Kommentar = "text")

    # Initial read of all data
  range <- sprintf("A%i:W250", headerline)                            # hard-coded
  dat_init <- read_excel(filename, sheet = sheetname, range = range, col_types = columntypes)
  colnames(dat_init)[1] <- "Nr"
  # Pe	P	Gen.	other
  # gl	P	excr.	excr.
  colnames_original <- colnames(dat_init)
  cat("Headers:", paste(colnames_original, collapse = ", "), "\n")
  colnames(dat_init)[9:12]
  colnames(dat_init)[9:12] <- c("Pe_gl", "P_P", "Gen_excr", "Other_excr")
  dat_init
  # Pick "cuts" in data
  sel <- substr(dat_init$Code, 1, 4) %in% "----"
  cut <- which(sel)
  cut
  # Read part 1 (males)
  range <- sprintf("A%i:W%i", headerline + cut[1] + 1, headerline + cut[2] - 1); range
  dat_part1 <- read_excel(filename, sheet = sheetname, col_names = FALSE, range = range, col_types = columntypes)
  colnames(dat_part1) <- colnames(dat_init)
  # Read part 2 (females)
  range <- sprintf("A%i:W%i", headerline + cut[2] + 1, headerline + cut[3] - 1); range
  dat_part2 <- read_excel(filename, sheet = sheetname, col_names = FALSE, range = range, col_types = columntypes)
  colnames(dat_part2) <- colnames(dat_init)
  
  # Sex for each part
  cat("Part 1:", dat_init[cut[1] + 0:1,] %>% pull(Kommentar), ";  ")
  cat("Part 2:", dat_init[cut[2] + 0:1,] %>% pull(Kommentar), "\n")

  # Extract station name and year from the sheet name (e.g. "ST6_05" = Station 6, 2005), unless stations_from_sheetnames = FALSE (purpursnegl)
  if (stations_from_sheetnames){
    sheetname_split <- strsplit(sheetname, split = "_")[[1]]
    station <- sheetname_split[1]
    year <- as.numeric(sheetname_split[2]) + 2000
  } else {
    station <- NA
    year <- NA
  }
    
    
  data <- bind_rows(
    data.frame(Sheet = sheetname, Station = station, Year = year, Sex = "M", dat_part1, stringsAsFactors = FALSE),   # NOTE: we assume that part 1 is always the males and part 2 is the females
    data.frame(Sheet = sheetname, Station = station, Year = year, Sex = "F", dat_part2, stringsAsFactors = FALSE)
  ) 
  
  list(data = as_tibble(data), colnames = colnames_original, metadata = metadata)
  
  }

# debugonce(read_imposex)
# fn_full <- "K:/Avdeling/Mar/Msc/Artikkel MSC Littorina/Data/Intersex og imposex/Strandsnegl-Littorina/Strandsnegl intersex 1997.xls"
# read_intersex_type1(fn_full, 1, headerline = 6)


read_intersex_type2 <- function(filename, sheetname, headerline){
  
  # The next line ('columntypes') was constructed from the output of 
  # example_of_dataframe %>% map_chr(class) %>% dput()
  # in this case, specifically:
  # data_list_onefile[[1]]$data %>% map_chr(class) %>% dput()
  columntypes <- c(Nr = "numeric", Shellheight = "numeric", 
    Sex = "text", F = "numeric", M = "numeric", Mature = "text", 
    ISI = "numeric", PRL = "numeric", Trematodes = "numeric", N_penisglands = "numeric", 
    Comments = "text")
  
  # Initial read of all data
  range <- sprintf("A%i:K150", headerline)
  #dat_init <- read_excel(fn_full, sheet = sheetname, range = range, guess_max = 15)
  dat_init <- read_excel(fn_full, sheet = sheetname, range = range, col_types = columntypes)
  
  # Keep original col names
  colnames_original <- colnames(dat_init)
  
  # Set 
  colnames(dat_init) <- c("Nr","Shellheight","Sex","F","M","Mature", "ISI", "PRL", "Trematodes", "N_penisglands", "Comments")

  # Select rows
  sel <- substr(dat_init$Sex, 1, 1) %in% c("F","M") | dat_init$F == 1 | dat_init$M == 1
  pick_rows <- sel & !is.na(sel)
  
  # Extract station name and year from the sheet name (e.g. "ST6_05" = Station 6, 2005)
  sheetname_split <- strsplit(sheetname, split = "_")[[1]]
  station <- sheetname_split[1]
  year <- as.numeric(sheetname_split[2]) + 2000
  
  # Data
  data <- data.frame(Sheet = sheetname, Station = station, Year = year, dat_init[pick_rows,], stringsAsFactors = FALSE)
  
  list(data = as_tibble(data), colnames = colnames_original)
  
}

# debugonce(read_imposex)
# fn_full <- "K:/Avdeling/Mar/Msc/Artikkel MSC Littorina/Data/Intersex og imposex/Strandsnegl-Littorina/Strandsnegl intersex 2005 til 2018_lis.xls"
# read_intersex_type2(fn_full, 1, headerline = 3)
