---
title: "02_Read_data_2021"
author: "DHJ"
date: "7 4 2022"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

Get 2021 data  

* Code from "C:\Data\seksjon 212\Milkys\994_Industry_data_2021.Rmd"  


## 1. Preparations  

### Packages + functions  


### Stations from O-number  


```
##   PROJECT_ID O_NUMBER                         PROJECT_NAME
## 1       9246    15404                         Viktilt 2022
## 2      12578    15404 Miljøovervåking i Vikkilen 2015-2022
## 3      12386    15404        Overvåking av Vikkilen i 2018
##                             PROJECT_DESCRIPTION
## 1                Overvåking i Vikkilen for Nymo
## 2 Sedimenter, blåskjell, strandsnegl, bunnfauna
## 3                        Blåskjell, Strandsnegl
## # A tibble: 25 x 4
##    STATION_ID PROJECT_ID STATION_CODE STATION_NAME
##         <dbl> <chr>      <chr>        <chr>       
##  1      67469 9246,12578 A1           Vikkilen    
##  2      67470 9246,12578 A2           Vikkilen    
##  3      67471 9246       A3           Vikkilen    
##  4      67472 9246       A4           Vikkilen    
##  5      67473 9246       A5           Vikkilen    
##  6      67986 9246,12578 B1           Vikkilen    
##  7      52005 12578      B16          B16         
##  8      67475 9246       B16          Vikkilen    
##  9      67474 9246,12578 B5           Vikkilen    
## 10      67476 9246,12578 BL1          Stanghlm    
## # ... with 15 more rows
```


## 2. Biota data  

### Biota (specimens)     
* Data for 2016, 2020 and 2021            

```
## 
## 2016 2018 2021 
##   10    8    9 
##              Year
## STATION_CODE  2016 2018 2021
##   BL1            1    0    1
##   BL3            0    1    1
##   BL4            1    1    1
##   BL5            1    1    1
##   BL6            1    1    1
##   Lit1           1    1    1
##   Lit4           1    1    1
##   Lit6           1    1    1
##   Lit7           1    1    1
##   Vikk_krabbe    1    0    0
##   Vikk_torsk     1    0    0
```


### Samples and chemistry values  

```
## Joining, by = "SPECIMEN_ID"
```

```
## 101 chemical methods downloaded
## [1] 0
## [1] 0
##              MYEAR
## STATION_CODE  2016 2018 2021
##   BL1           34    0   43
##   BL3            0   63   43
##   BL4           34   64   43
##   BL5           34   64   43
##   BL6           34   64   43
##   Lit1           2   17   16
##   Lit4           2   17   16
##   Lit6           2   17   16
##   Lit7           2   17   16
##   Vikk_krabbe    3    0    0
##   Vikk_torsk     6    0    0
```


## 3. Sediment data     

* 2016 + 2021  


```
## Joining, by = "METHOD_ID"
## Joining, by = "SLICE_ID"
## Joining, by = "SAMPLE_ID"
## Joining, by = "STATION_ID"
```

```
## [1] 23
## 
## 2016 2021 
##   18    5 
## [1] 38
## [1] 787
## [1] 787
##             year(SAMPLE_DATE)
## STATION_CODE 2016 2021
##          A1    76   51
##          A2    76   51
##          A3    72    0
##          A4    73    0
##          A5    73    0
##          B1    42   52
##          B16   76   51
##          B5    42   52
```


## 4. Save  

```
## Saved file Data/21_data_2016-2021_biota.csv 
## Saved file Data/21_data_2016-2021_sediment.csv
```

