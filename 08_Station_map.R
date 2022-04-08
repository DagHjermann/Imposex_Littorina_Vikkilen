
library(leaflet)
library(dplyr)

df1 <- read.table(textConnection("
Station Lon Lat
7 8.612219	58.36288
6 8.610732	58.35824
5 8.60455	58.35223
4 8.601422	58.34428
1 8.593184	58.30604
71G 9.80458	58.98496
"), header = TRUE)

leaf1 <- leaflet(df1) %>% 
  addTiles() %>% 
  addMarkers(lng = ~Lon, lat = ~Lat, label = ~Station) 

leaf1

df2 <- df1
sel <- df2$Station == "5" 
df2$Lon[sel] <- 8.61007
df2$Lat[sel] <- 58.35595
df2$Station[sel] <- "5 + 5b" 


leaf2 <- leaflet(df2) %>% 
  addTiles() %>% 
  addMarkers(lng = ~Lon, lat = ~Lat, label = ~Station) 

leaf2

htmlwidgets::saveWidget(leaf, file = "Temp.html")
file.copy(from = "Temp.html", to = "Data/08_Stasjoner_Vikkilen_paper.html")
file.remove("Temp.html")
