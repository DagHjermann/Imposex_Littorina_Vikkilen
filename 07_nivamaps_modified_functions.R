
#
# Changes to add_polyline
# - added crs as argument 
#

add_polyline <- function (map, data, col_lat = 1, col_long = 2, border = TRUE, 
          border_color = "black", border_thickness = 3, border_opacity = 1, 
          fill = FALSE, fill_color = "#e41a1c", fill_opacity = 1, crs = 32633){
  data2 <- as.matrix(data[, c(col_long, col_lat)])
  border <- ifelse(border, "true", "false")
  fill <- ifelse(fill, "true", "false")
  polyline_txt <- "\nvar marker = L.polyline(%s, {\n        stroke: %s,\n        color: '%s',\n        weight: %1.0f,\n        opacity: %.3f,\n        fill: %s,\n        fillColor: '%s',\n        fillOpacity: %.3f\n        }).addTo(map);\n"
  data_tr <- longlat_to_leaflet_multipoint(data2, crs = crs)
  new_part <- sprintf(polyline_txt, nivamaps:::reformat_data(data = data_tr, 
                                                  col1 = 2, col2 = 1), border, border_color, border_thickness, 
                      border_opacity, fill, fill_color, fill_opacity)
  N <- length(map)
  c(map[1:(N - 1)], new_part, map[N])
}

# added crs as argument
add_points <- function (map, data, col_lat = 1, col_long = 2, radius = 3, fill = TRUE, 
          color = "#e41a1c", opacity = 1, border = FALSE, border_color = "black", 
          border_thickness = 1, border_opacity = 1, crs = 32633) 
{
  data <- as.matrix(data)
  border <- ifelse(border, "true", "false")
  fill <- ifelse(fill, "true", "false")
  circleMarker_txt <- "    var marker = L.circleMarker([%.5f,%.5f], {\n      radius: %1.0f,\n      stroke: %s,\n      color: '%s',\n      weight: %1.0f,\n      opacity: %.3f,\n      fill: %s,\n      fillColor: '%s',\n      fillOpacity: %.3f\n      }).addTo(map);\n"
  if (is.null(dim(data))) 
    data <- matrix(data, ncol = 2, byrow = TRUE)
  data2 <- matrix(data[, c(col_long, col_lat)], ncol = 2)
  data_tr <- longlat_to_leaflet_multipoint(data2, crs = crs)
  new_part <- 1:nrow(data_tr) %>% purrr::map(~sprintf(circleMarker_txt, 
                                                      data_tr[., 2], data_tr[., 1], radius, border, border_color, 
                                                      border_thickness, border_opacity, fill, color, opacity)) %>% 
    paste(collapse = "")
  N <- length(map)
  c(map[1:(N - 1)], new_part, map[N])
}
  

  