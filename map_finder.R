library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(htmltools)

report_areas <- sf::st_read(
  dsn = '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS',
  layer = 'Report_Units_HUC08',
  stringsAsFactors = FALSE) 

logo <- base64enc::base64encode("//deqhq1/WQNPS/Status_and_Trend_Reports/Figures/DEQ-logo-color-non-transp71x107.png")

report_areas <- st_transform(report_areas, 4326)

report_name_abr <- list("Black Rock Desert-Humboldt"="blackrock",
                        "Columbia River"="columbiariv",
                        "Deschutes"="deschutes",
                        "Goose Lake"="gooselake",
                        "Grande Ronde"="granderonde",
                        "John Day"="johnday",
                        "Klamath"="klamath",
                        "Malheur"="malheur",
                        "Mid-Coast"="midcoast",
                        "Middle Columbia-Hood"="midcohood",
                        "North Coast-Lower Columbia"="ncoast",
                        "Oregon Closed Basins"="orclosed",
                        "Owyhee"="owyhee",
                        "Powder-Burnt"="powderburnt",
                        "Rogue"="rogue",
                        "Sandy"="sandy",
                        "Snake River"="snakeriv",
                        "South Coast"="scoast",
                        "Umatilla-Walla Walla-Willow"="umatilla",
                        "Umpqua"="umpqua",
                        "Willamette"="willamette")

map_name_abr <- list("Black Rock Desert-Humboldt"="blackrock",
                     "Columbia River"="columbiariv",
                     "Deschutes"="deschutes",
                     "Goose Lake"="gooselake",
                     "Grande Ronde"="granderonde",
                     "John Day"="johnday",
                     "Klamath"="klamath",
                     "Malheur"="malheur",
                     "Mid-Coast"="midcoast",
                     "Middle Columbia-Hood"="midcohood",
                     "North Coast-Lower Columbia"="ncoast",
                     "Oregon Closed Basins"="orclosed",
                     "Owyhee"="owyhee",
                     "Powder-Burnt"="powderburnt",
                     "Rogue"="rogue",
                     "Sandy"="sandy",
                     "Snake River"="snakeriv",
                     "South Coast"="scoast",
                     "Umatilla-Walla Walla-Willow"="umatilla",
                     "Umpqua"="umpqua",
                     "Willamette-Clackamas"="will_clack",
                     "Willamette-Lower"="will_lower",
                     "Willamette-McKenzie-Forks"="will_mck_forks",
                     "Willamette-Middle"="will_middle",
                     "Willamette-Santiam-Upper"="will_san_upper",
                     "Willamette-Tualatin"="will_tualatin")

basin_shp <- report_areas %>% group_by(REPORT) %>% summarise(basin = unique(REPORT))
map_shp <- report_areas %>% group_by(MAP) %>% summarise(basin = unique(REPORT),
                                                        subbasins = paste(unique(HU_8_NAME), collapse = "<br>"))

pal <- colorFactor(palette = "Paired",
                   domain = unique(map_shp$MAP))

map_shp <- map_shp %>% mutate(map_link = paste0("<a href='", report_name_abr[basin], "/", map_name_abr[MAP], 
                                                          "_map.html' style='width:600px' target='_blank'>", MAP, "</a>"),
                                        color = pal(MAP))

report_areas <- report_areas %>% mutate(map_link = paste0("<a href='", report_name_abr[REPORT], "/", map_name_abr[MAP], 
                                                          "_map.html' style='width:600px' target='_blank'>", MAP, "</a>"),
                                        color = pal(MAP))

map_locator <- leaflet(report_areas) %>% addTiles() %>% 
  addPolygons(data = map_shp, group = "Map Polygons",
              label = ~lapply(paste0("<b>Map:</b> ", MAP,
                                     "<br><b>Basin:</b> ", basin,
                                     "<br><b>Subbasins:</b><br>", subbasins,
                                     "<br><b>Click area for link to map"), htmltools::HTML),
              popup = ~paste0("Status and Trends Map: ", map_link), 
              stroke = FALSE, fill = TRUE, fillOpacity = 0.5, fillColor = ~color,
              highlightOptions = highlightOptions(fillColor = "black", fillOpacity = 0.75)) %>% 
  addPolygons(group = "Subbasin Polygons",
              color = "black", weight = 2, opacity = 1, fill = FALSE) %>% 
  addPolygons(data = basin_shp, group = "Basin Polygons",
              color = "black", weight = 4, opacity = 1, fill = FALSE) %>%  
  addLayersControl(overlayGroups = c("Subbasin Polygons", "Basin Polygons", "Map Polygons")) %>%
  addControl(position = "bottomright", className = "logo",
             html = sprintf('<html><body><div style="opacity:1">
                                        <img width="60" src="data:image/png;base64,%s">
                            </div></body></html>', logo)) %>% 
  addControl(position = "bottomleft", className = "info", 
             html = "Hover over the map to determine the name of the basin, subbasin, and relevant status and trends map.<br> Click on an area of interest to obtain a link to its associated status and trends map.") %>% 
  leaflet.extras::addSearchFeatures(targetGroups = "Map Polygons",
                                    options = searchFeaturesOptions(openPopup = TRUE, textPlaceholder = "Search basins..."))

htmlwidgets::saveWidget(map_locator, paste0("//deqhq1/WQNPS/Status_and_Trend_Reports/2019/web_wqst_2019/2019_map_locator.html"), 
                        title = paste("Oregon Status and Trends Map Locator"), 
                        background = "grey", selfcontained = FALSE)
