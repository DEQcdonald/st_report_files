library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(htmltools)

report_areas <- sf::st_read(
  dsn = '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS',
  layer = 'Report_Units_HUC08',
  stringsAsFactors = FALSE) 

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

pal <- colorFactor(palette = "Paired",
            domain = unique(report_areas$MAP))

report_areas <- report_areas %>% mutate(map_link = paste0("<a href='", report_name_abr[REPORT], "/", map_name_abr[MAP], 
                                                          "_map.html' style='width:600px' target='_blank'>", MAP, "</a>"),
                                        color = pal(MAP))

map_locator <- leaflet(report_areas) %>% addTiles() %>% 
  addPolygons(group = "Subbasin Polygons",
              label = ~lapply(paste0("<b>Basin:</b> ", REPORT,
                                     "<br><b>Subbasin:</b> ", HU_8_NAME,
                                     "<br><b>Map:</b> ", MAP,
                                     "<br><b>Click for link to map"), htmltools::HTML),
              popup = ~paste0("Status and Trends Map: ", map_link),
              color = "black", weight = 2, opacity = 1, 
              fillColor = ~color, fillOpacity = 0.6) %>% 
  addLayersControl(overlayGroups = c("Subbasin Polygons")) %>%
  addControl(position = "bottomleft", className = "info", 
             html = "Hover over the map to determine the name of the basin, subbasin, and associated status and trends map.<br> Click on an area of interest to obtain a link to the relevant status and trends map.") %>% 
  leaflet.extras::addSearchFeatures(targetGroups = "Subbasin Polygons",
                                    options = searchFeaturesOptions(openPopup = TRUE, textPlaceholder = "Search basins..."))

htmlwidgets::saveWidget(map_locator, paste0("//deqhq1/WQNPS/Status_and_Trend_Reports/2019/web_wqst_2019/2019_map_locator.html"), 
                        title = paste("Oregon Status and Trends Map Locator"), 
                        background = "grey", selfcontained = FALSE)
