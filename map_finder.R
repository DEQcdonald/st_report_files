library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(htmltools)
library(htmlwidgets)

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
              popup = ~lapply(paste0("<b>Status and Trends Map:</b><br>", map_link,
                                     "<br><b>Basin:</b> ", basin,
                                     "<br><b>Subbasins:</b><br>", subbasins), 
                              htmltools::HTML),
              # popup = ~paste0("", map_link), 
              stroke = FALSE, fill = TRUE, fillOpacity = 0.5, fillColor = ~color,
              highlightOptions = highlightOptions(fillColor = "black", fillOpacity = 0.75)) %>% 
  addPolygons(group = "Subbasin Polygons",
              color = "black", weight = 2, opacity = 1, fill = FALSE) %>% 
  addPolygons(data = basin_shp, group = "Basin Polygons",
              color = "black", weight = 4, opacity = 1, fill = FALSE) %>%  
  addLayersControl(overlayGroups = c("Subbasin Polygons", "Basin Polygons", "Map Polygons"),
                   options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
                   ) %>%
  addControl(position = "bottomright", className = "logo",
             html = sprintf('<html><body><div style="opacity:1">
                                        <a href="https://www.oregon.gov/deq/wq/programs/Pages/wqstatustrends.aspx">
                                        <img width="50" src="data:image/png;base64,%s">
                            </a></div></body></html>', logo)) %>% 
  # addControl(position = "bottomleft", className = "info", 
  #            html = "Hover over the map to determine the name of the basin, subbasin,<br>and relevant status and trends map. Click on an area of interest<br>to obtain a link to its associated status and trends map.") %>% 
  leaflet.extras::addSearchFeatures(targetGroups = "Map Polygons",
                                    options = searchFeaturesOptions(propertyName = "popup", openPopup = TRUE, 
                                                                    textPlaceholder = "Find a basin, subbasin, or map name", zoom = 8)) %>%
  leaflet.extras::addSearchOSM(options = searchOptions(textPlaceholder = "Find a location", zoom = 9)) %>% 
  htmlwidgets::onRender(jsCode = "function(el, x){
  var info = document.getElementsByClassName('info');
  for (var i = 0; i < info.length; i++) {
  info[i].style.marginBottom = '20px';
  info[i].style.borderRadius = '0px';
  }
  var logo = document.getElementsByClassName('logo');
  for (var i = 0; i < logo.length; i++) {
  logo[i].style.marginBottom = '0px';
  logo[i].style.marginRight = '5px';
                        }
                        }") %>% 
  htmlwidgets::appendContent(tags$head(tags$meta(name="viewport", content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")))

htmlwidgets::saveWidget(map_locator, paste0("//deqhq1/WQNPS/Status_and_Trend_Reports/2019/wqst_2019/map_locator.html"), 
                        title = paste("Oregon Status and Trends Map Locator"), 
                        background = "grey", selfcontained = FALSE)
