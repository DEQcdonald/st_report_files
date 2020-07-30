library(devtools)
library(rgdal)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(leafem)
library(dplyr)
library(htmlwidgets)
library(htmltools)

# devtools::install_github('donco/odeqstatusandtrends', host = 'https://api.github.com', force = TRUE, upgrade='never')
library(odeqstatusandtrends)

# Inputs ----

report_year <- '2020'

top_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/2020-Revision'
gis_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS'

# Web Maps  -------------------------------------------

HUC_shp <- rgdal::readOGR(dsn = gis_dir, layer = 'Report_Units_HUC08',
                          integer64="warn.loss", verbose = FALSE, stringsAsFactors = FALSE)

report_names <- sort(unique(HUC_shp$REPORT))

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
#name <- "Willamette"
#name <- "Owyhee"
#name <- "Umatilla-Walla Walla-Willow"

for (name in report_names){
  
  name_abr <- report_name_abr[[name]]

  print(paste0("Creating parameter summary map for the ", name, " Basin..."))
  
  data_dir <- paste0(top_dir,'/', report_year,'-', name)
  
  load(file = paste0(data_dir, "/", name, "_eval_date.RData"))
  
  if(dir.exists(paste0(top_dir,'/wqst_map'))) {
  } else {dir.create(paste0(top_dir,'/wqst_map'))}
  output_dir <- paste0(top_dir,'/wqst_map/', name_abr)

  if(dir.exists(output_dir)) {
  } else {dir.create(output_dir)}
  
  load(paste0(data_dir, "/", name, "_param_summary_by_station.RData"))
  load(paste0(data_dir, "/", name, "_param_summary_by_AU.RData"))
  
  report_shp <- HUC_shp[HUC_shp$REPORT %in% name, ]
  
  map_names <- sort(unique(report_shp$MAP))
  
  #m <- "Willamette-Clackamas"
  
  for(m in map_names){
    
    m_abr <- map_name_abr[[m]]
    
    print(paste0(m, "..."))
    
    map <- odeqstatusandtrends::parameter_summary_map(param_summary = param_sum_stn, 
                                                      au_param_summary = param_sum_au, 
                                                      area = report_shp[report_shp$MAP == m,], 
                                                      proj_dir = output_dir)
    
    htmlwidgets::saveWidget(map, paste0(output_dir, "/", m_abr, "_map.html"),
                            title = paste(m, "Status and Trends Map"),
                            background = "grey", selfcontained = FALSE)
  }
}
