library(devtools)
library(rgdal)
#library(RODBC)
library(dplyr)
# devtools::install_github('donco/odeqstatusandtrends', host = 'https://api.github.com', upgrade='never')
library(odeqstatusandtrends)
# devtools::install_github('donco/odeqassessment', host = 'https://api.github.com', upgrade='never')
library(odeqassessment)
# devtools::install_github('rmichie/wqdb/wqdb', host = 'https://api.github.com', upgrade='never')
# library(wqdb)
# devtools::install_github('rmichie/owri/owri', host = 'https://api.github.com', upgrade='never')
library(owri)

# devtools::install_github('TravisPritchardODEQ/AWQMSdata', host = 'https://api.github.com', upgrade='never')
library(AWQMSdata)
library(dataRetrieval)
library(ggplot2)
library(lubridate)
library(pbapply)
library(tidyr)
library(htmltools)
library(captioner)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(leafem)
library(knitr)
library(mapview)
library(rmarkdown)
library(kableExtra)
library(htmlwidgets)
library(rmapshaper)
library(RODBC)
library(RSQLite)
library(htmltools)
library(Rcpp)
library(writexl)

# webshot::install_phantomjs()

start.date = "1999-01-01"
end.date = "2018-12-30"
complete.years <- c(as.integer(substr(start.date, start = 1, stop = 4)):as.integer(substr(end.date, start = 1, stop = 4)))

query_dates <- c(start.date, end.date)

gis_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS'
# gis_dir <- '//deqhq1/dwp-public/SpecialProjects/NRCS_NWQI'

# wq_db <- paste0(project_dir, name,"_data_raw_",paste0(query_dates, collapse = "."),".db")

# support_files_dir <- "//deqhq1/GISLIBRARY/Base_Data/Hydrography/Watershed_Boundaries/WBD_OR.gdb/WBD_OR.gdb/WBD_OR"

HUC_shp <- readOGR(dsn = gis_dir, layer = 'Report_Units_HUC08', integer64="warn.loss", verbose = FALSE, stringsAsFactors = FALSE)
# tmdls <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/TMDL map/TMDL_Feature_gdb/TMDLs.gdb", layer = "allocations_watershed") %>% st_transform(4326)
# basin_shp <- readOGR(dsn = gis_dir, layer = 'TualatinJWC_DWSA_dissolve', integer64="warn.loss", verbose = FALSE, stringsAsFactors = FALSE)
# HUC_shp <- HUC_shp[HUC_shp$REPORT != "Willamette",]

missing_AUs <- NULL
wqp_stns <- NULL
state_param_sum_au <- NULL
state_param_sum_stn <- NULL

basin_names <- sort(unique(HUC_shp$REPORT))

for (i in basin_names){
  
  name <- i
  print(paste0("Creating parameter summary table for the ", i, " Basin..."))
  
  if(dir.exists(paste0('//deqhq1/WQNPS/Status_and_Trend_Reports/2019/2019-', name, '/'))) {
  } else {dir.create(paste0('//deqhq1/WQNPS/Status_and_Trend_Reports/2019/2019-', name, '/'))}
  
  project_dir <- paste0('//deqhq1/WQNPS/Status_and_Trend_Reports/2019/2019-', name, '/')
  
  eval_date <- Sys.Date()
  save(eval_date, file = paste0(project_dir, name, "_eval_date.RData"))
  
  basin_shp <- HUC_shp[HUC_shp$REPORT %in% i, ]
  
  # print("Checking for TMDLs...")
  # basin_tmdls <- tmdls[sf::st_intersects(tmdls, sf::st_transform(sf::st_as_sf(basin_shp), 4326)) %>% lengths > 0,]
  # st_geometry(basin_tmdls) <- NULL
  # basin_tmdls <- basin_tmdls[grep("tss|phosphorus|tp|solids", basin_tmdls$NPS2, ignore.case = TRUE),]
  
  hucs <- unique(basin_shp$HUC_8)
  
  stations_AWQMS <- get_stations_AWQMS(basin_shp)
  missing_AUs <- bind_rows(missing_AUs, attr(stations_AWQMS, 'missing_AUs'))
  
  stations_wqp <- get_stations_WQP(polygon = basin_shp, start_date = start.date, end_date = end.date,
                                   huc8 = hucs, exclude.tribal.lands = TRUE)

  if(is.data.frame(stations_wqp) && nrow(stations_wqp) > 0){
    print("Add these stations to the Stations Database:")
    print(stations_wqp)
    wqp_stns <- bind_rows(wqp_stns, stations_wqp)
  } else {stations_wqp <- NULL}

  if(file.exists(paste0(project_dir, name, "_data_raw_", start.date, "-", end.date, ".RData"))){
    load(paste0(project_dir, name, "_data_raw_", start.date, "-", end.date, ".RData"))
  } else {
    data_raw <- GetData(parameters = c("Temperature", "Bacteria", "TSS", "DO", "TP", "pH"),
                        stations_AWQMS = stations_AWQMS,
                        stations_WQP = stations_wqp,
                        start.date = start.date,
                        end.date = end.date,
                        huc8 = hucs)
    
    print(paste0("Saving raw data from query..."))
    
    save(data_raw, file = paste0(project_dir, name, "_data_raw_", start.date, "-", end.date, ".RData"))
  }
  
  data_raw[, c("StationDes", "HUC8", "HUC8_Name", "HUC10", "HUC12", "HUC12_Name",
               "Lat_DD", "Long_DD", "Reachcode", "Measure", "AU_ID", 141:149)] <-
    stations_AWQMS[match(data_raw$MLocID, stations_AWQMS$MLocID),
                   c("StationDes", "HUC8", "HUC8_Name", "HUC10", "HUC12", "HUC12_Name",
                     "Lat_DD", "Long_DD", "Reachcode", "Measure", "AU_ID", "FishCode", "SpawnCode", "WaterTypeCode", 
                     "WaterBodyCode", "BacteriaCode", "DO_code", "ben_use_code", "pH_code", "DO_SpawnCode")]
  data_raw$Datum <- stations_AWQMS[match(data_raw$MLocID, stations_AWQMS$MLocID),
                                   c("Datum")]
  data_raw$ELEV_Ft <- stations_AWQMS[match(data_raw$MLocID, stations_AWQMS$MLocID),
                                     c("ELEV_Ft")]
  
  au_names <- read.csv('//deqhq1/WQNPS/Status_and_Trend_Reports/Lookups_Statewide/AssessmentUnits_OR_Dissolve.txt')
  stations_AWQMS$AU_Name <- au_names[match(stations_AWQMS$AU_ID, au_names$AU_ID),
                                     c("AU_Name")]
  
  # Clean data and add criteria ---------------------------------------------
  
  data_clean <- CleanData(data_raw)
  data_clean <- add_criteria(data_clean)
  
  # Assess various parameters -----------------------------------------------
  
  data_assessed <- NULL
  status <- NULL
  excur_stats <- NULL
  trend <- NULL
  
  if(any(unique(data_clean$Char_Name) %in% AWQMS_Char_Names('pH'))){
    print("Assessing pH...")
    data_pH <- data_clean %>% filter(Char_Name == "pH")
    data_pH <- Censored_data(data_pH, criteria = 'pH_Min')
    data_pH <- odeqassessment::pH_assessment(data_pH)
    data_assessed <- bind_rows(data_assessed, data_pH)
    
    pH_status <- status_stns(data = data_pH, year_range = c(min(complete.years), max(complete.years)))
    status <- bind_rows(status, pH_status)
    
    pH_excur_stats <- odeqstatusandtrends::excursion_stats(data = data_pH, year_range = c(min(complete.years), max(complete.years)))
    excur_stats <- bind_rows(excur_stats, pH_excur_stats)
    
    pH_trend <- trend_stns(data_pH)
    trend <- bind_rows(trend, pH_trend)
  }
  
  if(any(unique(data_clean$Char_Name) %in% AWQMS_Char_Names('Temperature')) & 
     any(unique(data_clean[data_clean$Char_Name == "Temperature, water",]$Statistical_Base) %in% "7DADM")){
    print("Assessing temperature...")
    data_temp <- data_clean %>% filter(Char_Name == "Temperature, water", Statistical_Base == "7DADM")
    data_temp <- Censored_data(data_temp, criteria = "temp_crit")
    data_temp <- odeqassessment::temp_assessment(data_temp)
    data_assessed <- bind_rows(data_assessed, data_temp)
    
    temp_status <- status_stns(data_temp, year_range = c(min(complete.years), max(complete.years)))
    status <- bind_rows(status, temp_status)
    
    temp_excur_stats <- odeqstatusandtrends::excursion_stats(data = data_temp, year_range = c(min(complete.years), max(complete.years)))
    excur_stats <- bind_rows(excur_stats, temp_excur_stats)
    
    temp_trend <- trend_stns(data_temp)
    trend <- bind_rows(trend, temp_trend)
  }
  
  if(any(unique(data_clean$Char_Name) %in% AWQMS_Char_Names('TP'))){
    print("Assessing total phosphorus...")
    data_TP <- data_clean %>% filter(Char_Name == "Phosphate-phosphorus")
    data_TP <- Censored_data(data_TP, criteria = "TP_crit")
    data_TP <- TP_assessment(data_TP)
    data_assessed <- bind_rows(data_assessed, data_TP)
    
    TP_status <- status_stns(data = data_TP, year_range = c(min(complete.years), max(complete.years)))
    status <- bind_rows(status, TP_status)
    
    TP_excur_stats <- odeqstatusandtrends::excursion_stats(data = data_TP, year_range = c(min(complete.years), max(complete.years)))
    excur_stats <- bind_rows(excur_stats, TP_excur_stats)
    
    TP_trend <- trend_stns(data = data_TP)
    trend <- bind_rows(trend, TP_trend)
  }
      
  if(any(unique(data_clean$Char_Name) %in% AWQMS_Char_Names('TSS'))){
    print("Assessing total suspended solids...")
    data_TSS <- data_clean %>% filter(Char_Name == "Total suspended solids")
    data_TSS <- Censored_data(data_TSS, criteria = "TSS_crit")
    data_TSS <- TSS_assessment(data_TSS)
    data_assessed <- bind_rows(data_assessed, data_TSS)
    
    TSS_status <- status_stns(data = data_TSS, year_range = c(min(complete.years), max(complete.years)))
    status <- bind_rows(status, TSS_status)
    
    TSS_excur_stats <- odeqstatusandtrends::excursion_stats(data = data_TSS, year_range = c(min(complete.years), max(complete.years)))
    excur_stats <- bind_rows(excur_stats, TSS_excur_stats)
    
    TSS_trend <- trend_stns(data = data_TSS)
    trend <- bind_rows(trend, TSS_trend)
  }
  
  if(any(unique(data_clean$Char_Name) %in% AWQMS_Char_Names('bacteria'))){
    print("Assessing bacteria...")
    data_bact <- data_clean %>% filter(Char_Name %in% AWQMS_Char_Names('bacteria'))
    data_bact <- data_bact %>% mutate(bact_crit_min = pmin(bact_crit_ss, bact_crit_geomean, bact_crit_percent, na.rm = TRUE))
    data_bact <- Censored_data(data_bact, criteria = "bact_crit_min")
    data_ent <- Coastal_Contact_rec(data_bact)
    data_eco <- Fresh_Contact_rec(data_bact)
    data_shell <- Shell_Harvest(data_bact)
    data_bact <- bind_rows(data_ent, data_eco, data_shell)
    data_assessed <- bind_rows(data_assessed, data_bact)
    
    bact_status <- status_stns(data_bact, year_range = c(min(complete.years), max(complete.years)))
    status <- bind_rows(status, bact_status)
    
    bact_excur_stats <- odeqstatusandtrends::excursion_stats(data = data_bact, year_range = c(min(complete.years), max(complete.years)))
    excur_stats <- bind_rows(excur_stats, bact_excur_stats)
    
    bact_trend <- trend_stns(data_bact)
    trend <- bind_rows(trend, bact_trend)
  }
  
  if(any(unique(data_clean$Char_Name) %in% AWQMS_Char_Names('DO'))){
    print("Assessing dissolved oxygen...")
    data_DO <- data_clean %>% filter(Char_Name %in% c("Dissolved oxygen (DO)", "Dissolved oxygen saturation", "Temperature, water"))
    data_DO <- Censored_data(data_DO, criteria = "DO_crit_min")
    data_DO <- DO_assessment(data_DO)
    data_assessed <- bind_rows(data_assessed, data_DO)
    
    DO_status <- status_stns(data_DO, year_range = c(min(complete.years), max(complete.years)))
    status <- bind_rows(status, DO_status)
    
    DO_excur_stats <- odeqstatusandtrends::excursion_stats(data = data_DO, year_range = c(min(complete.years), max(complete.years)))
    excur_stats <- bind_rows(excur_stats, DO_excur_stats)
    
    DO_trend <- trend_stns(data_DO)
    trend <- bind_rows(trend, DO_trend)
  }
  
  print(paste0("Saving assessed data..."))
  
  save(data_assessed, file = paste0(project_dir, name, "_data_assessed.RData"))
  save(status, trend, excur_stats, file = paste0(project_dir, name, "_status_trend_excur_stats.RData"))
  
  # Assess status by parameter ----------------------------------------------
  
  # pH_status <- status_stns(data_pH, year_range = c(min(complete.years), max(complete.years)))
  # temp_status <- status_stns(data_temp, year_range = c(min(complete.years), max(complete.years)))
  # TP_status <- status_stns(data_TP, year_range = c(min(complete.years), max(complete.years)))
  # TSS_status <- status_stns(data_TSS, year_range = c(min(complete.years), max(complete.years)))
  # bact_status <- status_stns(data_bact, year_range = c(min(complete.years), max(complete.years)))
  # DO_status <- status_stns(data_DO, year_range = c(min(complete.years), max(complete.years)))
  # status <- bind_rows(pH_status, temp_status, TP_status, TSS_status, bact_status, DO_status)
  
  # pH_trend <- trend_stns(data_pH)
  # temp_trend <- trend_stns(data_temp)
  # TP_trend <- trend_stns(data_TP)
  # TSS_trend <- trend_stns(data_TSS)
  # bact_trend <- trend_stns(data_bact)
  # DO_trend <- trend_stns(data_DO)
  # trend <- bind_rows(pH_trend, temp_trend, TP_trend, TSS_trend, bact_trend, DO_trend)
  
  # save(status, trend, stations_AWQMS, file = paste0(project_dir, name, "_seaken_inputs.RData"))
  
  
  # Assess trends -----------------------------------------------------------
  
  print("Assessing trends via the Seasonal Kendall Analysis...")
  seaken_columns <- c("MLocID", "Char_Name", "sample_datetime", "Statistical_Base", "Result_cen", "tp_month", "tp_year")
  seaken_data <- merge(data_assessed[,colnames(data_assessed)[colnames(data_assessed) %in% seaken_columns]], 
                       trend[,c("MLocID", "Char_Name")], by = c("MLocID", "Char_Name"), all = FALSE)
  
  if(nrow(seaken_data) > 0){
    seaKen <- sea_ken(data = seaken_data)
    seaKen_sample_size <- attributes(seaKen)$sample_size
    
    save(seaKen, file = paste0(project_dir, name, "_seaken.RData"))
  } else {
    print("There were no stations with data that qualified for trend analysis...")
    seaKen <- NULL
  }
  
  # OWRI summary -------------------------------------------------------------
  
  print(paste0("Creating OWRI summary for the ", i, " Basin..."))
  
  owri.db <- "//deqhq1/WQNPS/Status_and_Trend_Reports/OWRI/OwriDbExport_122618.db"
  owri_summary <- tryCatch(owri_summary(owri.db = owri.db, complete.years = complete.years, huc8 = hucs),
                           error = function(e) {
                             print("Error: OWRI summary. Setting to NULL.")
                             data.frame(NoProjects="NULL")})
  
  # Create summary table ------------------------------------------------------
  
  print(paste0("Saving parameter summary tables..."))
  
  param_sum_stn <- parameter_summary_by_station(status, seaKen, stations_AWQMS)
  param_sum_au <- parameter_summary_by_au(status, seaKen, stations_AWQMS)
  
  state_param_sum_stn <- bind_rows(state_param_sum_stn, param_sum_stn)
  state_param_sum_au <- bind_rows(state_param_sum_au, param_sum_au)
  
  writexl::write_xlsx(list(summary_by_station=param_sum_stn,
                           summary_by_AU=param_sum_au,
                           excursion_stats=excur_stats,
                           trend_stats=seaKen,
                           owri_summary=owri_summary), path=paste0(project_dir, name,"_WQS&T_results_DRAFT_", eval_date, ".xlsx"))
  
  save(param_sum_stn, file = paste0(project_dir, name, "_param_summary_by_station.RData"))
  save(param_sum_au, file = paste0(project_dir, name, "_param_summary_by_AU.RData"))
  save(owri_summary, file = paste0(project_dir, name, "_owri_summary_by_subbasin.RData"))
}

writexl::write_xlsx(list(station_summary = state_param_sum_stn,
                         AU_summary = state_param_sum_au,
                         missing_AUs = missing_AUs,
                         wqp_stations = wqp_stns),
                         path=paste0("//deqhq1/WQNPS/Status_and_Trend_Reports/2019/", "Oregon", "_parameter_summary.xlsx"))
                         
# write.csv(state_param_sum_stn, paste0("//deqhq1/WQNPS/Status_and_Trend_Reports/2019/", "Oregon", "_param_summary_by_station.csv"), row.names = FALSE)
# write.csv(state_param_sum_au, paste0("//deqhq1/WQNPS/Status_and_Trend_Reports/2019/", "Oregon", "_param_summary_by_AU.csv"), row.names = FALSE)

# write.csv(missing_AUs, paste0("//deqhq1/WQNPS/Status_and_Trend_Reports/2019/", "Oregon", "_missing_AU.csv"), row.names = FALSE)
# write.csv(wqp_stns, paste0("//deqhq1/WQNPS/Status_and_Trend_Reports/2019/", "Oregon", "_wqp_stns.csv"), row.names = FALSE)

for (i in basin_names){
  name <- i
  print(paste0("Creating parameter summary map for the ", i, " Basin..."))
  
  project_dir <- paste0('//deqhq1/WQNPS/Status_and_Trend_Reports/2019/2019-', name, '/')
  
  load(file = paste0(project_dir, name, "_eval_date.RData"))
  
  draft_dir <- paste0(project_dir,'WQST_2019-',name,'_DRAFT_', eval_date, '/')
  
  if(dir.exists(draft_dir)) {
  } else {dir.create(draft_dir, recursive = TRUE)}
  
  load(paste0(project_dir, name, "_param_summary_by_station.RData"))
  load(paste0(project_dir, name, "_param_summary_by_AU.RData"))
  
  basin_shp <- HUC_shp[HUC_shp$REPORT %in% i, ]
  
  map <- parameter_summary_map(param_summary = param_sum_stn, au_param_summary = param_sum_au, area = basin_shp)
  
  htmlwidgets::saveWidget(map, paste0(draft_dir, name, "_param_map.html"), 
                          title = paste(name, "Status and Trends Map"), 
                          background = "grey", selfcontained = FALSE)
}
