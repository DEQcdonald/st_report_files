library(devtools)
library(rgdal)
#library(RODBC)
library(dplyr)
# devtools::install_github('donco/odeqstatusandtrends', host = 'https://api.github.com', force = TRUE, upgrade='never')
library(odeqstatusandtrends)
# devtools::install_github('donco/odeqassessment', host = 'https://api.github.com', force = TRUE, upgrade='never')
library(odeqassessment)
# devtools::install_github('donco/odeqtmdl', host = 'https://api.github.com', force = TRUE, upgrade='never')
library(odeqtmdl)
# devtools::install_github('rmichie/wqdb/wqdb', host = 'https://api.github.com', force = TRUE, upgrade='never')
# library(wqdb)
# devtools::install_github('rmichie/owri/owri', host = 'https://api.github.com', upgrade='never')
library(owri)

# devtools::install_github('TravisPritchardODEQ/AWQMSdata', host = 'https://api.github.com', force = TRUE, upgrade='never')
library(AWQMSdata)
library(dataRetrieval)
library(ggplot2)
library(lubridate)
library(pbapply)
library(tidyr)
library(htmltools)
library(knitr)
library(rmarkdown)
library(kableExtra)
library(RODBC)
library(RSQLite)
library(Rcpp)

# webshot::install_phantomjs()

# Inputs ----

start.date = "2000-01-01"
end.date = "2019-12-31"
web_output <- TRUE

top_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/2020'
gis_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS'
# gis_dir <- '//deqhq1/dwp-public/SpecialProjects/NRCS_NWQI'

# ----

complete.years <- c(as.integer(substr(start.date, start = 1, stop = 4)):as.integer(substr(end.date, start = 1, stop = 4)))
start_year <- min(complete.years)
end_year <- max(complete.years)

query_dates <- c(start.date, end.date)

# wq_db <- paste0(project_dir, name,"_data_raw_",paste0(query_dates, collapse = "."),".db")

# support_files_dir <- "//deqhq1/GISLIBRARY/Base_Data/Hydrography/Watershed_Boundaries/WBD_OR.gdb/WBD_OR.gdb/WBD_OR"

HUC_shp <- rgdal::readOGR(dsn = gis_dir, layer = 'Report_Units_HUC08', integer64="warn.loss", verbose = FALSE, stringsAsFactors = FALSE)
# tmdls <- sf::st_read(dsn = "//deqhq1/TMDL/Planning statewide/TMDL map/TMDL_Feature_gdb/TMDLs.gdb", layer = "allocations_watershed") %>% st_transform(4326)
# basin_shp <- readOGR(dsn = gis_dir, layer = 'TualatinJWC_DWSA_dissolve', integer64="warn.loss", verbose = FALSE, stringsAsFactors = FALSE)
# HUC_shp <- HUC_shp[HUC_shp$REPORT != "Willamette",]

au_names <- read.csv('//deqhq1/WQNPS/Status_and_Trend_Reports/Lookups_Statewide/AssessmentUnits_OR_Dissolve.txt', stringsAsFactors = FALSE)

missing_AUs <- NULL
wqp_stns <- NULL
state_param_sum_au <- data.frame()
state_param_sum_stn <- data.frame()
state_drop_summary <- NULL
state_status_reason <- NULL
state_target_data <- NULL

report_names <- sort(unique(HUC_shp$REPORT))

#name <- "Willamette"
#name <- "Owyhee"

for (name in report_names){
  
  print(paste0("Creating parameter summary table for the ", name, " Basin..."))
  
  data_dir <- paste0(top_dir,'/2020-', name)
  
  if(dir.exists(data_dir)) {
  } else {dir.create(data_dir)}
  
  eval_date <- Sys.Date()
  save(eval_date, file = paste0(data_dir, "/", name, "_eval_date.RData"))
  
  basin_shp <- HUC_shp[HUC_shp$REPORT %in% name, ]
  
  # print("Checking for TMDLs...")
  # basin_tmdls <- tmdls[sf::st_intersects(tmdls, sf::st_transform(sf::st_as_sf(basin_shp), 4326)) %>% lengths > 0,]
  # st_geometry(basin_tmdls) <- NULL
  # basin_tmdls <- basin_tmdls[grep("tss|phosphorus|tp|solids", basin_tmdls$NPS2, ignore.case = TRUE),]
  
  hucs <- unique(basin_shp$HUC_8)

  stations_AWQMS <- odeqstatusandtrends::get_stations_AWQMS(basin_shp)
  missing_AUs <- dplyr::bind_rows(missing_AUs, attr(stations_AWQMS, 'missing_AUs'))
  
  stations_dropped <- dplyr::bind_rows(stations_AWQMS[,c("MLocID", "OrgID")], missing_AUs[,c("MLocID", "OrgID")])
  stations_dropped$missing_au <- dplyr::if_else(stations_dropped$MLocID %in% missing_AUs$MLocID, TRUE, FALSE)

  stations_wqp <- odeqstatusandtrends::get_stations_WQP(polygon = basin_shp, start_date = start.date, end_date = end.date,
                                   huc8 = hucs, exclude.tribal.lands = TRUE)

  # if(is.data.frame(stations_wqp) && nrow(stations_wqp) > 0){
  #   print("Add these stations to the Stations Database:")
  #   print(stations_wqp)
  #   wqp_stns <- dplyr::bind_rows(wqp_stns, stations_wqp)
  # } else {stations_wqp <- NULL}
  
  if(file.exists(paste0(data_dir, "/", name, "_data_raw_", start.date, "-", end.date, ".RData"))){
    load(paste0(data_dir, "/", name, "_data_raw_", start.date, "-", end.date, ".RData"))
  } else {
    data_raw <- odeqstatusandtrends::GetData(parameters = c("Temperature", "Bacteria", "TSS", "DO", "TP", "pH"),
                                             stations_AWQMS = stations_AWQMS,
                                             # stations_WQP = stations_wqp,
                                             start.date = start.date,
                                             end.date = end.date,
                                             huc8 = hucs)
    
    print(paste0("Saving raw data from query..."))
    
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
    
    save(data_raw, file = paste0(data_dir, "/", name, "_data_raw_", start.date, "-", end.date, ".RData"))
  }
  
  stations_AWQMS$AU_Name <- au_names[match(stations_AWQMS$AU_ID, au_names$AU_ID), c("AU_Name")]

  stations_dropped$no_data <- dplyr::if_else(!stations_dropped$MLocID %in% unique(data_raw$MLocID) & !stations_dropped$missing_au, TRUE, FALSE)

  # Clean data and add criteria ---------------------------------------------
  data_dropped <- NULL
  data_clean <- odeqstatusandtrends::CleanData(data_raw)
  drop_summary <- merge(stations_dropped, data_dropped, by = c("MLocID"), all = TRUE)
  
  state_drop_summary <- dplyr::bind_rows(state_drop_summary, drop_summary)
  save(drop_summary, file = paste0(data_dir, "/", name, "_drop_summary.RData"))
  # add geoID
  # add TMDL ID
  rm(list = ls()[ls() %in% c("basin_shp", "data_raw", "drop_summary")])
  gc()
  # Assess various parameters -----------------------------------------------
  
  data_assessed <- NULL
  status <- NULL
  excur_stats <- NULL
  stat_summary <- NULL
  trend <- NULL
  data_clean$Spawn_type <- NA
  status_reason <- NULL
  
  # pH ----
  if(any(unique(data_clean$Char_Name) %in% odeqstatusandtrends::AWQMS_Char_Names('pH'))){
    print("Assessing pH...")
    data_pH <- data_clean %>% dplyr::filter(Char_Name == "pH")
    data_pH <- odeqstatusandtrends::add_criteria(data_pH)
    data_pH <- odeqassessment::Censored_data(data_pH, criteria = 'pH_Min')
    data_pH <- odeqassessment::pH_assessment(data_pH)
    data_pH$status_period <- odeqstatusandtrends::status_periods(datetime = data_pH$sample_datetime, 
                                                                 periods=4, 
                                                                 year_range = c(start_year,end_year))
    data_assessed <- dplyr::bind_rows(data_assessed, data_pH)
    
    pH_status <- odeqstatusandtrends::status_stns(df=data_pH)
    status <- dplyr::bind_rows(status, pH_status)
    
    pH_excur_stats <- odeqstatusandtrends::excursion_stats(df=data_pH)
    excur_stats <- dplyr::bind_rows(excur_stats, pH_excur_stats)
    
    pH_trend <- odeqstatusandtrends::trend_stns(data_pH)
    trend <- dplyr::bind_rows(trend, pH_trend)
    
    #Clear up memory
    rm(list = ls()[ls() %in% c("data_pH", "pH_status", "pH_excur_stats", "pH_trend")])
    gc()
  }
  
  # Temperature----
  if(any(unique(data_clean$Char_Name) %in% odeqstatusandtrends::AWQMS_Char_Names('Temperature')) &
     any(unique(data_clean[data_clean$Char_Name == "Temperature, water",]$Statistical_Base) %in% "7DADM")){
    print("Assessing temperature...")
    data_temp <- data_clean %>% dplyr::filter(Char_Name == "Temperature, water", Statistical_Base == "7DADM")
    data_temp <- odeqstatusandtrends::add_criteria(data_temp)
    
    data_temp <- odeqassessment::Censored_data(data_temp, criteria = "temp_crit")
    data_temp <- odeqassessment::temp_assessment(data_temp)
    
    data_temp_dmax <- data_clean %>% dplyr::filter(Char_Name == "Temperature, water", Statistical_Base == "Maximum")
    if(any(data_temp_dmax$Reachcode %in% odeqtmdl::tmdl_db[tmdl_db$pollutant_name_AWQMS == "Temperature, water",]$ReachCode)){
      data_temp_dmax <- which_target_df(data_temp_dmax, all_obs = FALSE)
      data_temp_dmax <- odeqassessment::Censored_data(data_temp_dmax, criteria = "target_value")
      data_temp_dmax <- target_assessment(data_temp_dmax)
      data_temp <- dplyr::bind_rows(data_temp, data_temp_dmax)
    }
    
    data_temp$status_period <- odeqstatusandtrends::status_periods(datetime = data_temp$sample_datetime, 
                                                                   periods=4, 
                                                                   year_range = c(start_year,end_year))
    data_assessed <- dplyr::bind_rows(data_assessed, data_temp)
    
    temp_status <- odeqstatusandtrends::status_stns(data_temp)
    status <- dplyr::bind_rows(status, temp_status)
    
    temp_excur_stats <- odeqstatusandtrends::excursion_stats(df=data_temp)
    excur_stats <- dplyr::bind_rows(excur_stats, temp_excur_stats)
    
    temp_trend <- odeqstatusandtrends::trend_stns(data_temp)
    trend <- dplyr::bind_rows(trend, temp_trend)
    
    #Clear up memory
    rm(list = ls()[ls() %in% c("data_temp", "data_temp_dmax", "temp_status", "temp_excur_stats", "temp_trend")])
    gc()
  }
  
  # TP -----
  if(any(unique(data_clean$Char_Name) %in% odeqstatusandtrends::AWQMS_Char_Names('TP'))){
    print("Assessing total phosphorus...")
    data_TP <- data_clean %>% dplyr::filter(Char_Name == odeqstatusandtrends::AWQMS_Char_Names('TP'))
    data_TP <- which_target_df(data_TP)
    
    data_TP <- odeqassessment::Censored_data(data_TP, criteria = "target_value")
    
    data_TP <- target_assessment(data_TP)
    
    # data_TP <- odeqassessment::TP_assessment(data_TP)
    data_TP$status_period <- odeqstatusandtrends::status_periods(datetime = data_TP$sample_datetime, 
                                                                 periods=4, 
                                                                 year_range = c(start_year,end_year))
    data_assessed <- dplyr::bind_rows(data_assessed, data_TP)
    
    TP_status <- odeqstatusandtrends::status_stns(df =data_TP)
    status <- dplyr::bind_rows(status, TP_status)
    
    TP_excur_stats <- odeqstatusandtrends::excursion_stats(df =data_TP)
    excur_stats <- dplyr::bind_rows(excur_stats, TP_excur_stats)
    
    TP_trend <- odeqstatusandtrends::trend_stns(df =data_TP)
    trend <- dplyr::bind_rows(trend, TP_trend)
    
    #Clear up memory
    rm(list = ls()[ls() %in% c("data_TP", "TP_status", "TP_excur_stats", "TP_trend")])
    gc()
  }
  
  # TSS ----
  if(any(unique(data_clean$Char_Name) %in% odeqstatusandtrends::AWQMS_Char_Names('TSS'))){
    print("Assessing total suspended solids...")
    data_TSS <- data_clean %>% dplyr::filter(Char_Name == "Total suspended solids")
    data_TSS <- which_target_df(data_TSS)
    
    data_TSS <- odeqassessment::Censored_data(data_TSS, criteria = "target_value")
    
    data_TSS <- target_assessment(df = data_TSS)
    
    # data_TSS <- odeqassessment::TSS_assessment(data_TSS)
    data_TSS$status_period <- odeqstatusandtrends::status_periods(datetime = data_TSS$sample_datetime, 
                                                                  periods=4, 
                                                                  year_range = c(start_year,end_year))
    data_assessed <- dplyr::bind_rows(data_assessed, data_TSS)
    
    TSS_status <- odeqstatusandtrends::status_stns(df=data_TSS)
    status <- dplyr::bind_rows(status, TSS_status)
    
    TSS_excur_stats <- odeqstatusandtrends::excursion_stats(df=data_TSS)
    excur_stats <- dplyr::bind_rows(excur_stats, TSS_excur_stats)
    
    TSS_trend <- odeqstatusandtrends::trend_stns(df =data_TSS)
    trend <- dplyr::bind_rows(trend, TSS_trend)
    
    #Clear up memory
    rm(list = ls()[ls() %in% c("data_TSS", "TSS_status", "TSS_excur_stats", "TSS_trend")])
    gc()
  }
  
  # Bacteria --------
  if(any(unique(data_clean$Char_Name) %in% odeqstatusandtrends::AWQMS_Char_Names('bacteria'))){
    print("Assessing bacteria...")
    data_bact <- data_clean %>% dplyr::filter(Char_Name %in% odeqstatusandtrends::AWQMS_Char_Names('bacteria'))
    data_bact <- odeqstatusandtrends::add_criteria(data_bact)
    data_bact <- data_bact %>% dplyr::mutate(bact_crit_min = pmin(bact_crit_ss, bact_crit_geomean, bact_crit_percent, na.rm = TRUE))
    data_bact <- odeqassessment::Censored_data(data_bact, criteria = "bact_crit_min")
    data_ent <- odeqassessment::Coastal_Contact_rec(data_bact)
    data_eco <- odeqassessment::Fresh_Contact_rec(data_bact)
    data_shell <- odeqassessment::Shell_Harvest(data_bact)
    data_bact <- dplyr::bind_rows(data_ent, data_eco, data_shell)
    data_bact$status_period <- odeqstatusandtrends::status_periods(datetime = data_bact$sample_datetime, 
                                                                   periods=4, 
                                                                   year_range = c(start_year,end_year))
    data_assessed <- dplyr::bind_rows(data_assessed, data_bact)
    
    bact_status <- odeqstatusandtrends::status_stns(data_bact)
    status <- dplyr::bind_rows(status, bact_status)
    
    bact_excur_stats <- odeqstatusandtrends::excursion_stats(df=data_bact)
    excur_stats <- dplyr::bind_rows(excur_stats, bact_excur_stats)
    
    bact_trend <- odeqstatusandtrends::trend_stns(data_bact)
    trend <- dplyr::bind_rows(trend, bact_trend)
    
    #Clear up memory
    rm(list = ls()[ls() %in% c("data_bact", "data_ent", "data_eco", "data_shell", "bact_status", "bact_excur_stats", "bact_trend")])
    gc()
  }
  
  # Dissolved Oxygen ---
  if(any(unique(data_clean$Char_Name) %in% odeqstatusandtrends::AWQMS_Char_Names('DO'))){
    print("Assessing dissolved oxygen...")
    data_DO <- data_clean %>% dplyr::filter(Char_Name %in% c("Dissolved oxygen (DO)", "Dissolved oxygen saturation", "Temperature, water"))
    data_DO <- odeqstatusandtrends::add_criteria(data_DO)
    data_DO <- odeqassessment::Censored_data(data_DO, criteria = "DO_crit_min")
    data_DO <-  odeqassessment::DO_assessment(data_DO)
    data_DO$status_period <- odeqstatusandtrends::status_periods(datetime = data_DO$sample_datetime, 
                                                                 periods=4, 
                                                                 year_range = c(start_year, end_year))
    data_assessed <- dplyr::bind_rows(data_assessed, data_DO)
    
    DO_status <- odeqstatusandtrends::status_stns(df=data_DO)
    status <- dplyr::bind_rows(status, DO_status)
    
    DO_excur_stats <- odeqstatusandtrends::excursion_stats(df=data_DO)
    excur_stats <- dplyr::bind_rows(excur_stats, DO_excur_stats)
    
    DO_trend <- odeqstatusandtrends::trend_stns(data_DO)
    trend <- dplyr::bind_rows(trend, DO_trend)
    
    #Clear up memory
    rm(list = ls()[ls() %in% c("data_DO", "DO_status", "DO_excur_stats", "DO_trend")])
    gc()
  }
  
  stat_summary <- odeqstatusandtrends::summary_stats(data_assessed)
  
  print(paste0("Saving assessed data..."))
  
  save(data_assessed, file = paste0(data_dir, "/", name, "_data_assessed.RData"))
  save(status, trend, excur_stats, stat_summary, file = paste0(data_dir, "/", name, "_status_trend_excur_stats.RData"))
  rm(list = ls()[ls() %in% c("stat_summary", "data_clean")])
  gc()
  
  # Assess trends -----------------------------------------------------------
  
  print("Assessing trends via the Seasonal Kendall Analysis...")
  seaken_columns <- c("MLocID", "Char_Name", "sample_datetime", "Statistical_Base", "Result_cen", "tp_month", "tp_year")
  seaken_data <- merge(data_assessed[,colnames(data_assessed)[colnames(data_assessed) %in% seaken_columns]],
                       trend[,c("MLocID", "Char_Name")], by = c("MLocID", "Char_Name"), all = FALSE)
  
  if(nrow(seaken_data) > 0){
    seaKen <- odeqstatusandtrends::sea_ken(data = seaken_data)
    seaKen_sample_size <- attributes(seaKen)$sample_size
    
    save(seaKen, file = paste0(data_dir, "/", name, "_seaken.RData"))
  } else {
    print("There were no stations with data that qualified for trend analysis...")
    seaKen <- NULL
  }
  
  # OWRI summary -------------------------------------------------------------
  
  print(paste0("Creating OWRI summary for the ", name, " Basin..."))
  
  owri.db <- "//deqhq1/WQNPS/Status_and_Trend_Reports/OWRI/OwriDbExport_122618.db"
  owri_summary <- tryCatch(owri::owri_summary(owri.db = owri.db, complete.years = complete.years, huc8 = hucs),
                           error = function(e) {
                             print("Error: OWRI summary. Setting to NULL.")
                             data.frame(NoProjects="NULL")})
  
  # Create summary table ------------------------------------------------------
  
  print(paste0("Saving parameter summary tables..."))
  
  stn_orgs <- data_assessed %>% 
    dplyr::group_by(MLocID, Char_Name) %>% 
    dplyr::summarise(Organizations = paste(unique(Org_Name), collapse = ", "))
  param_sum_stn <- odeqstatusandtrends::parameter_summary_by_station(status, seaKen, stations_AWQMS)
  param_sum_stn <- merge(param_sum_stn, stn_orgs, by = c("Char_Name", "MLocID"), all.x = TRUE, all.y = FALSE)
  
  au_orgs <- data_assessed %>% 
    dplyr::group_by(AU_ID, Char_Name) %>% 
    dplyr::summarise(Stations = paste(unique(MLocID), collapse = ", "),
                     Organizations = paste(unique(Org_Name), collapse = ", "))
  param_sum_au <- odeqstatusandtrends::parameter_summary_by_au(status, seaKen, stations_AWQMS)
  param_sum_au <- merge(param_sum_au, au_orgs, by = c("Char_Name", "AU_ID"), all.x = TRUE, all.y = FALSE)
  
  state_param_sum_stn <- rbind(state_param_sum_stn, param_sum_stn)	
  state_param_sum_au <- rbind(state_param_sum_au, param_sum_au)
  state_status_reason <- dplyr::bind_rows(state_status_reason, status_reason)
  
  target_data <- unique(data_assessed[, c("MLocID", "Char_Name", "target_value", "target_stat_base")])
  state_target_data <- dplyr::bind_rows(state_target_data, target_data)
  
  save(param_sum_stn, file = paste0(data_dir, "/", name, "_param_summary_by_station.RData"))
  save(param_sum_au, file = paste0(data_dir, "/", name, "_param_summary_by_AU.RData"))
  save(owri_summary, file = paste0(data_dir, "/", name, "_owri_summary_by_subbasin.RData"))
  save(status_reason, file = paste0(data_dir, "/", name, "_status_reason.RData"))
  
  rm(list = ls()[ls() %in% c("data_assessed", "seaken_data", "param_sum_stn", "param_sum_au", "owri_summary", "stn_orgs", "au_orgs")])
  gc()
}

save(state_param_sum_stn, file = paste0(top_dir, "/Oregon_param_summary_by_station.RData"))	
save(state_param_sum_au, file = paste0(top_dir, "/Oregon_param_summary_by_AU.RData"))
save(state_drop_summary, state_status_reason, file = paste0(top_dir, "/Oregon_drop_summary.RData"))
save(state_status_reason, file = paste0(top_dir, "/Oregon_status_reason.RData"))
save(state_target_data, file = paste0(top_dir, "/Oregon_target_data.RData"))

