library(devtools)
library(rgdal)
#library(RODBC)
library(dplyr)
# devtools::install_github('donco/odeqstatusandtrends', host = 'https://api.github.com', force = TRUE, upgrade='never')
library(odeqstatusandtrends)
# devtools::install_github('donco/odeqassessment', host = 'https://api.github.com', force = TRUE, upgrade='never')
library(odeqassessment)
# devtools::install_github('rmichie/wqdb/wqdb', host = 'https://api.github.com', upgrade='never')
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

start.date = "1999-01-01"
end.date = "2018-12-30"
web_output <- TRUE

top_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/2019'
gis_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS'
# gis_dir <- '//deqhq1/dwp-public/SpecialProjects/NRCS_NWQI'

# ----

complete.years <- c(as.integer(substr(start.date, start = 1, stop = 4)):as.integer(substr(end.date, start = 1, stop = 4)))

query_dates <- c(start.date, end.date)

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
state_org_sums <- NULL
state_station_sums <- NULL
state_param_sum_stn_data <- NULL
state_param_sum_au_data <- NULL

report_names <- sort(unique(HUC_shp$REPORT))

appendix_letter <- list("Black Rock Desert-Humboldt"="A",
                        "Columbia River"="B",
                        "Deschutes"="C",
                        "Goose Lake"="D",
                        "Grande Ronde"="E",
                        "John Day"="F",
                        "Klamath"="G",
                        "Malheur"="H",
                        "Mid-Coast"="I",
                        "Middle Columbia-Hood"="J",
                        "North Coast-Lower Columbia"="K",
                        "Oregon Closed Basins"="L",
                        "Owyhee"="M",
                        "Powder-Burnt"="N",
                        "Rogue"="O",
                        "Sandy"="P",
                        "Snake River"="Q",
                        "South Coast"="R",
                        "Umatilla-Walla Walla-Willow"="S",
                        "Umpqua"="T",
                        "Willamette"="U")

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

for (name in report_names){

  name_abr <- report_name_abr[[name]]
  a.letter <- appendix_letter[[name]]

  print(paste0("Creating parameter summary table for the ", name, " Basin..."))

  data_dir <- paste0(top_dir,'/2019-', name)

  if(dir.exists(data_dir)) {
  } else {dir.create(data_dir)}

  if(web_output) {
    if(dir.exists(paste0(top_dir,'/web_wqst_2019'))) {
    } else {dir.create(paste0(top_dir,'/web_wqst_2019'))}
    output_dir <- paste0(top_dir,'/web_wqst_2019/', name_abr)
    xlsx_name <- paste0("Appendix_",a.letter,"_",name,"_Results.xlsx")
  } else {
    output_dir <- paste0(data_dir,'/WQST_2019-',name,'_DRAFT_', eval_date)
    xlsx_name <- paste0("Appendix_",a.letter,"_",name,"_Results_DRAFT_", eval_date, ".xlsx")
  }

  if(dir.exists(output_dir)) {
  } else {dir.create(output_dir)}

  eval_date <- Sys.Date()
  save(eval_date, file = paste0(data_dir, name, "_eval_date.RData"))

  basin_shp <- HUC_shp[HUC_shp$REPORT %in% name, ]

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

  if(file.exists(paste0(data_dir, "/", name, "_data_raw_", start.date, "-", end.date, ".RData"))){
    load(paste0(data_dir, "/", name, "_data_raw_", start.date, "-", end.date, ".RData"))
  } else {
    data_raw <- GetData(parameters = c("Temperature", "Bacteria", "TSS", "DO", "TP", "pH"),
                        stations_AWQMS = stations_AWQMS,
                        stations_WQP = stations_wqp,
                        start.date = start.date,
                        end.date = end.date,
                        huc8 = hucs)

    print(paste0("Saving raw data from query..."))

    save(data_raw, file = paste0(data_dir, "/", name, "_data_raw_", start.date, "-", end.date, ".RData"))
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

  save(data_assessed, file = paste0(data_dir, "/", name, "_data_assessed.RData"))
  save(status, trend, excur_stats, file = paste0(data_dir, "/", name, "_status_trend_excur_stats.RData"))

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

  # save(status, trend, stations_AWQMS, file = paste0(data_dir, name, "_seaken_inputs.RData"))


  # Assess trends -----------------------------------------------------------

  print("Assessing trends via the Seasonal Kendall Analysis...")
  seaken_columns <- c("MLocID", "Char_Name", "sample_datetime", "Statistical_Base", "Result_cen", "tp_month", "tp_year")
  seaken_data <- merge(data_assessed[,colnames(data_assessed)[colnames(data_assessed) %in% seaken_columns]],
                       trend[,c("MLocID", "Char_Name")], by = c("MLocID", "Char_Name"), all = FALSE)

  if(nrow(seaken_data) > 0){
    seaKen <- sea_ken(data = seaken_data)
    seaKen_sample_size <- attributes(seaKen)$sample_size

    save(seaKen, file = paste0(data_dir, "/", name, "_seaken.RData"))
  } else {
    print("There were no stations with data that qualified for trend analysis...")
    seaKen <- NULL
  }

  # OWRI summary -------------------------------------------------------------

  print(paste0("Creating OWRI summary for the ", name, " Basin..."))

  owri.db <- "//deqhq1/WQNPS/Status_and_Trend_Reports/OWRI/OwriDbExport_122618.db"
  owri_summary <- tryCatch(owri_summary(owri.db = owri.db, complete.years = complete.years, huc8 = hucs),
                           error = function(e) {
                             print("Error: OWRI summary. Setting to NULL.")
                             data.frame(NoProjects="NULL")})

  # Create summary table ------------------------------------------------------

  print(paste0("Saving parameter summary tables..."))

  stn_orgs <- data_assessed %>% group_by(MLocID, Char_Name) %>% summarise(Organizations = paste(unique(Org_Name), collapse = ", "))
  param_sum_stn <- parameter_summary_by_station(status, seaKen, stations_AWQMS)
  param_sum_stn <- merge(param_sum_stn, stn_orgs, by = c("Char_Name", "MLocID"), all.x = TRUE, all.y = FALSE)

  au_orgs <- data_assessed %>% group_by(AU_ID, Char_Name) %>% summarise(Stations = paste(unique(MLocID), collapse = ", "),
                                                                        Organizations = paste(unique(Org_Name), collapse = ", "))
  param_sum_au <- parameter_summary_by_au(status, seaKen, stations_AWQMS)
  param_sum_au <- merge(param_sum_au, au_orgs, by = c("Char_Name", "AU_ID"), all.x = TRUE, all.y = FALSE)

  save(param_sum_stn, file = paste0(data_dir, "/", name, "_param_summary_by_station.RData"))
  save(param_sum_au, file = paste0(data_dir, "/", name, "_param_summary_by_AU.RData"))
  save(owri_summary, file = paste0(data_dir, "/", name, "_owri_summary_by_subbasin.RData"))
}
