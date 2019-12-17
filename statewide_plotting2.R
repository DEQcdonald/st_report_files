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
library(knitr)
library(rmarkdown)
library(kableExtra)
library(htmlwidgets)
library(rmapshaper)
library(RODBC)
library(RSQLite)
library(htmltools)
library(Rcpp)
# library(grid)
# library(gridExtra)
# webshot::install_phantomjs()

start.date = "1999-01-01"
end.date = "2018-12-30"
complete.years <- c(as.integer(substr(start.date, start = 1, stop = 4)):as.integer(substr(end.date, start = 1, stop = 4)))

query_dates <- c(start.date, end.date)

gis_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS'

HUC_shp <- readOGR(dsn = gis_dir, layer = 'Report_Units_HUC08', integer64="warn.loss", verbose = FALSE, stringsAsFactors = FALSE)
# HUC_shp <- HUC_shp[!(HUC_shp$REPORT %in% c("Klamath","Willamette")),]

# stations_AWQMS <- get_stations_AWQMS(HUC_shp)

report_names <- sort(unique(HUC_shp$REPORT))

#name <- "Willamette"

for (name in report_names){

  print(paste0("Creating plots for the ", name, " Basin..."))

  project_dir <- paste0('//deqhq1/WQNPS/Status_and_Trend_Reports/2019/2019-', name, '/')
  
  load(file = paste0(project_dir, name, "_eval_date.RData"))

  plot_dir <- paste0(project_dir,'WQST_2019-',name,'_DRAFT_', eval_date, '/Plots/')
  
  basin_shp <- HUC_shp[HUC_shp$REPORT %in% name, ]
  
  stations_AWQMS <- get_stations_AWQMS(basin_shp)
  huc_names <- unique(stations_AWQMS[,c("HUC8", "HUC8_Name")])

  if(dir.exists(plot_dir)) {
  } else {dir.create(plot_dir, recursive = TRUE)}

  load(file = paste0(project_dir, name, "_data_assessed.RData"))

  if(file.exists(paste0(project_dir, name, "_seaken.RData"))){
    load(file = paste0(project_dir, name, "_seaken.RData"))
  } else {seaKen <- data.frame()}

  load(paste0(project_dir, name, "_param_summary_by_station.RData"))
  
  # Temperature plots -------------------------------------------------------

  temp_stations <- unique(param_sum_stn[param_sum_stn$Char_Name == "Temperature, water",]$MLocID)
  temp_plots <- list()

  count <- 1
  for(temp_station in temp_stations){
    print(paste0("Plotting temperature data for station: ", temp_station, " (", count, " of ", length(temp_stations), ")...",name))

    plot_data <- data_assessed %>% filter(Char_Name == "Temperature, water", MLocID == temp_station)
    
    huc <- unique(plot_data$HUC8)
    subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name

    if(dir.exists(paste0(plot_dir, subbasin, "/Temperature"))) {
    } else {dir.create(paste0(plot_dir, subbasin, "/Temperature"), recursive = TRUE)}

    p <- plot_temperature(data = plot_data, seaKen = seaKen[seaKen$Char_Name == "Temperature, water",], station = temp_station)

    ggsave(plot = p,
           filename = paste0(plot_dir, subbasin, "/Temperature/temp_", temp_station, ".jpeg"),
           device = "jpeg",
           width = 8, height = 6)
    count <- count + 1

    temp_plots[[temp_station]] <- p
  }

  temp_plots[2]

  # pH Plots ----------------------------------------------------------------

  pH_stations <- unique(c(param_sum_stn[param_sum_stn$Char_Name == "pH",]$MLocID))
  pH_plots <- list()

  count <- 1
  for(pH_station in pH_stations){
    print(paste0("Plotting pH data for station: ", pH_station, " (", count, " of ", length(pH_stations), ")...",name))

    plot_data <- data_assessed %>% filter(Char_Name == "pH", MLocID == pH_station)
    
    huc <- unique(plot_data$HUC8)
    subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
    
    if(dir.exists(paste0(plot_dir, subbasin, "/pH"))) {
    } else {dir.create(paste0(plot_dir, subbasin, "/pH"), recursive = TRUE)}

    p <- plot_pH(plot_data, seaKen[seaKen$Char_Name == "pH",], pH_station)

    ggsave(plot = p,
           filename = paste0(plot_dir, subbasin, "/pH/pH_", pH_station, ".jpeg"),
           device = "jpeg",
           width = 8, height = 6)
    count <- count + 1

    pH_plots[[pH_station]] <- p
  }

  # Total Phosphorus plots --------------------------------------------------

  TP_stations <- unique(param_sum_stn[param_sum_stn$Char_Name == "Phosphate-phosphorus",]$MLocID)
  TP_plots <- list()

  count <- 1
  for(TP_station in TP_stations){
    print(paste0("Plotting TP data for station: ", TP_station, " (", count, " of ", length(TP_stations), ")...",name))

    plot_data <- data_assessed %>% filter(Char_Name == "Phosphate-phosphorus", MLocID == TP_station)
    
    huc <- unique(plot_data$HUC8)
    if(is.na(unique(plot_data$HUC8))){
      huc <- unique(stations_AWQMS[stations_AWQMS$MLocID == TP_station, ]$HUC8)
    }
    subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
    
    if(dir.exists(paste0(plot_dir, subbasin, "/TP"))) {
    } else {dir.create(paste0(plot_dir, subbasin, "/TP"), recursive = TRUE)}

    if(!all(is.na(plot_data$Result_cen))){
    p <- plot_TP(data = plot_data, seaKen = seaKen[seaKen$Char_Name == "Phosphate-phosphorus",], station = TP_station)

    ggsave(plot = p,
           filename = paste0(plot_dir, subbasin, "/TP/TP_", TP_station, ".jpeg"),
           device = "jpeg",
           width = 8, height = 6)
    TP_plots[[TP_station]] <- p
    } else {print("There are no detections to plot at this station")}
    count <- count + 1
  }

  TP_plots[2]

  # Total suspended solids plots --------------------------------------------

  TSS_stations <- unique(param_sum_stn[param_sum_stn$Char_Name == "Total suspended solids",]$MLocID)
  TSS_plots <- list()

  count <- 1
  for(TSS_station in TSS_stations){
    print(paste0("Plotting TSS data for station: ", TSS_station, " (", count, " of ", length(TSS_stations), ")...", name))

    plot_data <- data_assessed %>% filter(Char_Name == "Total suspended solids", MLocID == TSS_station)
    
    huc <- unique(plot_data$HUC8)
    subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
    
    if(dir.exists(paste0(plot_dir, subbasin, "/TSS"))) {
    } else {dir.create(paste0(plot_dir, subbasin, "/TSS"), recursive = TRUE)}

    if(!all(is.na(plot_data$Result_cen))){
      p <- plot_TSS(data = plot_data, seaKen = seaKen[seaKen$Char_Name == "Total suspended solids",], station = TSS_station)

      ggsave(plot = p,
             filename = paste0(plot_dir, subbasin, "/TSS/TSS_", TSS_station, ".jpeg"),
             device = "jpeg",
             width = 8, height = 6)

      TSS_plots[[TSS_station]] <- p
    } else {print("There are no detections to plot at this station")}
    count <- count + 1
  }

  TSS_plots[2]

  # Bacteria plots ----------------------------------------------------------

  data_bact <- data_assessed %>% dplyr::filter(Char_Name %in% AWQMS_Char_Names("bacteria"))
  bact_stations <- unique(param_sum_stn[param_sum_stn$Char_Name %in% AWQMS_Char_Names("bacteria"),]$MLocID)
  bact_plots <- list()
  bact_params <- AWQMS_Char_Names('bacteria')[AWQMS_Char_Names('bacteria') %in% unique(c(param_sum_stn$Char_Name, as.character(seaKen$Char_Name)))]

  for(bact_param in bact_params){

    param_stations <- unique(param_sum_stn[param_sum_stn$Char_Name == bact_param,]$MLocID)
    count <- 1
    for(bact_station in param_stations){
      print(paste0("Plotting ", bact_param, " data for station: ", bact_station, " (", count, " of ", length(param_stations), ")...",name))

      plot_data <- data_assessed %>% filter(Char_Name == bact_param, MLocID == bact_station)
     
      huc <- unique(plot_data$HUC8)
      subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
      
      if(dir.exists(paste0(plot_dir, subbasin, "/", bact_param))) {
      } else {dir.create(paste0(plot_dir, subbasin, "/", bact_param), recursive = TRUE)}

      p <- plot_bacteria(data = plot_data, seaKen = seaKen[seaKen$Char_Name == bact_param,], station = bact_station)

      ggsave(plot = p,
             filename = paste0(plot_dir, subbasin, "/", bact_param, "/", bact_param, "_", bact_station, ".jpeg"),
             device = "jpeg",
             width = 8, height = 6)
      count <- count + 1

      bact_plots[[bact_station]] <- p
    }
  }

  bact_plots[2]

  # Dissolved oxygen plots -------------------------------------------------------

  data_DO <- data_assessed %>% dplyr::filter(Char_Name %in% c("Dissolved oxygen (DO)"))
  DO_stations <- unique(param_sum_stn[param_sum_stn$Char_Name == "Dissolved oxygen (DO)",]$MLocID)
  # DO_stations <- unique((data_DO %>% filter(is.na(Statistical_Base), MLocID %in% DO_stations))$MLocID)
  DO_plots <- list()

  count <- 1
  for(DO_station in DO_stations){
    print(paste0("Plotting dissolved oxygen data for station: ", DO_station, " (", count, " of ", length(DO_stations), ")...",name))

    # plot_data <- data_DO %>% filter(Char_Name == "Dissolved oxygen (DO)", MLocID == DO_station, is.na(Statistical_Base))
    plot_data <- data_DO %>% filter(MLocID == DO_station)
    
    huc <- unique(plot_data$HUC8)
    subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
    
    if(dir.exists(paste0(plot_dir, subbasin, "/DO"))) {
    } else {dir.create(paste0(plot_dir, subbasin, "/DO"), recursive = TRUE)}

    p_list <- plot_DO(data = plot_data, seaKen = seaKen[seaKen$Char_Name == "Dissolved oxygen (DO)",], 
                 station = DO_station)

    for(p in names(p_list)){
      print(paste("Saving", p, "plot..."))
      
      ggsave(plot = p_list[[p]],
             filename = paste0(plot_dir, subbasin, "/DO/DO_", DO_station, "_", p, ".jpeg"),
             device = "jpeg",
             width = 8, height = 6)
      
      # DO_plots[[DO_station]] <- p
    }
    count <- count + 1
  }

}
