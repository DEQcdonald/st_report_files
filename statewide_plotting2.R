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

# Inputs ----

report_year <- '2020'

start.date = "2000-01-01"
end.date = "2019-12-31"

top_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/2020'
gis_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS'

# ----

complete.years <- c(as.integer(substr(start.date, start = 1, stop = 4)):as.integer(substr(end.date, start = 1, stop = 4)))
query_dates <- c(start.date, end.date)

HUC_shp <- rgdal::readOGR(dsn = gis_dir, layer = 'Report_Units_HUC08', integer64="warn.loss", verbose = FALSE, stringsAsFactors = FALSE)
# HUC_shp <- HUC_shp[!(HUC_shp$REPORT %in% c("Klamath","Willamette")),]

charnames <- data.frame(awqms = c("Temperature, water", "Dissolved oxygen (DO)", "pH", "Total suspended solids", odeqstatusandtrends::AWQMS_Char_Names('TP'),
                                  "Fecal Coliform", "Escherichia coli", "Enterococcus"),
                        folder = c("Temperature", "DO", "pH", "TSS", "TP", "Fecal_Coliform", "Ecoli", "Enterococcus"),
                        file = c("temp", "DO", "pH", "TSS", "TP", "FeColi", "Ecoli", "Enterococcus"),
                        stringsAsFactors = FALSE)

# stations_AWQMS <- get_stations_AWQMS(HUC_shp)

report_names <- sort(unique(HUC_shp$REPORT))

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
#name <- "Klamath"
#name <- "Umatilla-Walla Walla-Willow"
#name <- "Owyhee"

for (name in report_names){
  
  name_abr <- report_name_abr[[name]]
  
  print(paste0("Creating plots for the ", name, " Basin..."))
  
  data_dir <- paste0(top_dir,'/', report_year,'-', name)
  
  load(file = paste0(data_dir,'/', name,'_eval_date.RData'))
  
  output_dir <- paste0(top_dir,'/wqst_map/', name_abr)
  
  plot_dir <- paste0(output_dir,'/Plots/')
  
  basin_shp <- HUC_shp[HUC_shp$REPORT %in% name, ]
  
  stations_AWQMS <- odeqstatusandtrends::get_stations_AWQMS(basin_shp)
  
  huc_names  <- stations_AWQMS %>%
    dplyr::group_by(HUC8, HUC8_Name) %>%
    dplyr::summarize(n=dplyr::n()) %>%
    dplyr::filter(n==max(n)) %>%
    dplyr::select(-n) %>%
    as.data.frame()
  
  if(dir.exists(plot_dir)) {
  } else {dir.create(plot_dir, recursive = TRUE)}
  
  load(file = paste0(data_dir, "/", name, "_data_assessed.RData"))
  
  if(file.exists(paste0(data_dir, "/", name, "_seaken.RData"))){
    load(file = paste0(data_dir, "/", name, "_seaken.RData"))
  } else {seaKen <- data.frame()}
  
  load(paste0(data_dir, "/", name, "_param_summary_by_station.RData"))
  
  # Temperature plots -------------------------------------------------------
  
  temp_stations <- unique(param_sum_stn[param_sum_stn$Char_Name == "Temperature, water",]$MLocID)
  seaKen_temp = seaKen %>% dplyr::filter(Char_Name == "Temperature, water", trend %in% c("Improving", "Degrading", "Steady"))
  temp_plots <- list()
  
  count <- 1
  for(temp_station in temp_stations){
    print(paste0("Plotting temperature data for station: ", temp_station, " (", count, " of ", length(temp_stations), ")...",name))
    
    plot_data <- data_assessed %>% dplyr::filter(Char_Name == "Temperature, water", MLocID == temp_station) %>% 
      dplyr::mutate(AU_Name = unique(param_sum_stn[param_sum_stn$MLocID == temp_station,]$AU_Name))
    
    huc <- unique(plot_data$HUC8)
    subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
    
    if(dir.exists(paste0(plot_dir, subbasin, "/Temperature"))) {
    } else {dir.create(paste0(plot_dir, subbasin, "/Temperature"), recursive = TRUE)}
    
    if(any(!is.na(plot_data$tmdl_season))){
      for(t in unique(plot_data$target_stat_base)){
        if(is.na(t)){
          plot_stat_base <- plot_data %>% dplyr::filter(is.na(target_stat_base))
          t <- "no target"
        } else {
          plot_stat_base <- plot_data %>% dplyr::filter(target_stat_base == t)
        }
      p <- odeqstatusandtrends::plot_temp_tmdl(data = plot_stat_base, seaKen = seaKen_temp, station = temp_station)
      ggsave(plot = p,
             filename = paste0(plot_dir, subbasin, "/Temperature/temp_", temp_station, "_", t, ".jpeg"),
             device = "jpeg",
             width = 8, height = 6)
      }
    } else {
      p <- odeqstatusandtrends::plot_temperature(data = plot_data, seaKen = seaKen_temp, station = temp_station)
      ggsave(plot = p,
             filename = paste0(plot_dir, subbasin, "/Temperature/temp_", temp_station, ".jpeg"),
             device = "jpeg",
             width = 8, height = 6)
    }
    
    count <- count + 1
    
    temp_plots[[temp_station]] <- p
  }
  
  temp_plots[2]
  
  # pH Plots ----------------------------------------------------------------
  
  pH_stations <- unique(c(param_sum_stn[param_sum_stn$Char_Name == "pH",]$MLocID))
  seaKen_pH = seaKen %>% dplyr::filter(Char_Name == "pH", trend %in% c("Improving", "Degrading", "Steady"))
  pH_plots <- list()
  
  count <- 1
  for(pH_station in pH_stations){
    print(paste0("Plotting pH data for station: ", pH_station, " (", count, " of ", length(pH_stations), ")...",name))
    
    plot_data <- data_assessed %>% dplyr::filter(Char_Name == "pH", MLocID == pH_station) %>% 
      dplyr::mutate(AU_Name = unique(param_sum_stn[param_sum_stn$MLocID == pH_station,]$AU_Name))
    
    huc <- unique(plot_data$HUC8)
    subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
    
    if(dir.exists(paste0(plot_dir, subbasin, "/pH"))) {
    } else {dir.create(paste0(plot_dir, subbasin, "/pH"), recursive = TRUE)}
    
    p <- odeqstatusandtrends::plot_pH(plot_data, seaKen_pH, pH_station)
    
    ggplot2::ggsave(plot = p,
                    filename = paste0(plot_dir, subbasin, "/pH/pH_", pH_station, ".jpeg"),
                    device = "jpeg",
                    width = 8, height = 6)
    count <- count + 1
    
    pH_plots[[pH_station]] <- p
  }
  
  # Total Phosphorus plots --------------------------------------------------
  
  TP_stations <- unique(param_sum_stn[param_sum_stn$Char_Name == odeqstatusandtrends::AWQMS_Char_Names('TP'),]$MLocID)
  seaKen_TP = seaKen %>% dplyr::filter(Char_Name == odeqstatusandtrends::AWQMS_Char_Names('TP'), trend %in% c("Improving", "Degrading", "Steady"))
  TP_plots <- list()
  
  count <- 1
  for(TP_station in TP_stations){
    print(paste0("Plotting TP data for station: ", TP_station, " (", count, " of ", length(TP_stations), ")...",name))
    
    plot_data <- data_assessed %>% dplyr::filter(Char_Name == odeqstatusandtrends::AWQMS_Char_Names('TP'), MLocID == TP_station) %>% 
      dplyr::mutate(AU_Name = unique(param_sum_stn[param_sum_stn$MLocID == TP_station,]$AU_Name))
    
    huc <- unique(plot_data$HUC8)
    if(is.na(unique(plot_data$HUC8))){
      huc <- unique(stations_AWQMS[stations_AWQMS$MLocID == TP_station, ]$HUC8)
    }
    subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
    
    if(dir.exists(paste0(plot_dir, subbasin, "/TP"))) {
    } else {dir.create(paste0(plot_dir, subbasin, "/TP"), recursive = TRUE)}
    
    if(!all(is.na(plot_data$Result_cen))){
      for(t in unique(plot_data$target_stat_base)){
        if(is.na(t)){
          plot_stat_base <- plot_data %>% dplyr::filter(is.na(target_stat_base))
          t <- "no target"
        } else {
          plot_stat_base <- plot_data %>% dplyr::filter(target_stat_base == t)
        }
        p <- odeqstatusandtrends::plot_TP(data = plot_stat_base, seaKen = seaKen_TP, station = TP_station)
        
        ggplot2::ggsave(plot = p,
                        filename = paste0(plot_dir, subbasin, "/TP/TP_", TP_station, "_", t, ".jpeg"),
                        device = "jpeg",
                        width = 8, height = 6)
        TP_plots[[TP_station]] <- p
      }
    } else {print("There are no detections to plot at this station")}
    count <- count + 1
  }
  
  TP_plots[2]
  
  # Total suspended solids plots --------------------------------------------
  
  TSS_stations <- unique(param_sum_stn[param_sum_stn$Char_Name == "Total suspended solids",]$MLocID)
  seaKen_TSS = seaKen %>% dplyr::filter(Char_Name == "Total suspended solids", trend %in% c("Improving", "Degrading", "Steady"))
  TSS_plots <- list()
  
  count <- 1
  for(TSS_station in TSS_stations){
    print(paste0("Plotting TSS data for station: ", TSS_station, " (", count, " of ", length(TSS_stations), ")...", name))
    
    plot_data <- data_assessed %>% dplyr::filter(Char_Name == "Total suspended solids", MLocID == TSS_station) %>% 
      dplyr::mutate(AU_Name = unique(param_sum_stn[param_sum_stn$MLocID == TSS_station,]$AU_Name))
    
    huc <- unique(plot_data$HUC8)
    subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
    
    if(dir.exists(paste0(plot_dir, subbasin, "/TSS"))) {
    } else {dir.create(paste0(plot_dir, subbasin, "/TSS"), recursive = TRUE)}
    
    if(!all(is.na(plot_data$Result_cen))){
      for(t in unique(plot_data$target_stat_base)){
        if(is.na(t)){
          plot_stat_base <- plot_data %>% dplyr::filter(is.na(target_stat_base))
          t <- "no target"
        } else {
          plot_stat_base <- plot_data %>% dplyr::filter(target_stat_base == t)
          }
      p <- odeqstatusandtrends::plot_TSS(data = plot_stat_base, seaKen = seaKen_TSS, station = TSS_station)
      
      ggplot2::ggsave(plot = p,
                      filename = paste0(plot_dir, subbasin, "/TSS/TSS_", TSS_station, "_", t, ".jpeg"),
                      device = "jpeg",
                      width = 8, height = 6)
      
      TSS_plots[[TSS_station]] <- p
      }
    } else {print("There are no detections to plot at this station")}
    count <- count + 1
  }
  
  TSS_plots[2]
  
  # Bacteria plots ----------------------------------------------------------
  
  data_bact <- data_assessed %>% dplyr::filter(Char_Name %in% odeqstatusandtrends::AWQMS_Char_Names("bacteria"))
  seaKen_bact = seaKen %>% dplyr::filter(Char_Name %in% odeqstatusandtrends::AWQMS_Char_Names("bacteria"), trend %in% c("Improving", "Degrading", "Steady"))
  bact_stations <- unique(param_sum_stn[param_sum_stn$Char_Name %in% odeqstatusandtrends::AWQMS_Char_Names("bacteria"),]$MLocID)
  bact_plots <- list()
  bact_params <- odeqstatusandtrends::AWQMS_Char_Names('bacteria')[odeqstatusandtrends::AWQMS_Char_Names('bacteria') %in% unique(c(param_sum_stn$Char_Name, as.character(seaKen$Char_Name)))]
  
  for(bact_param in bact_params){
    
    param_stations <- unique(param_sum_stn[param_sum_stn$Char_Name == bact_param,]$MLocID)
    count <- 1
    for(bact_station in param_stations){
      print(paste0("Plotting ", bact_param, " data for station: ", bact_station, " (", count, " of ", length(param_stations), ")...",name))
      
      plot_data <- data_assessed %>% dplyr::filter(Char_Name == bact_param, MLocID == bact_station) %>% 
        dplyr::mutate(AU_Name = unique(param_sum_stn[param_sum_stn$MLocID == bact_station,]$AU_Name))
      
      huc <- unique(plot_data$HUC8)
      subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
      
      if(dir.exists(paste0(plot_dir, subbasin, "/", charnames[charnames$awqms == bact_param, "folder"]))) {
      } else {dir.create(paste0(plot_dir, subbasin, "/", charnames[charnames$awqms == bact_param, "folder"]), recursive = TRUE)}
      
      # p <- plot_bacteria(data = plot_data, seaKen = seaKen_bact, station = bact_station)
      
      p_list <- odeqstatusandtrends::plot_bacteria(data = plot_data, seaKen = seaKen_bact, station = bact_station)
      
      for(p in names(p_list)){
        print(paste("Saving", p, "plot..."))
        
        ggplot2::ggsave(plot = p_list[[p]],
                        filename = paste0(plot_dir, subbasin, "/", charnames[charnames$awqms == bact_param, "folder"], "/", charnames[charnames$awqms == bact_param, "file"], "_", bact_station, "_", p, ".jpeg"),
                        device = "jpeg",
                        width = 8, height = 6)
        
        # DO_plots[[DO_station]] <- p
      }
      
      # ggsave(plot = p,
      #        filename = paste0(plot_dir, subbasin, "/", charnames[charnames$awqms == bact_param, "folder"], "/", charnames[charnames$awqms == bact_param, "file"], "_", bact_station, ".jpeg"),
      #        device = "jpeg",
      #        width = 8, height = 6)
      count <- count + 1
      
      # bact_plots[[bact_station]] <- p
    }
  }
  
  # bact_plots[2]
  
  # Dissolved oxygen plots -------------------------------------------------------
  
  data_DO <- data_assessed %>% dplyr::filter(Char_Name %in% c("Dissolved oxygen (DO)"))
  seaKen_DO = seaKen %>% dplyr::filter(Char_Name == "Dissolved oxygen (DO)", trend %in% c("Improving", "Degrading", "Steady"))
  DO_stations <- unique(param_sum_stn[param_sum_stn$Char_Name == "Dissolved oxygen (DO)",]$MLocID)
  # DO_stations <- unique((data_DO %>% dplyr::filter(is.na(Statistical_Base), MLocID %in% DO_stations))$MLocID)
  DO_plots <- list()
  
  count <- 1
  for(DO_station in DO_stations){
    print(paste0("Plotting dissolved oxygen data for station: ", DO_station, " (", count, " of ", length(DO_stations), ")...",name))
    
    # plot_data <- data_DO %>% dplyr::filter(Char_Name == "Dissolved oxygen (DO)", MLocID == DO_station, is.na(Statistical_Base))
    plot_data <- data_DO %>% dplyr::filter(MLocID == DO_station) %>% dplyr::mutate(AU_Name = unique(param_sum_stn[param_sum_stn$MLocID == DO_station,]$AU_Name))
    
    huc <- unique(plot_data$HUC8)
    subbasin <- huc_names[huc_names$HUC8 == huc,]$HUC8_Name
    
    if(dir.exists(paste0(plot_dir, subbasin, "/DO"))) {
    } else {dir.create(paste0(plot_dir, subbasin, "/DO"), recursive = TRUE)}
    
    p_list <- odeqstatusandtrends::plot_DO(data = plot_data, seaKen = seaKen_DO, 
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
