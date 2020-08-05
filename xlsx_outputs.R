library(devtools)
library(dplyr)
# devtools::install_github('donco/odeqstatusandtrends', host = 'https://api.github.com', upgrade='never')
library(odeqstatusandtrends)
library(tidyr)
library(lubridate)
library(openxlsx)
library(rgdal)
library(sf)
library(base64enc)

# Inputs ----
start.date = "2000-01-01"
end.date = "2019-12-31"
year = 2020
report_name <- paste0(year, " Oregon Statewide Status and Trend Report")

top_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/2020-Revision'
gis_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS'

logo <- "//deqhq1/WQNPS/Status_and_Trend_Reports/Figures/DEQ-logo-color-non-transp71x107.png"

au_names <- read.csv('//deqhq1/WQNPS/Status_and_Trend_Reports/Lookups_Statewide/AssessmentUnits_OR_Dissolve.txt', stringsAsFactors = FALSE)

# ----

complete.years <- c(as.integer(substr(start.date, start = 1, stop = 4)):as.integer(substr(end.date, start = 1, stop = 4)))

HUC_shp <- rgdal::readOGR(dsn = gis_dir, layer = 'Report_Units_HUC08',
                          integer64="warn.loss", verbose = FALSE, stringsAsFactors = FALSE)

agwqma_shp <- sf::st_read(dsn = "//deqhq1/WQNPS/Status_and_Trend_Reports/GIS",
                          layer = "ODA_AgWQMA_w_Willamette",
                          stringsAsFactors = FALSE)

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

# document name used for web download
report_name_abr <- list("Black Rock Desert-Humboldt"="BlackRock",
                        "Columbia River"="ColumbiaRiver",
                        "Deschutes"="Deschutes",
                        "Goose Lake"="GooseLake",
                        "Grande Ronde"="GrandeRonde",
                        "John Day"="JohnDay",
                        "Klamath"="Klamath",
                        "Malheur"="Malheur",
                        "Mid-Coast"="MidCoast",
                        "Middle Columbia-Hood"="MidColumbia",
                        "North Coast-Lower Columbia"="NCoast",
                        "Oregon Closed Basins"="ClosedBasins",
                        "Owyhee"="Owyhee",
                        "Powder-Burnt"="PowderBurnt",
                        "Rogue"="Rogue",
                        "Sandy"="Sandy",
                        "Snake River"="SnakeRiver",
                        "South Coast"="SouthCoast",
                        "Umatilla-Walla Walla-Willow"="Umatilla",
                        "Umpqua"="Umpqua",
                        "Willamette"="Willamette")

#name <- "Willamette"
#name <- "Owyhee"
#name <- "Umatilla-Walla Walla-Willow"

for (name in report_names){
  
  name_abr <- report_name_abr[[name]]
  a.letter <- appendix_letter[[name]]
  xlsx_name <- gsub(" ", "", paste0("Appendix",a.letter,name_abr,".xlsx"), fixed=TRUE)
  xlsx_name <- gsub("-","",xlsx_name, fixed=TRUE)

  output_dir <- paste0(top_dir,'/Statewide Report')

  data_dir <- paste0(top_dir,'/', year, '-', name)
  basin_shp <- HUC_shp[HUC_shp$REPORT %in% name, ]
  
  stations_AWQMS <- odeqstatusandtrends::get_stations_AWQMS(basin_shp)
  
  stations_AWQMS$AU_Name <- au_names[match(stations_AWQMS$AU_ID, au_names$AU_ID),
                                     c("AU_Name")]
  stations_AWQMS_shp <- stations_AWQMS %>%
    dplyr::mutate(Long_sf=Long_DD,
                  Lat_sf=Lat_DD)
  
  stations_AWQMS_shp <- sf::st_transform(sf::st_as_sf(stations_AWQMS_shp, coords = c("Long_sf",  "Lat_sf"), crs = sf::st_crs("+init=EPSG:4269")),crs=sf::st_crs(agwqma_shp))
  
  stations_AWQMS_shp <- sf::st_join(stations_AWQMS_shp, agwqma_shp, left=TRUE)
  
  stations_AgWQMA <- stations_AWQMS_shp %>%
    dplyr::select(MLocID, PlanNameST) %>%
    sf::st_drop_geometry() %>%
    unique()
  
  load(file = paste0(data_dir, "/", name, "_data_assessed.RData"))
  load(file = paste0(data_dir, "/", name, "_eval_date.RData"))
  load(file = paste0(data_dir, "/", name, "_owri_summary_by_subbasin.RData"))
  load(file = paste0(data_dir, "/", name, "_param_summary_by_AU.RData"))
  load(file = paste0(data_dir, "/", name, "_param_summary_by_station.RData"))
  load(file = paste0(data_dir, "/", name, "_status_trend_excur_stats.RData"))
  load(file = paste0(data_dir, "/", name, "_drop_summary.RData"))
  load(file = paste0(data_dir, "/", name, "_status_reason.RData"))
  
  if(file.exists(paste0(data_dir, "/", name, "_seaken.RData"))){
    load(file = paste0(data_dir, "/", name, "_seaken.RData"))
  } else {seaKen <- data.frame()}
  
  # Create summary table ------------------------------------------------------
  
  print(paste0("Saving parameter summary tables..."))
  
  #-- Notes ---------------------------
  
  Notes <- data.frame(stringsAsFactors=FALSE,
                      Table=c(paste0("Table ",a.letter,"-", seq(1,9, by=1),"      ")),
                      Description=c(paste0("Water quality status and/or trend results at monitoring stations within the ", 
                                           name,"."),
                                    paste0("Water quality status results for Assessment Units within the ", name,"."),
                                    paste0("Station summary describing the number of excursions and total number of results ",
                                           "shown as '(excursions/results)' for each parameter during each status period. ", 
                                           "If excursions > 0, the '(minimum;median;maximum)' of all excursion result values are provided. ",
                                           "If stations have a total phosphorus or total suspended solids status of 'Unassessed' due to ",
                                           "there being no TMDL targets, the (minimum;median;maximum) values for all results are provided. ",
                                           "All minimum, median, and maximum summary results are in units of mg/L except ",
                                           "temperature (degrees Celsius), pH (s.u.), and bacteria indicators (CFU/100mL)."),
                                    paste0("Seasonal Kendall trend test results for each parameter at monitoring stations within the ",name,"."),
                                    paste0("Summary of cumulative treatment outputs reported to the Oregon Watershed Restoration Inventory within the ", 
                                           name,"."),
                                    paste0("Summary of organizations that collected data in the ", 
                                           name,
                                           ", the number of results used in this analysis, and the number of unique stations monitored."),
                                    "Number of results per year for monitoring stations that fit the criteria to assess status or trends.",
                                    "Summary of results that were obtained through the data query and removed through various methods in the analysis process.",
                                    "Summary of stations with an 'Unassessed' status and the reason for that assignment.")
  )
  
  #-- Trend Stats ----------------------------  
  
  if(is.null(seaKen) | nrow(seaKen)==0){
    seaKen <- data.frame(Comment = "There were no stations with data that qualified for trend analysis")
  } else {
    seaKen <- seaKen %>%
      dplyr::left_join(stations_AWQMS, by="MLocID") %>%
      dplyr::left_join(stations_AgWQMA, by="MLocID") %>%
      dplyr::select('Station ID' = MLocID,
                    'Station Name' = StationDes,
                    'Subbasin Name' = HUC8_Name,
                    HUC8,
                    'Agricultural Water Quality Management Area'=PlanNameST,
                    'Assessment Unit ID' = AU_ID,
                    Latitude = Lat_DD,
                    Longitude = Long_DD,
                    Parameter = Char_Name,
                    p_value,
                    Slope=slope,
                    Intercept=intercept,
                    Significance=significance,
                    "Trend Result"=trend)
    
  }
  
  #-- Excursion Stats -----------------------
  
  bins <- odeqstatusandtrends::status_periods(datetime=data_assessed$sample_datetime, year_range=c(min(complete.years), max(complete.years)), bins_only = TRUE)
  bins <- sort(gsub("status_", "", bins))
  
  excur_stats_all <- data.frame()

  # This isn't used but not ready to delete yet.
  # excur_stats0 <- excur_stats %>%
  #   tidyr::pivot_longer(cols=c(starts_with("percent_excursion"), starts_with("results_n"),
  #                              starts_with("excursions_n"), starts_with("excursion_max"), 
  #                              starts_with("excursion_median"), starts_with("excursion_min")),
  #                       names_to = c("stat", "status_period"),
  #                       names_pattern = "(.*_status_*)_(.*)")
  
  for(i in 1:length(bins)) {
    
    excur_stats1 <- excur_stats %>%
      dplyr::right_join(status, by=c("MLocID", "Char_Name")) %>%
      dplyr::right_join(stat_summary, by=c("MLocID", "Char_Name", "Spawn_type")) %>%
      dplyr::mutate(Char_Name=dplyr::case_when(Char_Name=="Temperature, water" ~ paste0(Char_Name," ",Spawn_type),
                                               Char_Name=="Dissolved oxygen (DO)" ~ paste0(Char_Name," ",Spawn_type),
                                               TRUE ~ Char_Name)) %>%
      dplyr::select(MLocID, Char_Name, contains(bins[i]), -contains("percent")) %>%
      as.data.frame()
    
    colnames(excur_stats1) <- gsub(colnames(excur_stats1), pattern="_status", replacement = "")   
    colnames(excur_stats1) <- gsub(colnames(excur_stats1), pattern=paste0("_",bins[i]), replacement = "")

    if(!"results_n" %in% names(excur_stats1)) {
      excur_stats3 <- data.frame(Comment = paste0("There were no stations with results in the ", gsub("_","-",bins[i])," status period."))
    } else {
      excur_stats2 <- excur_stats1 %>%
        dplyr::rename(status=tidyr::contains("status")) %>%
        dplyr::mutate(status_period=gsub(gsub(bins[i], pattern="status_", replacement = ""),pattern="_", replacement = "-"),
                      excursion_min=if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(excursion_min, 1), excursion_min),
                      excursion_median=ifelse(grepl("Temperature|Dissolved oxygen", Char_Name), round(excursion_median, 1), excursion_median),
                      excursion_max=if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(excursion_max, 1), excursion_max),
                      min=if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(min, 1), min),
                      median=if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(median, 1), median),
                      max=if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(max, 1), max))
      
      excur_stats3 <- excur_stats2 %>%
        dplyr::filter(!is.na(results_n)) %>%
        dplyr::mutate(results=dplyr::case_when((Char_Name %in% c("Total Phosphorus, mixed forms", "Total suspended solids") & status=="Unassessed") ~ paste0("Unassessed N=",results_n," (", min,";", median,";", max,")"),
                                               (!(Char_Name %in% c("Total Phosphorus, mixed forms", "Total suspended solids") & status=="Unasssed") & excursions_n > 0) ~ paste0(excursions_n,"/",results_n,"; (",excursion_min,";", excursion_median,";", excursion_max,")"),
                                               TRUE ~ paste0(excursions_n,"/",results_n))) %>%
        dplyr::select(MLocID, status_period, Char_Name, results) %>%
        tidyr::pivot_wider(names_from=Char_Name, values_from=results) %>%
        dplyr::left_join(stations_AWQMS_shp, by="MLocID") %>%
        dplyr::select("Station ID" = MLocID,
                      "Station Name" = StationDes,
                      "Subbasin Name" = HUC8_Name,
                      HUC8,
                      "Agricultural Water Quality Management Area"=PlanNameST,
                      "Assessment Unit ID" = AU_ID,
                      "Latitude" = Lat_DD,
                      "Longitude" = Long_DD,
                      "Status Period"=status_period,
                      "Dissolved Oxygen"=contains("Dissolved oxygen (DO) Not_Spawn"),
                      "Dissolved Oxygen Spawning"=contains("Dissolved oxygen (DO) Spawn"),
                      tidyr::matches("Escherichia coli"),
                      tidyr::contains("Entero"),
                      tidyr::contains("Fecal"),
                      tidyr::one_of("pH"),
                      "Temperature Non-Spawning Period"=matches("Temperature, water Not_Spawn"),
                      "Temperature Spawning Period"=matches("Temperature, water Spawn"),
                      "Total Phosphorus"=contains("Phosphorus"),
                      "Total Suspended Solids"=matches("Total suspended solids")) %>%
        as.data.frame()
      
    }
    
    excur_stats_all <- dplyr::bind_rows(excur_stats_all, excur_stats3)
  }
  
  excur_stats_all <- excur_stats_all %>%
  dplyr::arrange(`Station ID`, `Status Period`)
  
  #-- Station Summary ---------------------------
  
  param_sum_stn <- param_sum_stn %>%
    dplyr::left_join(stations_AgWQMA, by="MLocID") %>%
    dplyr::select('Station ID' = MLocID,
                  'Station Name' = StationDes,
                  "Subbasin Name" = HUC8_Name,
                  HUC8,
                  'Agricultural Water Quality Management Area'=PlanNameST,
                  'Assessment Unit ID' = AU_ID,
                  "Latitude" = Lat_DD,
                  "Longitude" = Long_DD,
                  "Parameter" = Char_Name,
                  "Sampling Organizations" = Organizations,
                  grep('status', colnames(param_sum_stn), value = TRUE),
                  'Trend' = trend)
  
  colnames(param_sum_stn) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(param_sum_stn), perl = TRUE)
  colnames(param_sum_stn) <- gsub("_", " ", colnames(param_sum_stn), perl = TRUE)
  colnames(param_sum_stn) <- sapply(colnames(param_sum_stn), simpleCap, USE.NAMES = FALSE)
  
  #-- AU Summary ---------------------------
  
  param_sum_au <- param_sum_au %>%
    dplyr::select('Assessment Unit ID' = AU_ID,
                  'Assessment Unit Name' = AU_Name,
                  "Subbasin Name" = HUC8_Name,
                  HUC8,
                  "Parameter" = Char_Name,
                  'Station IDs' = Stations,
                  "Sampling Organizations" = Organizations,
                  grep('status', colnames(param_sum_au), value = TRUE)
    )
  
  colnames(param_sum_au) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(param_sum_au), perl = TRUE)
  colnames(param_sum_au) <- gsub("_", " ", colnames(param_sum_au), perl = TRUE)
  colnames(param_sum_au) <- sapply(colnames(param_sum_au), simpleCap, USE.NAMES = FALSE)
  
  # -- Results by Org --------------------  
  
  org_sums <- data_assessed %>%
    dplyr::mutate(Basin = name) %>%
    dplyr::group_by(Org_Name, Basin) %>%
    dplyr::summarise('Unique Stations' = length(unique(MLocID)),
                     'Temperature Results' = sum(Char_Name == "Temperature, water"),
                     'DO Results' = sum(Char_Name == "Dissolved oxygen (DO)"),
                     'pH Results' = sum(Char_Name == "pH"),
                     'TSS Results' = sum(Char_Name == "Total suspended solids"),
                     'TP Results' = sum(Char_Name == odeqstatusandtrends::AWQMS_Char_Names('TP')),
                     'E. Coli Results' = sum(Char_Name == "Escherichia coli"),
                     'Fecal Coliform Results' = sum(Char_Name == "Fecal Coliform"),
                     'Enterococcus Results' = sum(Char_Name == "Enterococcus")) %>%
    dplyr::rename('Sampling Organization' = Org_Name)
  
  #-- Results by Year ---------------------  
  
  station_sums <- data_assessed %>%
    dplyr::left_join(stations_AgWQMA, by="MLocID") %>%
    dplyr::mutate(#Basin = name,
      Year = lubridate::year(sample_datetime),
      HUC8_Name = stations_AWQMS[match(HUC8, stations_AWQMS$HUC8),]$HUC8_Name) %>%
    dplyr::group_by(MLocID, StationDes, HUC8_Name, HUC8, PlanNameST, AU_ID, Char_Name, Year) %>%
    dplyr::summarise(Results = n()) %>%
    tidyr::pivot_wider(names_from = "Year", values_from = "Results") %>%
    dplyr::rename('Station ID' = MLocID,
                  'Station Name' = StationDes,
                  "Subbasin Name" = HUC8_Name,
                  'Agricultural Water Quality Management Area'=PlanNameST,
                  'Assessment Unit ID' = AU_ID,
                  "Parameter" = Char_Name)

  # Dropped data summary ----------------------------------------------------

  stations_AWQMS <- bind_rows(stations_AWQMS, missing_au)
  
  drop_summary <- drop_summary %>% 
    dplyr::select(-OrgID) %>%
    dplyr::left_join(stations_AgWQMA, by="MLocID") %>%
    dplyr::left_join(stations_AWQMS, by = "MLocID") %>% 
    dplyr::select('Station ID' = MLocID,
                  'Station Name' = StationDes,
                  "Subbasin Name" = HUC8_Name,
                  HUC8,
                  'Agricultural Water Quality Management Area'=PlanNameST,
                  'Assessment Unit ID' = AU_ID,
                  "Latitude" = Lat_DD,
                  "Longitude" = Long_DD,
                  "Parameter" = Char_Name,
                  "Sampling Organization ID" = OrgID,
                  "Missing AU ID" = missing_au,
                  "No Available Data" = no_data,
                  "Data Start" = min_date,
                  "Data End" = max_date,
                  "Low Grade Observations" = low_grade,
                  "Observations Missing Timestamp" = missing_datetime
    )
  
  colnames(drop_summary) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(drop_summary), perl = TRUE)
  colnames(drop_summary) <- gsub("_", " ", colnames(drop_summary), perl = TRUE)
  colnames(drop_summary) <- sapply(colnames(drop_summary), simpleCap, USE.NAMES = FALSE)

  # Unassessed status reasoning ---------------------------------------------

  status_reason <- status_reason %>%
    dplyr::left_join(stations_AgWQMA, by="MLocID") %>%
    dplyr::left_join(stations_AWQMS, by = "MLocID") %>% 
    dplyr::select('Station ID' = MLocID,
                  'Station Name' = StationDes,
                  "Subbasin Name" = HUC8_Name,
                  HUC8,
                  'Agricultural Water Quality Management Area'=PlanNameST,
                  'Assessment Unit ID' = AU_ID,
                  "Latitude" = Lat_DD,
                  "Longitude" = Long_DD,
                  "Parameter" = Char_Name,
                  "Sampling Organization ID" = OrgID,
                  "Status Period" = status_period,
                  status,
                  reason
    )
  
  colnames(status_reason) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(status_reason), perl = TRUE)
  colnames(status_reason) <- gsub("_", " ", colnames(status_reason), perl = TRUE)
  colnames(status_reason) <- sapply(colnames(status_reason), simpleCap, USE.NAMES = FALSE)
  
  # Creating appendices -------------------------------------------
  
  print("Creating Appendices...")
  
  xlsx_list <- list(Notes, param_sum_stn, param_sum_au, excur_stats_all, seaKen, owri_summary, org_sums, station_sums, drop_summary, status_reason)
  names(xlsx_list) <- c("Notes",
                        paste0("Table_",a.letter,"1_Station_Summary"),
                        paste0("Table_",a.letter,"2_AU_Summary"),
                        paste0("Table_",a.letter,"3_Excursions"),
                        paste0("Table_",a.letter,"4_Trend_Stats"),
                        paste0("Table_",a.letter,"5_OWRI_Summary"),
                        paste0("Table_",a.letter,"6_Results_by_Org"),
                        paste0("Table_",a.letter,"7_Results_by_Year"),
                        paste0("Table_",a.letter,"8_Dropped_Data_Summary"),
                        paste0("Table_",a.letter,"9_Status_Reason"))
  
  openxlsx::write.xlsx(xlsx_list,
                       file=paste0(output_dir, "/", xlsx_name),
                       colWidths="auto",
                       firstActiveRow=c(12,2,2,2,2,2,2,2,2,2), 
                       firstRow=c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
                       rowNames=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), borders="rows",
                       startCol=c(2,1,1,1,1,1,1,1,1,1), startRow=c(11,1,1,1,1,1,1,1,1,1),
                       headerStyle=openxlsx::createStyle(fgFill = "#000000", halign = "LEFT", textDecoration = "Bold",
                                                         wrapText = TRUE, border = "Bottom", fontColour = "white",
                                                         fontName = "Arial", fontSize = 10))
  
  wb <- openxlsx::loadWorkbook(file=paste0(output_dir, "/", xlsx_name), isUnzipped = FALSE)
  
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial")
  
  for (sheet_name in names(xlsx_list[2:10])) {
    
    openxlsx::setColWidths(wb, sheet=sheet_name, cols=c(1:ncol(xlsx_list[[sheet_name]])), widths = "auto")
    
    openxlsx::addStyle(wb, sheet=sheet_name, 
                       rows=c(1:nrow(xlsx_list[[sheet_name]])), 
                       cols=c(1:ncol(xlsx_list[[sheet_name]])),
                       stack=TRUE, gridExpand = TRUE,
                       style=openxlsx::createStyle(valign = "top", fontName = "Arial", fontSize = 10))
    
  }
  
  openxlsx::setColWidths(wb, sheet="Notes", cols=c(2:3), widths = c(16,100))
  
  openxlsx::addStyle(wb, sheet="Notes", rows=c(11:20), cols=c(2:3), stack=TRUE, gridExpand = TRUE,
                     style=openxlsx::createStyle(wrapText = TRUE, valign = "top", fontName = "Arial", fontSize = 10))
  
  openxlsx::writeData(wb, sheet="Notes", x=report_name, 
                      startRow = 3, startCol = 3, rowNames = FALSE)
  
  openxlsx::writeData(wb, sheet="Notes", x=paste0("Appendix ",a.letter,": Tabular Results for the ",name), 
                      startRow = 4, startCol = 3, rowNames = FALSE)
  
  openxlsx::addStyle(wb, sheet="Notes", rows=c(3:4), cols=3, stack=TRUE, 
                     style=openxlsx::createStyle(textDecoration="bold", fontName = "Arial", fontSize = 10))
  
  openxlsx::insertImage(wb, sheet="Notes", logo, startRow = 2,  startCol = 2, 
                        width = 1.07, height = 1.57, units="in")
  
  openxlsx::saveWorkbook(wb, file=paste0(output_dir, "/", xlsx_name), overwrite = TRUE)
  
}
