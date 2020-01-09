library(devtools)
library(dplyr)
# devtools::install_github('donco/odeqstatusandtrends', host = 'https://api.github.com', upgrade='never')
library(odeqstatusandtrends)
library(tidyr)
library(lubridate)
library(openxlsx)
library(rgdal)
library(base64enc)

# Inputs ----

final_output <- TRUE

top_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/2019'
gis_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS'

logo <- "//deqhq1/WQNPS/Status_and_Trend_Reports/Figures/DEQ-logo-color-non-transp71x107.png"

au_names <- read.csv('//deqhq1/WQNPS/Status_and_Trend_Reports/Lookups_Statewide/AssessmentUnits_OR_Dissolve.txt', stringsAsFactors = FALSE)

# ----

HUC_shp <- rgdal::readOGR(dsn = gis_dir, layer = 'Report_Units_HUC08',
                          integer64="warn.loss", verbose = FALSE, stringsAsFactors = FALSE)

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
#name <- "Owyhee"
#name <- "Umatilla-Walla Walla-Willow"

for (name in report_names){

  name_abr <- report_name_abr[[name]]
  a.letter <- appendix_letter[[name]]
  xlsx_name <- gsub(" ", "_", paste0("Appendix_",a.letter,"_",name,"_Results.xlsx"), fixed=TRUE)
  xlsx_name <- gsub("-","_",xlsx_name, fixed=TRUE)

  if(final_output) {
    output_dir <- paste0(top_dir,'/Statewide Report')
  } else {
    output_dir <- paste0(data_dir,'/WQST_2019-',name,'_DRAFT_', eval_date)
  }
  data_dir <- paste0(top_dir,'/2019-', name)
  basin_shp <- HUC_shp[HUC_shp$REPORT %in% name, ]

  stations_AWQMS <- odeqstatusandtrends::get_stations_AWQMS(basin_shp)

  stations_AWQMS$AU_Name <- au_names[match(stations_AWQMS$AU_ID, au_names$AU_ID),
                                     c("AU_Name")]

  load(file = paste0(data_dir, "/", name, "_data_assessed.RData"))
  load(file = paste0(data_dir, "/", name, "_eval_date.RData"))
  load(file = paste0(data_dir, "/", name, "_owri_summary_by_subbasin.RData"))
  load(file = paste0(data_dir, "/", name, "_param_summary_by_AU.RData"))
  load(file = paste0(data_dir, "/", name, "_param_summary_by_station.RData"))
  load(file = paste0(data_dir, "/", name, "_status_trend_excur_stats.RData"))

  if(file.exists(paste0(data_dir, "/", name, "_seaken.RData"))){
    load(file = paste0(data_dir, "/", name, "_seaken.RData"))
  } else {seaKen <- data.frame()}

  # Create summary table ------------------------------------------------------

  print(paste0("Saving parameter summary tables..."))

  if(is.null(seaKen) | nrow(seaKen)==0){
    seaKen <- data.frame(Comment = "There were no stations with data that qualified for trend analysis")
  } else {
    seaKen <- seaKen %>%
      dplyr::left_join(stations_AWQMS, by="MLocID") %>%
      dplyr::select('Station ID' = MLocID,
                    'Station Name' = StationDes,
                    'Subbasin Name' = HUC8_Name,
                    HUC8,
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

  excur_stats <- excur_stats %>%
    dplyr::left_join(stations_AWQMS, by="MLocID") %>%
    dplyr::select('Station ID' = MLocID,
                  'Station Name' = StationDes,
                  "Subbasin Name" = HUC8_Name,
                  HUC8,
                  'Assessment Unit ID' = AU_ID,
                  "Latitude" = Lat_DD,
                  "Longitude" = Long_DD,
                  "Parameter" = Char_Name,
                  sort(grep('results', colnames(excur_stats), value = TRUE)),
                  sort(grep('excursions_n', colnames(excur_stats), value = TRUE)),
                  sort(grep('percent_excursion', colnames(excur_stats), value = TRUE)),
                  sort(grep('excursion_min', colnames(excur_stats), value = TRUE)),
                  sort(grep('excursion_median', colnames(excur_stats), value = TRUE)),
                  sort(grep('excursion_max', colnames(excur_stats), value = TRUE))
    )

  colnames(excur_stats) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(excur_stats), perl = TRUE)
  colnames(excur_stats) <- gsub("_", " ", colnames(excur_stats), perl = TRUE)
  colnames(excur_stats) <- sapply(colnames(excur_stats), simpleCap, USE.NAMES = FALSE)
  colnames(excur_stats) <- gsub(" N ", " n ", colnames(excur_stats), perl = TRUE)

  param_sum_stn <- param_sum_stn %>%
    dplyr::select('Station ID' = MLocID,
                  'Station Name' = StationDes,
                  "Subbasin Name" = HUC8_Name,
                  HUC8,
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

  org_sums <- data_assessed %>%
    dplyr::mutate(Basin = name) %>%
    dplyr::group_by(Org_Name, Basin) %>%
    dplyr::summarise('Unique Stations' = length(unique(MLocID)),
                     'Temperature Results' = sum(Char_Name == "Temperature, water"),
                     'DO Results' = sum(Char_Name == "Dissolved oxygen (DO)"),
                     'pH Results' = sum(Char_Name == "pH"),
                     'TSS Results' = sum(Char_Name == "Total suspended solids"),
                     'TP Results' = sum(Char_Name == "Phosphate-phosphorus"),
                     'E. Coli Results' = sum(Char_Name == "Escherichia coli"),
                     'Fecal Coliform Results' = sum(Char_Name == "Fecal Coliform"),
                     'Enterococcus Results' = sum(Char_Name == "Enterococcus")) %>%
    dplyr::rename('Sampling Organization' = Org_Name)

  station_sums <- data_assessed %>%
    dplyr::mutate(Basin = name,
                  Year = lubridate::year(sample_datetime),
                  Subbasin = stations_AWQMS[match(HUC8, stations_AWQMS$HUC8),]$HUC8_Name) %>%
    dplyr::group_by(MLocID, StationDes, Basin, Subbasin, AU_ID, Char_Name, Year) %>%
    dplyr::summarise(Results = n()) %>%
    tidyr::pivot_wider(names_from = "Year", values_from = "Results") %>%
    dplyr::rename('Station ID' = MLocID, 'Station Name' = StationDes, 'Assessment Unit ID' = AU_ID, "Parameter" = Char_Name)

  Notes <- data.frame(stringsAsFactors=FALSE,
                      Table=c(paste0("Table ",a.letter,"-", seq(1,7, by=1),"      ")),
                      #Sheet=c("Station_Summary","AU_Summary","Excursions","Trend_Stats",
                              #"OWRI_Summary",
                              #"Results_by_Org",
                              #"Results_by_Year"),
                      Description=c(paste0("Water quality status and/or trend results at monitoring stations within the ", 
                                           name,"."),
                                    paste0("Water quality status results for Assessment Units within the ", name,"."),
                                    "Summary of the total number of results, total excursions, the percent excursion, and the minimum, maximum, and median of all excursion values for each monitoring station, parameter, and status period.",
                                    paste0("Seasonal Kendall trend test results for each parameter at monitoring stations within the ",name,"."),
                                    paste0("Summary of cumulative treatment outputs reported to the Oregon Watershed Restoration Inventory within the ", 
                                           name,"."),
                                    paste0("Summary of organizations that collected data in the ", 
                                           name,
                                           ", the number of results used in this analysis, and the number of unique stations monitored."),
                                    "Number of results per year for monitoring stations that fit the criteria to assess status or trends."
                                    ))
  
  # Creating appendices -------------------------------------------

  print("Creating Appendices...")

  xlsx_list <- list(Notes, param_sum_stn, param_sum_au, excur_stats, seaKen, owri_summary, org_sums, station_sums)
  names(xlsx_list) <- c("Notes",
                        paste0("Table_",a.letter,"1_Station_Summary"),
                        paste0("Table_",a.letter,"2_AU_Summary"),
                        paste0("Table_",a.letter,"3_Excursions"),
                        paste0("Table_",a.letter,"4_Trend_Stats"),
                        paste0("Table_",a.letter,"5_OWRI_Summary"),
                        paste0("Table_",a.letter,"6_Results_by_Org"),
                        paste0("Table_",a.letter,"7_Results_by_Year"))

  openxlsx::write.xlsx(xlsx_list,
                       file=paste0(output_dir, "/", xlsx_name),
                       colWidths="auto",
                       firstActiveRow=c(12,2,2,2,2,2,2,2), 
                       firstRow=c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
                       rowNames=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), borders="rows",
                       startCol=c(2,1,1,1,1,1,1,1), startRow=c(11,1,1,1,1,1,1,1),
                       headerStyle=openxlsx::createStyle(fgFill = "#000000", halign = "LEFT", textDecoration = "Bold",
                                                         wrapText = TRUE, border = "Bottom", fontColour = "white",
                                                         fontName = "Arial", fontSize = 10))
  
  wb <- openxlsx::loadWorkbook(file=paste0(output_dir, "/", xlsx_name), isUnzipped = FALSE)
  
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial")
  
  for (sheet_name in names(xlsx_list[2:8])) {
    openxlsx::setColWidths(wb, sheet=sheet_name, cols=c(1:ncol(xlsx_list[[sheet_name]])), widths = "auto")
  
  openxlsx::addStyle(wb, sheet="Notes", rows=c(11:18), cols=c(2:3), stack=TRUE, gridExpand = TRUE,
                     style=openxlsx::createStyle(wrapText = TRUE, valign = "top", fontName = "Arial", fontSize = 10))
  
  openxlsx::setColWidths(wb, sheet="Notes", cols=c(2:3), widths = c(16,100))
  
  openxlsx::writeData(wb, sheet="Notes", x="2019 Oregon Statewide Status and Trend Report", 
                      startRow = 3, startCol = 3, rowNames = FALSE)
  
  openxlsx::writeData(wb, sheet="Notes", x=paste0("Appendix ",a.letter,": Tabular Results for the ",name), 
                      startRow = 4, startCol = 3, rowNames = FALSE)
  
  openxlsx::addStyle(wb, sheet="Notes", rows=c(3:4), cols=3, stack=TRUE, 
                     style=openxlsx::createStyle(textDecoration="bold", fontName = "Arial", fontSize = 10))
  
  openxlsx::insertImage(wb, sheet="Notes", logo, startRow = 2,  startCol = 2, 
                        width = 1.07, height = 1.57, units="in")

  openxlsx::saveWorkbook(wb, file=paste0(output_dir, "/", xlsx_name), overwrite = TRUE)

}
