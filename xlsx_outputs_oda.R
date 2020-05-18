
library(dplyr)
library(tidyr)
library(lubridate)
library(rgdal)
library(sf)
library(base64enc)
# devtools::install_github('donco/odeqstatusandtrends', host = 'https://api.github.com', force = TRUE, upgrade='never')
library(odeqstatusandtrends)

# Inputs ----
start.date = "1999-01-01"
end.date = "2018-12-31"

report_name <- "2019 Oregon Statewide Status and Trend Report"

final_output <- TRUE

top_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/2019-Revision'
gis_dir <- '//deqhq1/WQNPS/Status_and_Trend_Reports/GIS'

logo <- "//deqhq1/WQNPS/Status_and_Trend_Reports/Figures/DEQ-logo-color-non-transp71x107.png"

au_names <- read.csv('//deqhq1/WQNPS/Status_and_Trend_Reports/Lookups_Statewide/AssessmentUnits_OR_Dissolve.txt', stringsAsFactors = FALSE)

# ----

complete.years <- c(as.integer(substr(start.date, start = 1, stop = 4)):as.integer(substr(end.date, start = 1, stop = 4)))

HUC_shp <- rgdal::readOGR(dsn = gis_dir, layer = 'Report_Units_HUC08',
                          integer64="warn.loss", verbose = FALSE, stringsAsFactors = FALSE)

agwqma_shp <- sf::st_read(dsn = "//deqhq1/WQNPS/Status_and_Trend_Reports/GIS",
                          layer = "ODA_AgWQMA",
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

#name <- "Willamette"
#name <- "Owyhee"
#name <- "Umatilla-Walla Walla-Willow"

# Remove Columbia River and Snake
report_names <- report_names[c(1,3:16,18:21)]

for (name in report_names){
  
  a.letter <- appendix_letter[[name]]
  xlsx_name <- gsub(" ", "_", paste0("ODA_Appendix_",a.letter,"_",name,"_Results.xlsx"), fixed=TRUE)
  xlsx_name <- gsub("-","_",xlsx_name, fixed=TRUE)
  
  if(final_output) {
    output_dir <- paste0(top_dir,'/Statewide Report/WQST_2019_addon_ODA')
  } else {
    output_dir <- paste0(data_dir,'/WQST_2019-',name,'_DRAFT_', eval_date)
  }
  
  data_dir <- paste0(top_dir,'/2019-', name)
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
    dplyr::select(MLocID, StationDes, Lat_DD, Long_DD, HUC8_Name, HUC8, PlanName, AU_ID) %>%
    sf::st_drop_geometry() %>%
    unique()
  
  load(file = paste0(data_dir, "/", name, "_data_assessed.RData"))
  load(file = paste0(data_dir, "/", name, "_eval_date.RData"))
  load(file = paste0(data_dir, "/", name, "_status_trend_excur_stats.RData"))
  
  bins <- odeqstatusandtrends::status_periods(datetime=data_assessed$sample_datetime, year_range=c(min(complete.years), max(complete.years)), bins_only = TRUE)
  
  df.oda1 <- excur_stats %>%
    dplyr::right_join(status, by=c("MLocID", "Char_Name")) %>%
    dplyr::right_join(stat_summary, by=c("MLocID", "Char_Name", "Spawn_type")) %>%
    dplyr::mutate(Char_Name=ifelse(Char_Name %in% c("Temperature, water", "Dissolved oxygen (DO)"), paste0(Char_Name," ",Spawn_type), Char_Name)) %>%
    dplyr::select(MLocID, Char_Name, contains(bins[1]), -contains("percent")) %>%
    as.data.frame()
  
  colnames(df.oda1) <- gsub(colnames(df.oda1), pattern=paste0("_",bins[1]), replacement = "")
  
  if(!"results_n" %in% names(df.oda1)) {
    df.oda3 <- data.frame(Comment = paste0("There were no stations with results in the ", gsub("_","-",bins[1])," status period."))
  } 
  else {
    df.oda2 <- df.oda1 %>%
      dplyr::rename(status=contains("status")) %>%
      dplyr::mutate(Char_Name=gsub(Char_Name,pattern=" (DO)",replacement = "", fixed=TRUE),
                    status_period=gsub(gsub(bins[1], pattern="status_", replacement = ""),pattern="_", replacement = "-"),
                    excursion_min=dplyr::if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(excursion_min, 1), excursion_min),
                    excursion_median=ifelse(grepl("Temperature|Dissolved oxygen", Char_Name), round(excursion_median, 1), excursion_median),
                    excursion_max=dplyr::if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(excursion_max, 1), excursion_max),
                    min=dplyr::if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(min, 1), min),
                    median=dplyr::if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(median, 1), median),
                    max=dplyr::if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(max, 1), max))
    
    df.oda3 <- df.oda2 %>%
      dplyr::filter(!is.na(results_n)) %>%
      dplyr::mutate(results=dplyr::case_when(Char_Name %in% c(odeqstatusandtrends::AWQMS_Char_Names('TP'), "Total suspended solids") & status=="Unassessed" ~ paste0("Unassessed N=",results_n," (", min,"|", median,"|", max,")"),
                                             (!(Char_Name %in% c(odeqstatusandtrends::AWQMS_Char_Names('TP'), "Total suspended solids") & status=="Unasssed") & excursions_n > 0) ~ paste0(excursions_n,"/",results_n,"; (",excursion_min,"|", excursion_median,"|", excursion_max,")"),
                                             TRUE ~ paste0(excursions_n,"/",results_n))) %>%
      dplyr::select(MLocID, status_period, Char_Name, results) %>%
      tidyr::pivot_wider(names_from=Char_Name, values_from=results) %>%
      dplyr::left_join(stations_AgWQMA, by="MLocID") %>%
      dplyr::select("Station ID" = MLocID,
                    "Station Name" = StationDes,
                    "Agricultural Water Quality Management Area"=PlanName,
                    "Subbasin Name" = HUC8_Name,
                    HUC8,
                    "Assessment Unit ID" = AU_ID,
                    "Latitude" = Lat_DD,
                    "Longitude" = Long_DD,
                    "Status Period"=status_period,
                    "Dissolved Oxygen"=matches("Dissolved oxygen Not_Spawn"),
                    "Dissolved Oxygen Spawning Period"=matches("Dissolved oxygen Spawn"),
                    matches("Escherichia coli"),
                    contains("Entero"),
                    contains("Fecal"),
                    pH,
                    "Temperature Non-Spawning Period"=matches("Temperature, water Not_Spawn"),
                    "Temperature Spawning Period"=matches("Temperature, water Spawn"),
                    matches(odeqstatusandtrends::AWQMS_Char_Names('TP')),
                    matches("Total suspended solids"))
  }
  
  Notes <- data.frame(stringsAsFactors=FALSE,
                      Table=c(paste0("Table ",a.letter,"-", seq(8,8, by=1),"      ")),
                      Description=c(paste0("Station summary describing the number of excursions and total number of results shown as '(excursions/results)' for each parameter during the ",
                                           gsub("_","-",bins[1])," status period. ", 
                                           "If excursions > 0, the '(minimum|median|maximum)' of all excursion result values are provided. If stations have a total phosphorus or total suspended ", 
                                           "solids status of 'Unassessed' due to there being no TMDL targets, the (minimum|median|maximum) values for all results are provided. ",
                                           "All minimum, median, and maximum summary results are in units of mg/L except temperature (degrees Celsius), pH (s.u.), and bacteria indicators (CFU/100mL).")))
  
  
  xlsx_list <- list(Notes, df.oda3)
  
  names(xlsx_list) <- c("Notes",
                        paste0("Table_",a.letter,"8_ODA_Summary"))
  
  openxlsx::write.xlsx(xlsx_list,
                       file=paste0(output_dir, "/", xlsx_name),
                       colWidths="auto",
                       firstActiveRow=c(12,2), 
                       firstRow=c(FALSE,TRUE),
                       rowNames=c(FALSE,FALSE), borders="rows",
                       startCol=c(2,1), startRow=c(11,1),
                       headerStyle=openxlsx::createStyle(fgFill = "#000000", halign = "LEFT", textDecoration = "Bold",
                                                         wrapText = TRUE, border = "Bottom", fontColour = "white",
                                                         fontName = "Arial", fontSize = 10))
  
  wb <- openxlsx::loadWorkbook(file=paste0(output_dir, "/", xlsx_name), isUnzipped = FALSE)
  
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial")
  
  for (sheet_name in names(xlsx_list[2])) {
    
    openxlsx::setColWidths(wb, sheet=sheet_name, cols=c(1:ncol(xlsx_list[[sheet_name]])), widths = "auto")
    
    openxlsx::addStyle(wb, sheet=sheet_name, 
                       rows=c(1:nrow(xlsx_list[[sheet_name]])), 
                       cols=c(1:ncol(xlsx_list[[sheet_name]])),
                       stack=TRUE, gridExpand = TRUE,
                       style=openxlsx::createStyle(valign = "top", fontName = "Arial", fontSize = 10))
    
    openxlsx::setColWidths(wb, sheet="Notes", cols=c(2:3), widths = c(16,100))
    
    openxlsx::addStyle(wb, sheet="Notes", rows=c(11:12), cols=c(2:3), stack=TRUE, gridExpand = TRUE,
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
  
}