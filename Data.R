## Loading Packages In
library(shiny)
library(bslib)
library(maps)
library(mapproj)
library(sf)
library(leaflet)
library(ggplot2)
library(arcgis)
library(dplyr)
library(shinyBS)


## Loading in data from ArcGIS Online
statesST_all <- st_read("https://services3.arcgis.com/iuNbZYJOrAYBrPyC/ArcGIS/rest/services/States_forR/FeatureServer/0/query?where=1=1&outFields=*&f=geojson")
## documentation on how to move AK and HI: https://www.r-bloggers.com/2014/11/moving-the-earth-well-alaska-hawaii-with-r/
statesST_noAK <- statesST_all[statesST_all$STUSPS != "AK",]
statesST <- statesST_noAK[statesST_noAK$STUSPS!= "HI",]
## AK STATE FP == 02
## HI STATE FP == 15

########################
####   CDC WONDER   ####
########################
## 2018 ##
Wonder_18_Raw <- st_read("https://services3.arcgis.com/iuNbZYJOrAYBrPyC/ArcGIS/rest/services/CDCWonder2018_ExportFeatures/FeatureServer/0/query?where=1%3D1&objectIds=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=NAME%2C+GEOID%2C+STATEFP%2C+State_Code%2C+Deaths%2C+Population%2C+Crude_Rate&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&collation=&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson")
Wonder_18_Raw$Crude_Rate = as.numeric(as.character(Wonder_18_Raw$Crude_Rate))
Wonder_2018 <- Wonder_18_Raw %>% rename(
  Deaths_2018 = Deaths,
  Population_2018 = Population,
  Crude_Rate_2018 = Crude_Rate
)
## 2019 ##
Wonder_19_Raw <- st_read("https://services3.arcgis.com/iuNbZYJOrAYBrPyC/ArcGIS/rest/services/CDCWonder2019_ExportFeatures/FeatureServer/0/query?where=1%3D1&objectIds=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=NAME%2C+GEOID%2C+STATEFP%2C+State_Code%2C+Deaths%2C+Population%2C+Crude_Rate&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&collation=&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson")
Wonder_19_Raw$Crude_Rate = as.numeric(as.character(Wonder_19_Raw$Crude_Rate))
Wonder_2019 <- Wonder_19_Raw %>% rename(
  Deaths_2019 = Deaths,
  Population_2019 = Population,
  Crude_Rate_2019 = Crude_Rate
)
Wonder_2019_NG <- st_drop_geometry(Wonder_2019)
## 2020 ##
Wonder_20_Raw <- st_read("https://services3.arcgis.com/iuNbZYJOrAYBrPyC/ArcGIS/rest/services/CDCWonder2020_ExportFeatures/FeatureServer/0/query?where=1%3D1&objectIds=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=NAME%2C+GEOID%2C+STATEFP%2C+State_Code%2C+Deaths%2C+Population%2C+Crude_Rate&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&collation=&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson")
Wonder_20_Raw$Crude_Rate = as.numeric(as.character(Wonder_20_Raw$Crude_Rate))
Wonder_2020 <- Wonder_20_Raw %>% rename(
  Deaths_2020 = Deaths,
  Population_2020 = Population,
  Crude_Rate_2020 = Crude_Rate
)
Wonder_2020_NG <- st_drop_geometry(Wonder_2020)
## 2021 ##
Wonder_21_Raw <- st_read("https://services3.arcgis.com/iuNbZYJOrAYBrPyC/ArcGIS/rest/services/CDCWonder2021_ExportFeatures/FeatureServer/0/query?where=1%3D1&objectIds=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=NAME%2C+GEOID%2C+State_Code%2C+STATEFP%2C+MCD___Drug_Alcohol_Induced_Caus%2C+MCD___Drug_Alcohol_Induced_Ca_1%2C+Deaths%2C+Population%2C+Crude_Rate&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&collation=&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson")
Wonder_21_Raw$Crude_Rate = as.numeric(as.character(Wonder_21_Raw$Crude_Rate))
Wonder_2021 <- Wonder_21_Raw %>% rename(
  Deaths_2021 = Deaths,
  Population_2021 = Population,
  Crude_Rate_2021 = Crude_Rate
)
Wonder_2021_NG <- st_drop_geometry(Wonder_2021)
## 2022 ##
Wonder_22_Raw <- st_read("https://services3.arcgis.com/iuNbZYJOrAYBrPyC/ArcGIS/rest/services/CDCWonder2022_ExportFeatures/FeatureServer/0/query?where=1%3D1&objectIds=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=NAME%2C+GEOID%2C+STATEFP%2C+State_Code%2C+Deaths%2C+Population%2C+Crude_Rate&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&collation=&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson")
Wonder_22_Raw$Crude_Rate = as.numeric(as.character(Wonder_22_Raw$Crude_Rate))
Wonder_2022 <- Wonder_18_Raw %>% rename(
  Deaths_2022 = Deaths,
  Population_2022 = Population,
  Crude_Rate_2022 = Crude_Rate
)
Wonder_2022_NG <- st_drop_geometry(Wonder_2022)

merged_Wonder <- Wonder_2018 %>% 
  left_join(Wonder_2019_NG, by = "GEOID") %>%
  left_join(Wonder_2020_NG, by = "GEOID") %>%
  left_join(Wonder_2021_NG, by = "GEOID") %>%
  left_join(Wonder_2022_NG, by = "GEOID")

merge_test <- left_join(Wonder_2018, Wonder_2019_NG, by = "GEOID")

saveRDS(merged_Wonder, "Wonder_Full.rds")

## Loading in data from ArcGIS Online
statesST_all <- st_read("https://services3.arcgis.com/iuNbZYJOrAYBrPyC/ArcGIS/rest/services/States_forR/FeatureServer/0/query?where=1=1&outFields=*&f=geojson")
## documentation on how to move AK and HI: https://www.r-bloggers.com/2014/11/moving-the-earth-well-alaska-hawaii-with-r/
statesST_noAK <- statesST_all[statesST_all$STUSPS != "AK",]
statesST <- statesST_noAK[statesST_noAK$STUSPS!= "HI",]
saveRDS(statesST, "States_noAK_noHI.rds")






statesWikiTest <- read.csv("/Users/maxoreilly/Desktop/opioidDataDashboard/myApp/OODAttempt_oneFile/App-1/us_states_wikipedia_links.csv")
statesMerged <- left_join(statesST, statesWikiTest, by = c("NAME" = "State"))
labels = sprintf("<strong>%s</strong><br/>%g<br/>%g",
                 Wonder_21_Raw$NAME, Wonder_21_Raw$Crude_Rate, Wonder_21_Raw$Deaths) %>% lapply(htmltools::HTML)


land = statesST$AWATER
bin_pal = colorBin("Blues", domain = land)
CR = Wonder_21_Raw$Crude_Rate
bin_pal_CR = colorQuantile("Reds", domain = CR, n = 5)

Deaths = Wonder_21_Raw$Deaths
bin_pal_Deaths = colorQuantile("Reds", domain = Deaths, n = 5)




year <- 2018
ODorCount <- "Overdose Rate"
col_name <- if (ODorCount == "Overdose Rate") {
  paste0("Crude_Rate_", year)
} else {
  paste0("Deaths_", year)
}

test_map_data <- Wonder_Full %>% select(NAME, geometry, all_of(col_name))
#View(test_map_data)
colNames <- colnames(test_map_data)
print(colNames)




