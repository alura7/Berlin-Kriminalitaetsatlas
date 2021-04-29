#import data and clean it 
library("xlsx")
library(rgdal)
library(sf)
library('scales')
library(stringr)
library(tidyverse)
library(readxl)
library(dplyr)
library(broom)
library(rgdal)





#import data and clean it  from https://www.berlin.de/polizei/service/kriminalitaetsatlas/

xlsx_file <- "Fallzahlen2012-2020.xlsx"
# Probably can combine into 1 line of code - fix later 
sheetlist <- excel_sheets(xlsx_file)
sheetlist_Names <- sheetlist[grepl("Fall.*", sheetlist)]
crime_xlsx <- lapply(sheetlist_Names, function(x) read.xlsx2(xlsx_file, sheetName = x, startRow = 5))

set_col_names <- setNames(crime_xlsx,sheetlist_Names)
#Adding a year to data to help with analyse 
year = c(2012,2013, 2014, 2015, 2016,2017,2018,2019, 2020)
crime_df <- mapply(cbind, set_col_names, "Year"=year, SIMPLIFY=F)
#convert to tibble
crime_tb_list <- lapply(crime_df, function(x) as_tibble(x))
#convert list of tibble to tibble
crime_tb <- tibble(file = sheetlist_Names, data = crime_tb_list) 

crime_tb <- crime_tb %>% unnest(cols = c(data))  %>%
  filter(!grepl("Bezirk.*", Bezeichnung..Bezirksregion.))  %>% group_by(Year) %>% nest()

#look for all observations that contain Bezirk ungewiss and remove they are still recorded in 
#strafen insgesamt

#map for the destriks not as detailed as LOR schüssel map - just the 12 districts from Berlin 
berlin.districts <- st_read("Berlin_Bezirke.shp")
maping_districts <- sf::st_transform(berlin.districts, "+proj=longlat +datum=WGS84") 

#map open data Quelle- https://opendata-esri-de.opendata.arcgis.com/datasets/9418b96664554dbf9f92f94ed2d102cc_0?geometry=12.453%2C52.361%2C14.396%2C52.653

#file <- unzip("LOR_Bezirksregionen_-_Berlin-shp.zip")

#berlin.features <- st_read(file[[3]])
berlin.features <- st_read("Berlin_LOR_Bezirksregionen.shp")
#for leaflet 
maping <- sf::st_transform(berlin.features, "+proj=longlat +datum=WGS84") 
crime_unnested <- crime_tb %>%  unnest(cols = c(data))


crime_unnested[,5:22] <- sapply(crime_unnested[,5:22], as.numeric)

crime_unnested <- crime_unnested %>% 
  # as for some still to determined reason the column rauschgift delikte seperates into two - and need to be reunited 
  mutate(Rauschgiftdelikte = coalesce(Rauschgift.delikte, Rauschgif.tdelikte)) %>% 
  select(-c(Rauschgift.delikte, Rauschgif.tdelikte)) 

# rename the variables for better comprehension
crime_unnested <- crime_unnested %>% dplyr::rename(
  "Straftaten Insgesamt" = "Straftaten...insgesamt.",
  "Straßenraub/Handtaschenraub" = "Straßenraub..Handtaschen.raub",
  "Köperverletzung insgesamt" = "Körper.verletzungen...insgesamt.", 
  "Gefährl. und schwere Köperverletzung" = "Gefährl..und.schwere.Körper.verletzung",
  "Freiheitsberaubung/Nötigung/Bedrohung/Nachstelling"= "Freiheits.beraubung..Nötigung..Bedrohung..Nachstellung",
  "Diebstahl insgesamt"= "Diebstahl...insgesamt.",
  "Diebstahl von Kraftwagen"= "Diebstahl.von.Kraftwagen",
  "Diebstahl an/aus Kfz"= "Diebstahl..an.aus.Kfz",
  "Fahrrad-diebstahl"= "Fahrrad..diebstahl",
  "Wohnraumeinbruch"= "Wohnraum..einbruch",
  "Branddelikte insgesamt"= "Branddelikte...insgesamt.",
  "Brandstiftung"= "Brand..stiftung",
  "Sachbeschädigung insgesamt" = "Sach.beschädigung..insgesamt.",
  "Sachbeschädigung durch Graffiti"= "Sach.beschädigung.durch.Graffiti"
)

# combine the spacial data with the crime data                         
merged_map <- merge(maping, crime_unnested, by.x = "spatial_na" , by.y =  "LOR.Schlüssel..Bezirksregion.", all.x = T)

# save the dataframe                       
saveRDS(merged_map, file = "my_data.rds")






