library(shiny)
library(leaflet)
library(rgdal)
library(sf)
library('magrittr')
library('htmlwidgets')
library(stringr)
library(tidyverse)
library(readxl)
library(dplyr)
library(htmltools)
library(broom)
library(rgdal)
library(glue)
library(DT)


merged_map <- readRDS("my_data.rds")
colnames(merged_map)
ui <- bootstrapPage(
  

  navbarPage("Kriminalitätsatlas", id="main",
  
## map panel

  tabPanel("Karte",  
           
    div(class= "outer",
    tags$head(includeCSS("style.css")),
    theme = shinythemes::shinytheme("simplex"),
  
    leafletOutput('map', 
                 # width = '100%',
                  height = 1000),
    absolutePanel(
                top = 100, right = 20, draggable = T, id = 'controls', fixed = T, 
                width = "30%", style = "z-index:500; min-width: 300px;",
                height = "auto",
                h2("Daten zum Kriminalitätsatlas"),
                sliderInput('year',
                            "Jahr auswählen:",
                            2012,
                            2020,
                            value = 2020,
                            sep = ""
                            ),
                selectInput('straftaten', 
                            "Straftat wählen", 
                             merged_map %>% st_drop_geometry() %>%  dplyr::select(-c("Year", "Bezeichnung..Bezirksregion.","file",
                                                     "FID", "spatial_na","spatial_al","spatial_ty",
                                                     "BZR_NAME","PGR_NAME","BEZNAME","DATUM", "SHAPE_Leng",
                                                     "SHAPE_Area", "gml_id")) %>% colnames()
                            
                ),
                
                actionButton("go", "Suche"),
                actionButton("chart", "Grafik"),
                plotOutput("line_graph", height = 200, width = "100%"),
                actionButton("information_button", "Hinweise/Erläuterung")
                
  ) 

                )
 ),
  ## Graph panel 
             
 tabPanel("Grafik", plotOutput("plot", height = 1000)),
## Table panel 
 tabPanel("Daten", DT::dataTableOutput("data"))
  )
)

