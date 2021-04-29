#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(rgdal)
library(sf)
library('htmlwidgets')
library(stringr)
library(tidyverse)
library(dplyr)
library(htmltools)
library(broom)
library(purrr)
library(glue)
library(ggplot2)
library("xlsx")
library(readxl)

#uplaod tiddied data 
merged_map <- readRDS("my_data.rds")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  withProgress(
   
    message = 'loading...',
    output$map <- renderLeaflet({
  #map of Berlin before any buttons are pressed 
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>% 
        addPolygons(
          data = maping_districts,
          label = ~paste0(maping_districts$Gemeinde_n),
          smoothFactor = 0.8,
          weight = 2,
          fillOpacity = 0.01,
          color = "#444444"
        ) %>% 
        # adding second Polygons with the distircts further divided into smaller area
        addPolygons(
          data =  merged_map,
          stroke = T,
          label = glue("<strong> <u>Bezirk: </u > </strong> <br />
                        {merged_map$BEZNAME} : {merged_map$Bezeichnung..Bezirksregion.}")  %>% lapply(htmltools::HTML),
          smoothFactor = 0.5,
          fillOpacity = 0.01,
          weight = 0.2,
          color = "#444444",
          highlightOptions = highlightOptions(color = "white", weight = 2,
                                              bringToFront = T)
          )
    })
  )
  # with Suche button - shows map with data regarding the type of crime 
  observeEvent(input$go, {
    withProgress(
      message = 'loading...',
      value = 1/5, {
        straftaten <- input$straftaten
        year <- input$year
        crime_map <-  merged_map %>% filter(Year == year) 
        
        tryCatch({   
          
          incProgress(1/5)
     # creating  a color palette for the  fill opacity setting a range to show the crime rate - 
          min_max_values <-  crime_map  %>% select(straftaten) %>% unlist() %>% as.numeric() %>% range(.,na.rm = TRUE)
          pal <- colorNumeric(palette = "Reds", domain=c(min_max_values[1], min_max_values[2]))
          label_berlin_map <- glue('<strong> <u>Bezirk: </u > </strong> <br /> 
                                    {crime_map$BEZNAME} : {crime_map$Bezeichnung..Bezirksregion.} <br />
                                   <strong> <u>Fallzahlen: </u ></strong> <br /> 
                                   {straftaten} : {crime_map[[straftaten]]}') %>%  lapply(htmltools::HTML)
          
          leafletProxy("map") %>% 
            clearShapes() %>% 
            clearControls() %>% addPolygons(
              data = maping_districts,
              label = ~paste0(maping_districts$Gemeinde_n),
              smoothFactor = 0.8,
              weight = 2,
              fillOpacity = 0.01,
              color = "#444444"
               ) %>% 
            addPolygons(
              data = crime_map,
              stroke = T,
              fillColor = ~pal(as.numeric(crime_map[[straftaten]])),
              label = label_berlin_map,
              smoothFactor = 0.5,
              fillOpacity = 0.6,
              weight =0.5,
              color = "#444444",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = T)
              
            ) 
          
        },
        
        error = function(e) {
          showModal(modalDialog(title = "Sorry!",
                                tags$p("can't find data, try again")))
        }
        )
      }
    )
    
  })
  # Graph display of data using ggplot 
  observeEvent(input$chart, {
    
    straftaten <- input$straftaten
    year <- input$year
    
    
    tryCatch({
      straftaten_insgesamt_graph <- crime_unnested %>% 
        select(Bezeichnung..Bezirksregion.,Year, straftaten) %>% 
        filter(str_detect(Bezeichnung..Bezirksregion., "gesamt")) 
      
      output$line_graph<- renderPlot({
        ggplot(data = straftaten_insgesamt_graph, aes(x = Year, y= straftaten_insgesamt_graph[[straftaten]])) +
          ylab("Fallzahlen") +
          xlab("Jahr") +
          geom_line(color="#69b3a2", size = 1, alpha = 0.8) +
          ggtitle(paste0("Fallzahlen von 2012 bis 2019:\n", straftaten)) +
          theme_bw()
      
      })
    },
    error = function(e) {
      showModal(modalDialog(title = "Sorry!",
                            tags$p("can't find data, try again")))
    }
    )
  })
  # To inform on the data with link to website 
  observeEvent(input$information_button, {
    showModal(modalDialog(title = 'Information',
                          footer = modalButton("Exit"),
                          easyClose = T,
                          "Diese Anwendung erhebt keinen Anspruch auf Vollständigkeit.
                            Die Daten für den Kriminalatlas stammen von: \n 
                          https://www.berlin.de/polizei/service/kriminalitaetsatlas. \n
                          Für mehr Information Bitte klicken Sie auf den", 
                          tags$a(href = 'https://www.berlin.de/polizei/service/kriminalitaetsatlas/', "Link"))
                         
              ) 
  })
}

# Run the application 
#shinyApp(ui = ui, server = server)

