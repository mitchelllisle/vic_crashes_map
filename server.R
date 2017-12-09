library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(geojsonio)
library(aws.s3)
library(reshape2)
library(data.table)
library(highcharter)
library(lubridate)
library(stringr)
library(htmltools)

source("R/processing.R")

lga_polygons <- geojsonio::geojson_read("data/suburb_simple_005.geojson", what = "sp")
crash_data <- data.table::fread("data/Crashes_Last_Five_Years.csv")
years <- unique(year(dmy(crash_data$ACCIDENT_DATE)))

pal <- colorFactor(c("#F2B95B", "#D7021D" , "#FA475D", "#4A90E2"), domain = unique(crash_data$SEVERITY))


function(input, output, session) {
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(133.9892578125, -28.110748760633534, 5) %>%
      addProviderTiles(providers$CartoDB, providerTileOptions(minZoom = 5, maxZoom = 13))
  })
  
  observe({
    leafletProxy("map", data = lga_polygons) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        layerId = ~LGA_NAME16,
        weight = 2,
        opacity = 1,
        color = "gray",
        dashArray = "3",
        fillOpacity = 0.1,
        label = ~paste0(LGA_NAME16),
        highlight = highlightOptions(
          weight = 3,
          color = "black",
          dashArray = "",
          fillOpacity = 0,
          bringToFront = FALSE),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
    
    observeEvent(input$map_shape_click$id, {
      year <- input$years
      cat(year,"\n")
      lga <- input$map_shape_click$id
      cat(lga,"\n")
      output$LGA_NAME <- renderText(lga)
      selectedPolygon <- subset(lga_polygons, lga_polygons$LGA_NAME16 == lga)
      map_data <- crash_data %>% filter(year(dmy(ACCIDENT_DATE)) == year) %>% filter(LGA == lga)
      
      incidents_by_day <- incidents_by_day_func(map_data)
      type_percent <- type_percent_func(map_data)
      dayofweek_percent <- day_of_week_func(map_data)
      
    
      leafletProxy("map", data = lga_polygons) %>%
        clearMarkers() %>%
        fitBounds(selectedPolygon@bbox[1], selectedPolygon@bbox[2], selectedPolygon@bbox[3], selectedPolygon@bbox[4]) %>%
        addCircleMarkers(map_data$LONGITUDE, map_data$LATITUDE, 
                         color = ~pal(map_data$SEVERITY),
                         stroke = FALSE, 
                         fillOpacity = 1,
                         label = map_data$SEVERITY)
                         # radius = map_data$TOTAL_PERSONS * 1.5)
      
      output$incidents_by_day_chart <- renderHighchart({
        tryCatch({
          hchart(incidents_by_day, "line", hcaes(x = ACCIDENT_DATE, y = total_incidents_weekly_average), animation = TRUE, color = '#4A90E2', lineWidth = 3) %>%
            hc_xAxis(title = list(enabled = FALSE)) %>%
            hc_yAxis(title = list(enabled = FALSE))
        }, error = function(e){
          blank_chart
        })
      })
      
      output$type_percent_chart <- renderHighchart({
        tryCatch({
          hchart(type_percent, "column", hcaes(x = ACCIDENT_TYPE, y = total_incidents_percent), animation = FALSE, color = '#F5A623') %>%
            hc_xAxis(title = list(enabled = FALSE)) %>%
            hc_yAxis(title = list(enabled = FALSE))
        }, error = function(e){
          blank_chart
        })
      })
      
      output$dayOfWeek_chart <- renderHighchart({
        tryCatch({
          hchart(dayofweek_percent, "column", hcaes(x = DAY_OF_WEEK, y = total_incidents_percent), animation = FALSE, color = '#FA475D') %>%
            hc_xAxis(title = list(enabled = FALSE)) %>%
            hc_yAxis(title = list(enabled = FALSE))
        }, error = function(e){
          blank_chart
        })
      })
      
      blank_data <- data.frame(x = 0, y = 0.01)
      blank_chart <- hchart(blank_data, type = "column", animation = FALSE, color = 'white') %>%
        hc_xAxis(title = list(enabled = FALSE)) %>%
        hc_yAxis(max = 10, min = 0, plotLines = list(
          list(label = list(text = "Insufficient Data", align = "center", style = list(fontSize = '20px', color = "gray")),
               color = "white",
               width = 5,
               value = 5)))
      
    })
    
    output$years <- renderUI({
      selectInput('years', label = "Year", choices = years, selected = "2017")
    })
  })

}
