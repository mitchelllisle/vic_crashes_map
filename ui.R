library(leaflet)
library(highcharter)

navbarPage(windowTitle = "Mitchell Lisle | Victoria Traffic Incidents", img(src = "logo.png", height = "20px"), id="nav", collapsible = TRUE,
           tabPanel("Map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        leafletOutput("map", width="65%", height="100%"),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                      draggable = FALSE, top = 00, left = 0, right = 0, bottom = 0,
                                      width = "35%", height = "100%", overflow = "scroll",
                                      h1("Local Governement Area"),
                                      h5(textOutput("LGA_NAME")),
                                      h1("Filters"),
                                      shiny::uiOutput("years"),
                                      p("Incidents by Day (Rolling average)"),
                                      highchartOutput("incidents_by_day_chart", height = "200px"),
                                      p("Type of Incident (%)"),
                                      highchartOutput("type_percent_chart", height = "200px"),
                                      p("Day of Week (%)"),
                                      highchartOutput("dayOfWeek_chart", height = "200px")
                                      )
                        )
                    )
           )
