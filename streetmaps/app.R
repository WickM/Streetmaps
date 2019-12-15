#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(osmdata)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Santas Streetmaps"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "xmin",
                         label = "xmin",
                         value = 13.00346,
                         min = 0,
                         max = 90),
            numericInput(inputId = "ymin",
                         label = "ymin",
                         value = 47.73306,
                         min = 0,
                         max = 180),
            numericInput(inputId = "xmax",
                         label = "xmax",
                         value = 13.11145,
                         min = 0,
                         max = 90),
            numericInput(inputId = "ymax",
                         label = "ymax",
                         value = 47.84497,
                         min = 0,
                         max = 180),
            actionButton(inputId = "plot",label = "plot"),
            hr(),
            verbatimTextOutput("value")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("streetmap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$plot, {
        
        cord <- getbb("Salzburg")
        
        cord[1,1] <- input$xmin
        cord[1,2] <- input$xmax
        cord[2,1] <- input$ymin
        cord[2,2] <- input$ymax
        output$value <- renderPrint(cord)
        
        streets <- cord %>%
            opq()%>%
            add_osm_feature(key = "highway",
                            value = c("motorway",
                                      "trunk",
                                      "primary",
                                      "secondary",
                                      "tertiary"
                            )) %>%
            osmdata_sf()

        small_streets <- cord %>%
            opq()%>%
            add_osm_feature(key = "highway",
                            value = c("residential", "living_street",
                                      "unclassified",
                                      "service",
                                      "footway",
                                      "track",
                                      "pedestrian",
                                      "road",
                                      "path",
                                      "cycleway"
                            )) %>%
            osmdata_sf()

        output$streetmap <- renderPlot({
            ggplot() +
            geom_sf(data = streets$osm_lines,
                    inherit.aes = FALSE,
                    color = "#ffbe7f",
                    size = .4,
                    alpha = .8) +
            geom_sf(data = small_streets$osm_lines,
                    inherit.aes = FALSE,
                    color = "#ffbe7f",
                    size = .2,
                    alpha = .6) +
            coord_sf(xlim = c(cord[1,1], cord[1,2]),
                     ylim = c(cord[2,1], cord[2,2]),
                     expand = FALSE) +
            theme_void() +
            theme(
                plot.background = element_rect(fill = "#282828")
            )
         })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
