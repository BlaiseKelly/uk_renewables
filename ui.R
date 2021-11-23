library(shiny)
library(leaflet)

navbarPage("WATTS THE COST", id="main",
           tabPanel("Map", leafletOutput("re", height=1000)),
           tabPanel("Data", DT::dataTableOutput("data")),
           tabPanel("Read Me",includeMarkdown("readme.md")))