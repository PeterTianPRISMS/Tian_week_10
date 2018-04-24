#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(tidycensus)
library(leaflet)
library(shiny)
library(acs)
library(sf)
library(githubinstall)
# Add key to .Renviron
Sys.setenv(CENSUS_KEY="ac72b08aafee8c6d8bd69bb713ad3cc244555e64")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

ui <- fluidPage(
   
   # Application title
   titlePanel("American Community Survey Data"),
   br(),
   leafletOutput("map", height = "600px"),
   br(),
   # Sidebar
   sidebarLayout(
     sidebarPanel(

       textInput("stateInput", "State", value = "NJ"),
       radioButtons("statsID", "Statistics",  
                    choices = c("median household income", "median gross rent", "the ratio of them"))),
     mainPanel(
       plotOutput("main_plot")
       
     )
   ),
   titlePanel("American Community Survey")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$map <- renderPlot({
     stats_vec <- switch(input$statsID, 'median household income' = 'B19013_001', 'median gross rent' = 'B25064_001',
                         'the ratio of them' = "B25074_001"
     ) 
     df <- get_acs(
       geography = "tract",
       variables = stats_vec,
       state = input$stateInput,
  
       geometry = TRUE
     )
    
    
    pal <- colorQuantile("Greens", domain = df$estimate, n = 9)
    df %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.5,
                  color = ~ pal(estimate)) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~ estimate,
                title = "Median Household Income",
                opacity = 1)
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

