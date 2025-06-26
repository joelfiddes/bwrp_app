library(shiny)
library(sf)
library(leaflet)
library(dplyr)

# Load data
data_path <- "./catchments_with_forcing.csv"

# Read the CSV and convert to sf
catchments <- read.csv(data_path)
catchments['Annual_TP_mm'] = catchments['TP_mmhr'] * 24 * 365
catchments <- st_as_sf(catchments, wkt = "geometry")  # replace "geometry" with actual geometry column name

# List of variables for dropdown
variables <- c('LWin', 'SWin', 'Tair_C', 'RH', 'wind_ms', 
               'TP_mmhr', 'Sf_mmhr', 'Rf_mmhr', 'Tsurf', 'LWout', 'LWnet', 'SWnet', 
               'Rnet_Wm2', 'Rnet_MJm2hr', 'PET_mm_hr_penman', 'delta', 'PET_mm_hr_priestly')

# UI
ui <- fluidPage(
  titlePanel("Catchment Forcing Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_var", "Select Forcing Variable:", choices = variables)
    ),
    mainPanel(
      leafletOutput("map", height = "800px")
    )
  )
)

# Server
server <- function(input, output, session) {

  output$map <- renderLeaflet({
    req(input$selected_var)

    pal <- colorNumeric("viridis", domain = catchments[[input$selected_var]], na.color = "transparent")

    leaflet(catchments) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(get(input$selected_var)),
        fillOpacity = 0.8,
        color = "#444444",
        weight = 1,
        popup = ~paste0(input$selected_var, ": ", get(input$selected_var))
      ) %>%
      addLegend("bottomright", pal = pal, values = ~get(input$selected_var),
                title = input$selected_var)
  })

}

# Run app
shinyApp(ui, server)
