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

catchments$PET_mm_annual_priestly <- catchments$PET_mm_hr_priestly * 8760
catchments$PET_mm_annual_penman   <- catchments$PET_mm_hr_penman   * 8760


# List of variables for dropdown
variables <- c('LWin', 'SWin', 'Tair_C', 'RH', 'wind_ms', 
               'TP_mmhr', 'Sf_mmhr', 'Rf_mmhr', 'Tsurf', 'LWout', 'LWnet', 'SWnet', 
               'Rnet_Wm2', 'Rnet_MJm2hr', 'PET_mm_hr_penman', 'delta', 'PET_mm_hr_priestly', 'Annual_TP_mm', 'PET_mm_annual_priestly', 'PET_mm_annual_penman')
               
               variable_labels <- c(
  'LWin' = "Incoming Longwave Radiation [W/m²]",
  'SWin' = "Incoming Shortwave Radiation [W/m²]",
  'Tair_C' = "Air Temperature [°C]",
  'RH' = "Relative Humidity [%]",
  'wind_ms' = "Wind Speed [m/s]",
  'TP_mmhr' = "Total Precipitation [mm/hr]",
  'Sf_mmhr' = "Snowfall [mm/hr]",
  'Rf_mmhr' = "Rainfall [mm/hr]",
  'Tsurf' = "Surface Temperature [°C]",
  'LWout' = "Outgoing Longwave Radiation [W/m²]",
  'LWnet' = "Net Longwave Radiation [W/m²]",
  'SWnet' = "Net Shortwave Radiation [W/m²]",
  'Rnet_Wm2' = "Net Radiation [W/m²]",
  'Rnet_MJm2hr' = "Net Radiation [MJ/m²/hr]",
  'PET_mm_hr_penman' = "Penman PET [mm/hr]",
  'delta' = "Clausius-Clapeyron Slope [kPa/°C]",
  'PET_mm_hr_priestly' = "Priestly-Taylor PET [mm/hr]",
  'Annual_TP_mm' = "Annual Precipitation [mm/year]",
  'PET_mm_annual_priestly' = "Annual PET (Priestly-Taylor) [mm/year]",
  'PET_mm_annual_penman' = "Annual PET (Penman) [mm/year]"
)


# UI
ui <- fluidPage(
  titlePanel("Baluchistan Climate Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_var", "Select Forcing Variable:", choices = variable_labels)

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
      fillColor = ~pal(.data[[input$selected_var]]),
      fillOpacity = 0.8,
      color = "#444444",
      weight = 1,
      popup = ~paste0(variable_labels[[input$selected_var]], ": ", round(.data[[input$selected_var]], 2))
    ) %>%
    addLegend("bottomright", pal = pal, values = ~.data[[input$selected_var]],
              title = variable_labels[[input$selected_var]])
})

}

# Run app
shinyApp(ui, server)






