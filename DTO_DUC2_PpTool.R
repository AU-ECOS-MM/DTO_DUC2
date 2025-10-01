library(shiny)          # For the app
library(leaflet)        # For the app format
library(leaflet.extras) # Ditto
library(mapedit)        # For making the map
library(mapview)        # Making the map
library(dplyr)          # Data Org
library(lubridate)      # Date org
library(Rmisc)          # Plotting functions
library(ggpubr)         # Plotting functions
library(bslib)          ## ?????
library(stringr)        # Data org
library(suntools)       # Diel plot functions for sunrise/sunset
library(ggplot2)        # Plotting


##Functions
source('DPHpDPDplot.R')
source('DiurnalPlot.R')
load('DTO_DUC2_PpData.Rdata')


ui <- page_fillable(
  
  layout_columns(
    card(card_header("Map"), 
         editModUI("editor"),
         width = "100%",
         height = "100%",
    ),
    navset_card_tab(
      nav_panel(title="Detection Positive Hours per Day",
                plotOutput('dph_plot', height = '600px')),
      nav_panel(title = 'Daily Distribution of Detections',
                plotOutput('diurnal_plot', height = '600px')),
      nav_panel(title = 'Pressure Curve?', p('TBD'))
    ),
    
    card(card_header("Station Details"),
         tableOutput("station_table")
    ),
    card(card_header('Settings'),
         sliderInput('YMselect',
                     'Dates:',
                     min = as.Date('2015-01-01', '%Y-%m-%d'),
                     max = as.Date(Sys.Date(), '%Y-%m-%d'),
                     value=c(as.Date('2015-01-01', '%Y-%m-%d'),as.Date(Sys.Date(), '%Y-%m-%d'))),
         imageOutput('logo'),
    ), ## Update this if older data is incorporated
    
    col_widths = c(6,6,8,4),
    row_heights = c(3,1)
  )
  
  
)


server <- function(input, output, session) {
  # edit module returns sf
  edits <- callModule(editMod, "editor", mapview(loc_sf)@map)
  
  # Reactive expression for selected points
  selected_points <- reactive({
    req(edits()$finished)
    st_intersection(edits()$finished, loc_sf)
  })
  
  # Reactive expression for filtered data
  
  selected_data <- reactive({
    req(selected_points())
    selected_groups <- selected_points()$Station
    data=data %>%
      filter(Station %in% selected_groups) %>%
      select(Station, datetime, PPM)
    
    if (length(unique(data$Station)) < 5) {
      showNotification("Fewer than 5 stations selected. Data is not representative.", type = "warning")
    }
    
    data
  })
  
  filtered_data <- reactive({
    req(selected_data())
    data = selected_data() %>%
      
      filter(datetime >= input$YMselect[1],
             datetime <= input$YMselect[2])
    
    if (length(unique(data$Station)) < 5) {
      showNotification("Fewer than 5 stations selected. Data is not representative.", type = "warning")
    }
    
    data
    
    
  })
  
  selectLocs <- reactive({
    req(filtered_data())
    selected_groups <- unique(filtered_data()$Station)
    locations[locations$Station %in% selected_groups, ]
  })
  


  
  # Table output for station details
  output$station_table <- renderTable({
    req(selectLocs())
    
    selectLocs() %>%
      select(Station, long, lat) %>%
      mutate(
        
        long = format(round(long, 4), nsmall = 4),
        lat = format(round(lat, 4), nsmall = 4)
        
      )
    
    
  })
  
  
  # Example output: print selected data structure
  output$dph_plot <- renderPlot({
    req(filtered_data())
    #print(str(selected_data()))
    DPHpDPDplot(filtered_data())
    
  })
  
  output$diurnal_plot <- renderPlot({
    req(filtered_data())
    req(selectLocs)
    #print(str(selected_data()))
    diurnalPlot(filtered_data(), selectLocs())
    
  })
  
  output$logo <- renderImage({
    list(src = 'AU logo.png', height= '5%')
  }, deleteFile = FALSE)
}

shinyApp(ui, server)

