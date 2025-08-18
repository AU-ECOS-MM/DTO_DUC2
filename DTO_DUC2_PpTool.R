library("geosphere") # contains the distm function, which we'll use to calculate distance given long-lat. 
library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(mapedit)
library(mapview)
library(dplyr)
library(lubridate)
library(Rmisc)
library(ggpubr)
library(bslib)
library(stringer)

##Functions
source('DPHpDPDplot.R')


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
        nav_panel(title = 'Daily Distribution of Detections', p('TBD'))
    )
  ),
  card(card_header("Station Details"),
       tableOutput("station_table")
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
  
  # Reactive expression for filtered dataG
  
  selected_data <- reactive({
    req(selected_points())
    selected_groups <- selected_points()$Group
    dataG %>%
      filter(Group %in% selected_groups) %>%
      select(Group, datetime)
  })
  
  
  # Table output for station details
  output$station_table <- renderTable({
    req(selected_data())
    selected_groups <- unique(selected_data()$Group)
    selectLocs <- locations[locations$Group %in% selected_groups, ]
    
    selectLocs %>%
      select(Group, long, lat) %>%
      mutate(
        
        long = format(round(long, 4), nsmall = 4),
        lat = format(round(lat, 4), nsmall = 4)
        
      )
    
    
  })
  
  
  # Example output: print selected data structure
  output$dph_plot <- renderPlot({
    req(selected_data())
    #print(str(selected_data()))
    DPHpDPDplot(selected_data())

  })
}

shinyApp(ui, server)

