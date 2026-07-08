# ==========================================
# Travel Map - Pure R Smooth Animation
# ==========================================

library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(purrr)

# ==========================================
# Plane icon
# ==========================================
plane_icon <- makeIcon(
  iconUrl = "image.png",
  iconWidth = 35,
  iconHeight = 35
)

# ==========================================
# Load and Clean Data
# ==========================================
trip <- read_delim(
  "trip_with_coordinates.csv",
  delim = ";",
  show_col_types = FALSE
)

trip <- trip |>
  mutate(
    Initial_Data = as.Date(`Initial Data`, format = "%d/%m/%Y"),
    Final_Date = as.Date(`Final Date`, format = "%d/%m/%Y"),
    Stop = row_number()
  )

# ==========================================
# Generate Smooth Interpolated Frame-by-Frame Routes
# ==========================================
STEPS_PER_ROUTE <- 25 # Higher = smoother but slower; Lower = faster

animated_frames <- list()
frame_counter <- 1

for (i in 1:(nrow(trip) - 1)) {
  start_node <- trip[i, ]
  end_node   <- trip[i + 1, ]
  
  # Linear interpolation for latitudes and longitudes
  lats <- seq(start_node$Latitude, end_node$Latitude, length.out = STEPS_PER_ROUTE)
  lons <- seq(start_node$Longitude, end_node$Longitude, length.out = STEPS_PER_ROUTE)
  
  for (j in 1:STEPS_PER_ROUTE) {
    animated_frames[[frame_counter]] <- tibble(
      Frame = frame_counter,
      Route_Num = i,
      Latitude = lats[j],
      Longitude = lons[j],
      City_From = start_node$City,
      City_To = end_node$City,
      Next_Date = end_node$Initial_Data,
      Is_New_Route = (j == 1) # Marks when a fresh polyline needs to be drawn
    )
    frame_counter <- frame_counter + 1
  }
}

frames_df <- bind_rows(animated_frames)

# ==========================================
# User Interface
# ==========================================
ui <- fluidPage(
  titlePanel("Smooth Travel Map"),
  
  fluidRow(
    column(2, actionButton("play", "▶ Play", class = "btn-success")),
    column(2, actionButton("pause", "⏸ Pause", class = "btn-warning")),
    column(2, actionButton("reset", "↺ Reset", class = "btn-danger"))
  ),
  
  br(),
  h4(textOutput("status")),
  leafletOutput("map", height = "750px")
)

# ==========================================
# Server
# ==========================================
server <- function(input, output, session) {
  
  # Reactive states
  frame_index <- reactiveVal(1)
  playing     <- reactiveVal(FALSE)
  status_text <- reactiveVal("Ready to start")
  
  # ------------------------------------------
  # Render Static Outputs
  # ------------------------------------------
  output$status <- renderText({
    status_text()
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addCircleMarkers(
        data = trip,
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 5,
        color = "#007bc2",
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>Stop:</b> ", Stop, "<br>",
          "<b>City:</b> ", City, " - ", State, "<br>",
          "<b>Arrival:</b> ", format(Initial_Data, "%d/%m/%Y"), "<br>",
          "<b>Departure:</b> ", format(Final_Date, "%d/%m/%Y"), "<br>",
          "<b>Days:</b> ", Days
        )
      ) |>
      addMarkers(
        lng = trip$Longitude[1],
        lat = trip$Latitude[1],
        icon = plane_icon,
        layerId = "plane"
      ) |>
      setView(lng = -52, lat = -15, zoom = 4)
  })
  
  # ------------------------------------------
  # Control Button Observers
  # ------------------------------------------
  observeEvent(input$play, {
    playing(TRUE)
  })
  
  observeEvent(input$pause, {
    playing(FALSE)
  })
  
  observeEvent(input$reset, {
    playing(FALSE)
    frame_index(1)
    status_text("Ready to start")
    
    leafletProxy("map") |>
      clearShapes() |>
      removeMarker("plane") |>
      addMarkers(
        lng = trip$Longitude[1],
        lat = trip$Latitude[1],
        icon = plane_icon,
        layerId = "plane"
      )
  })
  
  # ------------------------------------------
  # Animation Engine (High-Speed Ticks)
  # ------------------------------------------
  # ------------------------------------------
  # Animation Engine (Fixed Line Drawing)
  # ------------------------------------------
  # ------------------------------------------
  # Animation Engine (Line drawn at the end/as a trail)
  # ------------------------------------------
  # ------------------------------------------
  # Animation Engine (Fixed Line Drawing)
  # ------------------------------------------
  observe({
    if (playing()) {
      # Tick every 40 milliseconds for 25 FPS smoothness
      invalidateLater(40, session) 
      
      current_frame <- isolate(frame_index())
      
      if (current_frame <= nrow(frames_df)) {
        row <- frames_df[current_frame, ]
        
        proxy <- leafletProxy("map")
        
        # FIX: Draw the full route line the exact moment the flight leg starts
        if (row$Is_New_Route) {
          start_stop <- trip[row$Route_Num, ]
          end_stop   <- trip[row$Route_Num + 1, ]
          
          proxy |> addPolylines(
            data = NULL,
            lng = c(start_stop$Longitude, end_stop$Longitude),
            lat = c(start_stop$Latitude, end_stop$Latitude),
            color = "#2ca25f", 
            weight = 3, 
            opacity = 0.8
          )
        }
        
        # Instantly update plane to the next micro-coordinate
        proxy |>
          removeMarker("plane") |>
          addMarkers(
            lng = row$Longitude,
            lat = row$Latitude,
            icon = plane_icon,
            layerId = "plane"
          )
        
        # Update text info
        status_text(
          paste0(
            "Flying: ", row$City_From, " → ", row$City_To,
            "   (", format(row$Next_Date, "%d/%m/%Y"), ")"
          )
        )
        
        frame_index(current_frame + 1)
      } else {
        playing(FALSE)
        status_text("Trip completed!")
      }
    }
  })
}

# ==========================================
# Run app
# ==========================================
shinyApp(ui = ui, server = server)
