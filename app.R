# ==========================================
# Travel Map - Posit Cloud
# Animated route with airplane
# ==========================================

# Force Posit Cloud to use a compatible version of terra
if (Sys.getenv("R_CONFIG_ACTIVE") == "cloud") {
  remotes::install_version("terra", version = "1.7-71", repos = "https://cloud.r-project.org", upgrade = "never")
}

library(shiny)
library(leaflet)
library(dplyr)
library(readr)

# ==========================================
# Plane icon
# ==========================================
plane_icon <- makeIcon(
  iconUrl = "image.png",
  iconWidth = 35,
  iconHeight = 35
)


# ==========================================
# Load data
# ==========================================
trip <- read_delim(
  "trip_with_coordinates.csv",
  delim = ";",
  show_col_types = FALSE
)


# Convert dates
trip$`Initial Data` <- as.Date(
  trip$`Initial Data`,
  format = "%d/%m/%Y"
)

trip$`Final Date` <- as.Date(
  trip$`Final Date`,
  format = "%d/%m/%Y"
)


# Add stop number
trip <- trip |>
  mutate(
    Stop = row_number()
  )


# ==========================================
# Create routes
# ==========================================
routes <- trip |>
  
  mutate(
    next_city = lead(City),
    next_state = lead(State),
    
    next_lat = lead(Latitude),
    next_lon = lead(Longitude),
    
    next_date = lead(`Initial Data`)
  ) |>
  
  filter(
    !is.na(next_city),
    City != next_city
  ) |>
  
  mutate(
    Route = row_number(),
    
    mid_lat = (Latitude + next_lat) / 2,
    mid_lon = (Longitude + next_lon) / 2
  )


# ==========================================
# User Interface
# ==========================================
ui <- fluidPage(
  
  titlePanel(
    "Travel Map"
  ),

  fluidRow(
    
    column(
      2,
      actionButton(
        "play",
        "▶ Play"
      )
    ),
    
    column(
      2,
      actionButton(
        "pause",
        "⏸ Pause"
      )
    ),
    
    column(
      2,
      actionButton(
        "reset",
        "↺ Reset"
      )
    )
    
  ),
  
  
  br(),
  
  
  h4(
    textOutput("status")
  ),
  
  leafletOutput(
    "map",
    height = "750px"
  )
  
)


# ==========================================
# Server
# ==========================================
server <- function(input, output, session) {

  # Current route number
  route_index <- reactiveVal(0)

  # Animation state
  playing <- reactiveVal(FALSE)
  
  # Timer every 2 seconds
  auto_refresh <- reactiveTimer(2000)
  
  # ------------------------------------------
  # Initial map
  # ------------------------------------------
  output$map <- renderLeaflet({
    
    
    leaflet() |>
      addTiles() |>
      # City markers
      addCircleMarkers(
        data = trip,
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 3,
        color = "blue",
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>Stop:</b> ", Stop,
          "<br>",
          "<b>City:</b> ", City,
          " - ", State,
          "<br>",
          "<b>Arrival:</b> ",
          format(`Initial Data`, "%d/%m/%Y"),
          "<br>",
          "<b>Departure:</b> ",
          format(`Final Date`, "%d/%m/%Y"),
          "<br>",
          "<b>Days:</b> ",
          Days
        )
      ) |>

      # Initial airplane position
      addMarkers(
        lng = trip$Longitude[1],
        lat = trip$Latitude[1],
        icon = plane_icon,
        layerId = "plane"
      ) |>

      setView(
        lng = -52,
        lat = -15,
        zoom = 4
      )
  })

  # ------------------------------------------
  # Buttons
  # ------------------------------------------
  observeEvent(input$play, {
    playing(TRUE)
  })
  
  observeEvent(input$pause, {
    playing(FALSE)
  })

  observeEvent(input$reset, {
    playing(FALSE)
    route_index(0)
    leafletProxy("map") |>
      clearShapes() |>
      removeMarker(
        "plane"
      ) |>
      addMarkers(
        lng = trip$Longitude[1],
        lat = trip$Latitude[1],
        icon = plane_icon,
        layerId = "plane"
      )
    output$status <- renderText(
      "Ready to start"
    )
  })

  # ------------------------------------------
  # Animation
  # ------------------------------------------
  observe({
    invalidateLater(10000)
    if (playing()) {
      current <- route_index() + 1

      if (current <= nrow(routes)) {
        # Add next line
        leafletProxy("map") |>
          addPolylines(
            data = routes[current,],
            lng = ~c(Longitude, next_lon),
            lat = ~c(Latitude, next_lat),
            color = "green",
            weight = 2,
            opacity = 0.8
          ) |>
          removeMarker(
            "plane"
          ) |>

          addMarkers(
            lng = routes$next_lon[current],
            lat = routes$next_lat[current],
            icon = plane_icon,
            layerId = "plane"
          )

        output$status <- renderText({
          paste0(
            "Move ",
            current,
            "/",
            nrow(routes),
            ":  ",
            routes$City[current],
            " → ",
            routes$next_city[current],
            "   (",
            format(routes$next_date[current], "%d/%m/%Y"),
            ")"
          )
        })
        
        route_index(current)

      } else {

        playing(FALSE)
        output$status <- renderText(
          "Trip completed!"
        )
      }
    }
  })
}

# ==========================================
# Run app
# ==========================================
shinyApp(
  ui = ui,
  server = server
)
