library(shiny)
library(markdown)
library(leaflet)
library(reactable)

#ADDED: libs and options for state boundaries
library(sf)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

shinyServer(function(input, output, session) {
  
  # Home
  output$home_text <- renderText({
    "Welcome to the Home tab! Replace this text or reference a Markdown file for content."
  })
  
  # Visualizations
  output$viz_plot <- renderPlot({
    plot(cars, main = "Placeholder Plot: speed vs. dist")
  })
  output$viz_note <- renderUI({
    tags$em("Tip: Replace this with your ggplot or plotly visualizations.")
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  #chatgpt code
  states_sf <- tigris::states(cb = TRUE, year = 2023) |> sf::st_transform(4326)
  keep_names <- c(state.name, "District of Columbia", "Puerto Rico")
  keep_names <- setdiff(keep_names, "Puerto Rico") #added  (match your UI choices exactly)
  states_sf <- states_sf[states_sf$NAME %in% keep_names, ]
  
  #ADDED: sync dropdown choices to actual shapes (prevents name mismatches)
  observe({ #added
    available <- sort(unique(states_sf$NAME)) #added
    current <- if (isTruthy(input$state_select)) input$state_select else "All states (USA)" #added
    updateSelectInput( #added
      session, "state_select",
      choices  = c("All states (USA)", available),
      selected = if (current %in% c("All states (USA)", available)) current else "All states (USA)"
    ) #added
  }) #added
  
  # ADDED: react to dropdown, zoom to selected state and draw outline (hardened)
  observeEvent(input$state_select, { #added
    req(input$state_select) #added
    
    if (input$state_select == "All states (USA)") { #added
      leafletProxy("map") %>% #added
        clearGroup("state_focus") %>% #added
        flyTo(lng = -98.5, lat = 39.8, zoom = 4) #added
      return(invisible()) #added
    } #added
    
    sel <- states_sf[states_sf$NAME == input$state_select, ] #added
    if (nrow(sel) < 1) { #added
      showNotification("State not found in shapes. Check the name.", type = "warning") #added
      return(invisible()) #added
    } #added
    
    bb <- sf::st_bbox(sel) #added
    leafletProxy("map") %>% #added
      clearGroup("state_focus") %>% #added
      addPolygons( #added
        data = sel,
        fill = FALSE,
        color = "#4169E1",
        weight = 2,
        opacity = 1,
        group = "state_focus"
      ) %>% #added
      fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]], #added
                lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])  #added
  }, ignoreInit = TRUE) #added
  
  # ---- Data Explorer ----
  output$data_table <- renderReactable({
    # Safely fetch from global.R
    df <- get0("melanoma_table", inherits = TRUE)
    req(!is.null(df))  # shows nothing until available
    
    reactable(
      df,
      searchable = TRUE,
      filterable  = TRUE,
      sortable    = TRUE,
      pagination  = TRUE,
      highlight   = TRUE,
      striped     = TRUE,
      bordered    = TRUE,
      resizable   = TRUE,
      defaultPageSize = 10,
      theme = reactableTheme(
        borderColor    = "#eee",
        highlightColor = "#f5f5f5"
      )
    )
  })
  
  output$data_summary <- renderText({
    df <- get0("melanoma_table", inherits = TRUE)
    if (is.null(df)) "melanoma_table not found (check global.R path)."
    else paste0("Rows: ", nrow(df), " | Columns: ", ncol(df))
  })
})