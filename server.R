library(shiny)
library(markdown)
library(leaflet)
library(reactable)
library(sf)
library(tigris)
library(dplyr)
options(tigris_use_cache = TRUE, tigris_class = "sf")

shinyServer(function(input, output, session) {
  
  output$home_text <- renderText({
    "Welcome to the Home tab! Replace this text or reference a Markdown file for content."
  })
  
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
  
  observe({
    available <- sort(unique(states_sf$NAME))
    current <- if (isTruthy(input$state_select)) input$state_select else "All states (USA)"
    updateSelectInput(
      session, "state_select",
      choices = c("All states (USA)", available),
      selected = if (current %in% c("All states (USA)", available)) current else "All states (USA)"
    )
  })
  
  # HELPER FUNCTION: Draw melanoma choropleth (reusable!)
  draw_melanoma <- function(state_name) {
    proxy <- leafletProxy("map")
    
    proxy %>% clearGroup("melanoma") %>% removeControl("melanoma_legend")
    
    # Only draw if checkbox is checked AND state is selected
    if (!("Melanoma by County" %in% input$viz_options)) {
      return(invisible())
    }
    
    if (state_name == "All states (USA)") {
      showNotification(
        "Please select a specific state to view melanoma data",
        type = "warning",
        duration = 3
      )
      return(invisible())
    }
    
    # Get state abbreviation
    state_abbr <- state.abb[match(state_name, state.name)]
    if (is.na(state_abbr) && state_name == "District of Columbia") {
      state_abbr <- "DC"
    }
    
    # Filter and join data
    state_counties <- counties_sf[counties_sf$STUSPS == state_abbr, ]
    counties_with_data <- state_counties %>%
      left_join(
        melanoma_table[, c("fips_melanoma", "county", "state", "avg_annual_ct")],
        by = c("GEOID" = "fips_melanoma")
      )
    
    counties_with_data <- counties_with_data[!is.na(counties_with_data$avg_annual_ct), ]
    
    
    # Create color palette
    pal <- colorBin(
      palette = "YlOrRd",
      domain = counties_with_data$avg_annual_ct,
      bins = c(0, 10, 25, 50, 100, 250, 500, Inf),
      na.color = "#808080"
    )
    
    # Draw the choropleth
    proxy %>%
      addPolygons(
        data = counties_with_data,
        fillColor = ~pal(avg_annual_ct),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        group = "melanoma",
        label = ~paste0(NAME, " County: ", avg_annual_ct, " cases/year"),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#665",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = counties_with_data$avg_annual_ct,
        title = "Annual<br>Melanoma Cases<br>(County Level)",
        opacity = 0.7,
        layerId = "melanoma_legend"
      )
  }
  
  # Watch state dropdown
  observeEvent(input$state_select, {
    req(input$state_select)
    
    proxy <- leafletProxy("map")
    
    # Clear everything when state changes
    proxy %>%
      clearGroup("state_focus") %>%
      clearGroup("melanoma") %>%
      removeControl("melanoma_legend")
    
    # Handle "All states" selection
    if (input$state_select == "All states (USA)") {
      proxy %>% flyTo(lng = -98.5, lat = 39.8, zoom = 4)
      return(invisible())
    }
    
    # Draw state outline
    sel <- states_sf[states_sf$NAME == input$state_select, ]
    if (nrow(sel) < 1) {
      showNotification("State not found", type = "warning")
      return(invisible())
    }
    
    bb <- sf::st_bbox(sel)
    proxy %>%
      addPolygons(
        data = sel,
        fill = FALSE,
        color = "#4169E1",
        weight = 2,
        opacity = 1,
        group = "state_focus"
      ) %>%
      fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
                lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
    
    # Draw melanoma if checkbox is checked
    draw_melanoma(input$state_select)
    
  }, ignoreInit = TRUE)
  
  # Watch checkbox - just call the helper function!
  observeEvent(input$viz_options, {
    req(input$state_select)
    draw_melanoma(input$state_select)
  }, ignoreInit = TRUE)
  
  # Data Explorer
  output$data_table <- renderReactable({
    df <- get0("melanoma_table", inherits = TRUE)
    req(!is.null(df))
    
    reactable(
      df,
      searchable = TRUE,
      filterable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      highlight = TRUE,
      striped = TRUE,
      bordered = TRUE,
      resizable = TRUE,
      defaultPageSize = 10,
      theme = reactableTheme(
        borderColor = "#eee",
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