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
  
  observeEvent(input$state_select, {
    req(input$state_select)
    
    proxy <- leafletProxy("map")
    
    #I think code below clears when not selected 
    proxy %>% 
      clearGroup("state_focus") %>% 
      clearGroup("melanoma") %>% 
      removeControl("melanoma_legend")
    
    if (input$state_select == "All states (USA)") {
      proxy %>% flyTo(lng = -98.5, lat = 39.8, zoom = 4)
      return(invisible())
    }
    
    sel <- states_sf[states_sf$NAME == input$state_select, ]
    if (nrow(sel) < 1) {
      showNotification("State not found in shapes. Check the name.", type = "warning")
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
      fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]], #Zooms to correct state
                lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
    
    
    if ("Melanoma by County" %in% input$viz_options) {
      
      state_abbr <- state.abb[match(input$state_select, state.name)]
      if (is.na(state_abbr) && input$state_select == "District of Columbia") {
        state_abbr <- "DC"
      }
      
      state_counties <- counties_sf[counties_sf$STUSPS == state_abbr, ]
      
      counties_with_data <- state_counties %>%
        left_join(
          melanoma_table[, c("fips_melanoma", "county", "state", "avg_annual_ct")],
          by = c("GEOID" = "fips_melanoma")
        )
      
      counties_with_data <- counties_with_data[!is.na(counties_with_data$avg_annual_ct), ]
      
      if (nrow(counties_with_data) > 0) {
        pal <- colorBin(
          palette = "YlOrRd",
          domain = counties_with_data$avg_annual_ct,
          bins = c(0, 10, 25, 50, 100, 250, 500, Inf),
          na.color = "#808080"
        )
        
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
    }
    
  }, ignoreInit = TRUE)
  
  
  
  
  #Watches the checkbox group for changes
  #Runs whenever user checks/unchecks ANY visualization option
  #Is code below not same as above why need both?
  observeEvent(input$viz_options, {
    proxy <- leafletProxy("map")
    
    proxy %>% clearGroup("melanoma") %>% removeControl("melanoma_legend")
    
    if ("Melanoma by County" %in% input$viz_options) {
      
      if (input$state_select == "All states (USA)") {
        showNotification(
          "Please select a specific state to view melanoma data (prevents server overload)", 
          type = "warning",
          duration = 3
        )
        return(invisible())
      }
      
      state_abbr <- state.abb[match(input$state_select, state.name)]
      if (is.na(state_abbr) && input$state_select == "District of Columbia") {
        state_abbr <- "DC"
      }
      
      state_counties <- counties_sf[counties_sf$STUSPS == state_abbr, ]
      
      counties_with_data <- state_counties %>%
        left_join(
          melanoma_table[, c("fips_melanoma", "county", "state", "avg_annual_ct")],
          by = c("GEOID" = "fips_melanoma")
        )
      
      counties_with_data <- counties_with_data[!is.na(counties_with_data$avg_annual_ct), ]
      
      if (nrow(counties_with_data) == 0) {
        showNotification("No melanoma data available for this state", type = "warning")
        return(invisible())
      }
      
      pal <- colorBin(
        palette = "YlOrRd",
        domain = counties_with_data$avg_annual_ct,
        bins = c(0, 10, 25, 50, 100, 250, 500, Inf),
        na.color = "#808080"
      )
      
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
    
  }, ignoreInit = TRUE)
  
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