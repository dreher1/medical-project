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
    "Welcome to the Home tab!"
  })
  
  output$viz_plot <- renderPlot({
    plot(cars, main = "Placeholder Plot")
  })
  
  output$viz_note <- renderUI({
    tags$em("Replace with visualizations.")
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  
  
  
  #main code block 
  # SINGLE OBSERVER - monitors both state_select and viz_options
  observe({
    
    proxy <- leafletProxy("map")
    
    # Clear all layers
    proxy %>% 
      clearGroup("state_focus") %>%
      clearGroup("melanoma") %>%
      removeControl("melanoma_legend")
    if (input$state_select == "All states (USA)") {
      proxy %>% flyTo(lng = -98.5, lat = 39.8, zoom = 4)
      
      # Show notification if user tries to check melanoma without selecting a state
      if (input$melanoma_view != "none") {
        showNotification(
          "Please select a specific state to view melanoma data",
          type = "warning",
          duration = 3
        )
      }
      
    } else {
      sel <- states_sf[states_sf$NAME == input$state_select, ]
      if (nrow(sel) > 0) {
        bb <- sf::st_bbox(sel)
        proxy %>%
          addPolygons(data = sel, fill = FALSE, color = "#4169E1", weight = 2, opacity = 1, group = "state_focus") %>%
          fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]], lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
      }
      
      
      # Get state abbreviation
      state_abbr <- state.abb[match(input$state_select, state.name)]
      if (is.na(state_abbr) && input$state_select == "District of Columbia") {
        state_abbr <- "DC"
      }
      
      # Filter counties for selected state
      state_counties <- counties_sf[counties_sf$STUSPS == state_abbr, ]
      
      # Check if melanoma checkbox is checked
      if (input$melanoma_view == "count") {
        
        # Join with melanoma data
        counties_with_data <- state_counties %>%
          left_join(
            melanoma_table[, c("fips_melanoma", "county", "state", "avg_annual_ct")],
            by = c("GEOID" = "fips_melanoma")
          )
        
        # Create color palette
        pal <- colorBin(
          palette = "YlOrRd",
          domain = counties_with_data$avg_annual_ct,
          bins = c(0, 10, 25, 50, 100, 250, 500, Inf),
          na.color = "#F0F0F0"
        )
        
        # Add county polygons
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = ~pal(avg_annual_ct),
            weight = 1,
            opacity = 1,
            color = "white",
            layerId = ~GEOID,
            fillOpacity = 0.7,
            group = "melanoma",
            label = ~ifelse(
              is.na(avg_annual_ct),
              paste0(NAME, " County: No data available"),
              paste0(NAME, " County: ", 
                     ifelse(avg_annual_ct == 0, "≤3", as.character(avg_annual_ct)), 
                     " cases/year")
            ),
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
      
      # INCIDENCE RATE VIEW
      if (input$melanoma_view == "rate") {
        
        counties_with_data <- state_counties %>%
          left_join(
            melanoma_table[, c("fips_melanoma", "county", "state", "age_adj_inc_rate")],
            by = c("GEOID" = "fips_melanoma")
          )
        
        pal <- colorBin(
          palette = "YlOrRd",
          domain = counties_with_data$age_adj_inc_rate,
          bins = c(0, 15, 20, 25, 30, 35, 40, 50, Inf),
          na.color = "#F0F0F0"
        )
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = ~pal(age_adj_inc_rate),
            weight = 1,
            opacity = 1,
            color = "white",
            layerId = ~GEOID,
            fillOpacity = 0.7,
            group = "melanoma",
            label = ~ifelse(
              is.na(age_adj_inc_rate),
              paste0(NAME, " County: Data suppressed (<16 cases)"),
              paste0(NAME, " County: ", round(age_adj_inc_rate, 1), " per 100k (age-adj)")
            ),
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
            values = counties_with_data$age_adj_inc_rate,
            title = "Age-Adjusted<br>Incidence Rate<br>(per 100,000)<br><span style='font-size:9px;'>(Gray = Suppressed)</span>",
            opacity = 0.7,
            layerId = "melanoma_legend"
          )
      }
      
    }  # Close else block
    
    # if ("UV Measurement (wmh2)" %in% input$viz_options) {
    #    counties_uv <- state_counties %>%
    #      left_join(uv_table %>% mutate(fips_uv = sprintf("%05d", as.numeric(fips_uv))) %>% select(fips_uv, uv_value), by = c("GEOID" = "fips_uv")) %>%
    #      filter(!is.na(uv_value))
    # 
    #    if (nrow(counties_uv) > 0) {
    #      # Bin UV into categories right here
    #      counties_uv$uv_category <- cut(
    #        counties_uv$uv_value,
    #        breaks = c(0, 4200, 4800, Inf),
    #        labels = c("low", "medium", "high")
    #      )
    # 
    #      pal_uv <- colorBin(palette = "Blues", domain = counties_uv$uv_value, bins = c(0, 4200, 4800, Inf))
    # 
    #      proxy %>%
    #        addPolygons(
    #          data = counties_uv,
    #          fillColor = ~pal_uv(uv_value),
    #          weight = 1,
    #          color = "blue",
    #          fillOpacity = 0.3,
    #          group = "uv",
    #          label = ~paste0(NAME, ": UV ", round(uv_value, 1))
    #        )
    #    }
    #  }
  })
  
  output$data_table <- renderReactable({
    req(melanoma_table)
    reactable(
      melanoma_table,
      searchable = TRUE,
      filterable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      defaultPageSize = 10
    )
  })
  
  output$data_summary <- renderText({
    paste0("Rows: ", nrow(melanoma_table), " | Columns: ", ncol(melanoma_table))
  })
})