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
  
  # ONE OBSERVE - handles everything
  observe({
    req(input$state_select)
    
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("state_focus") %>% clearGroup("melanoma") %>% clearGroup("uv")
    
    if (input$state_select == "All states (USA)") {
      proxy %>% flyTo(lng = -98.5, lat = 39.8, zoom = 4)
    } else {
      sel <- states_sf[states_sf$NAME == input$state_select, ]
      if (nrow(sel) > 0) {
        bb <- sf::st_bbox(sel)
        proxy %>%
          addPolygons(data = sel, fill = FALSE, color = "#4169E1", weight = 2, opacity = 1, group = "state_focus") %>%
          fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]], lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
      }
      
      state_abbr <- state.abb[match(input$state_select, state.name)]
      if (is.na(state_abbr) && input$state_select == "District of Columbia") state_abbr <- "DC"
      
      state_counties <- counties_sf[counties_sf$STUSPS == state_abbr, ]
      
      if ("Melanoma by County" %in% input$viz_options) {
        counties_melanoma <- state_counties %>%
          left_join(melanoma_table[, c("fips_melanoma", "avg_annual_ct")], by = c("GEOID" = "fips_melanoma")) %>%
          filter(!is.na(avg_annual_ct))
        
        if (nrow(counties_melanoma) > 0) {
          pal <- colorBin(palette = "YlOrRd", domain = counties_melanoma$avg_annual_ct, 
                          bins = c(0, 10, 25, 50, 100, 250, 500, Inf))
          
          proxy %>%
            addPolygons(data = counties_melanoma, fillColor = ~pal(avg_annual_ct), weight = 1,
                        color = "white", fillOpacity = 0.7, group = "melanoma",
                        label = ~paste0(NAME, ": ", avg_annual_ct, " cases/year"))
        }
      }
      
      if ("UV Measurement (wmh2)" %in% input$viz_options) {
        counties_uv <- state_counties %>%
          left_join(uv_table[, c("fips_uv", "uv_value", "uv_category")], by = c("GEOID" = "fips_uv")) %>%
          filter(!is.na(uv_value))
        
        if (nrow(counties_uv) > 0) {
          for (i in 1:nrow(counties_uv)) {
            county <- counties_uv[i, ]
            dash <- if (is.na(county$uv_category)) "0" else if (county$uv_category == "low") "10,10" else if (county$uv_category == "medium") "5,5" else "2,2"
            proxy %>% addPolygons(data = county, fillColor = "transparent", weight = 3, color = "#FF6B00",
                                  dashArray = dash, fillOpacity = 0, group = "uv",
                                  label = paste0(county$NAME, ": UV ", round(county$uv_value, 1)) %>% lapply(htmltools::HTML))
          }
        }
      }
    }
  })
  
  output$data_table <- renderReactable({
    req(melanoma_table)
    reactable(melanoma_table, searchable = TRUE, filterable = TRUE, sortable = TRUE, 
              pagination = TRUE, defaultPageSize = 10)
  })
  
  output$data_summary <- renderText({
    paste0("Rows: ", nrow(melanoma_table), " | Columns: ", ncol(melanoma_table))
  })
})