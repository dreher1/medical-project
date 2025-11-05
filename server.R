library(shiny)
library(markdown)
library(leaflet)
library(reactable)
library(sf)
library(tigris)
library(dplyr)
options(tigris_use_cache = TRUE, tigris_class = "sf")

shinyServer(function(input, output, session) {
  
  output$home_text <- renderUI({
    HTML("Welcome to our BIO-185 project on data visualization for Melanoma cases in the United States.
  <br><br>
         <strong><span style='font-size: 24px;'>Overview</span></strong><br><br>
         Below are four examples of invasive melanoma, and an overview of the general development process from the epidermal region of the skin to the fourth stage, which is complete invasion and spread to other organs of the body. The data in this project comes from the NCI (National Cancer Institute) in conjunction with the CDC (CDC.gov).")
  })
  
  output$home_melanoma <- renderUI({
    tags$div(
      style = "display: flex; justify-content: center; align-items: center; gap: 40px; margin: 20px auto;",
      tags$img(src = "cs-Accuracy-Dermoscopic-Criteria-Diagnosis-Melanoma-Situ-600x400.jpg", 
               width = "300px"),
      tags$img(src = "melanoma-stages.jpeg", 
               width = "300px")
    )
  })
  
  output$home_image <- renderUI({
    tags$div(
      style = "display: flex; justify-content: center; align-items: center; gap: 40px; margin: 20px auto;",
      tags$img(src = "National_Cancer_Institute_logo.svg.png", 
               width = "300px"),
      tags$img(src = "CDC_logo.png", 
               width = "300px")
    )
  })
  
  output$home_text2 <- renderUI({
    HTML("<strong><span style='font-size: 24px;'>Available Visualizations</span></strong><br><br>
    On this website, you can view and visualize the following data:
  <br><br>
  <strong>Melanoma Cases by County:</strong> This visualization is created from data collected by the National Cancer Institute. It contains the number of cases (average annual count from the years 2017-2021) for each county. The data is collected on a per-county basis.
  <br><br>
  <strong>Melanoma Rate (Age-Adjusted per 100k):</strong> This visualization shows the number of cases (per 100,000) (age adjusted per the NCI website). The data is also included in the original dataset provided by the NCI. The data is collected on a per-county basis.<br><br>
  <strong>UV Measurement:</strong> This is another choropleth map that shows the intensity of UV (in Watts per square meter) for each county in the choropleth map. The data is taken from the National Institute for Cancer GIS Portal for Patient Research, and contains the average values for intensity from the years 2020-2024.
  <br><br>To view the data, visit the Visualizations tab in the menu bar.")
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
  
  
  # Function to create bivariate colors for Leaflet
  create_bivariate_colors <- function(uv_values, melanoma_values) {
    
    # Remove NAs for quantile calculation
    uv_clean <- uv_values[!is.na(uv_values) & !is.na(melanoma_values)]
    mel_clean <- melanoma_values[!is.na(uv_values) & !is.na(melanoma_values)]
    
    if(length(uv_clean) < 3 || length(mel_clean) < 3) {
      return(rep("#F0F0F0", length(uv_values)))
    }
    
    # Calculate quantile breaks
    uv_breaks <- quantile(uv_clean, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    mel_breaks <- quantile(mel_clean, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    
    # Classify values
    uv_class <- cut(uv_values, breaks = uv_breaks, labels = 1:3, include.lowest = TRUE)
    mel_class <- cut(melanoma_values, breaks = mel_breaks, labels = 1:3, include.lowest = TRUE)
    
    # DkBlue bivariate palette (3x3 matrix)
    # Rows = melanoma (low to high), Cols = UV (low to high)
    color_matrix <- matrix(c(
      "#E8E8E8", "#ACE4E4", "#5AC8C8",  # Low melanoma
      "#DFBFD8", "#A28DA8", "#637994",  # Med melanoma  
      "#BE64AC", "#8C62AA", "#3B4994"   # High melanoma
    ), nrow = 3, byrow = TRUE)
    
    # Map to colors
    colors <- rep("#F0F0F0", length(uv_values))
    for (i in seq_along(colors)) {
      if (!is.na(uv_class[i]) && !is.na(mel_class[i])) {
        row_idx <- as.numeric(mel_class[i])
        col_idx <- as.numeric(uv_class[i])
        colors[i] <- color_matrix[row_idx, col_idx]
      }
    }
    
    return(colors)
  }
  
  
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
      
      
      if (input$melanoma_view == "uv") {
        
        counties_with_data <- state_counties %>%
          left_join(
            uv_table %>% 
              mutate(fips_uv = sprintf("%05d", as.numeric(fips_uv))) %>%
              select(fips_uv, uv_value),
            by = c("GEOID" = "fips_uv")
          )
        
        pal <- colorBin(
          palette = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", 
                      "#FC4E2A", "#E31A1C", "#BD0026", "#800026", "#67001F", "#4D0015", "#33000D"),
          domain = counties_with_data$uv_value,
          bins = c(0, 3400, 3600, 3800, 4000, 4200, 4400, 4600, 4800, 5000, 5200, 5400, 5700, Inf),
          na.color = "#F0F0F0"
        )
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = ~pal(uv_value),
            weight = 1,
            opacity = 1,
            color = "white",
            layerId = ~GEOID,
            fillOpacity = 0.7,
            group = "melanoma",
            label = ~ifelse(
              is.na(uv_value),
              paste0(NAME, " County: No UV data available"),
              paste0(NAME, " County: ", round(uv_value, 1), " W/m²")
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
            values = counties_with_data$uv_value,
            title = "UV Intensity<br>(W/m²)",
            opacity = 0.7,
            layerId = "melanoma_legend"
          )
      }
      # BIVARIATE VIEW (UV × Melanoma Rate)
      if (input$melanoma_view == "bivariate") {
        
        counties_with_data <- state_counties %>%
          left_join(
            melanoma_table %>% select(fips_melanoma, age_adj_inc_rate),
            by = c("GEOID" = "fips_melanoma")
          ) %>%
          left_join(
            uv_table %>% 
              mutate(fips_uv = sprintf("%05d", as.numeric(fips_uv))) %>%
              select(fips_uv, uv_value),
            by = c("GEOID" = "fips_uv")
          )
        
        # Create bivariate colors
        biv_colors <- create_bivariate_colors(
          counties_with_data$uv_value,
          counties_with_data$age_adj_inc_rate
        )
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = biv_colors,
            weight = 1,
            opacity = 1,
            color = "white",
            layerId = ~GEOID,
            fillOpacity = 0.7,
            group = "melanoma",
            label = ~paste0(
              NAME, " County",
              "<br>UV: ", ifelse(is.na(uv_value), "No data", paste0(round(uv_value, 1), " W/m²")),
              "<br>Melanoma: ", ifelse(is.na(age_adj_inc_rate), "Suppressed", 
                                       paste0(round(age_adj_inc_rate, 1), " per 100k"))
            ) %>% lapply(htmltools::HTML),
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#665",
              fillOpacity = 0.9,
              bringToFront = TRUE
            )
          ) %>%
          addControl(
            html = '<div style="background: white; padding: 12px; border: 2px solid #4169E1; border-radius: 5px;">
              <strong style="font-size: 13px;">UV × Melanoma Rate</strong><br>
              <table style="border-collapse: collapse; margin-top: 8px;">
                <tr>
                  <td style="width:28px;height:28px;background:#BE64AC;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#8C62AA;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#3B4994;border:1px solid white"></td>
                  <td rowspan="3" style="writing-mode: vertical-lr; transform: rotate(180deg); padding-left:8px; font-size:11px;">Higher Melanoma →</td>
                </tr>
                <tr>
                  <td style="width:28px;height:28px;background:#DFBFD8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#A28DA8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#637994;border:1px solid white"></td>
                </tr>
                <tr>
                  <td style="width:28px;height:28px;background:#E8E8E8;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#ACE4E4;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#5AC8C8;border:1px solid white"></td>
                </tr>
                <tr>
                  <td colspan="3" style="text-align:center; padding-top:5px; font-size:11px;">Higher UV →</td>
                </tr>
              </table>
            </div>',
            position = "bottomright",
            layerId = "melanoma_legend"
          )
      } 
    }
    
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