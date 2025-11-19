library(shiny)
library(markdown)
library(leaflet)
library(reactable)
library(sf)
library(tigris)
library(dplyr)
library(ggplot2)
options(tigris_use_cache = TRUE, tigris_class = "sf")

shinyServer(function(input, output, session) {
  
  # ============================================================================
  # BIVARIATE COLOR FUNCTIONS (using US-wide breaks from global.R)
  # ============================================================================
  
  # Standard Bivariate: UV × Melanoma Rate (no demographic adjustment)
  create_bivariate_colors <- function(uv_values, melanoma_values) {
    
    # Use US-wide breaks (calculated in global.R)
    uv_class <- cut(uv_values, breaks = us_uv_breaks, labels = 1:3, include.lowest = TRUE)
    mel_class <- cut(melanoma_values, breaks = us_melanoma_breaks, labels = 1:3, include.lowest = TRUE)
    
    # Color matrix
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
  
  # Risk-Adjusted Bivariate: UV × Melanoma per White Population
  create_weighted_bivariate_colors <- function(uv_values, melanoma_values, white_pct) {
    
    # Calculate melanoma rate PER WHITE POPULATION
    melanoma_per_white <- melanoma_values / (white_pct / 100)
    melanoma_per_white[white_pct < 10] <- NA  # Suppress if white pop < 10%
    
    # Use US-wide breaks (calculated in global.R)
    uv_class <- cut(uv_values, breaks = us_uv_breaks, labels = 1:3, include.lowest = TRUE)
    mel_class <- cut(melanoma_per_white, breaks = us_melanoma_per_white_breaks, labels = 1:3, include.lowest = TRUE)
    
    # Color matrix
    color_matrix <- matrix(c(
      "#E8E8E8", "#ACE4E4", "#5AC8C8",
      "#DFBFD8", "#A28DA8", "#637994",
      "#BE64AC", "#8C62AA", "#3B4994"
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
  
  # ============================================================================
  # HOME TAB OUTPUTS
  # ============================================================================
  
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
  
  # ============================================================================
  # VISUALIZATION TAB - MAP AND EXPLANATIONS
  # ============================================================================
  
  output$viz_plot <- renderPlot({
    plot(cars, main = "Placeholder Plot")
  })
  
  output$viz_note <- renderUI({
    tags$em("Replace with visualizations.")
  })
  
  output$map <- renderLeaflet({
    
    # Get default state
    default_state <- input$state_select
    if (is.null(default_state)) {
      default_state <- "Alabama"
    }
    
    # Get state boundaries
    sel <- states_sf[states_sf$NAME == default_state, ]
    
    # Get state abbreviation
    state_abbr <- state.abb[match(default_state, state.name)]
    if (is.na(state_abbr) && default_state == "District of Columbia") {
      state_abbr <- "DC"
    }
    
    # Filter counties for default state
    state_counties <- counties_sf[counties_sf$STUSPS == state_abbr, ]
    
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
    
    # Get bounding box
    bb <- sf::st_bbox(sel)
    
    # Create map with data
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = sel, fill = FALSE, color = "#4169E1", weight = 2, 
                  opacity = 1, group = "state_focus") %>%
      addPolygons(
        data = counties_with_data,
        fillColor = ~pal(avg_annual_ct),
        weight = 1, opacity = 1, color = "white",
        layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
        label = ~ifelse(
          is.na(avg_annual_ct),
          paste0(NAME, " County: No data available"),
          paste0(NAME, " County: ", 
                 ifelse(avg_annual_ct == 0, "≤3", as.character(avg_annual_ct)), 
                 " cases/year")
        ),
        highlightOptions = highlightOptions(
          weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright", pal = pal, 
        values = counties_with_data$avg_annual_ct,
        title = "Annual<br>Melanoma Cases<br>(County Level)",
        opacity = 0.7, layerId = "melanoma_legend"
      ) %>%
      fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]], 
                lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
  })
  
  # Dynamic explanation text based on selected visualization
  # Dynamic explanation text based on selected visualization
  output$viz_explanation <- renderUI({
    
    choice <- input$melanoma_view
    
    if (choice == "count") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
         <strong>About Melanoma Cases by County:</strong><br>
         This map displays the average annual count of invasive melanoma cases for each county from 2017-2021. 
         Values of 0 indicate suppressed data (≤3 cases). Data source: National Cancer Institute.
       </div>")
    } else if (choice == "rate") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
         <strong>About Age-Adjusted Incidence Rate:</strong><br>
         This map shows the age-adjusted incidence rate per 100,000 population. Age adjustment accounts for differences 
         in population age distributions. Gray counties indicate suppressed data (<16 cases). Data source: National Cancer Institute.
       </div>")
    } else if (choice == "uv") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
         <strong>About UV Measurement:</strong><br>
         This map displays average UV intensity measured in Watts per square meter (W/m²) for each county from 2020-2024. 
         Higher values indicate greater UV radiation exposure. Data source: National Institute for Cancer GIS Portal.
       </div>")
    } else if (choice == "bivariate") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
         <strong>About Bivariate Mapping:</strong><br>
         This map shows the relationship between UV exposure and melanoma rates simultaneously using a 3×3 color scheme.
         Counties are classified relative to <strong>national averages</strong>, not just within the selected state.
         Counties in the top-right corner (dark blue/purple) have both high UV and high melanoma rates compared to the entire US.
         <em>Note: This map does not account for population demographics.</em>
       </div>")
    } else if (choice == "bivariate_weighted") {
      HTML("<div style='padding: 15px; background-color: white; border: 1px solid #4169E1; border-radius: 8px; margin-top: 10px;'>
       <strong>About Risk-Adjusted Bivariate Mapping:</strong><br>
       This advanced map calculates melanoma rates <strong>per white (non-Hispanic) population</strong>, since melanoma 
       affects white populations 20-30× more than other racial groups. Counties are classified relative to <strong>national rates</strong>.
       <br><br>
       <strong>Why this matters:</strong> The displayed <strong>Rate per 100k White</strong> shows the melanoma burden 
       specifically within the at-risk population. A county with 30 cases per 100k total population and only 30% white 
       population actually has <strong>100 cases per 100k white residents</strong> - indicating a severe problem within 
       that vulnerable group.
       <br><br>
       <strong>Interpretation:</strong> 
       <ul style='margin: 10px 0;'>
         <li><strong>Low white % + High melanoma rate</strong> = Very high rate per white person (dark colors) = Severe localized problem</li>
         <li><strong>High white % + High melanoma rate</strong> = Moderate rate per white person (medium colors) = Expected endemic level</li>
         <li><strong>Darkest areas</strong> = Highest UV exposure combined with highest melanoma rates within white populations, relative to the entire US</li>
       </ul>
       This helps identify counties where melanoma disproportionately affects the white population, which may indicate 
       environmental, behavioral, or screening disparities that warrant targeted public health interventions.
     </div>")
    } else {
      return(NULL)
    }
  })
  
  # ============================================================================
  # MAIN MAP OBSERVER - Updates map based on user selections
  # ============================================================================
  
  observeEvent(c(input$state_select, input$melanoma_view, ignoreNULL=FALSE), {
    req(input$state_select, input$melanoma_view)
    # Force immediate evaluation
    state_val <- input$state_select
    view_val <- input$melanoma_view
    proxy <- leafletProxy("map")
    
    # Clear all existing layers
    proxy %>% 
      clearGroup("state_focus") %>%
      clearGroup("melanoma") %>%
      removeControl("melanoma_legend")
    
    # Zoom to selected state
    sel <- states_sf[states_sf$NAME == input$state_select, ]
    # Get bounding box
    bb <- sf::st_bbox(sel)
    
    # Special handling for Michigan - zoom closer to Lower Peninsula
    if (input$state_select == "Michigan") {
      proxy %>%
        addPolygons(data = sel, fill = FALSE, color = "#4169E1", weight = 2, 
                    opacity = 1, group = "state_focus") %>%
        setView(lng = -85.5, lat = 44.5, zoom = 6)  # Custom view for Michigan
    } else if (input$state_select == "Alaska") {
      proxy %>%
        addPolygons(data = sel, fill = FALSE, color = "#4169E1", weight = 2, 
                    opacity = 1, group = "state_focus") %>%
        setView(lng = -152, lat = 64, zoom = 3.49)
    } else {
      proxy %>%
        addPolygons(data = sel, fill = FALSE, color = "#4169E1", weight = 2, 
                    opacity = 1, group = "state_focus") %>%
        fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]], 
                  lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
    }
      
      # Get state abbreviation
      state_abbr <- state.abb[match(input$state_select, state.name)]
      if (is.na(state_abbr) && input$state_select == "District of Columbia") {
        state_abbr <- "DC"
      }
      
      # Filter counties for selected state
      state_counties <- counties_sf[counties_sf$STUSPS == state_abbr, ]
      
      # ========== MELANOMA CASE COUNT ==========
      if (input$melanoma_view == "count") {
        
        counties_with_data <- state_counties %>%
          left_join(
            melanoma_table[, c("fips_melanoma", "county", "state", "avg_annual_ct")],
            by = c("GEOID" = "fips_melanoma")
          )
        
        pal <- colorBin(
          palette = "YlOrRd",
          domain = counties_with_data$avg_annual_ct,
          bins = c(0, 10, 25, 50, 100, 250, 500, Inf),
          na.color = "#F0F0F0"
        )
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = ~pal(avg_annual_ct),
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~ifelse(
              is.na(avg_annual_ct),
              paste0(NAME, " County: No data available"),
              paste0(NAME, " County: ", 
                     ifelse(avg_annual_ct == 0, "≤3", as.character(avg_annual_ct)), 
                     " cases/year")
            ),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addLegend(
            position = "bottomright", pal = pal, 
            values = counties_with_data$avg_annual_ct,
            title = "Annual<br>Melanoma Cases<br>(County Level)",
            opacity = 0.7, layerId = "melanoma_legend"
          )
      }
      
      # ========== MELANOMA INCIDENCE RATE ==========
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
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~ifelse(
              is.na(age_adj_inc_rate),
              paste0(NAME, " County: Data suppressed (<16 cases)"),
              paste0(NAME, " County: ", round(age_adj_inc_rate, 1), " per 100k (age-adj)")
            ),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addLegend(
            position = "bottomright", pal = pal,
            values = counties_with_data$age_adj_inc_rate,
            title = "Age-Adjusted<br>Incidence Rate<br>(per 100,000)<br><span style='font-size:9px;'>(Gray = Suppressed)</span>",
            opacity = 0.7, layerId = "melanoma_legend"
          )
      }
      
      # ========== UV INTENSITY ==========
      if (input$melanoma_view == "uv") {
        
        counties_with_data <- state_counties %>%
          left_join(
            uv_table %>% 
              mutate(fips_uv = sprintf("%05d", as.numeric(fips_uv))) %>%
              select(fips_uv, uv_value),
            by = c("GEOID" = "fips_uv")
          )
        
        pal <- colorBin(
          palette = c("#FFFFCC", "#FED976", "#FEB24C", "#FD8D3C", 
                      "#E31A1C", "#BD0026", "#800026"),
          domain = counties_with_data$uv_value,
          bins = c(3000, 3400, 3800, 4200, 4600, 5000, 5400, 5800),
          na.color = "#F0F0F0"
        )
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = ~pal(uv_value),
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~ifelse(
              is.na(uv_value),
              paste0(NAME, " County: No UV data available"),
              paste0(NAME, " County: ", round(uv_value, 1), " W/m²")
            ),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addLegend(
            position = "bottomright", pal = pal,
            values = counties_with_data$uv_value,
            title = "UV Intensity<br>(W/m²)",
            opacity = 0.7, layerId = "melanoma_legend"
          )
      }
      
      # ========== BIVARIATE: UV × MELANOMA RATE ==========
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
        
        
        # Create bivariate colors using US-wide breaks
        biv_colors <- create_bivariate_colors(
          counties_with_data$uv_value,
          counties_with_data$age_adj_inc_rate
        )
      
          # Override with pure white for missing data (either UV or melanoma)
        biv_colors[is.na(counties_with_data$age_adj_inc_rate) | is.na(counties_with_data$uv_value)] <- "#FFFFFF"
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = biv_colors,
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~paste0(
              NAME, " County",
              "<br>UV: ", ifelse(is.na(uv_value), "No data", paste0(round(uv_value, 1), " W/m²")),
              "<br>Melanoma: ", ifelse(is.na(age_adj_inc_rate), "Suppressed", 
                                       paste0(round(age_adj_inc_rate, 1), " per 100k"))
            ) %>% lapply(htmltools::HTML),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addControl(
            html = '<div style="background: white; padding: 12px; border: 2px solid #4169E1; border-radius: 5px;">
              <strong style="font-size: 13px;">UV × Melanoma Rate</strong><br>
              <p style="font-size: 10px; margin: 5px 0;">(National scale)</p>
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
               <p style="font-size: 10px; color: #666; margin-top: 8px;">
                Light White = Missing/suppressed data</p>
            </div>',
            position = "bottomright",
            layerId = "melanoma_legend"
          )
      }
      
      # ========== RISK-ADJUSTED BIVARIATE: UV × MELANOMA PER WHITE POP ==========
      if (input$melanoma_view == "bivariate_weighted") {
        
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
          ) %>%
          left_join(
            county_demographics %>%
              select(fips_demo, white_not_h_or_l_pct),
            by = c("GEOID" = "fips_demo")
          )
        
        # Create risk-weighted bivariate colors using US-wide breaks
        biv_colors <- create_weighted_bivariate_colors(
          counties_with_data$uv_value,
          counties_with_data$age_adj_inc_rate,
          counties_with_data$white_not_h_or_l_pct
        )
        
        # Override with pure white for suppressed melanoma data
        biv_colors[is.na(counties_with_data$age_adj_inc_rate)] <- "#FFFFFF"
        
        # Calculate risk score for display
        counties_with_data$risk_score <- counties_with_data$age_adj_inc_rate / 
          (counties_with_data$white_not_h_or_l_pct / 100)
        
        proxy %>%
          addPolygons(
            data = counties_with_data,
            fillColor = biv_colors,
            weight = 1, opacity = 1, color = "white",
            layerId = ~GEOID, fillOpacity = 0.7, group = "melanoma",
            label = ~paste0(
              NAME, " County",
              "<br>UV: ", ifelse(is.na(uv_value), "No data", paste0(round(uv_value, 1), " W/m²")),
              "<br>Melanoma Rate: ", ifelse(is.na(age_adj_inc_rate), "Suppressed", 
                                            paste0(round(age_adj_inc_rate, 1), " per 100k total")),
              "<br>White Pop: ", ifelse(is.na(white_not_h_or_l_pct), "No data",
                                        paste0(round(white_not_h_or_l_pct, 1), "%")),
              "<br><strong>Rate per 100k White: ", ifelse(is.na(risk_score), "N/A",
                                                          paste0(round(risk_score, 1), "</strong>"))
            ) %>% lapply(htmltools::HTML),
            highlightOptions = highlightOptions(
              weight = 2, color = "#665", fillOpacity = 0.9, bringToFront = TRUE
            )
          ) %>%
          addControl(
            html = '<div style="background: white; padding: 12px; border: 2px solid #4169E1; border-radius: 5px;">
              <strong style="font-size: 13px;">Risk-Adjusted Map</strong><br>
              <p style="font-size: 10px; margin: 5px 0;">UV × Melanoma per white pop (National scale)</p>
              <table style="border-collapse: collapse; margin-top: 8px;">
                <tr>
                  <td style="width:28px;height:28px;background:#BE64AC;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#8C62AA;border:1px solid white"></td>
                  <td style="width:28px;height:28px;background:#3B4994;border:1px solid white"></td>
                  <td rowspan="3" style="writing-mode: vertical-lr; transform: rotate(180deg); padding-left:8px; font-size:11px;">Higher Risk →</td>
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
               <p style="font-size: 10px; color: #666; margin-top: 8px;">
                Light White = Missing/suppressed data</p>
            </div>',
            position = "bottomright",
            layerId = "melanoma_legend"
          )
      }
      
    } # End of specific state view
    
  ) # End of observe block
  
  # ============================================================================
  # DATA EXPLORER TAB
  # ============================================================================
  
  # Melanoma table
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
  
  output$download_data <- downloadHandler(
    filename = function() {
      "cleaned_melanoma_table.csv"
    },
    content = function(file) {
      write.csv(melanoma_table, file, row.names = FALSE)
    }
  )
  
  # UV table
  output$uv_table <- renderReactable({
    req(uv_table)
    reactable(
      uv_table,
      searchable = TRUE,
      filterable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      defaultPageSize = 10
    )
  })
  
  output$uv_summary <- renderText({
    paste0("Rows: ", nrow(uv_table), " | Columns: ", ncol(uv_table))
  })
  
  output$download_uv <- downloadHandler(
    filename = function() {
      "cleaned_uv_table.csv"
    },
    content = function(file) {
      write.csv(uv_table, file, row.names = FALSE)
    }
  )
  
  # County demographics table
  output$demographics_table <- renderReactable({
    req(county_demographics)
    reactable(
      county_demographics,
      searchable = TRUE,
      filterable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      defaultPageSize = 10
    )
  })
  
  output$demographics_summary <- renderText({
    paste0("Rows: ", nrow(county_demographics), " | Columns: ", ncol(county_demographics))
  })
  
  output$download_demographics <- downloadHandler(
    filename = function() {
      "cleaned_county_population.csv"
    },
    content = function(file) {
      write.csv(county_demographics, file, row.names = FALSE)
    }
  )
  
  
  
  # ============================================================================
  # STATISTICAL ANALYSIS TAB
  # ============================================================================
  
  analysis_data <- reactive({
    melanoma_table %>%
      left_join(
        uv_table %>% 
          mutate(fips_uv = sprintf("%05d", as.numeric(fips_uv))) %>%
          select(fips_uv, uv_value),
        by = c("fips_melanoma" = "fips_uv")
      ) %>%
      left_join(
        county_demographics %>% 
          select(fips_demo, white_not_h_or_l_pct, black_pct, 
                 hispanic_or_latino_pct, asian_pct),
        by = c("fips_melanoma" = "fips_demo")
      ) %>%
      filter(!is.na(age_adj_inc_rate) & !is.na(uv_value) & !is.na(white_not_h_or_l_pct))
  })
  
  # 1. Correlation summary
  output$correlation_summary <- renderText({
    data <- analysis_data()
    
    cor_uv <- cor(data$uv_value, data$age_adj_inc_rate)
    cor_white <- cor(data$white_not_h_or_l_pct, data$age_adj_inc_rate)
    cor_uv_white <- cor(data$uv_value, data$white_not_h_or_l_pct)
    
    # Partial correlation
    m_melanoma_vs_white <- lm(age_adj_inc_rate ~ white_not_h_or_l_pct, data = data)
    m_uv_vs_white <- lm(uv_value ~ white_not_h_or_l_pct, data = data)
    partial_cor <- cor(residuals(m_melanoma_vs_white), residuals(m_uv_vs_white))
    
    paste0(
      "Sample Size: ", nrow(data), " counties\n\n",
      "=== BIVARIATE CORRELATIONS ===\n",
      "UV vs Melanoma: r = ", round(cor_uv, 3), " (NEGATIVE - spurious!)\n",
      "White % vs Melanoma: r = ", round(cor_white, 3), " (STRONG POSITIVE)\n",
      "UV vs White %: r = ", round(cor_uv_white, 3), " (STRONG NEGATIVE - key confounder!)\n\n",
      "=== PARTIAL CORRELATION (controlling for white %) ===\n",
      "UV vs Melanoma | White %: r = ", round(partial_cor, 3), " (weak but POSITIVE)\n\n",
      "=== KEY INSIGHTS ===\n",
      "1. Crude UV-melanoma correlation is NEGATIVE due to geographic confounding\n",
      "2. UV and white % are strongly inversely related (Northern states paradox)\n",
      "3. After controlling for demographics, UV has POSITIVE but WEAK effect\n",
      "4. White % is ", round(abs(cor_white / cor_uv), 1), "× stronger predictor\n",
      "5. Partial correlation explains only ", round(partial_cor^2 * 100, 1), "% of variance"
    )
  })
  
  # 2. Multiple Regression Comparison
  output$multiple_regression_plot <- renderPlot({
    data <- analysis_data()
    
    m1 <- lm(age_adj_inc_rate ~ uv_value, data = data)
    m2 <- lm(age_adj_inc_rate ~ white_not_h_or_l_pct, data = data)
    m3 <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    m4 <- lm(age_adj_inc_rate ~ uv_value * white_not_h_or_l_pct, data = data)
    
    comparison <- data.frame(
      Model = c("UV Only", "White % Only", "UV + White %", "UV × White %"),
      R_squared = c(summary(m1)$r.squared, summary(m2)$r.squared, 
                    summary(m3)$r.squared, summary(m4)$r.squared),
      AIC = c(AIC(m1), AIC(m2), AIC(m3), AIC(m4))
    )
    
    comparison$Model <- factor(comparison$Model, levels = comparison$Model)
    comparison$Variance_Explained <- comparison$R_squared * 100
    
    ggplot(comparison, aes(x = Model, y = Variance_Explained, fill = Model)) +
      geom_col(color = "black", width = 0.7) +
      geom_text(aes(label = sprintf("R² = %.1f%%\nAIC = %.0f", Variance_Explained, AIC)),
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_manual(values = c("#E74C3C", "#F39C12", "#27AE60", "#2E86AB")) +
      labs(title = "Model Comparison: Variance Explained", 
           subtitle = "Lower AIC = Better fit",
           x = "", y = "Variance Explained (%)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", plot.title = element_text(face = "bold")) +
      ylim(0, max(comparison$Variance_Explained) * 1.2)
  })
  
  # 3. Effect Size Comparison
  output$regression_coefficients <- renderPlot({
    data <- analysis_data()
    
    data_std <- data %>%
      mutate(
        uv_std = scale(uv_value)[,1],
        white_std = scale(white_not_h_or_l_pct)[,1],
        melanoma_std = scale(age_adj_inc_rate)[,1]
      )
    
    m_std <- lm(melanoma_std ~ uv_std + white_std, data = data_std)
    
    coef_data <- as.data.frame(confint(m_std))
    coef_data$Variable <- rownames(coef_data)
    coef_data$Estimate <- coef(m_std)
    colnames(coef_data)[1:2] <- c("CI_lower", "CI_upper")
    
    coef_data <- coef_data[coef_data$Variable != "(Intercept)", ]
    coef_data$Variable <- c("UV Intensity", "White Population %")
    
    ggplot(coef_data, aes(x = Variable, y = Estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                    width = 0.2, linewidth = 1.5, color = "#2E86AB") +
      geom_point(size = 5, color = "#E74C3C") +
      coord_flip() +
      labs(
        title = "Standardized Regression Coefficients (Effect Sizes)",
        subtitle = "1 SD increase in predictor → change in melanoma rate (in SD units)",
        x = "",
        y = "Standardized Coefficient (95% CI)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # 4. Partial Regression Plot
  output$partial_regression <- renderPlot({
    data <- analysis_data()
    
    m_melanoma_vs_white <- lm(age_adj_inc_rate ~ white_not_h_or_l_pct, data = data)
    resid_melanoma <- residuals(m_melanoma_vs_white)
    
    m_uv_vs_white <- lm(uv_value ~ white_not_h_or_l_pct, data = data)
    resid_uv <- residuals(m_uv_vs_white)
    
    plot_data <- data.frame(resid_uv = resid_uv, resid_melanoma = resid_melanoma)
    
    ggplot(plot_data, aes(x = resid_uv, y = resid_melanoma)) +
      geom_point(alpha = 0.4, size = 2, color = "#2E86AB") +
      geom_smooth(method = "lm", color = "#E74C3C", linewidth = 1.5, se = TRUE) +
      labs(
        title = "Partial Regression Plot: UV Effect After Controlling for White %",
        subtitle = "Shows pure UV-melanoma relationship independent of demographics",
        x = "UV Intensity (residuals after removing white % effect)",
        y = "Melanoma Rate (residuals after removing white % effect)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # 5. Model Diagnostics
  output$residual_diagnostics <- renderPlot({
    data <- analysis_data()
    m_full <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    
    par(mfrow = c(2, 2))
    plot(m_full, which = c(1, 2, 3, 5))
    par(mfrow = c(1, 1))
  }, height = 600)
  
  # 6. Model Interpretation
  output$model_interpretation <- renderUI({
    data <- analysis_data()
    m3 <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    
    uv_coef <- coef(m3)["uv_value"]
    white_coef <- coef(m3)["white_not_h_or_l_pct"]
    uv_pval <- summary(m3)$coefficients["uv_value", "Pr(>|t|)"]
    
    HTML(paste0("<div style='padding: 20px; background-color: #D4EDDA; border-left: 5px solid #28A745;'>
    <h4 style='color: #155724;'>✅ What These Results Mean</h4>
    
    <p><strong>Multiple Regression Results:</strong></p>
    <ul>
      <li><strong>UV Effect:</strong> β = ", sprintf("%.5f", uv_coef), 
                " (p ", ifelse(uv_pval < 0.001, "< 0.001", paste0("= ", round(uv_pval, 4))), ")<br>",
                "→ 100 W/m² UV increase = ", sprintf("%.2f", uv_coef * 100), 
                " more cases per 100k</li>
      
      <li><strong>White % Effect:</strong> β = ", sprintf("%.3f", white_coef), " (p < 0.001)<br>",
                "→ 10% white pop increase = ", sprintf("%.2f", white_coef * 10), " more cases per 100k</li>
    </ul>
    
    <p><strong>Effect Size:</strong></p>
    <ul>
      <li>1 SD UV change (", round(sd(data$uv_value)), " W/m²) → ", 
                sprintf("%.2f", uv_coef * sd(data$uv_value)), " cases/100k</li>
      <li>1 SD white % change (", round(sd(data$white_not_h_or_l_pct), 1), "%) → ", 
                sprintf("%.2f", white_coef * sd(data$white_not_h_or_l_pct)), " cases/100k</li>
      <li><strong>White % effect is ", 
                round(abs(white_coef * sd(data$white_not_h_or_l_pct)) / 
                        abs(uv_coef * sd(data$uv_value)), 1), "× larger</strong></li>
    </ul>
    
    <p style='background: #FFF3CD; padding: 10px; border-radius: 5px;'>
    <strong>Conclusion:</strong> UV has a statistically significant positive effect, but white population % 
    dominates county-level melanoma patterns. Demographics > environment in ecological data.</p>
  </div>"))
  })
  
  # 7. Full Regression Output
  output$regression_summary <- renderPrint({
    data <- analysis_data()
    
    cat("=== SIMPLE MODELS ===\n\n")
    
    m1 <- lm(age_adj_inc_rate ~ uv_value, data = data)
    cat("Model 1: UV only\n")
    cat("R² =", round(summary(m1)$r.squared, 4), "\n")
    cat("UV coefficient:", round(coef(m1)[2], 5), "\n\n")
    
    m2 <- lm(age_adj_inc_rate ~ white_not_h_or_l_pct, data = data)
    cat("Model 2: White % only\n")
    cat("R² =", round(summary(m2)$r.squared, 4), "\n")
    cat("White % coefficient:", round(coef(m2)[2], 5), "\n\n")
    
    cat("=== MULTIPLE REGRESSION ===\n\n")
    m3 <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    print(summary(m3))
    
    cat("\n=== INTERACTION MODEL ===\n\n")
    m4 <- lm(age_adj_inc_rate ~ uv_value * white_not_h_or_l_pct, data = data)
    print(summary(m4))
    
    cat("\n=== MODEL COMPARISON ===\n")
    cat("AIC: UV =", round(AIC(m1), 1), "| White% =", round(AIC(m2), 1),
        "| Both =", round(AIC(m3), 1), "| Interaction =", round(AIC(m4), 1), "\n")
  })
  
  # 8. Occupation Analysis
  analysis_data_occupation <- reactive({
    analysis_data() %>%
      left_join(occupation_data %>% select(fips_occupation, outdoor_pct, farming_pct, construction_pct),
                by = c("fips_melanoma" = "fips_occupation")) %>%
      filter(!is.na(outdoor_pct))
  })
  
  output$occupation_correlation <- renderText({
    data <- analysis_data_occupation()
    
    cor_outdoor <- cor(data$outdoor_pct, data$age_adj_inc_rate)
    cor_farming <- cor(data$farming_pct, data$age_adj_inc_rate)
    cor_construction <- cor(data$construction_pct, data$age_adj_inc_rate)
    
    m_melanoma_vs_white <- lm(age_adj_inc_rate ~ white_not_h_or_l_pct, data = data)
    m_outdoor_vs_white <- lm(outdoor_pct ~ white_not_h_or_l_pct, data = data)
    partial_cor <- cor(residuals(m_melanoma_vs_white), residuals(m_outdoor_vs_white))
    
    paste0(
      "Sample Size: ", nrow(data), " counties\n\n",
      "=== BIVARIATE CORRELATIONS ===\n",
      "Total Outdoor %: r = ", round(cor_outdoor, 3), "\n",
      "Farming %: r = ", round(cor_farming, 3), "\n",
      "Construction %: r = ", round(cor_construction, 3), "\n\n",
      "=== PARTIAL CORRELATION (controlling for white %) ===\n",
      "Outdoor % vs Melanoma: r = ", round(partial_cor, 3), "\n\n",
      "Counties with more outdoor workers have LOWER melanoma.\n",
      "Likely reflects healthcare access disparities in rural areas."
    )
  })
  
  output$occupation_regression <- renderPrint({
    data <- analysis_data_occupation()
    
    m1 <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    m2 <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct + outdoor_pct, data = data)
    m3 <- lm(age_adj_inc_rate ~ uv_value * outdoor_pct + white_not_h_or_l_pct, data = data)
    
    cat("=== MODEL COMPARISON ===\n\n")
    cat("Baseline: R² =", round(summary(m1)$r.squared, 4), "| AIC =", round(AIC(m1), 1), "\n")
    cat("+ Outdoor %: R² =", round(summary(m2)$r.squared, 4), "| AIC =", round(AIC(m2), 1), "\n")
    cat("+ Interaction: R² =", round(summary(m3)$r.squared, 4), "| AIC =", round(AIC(m3), 1), "\n\n")
    
    cat("=== MODEL WITH OUTDOOR % ===\n")
    print(summary(m2))
    
    cat("\n=== INTERACTION MODEL ===\n")
    print(summary(m3))
  })
  
  output$occupation_interpretation <- renderUI({
    HTML("<div style='padding: 20px; background-color: #FFF3CD; border-left: 5px solid #FF6B35;'>
    <h4 style='color: #FF6B35;'>🔍 The Outdoor Work Paradox</h4>
    <p><strong>Finding:</strong> Counties with MORE outdoor workers have LOWER melanoma rates</p>
    <p><strong>Explanations:</strong></p>
    <ol>
      <li><strong>Detection bias:</strong> Rural counties lack dermatology access</li>
      <li><strong>Dataset limitation:</strong> Invasive melanoma only (excludes early-stage)</li>
      <li><strong>Healthcare disparities:</strong> Less insurance/screening in rural areas</li>
    </ol>
  </div>")
  })
  
  output$occupation_uv_plot <- renderPlot({
    data <- analysis_data_occupation()
    
    data <- data %>%
      mutate(outdoor_tertile = cut(outdoor_pct, 
                                   breaks = quantile(outdoor_pct, c(0, 1/3, 2/3, 1)),
                                   labels = c("Low", "Medium", "High")))
    
    ggplot(data, aes(x = uv_value, y = age_adj_inc_rate, color = outdoor_tertile)) +
      geom_point(alpha = 0.4, size = 2) +
      geom_smooth(method = "lm", se = TRUE, linewidth = 1.5) +
      scale_color_manual(values = c("#27AE60", "#F39C12", "#E74C3C"), name = "Outdoor Work %") +
      labs(title = "UV × Melanoma by Outdoor Occupation", x = "UV (W/m²)", y = "Melanoma (per 100k)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  output$occupation_sensitivity <- renderPrint({
    data <- analysis_data_occupation()
    
    cat("=== SENSITIVITY ANALYSIS ===\n\n")
    
    high_white <- data %>% filter(white_not_h_or_l_pct > 85)
    m_high_white <- lm(age_adj_inc_rate ~ uv_value + outdoor_pct, data = high_white)
    
    cat("1. HIGH WHITE % (>85%):\n")
    cat("   N =", nrow(high_white), "\n")
    cat("   Outdoor coef:", round(coef(m_high_white)["outdoor_pct"], 4), "\n")
    cat("   p =", format(coef(summary(m_high_white))["outdoor_pct", "Pr(>|t|)"], scientific=TRUE), "\n\n")
    
    high_farming <- data %>% filter(farming_pct > median(farming_pct))
    m_high_farming <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = high_farming)
    
    cat("2. HIGH FARMING (>median):\n")
    cat("   N =", nrow(high_farming), "\n")
    cat("   UV coef:", round(coef(m_high_farming)["uv_value"], 6), "\n")
    cat("   p =", format(coef(summary(m_high_farming))["uv_value", "Pr(>|t|)"], scientific=TRUE), "\n\n")
    
    low_outdoor <- data %>% filter(outdoor_pct < quantile(outdoor_pct, 0.25))
    m_low_outdoor <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = low_outdoor)
    
    cat("3. LOW OUTDOOR (bottom 25%):\n")
    cat("   N =", nrow(low_outdoor), "\n")
    cat("   UV coef:", round(coef(m_low_outdoor)["uv_value"], 6), "\n")
    cat("   p =", format(coef(summary(m_low_outdoor))["uv_value", "Pr(>|t|)"], scientific=TRUE), "\n")
  })
  # 9. ANOVA: UV and White% contributions
  output$anova_table <- renderPrint({
    data <- analysis_data()
    
    # Sequential ANOVA (order matters)
    m_full <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    
    cat("=== TYPE I ANOVA (Sequential) ===\n")
    cat("Tests each variable after those before it\n\n")
    print(anova(m_full))
    
    cat("\n=== TYPE II ANOVA (Marginal) ===\n")
    cat("Tests each variable after all others\n\n")
    if(requireNamespace("car", quietly = TRUE)) {
      print(car::Anova(m_full, type = "II"))
    } else {
      cat("Install 'car' for Type II: install.packages('car')\n")
    }
  })
  
  # 10. Nested Model Comparison
  output$nested_models <- renderPrint({
    data <- analysis_data()
    
    m0 <- lm(age_adj_inc_rate ~ 1, data = data)
    m1 <- lm(age_adj_inc_rate ~ uv_value, data = data)
    m2 <- lm(age_adj_inc_rate ~ white_not_h_or_l_pct, data = data)
    m3 <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    
    cat("=== NESTED MODEL F-TESTS ===\n\n")
    
    cat("1. Does UV improve over intercept-only?\n")
    print(anova(m0, m1))
    
    cat("\n2. Does White% improve over intercept-only?\n")
    print(anova(m0, m2))
    
    cat("\n3. Does adding White% improve UV-only model?\n")
    print(anova(m1, m3))
    
    cat("\n4. Does adding UV improve White%-only model?\n")
    print(anova(m2, m3))
  })
  # 11. Residual Analysis
  output$residual_analysis <- renderPrint({
    data <- analysis_data()
    m <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    
    cat("=== RESIDUAL DIAGNOSTICS ===\n\n")
    cat("Model: melanoma ~ UV + White%\n\n")
    
    resids <- residuals(m)
    
    cat("Residual Summary:\n")
    print(summary(resids))
    
    cat("\n\nShapiro-Wilk Normality Test:\n")
    if(length(resids) <= 5000) {
      print(shapiro.test(resids))
    } else {
      cat("Sample too large, use Q-Q plot instead\n")
    }
    
    cat("\n\nHomoscedasticity (Breusch-Pagan Test):\n")
    if(requireNamespace("lmtest", quietly = TRUE)) {
      print(lmtest::bptest(m))
    } else {
      cat("Install 'lmtest': install.packages('lmtest')\n")
    }
  })
  
  output$residual_plots <- renderPlot({
    data <- analysis_data()
    m <- lm(age_adj_inc_rate ~ uv_value + white_not_h_or_l_pct, data = data)
    
    par(mfrow = c(2, 2))
    plot(m, which = 1:4)
    par(mfrow = c(1, 1))
  }, height = 600)
  
})
  
  