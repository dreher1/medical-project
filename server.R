library(shiny)
library(markdown)
library(leaflet)
library(reactable)
library(sf)
library(tigris)
library(dplyr)
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
              Darkest = Highest UV + melanoma rate within white populations (US-wide)</p>
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
}) 

# End of shinyServer