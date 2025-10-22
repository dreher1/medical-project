library(shiny)
library(markdown)
library(leaflet)
library(reactable)

shinyServer(function(input, output, session) {
  
  # Home
  output$home_text <- renderText({
    "Welcome to the Home tab! Replace this text or reference a Markdown file for content."
  })
  
  # Visualizations (placeholders)
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
  
  # Optional: watch checkbox selections (use to add/remove Leaflet layers later)
  observeEvent(input$viz_options, ignoreInit = TRUE, {
    message("Selected layers: ", paste(input$viz_options, collapse = ", "))
  })
  
  # ---- Data Explorer ----
  output$data_table <- renderReactable({
    df <- get0("melanoma_table", inherits = TRUE)
    req(!is.null(df))
    
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
