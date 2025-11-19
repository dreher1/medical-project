library(shiny)
library(markdown)
library(leaflet)
library(reactable)
library(biscale)
library(ggplot2)
library(cowplot)

# Define custom colors taken from an online template
royal_blue <- "#4169E1" 
light_blue <- "#E8F0FF" 
dark_blue  <- "#1E3A8A"


#state dropdown choices (includes DC)
state_choices <- c(sort(c(state.name, "District of Columbia")))

#create the user interface establishment
shinyUI(
  navbarPage(
    title = "BIOL-185 Project - Melanoma Case Studies",
    id = "main_navbar",
    inverse = TRUE,  # makes navbar dark-themed by default
    
    # Apply a royal blue background color to the navbar
    header = tags$style(HTML(paste0("
      .navbar {
        background-color: ", royal_blue, ";
        border-color: ", royal_blue, ";
      }
      .navbar-brand, .navbar-nav li a {
        color: white !important;
        font-weight: 500;
      }
      .navbar-nav li a:hover {
        background-color: ", dark_blue, " !important;
      }
      body {
        background-color: ", light_blue, ";
        color: #0A0A0A;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      h2, h3 {
        color: ", dark_blue, ";
      }
      .well, .panel {
        background-color: white !important;
        border: 1px solid ", royal_blue, ";
        border-radius: 10px;
        box-shadow: 0 0 8px rgba(0,0,0,0.05);
      }
    "))),
    
    # home tab
    tabPanel(
      "Home",
      fluidPage(
        h2("Home"),
        uiOutput("home_text"),
        uiOutput("home_melanoma"),
        uiOutput("home_image"),
        uiOutput("home_text2")
      )
    ),
    
    # Visualizations tab
    tabPanel(
      "Visualizations",
      fluidPage(
        h2("Visualizations"),
        h3("Interactive Map"),
        
        # State dropdown
        selectInput(
          inputId = "state_select",
          label   = "Focus state:",
          choices = state_choices,
          selected = "All states (USA)",
          width = "300px"
        ),
        
        #check boxes
        radioButtons(
          inputId = "melanoma_view",  # Changed from "viz_options"
          label   = "Show data layers:",
          choices = c(
            "Melanoma by County" = "count",  # Keep your title, add value
            "Melanoma Rate (Age-Adjusted per 100k)" = "rate",  # Keep your title, add value
            "UV Measurement (wmh2)" = "uv",
            "Bivariate: UV × Melanoma Rate" = "bivariate" ,
            "Risk-Adjusted: UV × Melanoma (White Pop Weighted)" = "bivariate_weighted"  # NEW
          ),
          selected = "count"   
        ),
        
        leafletOutput("map", height = "400px"),
        uiOutput("viz_explanation")
      )
    ),
    
    # Data Explorer tab
    tabPanel(
      "Data Explorer",
      fluidPage(
        h2("Data Explorer"),
        
        # Melanoma Table Section
        h3("Melanoma Data"),
        p("Explore the melanoma_table interactively:"),
        reactableOutput("data_table", height = 500),
        br(),
        textOutput("data_summary"),
        downloadButton("download_data", "Download Melanoma Data (CSV)"),
        br(),
        br(),
        
        # UV Table Section
        h3("UV Data"),
        p("Explore the UV data interactively:"),
        reactableOutput("uv_table", height = 500),
        br(),
        textOutput("uv_summary"),
        downloadButton("download_uv", "Download UV Data (CSV)"),
        br(),
        br(),
        
        # County Demographics Table Section
        h3("County Demographics Data"),
        p("Explore the county population data interactively:"),
        reactableOutput("demographics_table", height = 500),
        br(),
        textOutput("demographics_summary"),
        downloadButton("download_demographics", "Download County Population Data (CSV)"),
        br()
      )
    ),
    tabPanel(
      "Statistical Analysis",
      fluidPage(
        h2("Statistical Analysis: Confounding in UV-Melanoma Relationship"),
        p("Examining how demographic confounding masks the true UV-melanoma relationship."),
        
        wellPanel(
          h3("1. Correlation Summary & Confounding Detection"),
          verbatimTextOutput("correlation_summary")
        ),
        
        hr(),
        
        wellPanel(
          h3("2. Multiple Regression: Controlling for Demographics"),
          p("How does the UV coefficient change when we control for white population %?"),
          plotOutput("multiple_regression_plot", height = "450px")
        ),
        
        hr(),
        
        wellPanel(
          h3("3. Effect Size Comparison"),
          p("Standardized coefficients show which variables matter most."),
          plotOutput("regression_coefficients", height = "400px")
        ),
        
        hr(),
        
        wellPanel(
          h3("4. Partial Regression: Pure UV Effect"),
          p("Visualizing UV-melanoma relationship after removing demographic confounding."),
          plotOutput("partial_regression", height = "500px")
        ),
        
        hr(),
        
        wellPanel(
          h3("5. Model Diagnostics"),
          p("Checking regression assumptions: linearity, normality, homoscedasticity, outliers."),
          plotOutput("residual_diagnostics", height = "600px")
        ),
        
        hr(),
        
        wellPanel(
          h3("6. Model Interpretation"),
          uiOutput("model_interpretation")
        ),
        
        hr(),
        
        wellPanel(
          h3("7. Full Regression Output"),
          verbatimTextOutput("regression_summary")
        ),
        
        hr(),
        hr(),
        
        wellPanel(
          h3("8. Occupational Exposure Analysis"),
          uiOutput("occupation_interpretation"),
          hr(),
          h4("Correlations with Melanoma Rate"),
          verbatimTextOutput("occupation_correlation"),
          hr(),
          h4("UV × Outdoor Work Interaction"),
          plotOutput("occupation_uv_plot", height = "500px"),
          hr(),
          h4("Regression Model Comparison"),
          verbatimTextOutput("occupation_regression"),
          hr(),
          h4("Sensitivity Analysis"),
          verbatimTextOutput("occupation_sensitivity")
        ),
        
        hr(),
        
        wellPanel(
          h3("9. ANOVA: Variable Contributions"),
          verbatimTextOutput("anova_table")
        ),
        
        hr(),
        
        wellPanel(
          h3("10. Nested Model Comparisons"),
          verbatimTextOutput("nested_models")
        ),
        
        hr(),
        
        wellPanel(
          h3("11. Residual Diagnostics"),
          verbatimTextOutput("residual_analysis"),
          plotOutput("residual_plots", height = "600px")
        )
      )
    )
  )
)