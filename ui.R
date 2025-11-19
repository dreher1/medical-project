library(shiny)
library(markdown)
library(leaflet)
library(reactable)
library(biscale)
library(ggplot2)
library(cowplot)
library(plotly)
library(DT)

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
          inputId = "melanoma_view",
          label   = "Show data layers:",
          choices = c(
            "Melanoma by County" = "count",
            "Melanoma Rate (Age-Adjusted per 100k)" = "rate",
            "UV Measurement (wmh2)" = "uv",
            "Physician Availability per 100k" = "md_availability",
            "Bivariate: UV × Melanoma Rate" = "bivariate",
            "Risk-Adjusted: UV × Melanoma (White Pop Weighted)" = "bivariate_weighted",
            "Healthcare Access: MD Availability × Melanoma Rate" = "bivariate_md"
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
    
    # Statistical Analysis tab - IMPROVED VERSION
    tabPanel(
      "Statistical Analysis",
      fluidPage(
        # Add custom CSS for better styling
        tags$head(
          tags$style(HTML("
            .stat-card {
              padding: 20px;
              border-radius: 10px;
              color: white;
              text-align: center;
              margin-bottom: 20px;
              box-shadow: 0 4px 6px rgba(0,0,0,0.1);
            }
            .stat-value {
              font-size: 36px;
              font-weight: bold;
              margin-bottom: 5px;
            }
            .stat-label {
              font-size: 14px;
              opacity: 0.9;
              text-transform: uppercase;
            }
            .insight-box {
              padding: 15px;
              margin-bottom: 15px;
              border-radius: 8px;
            }
            .section-header {
              color: #1976D2;
              font-weight: bold;
              margin-top: 30px;
              margin-bottom: 20px;
              padding-bottom: 10px;
              border-bottom: 2px solid #E0E0E0;
            }
            .info-box {
              background: #E1F5FE;
              border-left: 4px solid #039BE5;
              padding: 15px;
              margin: 15px 0;
              border-radius: 4px;
            }
          "))
        ),
        
        h2("Statistical Analysis of UV-Melanoma Relationship", 
           style = "color: #1565C0; font-weight: bold; text-align: center; margin-bottom: 30px;"),
        
        # Key metrics at the top
        uiOutput("key_metrics"),
        
        br(),
        
        # Main Analysis Section
        h3("1. Interactive Data Exploration", class = "section-header"),
        wellPanel(
          plotlyOutput("interactive_scatter", height = "500px"),
          p("Hover over points to see county details. Color indicates white population percentage.", 
            style = "text-align: center; color: #666; margin-top: 10px;")
        ),
        
        h3("2. Simpson's Paradox Revealed", class = "section-header"),
        wellPanel(
          plotOutput("simpsons_paradox", height = "500px"),
          HTML("<div class='alert alert-warning' style='margin-top: 15px;'>
                <strong>Key Finding:</strong> The overall UV-melanoma relationship appears negative (red dashed line), 
                but within demographic groups, it's positive. This is Simpson's Paradox - a statistical phenomenon 
                where trends reverse when data is aggregated.</div>")
        ),
        
        h3("3. Regression Analysis (Linear Model)", class = "section-header"),
        wellPanel(
          plotOutput("regression_visual", height = "400px"),
          HTML("<div class='info-box'>
                <strong>What is Linear Regression?</strong><br>
                We're using a mathematical equation to predict melanoma rates based on UV exposure and demographics:<br>
                <code>Melanoma Rate = β₀ + β₁(UV) + β₂(White%) + error</code><br><br>
                The bars show how much each factor increases melanoma rates. Error bars show uncertainty.</div>")
        ),
        
        h3("4. ANOVA - Analysis of Variance", class = "section-header"),
        wellPanel(
          plotOutput("anova_visual", height = "400px"),
          HTML("<div class='info-box'>
                <strong>What is ANOVA?</strong><br>
                ANOVA tells us how much of the variation in melanoma rates each factor explains. 
                Think of it like dividing a pie - how big is each factor's slice?<br><br>
                <strong>F-Statistics:</strong> Measure how much better each predictor is than random chance. 
                Bigger F = more important predictor.</div>")
        ),
        
        h3("5. Model Performance", class = "section-header"),
        wellPanel(
          plotOutput("model_comparison", height = "400px"),
          HTML("<div style='padding: 10px; background: #F5F5F5; border-radius: 5px; margin-top: 15px;'>
                <strong>Interpretation:</strong> Adding demographics to UV dramatically improves model performance. 
                The interaction model (UV × Demographics) performs best, suggesting UV effects vary by population composition.</div>")
        ),
        
        h3("6. Effect Size Analysis", class = "section-header"),
        wellPanel(
          plotOutput("effect_comparison", height = "350px"),
          HTML("<div style='padding: 10px; background: #E3F2FD; border-radius: 5px; margin-top: 15px;'>
                <strong>What are Standardized Effects?</strong><br>
                These show the relative importance of each factor when both are measured on the same scale. 
                A larger effect size means that factor has more influence on melanoma rates.</div>")
        ),
        
        h3("7. True UV Effect (Controlling for Demographics)", class = "section-header"),
        wellPanel(
          plotOutput("partial_plot", height = "450px"),
          HTML("<div class='alert alert-success' style='margin-top: 15px;'>
                <strong>What This Shows:</strong> After removing demographic influence (statistical control), 
                UV exposure shows a positive relationship with melanoma rates - exactly what we'd expect biologically. 
                This is the 'true' UV effect.</div>")
        ),
        
        h3("8. Model Predictions - How Accurate Are We?", class = "section-header"),
        wellPanel(
          plotlyOutput("residual_map", height = "500px"),
          HTML("<div class='info-box'>
                <strong>What are Model Predictions?</strong><br>
                Using our equation from the regression, we predict what each county's melanoma rate should be 
                based on its UV exposure and demographics. Points close to the diagonal line = good predictions. 
                Points far away = something else is affecting melanoma rates there (healthcare access, behaviors, etc.).</div>")
        ),
        
        h3("9. Model Diagnostics - Is Our Model Valid?", class = "section-header"),
        wellPanel(
          plotOutput("diagnostic_plots", height = "600px"),
          HTML("<div class='info-box'>
                <strong>Understanding Diagnostic Plots:</strong><br>
                <b>1. Linearity:</b> Checks if the relationship is straight-line (linear) vs curved<br>
                <b>2. Normality:</b> Checks if prediction errors follow a bell curve (important for statistics)<br>
                <b>3. Equal Variance:</b> Checks if our predictions are equally good across all values<br>
                <b>4. Influential Points:</b> Identifies counties that might be skewing our results<br><br>
                Our model passes most checks but shows some counties with unusual patterns.</div>")
        ),
        
        h3("10. Summary of Findings", class = "section-header"),
        wellPanel(
          uiOutput("insights_summary")
        ),
        
        h3("11. Counties of Interest", class = "section-header"),
        wellPanel(
          h4("Counties with Largest Prediction Errors", style = "color: #424242; text-align: center;"),
          DTOutput("county_table"),
          HTML("<div style='padding: 10px; background: #F5F5F5; border-radius: 5px; margin-top: 15px;'>
                <strong>Why These Matter:</strong> Counties with large residuals (prediction errors) may have unique 
                factors not captured by our model - perhaps differences in healthcare access, screening practices, 
                sun protection behaviors, or occupational exposures.</div>")
        ),
        
        br(),
        br()
      )
    )
  )
)
