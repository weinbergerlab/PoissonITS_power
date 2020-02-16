library(shiny, quietly = TRUE)
library(lme4)
library(dummy)
library(MASS)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(grDevices)

shinyUI(fluidPage(
  
  # Application title
  titlePanel('ITS power calculator for count data'),
  
  mainPanel(
    plotOutput("powerImage", height=600  )
      ),
  sidebarLayout(position='left',
    sidebarPanel(
      downloadButton("downloadData", "Download sample dataset"),
      fileInput("excel_input", "Choose .xlsx, .xls, or .csv Files",
                accept = c(
                  "xlsx",
                  ".xls", '.csv'),
                multiple=F
      ), 
      
        sliderInput("exp.vax.rr",
                    "Expected decline (relative rate):",
                    min = 0.2,
                    max = 0.95,
                    value = 0.8),
        #checkboxInput("adjust.overdisperse",
        #              "Adjust for overdispersion?",
        #              value = TRUE),
        tags$hr(),
        selectInput(
          inputId = 'date.name', 
          label = 'Select date variable (YYYY-MM-01):',
          choices = NULL, 
          selected = NULL,
          selectize = TRUE
        ), 
        selectInput(
          inputId = 'outcome.name', 
          label = 'Select outcome variable.',
          choices = NULL, 
          selected = NULL,
          selectize = TRUE
        ), 
        selectInput(
          inputId = 'covariate',
          label = 'Select control variables (unlogged)',
          choices = NULL,
          selected = NULL,
          selectize = TRUE, 
          multiple = TRUE
        ),
        tags$hr(),
        dateInput(
          inputId = 'intervention_date',
          label = 'Date when intervention begins (YYYY-MM-DD)',
          value='yyyy-mm-dd'
        ),
        numericInput(
          inputId = 'nsim',
          label = 'Number of simulations',
          value=100
        ),
        
      tags$hr(),
        actionButton(
          inputId = 'run_analysis', 
          label = 'Run Analysis'
        )
      ),
 

    column(12, align = 'justify',
           hr(),
           span("GETTING STARTED: The input file should be a .csv or .xlsx or .xls and the date should be in the YYYY-MM-01 format. Click the 'download sample dataset' button to download a template. The outcome variable should have the number of cases per unit time. Control variables can be included. These can be counts of other diseases. For source code, visit the "),
           a(target = '_blank', 'GitHub page.', href = 'https://github.com/weinbergerlab/PoissonITS_power'),
           hr(), 
           span("RESULTS: The first plot shows the simulated time series, which have characteristics (trend, seasonality, noise) similar to the original data from the pre-intervention period. The second plot shows the N estimate of the rate ratio for each of the simulations. The red dashed line shows the 'true' effect of the intervention, and each dot and line indicates the median and 95% confidence interval for each of the simulations. The final plot shows the estimate from this study (red dot) in the context of the results from different states in Brazil. "),
           hr(),
           span('This project is supported by a grant from the Bill and Melinda Gates Foundation to Yale University (PI: Weinberger).') 
    )),
  fluidRow(
    column(6, align = 'center',
           imageOutput('Yale')
    )
  )
)
)