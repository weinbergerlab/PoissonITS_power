library(shiny, quietly = TRUE)
library(lme4)
library(dummy)
library(MASS)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(grDevices)
library(dplyr)

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
          label = 'Select control variable(s) (counts per month), or leave blank',
          choices = NULL,
          selected = NULL,
          selectize = TRUE, 
          multiple = TRUE
        ),
        tags$hr(),
        dateInput(
          inputId = 'intervention_date',
          label = 'Date when intervention begins (YYYY-MM-01)',
          value='yyyy-mm-dd'
        ),
      sliderInput(
        inputId = 'pre_months',
        label = 'Number of months pre-vaccination',
        min=6, max=120, value=60
      ),
      sliderInput(
        inputId = 'post_months',
        label = 'Number of months post-vaccination',
        min=6, max=120, value=60
      ),
      sliderInput(
        inputId = 'decline.length',
        label = 'How long before rates stabilize after intervention?',
        min=1, max=48, value=24
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
           span("RESULTS: The first plot shows the simulated time series, which have characteristics (trend, seasonality, noise) similar to the original data from the pre-intervention period. The second plot shows the estimate of the rate ratio for each of the simulations. The red dashed line shows the 'true' effect of the intervention, and each dot and line indicates the median and 95% confidence interval for each of the simulations. The final plot shows the estimate from this study (red dot) in the context of the results from different states in Brazil. "),
           hr(),
           span("SAMPLE DATA: Some sample formatted time series from Chile are provided as a demonstration. The variables are date in months, number of hospitalizations due to all-cause pneumonia (J12_18) and number of non-respiratory hospitalization (ach_noj) as a control variable. These time series represent the number of cases among children <24 months of age. Pneumococcal vaccine was introduced in Chile in January 2011.  These data were originally obtained from the Chilean Ministry of Health's website, and were described in CAW Bruhn et al, PNAS 2017"),
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