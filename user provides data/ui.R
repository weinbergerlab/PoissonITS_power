library(shiny, quietly = TRUE)
library(lme4)
library(dummy)
library(MASS)
library(lubridate)
library(RColorBrewer)

shinyUI(fluidPage(
  
  # Application title
  titlePanel('ITS power calculator for count data'),
  
  mainPanel(
    plotOutput("powerImage", height=600  )
      ),
  sidebarLayout(position='left',
    sidebarPanel(
        fileInput(
          inputId = 'in_file',
          label = 'Choose a file* to upload.', 
          accept = c(
            '.csv'
          )
        ), 
        sliderInput("exp.vax.rr",
                    "Expected decline (relative rate):",
                    min = 0.2,
                    max = 0.95,
                    value = 0.8),
        checkboxInput("adjust.overdisperse",
                      "Adjust for overdispersion?",
                      value = TRUE),
        tags$hr(),
        selectInput(
          inputId = 'date.name', 
          label = 'Select date variable:',
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
          multiple = FALSE
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
           span('*To use this program properly, the file should be a .csv and the date must be in the YYYY-MM-01 format. For sample data sets, source code, and additional analysis code, visit the github sitesite\'s'),
           a(target = '_blank', 'GitHub page.', href = 'https://github.com/weinbergerlab/synthetic-control/'),
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