##VERSION 7 SWITCHES TO A GAMMA DISTRIBUTION FOR BETWEEN SEASON VARIABILITY
#Version 7A switches to autoregressive noise, where the observation noise slider controls innovations
#and season shock adds additional variations every 12 months

##TO ESTIMATE AR and noise components from real data: ar.test<- ar(test, aic = TRUE, order.max = 1)

#Clear workspace to get rid of old junk
# Define server logic required to draw a histogram

shinyServer(function(input, output, clientData, session) {

  source('power_calc_custom_functions shiny.R')
  
  output$Yale <- renderImage(list(
    src = 'images/Yale Logo.png',
    contentType = 'image/png', 
    alt = 'Yale Logo', 
    height = 100
  ), deleteFile = FALSE)
  
  input_data <- reactive({
    data_file <- input$in_file
    if (is.null(data_file)) {return(NULL)}
    data <- read.csv(data_file$datapath, check.names = FALSE, stringsAsFactors = FALSE)
    return(data)
  })
  observe({
    ds <- input_data()
    updateSelectInput(session = session, inputId = 'date.name', choices = colnames(ds))
  })
  observe({
    ds <- input_data()
    updateSelectInput(session = session, inputId = 'outcome.name', choices = colnames(ds))
  })
  observe({
    ds <- input_data()
    updateSelectInput(session = session, inputId = 'covariate', choices = colnames(ds))
  })

  ##########################
#######################
 power1<- observeEvent(input$run_analysis, {
    ds.select<-input_data() 
    withProgress(message = 'Making plot', value = 0, {
      sim.data<-ts.extract.func(ds2=ds.select, 
                              outcome.name=input$outcome.name,
                              covar.names=input$covariate,
                              nsim=input$nsim, #Number of simulations, as set above
                              ve.irr=input$exp.vax.rr,  #Expected vaccine effect (IRR)
                              post.start=input$intervention_date,
                              date.name=input$date.name)
    
    vax.eff<- est.vax.eff(sim.data ,overdisperse1=T ,n.season=12,intervention_date=input$intervention_date,time_points1=ds.select[,input$date.name] ) 
    output$powerImage <- renderPlot({
      title.text<-paste0('Simulations to detect a change of ',input$exp.vax.rr,';',input$nsim, ' Simulations',
                       '\nPower=', 100*round(mean(vax.eff[,'97.5%']<1),2), "%") 
     plot(x=vax.eff[,'50%'], y=1:nrow(vax.eff), bty='l', main=title.text, 
          xlab='Estimated Rate Ratio', ylab='Simulation Number', xlim=c(0.2,2), pch=16)
     arrows(y0=1:nrow(vax.eff), x0=vax.eff[,'2.5%'], x1=vax.eff[,'97.5%'], length=0, col='gray' )
     abline(v=1, col='black',lty=2)
     abline(v=input$exp.vax.rr, col='red',lty=2)
     
           #text(mean(vax.eff[,'97.5%']<1),2)
      
    })
   }) 
 }) 


#output$distPlot <- renderPlot({
#	  })
}) 
