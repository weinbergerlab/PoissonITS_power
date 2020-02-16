##VERSION 7 SWITCHES TO A GAMMA DISTRIBUTION FOR BETWEEN SEASON VARIABILITY
#Version 7A switches to autoregressive noise, where the observation noise slider controls innovations
#and season shock adds additional variations every 12 months

##TO ESTIMATE AR and noise components from real data: ar.test<- ar(test, aic = TRUE, order.max = 1)

#Clear workspace to get rid of old junk
# Define server logic required to draw a histogram

source('power_calc_custom_functions shiny.R')

adjust.overdisperse=T
rr.ests.compile.trim<-readRDS('./Data/sim1.trim.rds')
power.month.trim<-lapply(rr.ests.compile.trim, function(x){
  y<-x[,"97.5%",]  
  power<-apply(y, 2, function(z) mean(z<1))
  return(power)
})
power.month.trim.array<-do.call('cbind', power.month.trim)
mse.trim<-mapply(mse.func, rr.ests.compile.trim, true.rr=0.8, SIMPLIFY=F)
sample.ds<-read.csv('./Data/brazil_region1_ag8.csv')
sample.ds<-sample.ds[,c('date',	'J12_18',	'ach_noj')]
re.sd<-readRDS('./Data/brazil.state.phi.sd.rds')

# Auto-detect viable time columns and their formats. Empty list if none are found, NULL if data is NULL
dateColumns = function(data) {
  names(data) %>% 
    sapply(function(name) {
      # List of viable formats for named column; NULL if none
      dateFormats %>% 
        lapply(function(format) {
          if(validFormat(data[[name]], format)) {
            format
          }
        }) %>% 
        compact() %>%
        (function(x) {
          if (length(x) > 0) {
            x
          }
        })
    }, simplify = FALSE, USE.NAMES = TRUE) %>%
    compact() 
}


shinyServer(function(input, output, clientData, session) {

  output$Yale <- renderImage(list(
    src = 'images/Yale Logo.png',
    contentType = 'image/png', 
    alt = 'Yale Logo', 
    height = 100
  ), deleteFile = FALSE)
  
  input_data <- reactive({
    file.names.import<-input$excel_input$datapath
    
    data_file <- file.names.import
    if (is.null(data_file)) {return(NULL)}
    if(grepl('xls',data_file)){
      data<-read_excel(data_file) 
    }else if(grepl('csv', data_file )){
      data<-read.csv(data_file,check.names = FALSE, stringsAsFactors = FALSE) 
    }    
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
    vax.eff<- est.vax.eff(sim.data ,overdisperse1=T ,n.season=12,intervention_date=input$intervention_date,
                          time_points1=as.Date(ds.select[,input$date.name] , tryFormats = c('%m/%d/%Y',"%Y-%m-%d", "%Y/%m/%d")) )
    output$powerImage <- renderPlot({
    par(mfrow=c(2,2))
      
      sim.ts<-sim.data$preds.stage2
      trans.gray=rgb(0,0,0, alpha=0.2)
      date1=as.Date(ds.select[,input$date.name], tryFormats = c('%m/%d/%Y',"%Y-%m-%d", "%Y/%m/%d"))
      matplot(date1,sim.ts, type='l', col=trans.gray, ylim=c(0, max(sim.ts)),xlab='', bty='l',xaxt='n', ylab='Simulated cases', main='Simulated time series')
      axis(side=1, at=seq.Date(from=min(date1), to=max(date1), by='year' ), labels= year(seq.Date(from=min(date1), to=max(date1), by='year' )) )
      abline(v= input$intervention_date, lty=2, col='red')
      
      
        title.text<-paste0('Estimates of the true change of ',input$exp.vax.rr,';',input$nsim, ' Simulations',
                       '\nPower=', 100*round(mean(vax.eff[,'97.5%']<1),2), "%") 
     plot(x=vax.eff[,'50%'], y=1:nrow(vax.eff), bty='l', main=title.text, 
          xlab='Estimated Rate Ratio', ylab='Simulation Number', xlim=c(0.2,2), pch=16)
     arrows(y0=1:nrow(vax.eff), x0=vax.eff[,'2.5%'], x1=vax.eff[,'97.5%'], length=0, col='gray' )
     abline(v=1, col='black',lty=2)
     abline(v=input$exp.vax.rr, col='red',lty=2)
     
           #text(mean(vax.eff[,'97.5%']<1),2)
     
      #Plot of Power from Brazil simulation study
      pal1<-viridis_pal(option='cividis')(length(power.month.trim))
      legend_image <- as.raster(matrix(pal1, ncol=1))
      par( mar=c(4,4,1,1))
      for(i in 1:length(power.month.trim)){
        #plot(re.sd, power.month[[i]], bty='l', main='Random noise vs power')
        if(i==1){
          plot(re.sd, power.month.trim[[i]], bty='l', xaxt='n', xlab='Unexplained variability (SD)',ylab='Power', ylim=c(0,1), col=pal1[i], pch=16)
          axis(side=1, at=c(2:10),labels=round(exp(2:10),-1) )
        }else{
          points(re.sd, power.month.trim[[i]], col=pal1[i], pch=16)
        }
        points(sim.data$re.sd, mean(vax.eff[,'97.5%']<1), col='red', pch=16, cex=5  )
      }
      rasterImage(legend_image, 0.3, 0.6, 0.31,0.9)
      text( x=rep(0.32, 3) , y=seq(from=0.9, to=0.6, length.out=3) , rev(seq(from=12, to=72 ,by=24)))
      text(x=0.3, y=0.94, 'Baseline Months (N)', adj=c(0,0), xpd=NA)
      
      # #Plot of bias from Brazil simulation study
      # for(i in 1:length(mse.trim)){
      #   if(i==1){
      #     plot(re.sd, sqrt(mse.trim[[i]]), bty='l', xaxt='n', xlab='Unexplained variability (SD)',ylab='Bias (RMSE)', ylim=c(0,0.26), col=pal1[i], pch=16)
      #     axis(side=1, at=c(2:10),labels=round(exp(2:10),-1) )
      #   }else{
      #     points(re.sd, sqrt(mse.trim[[i]]), col=pal1[i], pch=16)
      #     
      #   }
      #   points(sim.data$re.sd, sqrt(mean( (vax.eff[,'50%']-input$exp.vax.rr )^2 )), col='red', pch=16, cex=5  )
      #   
      # }
      # rasterImage(legend_image, 0.075, 0.15, 0.085,0.25)
      # text( x=rep(0.095, 3) , y=seq(from=0.25, to=0.15, length.out=3) , rev(seq(from=12, to=72 ,by=24)))
      # text(x=0.075, y=0.26, 'Baseline Months (N)', adj=c(0,0), xpd=NA)
    })
})
 })
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = 'chile.csv',
    content = function(file) {
      write.csv(sample.ds, file, row.names=F)
    }
  )
})
#plot from brazil simulation study showing results in context

