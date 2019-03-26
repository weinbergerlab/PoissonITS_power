##VERSION 7 SWITCHES TO A GAMMA DISTRIBUTION FOR BETWEEN SEASON VARIABILITY
#Version 7A switches to autoregressive noise, where the observation noise slider controls innovations
#and season shock adds additional variations every 12 months

##TO ESTIMATE AR and noise components from real data: ar.test<- ar(test, aic = TRUE, order.max = 1)

#Clear workspace to get rid of old junk
rm(list=ls(all=TRUE))
library(shiny)
library(RColorBrewer)
library(lme4)
# Define server logic required to draw a histogram

shinyServer(function(input, output, clientData, session) {

###################PART OF THE PROGRAM THAT RERUNS WHEN SLIDERS CHANGE
output$ui.pct.decline<- renderUI({
    if (is.null(input$setting))
      return()

 ###BASE INPUT IN THIS VERSION ON THE SID DATA FROM COLORADO FOR 0-1 year OLD CHILDREN

    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$setting,
      "IPD (any serotype)" = 
 	  sliderInput("pct.decline.yr",
                  "True vaccine-associated change (%/year):",
                  min = -60,
                  max = 10,
                  value = -30.15),
	"Pneumococcal pneumonia (any serotype)"=
	   sliderInput("pct.decline.yr",
                  "True vaccine-associated change (%/year):",
                  min = -60,
                  max = 10,
                  value = -13.3),
	"All cause pneumonia"=
	   sliderInput("pct.decline.yr",
                  "True vaccine-associated change (%/year):",
                  min = -60,
                  max = 10,
                  value = 0.13)
			#actual value for this is 0.13
		)
	})	

output$ui.season.amp<- renderUI({
  if (is.null(input$setting)  )
      return()
  
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$setting,
	"IPD (any serotype)" = 
       sliderInput("season.amp",
                  "Strength of seasonality:",
                  min = 0,
                  max = 1,
                  value = 0.54),
		"Pneumococcal pneumonia (any serotype)"=
	  sliderInput("season.amp",
                  "Strength of seasonality:",
                  min = 0,
                  max = 1,
                  value = 0.91),
	"All cause pneumonia"=
	 sliderInput("season.amp",
                  "Strength of seasonality:",
                  min = 0,
                  max = 1,
                  value = 0.86)
	)
})	

output$ui.n.cases<- renderUI({
      if (is.null(input$setting))
      return()

    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$setting,
	"IPD (any serotype)" = 
	 sliderInput("mean.cases.n",
                  "Average number of cases per week:",
                  min = 1.0,
                  max = 500,
                  value = exp(1.12)),
	"Pneumococcal pneumonia (any serotype)"=
 sliderInput("mean.cases.n",
                  "Average number of cases per week:",
                  min = 1.0,
                  max = 500,
                  value = exp(0.93) ),
	"All cause pneumonia"=
 sliderInput("mean.cases.n",
                  "Average number of cases per week:",
                  min = 1.0,
                  max = 500,
                  value = exp(4.64)	)
	)
	})	

output$ui.rand.noise<- renderUI({
      if (is.null(input$setting))
      return()

    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$setting,
	"IPD (any serotype)" = 
 sliderInput("observation.noise",
                  "Amount of random variation in data:",
                  min = 1e-6,
                  max = 0.4,
                  value = 0.33),
	"Pneumococcal pneumonia (any serotype)"=
 sliderInput("observation.noise",
                  "Amount of random variation in data:",
                  min = 1e-6,
                  max = 0.4,
                  value = 0.25),
	"All cause pneumonia"=
 sliderInput("observation.noise",
                  "Amount of random variation in data:",
                  min = 1e-6,
                  max = 0.4,
                  value = 0.23)
	)
	})	

##########################
#######################
output$distPlot <- renderPlot({
	if (is.null(input$pct.decline.yr)){
	return()}

	n.year.post.PCV <- 		input$years.post.pcv.data      #how many years post-pcv data?
	n.year.pre.PCV  <-		input$years.pre.pcv.data     #how many years pre-PCV data?
	n.years.fill <-           	n.year.pre.PCV + n.year.post.PCV 
	year.introduce.vax.fill.vec<- n.years.fill -n.year.post.PCV  #What year is vaccine introduced?
	irr.yr.vax                 <-  1+(input$pct.decline.yr/100)
	irr.sec.trend.yr			<- 1 + (input$secular.trend.yr/100)
	vax.change.coeff.fill.vec <-   irr.yr.vax #What is the IRR per year?
	std.dev.year.fill.vec <-       input$year.noise #Variation between years
	mean.cases.vec<-               input$mean.cases.n  #What is the intercept (cases/month)?
	obs.noise.fill.vec <-          input$observation.noise #How much random noise (note there is also poisson observation noise added to the data separately?

	n.reps<-input$sim.n

	coeffs.end<- matrix(ncol=1, nrow=n.reps) #Create empty data frame


	#CHECKBOXES
 	if (input$model.season) include.seasonality <- 1 else include.seasonality <- 0
	if (input$model.secular.trend) include.secular.trend <-1 else include.secular.trend <-0      

	sim.function <-function(n.years.fill2,year.introduce.vax.fill, vax.change.coeff.fill,mean.cases, obs.noise.fill)
		{

			#monthly data for 10 years
			n.years<- n.years.fill2

			#Year vaccine introduced
			year.introduce.vax <-year.introduce.vax.fill

			#coeff for vaccine change per month is ln(IRR)/12, so a 10%/year decline is ln(0.9)/12, 30%/year would be ln ln(0.7)/12  try 0.8
			vax.change.coeff=log(vax.change.coeff.fill)/12

			#Standard deviation in mean between years...try 0.2
			#std.dev.year<-std.dev.year.fill

			#Observation noise...
			obs.noise=0.3

			#Vector of monthly values
			t <- 1:(12*10)

			#Seasonal variation
			season.component <-  1 * cos(2*3.14159*t/12+3.14159)
	
			#Year to year variation in mean levels, centered at mean with a certain stddev
			year.index=1:10
			#year.noise <- rgamma(n.years, shape=std.dev.year, scale = 0.15)
			#year.noise.tab<-cbind.data.frame(year.noise, year.index)

			#merge in yearly noise with seasonal variations
			d <- cbind.data.frame(t,season.component)
			d$year <-ceiling(t/12)

			d2<-d
			#d2<-merge(d,year.noise.tab, by.x="year", by.y="year.index")

			#Random observation-level noise
			d2$obs.noise <-rnorm(nrow(d2),mean=0, obs.noise)

			year.introduce.vax=7
			#vaccine introduced in year x and can be either a change in mean or changslope
			change.month<-(year.introduce.vax*12+1)
			#print(change.month)
			d2$meanchng<-0
			d2$meanchng[(year.introduce.vax*12+1):(nrow(d2)) ]  <-1

			d2$changslope<-d2$t -(year.introduce.vax*12)
			d2$changslope[ d2$changslope<0 ]<- 0

			include.seasonality=1
			d2$sin12.model <-include.seasonality*sin(2*3.14159*d2$t/12)
			d2$cos12.model <-include.seasonality*cos(2*3.14159*d2$t/12)
			
			include.secular.trend=1
			secular.trend.model <-t*include.secular.trend 			

			#GENERATE THE SIMULATED TIME SERIES
			irr.sec.trend.yr<-1
			vax.change.coeff<- -0.8
			mean.cases=10
			d2$exp.case.mean.no.int<-exp( season.component + log(irr.sec.trend.yr)/12*t +  d2$obs.noise + d2$changslope*vax.change.coeff)
     			beta0 <- log(mean.cases) - mean(log(d2$exp.case.mean.no.int)) #calculate intercept as function of mena cases and model components
			d2$exp.case.mean.no.int <- exp(beta0)* d2$exp.case.mean.no.int #full model with intercept
			d2$exp.case <-rpois(length(t),d2$exp.case.mean)  #The observed number of cases is drawn from some true mean
			d2$TID <-as.factor(t)

			plot.df<-cbind.data.frame(	d2$t,d2$exp.case)

			#FIT MODEL
			glmm1 <-glmer(exp.case ~  secular.trend.model + sin12.model +  cos12.model + changslope + (1|TID )  , data=d2, family = poisson, nAGQ = 0, control = glmerControl(check.rankX = "stop.deficient"), silent = TRUE)
			summary.glmm1<-summary(glmm1)
	
 coef.cp2 <- as.data.frame(summary.glmm1$coefficients)["changslope",1]
			coef.cp<-coef(glmm1)["changslope"]
			list.results<-list(summary.glmm1,coef.cp,plot.df)
			return(list.results)
		}

		irr.end<-NA
		est.pct.change.vax <-NA
		est.vax.change.irr <-NA
		
		sim.data <-data.frame(simn=numeric(),exp.case=numeric() )
		for (i in 1:n.reps){
			func<-sim.function(n.years.fill2=n.years.fill, year.introduce.vax.fill=year.introduce.vax.fill.vec, 
			vax.change.coeff.fill=vax.change.coeff.fill.vec,
			mean.cases=mean.cases.vec, obs.noise.fill=obs.noise.fill.vec)
			est.vax.change.irr<-exp(12*as.numeric(as.character(func[2])))
			irr.end[i] <-est.vax.change.irr  ##GIVES CHANGE PER YEAR POSTVAX
			
			sim.dataout<-cbind(i,unlist(func[[3]][1]),unlist(func[[3]][2]) )
			sim.data<-rbind(sim.data,sim.dataout)
					}
			
			est.pct.change.vax=100*(irr.end-1)
			quant.pct<- as.character(round(quantile(est.pct.change.vax,  probs = c(2.5, 50,97.5)/100),1))
			ci.vector<-paste("95%CI:", quant.pct[1], "," , quant.pct[3], sep='')			
			median.vector<-paste("Median:", quant.pct[2],sept="")
			mean.vector<-paste("Mean:", mean(est.pct.change.vax),sept="")
			std.dev.vector <- paste("Standard Deviation:", round(sqrt(var(est.pct.change.vax)),digits=2)  )
		


m <- rbind(c(2,2, 1,3))
layout(m)
par(mar = c(3, 3, 2, 1))
par(xaxs="i",  yaxs="i") # OPTION ENSURES X AND Y AXES MEET AT 0,0
	names(sim.data)<-c("i","month", "cases")
	# draw the histogram with the specified number of bins
	d <- density(est.pct.change.vax) # returns the density data 
	plot(d, bty="l",xlim=c(-90, 100), main="", lwd=2, col="red")
	#hist(est.pct.change.vax, bty="l",xlim=c(-90, 100), main="", lwd=2, col="red") # plots the results
	abline(v=0,lty="dotted",col="darkgray")
	abline(v=input$pct.decline.yr,lty="dashed",col="darkblue")
	text(input$pct.decline.yr,0.0,"Actual change (%)",srt=270, adj=c(1.0,1.00), cex=1.5 , col="darkgray")
	
	title(main="Estimates of % change from different simulations", cex.main=1.5)

	#Draw the time series
	color.vector=brewer.pal(n.reps ,"Blues")
	interaction.plot(sim.data$month,sim.data$i, sim.data$cases, axes=FALSE, bty="l",col=color.vector,ylab="Cases", xlab="time(months)", legend=FALSE, main="",ylim=c(0,max(sim.data$cases)) ) 
	axis(side=1, at=c(1,12,24,36,48,60,72,84,96,108,120,132,144) ,col="darkgray" )
		ymaxval<- max(sim.data$cases)
	yticks<- round(c( 0, ymaxval*0.1,  ymaxval*0.2,  ymaxval*0.3, ymaxval*0.4,  ymaxval*0.5,  ymaxval*0.6,  ymaxval*0.7,  ymaxval*0.8,  ymaxval*0.9,  ymaxval*1.0, ymaxval*1.1))
	axis(side=2, at=yticks ,col="darkgray" )
	title(main="", xlab="time(months)" , ylab="Cases", xpd=NA)

	
	plot(1:10, 1:10,col="white", axes=FALSE)
	text(2,10, median.vector , cex=3,adj=c(0, 0.5), xpd=NA  ) #Adds the median of the density plot
	#text(2,9, mean.vector , cex=3,adj=c(0, 0.5), xpd=NA  ) #Adds the median of the density plot
	text(2,9, ci.vector , cex=3,adj=c(0, 0.5), xpd=NA ) #Adds the median of the density plot
	text(2,8, std.dev.vector , cex=3,adj=c(0, 0.5), xpd=NA ) #Adds the median of the density plot



	  })
})
