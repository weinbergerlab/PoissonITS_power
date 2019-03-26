library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Vaccine Impact Confidence Evaluation (VICE)"),
 fluidRow(
    column(12, offset = 0,
      p("Use the sliders to alter key characteristics of the data and see how it affects the
		ability to detect a vaccine-associated change using simulated data", 
        style = "font-family: 'Source Sans Pro';")
    )
  ),

# Show a plot of the generated distribution
	
      plotOutput("distPlot",height = "300px"),
    
  hr(),

  fluidRow(
    column(4,
	#h4("Parameters"),
    selectInput("setting", "Choose a type of sample data:", 
                  choices = c("IPD (any serotype)", "Pneumococcal pneumonia (any serotype)", "All cause pneumonia"), 
			selected="IPD (any serotype)"),
   sliderInput("years.pre.pcv.data",
                  "Years of Data from the pre-vaccine period:",
                  min = 1,
                  max = 10,
                  value = 2),
  sliderInput("years.post.pcv.data",
                  "Years of data from the post-vaccine period:",
                  min = 1,
                  max = 10,
                  value = 3),
   uiOutput("ui.pct.decline")
   ),

  column(4,offset=0.5,
  sliderInput("secular.trend.yr",
                  "Trend unrelated to vaccine (%/year):",
                  min = -30,
                  max = 30,
                  value = -10),
  uiOutput("ui.season.amp"),
  uiOutput("ui.n.cases")
  ),
 
 column(4,offset=0.5,
	uiOutput("ui.rand.noise"),

  checkboxInput(inputId = "model.season",
      label = strong("Control for seasonality in the model?"),
      value = TRUE),
  checkboxInput(inputId = "model.secular.trend",
      label = strong("Control for secular trend in the model?"),
      value = TRUE),   
  numericInput("sim.n", label = h3("Number of simulations"), 
        value = 100)
		) ,

   ###Bottom labels
 fluidRow(
	  column(12, offset = 0,
  	    p("GETTING STARTED: Use the drop down menu to select a type of data, which will give you some reasonable initial values to
		start working with. You can then use the sliders to see how adding additional years of data influences the accuracy of
		the estimates of vaccine impact. You can also adjust the sliders to make seasonality more or less intense, to
		add trends unrelated to vaccination, and to adjust the amount of noise (random variation) in the time series. The checkboxes
		can be used to change the regression model used to estimate vaccine impact: if you uncheck the boxes, the model
		will no longer control for trends unrelated to vaccination or for seasonality.", 
 	       style = "font-family: 'Source Sans Pro';")
 	   )
 	 ),
  	  column(12, offset = 0,
  	    p("Basic explanation: Based on the values of the sliders, time series are simulated. The time series
		is a simple harmonic variable with random Poisson error and an additional random log-normal error. The effect of the vaccine is simulated
		by adding a log-linear trend starting at the time indicated by the sliders and the rate of decline indicated by the sliders. 
		
		 More technical explanation: The time series have the form: ObservedCases~Poisson(Mu); 
		log(Mu)= log(N.Cases.Wk) + log(PctChangeYearSlider)/12*t + SeasonalitySlider*cos(2*3.14159*t/12+3.14159)+ log(1+(PctDeclineYearSlider/100))/12*VaxSpline+ error).
		Where t is an index for time in months, the cosine term adds seasonality, the VaxSpline term adds a linear trend that takes a value of 0 before 
		vaccine introduction and then declines at a (log)linear rate after. The random variation slider adds random normal noise to the estimates, where the value of the slider
		gives the standard deviation of the noise.
		A quasi-Poisson regression model is then fit to each of these time series where the covariates are an index variable
		that captures secular trends, another index that has a value of 0 prior to vaccine introduction that captures
		the vaccine effect, and sine and cosine terms with a period of 12 months to capture seasonality. The secular trend 
		and seasonality covariates can be removed using the checkboxes. The coefficient for the spline term is used to 
			estimate the vaccine-associated decline: PercentChangeYear=100*(exp(beta.spline*12)-1). The density plot
			shows the distribution of these estimates.  The simulated data and models assume that once the vaccine is
			introduced, it has an immediate effect, and the rate of decline is constant and continues to the end of the time series.
			For longer time series, this might not be realistic, since disease rates will likely plateau (particularly for
			less specific outcomes such as all-cause pneumonia).", 
 	       style = "font-family: 'Source Sans Pro';")
 	   )
 	 ),
	 fluidRow(
  	  column(12, offset = 0,
  	    p("This tool was developed by Dan Weinberger at Yale School of Public Health in collaboration with Christian Bruhn,
		Rob Taylor, Lone Simonsen, and Cynthia Schuck-Paim. Funding was provided by
		a grant from the Bill and Melinda Gates Foundation. Thanks to Bill Hanage, Kate O'Brien, Keith Klugman, Matt Moore, and Gili Regev-Yochay
		for comments on the design.", 
 	       style = "font-family: 'Source Sans Pro';")
 	   )
 	 )
    
    
  )
)
