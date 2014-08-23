

#load library

library(shiny)


# Set mean & sd value for Normal Distribution
mean_sd <- function() {
  wellPanel(
    numericInput("mean", "Mean:", 0),
    numericInput("sd", "Standard Deviation : ", 1, min=0),
    sliderInput("range", "Range:",
                min = -30, max = 30, value = c(-10,10))
  )
}

# set lambda value for Poission Distribution
lambda <- function() {
  wellPanel(
    numericInput("lambda", "Lambda:", 1),
    sliderInput("range", "Range:",
              min = -30, max = 30, value = c(-10,10))
  )
}

#set Degrees of Freedom and Non-centralityvalue for Student's T Distribution
df_ncp <- function() {
  wellPanel(
    numericInput("df", "Degrees of Freedom:", 1, min=0),
    numericInput("ncp", "Non-centrality parameter:", 0, min=0),
    sliderInput("range", "Range:",
                min = -30, max = 30, value = c(-10,10))
  )
}


#set Binomial probability and number of trials value for Binomial Distribution
p_binom <- function () {
  wellPanel(
    numericInput("size", "Number of trials:", 1000, min=0, step=100),
    numericInput("prob", "probability of success on each trial:", .5, min=0, max=1),
    sliderInput("range", "Range:",
                min = -1000, max = 1000, value = c(400,600))
  )
}

#Set N, M,K value for Hypergeometric distribution
p_hyper <- function() {
  wellPanel(
    numericInput("m", "Number of white marbles(M):", 2, min=0),
    numericInput("n", "Number of black marbles(N):", 3, min=0),
    numericInput("k", "Number of marbles drawn(K):", 10, min=0),
    sliderInput("range", "Range:",
                min = -30, max = 30, value = c(-10,10))
				)
  }

min_max <- function() {
  wellPanel(
    numericInput("mn", "Minimum:", 0),
    numericInput("mx", "Maximum:", 1),
    sliderInput("range", "Range:",
                min = -100, max = 100, value = c(-10,10))
  )
}

getParams <- function(input) {
  switch(input$fun,
         "binom" = return (list(size=input$size,prob=input$prob)),
         "chisq" = return (list(df=input$df,ncp=input$ncp)),
         "norm"  = return (list(mean=input$mean,sd=input$sd)),
         "pois"  = return (list(lambda=input$lambda)),
         "t"     = return (list(df=input$df,ncp=input$ncp)),
		 "hyper" = return (list(m=input$m,n=input$n,k=input$k)),
         return (list(0,1))
  )
}
  
  
 #Display the interactive plot
 
DistributionPlot <- function(input) {
  n <- input$n
  range <- input$range
  if (is.null(input$range) || is.null(input$type)) return()
  x <- seq(range[1], range[2], length=n)
  
  if (input$type == "cum") {
    switch(input$fun,
           "binom" = plot(function(x) pbinom(x, size=as.numeric(input$size), prob=as.numeric(input$prob)), range[1], range[2], col="red"),
           "chisq" = plot(function(x) pchisq(x, df=as.numeric(input$df), ncp=as.numeric(input$ncp)), col="red"),
           "norm"  = plot(function(x) pnorm(x, mean=as.numeric(input$mean), sd=as.numeric(input$sd)), range[1], range[2], col="red"),
           "pois"  = plot(function(x) ppois(x, lambda=as.numeric(input$lambda)), range[1], range[2], col="red"),
		   "hyper" = plot(function(x) phyper(x, m=as.numeric(input$m), n=as.numeric(input$n), k=as.numeric(input$k)), range[1], range[2], col="red"),
           "t"     = plot(function(x) pt(x, df=as.numeric(input$df),ncp=as.numeric(input$ncp)), range[1], range[2], col="red"),
           plot(function(x) pnorm(x), range[1], range[2], col="red")
    )
 
  } else {
    switch(input$fun,
           "binom" = y <- dbinom(x, size=as.numeric(input$size), prob=as.numeric(input$prob)),
           "chisq" = y <- dchisq(x, df=as.numeric(input$df), ncp=as.numeric(input$ncp)),
           "norm"  = y <- dnorm(x, mean=as.numeric(input$mean), sd=as.numeric(input$sd)),
           "pois"  = y <- dpois(x, lambda=as.numeric(input$lambda)),
		   "hyper" = y <- dhyper(x, m=as.numeric(input$m), n=as.numeric(input$n), k=as.numeric(input$k)),
           "t"     = y <- dt(x, df=as.numeric(input$df),ncp=as.numeric(input$ncp)),
           plot(function(x) dnorm(x), range[1], range[2], col="red")
    )

    if (length(x) == length(y)) {
	   
	   t= "No"
	   if (input$fun=='binom') t= "Binomial";
	   if (input$fun=='norm') t= "Normal(Gaussian)";
	   if (input$fun=='pois') t= "Poisson";
	   if (input$fun=='t')    t= "Student's T";
	   if (input$fun=='chisq') t= "Chi-squared";
	   if (input$fun=='hyper') t= "Hypergeometric";
      plot(x, y,  lty=6, xlab="Value", xlim=range, col="red", type="l",
           ylab="Density",
		   main=paste(t," Distribution")
		   )
    } else {
      p("The graph cannot be generated with these parameters.")
    }
  }
}



format_list <- function (l){
  ret <- "Params:"
  for (i in names(l)) {
    ret <- paste(ret,i)
    ret <- paste(ret,l[i],sep="=")
  }
  return (ret)
}


#shiny server logic
shinyServer(function(input, output) {
   
   #call for plot
  output$plot <- renderPlot({
    DistributionPlot(input)
  })
  
  
  output$fun1 <- renderText({
    input$fun
  })
  
  output$fun2 <- renderPrint({
    str(input$fun)
  })
  
  output$params <- renderText({
    format_list(getParams(input))
  })
  
  output$ui <- renderUI({
    switch(input$fun,
                "binom" = p_binom(),
                "chisq" = df_ncp(),
                "norm" = mean_sd(),
                "pois" = lambda(),
                "t" = df_ncp(),
				"hyper" = p_hyper(),
                helpText("Not supported - please edit ui.R")
         )
  })
})

