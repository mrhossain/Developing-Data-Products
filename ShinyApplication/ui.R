
#This is the shiny UI script
#load the library
library(shiny)


# ui creation 12 by 12 ui

shinyUI(fluidPage(
  style = "color:blue",
  titlePanel("Fun With Probability Distribution Function"),
  
  fluidRow(
    #12 column divided in 3 block each block contain 4 column
    column(12, wellPanel(
	 style = "color:blue",
     p("The plot is updated each time with respect to change the parameter and function.")
    ))
  ),
  #block 1
  fluidRow(
    column(4,
           p("Please select a probability Distribution function."),
           selectInput("fun", "Distribution Function",
                 choices = c("binomial" = "binom",
                             "chi-squared" = "chisq",
                             "hypergeometric" = "hyper",
                             "normal" = "norm",
                             "Poisson" = "pois",
                             "Student's t" = "t"),
                 selected = "norm"
          ),
          selectInput("type", "Type",
                      choices = c("density" = "ncum",
                                  "cumulative" = "cum"
                                  ),
                      selected = "ncum"
                      )
    ),
	#block 2
    column(4, 
      uiOutput("ui")
    ),
	#block 3
    column(4,
           sliderInput("n", "Number of observations:", 
                       min = 1, 
                       max = 200, 
                       value = 100,
                       step  = 5),
           br(),
		   style = "color:blue",
           p("Selected Parameters:"),
           verbatimTextOutput("params")
           
    )
  ),
  fluidRow(
      column(12,
           plotOutput("plot")
    )
  )
))


