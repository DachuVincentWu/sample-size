#AS ui
library(shiny)
library(htmltools) 

shinyUI(fluidPage(
  
  titlePanel("Sample Size Calculator"),

  sidebarLayout(
    sidebarPanel(
      #helpText("Please choose your study design. "),
      
      selectInput("data_type",label=h5("Data Type"),
                  choices = list("---Please Select---"=0,
                                 "Binary"=1,"Quantitative"=2,"Survival"=3,"Ordinal"=4)),
      uiOutput("design"),
      br(),br(),br(),br(),
      br(),br(),br(),br(),br(),
      HTML("<font color=8E8E8E><small><i>Da-Chu Wu,2014</i></small></font>")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #tags$style(type="text/css",
       #          ".shiny-output-error { visibility: hidden; }",
        #         ".shiny-output-error:before { visibility: hidden; }"),
      
      numericInput("alpha", 
                   label = HTML("<b><big>Type I error(&alpha;)</big></b>"),
                   value = 0.05,step=0.01,max=1,min=0),
      numericInput("power",label = HTML("<b><big>Power(1-&beta;)</big></b>"),
                   value = 0.9,step=0.05,max=1,min=0),
      br(),
      uiOutput("mode1"),
      uiOutput("mode2"),
      br(),
      uiOutput("mode_var"),
      tags$hr(),
      tags$h4(textOutput("sam")),
      tags$hr(),
      br(),
      uiOutput("img_prop_def"),
      br(),
      br()
    )
  )
))
