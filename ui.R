#AS ui
library(shiny)
library(htmltools) 

shinyUI(fluidPage(
  
  titlePanel("Sample Size Calculator"),
  
  sidebarLayout(fluid = T,
    sidebarPanel(width = 5,
      radioButtons("data_type",label=HTML("<font color=003D79><big>Data Type</big></font>"),selected = 0,
                   choices = list("Binary"=1,"Quantitative"=2,"Survival"=3,"Ordinal"=4)),
                 
      #selectInput("data_type",label=h5("Data Type"),,width = '400px',choices = list("---Please Select---"=0,"Binary"=1,"Quantitative"=2,"Survival"=3,"Ordinal"=4)),
      uiOutput("design"),
      tags$hr(),
      HTML("<font color=003D79><big>Result</big></font>"),
      br(),br(),
      uiOutput("sam_bin"),
      br(),tags$hr()
  
    ),
    
    mainPanel(width = 6,
      #tags$style(type="text/css",
      #          ".shiny-output-error { visibility: hidden; }",
      #         ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(
      tabPanel("Parameters",
        uiOutput("mode_bin"),br(),
        uiOutput("mode_var"),
        tags$hr()
        #uiOutput("run"),
            ),
      tabPanel("Manual",
        uiOutput("img_prop_def"),br(),
        tags$hr()
               )
      )
    )    
  )
  
))

