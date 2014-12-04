#AS ui
library(shiny)
library(htmltools) 

shinyUI(fluidPage(
  
  titlePanel("Sample Size Calculator"),
  #fluidRow(column(3, offset = 8,tags$h4(textOutput("sam_bin")))),
  sidebarLayout(fluid = T,
    sidebarPanel(width = 5,
      radioButtons("data_type",label=HTML("<font color=003D79><big>Data Type</big></font>"),selected = 0,
                   choices = list("Binary"=1,"Quantitative"=2,"Survival"=3,"Ordinal"=4)),
                 
      #selectInput("data_type",label=h5("Data Type"),,width = '400px',
      #           choices = list("---Please Select---"=0,"Binary"=1,"Quantitative"=2,"Survival"=3,"Ordinal"=4)),
      uiOutput("design"),
      tags$hr(),
      HTML("<font color=003D79><big>Result</big></font>"),
      br(),br(),
      uiOutput("sam_bin"),
      br(),tags$hr(),
      HTML("<font color=8E8E8E><small><i>Da-Chu Wu,2014</i></small></font>")
    ),
    
    mainPanel(width = 6,
      #tags$style(type="text/css",
      #          ".shiny-output-error { visibility: hidden; }",
      #         ".shiny-output-error:before { visibility: hidden; }"),
      
      #numericInput("alpha", 
       #            label = HTML("<b><big>Type I error(&alpha;)</big></b>"),
        #           value = 0.05,step=0.01,max=1,min=0),
      tabsetPanel(
      tabPanel("Parameters",
        uiOutput("mode1"),br(),
        uiOutput("mode_var"),
        tags$hr(),
        #uiOutput("run"),
        br(),br()
            ),
      tabPanel("Information",
        uiOutput("img_prop_def")
        
               )
      )
    )    
  )
  
))

