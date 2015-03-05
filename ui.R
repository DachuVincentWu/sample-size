#AS ui
library(shiny)
library(htmltools) 

shinyUI(fixedPage(
  
  titlePanel("Sample Size Calculator"),
  
  sidebarLayout(fluid = F,
    sidebarPanel(width = 6,
      radioButtons("data_type",label=HTML("<font color=003D79><big>Data Type</big></font>"),selected = 0,
                   choices = list("Binary"=1,"Quantitative"=2,"Survival"=3,"Ordinal"=4)),
      uiOutput("design"),
      tags$hr(),
      HTML("<font color=003D79><big><b>Result</b></big></font>"),
      br(),
      uiOutput("sam_bin"),
      uiOutput("sam_surv"),   ##ps
      br(),tags$hr()
      
    ),
    
    mainPanel(width = 6,
      #tags$style(type="text/css",
      #          ".shiny-output-error { visibility: hidden; }",
      #         ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(type = 'tabs',
      tabPanel("Parameters",
        br(),
        ### binary
        uiOutput("mode_bin"),uiOutput("mode_quant"),
        ### survival
        uiOutput("mod_surv_prop"),uiOutput("mod_surv_dist"),uiOutput("choice_alpha"),
        uiOutput("mode_surv")
            ),
      tabPanel("Manual",
        br(),
        uiOutput("bin_def"),uiOutput("quant_def"),
        uiOutput("surv_def")
               )
      )
    )    
  )
  
))
