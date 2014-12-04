#AS server
library(shiny)
library(htmltools)

shinyServer(
  
  function(input, output,session) {
    
    ### choose data type -> study design↳
    output$design<-renderUI({
      # help text
      validate(need(input$data_type != 0, "Please select a data type"))
      if(input$data_type==1){
        selectInput("type_binary",label=HTML("<font color=003D79><big>Study Design</big></font>"),width = '300px',selected = 0,
                    choices = list("---Please Select---"=0,"One Sample"=0.5,"↳ Equality Trial"=1,"↳ Non-Inferiority/Superiority Trial"=2,"↳ Bioequivalence Trial"=3,
                                   "Two Sample"=3.5,"↳ Equality Trial"=4,"↳ Non-Inferiority/Superiority Trial"=5,"↳ Bioequivalence Trial"=6))
      } else if(input$data_type==2){
        selectInput("type_quant",label=HTML("<font color=003D79><big>Study Design</big></font>"),width = '300px',selected = 0,
                    choices = list("---Please Select---"=0,"One Sample"=0.5,"↳ Equality Trial"=1,"↳ Non-Inferiority/Superiority Trial"=2,"↳ Bioequivalence Trial"=3,
                                   "Two Sample"=3.5,"↳ Equality Trial"=4,"↳ Non-Inferiority/Superiority Trial"=5,"↳ Bioequivalence Trial"=6))
      }else if(input$data_type==3){
        selectInput("type_surv",label=HTML("<font color=003D79><big>Study Design</big></font>"),width = '300px',selected = 0,
                    choices = list("---Please Select---"=0,"Survival 1"=1,
                                   "Survival 2"=2))
      }else if(input$data_type==4){
        selectInput("type_ord",label=HTML("<font color=003D79><big>Study Design</big></font>"),width = '300px',selected = 0,
                    choices = list("---Please Select---"=0,"Ordinal 1"=1,
                                   "Ordinal 2"=2))    
      }
    })
    
    ### design==binary -> input 
    # Binary data type
    output$mode1<-renderUI({
      validate(need(input$type_binary != 0, ""))
      if(input$data_type==1){
        if (input$type_binary==0){
          HTML("<font color=CE0000><i>Note:please choose a study design !</i></font>")
        }else if (input$type_binary==1){
          list(
          numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),br(),br(),
          numericInput("t2",label =HTML("<b><big>&theta;<sub>0</sub></big></b><small><br>(Known success proportion) "),
                       value = 0.6,step=0.001,max=1,min=0),      
          numericInput("t1",label =HTML("<b><big>&theta;</big></b><small><br>(Expected success proportion of sample)"),
                       value = 0.8,step=0.001,max=1,min=0),br(),br(),
          numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                       value = 0.9,step=0.05,max=1,min=0)
          )
          }else if (input$type_binary==4){
          list(
          numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),br(),br(),
          numericInput("p1",label =HTML("<b><big>&theta;<sub>1</sub></big></b>"),
                       value = 0.6,step=0.001,max=1,min=0),
          numericInput("p2",label =HTML("<b><big>&theta;<sub>2</sub></big></b>"),
                       value = 0.8,step=0.001,max=1,min=0),br(),br(),
          numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                       value = 0.9,step=0.05,max=1,min=0),
          numericInput("ratio",label =HTML("<b>r (n<sub>2</sub>/n<sub>1</sub>)</b>"),
                       value = 1.0,step=0.1,max=10,min=0.1) 
          )
        }
      }
    })
    # var assumption
    output$mode_var<-renderUI({
      validate(need(input$type_binary!=0 , ""))
      if(input$data_type==1){
        # choose "Two Sample - Test for Equality"
        if (input$type_binary==4){
          selectInput("var_type",label = HTML("<b>Variance Assumption</b>"),selected = 1,
                      choices =list("---Please select---"=0,"Unequal variance(Default)"=1,"Equal variance"=2) )   
        }
      } 
    })
   
    # submit button
    output$run<-renderUI({
      validate(need(input$type_binary != 0, ""))
      if(input$type_binary!=0){
        submitButton("run")
      }
    })
    
    ### output 
    output$sam_bin <- renderUI({
      validate(need(input$type_binary != 0, ""))
      # Binary data
      if(input$data_type==1){
        
        if(input$type_binary==0){
          paste("n = ?")
        }else if (input$type_binary==1){ # 1 sample test for equality
          temp1_n<-((qnorm(input$alpha/2)+qnorm(1-input$power))^2)*input$t1*(1-input$t1)/(input$t1-input$t2)^2
          HTML(("<font color=2F0000 size=3.5 face=arial><big>&#8594; N  = "),ceiling(temp1_n),"</big></font>")
        }else if (input$type_binary==4){ # 2 sample test for equality
          validate(need(input$var_type != 0, "----------------------------------"))
          if(input$var_type==1){ # Unequal variance(Default)
            p_pool<-(input$p1+(input$p2*input$ratio))/(input$ratio+1)
            temp2_n1<-((qnorm(input$alpha/2)*sqrt((1+input$ratio)*p_pool*(1-p_pool)/input$ratio))+(qnorm(1-input$power)*sqrt(input$ratio*input$p1*(1-input$p1)+input$p2*(1-input$p2))))^2/(input$p1-input$p2)^2
            list(
              HTML(("<font color=2F0000 size=3.5 face=arial><big>&#8594; n<sub>1 </sub>="),ceiling(temp2_n1),"</big></font>"),br(),br(),
              HTML(("<font color=2F0000 size=3.5 face=arial><big>&#8594; n<sub>2 </sub>="),ceiling(temp2_n1*input$ratio),"</big></font>"),br(),br(),
              HTML(("<font color=2F0000 size=3.5 face=arial><big>&#8594; N  = "),ceiling(temp2_n1*input$ratio)+ceiling(temp2_n1),"</big></font>")
            )
            }else if (input$var_type==2){ # Equal variance
            p_pool<-(input$p1+(input$p2*input$ratio))/(input$ratio+1)
            temp3_n1<- ((qnorm(input$alpha/2)+qnorm(1-input$power))^2)*((input$ratio+1)*p_pool*(1-p_pool)/input$ratio)/(input$p1-input$p2)^2
            list(
              HTML(("<font color=2F0000 size=3.5 face=arial><big>&#8594; n<sub>1 </sub>="),ceiling(temp3_n1),"</big></font>"),br(),br(),
              HTML(("<font color=2F0000 size=3.5 face=arial><big>&#8594; n<sub>2 </sub>="),ceiling(temp3_n1*input$ratio),"</big></font>"),br(),br(),
              HTML(("<font color=2F0000 size=3.5 face=arial><big>&#8594; N  = "),ceiling(temp3_n1*input$ratio)+ceiling(temp3_n1),"</big></font>")
            )
          }     
        }
      }
    })
    
    
    
    ### info
    output$img_prop_def<-renderUI({
      validate(need(input$type_binary!=0 , ""))
      if(input$data_type==1){
        if (input$type_binary==4){
          validate(need(input$var_type != 0, ""))
          if (input$var_type==1){
            tags$img(src="https://dl-web.dropbox.com/get/samplesize/formula_prop_def.png?_subject_uid=71521442&w=AAAglPwiWTJ17G3Oydk8U5jaOg02ltvJ5YvGr8F0vcCxOw", height = 800, width = 700)   
          }else if (input$var_type==2)  {
            tags$img(src="https://dl-web.dropbox.com/get/samplesize/formula_prop_equal.png?_subject_uid=71521442&w=AABJ7_JMKRXxJehiNZOLLoCOvf2DzFhJljx9MogeJzK2ig", height = 800, width = 700) 
          }
        }else if(input$type_binary==1){
          tags$img(src="https://dl-web.dropbox.com/get/samplesize/formula_prop_1.png?_subject_uid=71521442&w=AAA14x_kJ2tjQH9GM05LX0_HhC-F6ZFF6hmu_MiD1WfxGQ", height = 800, width = 700) 
        }
      }    
    })
    
   
  }
)
