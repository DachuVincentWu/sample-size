#AS server
#
library(shiny)
library(htmltools)
shinyServer(
  
  function(input, output,session) {
    ### data type -> study design
    
    output$design<-renderUI({
      validate(need(input$data_type != 0, "Please select a data type"))
      if(input$data_type==1){
        selectInput("type_binary",label=h5("Study Design"),
                    choices = list("---Please Select---"=0,"Proportion - 1 Sample Z test"=1,
                                  "Proportion - 2 Sample Z test"=2))
        } else if(input$data_type==2){
          selectInput("type_quant",label=h5("Study Design"),
                      choices = list("---Please Select---"=0,"Quantitative 1"=1,
                                     "Quantitative 2"=2))    
        }else if(input$data_type==3){
          selectInput("type_surv",label=h5("Study Design"),
                      choices = list("---Please Select---"=0,"Survival 1"=1,
                                     "Survival 2"=2))    
        }else if(input$data_type==4){
          selectInput("type_ord",label=h5("Study Design"),
                      choices = list("---Please Select---"=0,"Ordinal 1"=1,
                                     "Ordinal 2"=2))    
        }
     })
    
    ### design==binary -> input 
    output$mode_var<-renderUI({
      validate(need(input$type_binary!=0 , ""))
      if(input$data_type==1){
        if (input$type_binary==2){
          selectInput("var_type",label = HTML("<b><big>Type of Variance</big></b>"),selected = 1,
                      choices =list("---Please select---"=0,"H1:Unequal variance(Default)"=1,"H1:Equal variance"=2) )   
        }
      } 
    })
    
    output$img_prop_def<-renderUI({
      validate(need(input$type_binary!=0 , ""))
      if(input$data_type==1){
        if (input$type_binary==2){
          validate(need(input$var_type != 0, "----------------------------------"))
          if (input$var_type==1){
            tags$img(src="formula_prop_def.png", height = 180, width = 300)   
          }else if (input$var_type==2)  {
            tags$img(src="formula_prop_equal.png", height = 180, width = 300) 
          }
        }else if(input$type_binary==1){
          tags$img(src="formula_prop_1.png", height = 180, width = 300) 
        }
      }    
    })
    
    output$mode1<-renderUI({
      validate(need(input$type_binary!= 0, "Notice:"))
        if(input$data_type==1){
          if (input$type_binary==0){
            HTML("<font color=CE0000><i>note:</i></font>")
            
          }else if (input$type_binary==1){
            numericInput("t1",label =HTML("<b><big><big>&theta;</big></big></b><small><br>(Expected success proportion of sample)"),
                       value = 0.8,step=0.001,max=1,min=0)
          } else if(input$type_binary==2){
            numericInput("p1",label =HTML("<b><big><big>&theta;<sub>1</sub></big></big></b>"),
                         value = 0.8,step=0.001,max=1,min=0)
        }
        }
      })
  output$mode2<-renderUI({
    validate(need(input$type_binary != 0, "Please select a study design."))
    if(input$data_type==1){
      if (input$type_binary==0){
        HTML("<font color=CE0000><i>please choose a study design !</i></font>")
      }else if (input$type_binary==1){
        numericInput("t2",label =HTML("<b><big><big>&theta;<sub>0</sub></big></big></b><small><br>(Known success proportion) "),value = 0.6,step=0.001,max=1,min=0)      
      }else if (input$type_binary==2){
        numericInput("p2",label =HTML("<b><big><big>&theta;<sub>2</sub></big></big></b>"),
                     value = 0.6,step=0.001,max=1,min=0)
      }
    }
  })
  
  ### output 
  output$sam <- renderText({
    validate(need(input$type_binary != 0, "----------------------------------"))
 
    if(input$data_type==1){
    
      if(input$type_binary==0){
        paste("n = ?")
      }else if (input$type_binary==1){
    result1<-((qnorm(input$alpha/2)+qnorm(1-input$power))^2)*input$t1*(1-input$t1)/(input$t1-input$t2)^2
    paste("n = ",round(result1,digits=4))
      }else if(input$type_binary==2){ #default
        validate(need(input$var_type != 0, "----------------------------------"))
        if(input$var_type==1){
          p_pool<-(input$p1+input$p2)/2
      result2<-((qnorm(input$alpha/2)*sqrt(2*p_pool*(1-p_pool)))+(qnorm(1-input$power)*sqrt(input$p1*(1-input$p1)+input$p2*(1-input$p2))))^2/(input$p1-input$p2)^2
      paste("n = ",round(result2,digits=4))
        }else if (input$var_type==2){
          p_pool<-(input$p1+input$p2)/2
      result2<- ((qnorm(input$alpha/2)+qnorm(1-input$power))^2)*(2*p_pool*(1-p_pool))/(input$p1-input$p2)^2
      paste("n = ",round(result2,digits=4))
          
        }     
      }
    }
  })
}
)

