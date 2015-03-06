#AS server
library(shiny)
library(htmltools)

shinyServer(
  function(input, output) {
### choose data type -> study design??
    output$design<-renderUI({
      validate(need(input$data_type != 0 , "Please select a data type"))# help text
      if(input$data_type==1){
        selectInput("type_binary",label=HTML("<font color=003D79><big><b>Study Design</b></big></font>"),width = '300px',selected = 0,selectize=F,size=11,
                    choices = list("--- Please Select ---"=0,"One-arm"=0.5,"→ Equality Trial"=1,"→ Non-Inferiority/Superiority Trial"=2,"→ Equivalence Trial"=3,
                                   "Two-arm"=3.5,"→ Equality Trial"=4,"→ Non-Inferiority/Superiority Trial"=5,"→ Equivalence Trial"=6,"Three-arm"=6.5,"→ Non-Inferiority"=7 ))  ##ps
      } 
      else if(input$data_type==2){
        selectInput("type_quant",label=HTML("<font color=003D79><big><b>Study Design</b></big></font>"),width = '300px',selected = 0,selectize=F,size=11,
                    choices = list("--- Please Select ---"=0,"One-arm"=0.5,"→ Equality Trial"=1,"→ Non-Inferiority/Superiority Trial"=2,"→ Equivalence Trial"=3,
                                   "Two-arm"=3.5,"→ Equality Trial"=4,"→ Non-Inferiority/Superiority Trial"=5,"→ Equivalence Trial"=6,"Three-arm"=6.5,"→ Non-Inferiority"=7 ))  ##ps
      }
      else if(input$data_type==3){
        selectInput("type_surv",label=HTML("<font color=003D79><big><b>Study Design</b></big></font>"),width = '380px',selected = 0,selectize=F,size=8,
                    choices = list("--- Please Select ---"=0,"Two-arm"=0.5,"→ Exponential distribution--Schoenfeld & Richter"=1,"→ Exponential distribution--Lachin"=2,
                                   "→ Proportional-hazards regression model"=3,"→ Non-proportion hazard regression model"=4,"Three-arm"=5 ) )      ##ps       
      } 
      else if(input$data_type==4){
        selectInput("type_ord",label=HTML("<font color=003D79><big><b>Study Design</b></big></font>"),width = '300px',selected = 0,
                    choices = list("--- Please Select ---"=0,"→ Ordinal 1"=1,
                                   "→ Ordinal 2"=2))    
      }     
    })
    
### Binary data
    ### binary data -> input items
    output$mode_bin<-renderUI({
      validate(need(input$type_binary != 0, ""))
      if(input$data_type==1){
          #HTML("<font color=CE0000><i>Note:please choose a study design !</i></font>")
          if (input$type_binary==1) { # one-arm Equality
          list(
          numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),
          numericInput("t2",label =HTML("<b><big>&theta;<sub>0</sub></big></b><small><br>(Known success proportion)</small> "),
                       value = 0.6,step=0.001,max=1,min=0),      
          numericInput("t1",label =HTML("<b><big>&theta;</big></b><small><br>(Expected success proportion of sample)</small>"),
                       value = 0.8,step=0.001,max=1,min=0),
          numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                       value = 0.9,step=0.05,max=1,min=0),
          hr()
          )
          } 
          else if (input$type_binary==2) { # one-arm Non-inf/sup
            list(
            numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),
            numericInput("p0",label =HTML("<b><big>&theta;<sub>0</sub></big></b><small><br>(Known success proportion)</small>"),
                         value = 0.8,step=0.001,max=1,min=0),
            numericInput("p",label =HTML("<b><big>&theta;</big></b>"),
                         value = 0.6,step=0.001,max=1,min=0),
            numericInput("dif",label =HTML("<b><big>&delta;</big></b>"),
                         value = -0.1,step=0.001,max=1,min=-1),
            numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                         value = 0.9,step=0.05,max=1,min=0),
            hr()
            )
            
          }
          else if (input$type_binary==3) { #one-arm Bioequivalence
            list(
              numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                           value = 0.05,step=0.01,max=1,min=0),
              numericInput("p0",label =HTML("<b><big>&theta;<sub>0</sub></big></b><small><br>(Known success proportion)</small>"),
                           value = 0.8,step=0.001,max=1,min=0),
              numericInput("p",label =HTML("<b><big>&theta;</big></b>"),
                           value = 0.6,step=0.001,max=1,min=0),
              numericInput("eq_limit",label =HTML("<b><big>&delta;</big></b>"),
                           value = 0.1,step=0.001,max=1,min=0),
              numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                           value = 0.9,step=0.05,max=1,min=0),
              hr()
            )
          }
          else if (input$type_binary==4) { # two-arm Equality
            list(
            numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                        value = 0.05,step=0.01,max=1,min=0),
            numericInput("p1",label =HTML("<b><big>&theta;<sub>1</sub></big></b>"),
                        value = 0.6,step=0.001,max=1,min=0),
            numericInput("p2",label =HTML("<b><big>&theta;<sub>2</sub></big></b>"),
                        value = 0.8,step=0.001,max=1,min=0),
            numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                        value = 0.9,step=0.05,max=1,min=0),
            numericInput("ratio",label =HTML("<b><big>r </big>(n<sub>2</sub>/n<sub>1</sub>)</b>"),
                        value = 1.0,step=0.1,max=10,min=0.1),
            selectInput("var_type",label = HTML("<b>Variance Assumption</b>"),selected = 1,width = '300px',
                        choices =list("Different variances (Default)"=1,"Same variances"=2) ),
            hr()
          )
        } 
          else if (input$type_binary==5) { # two-arm Non-inf/sup
          list(
            numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),
            numericInput("p1",label =HTML("<b><big>&theta;<sub>1</sub></big></b>"),
                         value = 0.6,step=0.001,max=1,min=0),
            numericInput("p2",label =HTML("<b><big>&theta;<sub>2</sub></big></b>"),
                         value = 0.8,step=0.001,max=1,min=0),
            numericInput("dif",label =HTML("<b><big>&delta;</big></b>"),
                         value = 0.1,step=0.001,max=1,min=-1),
            numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                         value = 0.9,step=0.05,max=1,min=0),
            numericInput("ratio",label =HTML("<b><big>r </big>(n<sub>2</sub>/n<sub>1</sub>)</b>"),
                         value = 1.0,step=0.1,max=10,min=0.1),
            hr()
          ) 
        }
          else if (input$type_binary==6){ # two-arm Equivalence
            list(
              numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                           value = 0.05,step=0.01,max=1,min=0),
              numericInput("p1",label =HTML("<b><big>&theta;<sub>1</sub></big></b>"),
                           value = 0.6,step=0.001,max=1,min=0),
              numericInput("p2",label =HTML("<b><big>&theta;<sub>2</sub></big></b>"),
                           value = 0.8,step=0.001,max=1,min=0),
              numericInput("eq_limit",label =HTML("<b><big>&delta;</big></b>"),
                           value = 0.1,step=0.001,max=1,min=0),
              numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                           value = 0.9,step=0.05,max=1,min=0),
              numericInput("ratio",label =HTML("<b><big>r </big>(n<sub>2</sub>/n<sub>1</sub>)</b>"),
                           value = 1.0,step=0.1,max=10,min=0.1),
              hr()
            ) 
          }
      }
    })
    ### binary -> output
    output$sam_bin <- renderUI({
      validate(need(input$type_binary != 0 & input$alpha>0 & input$power>0, " "))
      if(input$data_type==1 ){
        if (input$type_binary==1){ # 1 sample test for equality
        temp1_n<-((qnorm(1-input$alpha/2)+qnorm(input$power))^2)*input$t1*(1-input$t1)/(input$t1-input$t2)^2  ##
        HTML( ("<P><PRE><font color=#000000 size=3 face=Times New Roman>&#8594; N  = "),ceiling(temp1_n),
              (".</font></P></PRE>") )
        } 
        else if (input$type_binary==2){ # 1 sample test for non-inferiority/superior
        temp2_n<- ((qnorm(1-input$alpha)+qnorm(input$power))^2)*(input$p*(1-input$p))/((input$p-input$p0)-input$dif)^2
        HTML( ("<P><PRE><font color=#000000 size=3 face=Times New Roman>&#8594; N  = "),ceiling(temp2_n),
              (".</font></P></PRE>") )
        } 
        else if (input$type_binary==3){
          temp3_n<-((qnorm(1-input$alpha)+qnorm(0.5+input$power/2))^2)*input$p*(1-input$p)/(input$eq_limit-abs(input$p-input$p0))^2
          HTML( ("<P><PRE><font color=#000000 size=3 face=Times New Roman>&#8594; N  = "),ceiling(temp3_n),
                (".</font></P></PRE>") )
        }
        else if (input$type_binary==4){ # 2 sample test for equality
          validate(need(input$var_type != 0, " "))
          if(input$var_type==1) { # Unequal variance(Default)
          p_pool<-(input$p1+(input$p2*input$ratio))/(input$ratio+1)
          temp42_n1<-((qnorm(input$alpha/2)*sqrt((1+input$ratio)*p_pool*(1-p_pool)/input$ratio))+(qnorm(1-input$power)*sqrt(input$ratio*input$p1*(1-input$p1)+input$p2*(1-input$p2))))^2/(input$p1-input$p2)^2
          HTML( ("<P><PRE><font color=#000000 size=3 face=Times New Roman>&#8594; N  = "),ceiling(temp42_n1*input$ratio)+ceiling(temp42_n1),
                ("<br>&#8594; n<sub>1 </sub>= "),ceiling(temp42_n1),
                ("<br>&#8594; n<sub>2 </sub>= "),ceiling(temp42_n1*input$ratio),
                (".</font></P></PRE>") )
          } 
          else if (input$var_type==2){ # Equal variance
          p_pool<-(input$p1+(input$p2*input$ratio))/(input$ratio+1)
          temp43_n1<-((qnorm(input$alpha/2)+qnorm(1-input$power))^2)*((input$ratio+1)*p_pool*(1-p_pool)/input$ratio)/(input$p1-input$p2)^2
          HTML(  ("<P><PRE><font color=#000000 size=3 face=Times New Roman>&#8594; N  = "),ceiling(temp43_n1*input$ratio)+ceiling(temp43_n1),
                 ("<br>&#8594; n<sub>1 </sub>= "),ceiling(temp43_n1),
                 ("<br>&#8594; n<sub>2 </sub>= "),ceiling(temp43_n1*input$ratio),
                 (".</font></P></PRE>") )
          }     
    } 
        else if (input$type_binary==5){ # 2 sample test for non-inferiority/superior
          temp5_n2<-((qnorm(1-input$alpha)+qnorm(input$power))^2)*(((input$p1*(1-input$p1))/input$ratio)+input$p2*(1-input$p2)) / ((input$p1-input$p2)-input$dif)^2
          temp5_n1<-temp5_n2*input$ratio
          HTML( ("<P><PRE><font color=#000000 size=3 face=Times New Roman>&#8594; N  = "),ceiling(temp5_n1)+ceiling(temp5_n2),
                ("<br>&#8594; n<sub>1 </sub>= "),ceiling(temp5_n1),
                ("<br>&#8594; n<sub>2 </sub>= "),ceiling(temp5_n2),
                (".</font></P></PRE>") )
        }
        else if (input$type_binary==6) {
          temp6_n2<-((qnorm(1-input$alpha)+qnorm(0.5+input$power/2))^2)*((input$p1*(1-input$p1)/input$ratio)+input$p2*(1-input$p2)) / (input$eq_limit-abs(input$p1-input$p2))^2
          temp6_n1<-temp6_n2*input$ratio
          HTML( ("<P><PRE><font color=#000000 size=3 face=Times New Roman>&#8594; N  = "),ceiling(temp6_n1)+ceiling(temp6_n2),
                ("<br>&#8594; n<sub>1 </sub>= "),ceiling(temp6_n1),
                ("<br>&#8594; n<sub>2 </sub>= "),ceiling(temp6_n2),
                (".</font></P></PRE>") )
        }
        else if (input$type_binary==7) {
          
        }
    }
    })
    ### binary -> manual
    output$bin_def<-renderUI({
      validate(need(input$type_binary != 0, ""))
      if(input$data_type==1){
        if (input$type_binary==1) { 
          list(
          HTML("<font color=#000000 face=Times New Roman><big><big>Two-sided Test for Equality (One-sample Proportion)</big></big></font>" ),br(),
          HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 84-88.</font>" ),br(),br(),          
          HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
          withMathJax( helpText(" $$ { H_0:\\theta-\\theta_0 =0 } $$" ) ),
          withMathJax( helpText(" $$ { H_0:\\theta-\\theta_0 \\neq0 } $$"  )),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
          withMathJax( helpText(" $${N=}\\frac{( Z_{\\alpha/2}+Z_{\\beta} )^2 [\\theta(1-\\theta)]}{(\\theta-\\theta_0)^2}$$" ) ),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&theta; : Expected success proportion of sample.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>0</sub> : Known success proportion of sample.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" ) 
          )
        }
        else if (input$type_binary==2) {
          list(
            HTML("<font color=#000000 face=Times New Roman><big><big>One-sided Test for Non-Inferiority/Superiority (One-sample Proportion)</big></big></font>" ),br(),
            HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 84-88.</font>" ),br(),br(),                 
            HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
            withMathJax( helpText(" $${ H_0:\\theta-\\theta_0 \\le \\delta } $$" ) ),
            withMathJax( helpText(" $${ H_0:\\theta-\\theta_0 \\gt \\delta } $$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
            withMathJax( helpText(" $${N=}\\frac{( Z_{\\alpha}+Z_{\\beta} )^2 [\\theta(1-\\theta)]}{(\\theta-\\theta_0-\\delta)^2}$$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta; : Expected success proportion of sample.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>0</sub> : Known success proportion of sample.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&delta; : The superiority margin(&delta;>0) or the non-inferiority margin(&delta;<0)</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" ) 
          )
        }
        else if (input$type_binary==3) {
          list(
            HTML("<font color=#000000 face=Times New Roman><big><big>One-sided Test for Equivalence (One-sample Proportion)</big></big></font>" ),br(),
            HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 84-88.</font>" ),br(),br(),          
            HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
            withMathJax( helpText(" $${ H_0:|\\theta-\\theta_0| \\ge\\delta } $$" ) ),
            withMathJax( helpText(" $${ H_0:|\\theta-\\theta_0| \\lt\\delta } $$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
            withMathJax( helpText(" $${N=}\\frac{( Z_{\\alpha}+Z_{\\beta/2} )^2 [\\theta(1-\\theta)]}{(\\delta-|\\theta-\\theta_0|)^2} $$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta; : Expected success proportion of sample.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>0</sub> : Known success proportion of sample.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&delta; : Equivalence limit.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" ) 
          )
        }
        else if (input$type_binary==4) {
          validate(need(input$var_type != 0, "Please choose the variance assumption."))
          if (input$var_type==1){
            list(
            HTML("<font color=#000000 face=Times New Roman><big><big>Two-sided Test for Equality (Two-sample Proportion)</big></big></font>" ),br(),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
            withMathJax( helpText(" $${ H_0:\\theta_1-\\theta_2 = 0 } $$" ) ),
            withMathJax( helpText(" $${ H_0:\\theta_1-\\theta_2 \\neq 0 } $$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
            withMathJax( helpText("$${\\theta=}\\frac{\\theta_1+r\\theta_2}{1+r}$$" ) ),
            withMathJax( helpText("$${n_2=rn_1 }$$" ) ),br(),
            withMathJax( helpText(" $${n_1=}\\frac{[ Z_{\\alpha/2}\\sqrt{\\frac{(r+1)}{r}\\theta(1-\\theta)}+Z_{\\beta}\\sqrt{\\theta_1(1-\\theta_1)+\\frac{1}{r}\\theta_2(1-\\theta_2)}]^2}{(\\theta_1-\\theta_2)^2}$$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>1</sub> : Expected success proportions of sample one.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>2</sub> : Expected success proportions of sample two.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>n<sub>1</sub> : Sample size of sample one.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>n<sub>2</sub> : Sample size of sample two.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>r : allocation ratio (n<sub>1</sub> /n<sub>2</sub> ).</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta; : Pooled success proportions.</big></font><br><hr>" )
            )}
          else if (input$var_type==2) {
            list(
            HTML("<font color=#000000 face=Times New Roman><big><big>One-sided Test for Equality (Two-sample Proportion)</big></big></font>" ),br(),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),br(),
            withMathJax( helpText(" $${ H_0:\\theta_1-\\theta_2 = 0 } $$" ) ),
            withMathJax( helpText(" $${ H_0:\\theta_1-\\theta_2 \\neq 0 } $$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
            withMathJax( helpText("$${\\theta=}\\frac{\\theta_1+r\\theta_2}{1+r}$$" ) ),
            withMathJax( helpText("$${n_2=rn_1 }$$" ) ),br(),
            withMathJax( helpText(" $${n_1=}\\frac{ (Z_{\\alpha/2}+Z_{\\beta})^2[\\frac{r+1}{r}\\theta(1-\\theta)]}{(\\theta_1-\\theta_2)^2}$$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>1</sub> : Expected success proportions of sample one.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>2</sub> : Expected success proportions of sample two.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>n<sub>1</sub> : Sample size of sample one.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>n<sub>2</sub> : Sample size of sample two.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>r : allocation ratio (n<sub>1</sub> /n<sub>2</sub> ).</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta; : Pooled success proportions.</big></font><br><hr>" )
          )
        }
        }
        else if (input$type_binary==5) {
          list(
            HTML("<font color=#000000 face=Times New Roman><big><big>One-sided Test for Non-Inferiority/Superiority (Two-sample Proportion)</big></big></font>" ),br(),
            HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 89-95.</font>" ),br(),br(),            
            HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),br(),
            withMathJax( helpText(" $${ H_0:\\theta_1-\\theta_2 \\le \\delta } $$" ) ),
            withMathJax( helpText(" $${ H_0:\\theta_1-\\theta_2 \\gt \\delta } $$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),br(),
            withMathJax( helpText("$${n_1=rn_2 }$$" ) ),br(),
            withMathJax( helpText(" $${n_2=}\\frac{ (Z_{\\alpha}+Z_{\\beta})^2}{(\\theta_1-\\theta_2-\\delta)^2} [\\frac{\\theta_1(1-\\theta_1)}{r}+\\theta_2(1-\\theta_2)]$$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>1</sub> : Expected success proportions of sample one.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>2</sub> : Expected success proportions of sample two.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&delta; : Equivalence limit.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>n<sub>1</sub> : Sample size of sample one.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>n<sub>2</sub> : Sample size of sample two.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>r : Allocation ratio (n<sub>1</sub> /n<sub>2</sub> ).</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" )
            
            )
        }
        else if (input$type_binary==6) {
          list(
            HTML("<font color=#000000 face=Times New Roman><big><big>One-sided Test for Equivalence (Two-sample Proportion)</big></big></font>" ),br(),
            HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 89-95.</font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),br(),
            withMathJax( helpText(" $${ H_0:|\\theta_1-\\theta_2| \\ge \\delta } $$" ) ),
            withMathJax( helpText(" $${ H_0:|\\theta_1-\\theta_2| \\lt \\delta } $$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),br(),
            withMathJax( helpText("$${n_1=rn_2 }$$" ) ),br(),
            withMathJax( helpText(" $${n_2=}\\frac{ (Z_{\\alpha}+Z_{\\beta/2})^2}{(\\delta-|\\theta_1-\\theta_2|)^2} [\\frac{\\theta_1(1-\\theta_1)}{r}+\\theta_2(1-\\theta_2)]$$" ) ),br(),
            HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>1</sub> : Expected success proportions of sample one.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&theta;<sub>2</sub> : Expected success proportions of sample two.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>&delta; : Equivalence limit.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>n<sub>1</sub> : Sample size of sample one.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>n<sub>2</sub> : Sample size of sample two.</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>r : Allocation ratio (n<sub>1</sub> /n<sub>2</sub> ).</big></font>" ),br(),br(),
            HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" )
          )
        }
      }
    })

### Quantitative Data
    ### continuous -> input items
    output$mode_quant <- renderUI({
      validate(need(input$data_type != 0, ""))
      if(input$data_type==2){
        if (input$type_quant==1){ # one-arm equality
          list(
            numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),
            numericInput("dif_mean",label =HTML("<b><big>&mu;-&mu;<sub>0</sub></big></b><small><br>(Difference in means)</small>"),
                         value = 0.5,step=0.001,max=Inf,min=0),
            numericInput("pop_var",label =HTML("<b><big>&sigma;<sup>2</sup></big></b>"),
                         value = 1,step=0.001,max=Inf,min=0),
            numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                         value = 0.9,step=0.05,max=1,min=0),
            hr()
            )
        }
        else if (input$type_quant==2){ # one-arm Sup/Inf
          list(
            numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),
            numericInput("dif_mean",label =HTML("<b><big>&mu;-&mu;<sub>0</sub></big></b><small><br>(Difference in means)</small>"),
                         value = 0.5,step=0.001,max=Inf,min=0),
            numericInput("pop_var",label =HTML("<b><big>&sigma;<sup>2</sup></big></b>"),
                         value = 1,step=0.001,max=Inf,min=0),
            numericInput("margin",label =HTML("<b><big>&delta;</big></b>"),
                         value = 0.5,step=0.001,max=Inf,min=0),
            numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                         value = 0.9,step=0.05,max=1,min=0),
            hr()
          )
        }
        else if (input$type_quant==3){ #one-arm Equivalence
          list(
            numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),
            numericInput("dif_mean",label =HTML("<b><big>&mu;-&mu;<sub>0</sub></big></b><small><br>(Difference in means)</small>"),
                         value = 0.5,step=0.001,max=Inf,min=0),
            numericInput("pop_var",label =HTML("<b><big>&sigma;<sup>2</sup></big></b>"),
                         value = 1,step=0.001,max=Inf,min=0),
            numericInput("margin",label =HTML("<b><big>&delta;</big></b>"),
                         value = 0.5,step=0.001,max=Inf,min=0),
            numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                         value = 0.9,step=0.05,max=1,min=0),
            hr()
          )
        }
        else if (input$type_quant==4){
          list(
            numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),
            numericInput("dif_mean",label =HTML("<b><big>&mu;<sub>2</sub>-&mu;<sub>1</sub></big></b><small><br>(Difference in means)</small>"),
                         value = 0.5,step=0.001,max=Inf,min=0),
            numericInput("pop_var",label =HTML("<b><big>&sigma;<sup>2</sup></big></b>"),
                         value = 1,step=0.001,max=Inf,min=0),
            numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                         value = 0.9,step=0.05,max=1,min=0),
            hr()
          )
        }
        else if (input$type_quant==5){
          list(
            numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),
            numericInput("dif_mean",label =HTML("<b><big>&mu;<sub>2</sub>-&mu;<sub>1</sub></big></b><small><br>(Difference in means)</small>"),
                         value = 0.5,step=0.001,max=Inf,min=0),
            numericInput("pop_var",label =HTML("<b><big>&sigma;<sup>2</sup></big></b>"),
                         value = 1,step=0.001,max=Inf,min=0),
            numericInput("margin",label =HTML("<b><big>&delta;</big></b>"),
                         value = 0.5,step=0.001,max=Inf,min=0),
            numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                         value = 0.9,step=0.05,max=1,min=0),
            hr()
          )
        }
        else if (input$type_quant==6){
          list(
            numericInput("alpha", label = HTML("<b>Type I error (&alpha;)</b>"),
                         value = 0.05,step=0.01,max=1,min=0),
            numericInput("dif_mean",label =HTML("<b><big>&mu;<sub>2</sub>-&mu;<sub>1</sub></big></b><small><br>(Difference in means)</small>"),
                         value = 0.5,step=0.001,max=Inf,min=0),
            numericInput("pop_var",label =HTML("<b><big>&sigma;<sup>2</sup></big></b>"),
                         value = 1,step=0.001,max=Inf,min=0),
            numericInput("margin",label =HTML("<b><big>&delta;</big></b>"),
                         value = 0.5,step=0.001,max=Inf,min=0),
            numericInput("power",label = HTML("<b>Power (1-&beta;)</b>"),
                         value = 0.9,step=0.05,max=1,min=0),
            hr()
          )
        }
      }
    })
    ### continuous -> outputs

    ### continuous -> manuals
    output$quant_def<-renderUI({
    validate(need(input$type_quant != 0, ""))
    if(input$data_type==2){
      if (input$type_quant==1) {
        list(
          HTML("<font color=#000000 face=Times New Roman><big><big>Two-sided Test for Equality (One-sample Mean)</big></big></font>" ),br(),
          HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 50-57.</font>" ),br(),br(),          
          HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
          withMathJax( helpText(" $$ { H_0:\\mu-\\mu_0 =0 } $$" ) ),
          withMathJax( helpText(" $$ { H_0:\\mu-\\mu_0 \\neq0 } $$"  )),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
          withMathJax( helpText(" $$ {N=}\\frac{(Z_{\\alpha/2}+Z_\\beta)^2\\sigma^2}{(\\mu-\\mu_0)^2} $$" ) ),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu; : Mean response of sample.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>0</sub> : Reference value.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&sigma;<sup>2</sup> : Population variance.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" ) 
        )
      }
      else if (input$type_quant==2) {
        list(
          HTML("<font color=#000000 face=Times New Roman><big><big>One-sided Test for Non-Inferiority/Superiority (One-sample Mean)</big></big></font>" ),br(),
          HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 50-57.</font>" ),br(),br(),           
          HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
          withMathJax( helpText(" $$ { H_0:\\mu-\\mu_0 \\le\\delta } $$" ) ),
          withMathJax( helpText(" $$ { H_0:\\mu-\\mu_0 \\gt\\delta } $$"  )),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
          withMathJax( helpText(" $$ {N=}\\frac{(Z_{\\alpha}+Z_\\beta)^2\\sigma^2}{(\\mu-\\mu_0-\\delta)^2} $$" )  ),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu; : Mean response of sample.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>0</sub> : Reference value.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&sigma;<sup>2</sup> : Population variance.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&delta; : The superiority margin(&delta;>0) or the non-inferiority margin(&delta;<0).</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" ) 
        )
      }
      else if (input$type_quant==3) {
        list(
          HTML("<font color=#000000 face=Times New Roman><big><big>One-sided Test for Equivalence (One-sample Mean)</big></big></font>" ),br(),
          HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 50-57.</font>" ),br(),br(),          
          HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
          withMathJax( helpText(" $$ { H_0:|\\mu-\\mu_0| \\ge\\delta } $$" ) ),
          withMathJax( helpText(" $$ { H_0:|\\mu-\\mu_0| \\lt\\delta } $$"  )),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
          withMathJax( helpText(" $$ {N=}\\frac{(Z_{\\alpha}+Z_{\\beta/2})^2\\sigma^2}{(\\delta-|\\mu-\\mu_0|)^2} $$" ) ),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu; : Mean response of sample.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>0</sub> : Reference value.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&sigma;<sup>2</sup> : Population variance.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&delta; : Equivalence limit.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" ) 
        )
      }
      else if (input$type_quant==4) {
        list(
          HTML("<font color=#000000 face=Times New Roman><big><big>Two-sided Test for Equality (Two-sample Means)</big></big></font>" ),br(),
          HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 57-65.</font>" ),br(),br(),          
          HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
          withMathJax( helpText(" $${ H_0:\\mu_1-\\mu_2 = 0 } $$" ) ),
          withMathJax( helpText(" $${ H_0:\\mu_1-\\mu_2 \\neq 0 } $$" ) ),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
          withMathJax( helpText("$$  $$" ) ),
          withMathJax( helpText("$${n_1=rn_2 }$$" ) ),br(),
          withMathJax( helpText(" $$ {n_2=}\\frac{(Z_{\\alpha/2}+Z_\\beta)^2\\sigma^2(1+\\frac{1}{r})}{(\\mu_2-\\mu_1)^2} $$" ) ),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>1</sub> : Mean response of control group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>2</sub> : Mean response of test group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>1</sub>-&mu;<sub>2</sub> : .</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&sigma;<sup>2</sup> : Population variance.</big></font>" ),br(),br(),          
          HTML("<font color=#000000 face=Times New Roman><big>n<sub>1</sub> : Sample size of control group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>n<sub>2</sub> : Sample size of test group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>r : Allocation ratio (n<sub>1</sub> /n<sub>2</sub> ).</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" )
        )
      }
      else if (input$type_quant==5) {
        list(
          HTML("<font color=#000000 face=Times New Roman><big><big>One-sided Test for Non-Inferiority/Superiority (Two-sample Means)</big></big></font>" ),br(),
          HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 57-65.</font>" ),br(),br(),          
          HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
          withMathJax( helpText(" $${ H_0:\\mu_1-\\mu_2 \\le\\delta } $$" ) ),
          withMathJax( helpText(" $${ H_0:\\mu_1-\\mu_2 \\gt\\delta } $$" ) ),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
          withMathJax( helpText("$$  $$" ) ),
          withMathJax( helpText("$${n_1=rn_2 }$$" ) ),br(),
          withMathJax( helpText(" $$ {n_2=}\\frac{(Z_{\\alpha}+Z_\\beta)^2\\sigma^2(1+\\frac{1}{r})}{(\\mu_2-\\mu_1-\\delta)^2} $$" ) ),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>1</sub> : Mean response of control group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>2</sub> : Mean response of test group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>1</sub>-&mu;<sub>2</sub> : .</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&sigma;<sup>2</sup> : Population variance.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&delta; : The superiority margin(&delta;>0) or the non-inferiority margin(&delta;<0).</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>n<sub>1</sub> : Sample size of control group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>n<sub>2</sub> : Sample size of test group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>r : Allocation ratio (n<sub>1</sub> /n<sub>2</sub> ).</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" )
        )
      }
      else if (input$type_quant==6) {
        list(
          HTML("<font color=#000000 face=Times New Roman><big><big>One-sided Test for Equivalence (Two-sample Means)</big></big></font>" ),br(),
          HTML("<font color=#6C6C6C face=Times New Roman>Chow, Shao and Wang (2003). Sample Size Calculations In Clinical Research, 57-65.</font>" ),br(),br(),          
          HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
          withMathJax( helpText(" $${ H_0:|\\mu_1-\\mu_2| \\ge\\delta } $$" ) ),
          withMathJax( helpText(" $${ H_0:|\\mu_1-\\mu_2| \\lt\\delta } $$" ) ),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
          withMathJax( helpText("$$  $$" ) ),
          withMathJax( helpText("$${n_1=rn_2 }$$" ) ),br(),
          withMathJax( helpText(" $$ {n_2=}\\frac{(Z_{\\alpha}+Z_{\\beta/2})^2\\sigma^2(1+\\frac{1}{r})}{(\\delta-|\\mu_2-\\mu_1|)^2} $$" ) ),br(),
          HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>1</sub> : Mean response of control group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>2</sub> : Mean response of test group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&mu;<sub>1</sub>-&mu;<sub>2</sub> : .</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&sigma;<sup>2</sup> : Population variance.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>&delta; : Equivalence limit.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>n<sub>1</sub> : Sample size of control group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>n<sub>2</sub> : Sample size of test group.</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>r : Allocation ratio (n<sub>1</sub> /n<sub>2</sub> ).</big></font>" ),br(),br(),
          HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" )
        )
      }
    }
    })

### Survival data
    ### survival -> input items
    output$mode_surv<-renderUI({
      validate(need(input$data_type != 0, ""))
      if(input$data_type==3){
        validate(need(input$type_surv != 0, ""))
        if (input$type_surv==1){
          list(
            #HTML("<font color=#5B00AE face=Times New Roman><big><big><b>Comparison of two survival curves</b></big></big></font>"),br(),
            #HTML("<font color=#000000 face=Times New Roman><big>Shoenfeld & Richter (1982)</big></font>" ), br(),br(),
            radioButtons("alpha0",label=HTML("<b>Type I error (&alpha;)--for two-sided test</b>"),selected = 0,  
                         choices =list("0.05"=1,"0.1"=2,"others"=3) , inline = T ),
            numericInput("power",label =HTML("<b>Power (1-&beta;)</b>"),
                         value = 0.8,step=0.001,max=1,min=0),
            radioButtons("unit",label=HTML("<b>The unit of time</b>"),selected = 1,  
                         choices =list("Months"=1,"Years"=2) , inline = T ),
            numericInput("r",label =HTML("<b>Recruitment duration (R)</b>"),
                         value = 2,step=0.01,max=80,min=0),      
            numericInput("f",label =HTML("<b>Follow-up duration (F)</b>"),
                         value = 10,step=0.01,max=80,min=0),  
            numericInput("mc",label =HTML("<b>Median survival time of the control group</b><br>(months)"),
                         value = 3,step=0.01,max=80,min=0),
            numericInput("ar",label =HTML("<b>Allocation ratio (experimental to control group) (r)</b>"),   #<b>Q<sub>C</sub></b>"
                         value = 1,step=0.0001,max=7,min=0.001),br(), 
            numericInput("hr",label =HTML("<b>Detectable hazard ratio of control group relative to experimental group (&Delta;)</b>"),
                         value = 1.5,step=0.0001,max=7,min=0)
          )
        }
        else if (input$type_surv==3){ #type_surv==2
          validate(need(input$prop_choice != 0, ""))
          if(input$prop_choice==1){
            list(
              #HTML("<font color=#5B00AE face=Times New Roman><big><big><b>Comparison of two survival curves</b></big></big></font>"),br(),
              #HTML("<font color=#000000 face=Times New Roman><big>Shoenfeld (1983)</big></font>" ),br(), br(),          
              radioButtons("alpha0",label=HTML("<b>Type I error (&alpha;)--for one-sided test</b>"),selected = 0,  
                           choices =list("0.05"=1,"0.1"=2,"others"=3) , inline = T ),
              numericInput("power",label =HTML("<b>Power (1-&beta;)</b>"),
                           value = 0.8,step=0.001,max=1,min=0),
              numericInput("r",label =HTML("<b>Recruitment duration (R)</b>"),
                           value = 2,step=0.01,max=80,min=0),      
              numericInput("f",label =HTML("<b>Follow-up duration (F)</b>"),
                           value = 10,step=0.01,max=80,min=0), 
              numericInput("ar",label =HTML("<b>Allocation ratio (experimental to control group) (r)</b>"),   #<b>Q<sub>C</sub></b>"
                           value = 1,step=0.0001,max=7,min=0.0001),
              numericInput("hr",label =HTML("<b>Detectable hazard ratio of control group relative to experimental group (&Delta;)</b>"),
                           value = 1.5,step=0.001,max=7,min=0),br(),
              HTML("<font color=#5B00AE face=Times New Roman><big>  Survival function value of the control group</big></font>" ),br(),        
              numericInput("surv_f", label = HTML("<b>(a)  at the recruitment duration (S<sub>C</sub>(R))</b>"),
                           value = 0.09921253,step=0.00000001,max=1,min=0),
            numericInput("surv_f0.5r", label = HTML("<b>(b)  at the follow-up duration plus 0.5 times of the recruitment duration (S<sub>C</sub>(F+0.5R))</b>"),
                           value = 0.07874503,step=0.00000001,max=1,min=0),
            numericInput("surv_fr", label = HTML("<b>(c)  at the length of trial (S<sub>C</sub>(F+R))</b>"),
                           value = 0.06249997,step=0.00000001,max=1,min=0)
            )
          }
          else if(input$prop_choice==2){
            validate(need(input$surv_dist != 0, ""))   #
            if (input$surv_dist==1){
              list(
                #HTML("<font color=#5B00AE face=Times New Roman><big><big><b>Comparison of two survival curves</b></big></big></font>"),br(),
                #HTML("<font color=#000000 face=Times New Roman><big>Shoenfeld (1983)</big></font>" ),br(), br(),          
                radioButtons("alpha0",label=HTML("<b>Type I error (&alpha;)--for one-sided test</b>"),selected = 0,  
                             choices =list("0.05"=1,"0.1"=2,"others"=3) , inline = T ),
                numericInput("power",label =HTML("<b>Power (1-&beta;)</b>"),
                             value = 0.8,step=0.001,max=1,min=0),
                numericInput("rate_exp", label = HTML("<b>Rate (&lambda;)</b><br>( parameter of exponetial distribution )"),
                             value = 0.2310491,step=0.0000001,max=20,min=0),
                radioButtons("unit",label=HTML("<b>The unit of time</b>"),selected = 1,  
                             choices =list("Months"=1,"Years"=2) , inline = T ),
                numericInput("r",label =HTML("<b>Recruitment duration (R)</b>"),
                             value = 2,step=0.01,max=80,min=0),      
                numericInput("f",label =HTML("<b>Follow-up duration (F)</b>"),
                             value = 10,step=0.01,max=80,min=0),  
                numericInput("ar",label =HTML("<b>Allocation ratio (experimental to control group) (r)</b>"),   #<b>Q<sub>C</sub></b>"
                             value = 1,step=0.0001,max=7,min=0.0001),
                numericInput("hr",label =HTML("<b>Detectable hazard ratio of control group relative to experimental group (&Delta;)</b>"),
                             value = 1.5,step=0.0001,max=7,min=0)
              )
            }
            else if (input$surv_dist==2){
              list(
                #HTML("<font color=#5B00AE face=Times New Roman><big><big><b>Comparison of two survival curves</b></big></big></font>"),br(),
                #HTML("<font color=#000000 face=Times New Roman><big>Shoenfeld (1983)</big></font>" ),br(), br(),          
                radioButtons("alpha0",label=HTML("<b>Type I error (&alpha;)--for one-sided test</b>"),selected = 0,  
                             choices =list("0.05"=1,"0.1"=2,"others"=3) , inline = T ),
                numericInput("power",label =HTML("<b>Power (1-&beta;)</b>"),
                             value = 0.8,step=0.001,max=1,min=0),
                numericInput("shape_gam", label = HTML("<b>Shape (&gamma;)</b><br>( parameter of gamma distribution )"),
                             value = 2,step=1,max=30,min=0),
                numericInput("rate_gam", label = HTML("<b>Rate (&lambda;)</b><br>( parameter of gamma distribution )"),
                             value = 3,step=0.001,max=30,min=0),                
                radioButtons("unit",label=HTML("<b>The unit of time</b>"),selected = 1,  
                             choices =list("Months"=1,"Years"=2) , inline = T ),
                numericInput("r",label =HTML("<b>Recruitment duration (R)</b>"),
                             value = 6,step=0.01,max=80,min=0),      
                numericInput("f",label =HTML("<b>Follow-up duration (F)</b>"),
                             value = 12,step=0.01,max=80,min=0),  
                numericInput("ar",label =HTML("<b>Allocation ratio (experimental to control group) (r)</b>"),   #<b>Q<sub>C</sub></b>"
                             value = 1,step=0.0001,max=7,min=0.001),
                numericInput("hr",label =HTML("<b>Detectable hazard ratio of control group relative to experimental group (&Delta;)</b>"),
                             value = 1.5,step=0.0001,max=7,min=0)
              )
            }
            else if (input$surv_dist==3){
              list(
                #HTML("<font color=#5B00AE face=Times New Roman><big><big><b>Comparison of two survival curves</b></big></big></font>"),br(),
                #HTML("<font color=#000000 face=Times New Roman><big>Shoenfeld (1983)</big></font>" ),br(), br(),          
                radioButtons("alpha0",label=HTML("<b>Type I error (&alpha;)--for one-sided test</b>"),selected = 0,  
                             choices =list("0.05"=1,"0.1"=2,"others"=3) , inline = T ),
                numericInput("power",label =HTML("<b>Power (1-&beta;)</b>"),
                             value = 0.8,step=0.001,max=1,min=0),
                numericInput("shape_wei", label = HTML("<b>Shape (<i>a</i>)</b><br>( parameter of weibull distribution )"),
                             value = 1,step=1,max=30,min=0),
                numericInput("scale_wei", label = HTML("<b>Scale (<i>b</i>)</b><br>( parameter of weibull distribution )"),
                             value = 2,step=1,max=30,min=0),  
                radioButtons("unit",label=HTML("<b>The unit of time</b>"),selected = 1,  
                             choices =list("Months"=1,"Years"=2) , inline = T ),
                numericInput("r",label =HTML("<b>Recruitment duration (R)</b>"),
                             value = 6,step=0.01,max=80,min=0),      
                numericInput("f",label =HTML("<b>Follow-up duration (F)</b>"),
                             value = 12,step=0.01,max=80,min=0),   
                numericInput("ar",label =HTML("<b>Allocation ratio (experimental to control group) (r)</b>"),   #<b>Q<sub>C</sub></b>"
                             value = 1,step=0.0001,max=7,min=0.001),
                numericInput("hr",label =HTML("<b>Detectable hazard ratio of control group relative to experimental group (&Delta;)</b>"),
                             value = 1.5,step=0.0001,max=7,min=0)
              )
            }
          }
        }
      }
    })
      # Proportion hazard (3) --Assumed distributuin or not ?
      output$mod_surv_prop<-renderUI({
      validate(need(input$type_surv!=0 , ""))
      if(input$data_type==3 & input$type_surv==3){ # Proportional-hazards regression model # type_surv==2
        radioButtons("prop_choice",label=HTML("<font color=003D79 face=Times New Roman><big><b>Do you want to use a specific distribution of survival times?</b></big></font>"),selected = 0,  
                       choices =list("Unspecific distribution"=1,"Specific distribution"=2), inline = T )           
      }    
    })
      # Surv distribution
      output$mod_surv_dist<-renderUI({
      validate(need(input$type_surv!=0 , ""))
      if(input$data_type==3){
        if(input$type_surv==3){ #type_surv==2
          validate(need(input$prop_choice!=0 , ""))
          if(input$prop_choice==2){
            radioButtons("surv_dist",label=HTML("<font color=003D79 face=Times New Roman><big><b>Distribution of survival times of the control group</b></big></font>"),selected = 0,  
                         choices =list("Exponetial distribution"=1,"Gamma distribution"=2,"Weibull distribution"=3),inline = T )       
          }
        }     
      }    
    })
      # alpha 排序問題!!! 
      output$choice_alpha<-renderUI({
      validate(need(input$alpha0!=0 , ""))
      if(input$alpha0==3){
        numericInput("alpha1", label = HTML("<b>Type I error (&alpha;)</b>"),
                     value = 0.025,step=0.0001,max=1,min=0) 
      }    
    })
        
    ### survival -> output
    output$sam_surv <- renderUI({       
      validate(need(input$type_surv != 0, ""))
      if(input$data_type==3){
        if(input$type_surv==0){
          paste("n = ?")
        }
        else if (input$type_surv==1){           
          
          validate(need(input$unit != 0, ""))
          if(input$unit==1){ f=input$f ; r=input$r ; mc=input$mc }
          else if(input$unit==2){ f=input$f*12 ; r=input$r*12 ; mc=input$mc*12 }
          
          h_c=log(2)/mc  ; h_e=h_c/input$hr  
          p_c=1-( exp(-h_c*f)-exp(-h_c*(f+r) ) )/(r*h_c)
          p_e=1-( exp(-h_e*f)-exp(-h_e*(f+r) ) )/(r*h_e)
          
          validate(need(input$alpha0 != 0, ""))
          if(input$alpha0==1){alp=0.05}
          else if(input$alpha0==2){alp=0.1}
          else if(input$alpha0==3){alp=input$alpha1}
          
          surv_nc=( qnorm(1-alp/2) + qnorm(input$power) )^2 *( 1/p_c+1/(p_e*input$ar) ) / ( log(input$hr)^2 )
          surv_ne=( qnorm(1-alp/2) + qnorm(input$power) )^2 *( input$ar/p_c + 1/p_e ) / ( log(input$hr)^2 )
          n_event=( qnorm(1-alp/2) + qnorm(input$power) )^2 / ( log(input$hr)^2 )*2 *(1+input$ar)^2 /input$ar
          #Result
          HTML( ("<P><PRE><font color=#000000 size=3 face=Times New Roman>With the allocation ratio (experimental group to control group)<u><strong> "),input$ar, 
                (" </strong></u>, significant level<u><strong> "),alp,
                (" </strong></u>of the 2-sided test with power<u><strong> "),input$power,
                (" </strong></u>.  Assume the recruitment duration of<u><strong> "),input$r,
                (" </strong></u>time units and follow-up duration of<u><strong> "),input$f,
                (" </strong></u>time units. If the median survival time of the control group known to be<u><strong> "),input$mc,
                (" </strong></u>time units and the detectable hazard ratio of control group relative to experimental group is<u><strong> "),input$hr,
                (" </strong></u>.<br><font color=#0000C6 size=3 face=Times New Roman>Then a total of<u><strong> "),round(surv_nc)+round(surv_ne),                
                (" </strong></u>patients are required in this two treatment parallel-design study, and the total number of events is<u><strong> "),round(n_event),
                (" </strong></u><font color=#005AB5 size=3.5 face=Times New Roman>. <br><u><strong> "),round(surv_nc),
                (" </strong></u>patients and<u><strong> "),round(surv_ne),
                (" </strong></u>patients will be randomized to control group and experimental group.</PRE></P>") 
          )  
        }
        else if (input$type_surv==3){  #type_surv==2  
          validate(need(input$prop_choice != 0, ""))
          if (input$prop_choice==1){
            
            d_c=  round( 1-( input$surv_f  + 4*input$surv_f0.5r  + input$surv_fr ) /6  ,2)  #
            d_e=  round( 1-(1-d_c)^(1/input$hr) , 2)   #
            d=  (d_c+d_e*input$ar)/(1+input$ar)       
            
            validate(need(input$alpha0 != 0, ""))
            if(input$alpha0==1){alp=0.05}
            else if(input$alpha0==2){alp=0.1}
            else if(input$alpha0==3){alp=input$alpha1}
            n_event=  ( qnorm(1-alp) +  qnorm(input$power) )^2 / (input$ar/(1+input$ar)^2 *log(input$hr)^2)
            n_surv= n_event / d
            HTML( ("<P><PRE><font color=#000000 size=3 face=Times New Roman>With the allocation ratio (experimental group to control group)<u><strong> "),input$ar, 
                  (" </strong></u>, significant level<u><strong> "),alp,
                  (" </strong></u>of the 1-sided test with power<u><strong> "),input$power,
                  (" </strong></u>. Assume the recruitment duration of<u><strong> "),input$r,
                  (" </strong></u>time units and follow-up duration of<u><strong> "),input$f,
                  (" </strong></u>time units. If the detectable hazard ratio of control group relative to experimental group is<u><strong> "),input$hr,("</strong></u>."),
                  ("<br><font color=#0000C6 size=3 face=Times New Roman>Then a total of<u><strong> "),round(n_surv/(1+input$ar))+round(n_surv*input$ar/(1+input$ar)),                
                  (" </strong></u>patients are required in this two treatment parallel-design study, and the total number of events is<u><strong> "),round(n_event),
                  (" </strong></u><font color=#005AB5 size=3 face=Times New Roman>.<br><u><strong> "),round(n_surv/(1+input$ar)),
                  (" </strong></u>patients and<u><strong> "),round(n_surv*input$ar/(1+input$ar)),
                  (" </strong></u>patients will be randomized to control group and experimental group.</font></P></PRE>") 
            )  
          }
          else if (input$prop_choice==2){
            validate(need(input$surv_dist != 0, ""))
            if (input$surv_dist==1){
              
              validate(need(input$unit != 0, ""))
              if(input$unit==1){ f=input$f ; r=input$r ; mc=input$mc }
              else if(input$unit==2){ f=input$f*12 ; r=input$r*12 ; mc=input$mc*12 }
              
              d_c=  1-( ( 1-pexp(f,input$rate_exp) ) + 4*( 1-pexp(f+0.5*r,input$rate_exp) ) +  ( 1-pexp(f+r,input$rate_exp) )  ) /6  
              d_e=  1-(1-d_c)^(1/input$hr)
              d=  (d_c+d_e*input$ar)/(1+input$ar)
              validate(need(input$alpha0 != 0, ""))
              if(input$alpha0==1){alp=0.05}
              else if(input$alpha0==2){alp=0.1}
              else if(input$alpha0==3){alp=input$alpha1}
              n_event = ( qnorm(1-alp) +  qnorm(input$power) )^2 /(  input$ar/(1+input$ar)^2 *log(input$hr)^2)
              n_exp= n_event / d 
              HTML( ("<font color=#000000 size=3.5 face=Times New Roman><big>With the allocation ratio"),input$ar, 
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>(experimental group to control group ), significant level"),alp,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>of the 2-sided test with power"),input$power,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>. Assume the recruitment duration of"),input$r,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>time units and follow-up duration of"),input$f,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>time units. If the detectable hazard ratio of control group relative to experimental group is "),input$hr,
                    ("<font color=#0000C6 size=3.5 face=Times New Roman><big>Then a total of "),round(n_exp/(1+input$ar))+round(n_exp*input$ar/(1+input$ar)),                
                    ("<font color=#0000C6 size=3.5 face=Times New Roman><big>patients are required in this two treatment parallel-design study, and the total number of events is"),round(n_event),
                    ("<font color=#005AB5 size=3.5 face=Times New Roman><big>."),round(n_exp/(1+input$ar)),
                    ("<font color=#005AB5 size=3.5 face=Times New Roman><big>patients and "),round(n_exp*input$ar/(1+input$ar)),
                    ("<font color=#005AB5 size=3.5 face=Times New Roman><big>patients will be randomized to control group and experimental group.") 
              )           
            }  
            else if (input$surv_dist==2) {
              
              validate(need(input$unit != 0, ""))
              if(input$unit==1){ f=input$f ; r=input$r ; mc=input$mc }
              else if(input$unit==2){ f=input$f*12 ; r=input$r*12 ; mc=input$mc*12 }
              
              d_c=  1-( ( 1-pgamma(f,input$shape_gam,input$rate_gam) ) + 4*( 1-pgamma(f+0.5*r,input$shape_gam,input$rate_gam) ) +  ( 1-pgamma(f+r,input$shape_gam,input$rate_gam) )  ) /6  
              d_e=  1-(1-d_c)^(1/input$hr)
              d=  (d_c+d_e*input$ar)/(1+input$ar)
              validate(need(input$alpha0 != 0, ""))
              if(input$alpha0==1){alp=0.05}
              else if(input$alpha0==2){alp=0.1}
              else if(input$alpha0==3){alp=input$alpha1}
              n_event = ( qnorm(1-alp) +  qnorm(input$power) )^2 /(  input$ar/(1+input$ar)^2 *log(input$hr)^2)
              n_gam= n_event / d
              HTML( ("<font color=#000000 size=3.5 face=Times New Roman><big>With the allocation ratio"),input$ar, 
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>(experimental group to control group ), significant level"),alp,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>of the 2-sided test with power"),input$power,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>. Assume the recruitment duration of"),input$r,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>time units and follow-up duration of"),input$f,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>time units. If the detectable hazard ratio of control group relative to experimental group is "),input$hr,
                    ("<font color=#0000C6 size=3.5 face=Times New Roman><big>Then a total of "),round(n_gam/(1+input$ar))+round(n_gam*input$ar/(1+input$ar)),                
                    ("<font color=#0000C6 size=3.5 face=Times New Roman><big>patients are required in this two treatment parallel-design study, and the total number of events is"),round(n_event),
                    ("<font color=#005AB5 size=3.5 face=Times New Roman><big>."),round(n_gam/(1+input$ar)),
                    ("<font color=#005AB5 size=3.5 face=Times New Roman><big>patients and "),round(n_gam*input$ar/(1+input$ar)),
                    ("<font color=#005AB5 size=3.5 face=Times New Roman><big>patients will be randomized to control group and experimental group.") 
              )  
            }  
            else if (input$surv_dist==3) {  
              validate(need(input$unit != 0, ""))
              if(input$unit==1){ f=input$f ; r=input$r ; mc=input$mc }
              else if(input$unit==2){ f=input$f*12 ; r=input$r*12 ; mc=input$mc*12 }
              
              d_c=  1-( ( 1-pweibull(f,shape=input$shape_wei,scale=input$scale_wei) ) + 4*( 1-pweibull(f+0.5*r,input$shape_wei,input$scale_wei) ) +  ( 1-pweibull(f+r,input$shape_wei,input$scale_wei) )  ) /6  
              d_e=  1-(1-d_c)^(1/input$hr)
              d=  (d_c+d_e*input$ar)/(1+input$ar)
              validate(need(input$alpha0 != 0, ""))
              if(input$alpha0==1){alp=0.05}
              else if(input$alpha0==2){alp=0.1}
              else if(input$alpha0==3){alp=input$alpha1}
              
              n_event = ( qnorm(1-alp) +  qnorm(input$power) )^2 /(  input$ar/(1+input$ar)^2 *log(input$hr)^2)
              n_wei= n_event / d
              HTML( ("<font color=#000000 size=3.5 face=Times New Roman><big>With the allocation ratio"),input$ar, 
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>(experimental group to control group ), significant level"),alp,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>of the 2-sided test with power"),input$power,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>. Assume the recruitment duration of"),input$r,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>time units and follow-up duration of"),input$f,
                    ("<font color=#000000 size=3.5 face=Times New Roman><big>time units. If the detectable hazard ratio of control group relative to experimental group is "),input$hr,
                    ("<font color=#0000C6 size=3.5 face=Times New Roman><big>Then a total of "),round(n_wei/(1+input$ar))+round(n_wei*input$ar/(1+input$ar)),                
                    ("<font color=#0000C6 size=3.5 face=Times New Roman><big>patients are required in this two treatment parallel-design study, and the total number of events is"),round(n_event),
                    ("<font color=#005AB5 size=3.5 face=Times New Roman><big>."),round(n_wei/(1+input$ar)),
                    ("<font color=#005AB5 size=3.5 face=Times New Roman><big>patients and "),round(n_wei*input$ar/(1+input$ar)),
                    ("<font color=#005AB5 size=3.5 face=Times New Roman><big>patients will be randomized to control group and experimental group.") 
              )  
            } 
          }
        }
      }
    })

    ### survival -> manual
    output$surv_def<-renderUI({
      validate(need(input$type_surv!=0 , ""))
      if (input$data_type==3){
        if (input$type_surv==1){
          
          list(HTML("<font color=#5B00AE face=Times New Roman><big><big><b>Comparison of two survival curves</b></big></big></font>") ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>Schoenfeld, D. A., & Richter, J. R. (1982). Nomograms for calculating the number of patients needed for a clinical trial with survival as an endpoint. Biometrics, 163-170</big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
               withMathJax( helpText(" $${ H_0:\\lambda_C=\\lambda_E } $$" ) ),
               withMathJax( helpText(" $${ H_a:\\lambda_C\\neq\\lambda_E } $$" ) ),
               HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),           
               withMathJax( helpText(" $${N=}\\frac{( Z_{\\alpha/2}+Z_{\\beta} )^2}{(ln\\Delta)^2} ( \\frac{1}{P_C Q_C} +\\frac{1}{P_E Q_E} )
                                     =\\frac{( Z_{\\alpha/2}+Z_{\\beta} )^2}{(ln\\Delta)^2} ( \\frac{1+r}{P_C} +\\frac{1+r}{rP_E} )\\!$$ ")) ,br(),
               HTML("<font color=#000000 face=Times New Roman><big><b>III. Notations</b></big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>&lambda;<sub>C</sub> : Hazard ratio of the control group.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>&lambda;<sub>E</sub> : Hazard ratio of the experimental group.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>Q<sub>C</sub> : Proportion of the control group.</big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>Q<sub>E</sub> : Proportion of the experimental group.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>r : Allocation ratio (experimental to control group).</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>&Delta; : Detectable hazard ratio of control group relative to experimental group.</big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>P<sub>C</sub> : Estimated probability of event of the control group.</big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>P<sub>E</sub> : Estimated probability of event of the experimental group.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" )      
               
               )        
        }
        else if (input$type_surv==3){ #type_surv==2
          list(HTML("<font color=#5B00AE face=Times New Roman><big><big><b>Comparison of two survival curves</b></big></big></font>"),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>Schoenfeld, D. A. (1983). Sample-size formula for the proportional-hazards regression model. Biometrics, 499-503.</big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>There are two situations for the distribution of survival times, unknow and known. There are three common dostribution of the survival times, the explanation of parameters is list below.</big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big><b>I. Hypothesis</b></big></font>" ),br(),
               HTML("<font color=#000000 face=Times New Roman><big>Schoenfeld (1983) supposed to perform a one-side test, so the hypothesis is</big></font>" ),br(), 
               withMathJax( helpText(" $${ H_0:\\lambda_C\\leq\\lambda_E } $$" ) ),
               withMathJax( helpText(" $${ H_a:\\lambda_C>\\lambda_E } $$" ) ),
               
               HTML("<font color=#000000 face=Times New Roman><big><b>II. Formula</b></big></font>" ),br(),
               withMathJax( helpText(" $${N=}\\frac{( Z_{\\alpha/2}+Z_{\\beta} )^2}{Q_C Q_E (ln\\Delta)^2 P}=\\frac{( Z_{\\alpha/2}+Z_{\\beta})^2(1+r)^2}{r(ln\\Delta)^2 P} \\!$$ ")) ,br(), 
               withMathJax( helpText(" $${P=} Q_C P_C+Q_E P_E = \\frac{P_C+rP_E}{1+r}$$ ")) , 
               withMathJax( helpText(" $${P_C=}1-\\frac{1}{6}( S_C(F)+S_C(F+0.5R)+S_C(F+R) )\\!$$ ")) ,
               withMathJax( helpText(" $${P_E=}1-(1-P_C)^{1/{\\Delta}} \\!$$ ")) , 
               
               HTML("<font color=#000000 face=Times New Roman><big><b>III. The distribution of the survival times (t)</b></big></font>" ),br(),br(),
               HTML("<font color=003D79 face=Times New Roman><big>(i) Exponential distribution </big></font>" ),br(),
               withMathJax( helpText(" $${f(t)=}{ \\lambda e^{-\\lambda t }} \\!$$ ")) , br(),
               HTML("<font color=#000000 face=Times New Roman><big>  &lambda; is the rate of exponential distribution </big></font>" ),br(),br(),
               
               HTML("<font color=003D79 face=Times New Roman><big>(ii) Gamma distribution</b></big></font>" ),br(),
               withMathJax( helpText(" $${f(t)=} \\frac{ {\\lambda^\\gamma} t^{\\gamma-1} e^{-\\lambda t } }{\\Gamma(\\gamma) }  \\!$$ ")) , 
               HTML("<font color=#000000 face=Times New Roman><big>   &gamma; is the shape of gamma distribution </big></font>" ), br(),
               HTML("<font color=#000000 face=Times New Roman><big>   &lambda; is the rate of gamma distribution </big></font>" ),br(),br(),
               
               HTML("<font color=003D79 face=Times New Roman><big>(iii) Weibull distribution</big></font>" ),br(),
               withMathJax( helpText(" $${f(t)=} \\frac{a}{b}(\\frac{t}{b})^{a-1} e^{-{\\frac{t}{b}}^a } \\!$$ ")) ,br(),               
               HTML("<font color=#000000 face=Times New Roman><big>   <i>a</i> is the shape of weibull distribution </big></font>" ),br(),
               HTML("<font color=#000000 face=Times New Roman><big>   <i>b</i> is the scale of weibull distribution </big></font>" ),br(),br(),
               
               HTML("<font color=#000000 face=Times New Roman><big><b>IV. Notations</b></big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>&#945; : (Type I error) the probability of rejecting the true null hypothesis.</big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>1-&#946; : (Power) the probability of rejecting the false null hypothesis.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>&lambda;<sub>C</sub> : Hazard ratio of the control group.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>&lambda;<sub>E</sub> : Hazard ratio of the experimental group.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>Q<sub>C</sub> : Proportion of the control group.</big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>Q<sub>E</sub> : Proportion of the experimental group.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>r : Allocation ratio (experimental to control group).</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>&Delta; : Detectable hazard ratio of control group relative to experimental group. </big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>P : Estimated probability of event.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>P<sub>C</sub> : Estimated probability of event of the control group.</big></font>" ),br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>P<sub>E</sub> : Estimated probability of event of the experimental group.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>S<sub>C</sub>(<i>t</i>) : Survival function at time t of the control group.</big></font>" ) ,br(),br(),
               HTML("<font color=#000000 face=Times New Roman><big>N : Total sample size.</big></font><br><hr>" )
          )  
        }
      }    
    })


  }
)
