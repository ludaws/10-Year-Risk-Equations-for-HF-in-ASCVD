
library(shiny); library(shinythemes); library(ggplot2);library(dplyr)

ui <- fluidPage(
    title ="ASCVD-HF Risk Equations",
    theme=shinytheme("cerulean"),
    tags$head(tags$style(HTML('.title {background-color: #337ab7; color: white;}'))),
    titlePanel(h1("10-year risk of HF with and without ASCVD", style = "font-weight:bold", class = "title")),
    sidebarLayout(
        sidebarPanel(
            tags$div(
                style = "background-color: #e7f3fe; color: #31708f; padding: 10px; border-radius: 5px; margin-bottom: 10px;", # Adjusted CSS for subtler blue and better text visibility
                h4("Does the patient have known atherosclerotic cardiovascular disease?*", style = "margin: 0 0 10px 0;"), # Reduced margin to tighten spacing
                radioButtons("ascvd", label = NULL, choices = c("Yes (ASCVD-HF model)" = "1","No (PCP-HF model)" = "0"), inline = TRUE) # Inline radio buttons
            ),
            fluidRow(
                column(4, "Age"),column(8, numericInput("age", NULL, value = 50, min = 18, max = 100))),
            fluidRow(
                column(4, "Sex"),column(8, radioButtons("sex", NULL, c("Male" = "0", "Female" = "1"), inline = TRUE))),
            fluidRow(
                column(4, "Body Mass Index"),column(8, numericInput("bmi", NULL, value = sprintf("%.1f", 25), min = 12, max = 100))),
            fluidRow(
                column(4, "Systolic Blood Pressure"),column(8, numericInput("sbp", NULL, value = 120, min = 60, max = 250))),
            fluidRow(
                column(4, "BP Medication"),column(8, radioButtons("bpmed", NULL, c("No" = "0", "Yes" = "1"),inline=TRUE)) ),
            fluidRow(
                column(4, "Total Cholesterol"),column(8, numericInput("tch", NULL, value = sprintf("%.1f",5.0), min = 0, max = 20))),
            fluidRow(
                column(4, "HDL-C"),column(8, numericInput("hdl", NULL, value = sprintf("%.1f",1.0), min = 0, max = 10))),
            fluidRow(
                column(4, "Glucose"),column(8, numericInput("gluc", NULL, sprintf("%.1f",value = 5.0), min = 0, max = 50))),
            fluidRow(
                column(4, "Glucose-lowering medication"),column(8, radioButtons("dmmed", NULL, c("No" = "0", "Yes" = "1"),inline=TRUE))),
            fluidRow(
                column(4, "QRS duration"),column(8, numericInput("qrs", NULL, value = 90, min = 25, max = 400))),
            fluidRow(
                column(4, "Smoker"),column(8, radioButtons("smoker", NULL, c("Not currently" = "0", "Current" = "1"),inline=TRUE))),
            conditionalPanel(
                condition = "input.ascvd == '1'", # Only show these rows if ASCVD is "Yes"
                fluidRow(
                    column(4, "Kidney disease"), column(8, radioButtons("ckd", NULL, c("No" = "0", "Yes" = "1"), inline = TRUE))
                ),
                fluidRow(
                    column(4, "Myocardial Infarction"), column(8, radioButtons("mi", NULL, c("No" = "0", "Yes" = "1"), inline = TRUE))
                ),
                fluidRow(
                    column(4, "Atrial fibrillation"), column(8, radioButtons("af", NULL, c("No" = "0", "Yes" = "1"), inline = TRUE))
                )
            ),
            conditionalPanel(
                condition = "input.ascvd == '0'", # Only show these rows if ASCVD is "No"
                fluidRow(
                    column(4, "Ethnicity"), column(8, radioButtons("ethnicity", NULL, c("White/European" = "0", "Black/African-American" = "1"), inline = TRUE))
                ),
            ),      
        ),
        mainPanel(
            htmlOutput("result"), 
            br(), br(), br(),
            htmlOutput("personrisk"),   
            div(
                style = "display: flex; justify-content: center;",
                plotOutput("grid", width = "500px", height = "400px")
            ),
            br(),
            htmlOutput("text") ,
            br(),
            htmlOutput("text2") 
      )
        
        
    ),

    fluidRow(column(width=12,align="left","The ASCVD-HF score was developed using data from the UK Biobank with external validation using data from the Australian Baker Biobank. The output % estimates above are derived from a cox regression model using the formula 1-S0^e^(Prognostic Index) where S0 is the predicted incident HF free survival at 10-years and the prognostic index is the intercept minus the sum of the coefficients. Discrimination in the development cohort was very good in the development cohort (C-statistic: Women 0.776, Men 0.728) and very good in the validation cohort (C-statistic: Women 0.753, Men: 0.711). The score is designed for use in outpatient settings for patients with known ASCVD to assist in stratification of HF risk, and guide screening approaches (e.g. echocardiography) and future trials for preventative treatments. Continuous variables are set to only include values from specific ranges (e.g. age must be between 18 and 90 years). Further details can be found in the original publication at [link]."))

)

# Define server
server <- function(input, output, session) {

    observeEvent(input$age,{ if(input$age > 90) {updateNumericInput(session, "age", value = 90)}})
    observeEvent(input$age,{ if(input$age <18) {updateNumericInput(session, "age", value = 18)}})
    observeEvent(input$sbp,{ if(input$sbp <60) {updateNumericInput(session, "sbp", value = 60)}})
    observeEvent(input$sbp,{ if(input$sbp > 250) {updateNumericInput(session, "sbp", value = 250)}})
    observeEvent(input$bmi,{ if(input$bmi > 50) {updateNumericInput(session, "bmi", value = 50)}})
    observeEvent(input$bmi,{ if(input$bmi <15) {updateNumericInput(session, "bmi", value = 15)}})
    observeEvent(input$tch,{ if(input$tch > 20) {updateNumericInput(session, "tch", value = 20)}})
    observeEvent(input$tch,{ if(input$tch <0.5) {updateNumericInput(session, "tch", value = 0.5)}})
    observeEvent(input$hdl,{ if(input$hdl > 10) {updateNumericInput(session, "hdl", value = 10)}})
    observeEvent(input$hdl,{ if(input$hdl <0.3) {updateNumericInput(session, "hdl", value = 0.3)}})
    observeEvent(input$gluc,{ if(input$gluc > 50) {updateNumericInput(session, "gluc", value = 50)}})
    observeEvent(input$gluc,{ if(input$gluc <1) {updateNumericInput(session, "gluc", value = 1)}})
    observeEvent(input$qrs,{ if(input$qrs > 250) {updateNumericInput(session, "qrs", value = 250)}})
    observeEvent(input$qrs,{ if(input$qrs <40) {updateNumericInput(session, "qrs", value = 40)}})
    
   risk<- reactive({
       if (input$ascvd == "1") {
           if (input$sex == "1") {
            risk<-100*(1-0.9525498^exp(0.1813484
                        +0.028444038* input$age+0.00011539319*pmax(input$age-49,0)^3-0.00036541177*pmax(input$age-62,0)^3+0.00025001858*pmax(input$age-68,0)^3
                        -0.024132958* input$bmi+0.00037424987*pmax(input$bmi-21.7859,0)^3-0.00061421437*pmax(input$bmi-27.449,0)^3+0.0002399645*pmax(input$bmi-36.2812,0)^3
                        -0.0040948284*input$sbp+0.0000036299045*pmax(input$sbp-115,0)^3-0.0000066116118*pmax(input$sbp-138,0)^3+0.0000029817073*pmax(input$sbp-166,0)^3
                        +0.98031532*(input$bpmed=="1")
                        -0.40868003*input$tch+0.062412963*pmax(input$tch-2.9328,0)^3-0.1258134*pmax(input$tch-3.8117,0)^3+0.055258131*pmax(input$tch-4.5055,0)^3+0.0081423032*pmax(input$tch-5.8402,0)^3
                        -0.1819672*input$hdl
                        +0.035257084*input$gluc+0.95425965*(input$dmmed=="1")
                        -0.0016906411*input$qrs+0.000023236784*pmax(input$qrs-72,0)^3-0.000038727974*pmax(input$qrs-84,0)^3+0.00001549119*pmax(input$qrs-102,0)^3
                        +0.56556036*(input$smoker=="1")
                        +0.55386321*(input$ckd=="1")
                        +0.43270194*(input$mi=="1")
                        +0.69475319*(input$af=="1")
                        +(input$bpmed=="1")*(-0.0059769373*input$sbp+0.00000015914563*pmax(input$sbp-115,0)^3-0.0000002898724*pmax(input$sbp-138,0)^3+0.00000013072677*pmax(input$sbp-166,0)^3)
                        -0.017058892*input$gluc*(input$dmmed=="1")))}
           else {risk<-100*(1-0.9030972^exp(-4.0181619
                        +0.036472416*input$age+0.000057268162*pmax(input$age-52,0)^3-0.00018325812*pmax(input$age-63,0)^3+0.00012598996*pmax(input$age-68,0)^3
                        +0.011588699*input$bmi+0.00035717001*pmax(input$bmi-23.7032,0)^3-0.00061163921*pmax(input$bmi-28.3126,0)^3+0.00025446921*pmax(input$bmi-34.7823,0)^3
                        -0.012861481*input$sbp+0.0000056952496*pmax(input$sbp-118,0)^3-0.000010733355*pmax(input$sbp-141,0)^3+0.0000050381054*pmax(input$sbp-167,0)^3
                        -0.53550676*(input$bpmed=="1")
                        +0.023941294*input$tch+0.0099732889*pmax(input$tch-2.6635,0)^3-0.017057666*pmax(input$tch-3.524,0)^3+0.0070843776*pmax(input$tch-4.7354,0)^3
                        +0.07304053*input$hdl
                        +0.084025983*input$gluc
                        +0.83430763*(input$dmmed=="1")
                        +0.017246086*input$qrs-0.0000002754859*pmax(input$qrs-80,0)^3+0.00000035419616*pmax(input$qrs-92,0)^3-0.000000078710258*pmax(input$qrs-134,0)^3
                        +0.48919679*(input$smoker=="1")
                        +0.67573205*(input$ckd=="1")
                        +0.54065135*(input$mi=="1")
                        +0.67748546*(input$af=="1")
                        +(input$bpmed=="1")*(0.0054988738*input$sbp-0.0000012885996*pmax(input$sbp-118,0)^3+0.0000024285146*pmax(input$sbp-141,0)^3-0.000001139915*pmax(input$sbp-167,0)^3)
                        -0.031279035*input$gluc*(input$dmmed=="1")))}}
           else {
               if (input$sex == "0") {
                   if (input$ethnicity == "0") {
                       risk<- 100*(1 - (0.98752^exp(
                           41.94 * log(input$age) 
                           - 0.88*log(input$age)*log(input$age) 
                           + log(input$sbp)*(ifelse(input$bpmed=="1",1.03,0.91)) 
                           + 0.74*(ifelse(input$smoker=="1",1,0))
                           + log(18*input$gluc)*(ifelse(input$dmmed=="1",0.90,0.78))  
                           + 0.49*log(38.67*input$tch)
                           - 0.44*log(38.67*input$hdl)
                           + 37.2*log(input$bmi)
                           - 8.83*log(input$age)*log(input$bmi)
                           + log(input$qrs)*0.63 - 171.5)))
                   } else {
                       risk<- 100*(1 - (0.98295^exp(
                           2.88 * log(input$age) 
                           + 2.31*log(input$sbp)*(ifelse(input$bpmed=="1",1,0)) 
                           + 2.17*log(input$sbp)*(ifelse(input$bpmed=="0",1,0)) 
                           + 1.66*(ifelse(input$smoker=="1",1,0))
                           - 0.25*log(input$age)*(ifelse(input$smoker=="1",1,0)) 
                           + 0.64*log(18*input$gluc)*(ifelse(input$dmmed=="1",1,0))  
                           + 0.58*log(18*input$gluc)*(ifelse(input$dmmed=="0",1,0)) 
                           - 0.81*log(38.67*input$hdl)
                           + 1.16*log(input$bmi)
                           + log(input$qrs)*0.73 - 28.73)))
                   }
               } else if (input$ethnicity == "0") {
                   risk<-100*(1 - (0.99348^exp(
                       20.55 * log(input$age) 
                       + 12.95*log(input$sbp)*(ifelse(input$bpmed=="1",1,0)) 
                       - 2.96*log(input$age)*log(input$sbp)*(ifelse(input$bpmed=="1",1,0))
                       + 11.86*log(input$sbp)*(ifelse(input$bpmed=="0",1,0)) 
                       - 2.73*log(input$age)*log(input$sbp)*(ifelse(input$bpmed=="0",1,0)) 
                       + 11.02*(ifelse(input$smoker=="1",1,0))
                       - 2.5*log(input$age)*(ifelse(input$smoker=="1",1,0))
                       + 1.04*log(18*input$gluc)*(ifelse(input$dmmed=="1",1,0))  
                       + 0.91*log(18*input$gluc)*(ifelse(input$dmmed=="0",1,0)) 
                       - 0.07*log(38.67*input$hdl)
                       + 1.33*log(input$bmi)
                       + 1.06*log(input$qrs) - 99.73)))
               }
                  else {
                      risk<-100*(1 - (0.99260^exp(
                          51.75 * log(input$age) 
                          + 29.0*log(input$sbp)*(ifelse(input$bpmed=="1",1,0)) 
                          - 6.59*log(input$age)*log(input$sbp)*(ifelse(input$bpmed=="1",1,0))
                          + 28.18*log(input$sbp)*(ifelse(input$bpmed=="0",1,0)) 
                          - 6.42*log(input$age)*log(input$sbp)*(ifelse(input$bpmed=="0",1,0)) 
                          + 0.76*(ifelse(input$smoker=="1",1,0))
                          + 0.97*log(18*input$gluc)*(ifelse(input$dmmed=="1",1,0))  
                          + 0.80*log(18*input$gluc)*(ifelse(input$dmmed=="0",1,0)) 
                          + 0.32*log(38.67*input$tch)
                          + 21.24*log(input$bmi)
                          - 5.0*log(input$age)* log(input$bmi)
                          + 1.27*log(input$qrs) - 233.9 )))
                  }
               }
        round(risk, digits = 1)
    })
    
   output$personrisk <- renderUI({
       if (input$ascvd == "1") {
           HTML(paste("<font size='5'>For a person with ASCVD and this risk profile, an estimated <b>", round(risk()), " out of 100 people</b> will have a hospital admission for heart failure at 10-years</font>"))
       } else {
           HTML(paste("<font size='5'>For a person without ASCVD and this risk profile, an estimated <b>", round(risk()), " out of 100 people</b> will have a hospital admission for heart failure at 10-years</font>"))
       }
   })
    output$result <- renderUI({
        HTML(paste("<b><font size='6.5'>Estimated 10-year risk of HF =<font/></b>", risk(), "%","</font>"))
        
    })
    output$grid <- renderPlot({
        # Calculate number of dots to color blue
        num_dots <- round(risk())
        
        # Generate 10x10 grid
        dots <- expand.grid(x = 1:10, y = 10:1)
        
        dots$color <- dots %>%
            mutate(row_num = row_number()) %>%
            mutate(color = ifelse(row_num <= num_dots, "red", "grey")) %>%
            pull(color)
        
        # Plot grid using ggplot2
        ggplot(dots, aes(x, y, fill = color)) +
            geom_point(shape = 21, size = 14) +
            scale_fill_identity() +
            theme_void() +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.margin = unit(c(12, 12, 12, 12), "pt"),
                plot.background = element_rect(fill = "transparent", color = NA),
                panel.spacing = unit(c(0, 0, 0, 0), "pt")
            )
    })
    output$text <- renderUI({
        if (input$ascvd == "1") {
            HTML(paste("<font size='4'>Atherosclerotic Cardiovascular Disease Heart Failure (ASCVD-HF) model used to calculate results among people with ASCVD.</font>"))
        } else {
            HTML(paste("<font size='4'>Pooled Cohort Equation Heart Failure (PCP-HF) model used to calculate results among people without ASCVD. Note results are calculated using the coefficients the published manuscript (https://www.jacc.org/doi/10.1016/j.jacc.2019.02.057) and results may differ slightly from the online PCP-HF calculator available here (https://hf-risk-calculator.surge.sh/).</font>"))
        }
         })
    output$text2 <- renderUI({    
    HTML(paste("<font size='3' color='#31708f'><b>*Atherosclerotic cardiovascular disease (ASCVD) includes known coronary disease (including prior stents or bypass surgery), prior myocardial infarction, prior stroke, and peripheral vascular disease.</b></font>"))
    })
    
}

shinyApp(ui = ui, server = server)
