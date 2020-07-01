library(shiny)
library(gdata)
library(rpart)
library(rpart.plot)
library(R.matlab)
library(R.oo)
library(R.methodsS3)
library(R.utils)
library(session)
library(htmltools)
library(tidyverse)
library(reshape)
library(shinythemes)
library(plm)
library(MASS)
library(Hmisc)
library(Amelia)
library(ggplot2)


sidebarPanel2 <- function (..., out = NULL,out1 = NULL,out2 = NULL,out3 = NULL, out4 = NULL,out5 = NULL,width = 4) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out,
      out1,
      out2,
      out3,
      out4,
      out5 
      
  )
}


shinyUI(pageWithSidebar(
  
  
  
  
  tags$a(
    href="https://www.gov.uk/government/organisations/department-for-international-development", 
    tags$img(src="dfid-ukaid.png",
             title="To DFID Website", 
             width="100",
             height="140",
             style="float:left;margin-top:-15px"
             
             
    ),
    
tags$a(href="https://datasciencecampus.ons.gov.uk/projects/faster-indicators-of-uk-economic-activity-shipping/", tags$h1("FASTER ECONOMIC INDICATORS",style="white-space:nowrap;color:blue;overflow:hidden;text-overflow:ellipsis;margin-left:120px;position:fixed")),

     
    tags$a(
      href="https://www.gov.uk/", 
      tags$img(src="UKFlag.png",
               title="To UK Government's website", 
               width="100",
               height="90",
               style="float:right"
               
              
               )
      
      
      
    )
    
    ),
  
  
  
  
  
  
  
  sidebarPanel2(fluid = FALSE,
    selectInput("dataset","Choose Criterion:",
                list("PortTraffic" ="pt",
                     "IMFvalue"="imf",
                     "NightLight"="rad",
                     "Pollution"="polut",
                     "AirTraffic"="at"
                     
                )),
    
    downloadButton("downloadData","Download"),
    
    out = h5("Project Members:"),
    out1 = h6("Tom Wilson"),
    out2 = h6("Tom Wilkinson"),
    out3 = h6("Rueben Laryea"),
    out4 = h6("Saliha Minhas"),
    out5 = h6("Rachel Barret")
    
    
  ),




  
  mainPanel(
    
    
    tags$script("Shiny.addCustomMessageHandler('messageBox', function(msg){window.alert(msg);})"),  
    tabsetPanel(id="tabSelected",
                tabPanel("BarCharts",plotOutput("plot")),
                tabPanel("CriterionData",tableOutput("table")),
                tabPanel("View all DataSet",tableOutput("alltable")),
                tabPanel("TimeSeries",plotOutput("Timeseries")),
                tabPanel("Models",verbatimTextOutput("models")),
                #tabPanel("View Decision Tree",plotOutput("plotree")),
                #tabPanel("View UTADIS output",verbatimTextOutput("utadisweight")),
                #tabPanel("Criteria Risk Visualisation",plotOutput("utadisvis")),
                tabPanel("Prediction",uiOutput("glmpred"), uiOutput("predictbutton"))
                
    )
    
  )

))
