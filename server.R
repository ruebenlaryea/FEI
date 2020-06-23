library(shiny)
library(gdata)
library(rpart)
library(rpart.plot)
library(R.matlab)
library(R.oo)
library(R.methodsS3)
library(R.utils)
library(session)
library(tidyverse)
library(reshape)
library(plm)
library(MASS)

#FEIdata <- read.xls("port_traffic_imf_exports.xlsx")
FEIdata <- read.csv("port_traffic_imf_exports.csv",sep = ",")
#panel regression using plm, fixed country effect
lm_imf <- plm(imf.val ~ port.traffic, data = FEIdata, index=c("country.code", "month.yr"))
#generalised linear models
glmodel <- glm(imf.val ~ port.traffic + country.name, data=FEIdata)
#summarise models in funtion below
#View(FEIdata)
#hivclasses <- read.xls("HIVClasses.xlsx")
#fit2 <- rpart(Classes~ Maize + Potato + Cassava + WealthGinIndex, method = 'class', data = hivclasses,control =rpart.control(minsplit =1,minbucket=1, cp=0))
#wt <- readMat("marginal_utils.mat")
#wt1 <- wt$marginal.utils
#wt2 <- wt1[4]
#numargpie <- sapply(wt2, function(x) as.numeric(as.character(x)))
#mumaize <- numargpie[1]
#mupotato <- numargpie[2]
#mucassava <- numargpie[3]
#muwgi <- numargpie[4]

#threshdata <- readMat("util_thresholds.mat")
#thresh <- threshdata$util.thresholds
#threshlevel <- thresh[1]

#accuraciesdata <- readMat("accuracies.mat")


utilityFunction <- function(x) {
  
  tags$script("Shiny.addCustomMessageHandler('messageBox', function(msg){window.alert(msg);})")
  
}

shinyServer(function(input,output,session){
  
  data <-   reactive({
    switch(input$dataset,
           pt=FEIdata$port.traffic,
           mt=FEIdata$month.yr,
           cn=FEIdata$country.name,
           cc=FEIdata$country.code,
           imf=FEIdata$imf.val,
           mn=FEIdata$month,
           yr=FEIdata$year,
           n<-nlevels(cn)
           
    )
    
  })
  
  # Generate a table of the dataset
  output$table <- renderTable({
    
    data.frame(x=data())
    
  })
  
  # Generate a table of the dataset
  output$alltable <- renderTable({
    
    FEIdata
    
  }) 
  
  
  
  output$plot <- renderPlot({
    n<-nlevels(FEIdata$country.name)
    barplot(data(),main ="BARPLOT OF INDICATORS",xlab = "Countries",col=rainbow(26)[1:n],legend = levels(FEIdata$country.name))
    
  })
  #Visualise UTADIS marginal utilities
  output$utadisvis <- renderPlot({
    
    
    
    lbls = c("PortTraffic","MonthYear","Country","CountryCode","IMFValue")
    n=4
    
    pie(numargpie, col=rainbow(n),labels = lbls, radius=1.1, main  ="PIE CHART SHOWING THE RISK LEVEL OF CRITERIA ")
    
    
    
  })
  
  
  
  
  #Predict UTADIS classes
  
  output$glmpred <- renderUI({
    
    wellPanel(
      
      p(strong("Please Enter Country Data to predict IMF value")),
      
      tagList(
        #First remove arrows from numeric field
        tags$style(HTML("
                        input[type=number] {
                        -moz-appearance:textfield;
                        }
                        input[type=number]::{
                        -moz-appearance:textfield;
                        }
                        input[type=number]::-webkit-outer-spin-button,
                        input[type=number]::-webkit-inner-spin-button {
                        -webkit-appearance: none;
                        margin: 0;
                        }
                        ")),
        numericInput("PortTraffic", "PortTraffic Data:", 0),
        selectInput("CountryName", "Choose Country:", levels(FEIdata$country.name)),
        #numericInput("Country", "Country Data:", 0),
        #numericInput("CountryCode", "CountryCode Data:", 0),
        #numericInput("IMF Value", "IMF Data:", 0),
        actionButton('action', 'Predict')
        
        )
      
      
      
        )
    
})
  
  output$models <-renderPrint({
    
    #FEIdata$country.name <- as.numeric(FEIdata$country.name) 
    ###building models
    #panel regression using plm, fixed country effect
    #lm_imf <- plm(imf.val ~ port.traffic, data = FEIdata, index=c("country.code", "month.yr"))
    #generalised linear models
    #glmodel <- glm(imf.val ~ port.traffic + country.name, data=FEIdata)
    #summarise models in funtion below
    
    print(summary(lm_imf))
    
    print(summary(glmodel))
  })
  
  
  output$predictbutton <- function() {
    
    if (is.null(input$action) || input$action == 0)
      return(NULL)
    
    return(isolate({
      #cgs <- ((input$maize) * mumaize) + ((input$potato) * mupotato) + ((input$cassava) * mucassava) + ((input$wealthginindex) * muwgi)
      #Example of new data for country
      newdata = data.frame(port.traffic = input$PortTraffic,country.name = input$CountryName)
      #predict using new data
      newimfvalue <- predict(glmodel, newdata, type="response")
      
      
        session$sendCustomMessage("messageBox", paste(" With a port traffic of", input$PortTraffic,"the country", input$CountryName, "will have an IMF value of ", newimfvalue, "US dollars"))
      
      
    }))
  }
  
  
  
  
  #Display results of the classification decision tree
  
  output$results <- renderPrint({
    
    printcp(fit2)
  })
  
  #Display classification summary
  output$classummary <- renderPrint({
    
    summary(fit2)
  })  
  
  #View(FEIdata)
  #Display Time Series
  output$Timeseries <- renderPlot({
    
    FEIdata$month.yr <- as.Date(FEIdata$month.yr)
    
    FEIdata <- drop_na(FEIdata)
    
    # Remove ODA-eligible countries we're not interested in
    FEIdata <- FEIdata %>% filter(!country.name %in% c('united kingdom', 'korea', 'china,p.r.: mainland') )
    
    #Normalise data within country and test overall linear regression and plot
    FEIdata$imf.nor <- ave(FEIdata$imf.val, FEIdata$country.code, FUN=function(x) (x -min(x))/(max(x) - min(x))) 
    
    FEIdata$port.nor <- ave(FEIdata$port.traffic, FEIdata$country.code, FUN=function(x) (x - min(x))/(max(x) - min(x))) 
    
    df_line <- FEIdata %>% dplyr::select(country.name, month.yr, port.nor, imf.nor)
    
    df_line <-df_line %>% melt(id=c('country.name', 'month.yr'))
    
    ggplot(df_line, aes(x=month.yr, y=value, colour=variable))+
      geom_line()+
      facet_wrap(~country.name, scales="free")+
      ggtitle(paste0("Normalised port traffic and IMF imports"))
    
    
    
  }) 
  
  #Display UTADIS weight summary
  output$utadisweight <- renderPrint({
    
    str(wt)
    str(threshdata)
    str(accuraciesdata)
    
    
    #summary(fit2)
  }) 
  
  #Plot the decision tree
  
  output$plotree <- renderPlot({
    
    prp(fit2, extra = 1, faclen=0, nn = T,
        box.col=c("green", "orange", "yellow")[findInterval(fit2$frame$yval, v = c(1,2,3))])
    
    
    
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function(){paste(input$dataset,".csv",sep=" ")},
    content = function(file){
      write.csv(data(),file)
      
    }
    
  )
  
} )
