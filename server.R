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
library(Hmisc)
library(Amelia)
library(ggplot2)


#FEIdata <- read.csv("port_traffic_imf_exports.csv",sep = ",")
FEIdata <- read.csv("all_factors_vs_imf1.csv",sep = ",")

####Data Exploration
#dim(FEIdata)
#View(FEIdata)
#sum(is.na(FEIdata))
#summary(FEIdata)
####plot null values
#missmap(FEIdata, legend = TRUE, col = c("blue", "black"))
####replace na values by median of column of interest
FEIdata$port.traffic<- impute(FEIdata$port.traffic, median)
FEIdata$air.traffic<-impute(FEIdata$air.traffic, median)
FEIdata$NO2<-impute(FEIdata$NO2, median)
FEIdata$radiance<-impute(FEIdata$radiance, median)
FEIdata$imf.val<-impute(FEIdata$imf.val, median)
#str(FEIdata)
FEIdata <- data.frame(FEIdata)
#View(FEIdata)
# check correlation between independent variables
#data = subset(FEIdata, select = -c(imf.val,country.name,country.code,month.yr,imf.imports,imf.exports) )
#cor(data)
####correlation results reveals high linear relation(0.67) between the independent variables "radiance" and "NO2". So, one of these variables is dropped 


#panel regression using plm, fixed country effect
lm_imf <- plm(imf.val ~ port.traffic + air.traffic  + NO2 + radiance, data = FEIdata, index=c("country.code", "month.yr"))
#generalised linear models
glmodel <- glm(imf.val ~ port.traffic + air.traffic + NO2 + radiance + country.name, data=FEIdata)



utilityFunction <- function(x) {
  
  tags$script("Shiny.addCustomMessageHandler('messageBox', function(msg){window.alert(msg);})")
  
}

shinyServer(function(input,output,session){
  
  data <-   reactive({
    switch(input$dataset,
           pt=as.numeric(FEIdata$port.traffic),
           imf=as.numeric(FEIdata$imf.val),
           rad =as.numeric(FEIdata$radiance),
           at = as.numeric(FEIdata$air.traffic),
           polut = as.numeric(FEIdata$NO2)
           #n<-nlevels(cn)
           
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
        selectInput("CountryName", "Choose Country:", levels(FEIdata$country.name)),
        numericInput("PortTraffic", "PortTraffic Data:", 0),
        numericInput("AirTraffic", "AirTraffic Data:", 0),
        numericInput("pollution", "Pollution Data:", 0),
        numericInput("radiance", "Radiance Data:", 0),
        actionButton('action', 'Predict')
        
        )
      
      
      
        )
    
})
  
  output$models <-renderPrint({
    
  cat("There is correlation of 0.67 between the two predictors N02 and Radiance,\n and a correlation of 0.71 between air traffic and port traffic \nBy convention one of these two highly correlated predictors should be dropped from the model\n")
    
    print(summary(lm_imf))
    
    print(summary(glmodel))
  })
  
  
  output$predictbutton <- function() {
    
    if (is.null(input$action) || input$action == 0)
      return(NULL)
    
    return(isolate({
      #cgs <- ((input$maize) * mumaize) + ((input$potato) * mupotato) + ((input$cassava) * mucassava) + ((input$wealthginindex) * muwgi)
      #Example of new data for country
      newdata = data.frame(port.traffic = input$PortTraffic, air.traffic =input$AirTraffic, NO2=input$pollution, radiance =input$radiance, country.name = input$CountryName)
      
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
