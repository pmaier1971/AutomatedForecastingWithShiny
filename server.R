rm(list=ls())
library(shiny)
library(quantmod)
library(zoo)
library(forecast)
library(ggplot2)

# misc. functions

misc.funggcast<-function(dn,fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn,end=en))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn,end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
  names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
  
  pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
  return(pd)
  
}

shinyServer(function(input, output) {
  
  # Data import
  attach(NULL, name="DATA.ENV")
  
  List.Countries <- c("US", "Eurozone", "UK", "Canada", "China")
  
  getSymbols('GDPC1',src='FRED', env=as.environment("DATA.ENV"))
  getSymbols('NAEXKP01EZQ652S',src='FRED', env=as.environment("DATA.ENV"))
  getSymbols('NAEXKP01GBQ652S',src='FRED', env=as.environment("DATA.ENV"))
  getSymbols('CANRGDPQDSNAQ',src='FRED', env=as.environment("DATA.ENV"))
  getSymbols('RGDPNACNA666NRUG',src='FRED', env=as.environment("DATA.ENV"))
  
  
  
  List.Series <- ls("DATA.ENV")
  #i<-1
  for (i in 1:length(List.Countries)){
    Country <- List.Countries[i]
    Data.Name <- paste(Country,".GDP.qq", sep="")
    FC.Name   <- paste(Country,".GDP.qq.FC", sep="")
    Chart.Name<- paste(Country,".GDP.qq.FC.chart", sep="")
    
    Data.Series <- get(List.Series[i])
    if (Country == "China") Data.Series <- 100*log(Data.Series / lag(Data.Series, 1))
    else Data.Series <- 400*log(Data.Series / lag(Data.Series, 1))
    
    assign(Data.Name, Data.Series)
    assign(FC.Name, auto.arima(Data.Series))
  }
  
  output$Macro.Control <- renderUI({
    selectInput("Macro.Control.Choice", "Select the country", List.Countries, selected=List.Countries[1])
  })
  
  output$Macro.Regression <- renderPrint({
    FC.Name <- paste(input$Macro.Control.Choice, ".GDP.qq.FC", sep="")
    return((summary(get(FC.Name))))
  })
  
  output$Macro.Chart <- renderPlot({
    FC.Name <- paste(input$Macro.Control.Choice, ".GDP.qq.FC", sep="")
    return( plot(forecast(get(FC.Name), h=4)))
  })
  
  
  # --------- STOCK MARKET
  
  ListOfCodes <- c("^GSPC", "BAC", "TMUS", "^TNX", "SPY")
  
  
  for (i in 1:length(ListOfCodes)){
    
    # getSymbols(ListOfCodes[i], src = "google") 
    getSymbols(ListOfCodes[i])
  }  
  
  output$Data.Realtime <- renderTable({
    data <- getQuote(ListOfCodes)
    #data$Trade.Time <- as.Date(data$Trade.Time)
    #    data <- data.frame(Ticker = rownames(data), data)
    #   stock <- subset(data, Ticker == input$StockSelectorChoice, select = Last)
    return(data)
  })
  
  output$StockSelector <- renderUI({
    selectInput("StockSelectorChoice", "Select a stock", ListOfCodes)
  })
  
  output$TestPlot <- renderPlot({
    #browser()
    Stock.Selected <- input$StockSelectorChoice
    Stock.Selected <- get(gsub("\\^", "",Stock.Selected))
    plot(Stock.Selected)
    
  })
  
  output$LatestValue <- renderTable({
    Stock.Selected <- input$StockSelectorChoice
    Stock.Selected <- get(gsub("\\^", "",Stock.Selected))
    Stock.Value    <- tail(Stock.Selected, 10)
    Stock.Info     <- data.frame(Period = as.character(index(Stock.Value)), Closing = Stock.Value[,4],
                                 Volume = Stock.Value[,5])
    rownames(Stock.Info) <- NULL
    return(Stock.Info)
  })
  
})
