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
  
  # Test whether we are online
  
  test <- try(getSymbols("GDPC1",src='FRED'))
  if (inherits(test, "try-error")) load(file="InputData.RData") else {
    rm("GDPC1")
    
    # Data import
    cat("\nDownloading data online....")
    misc.Import.Series <- function(Identifier, series.name, src="FRED"){
      cat(paste("\n    >>> ", series.name))
      getSymbols(series.name,src='FRED') #, env=as.environment("DATA.ENV"))
      assign(Identifier, get(series.name))
      return(Identifier)
    }
    
    # attach(NULL, name="DATA.ENV")
    assign("US.GDP.Real", get(getSymbols("GDPC1",src='FRED')))
    assign("EU.GDP.Real", get(getSymbols("NAEXKP01EZQ652S",src='FRED')))
    assign("UK.GDP.Real", get(getSymbols("NAEXKP01GBQ652S",src='FRED')))
    assign("CN.GDP.Real", get(getSymbols("CANRGDPQDSNAQ",src='FRED')))
    assign("CA.GDP.Real", get(getSymbols("RGDPNACNA666NRUG",src='FRED')))
    
    assign("US.Claims", get(getSymbols("IC4WSA",src='FRED')))
    assign("US.Payroll", get(getSymbols("PAYEMS",src='FRED')))
    assign("US.Unemployment", get(getSymbols("UNRATE",src='FRED')))
    assign("EU.Unemployment", get(getSymbols("LRHUTTTTEZM156S",src='FRED')))
    assign("US.CPI.Headline", get(getSymbols("CPIAUCSL",src='FRED')))
    assign("US.CPI.Core", get(getSymbols("CPILFESL",src='FRED')))
    assign("CA.CPI.Headline", get(getSymbols("CANCPIALLMINMEI",src='FRED')))
    assign("CA.CPI.Core", get(getSymbols("CANCPICORMINMEI",src='FRED')))
    
    assign("EU.CPI.Headline", get(getSymbols("CP0000EZ17M086NEST",src='FRED')))
    assign("EU.CPI.Headline2", get(getSymbols("CPHPTT01EZM661N",src='FRED')))
    assign("EU.CPI.Core", get(getSymbols("CPHPLA01EZM661N",src='FRED')))
    assign("US.IP", get(getSymbols("INDPRO",src='FRED')))
    assign("US.SOV.10Y", get(getSymbols("DGS10",src='FRED')))
    assign("EU.SOV.10Y", get(getSymbols("IRLTLT01EZM156N",src='FRED')))
    
    assign("FX.USDEUR", get(getSymbols("DEXUSEU",src='FRED')))
    assign("FX.EUR.Effective", get(getSymbols("RBXMBIS",src='FRED')))
    assign("FX.CADUSD", get(getSymbols("EXCAUS",src='FRED')))
    assign("FX.CA.Effective", get(getSymbols("RBCABIS",src='FRED')))
    
    assign("US.Survey.Empire", get(getSymbols("GACDINA066MNFRBNY",src='FRED')))
    assign("EU.Survey.ConsumerConfidence.Expectations", get(getSymbols("CSESFT02EZM460S",src='FRED')))
    assign("EU.Survey.ConsumerConfidence", get(getSymbols("CSCICP02EZM460S",src='FRED')))
    assign("EU.Survey.ManufacturingConfidence", get(getSymbols("BSCICP02EZM460S",src='FRED')))
    assign("EU.Survey.CapacityUtilization", get(getSymbols("BSCURT02EZQ160S",src='FRED')))
    
  
    #List.Series    <- ls("DATA.ENV")
    List.Variables <- ls()    
    
    cat("\n.... saving a new RData file")
    save(list=ls(), file="InputData.RData")
  }
  
  List.Countries <- c("US", "EU", "UK", "CN", "CA")
  
  browser()
  
  for (i in 1:length(List.Countries)){
    Country     <- List.Countries[i]
    Data.Name   <- paste(Country,".GDP.qq", sep="")
    FC.Name     <- paste(Country,".GDP.qq.FC", sep="")
    Chart.Name  <- paste(Country,".GDP.qq.FC.chart", sep="")

    Data.Series <- get(paste(Country, ".GDP.Real", sep=""))
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

  output$GDP.Comparison.History <- renderPlot({
    for (i in 1:length(List.Countries)){
      Country     <- List.Countries[i]
      Data.Name   <- paste(Country,".GDP.qq", sep="")
    }
      
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
