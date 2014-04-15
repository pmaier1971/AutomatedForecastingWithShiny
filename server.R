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

# misc.growth.monthly.to.qq <- function(x){
#   x <- 400*ln(x / lag(x, 1))
# }

misc.growth.quarterly.to.qq <- function(x){
  x <- 400*log(x / lag(x, 1))
}

misc.growth.quarterly.to.qq <- function(x){
  x <- 100*log(x / lag(x, 4))
}

misc.growth.monthly.to.yy <- function(x){
  x <- 100*log(x / lag(x, 12))
}

shinyServer(function(input, output) {
  
  # Test whether we are online
  List.Tickers <- c("^GSPC", "BAC", "TMUS", "^TNX", "SPY")
  
  test <- try(getSymbols("GDPC1",src='FRED'))
  if (inherits(test, "try-error")) load(file="InputData.RData") else {
    rm("GDPC1")
    
    # Is the data up-to-date?
    load(file="Last.Update.RData")
    
    if (!(Sys.Date() == Last.Update)) {
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
    
    assign("US.CPI.Headline", misc.growth.monthly.to.yy(get(getSymbols("CPIAUCSL",src='FRED'))))
    assign("US.CPI.Core", misc.growth.monthly.to.yy(get(getSymbols("CPILFESL",src='FRED'))))
    assign("CA.CPI.Headline", misc.growth.monthly.to.yy(get(getSymbols("CANCPIALLMINMEI",src='FRED'))))
    assign("CA.CPI.Core", misc.growth.monthly.to.yy(get(getSymbols("CANCPICORMINMEI",src='FRED'))))
    assign("EU.CPI.Headline", misc.growth.monthly.to.yy(get(getSymbols("CP0000EZ17M086NEST",src='FRED'))))
    assign("EU.CPI.Headline2", misc.growth.monthly.to.yy(get(getSymbols("CPHPTT01EZM661N",src='FRED'))))
    assign("EU.CPI.Core", misc.growth.monthly.to.yy(get(getSymbols("CPHPLA01EZM661N",src='FRED'))))
    
    assign("US.IP", misc.growth.monthly.to.yy(get(getSymbols("INDPRO",src='FRED'))))
    
    assign("US.SOV.10Y", get(getSymbols("DGS10",src='FRED')))
    assign("EU.SOV.10Y", get(getSymbols("IRLTLT01EZM156N",src='FRED')))
    
    assign("FX.EU.USD", get(getSymbols("DEXUSEU",src='FRED')))
    assign("FX.EU.Effective", get(getSymbols("RBXMBIS",src='FRED')))
    assign("FX.CA.USD", 1/get(getSymbols("EXCAUS",src='FRED')))
    assign("FX.CA.Effective", get(getSymbols("RBCABIS",src='FRED')))
    
    assign("US.Survey.Empire", get(getSymbols("GACDINA066MNFRBNY",src='FRED')))
    assign("EU.Survey.ConsumerConfidence.Expectations", get(getSymbols("CSESFT02EZM460S",src='FRED')))
    assign("EU.Survey.ConsumerConfidence", get(getSymbols("CSCICP02EZM460S",src='FRED')))
    assign("EU.Survey.ManufacturingConfidence", get(getSymbols("BSCICP02EZM460S",src='FRED')))
    assign("EU.Survey.CapacityUtilization", get(getSymbols("BSCURT02EZQ160S",src='FRED')))
    
    for (i in 1:length(List.Tickers)){
      # getSymbols(List.Tickers[i], src = "google") 
      getSymbols(List.Tickers[i])
    }  
    
    cat("\n.... saving a new RData file")
    save(list=ls(), file="InputData.RData")
    Last.Update <- Sys.Date()
    save(Last.Update, file = "Last.Update.RData")
    }
  }
  
  List.Countries <- c("US", "EU", "UK", "CN", "CA")
  List.Variables <- ls()
  
  
  for (i in 1:length(List.Countries)){
    Country     <- List.Countries[i]
    Data.Name   <- paste(Country,".GDP.Real.qq", sep="")
    FC.Name     <- paste(Country,".GDP.qq.FC.Naive", sep="")
    Chart.Name  <- paste(Country,".GDP.qq.FC..Naive.chart", sep="")

    Data.Series <- get(paste(Country, ".GDP.Real", sep=""))
    Data.Series <- tail(Data.Series, 60) # Last 15 years
    if (Country == "China") Data.Series <- 100*log(Data.Series / lag(Data.Series, 1))
    else Data.Series <- 400*log(Data.Series / lag(Data.Series, 1))
    
    assign(Data.Name, Data.Series)
    assign(FC.Name, auto.arima(Data.Series))
  }
  
  
  # Panel Overview
  output$Overview.Charts <-renderPlot({
   par(mfrow=c(2,3))
   for (i in 1:length(List.Countries)){
     FC.Name     <- paste(Country,".GDP.qq.FC.Naive", sep="")
     FC.Data     <- forecast(get(FC.Name), h=4)
     FC <- data.frame(Period = Sys.Date(),
                      FC.Data$mean)
     
     plot(FC.Data, main = List.Countries[i])
   }
   par(mfrow=c(1,1))    
  })
  
  
  # Panel Country Analysis
  
  output$UI.Country.Analysis <- renderUI({
    selectInput("Country.Analysis.Control.Choice", "Select the country", List.Countries, selected=List.Countries[1])
  })
  
  output$Charts.Country.Analysis <-renderPlot({
    Country <- input$Country.Analysis.Control.Choice
    par(mfrow=c(2,3))
    plot(get(paste(Country,".GDP.Real.qq", sep="")), main="Real GDP q/q")
    if (exists(paste(Country,".IP", sep="")))           plot(get(paste(Country,".IP", sep="")), main="Industrial production")
    if (exists(paste(Country,".Unemployment", sep=""))) plot(get(paste(Country,".Unemployment", sep="")), main="Unemployment")
    if (exists(paste(Country,".CPI.Headline", sep=""))) plot(get(paste(Country,".CPI.Headline", sep="")), main="Headline CPI")
    if (exists(paste("FX.",Country,".USD", sep="")))    plot(get(paste("FX.",Country,".USD", sep="")), main="Exchange rate against the USD")
    if (exists(paste(Country,".SOV.10Y", sep="")))      plot(get(paste(Country,".SOV.10Y", sep="")), main="10Y Sovereign bond")    
    par(mfrow=c(1,1))    
  })
  
  output$Commentary.Country.Analysis <-renderText({
    Country <- input$Country.Analysis.Control.Choice
    Commentary <- "<h4>Key figures</h4><ul>"
    
    Data       <- get(paste(Country,".GDP.Real.qq", sep=""))
    Change     <- round(coredata(Data[nrow(Data)])-coredata(Data[nrow(Data)-1]),1)
    Commentary <- paste(Commentary, "<li>Real GDP growth in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                        " was ", round(Data[nrow(Data)],1),"%", sep ="")
    if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% </li>", sep="")
    else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "%</li>", sep="")
    else                 Commentary <-paste(Commentary, ", (unchanged).</li>", sep="")
    
    if (exists(paste(Country,".Unemployment", sep=""))) {
      Data       <- get(paste(Country,".Unemployment", sep=""))  
      Change     <- round(coredata(Data[nrow(Data)])-coredata(Data[nrow(Data)-1]),1)
      Commentary <- paste(Commentary, "<li>Unemployment in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                          " was ", round(Data[nrow(Data)],1),"%", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% </li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "%</li>", sep="")
      else                 Commentary <-paste(Commentary, ", (unchanged).</li>", sep="")      
    }    
    
    if (exists(paste(Country,".IP", sep=""))) {
      Data       <- get(paste(Country,".IP", sep=""))
      Change     <- round(coredata(Data[nrow(Data)])-coredata(Data[nrow(Data)-1]),1)
      Commentary <- paste(Commentary, "<li>Industrial production in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                          " was ", round(Data[nrow(Data)],1),"%", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% </li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "%</li>", sep="")
      else                 Commentary <-paste(Commentary, " (unchanged).</li>", sep="")      
    }    
    
    if (exists(paste(Country,".CPI.Headline", sep=""))) {
      Data       <- get(paste(Country,".CPI.Headline", sep=""))
      Change     <- round(coredata(Data[nrow(Data)])-coredata(Data[nrow(Data)-1]),1)
      Commentary <- paste(Commentary, "<li>Headline CPI in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                          " was ", round(Data[nrow(Data)],1),"%", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% </li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "%</li>", sep="")
      else                 Commentary <-paste(Commentary, " (unchanged).</li>", sep="")      
    }    
    
    if (exists(paste(Country,".SOV.10Y", sep=""))) {
      Data       <- get(paste(Country,".SOV.10Y", sep=""))
      Change     <- round(coredata(Data[nrow(Data)])-coredata(Data[nrow(Data)-1]),1)
      Commentary <- paste(Commentary, "<li>The 10Y sovereign bond yield in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                          " was ", round(Data[nrow(Data)],1),"%", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% </li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "%</li>", sep="")
      else                 Commentary <-paste(Commentary, " (unchanged).</li>", sep="")      
    }    
    
    if (exists(paste("FX.",Country,".USD", sep=""))) {
      Data       <- get(paste("FX.",Country,".USD", sep=""))
      Change     <- round(log(coredata(Data[nrow(Data)])/coredata(Data[nrow(Data)-1]))*100,1)
      Commentary <- paste(Commentary, "<li>The exchange rate against the US dollar in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                          " was ", round(Data[nrow(Data)],3),"", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% </li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "%</li>", sep="")
      else                 Commentary <-paste(Commentary, " (unchanged).</li>", sep="")      
    }        
    return(Commentary)
  })
  
  # Panel Macroeconomic Forecasting
  
  # Dynamic UI
  output$UI.Macro.Control <- renderUI({
    selectInput("Macro.Control.Choice", "Select the country", List.Countries, selected=List.Countries[1])
  })
  
  output$UI.Variable.Control <- renderUI({
    Choices <- List.Variables[grep(paste(input$Macro.Control.Choice, ".", sep=""), List.Variables, fixed=TRUE)]
    selectInput("Variable.Control.Choice", 
                paste("Select the variable for country", input$Macro.Control.Choice), 
                Choices)#, selected=Choices[1])
  })

  Regression.Output <- reactive({
    if (grepl("GDP", input$Variable.Control.Choice)) Data <- get(paste(input$Variable.Control.Choice, ".qq", sep=""))
    else Data   <- get(input$Variable.Control.Choice)
    Regression  <- auto.arima(Data)
    return(Regression)
  })
  
  output$Macro.Regression <- renderPrint({
    return(summary(Regression.Output()))
  })
  
  output$Macro.Chart <- renderPlot({
    Forecast <-forecast(Regression.Output(), h=4)
    plot(Forecast, col="tomato", main = paste("Forecasting ", input$Variable.Control.Choice, 
                                              "\nModel: ", Forecast$method, sep=""))
  })

  
  # --------- STOCK MARKET
  
   
  output$Data.Realtime <- renderTable({
    data <- getQuote(List.Tickers)
    #data$Trade.Time <- as.Date(data$Trade.Time)
    #    data <- data.frame(Ticker = rownames(data), data)
    #   stock <- subset(data, Ticker == input$StockSelectorChoice, select = Last)
    return(data)
  })
  
  output$StockSelector <- renderUI({
    selectInput("StockSelectorChoice", "Select a stock", List.Tickers)
  })
  
  output$TestPlot <- renderPlot({
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
