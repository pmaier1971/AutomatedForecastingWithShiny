rm(list=ls())
library(shiny)
library(quantmod)
library(zoo)
library(forecast)
library(ggplot2)
library(lubridate)
library(Quandl)
library(shinyIncubator)
library(httr)
#library(jsonlite)
#library(RPushbullet)
#library(mygmailR)


# misc. functions

misc.growth.quarterlydata.to.qq.ar <- function(x){
  x <- 400*log(x / lag(x, 1))
}

misc.growth.quarterlydata.to.yy <- function(x){
  x <- 100*log(x / lag(x, 4))
}

misc.growth.monthlydata.to.yy <- function(x){
  x <- 100*log(x / lag(x, 12))
}


shinyServer(function(input, output, session) {
  
  ListOfCodes <- c("SP 500"="^GSPC", 
                   "Hang Seng" = "^HSI",
                   "Nikkei" = "^N225",
                   "Dax" = "^GDAXI",
                   "FTSE" = "^FTSE",
                   "UST 10Y"="^TNX", 
                   "Apple"="AAPL", 
                   "Bank of America"="BAC", 
                   "JP Morgan"="JPM", 
                   "T-Mobile"="TMUS")
  
  # Test whether we are online
  test        <- try(getSymbols("GDPC1",src='FRED'))
  
  #load(file="InputData.RData")
  response <- GET(url = "https://www.dropbox.com/s/wrfre0as7q7x1sn/InputData.RData?dl=0")
  load(rawConnection(response$content))
  
  if (!(inherits(test, "try-error"))) {
    # We are online. Is the data up-to-date?
    if (Sys.Date() != Last.Update) {
      # Data import
      cat("\nDownloading data online....")
      
      Data.US <- c("US.GDP.Real"="GDPC1", "US.Survey.PMI.M"="NAPM", "US.Survey.Empire"="GACDINA066MNFRBNY", 
                   "US.IP"= "INDPRO", "US.Claims"="IC4WSA", "US.Payroll"="PAYEMS",
                   "US.Unemployment"="UNRATE", "US.Unemployment.U6" = "U6RATE",
                   "US.Unemployment.PartTimeEconomicReasons" = "LNS12032194",
                   "US.Unemployment.PartTimeNonEconomicReasons" = "LNS12032200",
                   "US.Unemployment.MarginallyAttached" = "LNU05026642",
                   "US.Unemployment.ParticipationRate"="CIVPART",
                   "US.Unemployment.EmploymentToPopulation"="EMRATIO",
                   "US.Activity.ChicagoFed.Employment" = "EUANDH",
                   "US.Activity.ChicagoFed" = "CFNAI",
                   "US.Activity.PhillyFed.Current" = "USPHCI",
                   "US.Activity.PhillyFed.Leading" = "USSLIND",
                   "US.Activity.NYFed.Current" = "GACDISA066MSFRBNY",
                   "US.Activity.NYFed.Leading" = "GAFDISA066MSFRBNY",
                   "US.Activity.NYFed.AvWorkWeek.Current" = "AWCDISA066MSFRBNY",
                   "US.Activity.NYFed.NoEmployees.Current" = "NECDISA066MSFRBNY",
                   "US.Activity.NYFed.AvWorkWeek.Leading" = "AWFDINA066MNFRBNY",
                   "US.Activity.NYFed.NoEmployees.Leading" = "NEFDINA066MNFRBNY",
                   "US.Activity.SFFed.TechPulse" = "SFTPAGRM158SFRBSF",
                   "US.Activity.ISM.NonManufacturing.Employment" = "NMFEI",
                   "US.Activity.ISM.Manufacturing.Employment" = "NAPMEI",
                   "US.Activity.ADP" = "NPPTTL",
                   "US.Activity.InitialClaims.4W.MA" = "IC4WSA",
                   "US.Activity.InitialClaims" = "ICSA",
                   "US.Activity.ContinuedClaims.4W.MA" = "CC4WSA",
                   "US.Activity.ContinuedClaims" = "CCSA",
                   "US.JOLTS.QuitsRate" = "JTSQUR",
                   "US.JOLTS.HireRate" = "JTSHIR",
                   "US.JOLTS.JobOpeningsRate" ="JTSJOR",
                   "US.Unemployment.WageGrowth" = "CES0500000003",
                   "US.CPI.Headline"="CPIAUCSL", "US.CPI.Core"="CPILFESL",
                   "US.SOV.10Y"="DGS10", "US.FSI.Cleveland"="CFSI",
                   "US.HouseholdDebt" = "HDTGPDUSQ163N"      )      
      Data.EU <- c("EU.GDP.Real"="EUNGDP", "EU.Unemployment"="LRHUTTTTEZM156S",
                   "EU.CPI.Headline"="CP0000EZ17M086NEST", "EU.CPI.Core"="CPHPLA01EZM661N",
                   "EU.SOV.10Y"="IRLTLT01EZM156N", "FX.EU.USD"="DEXUSEU", "FX.EU.Effective"="RBXMBIS",
                   "EU.Survey.ConsumerConfidence.Expectations"="CSESFT02EZM460S",
                   "EU.Survey.ConsumerConfidence"="CSCICP02EZM460S",
                   "EU.Survey.ManufacturingConfidence"="BSCICP02EZM460S",
                   "EU.Survey.CapacityUtilization"="BSCURT02EZQ160S")
      Data.UK <- c("UK.GDP.Real"="UKNGDP", "UK.CPI.Headline"="CPALTT01GBQ657N", "UK.CPI.Core"="GBRCPICORMINMEI",
                   "UK.Unemployment"="LMUNRRTTGBM156S", "FX.UK.USD"="DEXUSUK")
      Data.CA <- c("CA.GDP.Real"="NAEXKP01CAQ189S", "CA.CPI.Headline"="CANCPIALLMINMEI", 
                   "CA.CPI.Core"="CANCPICORMINMEI", "FX.CA.USD"="EXCAUS", "FX.CA.Effective"="RBCABIS",
                   "CA.HouseholdDebt" = "HDTGPDCAQ163N")
      Data.MarketUpdate <- c("Market.Gold"="GOLDAMGBD228NLBM"
      )
      
      List.Countries <- c("Data.US", "Data.EU", "Data.UK", "Data.CA")
      
      # Assign names
      # Some variables need to be transformed
      List.Transformation <- c("US.CPI", "EU.CPI", "US.IP", "CA.IP")
      
      # withProgress(session, min = 1, max = length(List.Countries), {
      #  setProgress(message = "Downloading Data")
      for (idx.Country in 1:length(List.Countries)){
        #setProgress(value = idx.Country)
        #setProgress(detail = "Sorry, this is taking a while")
        cat("\n   Country: ", List.Countries[idx.Country])
        getSymbols(get(List.Countries[idx.Country]), src="FRED")
        for (idx in 1:length(get(List.Countries[idx.Country]))){
          cat("\n      - ", names(get(List.Countries[idx.Country]))[idx])
          x <- get(get(List.Countries[idx.Country])[idx])
          if (names(get(List.Countries[idx.Country]))[idx] %in% List.Transformation){
            x <- misc.growth.monthlydata.to.yy(x)
          }
          assign(names(get(List.Countries[idx.Country]))[idx], x)
        }
      }
      #  })
      
      # Get the market data
      for (i in 1:length(ListOfCodes)){
        getSymbols(ListOfCodes[i], src='yahoo')
      }  
      
      List.Countries <- c("US", "EU", "UK", "CA") #"CN"
      List.Variables <- ls()
      
      
      for (i in 1:length(List.Countries)){
        Country     <- List.Countries[i]
        Data.Name   <- paste(Country,".GDP.Real.qq", sep="")
        FC.Name     <- paste(Country,".GDP.qq.FC.Naive", sep="")
        Chart.Name  <- paste(Country,".GDP.qq.FC.Naive.chart", sep="")
        
        Data.Series <- get(paste(Country, ".GDP.Real", sep=""))
        Data.Series <- tail(Data.Series, 60) # Last 15 years
        if (Country == "China") Data.Series <- 100*log(Data.Series / lag(Data.Series, 1))
        else Data.Series <- 400*log(Data.Series / lag(Data.Series, 1))
        
        assign(Data.Name, Data.Series)
        assign(FC.Name, auto.arima(Data.Series))
      }
      
      
      cat("\nSaving a new RData file...")
      #pbPost("note", "New RData file saved", "New data found.")
      # send_gmail(subject="Test", body="Test Body", file_private = "gmailR.txt", dir_private = "~/Desktop/")
      Last.Update      <- Sys.Date()
      Last.Update.Time <- Sys.time()
      #save.image(file = "InputData.RData")
      writeBin(response$content, "InputData.RData")
      cat(" done")
    } else cat("\nData file is up to date")
  } 
  
 misc.UpDown <- function(x){
   x <- abs( round(x,2) )
   if        (x>0 )   return(paste0("up ", x))
   else if   (x<0 )   return(paste0("down ", x))
   else if   (x==0)   return("unchanged")
 }
  
  # Panel Overview
  output$MarketUpdate.Commentary <- renderText({
    Commentary <- paste0("<b>Market Update (as of ", Last.Update.Time, ")</b><p>")
    Commentary <- paste0(Commentary, "<ul><li><b>Europe:</b> In Frankfurt the DAX closed at ", round(last(GDAXI)[,4],2), ", ", misc.UpDown(last(diff(GDAXI)[,4])), " from the last close. ")
    #Commentary <- paste0(Commentary, "<li>The euro currently stands at ", round(last(DEXUSEU)[,4],2), ", ", misc.UpDown(last(diff(DEXUSEU)[,4])), " from the last close. ")
    Commentary <- paste0(Commentary, "</ul><ul><li><b>Asia:</b> The Nikkei closed at ", round(last(N225)[,4],2), ", ", misc.UpDown(last(diff(N225)[,4])), " points. ")
    Commentary <- paste0(Commentary, 
                         "Hong Kong's Hang Seng closed at ", round(last(HSI)[,4],2), ", ", misc.UpDown(last(diff(HSI)[,4])), " from the last close. ")
    return(Commentary)
  })
  
  output$Overview.Charts <-renderPlot({
    par(mfrow=c(2,3))
    for (i in 1:length(List.Countries)){
      History     <- get(paste0(List.Countries[i],".GDP.Real.qq"))
      FC.Name     <- paste0(List.Countries[i],".GDP.qq.FC.Naive")
      FC.Data     <- forecast(get(FC.Name), h=4)
      
      Chart.Data  <- zoo(, seq(from = index(History[1]), to = index(tail(History,1)) + months(12),
                               by = "3 months"))
      Chart.Data  <- merge(Chart.Data, zoo(data.frame(History = FC.Data$x, Fitted = FC.Data$fitted),
                                           as.Date(index(History))))
      
      Chart.Data  <- merge(Chart.Data, zoo(data.frame(FC.Data),
                                           seq(from = index(tail(History,1))+months(3), to = index(tail(History,1)) + months(12),
                                               by = "3 months")))
      Chart.Data[length(History),3] <- Chart.Data[length(History),1]
      Chart.Data <- Chart.Data[index(Chart.Data) > Sys.Date()-years(5),]
      
      plot(Chart.Data[,1], type="l", col="blue", main = List.Countries[i], ylab="", 
           lwd=2, ylim=range(Chart.Data[,4:7], na.rm = TRUE))  
      lines(Chart.Data[,2], col="red", lty=2)
      segments(index(Chart.Data), Chart.Data[,6], index(Chart.Data), Chart.Data[,7], col="deepskyblue", lwd=7)
      segments(index(Chart.Data), Chart.Data[,4], index(Chart.Data), Chart.Data[,5], col="tomato", lwd=10)
      lines(Chart.Data[,3], col="blue", lwd=1.5, pch=19, type="b")
      lines(Chart.Data[,3], col="blue", lwd=2, pch=19)
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
    Commentary <- paste(Commentary, "<li>Real GDP growth in ", format(as.yearqtr(index(Data[nrow(Data)])), "Q%q %Y"), 
                        " was ", round(Data[nrow(Data)],2),"%", sep ="")
    if (Change > 0) {     Commentary <- paste(Commentary, ". This is an acceleration of ", Change, "%, over the", 
                                              round(Data[nrow(Data)-1],2), "% last time.</li>", sep="")
    } else if  (Change < 0) { Commentary <-paste(Commentary, ", down ", Change, "%, relative to ", 
                                                 round(Data[nrow(Data)-1],2),
                                                 "% in the last quarter.</li>", sep="")
    } else  {               Commentary <-paste(Commentary, ", (unchanged).</li>", sep="")}
    
    if (exists(paste(Country,".Unemployment", sep=""))) {
      Data       <- get(paste(Country,".Unemployment", sep=""))  
      Change     <- round(coredata(Data[nrow(Data)])-coredata(Data[nrow(Data)-1]),2)
      Commentary <- paste(Commentary, "<li>Unemployment in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                          " was ", round(Data[nrow(Data)],1),"%", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% from ", format(index(Data[nrow(Data)-1]), "%B"),".</li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "% from ", format(index(Data[nrow(Data)-1]), "%B"),".</li>", sep="")
      else                 Commentary <-paste(Commentary, " (unchanged).</li>", sep="")      
    }    
    
    if (exists(paste(Country,".IP", sep=""))) {
      Data       <- get(paste(Country,".IP", sep=""))
      Change     <- round(coredata(Data[nrow(Data)])-coredata(Data[nrow(Data)-1]),1)
      Commentary <- paste(Commentary, "<li>Industrial production in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                          " was ", round(Data[nrow(Data)],1),"%", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% from ", format(index(Data[nrow(Data)-1]), "%B"),".</li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "% from ", format(index(Data[nrow(Data)-1]), "%B"),".</li>", sep="")
      else                 Commentary <-paste(Commentary, " (unchanged).</li>", sep="")      
    }    
    
    if (exists(paste(Country,".CPI.Headline", sep=""))) {
      Data       <- get(paste(Country,".CPI.Headline", sep=""))  
      Change     <- round(coredata(Data[nrow(Data)])-coredata(Data[nrow(Data)-1]),1)
      Commentary <- paste(Commentary, "<li>Headline CPI in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                          " was ", round(Data[nrow(Data)],1),"%", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% from ", format(index(Data[nrow(Data)-1]), "%B"),".</li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "% from ", format(index(Data[nrow(Data)-1]), "%B"),".</li>", sep="")
      else                 Commentary <-paste(Commentary, " (unchanged).</li>", sep="")      
    }    
    
    if (exists(paste(Country,".SOV.10Y", sep=""))) {
      Data       <- na.omit(get(paste(Country,".SOV.10Y", sep="")))
      Change     <- round(coredata(Data[nrow(Data)])-coredata(Data[nrow(Data)-1]),1)
      Commentary <- paste(Commentary, "<li>The 10Y sovereign bond yield on ", format(index(Data[nrow(Data)]), "%d %B %Y"), 
                          " was ", round(Data[nrow(Data)],1),"%", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "%  from the previous trading day.</li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "% from the previous trading day.</li>", sep="")
      else                 Commentary <-paste(Commentary, ", (unchanged).</li>", sep="")      
    }    
    
    if (exists(paste("FX.",Country,".USD", sep=""))) {
      Data       <- get(paste("FX.",Country,".USD", sep=""))
      Change     <- round(log(coredata(Data[nrow(Data)])/coredata(Data[nrow(Data)-1]))*100,1)
      Commentary <- paste(Commentary, "<li>Against the dollar, the exchange rate in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                          " was ", round(Data[nrow(Data)],1),"%", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% </li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "%</li>", sep="")
      else                 Commentary <-paste(Commentary, ", (unchanged).</li>", sep="")      
    }        
    return(Commentary)
  })
  
  # Panel Detailed Analysis
  # Panel US Labor Market
  
  US.LaborMarket.Dashboard.Data <- function(){
    Data.Dashboard <- Reduce(function(...) merge(...), list(US.Unemployment, 
                                                            US.Unemployment.U6,
                                                            US.Unemployment.PartTimeEconomicReasons,
                                                            US.Unemployment.PartTimeNonEconomicReasons,
                                                            US.Unemployment.MarginallyAttached,
                                                            US.Unemployment.ParticipationRate,
                                                            US.Unemployment.WageGrowth,
                                                            US.Payroll,
                                                            US.Unemployment.EmploymentToPopulation,
                                                            US.JOLTS.QuitsRate,
                                                            US.JOLTS.HireRate)
    )
    Data.Dashboard[,3]           <- 100*Data.Dashboard[,3]/(Data.Dashboard[,3]+Data.Dashboard[,4])
    Data.Dashboard               <- Data.Dashboard[,-4]
    Data.Dashboard[,7]           <- Data.Dashboard[,7]-lag(Data.Dashboard[,7], 1) # use change in payrolls
    Data.Dashboard$CES0500000003 <- 100*(Data.Dashboard$CES0500000003/lag(Data.Dashboard$CES0500000003, 12) -1)
    Data.Dashboard               <- Data.Dashboard[index(Data.Dashboard)>"1999-12-01",]
    names(Data.Dashboard)        <- c("Civilian Unemployment Rate (in %)",
                                      "Total unemployed, plus all marginally attached workers\n plus total employed part time for economic reasons (in %)",
                                      "Part-Time Employment for Economic Reasons \n(All Industries, relative to Total Part-Time)",
                                      "Not in Labor Force, Searched For Work and Available (Level)",
                                      "Civilian Labor Force Participation Rate (in %)",
                                      "Average Hourly Earnings of All Employees: \nTotal Private (y/y)",
                                      "Total Nonfarm Payroll Growth: All Employees (m/m)",
                                      "Civilian Employment-Population Ratio",
                                      "Quits: Total Nonfarm (Rate)",
                                      "Hires: Total Nonfarm (Rate)")
    return(Data.Dashboard)
  }
  
  output$US.LaborMarket.Dashboard <- renderPlot({
    Data <- as.zoo(US.LaborMarket.Dashboard.Data())
    Data.PostCrisis <- Data[index(Data)>"2007-12-01",]
    
    Chart.Layout <- matrix(c(1,1,2, 3,3,4,
                             5,5,6, 7,7,8,
                             9,9,10, 11,11,12,
                             13,13,14, 15,15,16
                             ,17,17,18, 19,19,20
    ), ncol=6, byrow=TRUE)
    layout(Chart.Layout)
    op <- par(mar = par("mar")/1.2)                     
    
    for (idx in 1:ncol(Data)) {
      Chart.Title <- names(Data)[idx]
      #Chart.Title <- names(Data.US)[grep(Chart.Title, Data.US)]
      plot(Data[,idx], col="blue", type="l", main=Chart.Title, ylab="", xlab="")
      Data.Bar    <- c(max(Data.PostCrisis[,idx], na.rm = TRUE), tail(na.omit(Data.PostCrisis[,idx]),1),
                       min(Data.PostCrisis[,idx], na.rm = TRUE))
      Data.Line   <- matrix(c(0,Data.Bar[1],0,0), nrow=2)
      plot(NA, ylim=c(-0.5, 0.5), xlim=c(max(Data.Bar),min(Data.Bar)), main="Latest Observation vs. \nBest and Worst Point", 
           yaxt="n", ylab="")
      lines(Data.Line, col="blue", lwd=5)
      points(Data.Bar[2],0, col="red", pch=19, cex=2)
    }
    par(op)
    #par(mfrow=c(1,1))
    
  })
  
  # Panel Macroeconomic Forecasting
  
  # Dynamic UI
  output$UI.Macro.Control <- renderUI({
    selectInput("Macro.Control.Choice", "Select the country", List.Countries, selected=List.Countries[1])
  })
  
  output$UI.Variable.Control <- renderUI({
    Choices <- List.Variables[grep(paste(input$Macro.Control.Choice, ".", sep=""), List.Variables, fixed=TRUE)]
    Choices <- Choices[!grepl(".qq", Choices)] 
    selectInput("Variable.Control.Choice", 
                paste("Select the variable for country", input$Macro.Control.Choice), 
                Choices)#, selected=Choices[1])
  })
  
  Regression.Data   <- reactive({
    if (grepl("GDP", input$Variable.Control.Choice)) R.Data <- get(paste(input$Variable.Control.Choice, ".qq", sep=""))
    else R.Data <- get(input$Variable.Control.Choice)
    return(R.Data)
  })
  
  Regression.Output <- reactive({
    Regression       <- list()
    Input            <- Regression.Data()
    Regression[[1]]  <- auto.arima(na.omit(Input))
    Regression[[2]]  <- ets(na.omit(Input))
    
    # For HoltWinters: convert xts to ts
    Date.Frequency <- switch(periodicity(Input)$scale,
                             daily=365,
                             weekly=52,
                             monthly=12,
                             quarterly=4,
                             yearly=1)
    pltStart <- as.POSIXlt(start(Input))
    Start    <- c(pltStart$year+1900,pltStart$mon+1)
    Input.ts <- ts(na.omit(Input[,names(Input)]), start=Start, frequency=Date.Frequency)
    Regression[[3]]  <- HoltWinters(Input.ts)
    
    return(Regression)
  })
  
  output$Macro.Regression <- renderPrint({
    return(summary(Regression.Output()[[1]]))
  })
  
  output$Macro.Regression.Commentary <- renderText({
    
    Date.Frequency <- switch(periodicity(Regression.Data())$scale,
                             daily=365,
                             weekly=52,
                             monthly=12,
                             quarterly=4,
                             yearly=1)
    
    if (Date.Frequency == 4) {
      Date.Start      <- format(as.yearqtr(end(Regression.Data()) ), "Q%q %Y")
      Forecast.Period <- format(as.yearqtr(end(Regression.Data()) + months(3)), "Q%q %Y")
    } else if (Date.Frequency == 12) {
      Date.Start      <- format(end(Regression.Data()) , "%B %Y")
      Forecast.Period <- format(end(Regression.Data()) + months(1), "%B %Y")
    } else if (Date.Frequency == 52) {
      Date.Start      <- format(end(Regression.Data()) , "%d %B %Y")
      Forecast.Period <- format(end(Regression.Data()) + weeks(1), "%d %B %Y")
    }
    
    Commentary <- paste0("The latest observation for ", Date.Start)
    Commentary <- paste0(Commentary, " was ", round(tail(Regression.Data(),1),2), ". ")
    Commentary <- paste0(Commentary, " Below forecasts from different models for the next observation (", Forecast.Period, "): <ul>")
    
    Pooled.Forecast <- 0
    Model.Type = c("An ARIMA model with optimal lag selection", 
                   "An exponential smoothing state space model",
                   "Holt-Winters Filtering, aimed to minimize the prediction error,")
    for (idx.model in 1:length(Regression.Output())){
      Forecast        <- forecast(Regression.Output()[[idx.model]], h=4)
      Pooled.Forecast <- Pooled.Forecast + Forecast$mean[1]
      Commentary      <- paste0(Commentary, "<li>Model ", idx.model, ": ", Model.Type[idx.model],
                                "  would forecast ", round(Forecast$mean[1],2), ".</li>")
    }
    Pooled.Forecast <- Pooled.Forecast / length(Regression.Output())
    Commentary      <- paste0(Commentary, "</ul>Simple forecast averaging can, in many cases, improve upon any single forecasting model. The average forecast over all models would predict ", 
                              round(Pooled.Forecast,2), " for the next release.")    
    return(Commentary)
  })
  
  output$Macro.Chart <- renderPlot({
    par(mfrow=c(2,2))
    for (idx.model in 1:length(Regression.Output())){
      Date.Frequency <- switch(periodicity(Regression.Data())$scale,
                               daily=365,
                               weekly=52,
                               monthly=12,
                               quarterly=4,
                               yearly=1)
      if (Date.Frequency ==4) {
        Forecast = forecast(Regression.Output()[[idx.model]], h=4)
        Forecast.index  <- seq(start(Regression.Data()), end(Regression.Data()) + years(1), by="3 months")
      } else if (Date.Frequency ==12 ) {
        Forecast = forecast(Regression.Output()[[idx.model]], h=12)
        Forecast.index  <- seq(start(Regression.Data()), end(Regression.Data()) + months(12), by="1 month")
      } else if (Date.Frequency ==52 ) {
        Forecast = forecast(Regression.Output()[[idx.model]], h=52)
        Forecast.index  <- seq(start(Regression.Data()), end(Regression.Data()) + weeks(52), by="1 week")
      }
      #browser()
      Chart.Data  <- zoo(, Forecast.index)
      Chart.Data  <- merge(Chart.Data, as.zoo(Regression.Data()))
      Chart.Data  <- merge(Chart.Data, zoo(data.frame(Forecast), 
                                           Forecast.index[!Forecast.index %in% index(Regression.Data())]))
      Chart.Data[end(Regression.Data()),] <- Chart.Data[end(Regression.Data()),1]
      names(Chart.Data) <- c("History", "Mean", "High", "Low", "Upper", "Lower")
      Chart.Data <- Chart.Data[index(Chart.Data)>=Sys.Date()-years(10),]
      
      #Plot.Data <- Chart.Data #zoo(Chart.Data[,-1], Chart.Data[,1])
      plot(Chart.Data$History, lwd=1, type="l", ylab="", xlab="", ylim=c(min(Chart.Data, na.rm=TRUE), max(Chart.Data, na.rm=TRUE)), #pch=19,
           main=paste0(input$Variable.Control.Choice, " (black)\nConfidence bands: 85% in blue, 95% in red"))
      lines(Chart.Data$High, col="blue", lwd=2)
      lines(Chart.Data$Low, col="blue", lwd=2)
      lines(Chart.Data$Upper, col="red", lwd=2)
      lines(Chart.Data$Lower, col="red", lwd=2)
      lines(Chart.Data$Mean, col="black", lwd=2, pch=19)
      
    }
    par(mfrow=c(1,1))
  })
  
  output$UI.Date <- renderText({
    Commentary.Date <- paste0("Last Data Update: ", format(Last.Update, "%d %B %Y"))
    return(Commentary.Date)
  })
  
  misc.EnsembleForecasting <- function(data, NoPredictors, NoReps, Date.Cutoff="2007-01-01"){
    # Function expects the dependent variable in the first column, and all predicts in the columns that follow
    NoVars     <- dim(data)[2]
    idx.Sample <- index(data) >= as.Date(Date.Cutoff)
    Results    <- matrix(NA, nrow=sum(idx.Sample), ncol=NoReps) 
    
    withProgress(session, min=1, max=NoReps, {
      setProgress(message = 'Calculation in progress',
                  detail = 'Calculating the Ensemble Forecast')
      
      for (idx.loop in (1:NoReps)){
        setProgress(value = idx.loop)
        VarsSelected <- sample(2:NoVars, NoPredictors, replace=FALSE)
        Regression   <- auto.arima(data[!idx.Sample,1], xreg=data[!idx.Sample,VarsSelected], allowdrift = FALSE)
        Forecast     <- predict(Regression, newxreg=data[ idx.Sample,VarsSelected], n.ahead=1)
        Results[,idx.loop]  <- Forecast$pred
      }
    
    Results.Reduced <- data.frame(Mean = apply(Results, 1, mean, na.rm=TRUE),
                                  t(apply(Results, 1, quantile, probs=c(0.1, 0.25, 0.4, 0.6, 0.75, 0.90), 
                                          na.rm=TRUE, names=TRUE)))
    Results.Reduced <- zoo(Results.Reduced, index(data)[idx.Sample])
    return(Results.Reduced)
  })
  }
  
  misc.plot.EnsembleForecasting <- function(EnsembleForecast, ChartTitle=""){
    plot(EnsembleForecast[,1], col="black", lwd=2, 
         ylim=c(min(EnsembleForecast, na.rm=TRUE), max(EnsembleForecast, na.rm=TRUE)),
         xlab="", ylab="", main=ChartTitle, 
         sub="Note: Intervall Ranges: 10%-90%; 25-75%; 40-60%")
    segments(index(EnsembleForecast), EnsembleForecast[,2], index(EnsembleForecast), EnsembleForecast[,7], lwd=10, col="lightskyblue")
    segments(index(EnsembleForecast), EnsembleForecast[,3], index(EnsembleForecast), EnsembleForecast[,6], lwd=10, col="dodgerblue")    
    segments(index(EnsembleForecast), EnsembleForecast[,4], index(EnsembleForecast), EnsembleForecast[,5], lwd=10, col="mediumblue")    
    lines(EnsembleForecast[,1], col="red", lwd=3)
  }
  
  EnsembleForecast.calc <- reactive({
    if (input$ForecastPooling.Selection == "GDP") {
    Forecast.Data <- Reduce(function(...) merge(...), list( US.Activity.ChicagoFed.Employment,
                                                            US.Activity.ChicagoFed,
                                                            US.Activity.PhillyFed.Current,
                                                            US.Activity.PhillyFed.Leading,
                                                            US.Activity.NYFed.Current,
                                                            US.Activity.NYFed.Leading,
                                                            US.Activity.NYFed.AvWorkWeek.Current,
                                                            US.Activity.NYFed.NoEmployees.Current,
                                                            US.Activity.NYFed.AvWorkWeek.Leading,
                                                            US.Activity.NYFed.NoEmployees.Leading,
                                                            US.Activity.SFFed.TechPulse,
                                                            US.Activity.ISM.NonManufacturing.Employment,
                                                            US.Activity.ISM.Manufacturing.Employment,
                                                            US.JOLTS.QuitsRate,
                                                            US.JOLTS.HireRate,
                                                            US.JOLTS.JobOpeningsRate)     )
    Forecast.Data   <- apply.quarterly(Forecast.Data, mean)
    index(Forecast.Data) <- index(Forecast.Data) - months(2)
    Forecast.Data   <- merge(US.GDP.Real.qq, Forecast.Data)
    } else if (input$ForecastPooling.Selection == "Nonfarm Payrolls") {
      Forecast.Data <- Reduce(function(...) merge(...), list( US.Payroll - lag(US.Payroll, 1),
                                                              US.Activity.ChicagoFed.Employment,
                                                              US.Activity.ChicagoFed,
                                                              US.Activity.PhillyFed.Current,
                                                              US.Activity.PhillyFed.Leading,
                                                              US.Activity.NYFed.Current,
                                                              US.Activity.NYFed.Leading,
                                                              US.Activity.NYFed.AvWorkWeek.Current,
                                                              US.Activity.NYFed.NoEmployees.Current,
                                                              US.Activity.NYFed.AvWorkWeek.Leading,
                                                              US.Activity.NYFed.NoEmployees.Leading,
                                                              US.Activity.SFFed.TechPulse,
                                                              US.Activity.ISM.NonManufacturing.Employment,
                                                              US.Activity.ISM.Manufacturing.Employment,
                                                              US.Activity.ADP - lag(US.Activity.ADP,1),
                                                              to.monthly(US.Activity.InitialClaims.4W.MA)[,1],
                                                              to.monthly(US.Activity.InitialClaims)[,1],
                                                              to.monthly(US.Activity.ContinuedClaims.4W.MA)[,1],
                                                              to.monthly(US.Activity.ContinuedClaims)[,1],
                                                              US.JOLTS.QuitsRate,
                                                              US.JOLTS.HireRate,
                                                              US.JOLTS.JobOpeningsRate)     )
    }
    Forecast.Data   <- Forecast.Data[index(Forecast.Data) >= as.Date("1980-01-01"),]
    Forecast.Result <- misc.EnsembleForecasting(data=Forecast.Data, NoPredictors=4, NoReps=250, Date.Cutoff="2012-01-01")
    return(Forecast.Result)
  })
  
  
  output$EnsembleForecast.Commentary <- renderText({
    EnsembleForecast.Result <- EnsembleForecast.calc()
    Commentary <- ""
    Commentary <- paste0(Commentary, "<ul><li>Based on information available up to ", format(Last.Update, "%A, %d %B %Y"), " in ca. 15 US activity indicators, the pooled forecast for ", input$ForecastPooling.Selection)
    if (input$ForecastPooling.Selection == "Nonfarm Payrolls") {
      Commentary <- paste0(Commentary, "for ", format(end(EnsembleForecast.Result), "%B %Y"))      
    }
    else if (input$ForecastPooling.Selection == "GDP") {
      Commentary <- paste0(Commentary, "for ", format(as.yearqtr(end(EnsembleForecast.Result)), "Q%q %Y"))
    }
    Commentary <- paste0(Commentary, " is ", round(tail(EnsembleForecast.Result[,1],1),2),"%. ")
    Commentary <- paste0(Commentary, "<li>The 25%-75% confidence bands around this forecast are ", round(tail(EnsembleForecast.Result[,3],1),2), "% and ",
                         round(tail(EnsembleForecast.Result[,6],1),2),"%. </ul>")
    return(Commentary)
  })
  
  output$EnsembleForecast.Plot <- renderPlot({
    return(misc.plot.EnsembleForecasting(  EnsembleForecast.Result <- EnsembleForecast.calc(), 
                                           ChartTitle=input$ForecastPooling.Selection))
  })
  
  
  # --------- STOCK MARKET
  
  
  output$Data.Realtime <- renderTable({
    data <- getQuote(ListOfCodes)
    data <- data.frame((names(ListOfCodes)), data)
    rownames(data) <- NULL
    colnames(data)[1]<-" "
    return(data)
  })
  
  output$StockSelector <- renderUI({
    selectInput("StockSelectorChoice", "Select a stock", names(ListOfCodes))
  })
  
  output$TestPlot <- renderPlot({
    Stock.Selected <- input$StockSelectorChoice
    Stock.Selected <- get(gsub("\\^", "", ListOfCodes[grep(Stock.Selected, names(ListOfCodes))]))
    plot(Stock.Selected, main=input$StockSelectorChoice)
    
  })
  
  output$LatestValue <- renderTable({
    Stock.Selected <- input$StockSelectorChoice
    Stock.Selected <- get(gsub("\\^", "", ListOfCodes[grep(Stock.Selected, names(ListOfCodes))]))
    Stock.Value    <- tail(Stock.Selected, 10)
    Stock.Info     <- data.frame(Period = as.character(index(Stock.Value)), Closing = Stock.Value[,4],
                                 Volume = Stock.Value[,5])
    rownames(Stock.Info) <- NULL
    return(Stock.Info)
  })
  
})