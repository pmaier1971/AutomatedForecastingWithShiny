rm(list=ls())
library(rvest)
library(shiny)
library(reshape)
library(googleVis)
library(quantmod)
library(zoo)
library(forecast)
library(ggplot2)
library(lubridate)
library(Quandl)
library(shinyIncubator)
library(httr)
library(rDrop)
library(tis)

# File Setup####
load("my_dropbox_credentials.rdata")
file.InputData        <- "https://www.dropbox.com/s/wrfre0as7q7x1sn/InputData.RData?dl=0"
file.Tracking         <- "https://www.dropbox.com/s/s4zvccv2dvqej7m/TrackingInfo.rda?dl=0"
file.DataTracking     <- "https://www.dropbox.com/s/xs4r6chlenm5kqk/DataTrackingInfo.rda?dl=0"
file.PriorGDPForecast <- "https://www.dropbox.com/s/6wpkhjmcsfjy8us/PriorGDPForecast.rda?dl=0"
file.PriorNFPForecast <- "https://www.dropbox.com/s/6480cbom34ysgnj/PriorNFPForecast.rda?dl=0"

# misc. functions

misc.growth.quarterlydata.to.qq.ar <- function(x){
  x <- (((x / lag(x, 1)^4)-1)*100)
}

misc.growth.quarterlydata.to.yy <- function(x){
  x <- 100*log(x / lag(x, 4))
}

misc.growth.monthlydata.to.yy <- function(x){
  x <- 100*log(x / lag(x, 12))
}

misc.NBER.Recessions <- function(){
  NBER.Recessions <- as.Date(as.character(t(nberDates())), format="%Y%m%d")
  Limits <- par('usr')
  for (idx in seq(1, length(NBER.Recessions), 2) ) {
    rect(NBER.Recessions[idx], Limits[3], NBER.Recessions[idx+1], Limits[4], col="#0000FF19", lty=0)
  }
}


shinyServer(function(input, output, session) {
  
  ListOfCodes <- c("SP 500"="^GSPC", 
                   "NASDAQ" = "^NDX",
                   "Hang Seng" = "^HSI",
                   "Nikkei" = "^N225",
                   "Dax" = "^GDAXI",
                   "FTSE" = "^FTSE",
                   "UST 10Y"="^TNX", 
                   "Apple"="AAPL", 
                   "Bank of America"="BAC", 
                   "JP Morgan"="JPM", 
                   "T-Mobile"="TMUS")
  
  
  # Test whether we are online - this allows adding code and running the app offline
  test.online        <- try(getSymbols("GDPC1",src='FRED'))
  
  if (!(inherits(test.online, "try-error"))) {
    status.online     <- TRUE
    # We are online. Load files from Dropbox
    data.input        <- GET(url = file.InputData)
    forecast.tracking <- (GET(url=file.Tracking))
    data.tracking     <- (GET(url=file.DataTracking))
    data.priorFC.GDP  <- (GET(url=file.PriorGDPForecast))
    data.priorFC.NFP  <- (GET(url=file.PriorNFPForecast))
    load(rawConnection(data.input$content))
#     load(rawConnection(forecast.tracking$content))
#     load(rawConnection(data.tracking$content))
#     load(rawConnection(data.priorFC.GDP$content))
#     load(rawConnection(data.priorFC.NFP$content))
    Table.DataTracking.Previous <- Table.DataTracking.Update # Save previous table
    
    # Is the data up-to-date?
    if (Sys.Date() != Last.Update) {
      # Data import
      cat("\n\n--------------------\nDownloading data online....")
      
      
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
                   "US.Activity.ISM.NonManufacturing" = "NMFBAI",
                   "US.Activity.ISM.Manufacturing" = "NAPM",
                   "US.Activity.ADP" = "NPPTTL",
                   "US.Activity.InitialClaims.4W.MA" = "IC4WSA",
                   "US.Activity.InitialClaims" = "ICSA",
                   "US.Activity.ContinuedClaims.4W.MA" = "CC4WSA",
                   "US.Activity.ContinuedClaims" = "CCSA",
                   #"US.Activity.TruckTonnage"="M03032USM350SNBR",
                   "US.JOLTS.QuitsRate" = "JTSQUR",
                   "US.JOLTS.HireRate" = "JTSHIR",
                   "US.JOLTS.JobOpeningsRate" ="JTSJOR",
                   "US.Unemployment.WageGrowth" = "CES0500000003",
                   "US.CPI.Headline"="CPIAUCSL", "US.CPI.Core"="CPILFENS",
                   "US.SOV.1Y"="DGS1", 
                   "US.SOV.2Y"="DGS2", 
                   "US.SOV.3Y"="DGS3", 
                   "US.SOV.5Y"="DGS5", 
                   "US.SOV.7Y"="DGS7", 
                   "US.SOV.10Y"="DGS10", 
                   "US.SOV.20Y"="DGS20", 
                   "US.SOV.30Y"="DGS30", 
                   "US.FSI.Cleveland"="CFSI",
                   "US.HouseholdDebt" = "HDTGPDUSQ163N",
                   "US.Housing.NewPrivateHousingStarts"="HOUSTNSA",
                   "US.Housing.NewPrivateHousingPermits"="PERMIT",
                   "US.Housing.NewPrivateHousingConstruction"="UNDCONTNSA",
                   "US.Housing.NewPrivateHousingCompleted"="COMPUTNSA",
                   "US.Housing.NewPrivate1UnitCompleted"="COMPU1UNSA",
                   "US.Housing.NewPrivate2UnitCompleted"="COMPU24UNSA",
                   "US.Housing.NewPrivate5UnitCompleted"="COMPU5MUNSA",
                   "US.Housing.30YMortgageRate"="MORTG",
                   "US.Housing.NewHomeSales" = "HSN1F",
                   "US.Housing.ExistingHomeSales" = "EXHOSLUSM495S",
                   "US.Housing.MonthlySupply"="MSACSR",
                   "US.Housing.AllTransactionsPriceIndex"="USSTHPI",
                   "US.Auto.Autosales" = "ALTSALES",
                   "US.Auto.MilesTraveled" = "M12MTVUSM227NFWA",
                   "US.Auto.FinanceCosts" = "TERMCBAUTO48NS",
                   "US.Auto.LoansSecuritized" = "MVLOAS",
                   "US.Auto.InventorySalesRatio" = "AISRSA"
      )
      Data.EU <- c("EU.GDP.Real"="EUNGDP", "EU.Unemployment"="LRHUTTTTEZM156S",
                   "EU.CPI.Headline"="CP0000EZ17M086NEST", "EU.CPI.Core"="00XESEEZ17M086NEST",
                   "EU.SOV.10Y"="IRLTLT01EZM156N", "FX.EU.USD"="DEXUSEU", "FX.EU.Effective"="RBXMBIS",
                   "EU.Survey.ConsumerConfidence.Expectations"="CSESFT02EZM460S",
                   "EU.Survey.ConsumerConfidence"="CSCICP02EZM460S",
                   "EU.Survey.ManufacturingConfidence"="BSCICP02EZM460S",
                   "EU.Survey.CapacityUtilization"="BSCURT02EZQ160S")
      Data.UK <- c("UK.GDP.Real"="UKNGDP", "UK.CPI.Headline"="CP0000GBM086NEST", "UK.CPI.Core"="GBRCPICORMINMEI",
                   "UK.Unemployment"="LMUNRRTTGBM156S", "FX.UK.USD"="DEXUSUK")
      Data.CA <- c("CA.GDP.Real"="NAEXKP01CAQ189S", "CA.CPI.Headline"="CANCPIALLMINMEI", 
                   "CA.CPI.Core"="CANCPICORMINMEI", "FX.CA.USD"="EXCAUS", "FX.CA.Effective"="RBCABIS",
                   "CA.HouseholdDebt" = "HDTGPDCAQ163N")
      Data.JP <- c("JP.CPI.Headline" = "JPNCPIALLMINMEI", 
                   "JP.CPI.Core" = "JPNCPICORMINMEI",
                   "JP.GDP.Real" = "NAEXKP01JPQ661S")
    
      Data.MarketUpdate <- c("Market.Gold"="GOLDAMGBD228NLBM"       ) # To be used later
      
      List.Countries   <- c("Data.US", "Data.EU", "Data.UK", "Data.CA", "Data.JP")
      max.DataDownload <- 0
      for (idx in 1:length(List.Countries)){
        max.DataDownload <- max.DataDownload + length(get(List.Countries[idx]))
      }
      
      # Assign names
      # Some variables need to be transformed
      List.Transformation <- c("US.CPI.Core", "US.CPI.Headline", "EU.CPI.Headline", "EU.CPI.Core", 
                               "US.IP", "CA.IP", "UK.CPI.Headline", "UK.CPI.Core", "CA.CPI.Headline", "CA.CPI.Core",
                               "JP.CPI.Headline", "JP.CPI.Core")
      
      withProgress(session, min = 1, max = max.DataDownload+10, {
        setProgress(message = "Downloading Data")
        Progress <- 1
        setProgress(value = Progress)
        Progress <- 5
        setProgress(value = Progress)
        
        Table.DataTracking.Update <- data.frame()
        
        for (idx.Country in 1:length(List.Countries)){
          
          cat("\n   Country: ", List.Countries[idx.Country])
          getSymbols(get(List.Countries[idx.Country]), src="FRED")
          for (idx in 1:length(get(List.Countries[idx.Country]))){
            setProgress(value = Progress)
            Progress <- Progress + 1
            
            cat("\n      - ", names(get(List.Countries[idx.Country]))[idx])
            x <- get(get(List.Countries[idx.Country])[idx])
            if (names(get(List.Countries[idx.Country]))[idx] %in% List.Transformation){
              x <- misc.growth.monthlydata.to.yy(x)  }
            assign(names(get(List.Countries[idx.Country]))[idx], x)
            
            Table.DataTracking.Update <- rbind(Table.DataTracking.Update,
                                               data.frame(Date = as.character(Sys.Date()),
                                                          Indicator = names(get(List.Countries[idx.Country]))[idx],
                                                          LastDate  = end(x),
                                                          LastValue = as.numeric(tail(x,1)),
                                                          PriorValue = as.numeric(tail(x,2)[1]) ))
          }
        }
      })
       
      List.Countries <- c("US", "EU", "UK", "CA", "JP") #"CN"
      List.Variables <- ls()
      
      
      for (i in 1:length(List.Countries)){
        Country     <- List.Countries[i]
        Data.Name   <- paste(Country,".GDP.Real.qq", sep="")
        FC.Name     <- paste(Country,".GDP.qq.FC.Naive", sep="")
        Chart.Name  <- paste(Country,".GDP.qq.FC.Naive.chart", sep="")
        
        Data.Series <- get(paste(Country, ".GDP.Real", sep=""))
        Data.Series <- tail(Data.Series, 60) # Last 15 years
        if (Country == "China") Data.Series <- 100*log(Data.Series / lag(Data.Series, 1))
        else Data.Series <- (((Data.Series / lag(Data.Series, 1))^4)-1)*100
        
        assign(Data.Name, Data.Series)
        assign(FC.Name, auto.arima(Data.Series))
      }
      
      Last.Update      <- Sys.Date()
      Last.Update.Time <- Sys.time()
      save.image("InputData.RData")
      
      # Do we have new data?
      Data.Change  <- FALSE
      
      if (!dim(Table.DataTracking.Update)[1]==dim(Table.DataTracking.Previous)[1]) {
        Data.Change <- TRUE
        cat("\n     --> New data series have been added")
      } else {
        Table.Change   <- which(!Table.DataTracking.Update[,c(4:5)]==Table.DataTracking.Previous[,c(4:5)])
        if (length(Table.Change)>1) {
          Data.Change  <- TRUE
          Table.Change <- Table.DataTracking[Table.Change,]
          Table.Change <-na.omit(Table.Change)
          print(Table.Change)
          cat("\n     --> New data is available for the forecast")
        } else cat("\n     --> Data for the forecast has not changed")
      }
    } else {
      cat("\nData file is up to date")
    }
    
    # Get the market data
    cat("\n   --> Downloading stock market data.... ")
    for (i in 1:length(ListOfCodes)){
      getSymbols(ListOfCodes[i], src='yahoo')
    }  
    cat("done.")

  } else {
    cat("\nNo online connection.")
    load("InputData.RData")
    status.online <- FALSE
  }
  
  misc.UpDown <- function(x){
    x <- abs( round(x,2) )
    if        (x>0 )   return(paste0("up ", x))
    else if   (x<0 )   return(paste0("down ", x))
    else if   (x==0)   return("unchanged")
  }
  
  # Market Update####
  output$MarketUpdate.Commentary <- renderText({
    Commentary <- paste0("<b>Market Update (", format(Last.Update.Time, "%d %B %Y, %H:%M"), ")</b><p>")
    Commentary <- paste0(Commentary, "<b>United States:</b><ul><li>The S&P 500 currently stands at ", 
                         round(last(GSPC)[,4],2), ", ", misc.UpDown(last(diff(GSPC)[,4])), " from the last close. ")
    Commentary <- paste0(Commentary, "<li>The NASDAQ is at ", 
                         round(last(NDX)[,4],2), ", ", misc.UpDown(last(diff(NDX)[,4])), " points. ")
    Commentary <- paste0(Commentary, "<li>The US 10-Year Treasury Yield is at ", 
                         round(last(TNX)[,4],2), " (", misc.UpDown(last(diff(GSPC)[,4])), " points).</ul>")
    Commentary <- paste0(Commentary, "<b>Europe:</b><ul><li> In Frankfurt the DAX closed at ", round(last(GDAXI)[,4],2), ", ", misc.UpDown(last(diff(GDAXI)[,4])), " from the last close. ")
    Commentary <- paste0(Commentary, "The London FTSE closed at ", round(last(FTSE)[,4],2), ", ", misc.UpDown(last(diff(FTSE)[,4])), " points. ")
    Commentary <- paste0(Commentary, "<li>The euro currently stands at ", 
                         round(last(DEXUSEU),3), ", ", misc.UpDown(last(diff(na.omit(DEXUSEU)))), 
                         " from the last close; ")
    Commentary <- paste0(Commentary, "the 10-Year Treasury Yield is at ", 
                         round(last(IRLTLT01EZM156N),3), " (", misc.UpDown(last(diff(IRLTLT01EZM156N))), 
                         " points). ")
    Commentary <- paste0(Commentary, "</ul><b>Asia:</b><ul><li> The Nikkei closed at ", round(last(N225)[,4],2), ", ", misc.UpDown(last(diff(N225)[,4])), " points. ")
    Commentary <- paste0(Commentary, 
                         "Hong Kong's Hang Seng closed at ", round(last(HSI)[,4],2), ", ", misc.UpDown(last(diff(HSI)[,4])), " from the last close. ")
    return(Commentary)
  })
  
  # Panel Overview####
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
      
      plot(Chart.Data[,1], type="l", col="blue", main = List.Countries[i], xlab="", ylab="", 
           lwd=2, ylim=range(Chart.Data[,4:7], na.rm = TRUE))  
      lines(Chart.Data[,2], col="red", lty=2)
      segments(index(Chart.Data), Chart.Data[,6], index(Chart.Data), Chart.Data[,7], col="deepskyblue", lwd=7)
      segments(index(Chart.Data), Chart.Data[,4], index(Chart.Data), Chart.Data[,5], col="tomato", lwd=10)
      lines(Chart.Data[,3], col="blue", lwd=1.5, pch=19, type="b")
      lines(Chart.Data[,3], col="blue", lwd=2, pch=19)
    }
    par(mfrow=c(1,1))    
  })
  
  
  # Panel Country Analysis####
  
  output$UI.Country.Analysis <- renderUI({
    selectInput("Country.Analysis.Control.Choice", "Select the country", List.Countries, selected=List.Countries[1])
  })
  
  output$Charts.Country.Analysis <-renderPlot({
    Country <- input$Country.Analysis.Control.Choice
    par(mfrow=c(2,3))
    plot(as.zoo(get(paste(Country,".GDP.Real.qq", sep=""))), main="Real GDP q/q", xlab="", ylab="")
    if (Country=="US") misc.NBER.Recessions()
    if (exists(paste(Country,".IP", sep="")))           plot(as.zoo(get(paste(Country,".IP", sep=""))), main="Industrial production", xlab="", ylab="")
    if (Country=="US") misc.NBER.Recessions()
    if (exists(paste(Country,".Unemployment", sep=""))) plot(as.zoo(get(paste(Country,".Unemployment", sep=""))), main="Unemployment", xlab="", ylab="")
    if (Country=="US") misc.NBER.Recessions()
    if (exists(paste(Country,".CPI.Headline", sep=""))) plot(as.zoo(get(paste(Country,".CPI.Headline", sep=""))), main="Headline CPI", xlab="", ylab="")
    if (Country=="US") misc.NBER.Recessions()
    if (exists(paste("FX.",Country,".USD", sep="")))    plot(as.zoo(get(paste("FX.",Country,".USD", sep=""))), main="Exchange rate against the USD", xlab="", ylab="")
    if (Country=="US") misc.NBER.Recessions()
    if (exists(paste(Country,".SOV.10Y", sep="")))      plot(as.zoo(get(paste(Country,".SOV.10Y", sep=""))), main="10Y Sovereign bond", xlab="", ylab="")    
    if (Country=="US") misc.NBER.Recessions()
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
  
  # Panel Detailed Analysis####
  
  US.ActivityMeasures.Data <- function(){
    Data.Dashboard <- Reduce(function(...) merge(...), list(US.Activity.ChicagoFed, 
                                                            US.Activity.PhillyFed.Leading,
                                                            US.Activity.NYFed.Leading,
                                                            US.Activity.NYFed.Current,
                                                            10*US.Activity.SFFed.TechPulse,
                                                            US.Activity.ISM.NonManufacturing,
                                                            US.Activity.ISM.Manufacturing ) )
    Data.Dashboard        <- data.frame(Period = index(Data.Dashboard), Data.Dashboard)
    Data.Dashboard        <- Data.Dashboard[Data.Dashboard$Period>"2007-12-01",]
    return(Data.Dashboard)
  }
  
  output$US.ActivityMeasures.Dashboard <- renderGvis({
    Data        <- US.ActivityMeasures.Data()
    names(Data) <- c("Period", 
                     "Chicago Fed National Activity Index", 
                     "Philly Fed Leading Index for the United States",
                     "Empire Survey: Current General Business Conditions",
                     "Empire Survey: Future General Business Conditions", 
                     "San Francisco Fed Tech Pulse",
                     "ISM Non-manufacturing: Business Activity Index", "ISM Manufacturing: PMI Composite Index")
    Data <- melt(Data, id.vars = "Period")
    
    Data.ISM <- Data[grep("ISM", Data$variable),]
    Data.Fed <- Data[grep("Fed", Data$variable),]
    
    Chart.ISM <- gvisAnnotatedTimeLine(Data.ISM, datevar="Period", date.format = "%Y-%m-%d", numvar = "value", idvar = "variable",
                                       options = list(displayAnnotations       = FALSE,
                                                      displayZoomButtons       = FALSE, 
                                                      legendPosition           ='newRow'))
    Chart.Fed <- gvisAnnotatedTimeLine(Data.Fed, datevar="Period", date.format = "%Y-%m-%d", numvar = "value", idvar = "variable",
                                       options = list(displayAnnotations       = FALSE,
                                                      displayZoomButtons       = FALSE, 
                                                      legendPosition           ='newRow'))
    return(gvisMerge(Chart.ISM, Chart.Fed, horizontal = TRUE))
    
  })
  
  output$US.ActivityMeasures.Change <- renderPlot({
    Data        <- US.ActivityMeasures.Data()
    Data        <- zoo(Data[,-1], Data[,1])
    Data.Change <- Data - lag(Data,-1)
    Data.Merge  <- merge(Data, Data.Change)
    Data.Merge  <- tail(data.frame(Period = index(Data.Merge), Data.Merge), 4)
    Data.Merge  <- melt(Data.Merge, id.vars = "Period")
    idx         <- grepl(".Change", Data.Merge$variable)
    Data.Merge$Type <- NA
    Data.Merge$Type[!idx] <- "Level"
    Data.Merge$Type[idx]  <- "Change"
    Data.Merge$variable   <- gsub(".Change", "", Data.Merge$variable)
    
    
    Data.Merge <- cast(Data.Merge, Period + variable ~ Type)
    
    #     Data.Merge$variable <- c("Chicago Fed National Activity Index", 
    #                              "Philly Fed Leading Index for the United States",
    #                              "Empire Survey: Current General Business Conditions",
    #                              "Empire Survey: Future General Business Conditions", 
    #                              "San Francisco Fed Tech Pulse",
    #                              "ISM Non-manufacturing: Business Activity Index", "ISM Manufacturing: PMI Composite Index")
    idx <- Data.Merge$variable %in% c("NAPM.Data", "NMFBAI.Data")
    Data.ISM <- Data.Merge[idx,]
    Data.Fed <- Data.Merge[!idx,]
    
    Data.ISM <- tail(na.omit(Data.ISM),4)
    Data.ISM$variable <- gsub("NAPM.Data", "ISM Manufacturing: PMI Composite Index", Data.ISM$variable)
    Data.ISM$variable <- gsub("NMFBAI.Data", "ISM Non-manufacturing: Business Activity Index", Data.ISM$variable)
    
    xaxis <- c(min(min(Data.ISM$Change)-1, -5), max(max(Data.ISM$Change)+1, 10))
    yaxis <- c(min(min(Data.ISM$Level)-1, 45), max(max(Data.ISM$Level)+1, 55))
    plot(Data.ISM[,3:4], xlim = xaxis, ylim=yaxis, col=c("red", "red", "blue", "blue"), pch=19) 
    text(Data.ISM[,3], Data.ISM[,4], Data.ISM[,2], cex=0.6, pos=4)
    legend("topleft", as.character(unique(format(as.Date(Data.ISM$Period), "%B %Y"))), fill=c("red", "blue"), horiz = FALSE)
    abline(h=50, v=0, lty=2)
  })
  
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
  
  US.HousingMarket.Dashboard.Data <- function(){
    Data.Dashboard <- Reduce(function(...) merge(...), list(
      US.Housing.NewPrivateHousingPermits,
      US.Housing.NewPrivateHousingStarts,
      US.Housing.NewPrivateHousingConstruction,
      US.Housing.NewPrivateHousingCompleted,
      US.Housing.NewPrivate1UnitCompleted,
      US.Housing.NewPrivate2UnitCompleted,
      US.Housing.NewPrivate5UnitCompleted,
      US.Housing.30YMortgageRate))
    
    names(Data.Dashboard) <- c("New Private Housing Units \nAuthorized by Building Permits",
                               "Housing Starts: Total: \nNew Privately Owned Housing Units Started",
                               "New Privately-Owned Housing Units Under Construction: Total",
                               "New Privately-Owned Housing Units Completed: Total",
                               "New Privately-Owned Housing Units Completed: \n1-Unit Structures",
                               "New Privately-Owned Housing Units Completed: \n2-4 Unit Structures",
                               "New Privately-Owned Housing Units Completed: \n5-Unit Structures or More",
                               "30-Year Conventional Mortgage Rate"
    )
    return(Data.Dashboard)
  }
  
  
  output$US.HousingMarket.Dashboard <- renderPlot({
    Data <- as.zoo(US.HousingMarket.Dashboard.Data())
    if (input$UIHousingDashboardHistoryControl) Data <- Data[index(Data)>(Sys.Date() - years(20)),] else Data <- Data[index(Data)>(Sys.Date() - years(5)),]
    
    Chart.Layout <- matrix(c(1,2,3,4,5,6,7,8), ncol=2, byrow=TRUE)
    layout(Chart.Layout)
    op <- par(mar = par("mar")/1.2)                     
    
    for (idx in 1:ncol(Data)) {
      Chart.Title <- names(Data)[idx]
      plot(Data[,idx], col="blue", type="l", lwd = 2, main=Chart.Title, ylab="", xlab="")
      lines(rollmean(Data[,idx],k=6, align="right"), col="red", lty=2)
      legend("bottomleft", legend=c("Raw Data", "6 Month Moving Average"), fill=c("blue", "red"))
    }
    par(op)
  })
  
  US.AutoMarket.Dashboard.Data <- function(){
    Data.Dashboard <- Reduce(function(...) merge(...), list(
      US.Auto.Autosales,
      misc.growth.monthlydata.to.yy(US.Auto.Autosales),
      US.Auto.InventorySalesRatio,
      US.Auto.MilesTraveled,
      US.Auto.FinanceCosts,
      US.Auto.LoansSecuritized
    ))
    
    names(Data.Dashboard) <- c("Light Weight Vehicle Sales: Autos & Light Trucks (Level)",
                               "Light Weight Vehicle Sales: Autos & Light Trucks (y/y)",
                               "Auto Inventory/Sales Ratio",
                               "Moving 12-Month Total Vehicle Miles Traveled",
                               "Finance Rate on Consumer Installment Loans at Commercial Banks, New Autos 48 Month Loan
                               
                               ",
                               "Motor Vehicle Loans Owned and Securitized, Outstanding"
    )
    return(Data.Dashboard)
  }
  
  output$US.AutoMarket.Dashboard <- renderPlot({
    Data <- as.zoo(US.AutoMarket.Dashboard.Data())
    if (input$UIAutoDashboardHistoryControl) Data <- Data[index(Data)>(Sys.Date() - years(20)),] else Data <- Data[index(Data)>(Sys.Date() - years(5)),]
    
    Chart.Layout <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
    layout(Chart.Layout)
    op <- par(mar = par("mar")/1.2)                     
    
    for (idx in 1:ncol(Data)) {
      Chart.Title <- names(Data)[idx]
      miss <- !is.na(Data[,idx])
      plot(Data[miss,idx], col="blue", type="l", main=Chart.Title, ylab="", xlab="")
      if (idx <= ncol(Data) -2) {
        lines(rollmean(Data[,idx],k=6, align="right"), col="red", lty=2)
        legend("topleft", legend=c("Raw Data", "6 Month Moving Average"), fill=c("blue", "red"))
      }
    }
    par(op)
  })
  
  US.InterestRates.Dashboard.Data <- function(){
    Data.Dashboard <- Reduce(function(...) merge(...), list( US.SOV.1Y, 
                                                             US.SOV.2Y, 
                                                             US.SOV.3Y, 
                                                             US.SOV.5Y, 
                                                             US.SOV.7Y, 
                                                             US.SOV.10Y ))
    return(Data.Dashboard)
  }
  
  output$US.InterestRates.Dashboard <- renderPlot({
    Data.Rates     <- US.InterestRates.Dashboard.Data()
    week           <- function(x)format(x, '%Y.W%W')
    month          <- function(x)format(x, '%Y.M%m')
    year           <- function(x)format(x, '%Y')
    Data.Rates.dim <- length(colnames(Data.Rates))
    
    Data.Rates.Y <- as.zoo(Data.Rates[index(Data.Rates)>=Sys.Date()-years(15),])
    
    Data.Rates[,2:Data.Rates.dim] <- Data.Rates[,2:Data.Rates.dim] - Data.Rates[,1:(Data.Rates.dim-1)]
    
    Data.Rates.M <- as.zoo(Data.Rates[index(Data.Rates)>=Sys.Date()-years(5),])
    Data.Rates.W <- as.zoo(Data.Rates[index(Data.Rates)>=Sys.Date()-years(1),])
    Data.Rates.D <- na.omit(as.zoo(Data.Rates[index(Data.Rates)>=Sys.Date()-months(6),]))
    Data.Rates.W <- aggregate(Data.Rates.W, by=week, FUN=mean, na.rm=TRUE)
    Data.Rates.M <- aggregate(Data.Rates.M, by=month, FUN=mean, na.rm=TRUE)
    Data.Rates.Y <- aggregate(Data.Rates.Y, by=year, FUN=mean, na.rm=TRUE)
    
    Data.Rates.Names<-c("1Y", "2Y", "3Y", "5Y", "7Y", "10Y") # , "20Y", "30Y")
    
    par(mfrow = c(2,2))
    barplot(Data.Rates.D, col=rainbow(Data.Rates.dim), #cex.main=0.75,
            main="Treasury Rates (Constant Maturity, Daily Yields in % For The Last 6 Months)")
    legend("bottomleft", Data.Rates.Names, fill=rainbow(Data.Rates.dim), cex=0.75)
    
    barplot(na.omit(Data.Rates.W), col=rainbow(Data.Rates.dim), #cex.main=0.75,
            main="Treasury Rates (Constant Maturity, Weekly Average Yields in % For The Last Year)")
    legend("bottomleft", Data.Rates.Names, fill=rainbow(Data.Rates.dim), cex=0.75)
    
    barplot(Data.Rates.M, col=rainbow(Data.Rates.dim), #cex.main=0.75,
            main="Treasury Rates (Monthly Average Yield in % For The Last 5 Years)")
    legend("bottomleft", Data.Rates.Names, fill=rainbow(Data.Rates.dim), cex=0.75)
    
    plot(na.omit(Data.Rates.Y[,Data.Rates.dim]), type="n", col=rainbow(Data.Rates.dim), 
         main="Treasury Rates (Yearly Average Yields in % For The Last 15 Years)", xlab="", ylab="",
         ylim=c(min(Data.Rates.Y), max(Data.Rates.Y)))
    for (idx in 1:Data.Rates.dim){
      lines(Data.Rates.Y[,idx], col=tail(rainbow(Data.Rates.dim)[idx],1))
    }
    grid(NA, NULL, lty=2, col="gray")
    legend("bottomleft", Data.Rates.Names, fill=rainbow(Data.Rates.dim), cex=0.75)
    par(mfrow = c(1,1))
  })
  
output$US.InterestRates.Commentary <- renderText({
  Commentary <- "US Treasuries are often considered the 'riskless asset' for the US economy, and therefore play a vital role as benchmark interest rates for other asset classes. "
  Commentary <- paste0(Commentary, "Treasuries are very liquid and heavily traded in secondary markets. A compression in the spread between shorter and longer maturities - i.e. a flattening of the yield curve - indicates that investors gain less (additional) compensation for holding longer-term securities. ")
  Commentary <- paste0(Commentary, "<p>Below we plot US Treasury Rates at different maturities. ")
  Commentary <- paste0(Commentary, "On ", format(index(tail(TNX,1)), "%B %d, %Y"), " the following yields were observed at closing: ")
  Commentary <- paste0(Commentary, "<ul><li> On the short end of the curve, the 1Y closed at ", tail(100*US.SOV.1Y,1)," bps, ", misc.UpDown(last(diff(na.omit(100*US.SOV.5Y)))), " from the last close.")
  Commentary <- paste0(Commentary, "<li> 5Y and 7Y Treasuries closed at ", tail(US.SOV.5Y,1),"% and ", tail(US.SOV.7Y,1),"%.")
  Commentary <- paste0(Commentary, "<li> Lastly, on the long end of the curve, the 10Y US Treasury closed at ", tail(100*TNX[,4],1)," bps; ", misc.UpDown(last(diff(100*TNX[,4]))), ".")
  Commentary <- paste0(Commentary, "</ul> Below we plot US Treasury interest rates and spreads for differt time horizons.")
  return(Commentary)
})

VehicleSales.HTML <- GET(url="https://www.dropbox.com/s/lmyh1s68ntb73q2/VehicleSales.html?dl=0")
VehicleSales.HTML <-content(VehicleSales.HTML, as="text")
output$VehiclesSales.Dashboard <- renderText({VehicleSales.HTML})

Housing.HTML <- GET(url="https://www.dropbox.com/s/ad85f97rk5eoj2v/Housing.html?dl=0")
Housing.HTML <-content(Housing.HTML, as="text")
output$Housing.Dashboard <- renderText({Housing.HTML})



InternationalInflation.HTML <- GET(url="https://www.dropbox.com/s/xni9gh5j2czblcf/Inflation.html?dl=0")
InternationalInflation.HTML <-content(InternationalInflation.HTML, as="text")
output$International.InflationAnalysis.Dashboard <- renderText({InternationalInflation.HTML})

output$International.Inflation.Dashboard <- renderPlot({
  
  CoreInflation.Comparison.Data <- CoreInflation.Comparison.Data[index(CoreInflation.Comparison.Data)>=Sys.Date()-years(input$InflationComparisonChoice)]
 # CoreInflation.Comparison.Data <- apply(CoreInflation.Comparison.Data, 2, function(X) 100*(X/X[1]))
 # CoreInflation.Comparison.Data <- cbind(CoreInflation.Comparison.Data, c(100, 100 * cumprod(rep(1.02^(1/12), nrow(CoreInflation.Comparison.Data)-1))))
 
 Inflation.Comparison.Data.Chart <- apply(CoreInflation.Comparison.Data, 2, function(X) 100*(X/X[1]))
 Inflation.Comparison.Data.Chart <- cbind(Inflation.Comparison.Data.Chart, c(100, 100 * cumprod(rep(1.02^(1/12), nrow(Inflation.Comparison.Data.Chart)-1))))
 
 plot(as.zoo(Inflation.Comparison.Data.Chart[,1]), main = "Cumulative Change in the Price Level (Headline)", type = "n",
      xaxt="n", xlab="", ylab="", ylim=c(min(Inflation.Comparison.Data.Chart, na.rm=TRUE), max(Inflation.Comparison.Data.Chart, na.rm=TRUE)))
 axis(1, at=row(CoreInflation.Comparison.Data[,1]), label = index(CoreInflation.Comparison.Data),
      col.axis="black", cex.axis=0.7)
 line.color <- rainbow(ncol(Inflation.Comparison.Data.Chart))
 for (idx in 1:ncol(Inflation.Comparison.Data.Chart)){
   if (idx == ncol(Inflation.Comparison.Data.Chart)) {
     lines(as.zoo(Inflation.Comparison.Data.Chart[,idx]), col = line.color[idx], lwd=2, lty=2)
   } else {
     lines(as.zoo(Inflation.Comparison.Data.Chart[,idx]), col = line.color[idx], lwd=2)
   }
 }
 legend("topleft", legend=c("United States", "Euro Area", "United Kingdom", "Japan", "2% Trend"), fill=rainbow(idx), cex=0.75) 
 
})

  # Panel Macroeconomic Forecasting####
  
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
      
      Chart.Data  <- zoo(, Forecast.index)
      Chart.Data  <- merge(Chart.Data, as.zoo(Regression.Data()))
      Chart.Data  <- merge(Chart.Data, zoo(data.frame(Forecast), 
                                           Forecast.index[!Forecast.index %in% index(Regression.Data())]))
      Chart.Data[end(Regression.Data()),] <- Chart.Data[end(Regression.Data()),1]
      names(Chart.Data) <- c("History", "Mean", "High", "Low", "Upper", "Lower")
      Chart.Data <- Chart.Data[index(Chart.Data)>=Sys.Date()-years(10),]
      
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
  
  # EnsembleForecasting####
  misc.EnsembleForecasting <- function(data, NoPredictors, NoReps){
    # Function expects the dependent variable in the first column, and all predicts in the columns that follow
    idx.Sample <- index(data) >= max(index(na.omit(data[,1])))
    NoVars     <- dim(data)[2]
    Results    <- matrix(NA, nrow=dim(data)[1], ncol=NoReps) 
    
    cat("\n    --> Calculate EnsembleForecast")
    
    withProgress(session, min=1, max=NoReps, {
       setProgress(message = 'Calculation in progress',
                   detail = 'Calculating the Ensemble Forecast')
    
      for (idx.loop in (1:NoReps)){
       setProgress(value = idx.loop)
        VarsSelected   <- sample(2:NoVars, NoPredictors, replace=FALSE)
       #Regression     <- auto.arima(data[!idx.Sample,1], xreg=data[!idx.Sample,VarsSelected], allowdrift = FALSE)
       Regression     <- Arima(data[!idx.Sample,1], xreg=data[!idx.Sample,VarsSelected], order=c(1,0,0))
       Regression.fit <- fitted(Regression)
       
       Forecast       <- predict(Regression, newxreg=data[idx.Sample,VarsSelected], n.ahead=1)
       Results[,idx.loop]  <- c(fitted(Regression), Forecast$pred)
      }
      
      Results.Reduced <- data.frame(Mean = apply(Results, 1, mean, na.rm=TRUE),
                                    t(apply(Results, 1, quantile, probs=c(0.1, 0.25, 0.4, 0.6, 0.75, 0.90), 
                                            na.rm=TRUE, names=TRUE)))
      Results.Reduced <- zoo(Results.Reduced, index(data))
      Results.Reduced <- merge(Results.Reduced, as.zoo(data[,1]))
  Results.Reduced <- Results.Reduced[!is.na(Results.Reduced[,1]),]
      return(Results.Reduced)
   })
  }
  
  misc.plot.EnsembleForecasting <- function(EnsembleForecast, ChartTitle=""){
    if (dim(EnsembleForecast)[2]<=7) SubTitle <- "Note: Red = Mean Forecast; Interval Ranges: 10%-90%; 25-75%; 40-60%"
    if (dim(EnsembleForecast)[2]>7) SubTitle <- "Note: Green=Actuals; Red = Mean Forecast; Interval Ranges: 10%-90%; 25-75%; 40-60%"
    
    plot(EnsembleForecast[,1], col="black", lwd=2, 
         ylim=c(min(EnsembleForecast, na.rm=TRUE), max(EnsembleForecast, na.rm=TRUE)),
         xlab="", ylab="", main=ChartTitle, 
         sub=SubTitle)
    segments(index(EnsembleForecast), EnsembleForecast[,2], index(EnsembleForecast), EnsembleForecast[,7], lwd=10, col="lightskyblue")
    segments(index(EnsembleForecast), EnsembleForecast[,3], index(EnsembleForecast), EnsembleForecast[,6], lwd=10, col="dodgerblue")    
    segments(index(EnsembleForecast), EnsembleForecast[,4], index(EnsembleForecast), EnsembleForecast[,5], lwd=10, col="mediumblue")    
    lines(EnsembleForecast[,1], col="red", lwd=3)
    if (dim(EnsembleForecast)[2]>7) lines(EnsembleForecast[,8], col="green", lwd=3, lty=2)
  }
  
  misc.ShiftPeriodForMissingValues <- function(Forecast.Data){
    for (idx.Period in (2:dim(Forecast.Data)[2])){
      loop.stop <- FALSE
      while (loop.stop == FALSE){
        if(!end(Forecast.Data[,idx.Period][!is.na(Forecast.Data[,idx.Period])])>=
             end(Forecast.Data[,1][!is.na(Forecast.Data[,1])])){
          cat("\nMissing: ", idx.Period)
          Forecast.Data[, idx.Period] <- 
            c(NA, Forecast.Data[(1:length(Forecast.Data[, idx.Period])-1), idx.Period])
        } else loop.stop <- TRUE
      }
    }
    return(Forecast.Data)
  }
  
  EnsembleForecast.calc <- reactive({
    Forecast.Selection <- input$ForecastPooling.Selection
    if (Data.Change) {
      cat("\n    -> New data for the forecast is available")
      if (Forecast.Selection == "GDP") {
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
                                                                US.Activity.ISM.NonManufacturing,
                                                                US.Activity.ISM.Manufacturing,
                                                                US.JOLTS.QuitsRate,
                                                                US.JOLTS.HireRate,
                                                                US.JOLTS.JobOpeningsRate)     )
        Forecast.Data            <- apply.quarterly(Forecast.Data, mean)
        index(Forecast.Data)     <- as.Date(as.yearqtr(index(Forecast.Data)))
        Forecast.Data            <- merge(US.GDP.Real.qq, Forecast.Data)
        Forecast.Data            <- misc.ShiftPeriodForMissingValues(Forecast.Data)
        Lag.Forecast.Data        <- Forecast.Data
        index(Lag.Forecast.Data) <- index(Lag.Forecast.Data) + months(3)
        Forecast.Data            <- merge(Forecast.Data, Lag.Forecast.Data)
      } else if (Forecast.Selection == "Nonfarm Payrolls") {
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
                                                                US.Activity.ISM.NonManufacturing,
                                                                US.Activity.ISM.Manufacturing,
                                                                US.Activity.ADP - lag(US.Activity.ADP,1),
                                                                to.monthly(US.Activity.InitialClaims.4W.MA)[,1],
                                                                to.monthly(US.Activity.InitialClaims)[,1],
                                                                to.monthly(US.Activity.ContinuedClaims.4W.MA)[,1],
                                                                to.monthly(US.Activity.ContinuedClaims)[,1],
                                                                US.JOLTS.QuitsRate,
                                                                US.JOLTS.HireRate,
                                                                US.JOLTS.JobOpeningsRate)     )
        Forecast.Data            <- misc.ShiftPeriodForMissingValues(Forecast.Data)
        Lag.Forecast.Data        <- Forecast.Data
        index(Lag.Forecast.Data) <- index(Lag.Forecast.Data) + months(1)
        Forecast.Data            <- merge(Forecast.Data, Lag.Forecast.Data)
      }
      Forecast.Data   <- Forecast.Data[index(Forecast.Data) >= as.Date("1980-01-01"),]
      Forecast.Result <- misc.EnsembleForecasting(data=Forecast.Data, NoPredictors=4, NoReps=250)
      Forecast.Result <- Forecast.Result[1:(match(max(index(na.omit(Forecast.Result[,8]))), index(Forecast.Result))+1),]
      Forecast.Result <- Forecast.Result[index(Forecast.Result) >="2000-01-01",]
      
      Forecast.Result.save          <-  head(Forecast.Result[index(Forecast.Result)>max(index(na.omit(Forecast.Result[,8]))),],1)
      # Save for the tracking table
      Table.ForecastTracking.Update <- data.frame(ForecastDate     = Sys.Date(),
                                                  NewRelease       = NA,
                                                  ValueNewRelease  = NA,
                                                  ForecastVariable = Forecast.Selection, #input$ForecastPooling.Selection,
                                                  ForecastPeriod   = index(Forecast.Result.save),
                                                  ForeastMean = Forecast.Result.save[,1],
                                                  Forecast10P = Forecast.Result.save[,2],
                                                  Forecast25P = Forecast.Result.save[,3],
                                                  Forecast40P = Forecast.Result.save[,4],
                                                  Forecast60P = Forecast.Result.save[,5],
                                                  Forecast75P = Forecast.Result.save[,6],
                                                  Forecast90P = Forecast.Result.save[,7]
      )
      if (!exists("Table.ForecastTracking")) Table.ForecastTracking <- Table.ForecastTracking.Update else  Table.ForecastTracking <- rbind(Table.ForecastTracking, Table.ForecastTracking.Update)
      if (status.online) {
        dropbox_save(dropbox_credentials, Table.ForecastTracking, file='work/bac/AutomatedForecastingWithShiny/TrackingInfo.rda')
        cat("\n    - Forecast saved in Dropbox") }
      if (!exists("Table.DataTracking")) Table.DataTracking <- Table.DataTracking.Update else  Table.DataTracking <- rbind(Table.DataTracking, Table.DataTracking.Update)
      if (status.online) { 
        dropbox_save(dropbox_credentials, Table.DataTracking, file='work/bac/AutomatedForecastingWithShiny/DataTrackingInfo.rda')
        cat("\n    - Data tracking saved in Dropbox")
      }
      if (Forecast.Selection == "GDP")              {
        Prior.ForecastResult.GDP <- Forecast.Result
        if (status.online) dropbox_save(dropbox_credentials, Prior.ForecastResult.GDP, file='work/bac/AutomatedForecastingWithShiny/PriorGDPForecast.rda') 
      }
      if (Forecast.Selection == "Nonfarm Payrolls") {
        Prior.ForecastResult.NFP <- Forecast.Result
        if (status.online) { 
          dropbox_save(dropbox_credentials, Prior.ForecastResult.NFP, file='work/bac/AutomatedForecastingWithShiny/PriorNFPForecast.rda')
          cat("\n    - Saved as prior forecast in Dropbox") }
      }
    } else {
      if (Forecast.Selection == "GDP")              Forecast.Result <- Prior.ForecastResult.GDP
      if (Forecast.Selection == "Nonfarm Payrolls") Forecast.Result <- Prior.ForecastResult.NFP
      cat("\n   --> Using forecast results stored previously")
    }
    return(Forecast.Result)
  })
  
  output$Forecast.Tracking <- renderPrint({
    if (input$ForecastPooling.Selection == "GDP")              Table.Change.Print <- Table.ForecastTracking.GDP
    if (input$ForecastPooling.Selection == "Nonfarm Payrolls") Table.Change.Print <- Table.ForecastTracking.NFP
    
    Table.Change.Print <- subset(Table.Change.Print, ForecastPeriod == max(ForecastPeriod))
    
    Table.Change.Print <- Table.Change.Print[,c(1:3,6)]
    
    Table.Change.Print[,4]       <-round(Table.Change.Print[,4],2)
    rownames(Table.Change.Print) <- NULL
    colnames(Table.Change.Print) <- c("Date", "Indicator", "Value", "Forecast")
    return(Table.Change.Print)
  })
  
  output$EnsembleForecast.Commentary <- renderText({
    EnsembleForecast.Result <- EnsembleForecast.calc()
    Commentary <- ""
    Commentary <- paste0(Commentary, "<ul><li>Based on information available up to ", format(Last.Update, "%A, %d %B %Y"), " in ca. 15 US activity indicators, the pooled forecast for ", input$ForecastPooling.Selection)
    if (input$ForecastPooling.Selection == "Nonfarm Payrolls") {
      Commentary <- paste0(Commentary, " for ", format(end(EnsembleForecast.Result), "%B %Y"))      
    }
    else if (input$ForecastPooling.Selection == "GDP") {
      Commentary <- paste0(Commentary, " for ", format(as.yearqtr(end(EnsembleForecast.Result)), "Q%q %Y"))
    }
    Commentary <- paste0(Commentary, " is ", round(tail(EnsembleForecast.Result[,1],1),2),". ")
    Commentary <- paste0(Commentary, "<li>The 25%-75% confidence bands around this forecast are ", round(tail(EnsembleForecast.Result[,3],1),2), " and ",
                         round(tail(EnsembleForecast.Result[,6],1),2),". </ul>")
    return(Commentary)
  })
  
  output$EnsembleForecast.Plot <- renderPlot({
    return(misc.plot.EnsembleForecasting(  EnsembleForecast.Result <- EnsembleForecast.calc(), 
                                           ChartTitle=input$ForecastPooling.Selection))
  })
  
  output$EnsembleForecast.Tracking <- renderPlot({
    cat("\n       Show forecast tracking chart")
    if (input$ForecastPooling.Selection == "GDP")              Chart.Data <- Table.ForecastTracking.GDP
    if (input$ForecastPooling.Selection == "Nonfarm Payrolls") Chart.Data <- Table.ForecastTracking.NFP
    ChartTitle <- paste0("Forecast Tracking for ", input$ForecastPooling.Selection)    
    Chart.Data <- subset(Chart.Data, ForecastPeriod == max(ForecastPeriod))
    Chart.Data <- Chart.Data[, -(3:4)]    
    
    plot(Chart.Data[,4], type="l", ylim=c(min(Chart.Data[,-(1:3)], na.rm=TRUE), max(Chart.Data[,-(1:3)], na.rm=TRUE)),
         xaxt="n", xlab="", ylab="", main=ChartTitle)
    
    segments(index(Chart.Data[,1]), Chart.Data[,5], index(Chart.Data[,1]), Chart.Data[,10], lwd=10, col="lightskyblue")
    segments(index(Chart.Data[,1]), Chart.Data[,6], index(Chart.Data[,1]), Chart.Data[,9], lwd=10, col="dodgerblue")    
    segments(index(Chart.Data[,1]), Chart.Data[,7], index(Chart.Data[,1]), Chart.Data[,8], lwd=10, col="mediumblue")    
    lines(Chart.Data[,4], col="red", lwd=3)
    
    points(index(Chart.Data[,1]), Chart.Data[,4], pch=19, col="black")
    axis(1, at=index(Chart.Data[,1]), labels = Chart.Data[,1], las=3, cex.axis=.75)
    text.seq <- rep(c("TRUE", "FALSE"), length(index(Chart.Data)/2))
    
    text(x=index(Chart.Data[,5]), 
         y=(par('usr')[4]+par('usr')[3])/2 + rep(c(1,-1),length(index(Chart.Data))/2) * (par('usr')[4]-par('usr')[3])/2.5, 
         labels=Chart.Data[,2], cex=0.75, srt=-15)
  })
  
  # STOCK MARKET####
  
  
  output$Data.Realtime <- renderTable({
    data <- getQuote(ListOfCodes)
    data <- data.frame((names(ListOfCodes)), data[,2:3])
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
    chartSeries(Stock.Selected, name=paste(input$StockSelectorChoice, " (Closing)"),theme=chartTheme('white'))
    addMACD()
  })
  
  output$LatestValue <- renderTable({
    Stock.Selected <- input$StockSelectorChoice
    Stock.Selected <- get(gsub("\\^", "", ListOfCodes[grep(Stock.Selected, names(ListOfCodes))]))
    Stock.Value    <- tail(Stock.Selected, 10)
    Stock.Info     <- data.frame(Period = as.character(index(Stock.Value)), Closing = Stock.Value[,4],
                                 Volume = Stock.Value[,5])
    names(Stock.Info) <- c("Period", "Closing", "Volume")
    rownames(Stock.Info) <- NULL
    return(Stock.Info)
  })



output$FOMC.Current <- renderText({ return(FOMC.Text ) })

})
