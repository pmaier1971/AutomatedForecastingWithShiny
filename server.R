rm(list=ls())
library(shiny)
library(quantmod)
library(zoo)
library(forecast)
library(ggplot2)
library(lubridate)
library(Quandl)
library(shinyIncubator)


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
  
  # Test whether we are online
  ListOfCodes <- c("SP 500"="^GSPC", 
                   "UST 10Y"="^TNX", 
                   "Apple"="AAPL", 
                   "Bank of America"="BAC", 
                   "JP Morgan"="JPM", 
                   "T-Mobile"="TMUS")
  
  ListofQuandlCodes <- c()
  
  test        <- try(getSymbols("GDPC1",src='FRED'))
  
  load(file="InputData.RData")
  if (!(inherits(test, "try-error"))) {
    # Is the data up-to-date?
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
                   "US.JOLTS.QuitsRate" = "JTSQUR",
                   "US.JOLTS.HireRate" = "JTSHIR",
                   "US.JOLTS.JobOpeningsRate" ="JTSJOR",
                   "US.Unemployment.WageGrowth" = "CES0500000003",
                   "US.CPI.Headline"="CPIAUCSL", "US.CPI.Core"="CPILFESL",
                   "US.SOV.10Y"="DGS10", "US.FSI.Cleveland"="CFSI"
                   )      
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
                   "CA.CPI.Core"="CANCPICORMINMEI", "FX.CA.USD"="EXCAUS", "FX.CA.Effective"="RBCABIS")
      
      List.Countries <- c("Data.US", "Data.EU", "Data.UK", "Data.CA")
      
      # Assign names
      # Transformation needed
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
      
      for (i in 1:length(ListOfCodes)){
        getSymbols(ListOfCodes[i])
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
      
      
      cat("\n.... saving a new RData file")
      Last.Update <- Sys.Date()
      save.image(file = "InputData.RData")
    } else cat("\nData file is up to date")
  } 
  
  
  # Panel Overview
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
      segments(index(Chart.Data), Chart.Data[,6], index(Chart.Data), Chart.Data[,7], col="deepskyblue", lwd=10)
      segments(index(Chart.Data), Chart.Data[,4], index(Chart.Data), Chart.Data[,5], col="tomato", lwd=15)
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
    if (Change > 0)      Commentary <- paste(Commentary, ". This is an acceleration of ", Change, "%,
                                             over the", 
                                             round(Data[nrow(Data)-1],2), "% last time.</li>", sep="")
    else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "%, relative to ", 
                                            round(Data[nrow(Data)-1],2),
                                            "% in the last quarter.</li>", sep="")
    else                 Commentary <-paste(Commentary, ", (unchanged).</li>", sep="")
    
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
      Data       <- get(paste(Country,".SOV.10Y", sep=""))
      Change     <- round(coredata(Data[nrow(Data)])-coredata(Data[nrow(Data)-1]),1)
      Commentary <- paste(Commentary, "<li>The 10Y sovereign bond yield in ", format(index(Data[nrow(Data)]), "%B %Y"), 
                          " was ", round(Data[nrow(Data)],1),"%", sep ="")
      if (Change > 0)      Commentary <- paste(Commentary, ", up ", Change, "% </li>", sep="")
      else if (Change < 0) Commentary <-paste(Commentary, ", down ", Change, "%</li>", sep="")
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
    Data.Dashboard[,7]           <- Data.Dashboard[,7]-lag(Data.Dashboard[,7], 1)
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
    Date.Frequency   <- index(Regression.Data())[length(Regression.Data())] - index(Regression.Data())[length(Regression.Data())-1]
    Regression       <- list()
    Regression[[1]]  <- auto.arima(Regression.Data())
    #     Regression[[2]]  <- ets(Regression.Data())
    #     Regression[[3]]  <- HoltWinters(ts(Regression.Data(), frequency=Date.Frequency))
    return(Regression)
  })
  
  output$Macro.Regression <- renderPrint({
    return(summary(Regression.Output()[[1]]))
  })
  
  output$Macro.Regression.Commentary <- renderText({
    
    Forecast          <- forecast(Regression.Output()[[1]], h=4)
    Date.Start        <- index(Regression.Data())[length(Regression.Data())]
    Date.Frequency    <- Date.Start - index(Regression.Data())[length(Regression.Data())-1]
    if (Date.Frequency > 85) {
      Forecast.index  <- seq(Date.Start + months(3), Date.Start + years(1), by="3 months")
      Date.Start      <- format(as.yearqtr(Date.Start), "Q%q %Y")
      Forecast.Period <- format(as.yearqtr(Forecast.index[1]), "Q%q %Y")
    } else if (Date.Frequency > 27) {
      Forecast.index  <- seq(Date.Start + months(3), Date.Start + years(1), by="1 month")
      Date.Start      <- format(Date.Start, "%B %Y")
      Forecast.Period <- format(Forecast.index[1], "%B %Y")
    }else if (Date.Frequency > 6) {
      Forecast.index  <- seq(Date.Start + months(3), Date.Start + years(1), by="1 week")
      Date.Start      <- format(Date.Start, "%d %B %Y")
      Forecast.Period <- format(Forecast.index[1], "%d %B %Y")
    }
    
    Commentary <- ("<ul><li>")
    Commentary <- paste0(Commentary, "The latest observation for ", Date.Start)
    Commentary <- paste0(Commentary, " was ", round(tail(Regression.Data(),1),2), ".")
    Commentary <- paste0(Commentary, "<li>A simple ARIMA/XARIMA model with optimal lag selection - as shown below - would forecasts ")
    Commentary <- paste0(Commentary, round(Forecast$mean[1],2)," for ", Forecast.Period, ".</li></ul>")
    return(Commentary)
  })
  
  output$Macro.Chart <- renderPlot({
    #par(mfrow=c(2,2))
    
    for (idx.model in 1:length(Regression.Output())){
      cat("\n       - Model ", idx.model)
      #Forecast          <- forecast(Regression.Output()[[idx.model]], h=4)
      
      Forecast = try(forecast(Regression.Output()[[idx.model]], h=4), silent=TRUE)
      if (class(Forecast)[1] != 'try-error') {
        Date.Start        <- index(Regression.Data())[length(Regression.Data())]
        Date.Frequency    <- Date.Start - index(Regression.Data())[length(Regression.Data())-1]
        if (Date.Frequency > 85) {
          Forecast.index  <- seq(Date.Start + months(3), Date.Start + years(1), by="3 months")
        } else if (Date.Frequency > 27) {
          Forecast.index  <- seq(Date.Start + months(1), Date.Start + months(4), by="1 month")
        } else if (Date.Frequency <= 7) {
          Forecast.index  <- seq(Date.Start + 7, Date.Start + weeks(4), by="1 week")
        }
        Forecast.df <- data.frame(Forecast)
        Chart.Data  <- data.frame(Period = index(Regression.Data()),
                                  Regression.Data(),
                                  Regression.Data(),
                                  Regression.Data(),
                                  Regression.Data(),
                                  Regression.Data())
        names(Chart.Data) <- c("Period", "Mean", "High", "Low", "Upper", "Lower")
        Chart.Data  <- rbind(Chart.Data, data.frame(Period = Forecast.index,
                                                    Mean = Forecast.df[,1],
                                                    High = Forecast.df[,2],
                                                    Low = Forecast.df[,3],
                                                    Upper = Forecast.df[,4],
                                                    Lower = Forecast.df[,5]))
        Plot.Data <- zoo(Chart.Data[,-1], Chart.Data[,1])
        plot(Plot.Data$Mean, lwd=1, type="o", ylab="", xlab="", pch=19,
             main=paste0(input$Variable.Control.Choice, " (black)\nConfidence bands: 85% in blue, 95% in red"))
        lines(Plot.Data$High, col="blue", lwd=2)
        lines(Plot.Data$Low, col="blue", lwd=2)
        lines(Plot.Data$Upper, col="red", lwd=2)
        lines(Plot.Data$Lower, col="red", lwd=2)
        lines(Plot.Data$Mean, col="black", lwd=2, pch=19)
      } else
        plot(predict(Regression.Output()[[idx.model]], h=4))
    }
    #par(mfrow=c(1,1))
  })
  
  output$UI.Date <- renderText({
    Commentary.Date <- paste0("Last Data Update: ", format(Last.Update, "%d %B %Y"))
    return(Commentary.Date)
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
