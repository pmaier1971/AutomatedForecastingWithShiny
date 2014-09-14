file.About <- "About.md"

library(shiny)
library(shinyIncubator)

shinyUI(fluidPage(theme = "bootstrap3.css",
                  tags$head(includeScript("google-analytics.js")),
                  progressInit(),
                  navbarPage("Economic Dashboard",
                             tabPanel("Overview",
                                      sidebarLayout(
                                        sidebarPanel(    
                                          htmlOutput("MarketUpdate.Commentary")
                                          ),
                                      mainPanel(
                                          HTML("<h3>GDP Growth: History And Projections For Major Economies</h3> 
                                               Charts show naive time-series forecasts for the next 4 quarters. All data shown as q/q growth rates (annualized).<p>Note: This page might take a few seconds to load, if the data is refreshed."),
                                      plotOutput("Overview.Charts"),
                                      htmlOutput("UI.Date")
                             ))),
                             tabPanel("Country View",
                                      sidebarLayout(
                                        sidebarPanel(
                                          uiOutput("UI.Country.Analysis"),
                                          htmlOutput("Commentary.Country.Analysis")
                                        ),
                                        mainPanel(
                                          HTML("<h3>Latest developments</h3>"),
                                          plotOutput("Charts.Country.Analysis")
                                        )
                                      )
                             ),
                             navbarMenu("Detailed Analysis",           
                                        tabPanel("US Labor Market",
                                                 h3("US Labor Market Indicators: Current Values, Relative to the Worst Point This Cycle"),
                                                 h5("Work in progress. The large charts show the longer-term evolution since 2000; the small charts show progress among different labor market indicators during this recovery/expansion. The cycle is defined as starting in 2008;
                                                    current values are shown relative to the best and worst readings of this indicator during this cycle."),
                                                 plotOutput("US.LaborMarket.Dashboard", height="1000px")
                                        )
                             ),
                             navbarMenu("Forecasting",
                                        tabPanel("Time Series Forecasting",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     uiOutput("UI.Macro.Control"),
                                                     uiOutput("UI.Variable.Control")
                                                     #checkboxInput("RegressionXREGControlChoice", "Add additional regressors", FALSE)
                                                   ),
                                                   mainPanel(
                                                     HTML("<h3>Time Series Forecasting: Comparison of Different Approaches</h3>"),
                                                     htmlOutput("Macro.Regression.Commentary"),
                                                     plotOutput("Macro.Chart"),
                                                     HTML("Regression specification (Arima model)"),
                                                     checkboxInput("UIRegressionSpecControl", "Show Regression Output", value=FALSE),
                                                     conditionalPanel(condition = "input.UIRegressionSpecControl",
                                                                      verbatimTextOutput("Macro.Regression")
                                                     )
                                                   )
                                                 )
                                        ),
                                        tabPanel("Forecast Pooling",
                                                 h3("Forecast Pooling"),
                                                 HTML("Here we employ Forecast Pooling, also known as Ensemble Forecasting. From <a href='http://en.wikipedia.org/wiki/Ensemble_forecasting'>Wikipedia</a>:
                                                       <p><blockquote><i>'Ensemble forecasting is a numerical prediction method that is used to attempt to generate a representative 
                                                       sample of the possible future states of a dynamical system.... When many different forecast models are used to try to generate 
                                                       a forecast, the approach is termed multi-model ensemble forecasting. This method of forecasting has been shown to improve forecasts when compared to a 
                                                       single model-based approach'</i></blockquote><p>
                                                       Our approach uses the following features: <ul><li>Two forecasts are available, for the change in US Nonfarm Payrolls (m/m) and for US real GDP (q/q). 
                                                      <li>The forecast use about 20 activity indicators (including ISM Manufacturing, JOLTS data, and regional Fed activity indicators),
                                                      of which we randomly use 4 as predictors. <li>This process is repeated 250 times to generate the mean forecast, as well as various percentiles of the distribution. 
                                                                            </ul><p>The code for this model is on GitHub (see 'About' tab). Note: It may take a few minutes until the forecast is shown.<br>"),
                                                 br(),
                                                 selectInput("ForecastPooling.Selection",
                                                             "Select A Variable To Forecast",
                                                             c("Nonfarm Payrolls", "GDP") ),
                                                 htmlOutput("EnsembleForecast.Commentary"),
                                                 plotOutput("EnsembleForecast.Plot")
                                        )
                             ),
                             navbarMenu("Stock Market",
                                        tabPanel("Real-Time Data",
                                                 tableOutput("Data.Realtime")
                                        ),
                                        tabPanel("Historical Data",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     uiOutput("StockSelector")
                                                   ),
                                                   mainPanel(
                                                     plotOutput("TestPlot"),
                                                     
                                                     tableOutput("LatestValue")
                                         )
                                       )
                              )
                   ),
                   tabPanel("About",
                            HTML("<h3>Description</h3>This page attempts to illustrate how to set up pages with macroeconomic data and forecasts with <a href='rstudio.com'>R/RStudio/shiny</a>. Some of the features are:
              <ul><li>The latest macroeconomic data for various countries can easily be visualized to get a snapshot of current developments.</li>
              <li>Simple forecasts illustrate possible ranges for future macroeconomic outcomes;</li>
              <li>The entire tool is automated; the data is automatically refreshed and the commentary is auto-generated.</li></ul>
              As the focus lies on automation, the econometric models are very simplistic, and the forecasts should not be taken too seriously.
              The data is taken from <a href='http://research.stlouisfed.org/fred2/'> St. Louis Federal Reserve Bank's FRED database</a> and Yahoo; the CSS theme is SuperHero from <a href='http://bootswatch.com'>Bootswatch</a>. The 
              US Labor Market Analysis is inspired by <a href='http://graphics.thomsonreuters.com/14/yellen/index.html'>Thomson Reuters</a>.<p>
              Future enhancements will include better models, more data, better data visualization, and an automated email notification if new data has been released. <p>
              The code can be found in <a href='https://github.com/pmaier1971/AutomatedForecastingWithShiny' onclick='ga('send', 'event', 'click', 'link', 'GithubLink', 1)'>my GitHub respository</a>. 
                                 Please contact me at pmaier1971 (at) gmail.com or <a href='https://www.linkedin.com/pub/philipp-maier/5/966/653' onclick='ga('send', 'event', 'click', 'link', 'LinkedInLink', 1)'>through my LinkedIn profile</a> if you have questions or suggestions.")
                   )                   
)))
