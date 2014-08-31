
library(shiny)

shinyUI(fluidPage(theme = "bootstrap3.css",
  navbarPage("Economic Dashboard",
                   tabPanel("Overview",
                            HTML("<h3>GDP growth in major economies</h3> All data shown as q/q growth rates (annualized). Charts also include naive time-series forecasts for the next 4 quarters."),
                            plotOutput("Overview.Charts"),
                            htmlOutput("UI.Date")
                   ),
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
                   tabPanel("Macroeconomic Forecasts",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("UI.Macro.Control"),
                                uiOutput("UI.Variable.Control")
                                #checkboxInput("RegressionXREGControlChoice", "Add additional regressors", FALSE)
                              ),
                              mainPanel(
                                HTML("<h3>Forecasting using ARIMA/XARIMA models</h3>"),
                                htmlOutput("Macro.Regression.Commentary"),
                                plotOutput("Macro.Chart"),
                                HTML("Regression specification"),
                                checkboxInput("UIRegressionSpecControl", "Show Regression Output", value=FALSE),
                                conditionalPanel(condition = "input.UIRegressionSpecControl",
                                                 verbatimTextOutput("Macro.Regression")
                                )
                              )
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
              The data is taken from <a href='http://research.stlouisfed.org/fred2/'> St. Louis Federal Reserve Bank's FRED database</a> and Yahoo; the CSS theme is SuperHero from <a href='http://bootswatch.com'>Bootswatch</a>. <p>
              Future enhancements will include better models, more data, better data visualization, and an automated email notification if new data has been released. <p>
              The code can be found in <a href='https://github.com/pmaier1971/AutomatedForecastingWithShiny'>my GitHub respository</a>. Please contact me at pmaier1971 (at) gmail.com if you have questions or suggestions.")
                   )                   
)))
