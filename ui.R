
library(shiny)


shinyUI(navbarPage("Macroeconomic Analysis",
                   tabPanel("Dashboard",
                            HTML("<h3>GDP growth in major economies</h3> All data shown as q/q growth rates (annualized). Charts also include naive time-series forecasts for the next 4 quarters"),
                            plotOutput("Overview.Charts"),
                            htmlOutput("DateDataUpdate")
                   ),
                   tabPanel("Country Overview",
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
                                uiOutput("UI.Variable.Control"),
                                checkboxInput("RegressionXREGControlChoice", "Add additional regressors", FALSE)
                              ),
                              mainPanel(
                                HTML("<h3>Forecasting using ARIMA/XARIMA models</h3>"),
                                plotOutput("Macro.Chart"),
                                HTML("Regression specification"),
                                verbatimTextOutput("Macro.Regression")
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
              <li>The entire tool is automated; the data is automatically refreshed (and stored in an RData file for faster loading);</li>
              All macroeconomic data is from FRED and quandl. Stock market data is from Yahoo. As the focus lies on automation, the econometric models are very simplistic, and the forecasts should not be taken too seriously.<p>
              Future enhancements will include an automated email notification if new data has been released. <p>
              Please contact me at pmaier1971 (at) gmail.com if you have questions or suggestions.")
                   )                   
))
