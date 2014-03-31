
library(shiny)


shinyUI(navbarPage("Automated GDP Forecasting With Shiny",
                   tabPanel("Macroeconomic Forecasts",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("Macro.Control")
                              ),
                              mainPanel(
                                HTML("GDP Forecast Using A Simple ARIMA Model"),
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
                   )
                   
))
