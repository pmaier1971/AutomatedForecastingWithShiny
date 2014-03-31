
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

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
#                               
#                    
#   # Sidebar with a slider input for number of observations
#   sidebarPanel(
#     conditionalPanel(condition = "input.tabs == 'Macroeconomic Forecasts'",
#                      
#     ),
#     conditionalPanel(condition = "input.tabs == 'Stock Market Data'",
#                      uiOutput("StockSelector")
#     )
#   ),
#   
#   # Show a plot of the generated distribution
# mainPanel(
#   tabsetPanel(id="tabs",
#               tabPanel("Macroeconomic Forecasts",
#                        HTML("GDP Forecast Using A Simple ARIMA Model"),
#                        plotOutput("Macro.Chart"),
#                        HTML("Regression specification"),
#                        verbatimTextOutput("Macro.Regression")
#               ),
#               tabPanel("Stock Market Data",
#                        plotOutput("TestPlot"),
#                        tableOutput("Data.Realtime"),
#                        tableOutput("LatestValue")
#                        #textOutput("LatestValue")
#                        #verbatimTextOutput("LatestValue")
#               )
#   ))
# ))
