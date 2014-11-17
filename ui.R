library(shiny)
library(shinyIncubator)
library(rmarkdown)
library(rDrop)

TwitterString <- "<a width='1000' class='twitter-timeline' href='https://twitter.com/EconomicsShiny' data-widget-id='526196846738165760'>Tweets by @EconomicsShiny</a> <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document,'script','twitter-wjs');</script>"

shinyUI(fluidPage(#theme = "bootstrap3.css",
                  tags$head(includeScript("google-analytics.js")),
                  progressInit(),
                  navbarPage("Economic Dashboard",
                             tabPanel("Overview",
                                      sidebarLayout(
                                        sidebarPanel(    
                                          HTML("<a href='https://twitter.com/EconomicsShiny' class='twitter-follow-button' data-show-count='false'>Follow @EconomicsShiny</a>
<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>"),
                                          htmlOutput("MarketUpdate.Commentary")
                                          ),
                                        mainPanel(
                                          HTML("<h3>GDP Growth: History And Projections For Major Economies</h3> 
                                               Charts show naive time-series forecasts for the next 4 quarters. All data shown as q/q growth rates (annualized).<p>Note: This page will take a few seconds to update the data."),
                                          plotOutput("Overview.Charts"),
                                          htmlOutput("UI.Date")
                                          ))),
                             tabPanel("What's New?",
                                      HTML(TwitterString)
                             ),
                             tabPanel("Country View",
                                      sidebarLayout(
                                        sidebarPanel(
                                          uiOutput("UI.Country.Analysis"),
                                          htmlOutput("Commentary.Country.Analysis")
                                        ),
                                        mainPanel(
                                          HTML("<h3>Longer-Term View</h3>"),
                                          plotOutput("Charts.Country.Analysis")
                                        )
                                      )
                             ),
                             navbarMenu("Detailed Analysis", 
                                        tabPanel("US Activity Surveys",
                                                 h3("High-Frequency Surveys of US Economic Activity"),
                                                 HTML("High-frequency indicators have some interesting properties. <ul><li>They provide an early snapshot of economic activity in different sectors of the US economy. 
<li>Unlike GDP, which is only published quarterly and can be heavily revised,
these indicators are released monthly (and data revisions are small). <li>They are survey-based, and may thus exaggerate positive or negative sentiment.</ul><p>We have re-scaled the San Fransisco Fed Tech Pulse by multiplying it by 10. 
                                                      You can zoom into the charts using the scroll wheel of the mouse or using the date range selector underneath the chart."),
                                                 htmlOutput("US.ActivityMeasures.Dashboard")
                                                 #plotOutput("US.ActivityMeasures.Change")
                                        ),
                                        tabPanel("US Labor Market",
                                                 h3("US Labor Market Indicators: Current Values, Relative to the Worst Point This Cycle"),
                                                 h5("The large charts show the longer-term evolution since 2000; the small charts show progress among different labor market indicators during this recovery/expansion. The cycle is defined as starting in 2008;
                                                    current values are shown relative to the best and worst readings of this indicator during this cycle."),
                                                 plotOutput("US.LaborMarket.Dashboard", height="1000px")
                                        ),
                                        tabPanel("US Housing Market",
                                                 htmlOutput("Housing.Dashboard")
                                                 #                                                  h3("US Housing Market Indicators"),
                                                 #                                                  checkboxInput("UIHousingDashboardHistoryControl", "Show More History", value=FALSE),
                                                 #                                                  plotOutput("US.HousingMarket.Dashboard", height="1000px")
                                        ),
                                        tabPanel("US Vehicle Sales",
                                                 htmlOutput("VehiclesSales.Dashboard")
                                                 #                                                  h3("US Vehicles Sales and Auto Market Indicators"),
                                                 #                                                  checkboxInput("UIAutoDashboardHistoryControl", "Show More History", value=FALSE),
                                                 #                                                  plotOutput("US.AutoMarket.Dashboard", height="1000px")
                                        ),
                                        tabPanel("US Treasury Interest Rates",
                                                 h3("Evolution of US Treasury Constant Maturity Rates  (in %)"),
                                                 htmlOutput("US.InterestRates.Commentary"),
                                                 plotOutput("US.InterestRates.Dashboard", height="1000px")
                                        ),
                                        tabPanel("International Inflation Comparison",
                                                 htmlOutput("International.InflationAnalysis.Dashboard")
                                                 #                                         ),
                                                 #                                         tabPanel("International Inflation Comparison",
                                                 #                                                  sidebarLayout(
                                                 #                                                    sidebarPanel(
                                                 #                                                      selectInput("InflationComparisonChoice", "Select A Period",
                                                 #                                                                  c("Over The Past 5 Years" = 5, 
                                                 #                                                                    "Over The Past 2 Years" = 2, 
                                                 #                                                                    "Over The Past Year" = 1))
                                                 #                                                    ),
                                                 #                                                    mainPanel(
                                                 #                                                      h3("International Inflation Comparison"),
                                                 #                                                      HTML("Which country had has the highest headline inflation rate?<p>"),
                                                 #                                                      plotOutput("International.Inflation.Dashboard")
                                                 #                                                    ))
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
                                                     checkboxInput("UIRegressionSpecControl", "Show Regression Output", value=FALSE),
                                                     conditionalPanel(condition = "input.UIRegressionSpecControl",
                                                                      HTML("Regression Specification (Arima Model, Lag Selection Based On AIC/BIC)"),
                                                                      verbatimTextOutput("Macro.Regression")
                                                     )
                                                   )
                                                 )
                                        ),
                                        tabPanel("Ensemble Forecasting",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     selectInput("ForecastPooling.Selection",
                                                                 "Select A Variable To Forecast",
                                                                 c("GDP", "Nonfarm Payrolls") ),
                                                     htmlOutput("EnsembleForecast.Commentary")
                                                   ),
                                                   mainPanel(
                                                     HTML("<h3>Ensemble Forecasting For GDP and Nonfarm Payrolls</h3>"),
                                                     checkboxInput("EnsembleForecastingDescChoice", "Show Model Description", FALSE),
                                                     HTML("<p>Below the in-sample fit; the box on the left provides details on the forecast for the next data release. The checkbox underneath the chart toggles the tracking information, i.e. how the forecast changed in response to new data."),
                                                     conditionalPanel(condition = "input.EnsembleForecastingDescChoice",
                                                                      includeMarkdown("Description.EnsembleForecasting.md")),
                                                     plotOutput("EnsembleForecast.Plot"),
                                                     
                                                     checkboxInput("EnsembleForecastingUpdateChoice", "Hide Tracking", FALSE),
                                                     conditionalPanel(condition = "!input.EnsembleForecastingUpdateChoice",
                                                                      HTML("<p>The chart below shows how the forecast has changed in light of new data releases. The description on each bar
indicates the data; the table shows the additional details of the data release. Both chart and table are updated automatically as new information becomes available.<p>"),
                                                                      plotOutput("EnsembleForecast.Tracking"),
                                                                      verbatimTextOutput("Forecast.Tracking"))
                                                   )))
                             ),
                             tabPanel("Stock Market",
                                      sidebarLayout(
                                        sidebarPanel(
                                          uiOutput("StockSelector"),
                                          h5("Near-Real Time Quotes: Selected Stocks"),
                                          tableOutput("Data.Realtime")
                                        ),
                                        mainPanel(
                                          plotOutput("TestPlot", height = "750px"),
                                          h5("Historical Performance (Last 10 Trading Days):"),
                                          tableOutput("LatestValue")
                                        )
                                      )
                             ),
                             tabPanel("About",
                                      includeMarkdown("About.md")
                             )                   
                             )))
