### Description

This project is still in a relatively early stage. It is a bit of a hobby of mine to think about different ways to visualize and analyze macroeconomic data, so I developed this page to try out different techniques and to set up an automated web page in <a href='rstudio.com'>RStudio and shiny</a>. The set of economic indicators included are ones I personally find interesting to follow, but note that the list is still far from complete. I have also added a couple of forecasting techniques, but the main focus of this application is automation, so the econometric models are rather simplistic, and the forecasts should not be taken too seriously. 

Some of the features currently implemented are:

+ The latest macroeconomic data for various countries can be visualized to get a snapshot of current developments.

+ Simple forecasts illustrate possible ranges for future macroeconomic outcomes;

+ Dropbox integration allows tracking updates to forecasts; Pushbullet notifications inform when errors occur;

+ When information is outdated, the web page automatically pulls the latest data from the web; a separate process is run on my local machine to pull data and post charts automatically on Twitter.

+ The tool is fully automated; the data is automatically refreshed and the commentary is auto-generated.


The data is taken from <a href='http://research.stlouisfed.org/fred2/'> St. Louis Federal Reserve Bank's FRED database</a> and Yahoo. Previous versions had the CSS theme is SuperHero from <a href='http://bootswatch.com'>Bootswatch</a>. The US Labor Market Analysis is inspired by <a href='http://graphics.thomsonreuters.com/14/yellen/index.html'>Thomson Reuters</a>.

Future enhancements will include better models, more data, better data visualization, and an automated email notification if new data has been released. Twitter integration is run from my local machine, as I found this to be more stable.

The code can be found on <a href='https://github.com/pmaier1971/AutomatedForecastingWithShiny'>GitHub</a>. For comments, suggestions, or other feedback, please do not hesitate to contact me at pmaier1971+shiny@gmail.com, at <a href='https://twitter.com/PMaier1971'>@PMaier1971</a>, or <a href='https://www.linkedin.com/pub/philipp-maier/5/966/653'>through my LinkedIn profile</a>.

### Disclaimer

This page is not affiliated with any of the institutions I currently work for, or I have worked for in the past. All views expressed are my own. No forecast should be used as input for business or investment decisions. The stock market information displayed is to test the robustness of retrieving stock market data, not because I have any interest in or can recommend these particular stocks.

