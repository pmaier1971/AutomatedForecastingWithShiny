### Description

This project is still in a relatively early stage. It was developed in my spare time as tool to pull macroeconomic data, visualize it, and illustrate some simple forecasting techniques. All is done with <a href='rstudio.com'>RStudio and shiny</a>. The set of economic indicators included are ones I personally find interesting to follow, but note that the list is still far from complete.

Some of the features currently implemented are:

+ The latest macroeconomic data for various countries can be visualized to get a snapshot of current developments.

+ Simple forecasts illustrate possible ranges for future macroeconomic outcomes;

+ Dropbox integration allows tracking updates to forecasts;

+ The tool is fully automated; the data is automatically refreshed and the commentary is auto-generated.


As this application focuses primarily on automation, the econometric models are rather simplistic, and the forecasts should not be taken too seriously. The data is taken from <a href='http://research.stlouisfed.org/fred2/'> St. Louis Federal Reserve Bank's FRED database</a> and Yahoo; the CSS theme is SuperHero from <a href='http://bootswatch.com'>Bootswatch</a>. The US Labor Market Analysis is inspired by <a href='http://graphics.thomsonreuters.com/14/yellen/index.html'>Thomson Reuters</a>.

Future enhancements will include better models, more data, better data visualization, and an automated email notification if new data has been released. 

The code can be found on <a href='https://github.com/pmaier1971/AutomatedForecastingWithShiny'>GitHub</a>. For comments, suggestions, or other feedback, please do not hesitate to contact me at pmaier1971+shiny@gmail.com, at <a href='https://twitter.com/PMaier1971'>@PMaier1971</a>, or <a href='https://www.linkedin.com/pub/philipp-maier/5/966/653'>through my LinkedIn profile</a>.
