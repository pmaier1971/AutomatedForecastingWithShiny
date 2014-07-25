Automated Forecasting With R/shiny
-----------------------------------
v. 0.21

This project is still in a very early stage. It was developed as I was attempting to set up an automated tool to pull macroeconomic data, have it refresh periodically, visualize it, and do some simple forecasting. All is done with R/RStudio/shiny.

Right now I am still in the stage of exploring the different possibilities. Some of the features currently implemented are:

- The latest macroeconomic data for various countries can easily be visualized to get a snapshot of current developments.
- Simple forecasts illustrate possible ranges for future macroeconomic outcomes;
- The entire tool is automated; the data is automatically refreshed and the commentary is auto-generated.

Future enhancements will include an automated email notification if new data has been released; a much richer data environment with better forecasting models, and better visual presentation of the data.

As the focus lies on automation, the econometric models are very simplistic, and the forecasts should not be taken too seriously. Also, since I'm still trying to find my way around R, the code is clearly anything but perfect.

Please contact me at pmaier1971 (at) gmail.com if you have questions or suggestions. The application is also hosted at https://pmaier1971.shinyapps.io/AutomatedForecastingWithShiny/