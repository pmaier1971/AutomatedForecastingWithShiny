Automated Forecasting With R/shiny
-----------------------------------
v. 0.5

This project is still in an early stage. It was developed as tool to pull macroeconomic data, visualize it, and illustrate some simple forecasting techniques. All is done with R/RStudio/shiny.

Some of the features currently implemented are:

- Different types of macroeconomic data for various countries can be visualized to get a snapshot of current developments; the code will automatically detect what data is available or not and adjust the output accordingly;
- Some simple and less simple (automated) forecasting techniques illustrate possible ranges for future macroeconomic outcomes;
- A "Detailed Analysis" tab with data on the US labor market (geared towards the Federal Reserve's monetary policy);
- The entire tool is automated, with data being automatically refreshed and the commentary being auto-generated.

Future enhancements will explore email notification if new data has been released; a much richer data environment with better forecasting models, and better visual presentation of the data.

As the focus lies on automation, the econometric models are very simplistic, and the forecasts should not be taken too seriously. Also, since I'm still trying to find my way around R, the code is clearly anything but perfect.

Please contact me at pmaier1971 (at) gmail.com or www.linkedin.com/pub/philipp-maier/5/966/653 if you have questions or suggestions. The application is hosted at https://pmaier1971.shinyapps.io/AutomatedForecastingWithShiny/