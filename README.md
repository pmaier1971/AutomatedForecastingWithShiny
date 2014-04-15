This page was developed as I was attempting to set up an automated tool to pull macroeconomic data, have it refresh periodically, visualize it, and do some simple forecasting. All is done with R/RStudio/shiny.

Some of the features are:

- The latest macroeconomic data for various countries can easily be visualized to get a snapshot of current developments.
- Simple forecasts illustrate possible ranges for future macroeconomic outcomes;
- The entire tool is automated; the data is automatically refreshed every day (and stored in an RData file for faster loading) and the commentary is auto-generated.

Future enhancements will include an automated email notification if new data has been released.

As the focus lies on automation, the econometric models are very simplistic, and the forecasts should not be taken too seriously. Also, since I'm still trying to find my way around R, the code is anything but perfect.

Please contact me at pmaier1971 (at) gmail.com if you have questions or suggestions.