## Methodology

Often, more than one model can be used to produce a forecast for the same variable. Many methods have been proposed to select the single best forecasting model among different models (e.g. based on past performance). An alternative technique is to combine or 'pool' different models to obtain a pooled forecast. Past studies have typically concluded that forecast pooling can be a very effective way to improve forecasting accuracy (see also here: http://en.wikipedia.org/wiki/Consensus_forecast).

A somewhat related technique is to use one common (large) data set, from which a series of predictors are randomly chosen (with replacement). Then, using these predictors, a forecast is made. By repeating this process often enough, a distribution of forecasts can be obtained. This technique is called 'Ensemble Forecasting' or 'Ensemble Learning'. Wikipedia has more here: http://en.wikipedia.org/wiki/Ensemble_learning

In implementing this approach, we are somewhat limited by the fact that the process is calculated every time this specific web page is opened, so we need to trade off speed vs. accuracy here. For this implementation of the algorithm, we therefore decided to use the following features:

+ Two forecasts are available, for the change in US Nonfarm Payrolls (m/m) and for US real GDP (q/q).

+ The data set for forecast comprises about 20 activity indicators (including ISM Manufacturing, JOLTS data, and regional Fed activity indicators), plus their lagged values.

+ For each round of forecasts, we randomly select 4 variables from the data set as predictors, and estimate an AR(1) model. Note: we also experimented with R's auto.arima function (of the forecast package) to optimally select lag terms, but we found that (i) this is much slower, and (ii) the forecast accuracy does not deteriorate substantially by simply imposing an AR(1) model.

This process is repeated 250 times to generate the mean forecast, as well as various percentiles of the distribution. (Again, to fully reap the benefits of this technique, ideally the data set should be larger, and one would run many more repetitions.)
