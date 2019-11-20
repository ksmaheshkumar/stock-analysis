# Load libraries
library(forecast)
#library(tseries)
#library(timeSeries)
#library(fGarch)
#library(ggplot2)
library(aTSA)
library(RSQLite)
library(xts)
library(PerformanceAnalytics)
library(rugarch)

# Load dataframe from database
db_to_df <- function(dbname, tbname){
  sqlite_driver <- dbDriver("SQLite")
  db <- dbConnect(sqlite_driver, dbname)
  table <- dbReadTable(db, "eow")
  return(table)
}

table <- db_to_df("stocks-updated.db", "eow")

get_company <- function (sto_name){
  return(table[table$symbol == sto_name,])
}

to_ts <- function(tdf, colname){
  dates <- seq(as.Date("2011-01-14"), length=453, by="weeks")
  tdf <- xts(tdf[colname], order.by=dates)
  names(tdf) <- colname
  return(tdf)
}

tt_split <- function(tdf, d1, d2){
  train <- tdf[d1]
  test <- tdf[d2]
  return(list("train" = train, "test" = test))
}

# any company
df <- get_company("AAPL")
# adj close time series
dfac <- to_ts(df, colname="adjclose")
# returns time series
dfr <- Return.calculate(dfac, method = "discrete")
names(dfr) <- "ret"
# log returns time series
dflr <- Return.calculate(dfac, method = "log")
names(dflr) <- "logret"

###############################################

# Plots
chart.TimeSeries(dfac)
chart.TimeSeries(dfr)
chart.TimeSeries(dflr)

############# EDA ############################

# Decompose Close
components <- decompose(ts(dfac, frequency = 365.25/7))
plot(components)
# Stationary tests (Augmented Dickey–Fuller test)
adf.test(dfac)
# Acf and pacf (Auto correlation and partial auto correalation)
acf(dfac)
pacf(dfac)

# Decompose log returns
components <- decompose(ts(dflr, frequency = 365.25/7))
plot(components)
# Stationary tests (Augmented Dickey–Fuller test)
adf.test(dflr[complete.cases(dflr),])
adf.test(dfr)
# Acf and pacf (Auto correlation and partial auto correalation)
acf(dflr[complete.cases(dflr), ])
pacf(dflr[complete.cases(dflr), ])

#################  training on price ##############

# Split into train and test dataset
split <- tt_split(dfac, "/2018-12", "2019")
dtrain <- split$train
dtest <- split$test

# adf test
print(adf.test(dtrain))
# fit model
mfit <- forecast::auto.arima(dtrain, lambda = "auto")

# summary
summary(mfit)
plot(resid(mfit))


### Lack of fit test
Box.test(resid(mfit), lag=10, type = "Ljung-Box")
tsdiag(mfit)

# prediction length required
predlen <- length(dtest)
acforecast <- forecast(mfit, h=predlen)

plot(acforecast)
head(acforecast$mean)
head(acforecast$upper)
head(acforecast$lower)

#################  training on logret ##############

# Split into train and test dataset
split <- tt_split(dflr, "/2018-12", "2019")
dtrain <- split$train
dtest <- split$test

# adf test
print(adf.test(dtrain))
# fit model
mfit <- forecast::auto.arima(dtrain, lambda = "auto")

# summary
summary(mfit)
plot(resid(mfit))


### Lack of fit test
Box.test(resid(mfit), lag=10, type = "Ljung-Box")
tsdiag(mfit)

# prediction length required
predlen <- length(dtest)
lrforecast <- forecast(mfit, h=predlen)

plot(lrforecast)
head(lrforecast$mean)
head(lrforecast$upper)
head(lrforecast$lower)

####################Garch###########################

#Dataset forecast upper first 5 values
fitarfima = autoarfima(data = dtrain,
                       ar.max = 2, ma.max = 2,
                       criterion = "AIC", method = "full")
#202  result   
#define the model
garch11ac=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(2,2)))
#estimate model 
garch11acfit=ugarchfit(spec=garch11ac, data=dtrain[complete.cases(dtrain),])

#conditional volatility plot
plot.ts(sigma(garch11acfit), ylab="sigma(t)", col="blue")
#Model akike
infocriteria(garch11acfit)
#Normal residuals
garchres <- data.frame(residuals(garch11acfit))
plot(garchres$residuals.garch11acfit.)
#Standardized residuals
garchres <- data.frame(residuals(garch11acfit, standardize=TRUE)) 
#Normal Q plot
qqnorm(garchres$residuals.garch11acfit..standardize...TRUE.)
qqline(garchres$residuals.garch11acfit..standardize...TRUE.)
#Squared standardized residuals Ljung Box
garchres <- data.frame(residuals(garch11acfit, standardize=TRUE)^2) 
Box.test(garchres$residuals.garch11acfit..standardize...TRUE..2, type="Ljung-Box")
#GARCH Forecasting
# prediction length required
predlen <- length(dtest)
garchforecast <- ugarchforecast(garch11acfit, n.ahead = predlen )
plot(garchforecast)

#######################################


