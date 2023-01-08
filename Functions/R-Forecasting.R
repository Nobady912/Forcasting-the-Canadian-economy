#######################################
# ECON 485
# Forecasting with ARMA and VAR

# Initialize
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

#setwd("G:/My Drive/Studies/Teaching/Classes/Econ 485/2022/R")
setwd("/Volumes/GoogleDrive/Shared drives/ECON 485 Group RepV1")


# Packages
library(lubridate)    # Easy date conversions
library(cansim)       # Get data from StatsCan
library(OECD)         # Get data from OECD
library(WDI)          # Get data from World Bank
library(fredr)        # Get data from FRED
library(forecast)     # Time Series Forecasting
library(vars)         # Vector Autoregression
library(plotrix)      # Draw a circle

# Functions
source("functions/ts_cansim.R")
source("functions/ts_fred.R")
source("functions/ts_oecd.R")

######################################



## 1. Estimating ARMA Models ----

var = 'v2091072'     # Canadian Monthly Employment
begin = '1970-01-01'

emp <- ts_cansim(var,begin)
emp.yoy <- diff(log(emp),12)


# a) ARMA(q,p)

mod.emp <- Arima(emp.yoy, c(2,0,0))
plot(emp.yoy, xlab = '')
lines(fitted(mod.emp), col = 'blue')

# (1) Check for stability
plot(mod.emp)

# (2) Check the residuals
plot(mod.emp$residuals)
plot(density(mod.emp$residuals))

ehat <- mod.emp$residuals
summary(Arima(ehat, order = c(1,0,0), include.mean = FALSE))
# Conclude: Serial correlation in the error terms can't be rejected!
acf(ehat)
checkresiduals(mod.emp)
# Conclude (Box-Ljung): error terms are significantly different from white noise 

# Calculate AIC and BIC
aic <- AIC(mod.emp)
aic
bic <- BIC(mod.emp)
bic

# b) Determine the lag length of AR and MA components automatically

mod.emp <- auto.arima(emp.yoy, seasonal = FALSE, stationary = TRUE, ic = 'bic')
summary(mod.emp)

# (1) Stability
plot(mod.emp)

# (2) Residuals
checkresiduals(mod.emp)

# Calculate AIC and BIC
aic <- AIC(mod.emp)
aic
bic <- BIC(mod.emp)
bic


## 2. Forecasting with ARMA ----

autoplot(emp.yoy, xlab = '') # comes with the forecast package
# Forecast 12 months ahead
fc.emp <- forecast(mod.emp, h = 14)
autoplot(fc.emp, include = 60, xlab = '')


## 3. Estimating VARs ----
date.start <- '1997-01-01'

cpi <- ts_cansim("v41690914", start = date.start)
gdp <- ts_cansim("v65201210", start = date.start)
ten.year <- ts_cansim("v122543", start = date.start)
wage <- ts_cansim("v103451670", start = date.start)

plot(cbind(cpi, gdp, ten.year, wage))

# Make variables stationary (economic intuition)
CPI <- diff(log(cpi), 12)
GDP <- diff(log(gdp), 12)
TEN.YEAR <- ten.year
WAGE <- diff(log(wage), 12)

plot(cbind(ten.year, diff(ten.year), diff(ten.year,12)))


data <- cbind(CPI, GDP, TEN.YEAR, WAGE)
plot(data)
# The last period for which all data is available needs to be the end
# date of our estimation sample!
tail(data)

date.est.start <- c(1998, 1)
date.est.end <- c(2022, 7)
data <- window(data, start = date.est.start, end = date.est.end)
tail(data)
head(data)

# a) Selecting the lag length
VARselect(data, lag.max = 11, type = "const")  
# Note 1: maximum lag length should never be longer than 1 year
# Note 2: SC stands for Schwarz information criterion and is the BIC

n.lag <- 3

# b) Estimation
mod.var <- VAR(data,  p = n.lag, type = c("const"))
#plot(mod.var)
# The Coefficient Matrix
coef(mod.var)

# c) Testing Stability
#plot(mod.var)   # Residuals
roots(mod.var)  # unit root

#dev.new(width = 5, height = 5, unit="in")
plot(roots(mod.var, modulus = FALSE), xlim = c(-2,2),  ylim = c(-2,2))
draw.circle(x = 0, y = 0, radius = 1)
#dev.off()

# d) Granger Causality
causality(mod.var, cause = 'TEN.YEAR')

# e) Forecasting
fc.var <- forecast(mod.var, h = 29)
plot(fc.var, include = 60)

# Converting first difference back to level
ten.year.diff <- c(TEN.YEAR,fc.var$forecast$TEN.YEAR$mean) # combining difference of the first year and the forecast
ten.year.fc <- ts(cumsum(c(ten.year[1], ten.year.diff)), start = start(ten.year), freq = 12) # summing back our first time period, then all the rest...
plot(ten.year.fc)
lines(ten.year, col = 'blue')

gr_cpi <- diff(cpi, 1)
plot(gr_cpi)

## 5. VAR with Exogenous Variables ----

fredr_set_key('7b15ffc9ff456a8d5e3e579d2b04a9f8')
wti <- ts_fred('MCOILWTICO', start = date.start) # WTI oil price
plot(wti, xlab = '')
gdp.us <- ts_fred('BBKMGDP', start = date.start) 
plot(gdp.us, xlab = '')

exomat <- window(cbind(wti,gdp.us), start = date.est.start, end = date.est.end)
tail(exomat) # jul GDP is missing, ideally find expert opinion on what that value is

# How do we get the last value of US GDP??? Be Creative!
# For now I will fill it in with the last known value:
n <- nrow(exomat)
exomat[,'gdp.us'][n] <- exomat[,'gdp.us'][(n-1)]
plot(exomat)

# a) Estimation with exogenous variable  (usgdp and wti)

VARselect(data, lag.max = 10, type = "const", exogen = exomat)

n.lag <- 3

mod.var.oil <- VAR(data,  p = n.lag, type = c("const"), exogen = exomat)
#plot(mod.var.oil)

roots(mod.var.oil)

# causality(mod.var.oil, cause = 'wti') # Does not work, find other way?


# b) Conditional/Scenario Forecast with exogenous variable(s)

h <- 29

wti.path <- rep(NA, h)
gdp.us.path <- rep(NA, h)

# Scenario 1 (Base Case):
gdp.us.path <- rep(mean(gdp.us), h)           # 
wti.path <- rep(wti[length(wti)], h) # Oil price stays at current level
plot(c(wti,wti.path), type = 'l')

exomat.path <- cbind(wti.path, gdp.us.path)
colnames(exomat.path) <- c('wti','gdp.us')

fc.var <- forecast(mod.var.oil, h = h, dumvar = exomat.path)
plot(fc.var, include = 36)

fc.var$model$varresult

# Scenario 2: 
wti.path <- seq(from = wti[length(wti)],  
                to = wti[length(wti)]*2, 
                length.out = h) # Oil price gradually doubles

exomat.path <- cbind(wti.path, gdp.us.path)
colnames(exomat.path) <- c('wti','gdp.us')

fc.var <- forecast(mod.var.oil, h = h, dumvar = exomat.path)
plot(fc.var, include = 36)


# Scenario 3: 
wti.path <- seq(from = wti[length(wti)],  
                to = 20, 
                length.out = h) # Oil price falls gradually to $20

exomat.path <- cbind(wti.path, gdp.us.path)
colnames(exomat.path) <- c('wti','gdp.us')

fc.var <- forecast(mod.var.oil, h = h, dumvar = exomat.path)
plot(fc.var, include = 36)


# Scenario 4: 
wti.path <- rep(20, h)          # Oil drops to $20 and stays there

exomat.path <- cbind(wti.path, gdp.us.path)
colnames(exomat.path) <- c('wti','gdp.us')

fc.var <- forecast(mod.var.oil, h = h, dumvar = exomat.path)
plot(fc.var, include = 36)

