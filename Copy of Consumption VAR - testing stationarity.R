# Sameple Estimating VARs ----
# attempt to construct a VAR model of consumption and forecast consumption
# variables to (maybe) include: consumer sentiment index, consumption measures (durable, non-durable, services), 
# inflation (decompose?), unemployment, overnight rate
# wage index, gdp?
#######################################
# Initialize
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

#setwd: Arts Lab: "/Users/labuser/Downloads/Temp_WD"
setwd("/Users/labuser/Desktop/temp_wd")

# Packages
library(lubridate)    # Easy date conversions
library(cansim)       # Get data from StatsCan
library(OECD)         # Get data from OECD
library(WDI)          # Get data from World Bank
library(fredr)        # Get data from FRED
library(forecast)     # Time Series Forecasting
library(vars)         # Vector Autoregression
library(plotrix)      # Draw a circle
library(ggplot2)
library(urca)
library(tseries)

# Functions
source("functions/ts_cansim.R")
source("functions/ts_fred.R")
source("functions/ts_oecd.R")

######################################


date.start <- '1997-01-01'

gdp <- ts_cansim("v65201210", start = date.start) # GDP, monthly, SA
cpi <- ts_cansim("v41690914", start = date.start) # CPI, monthly, SA
unemp <- ts_cansim("v2062815", start = date.start) # unemployment rate, monthly, SA
wage <- ts_cansim("v103451670", start = date.start) # wages by occupation, monthly, NOT SA

# only include one of these 
nmloan <- ts_cansim("v1231415568", start = date.start) # non-mortgage loans of households, monthly, NOT SA
mloan <- ts_cansim("v1231415577", start = date.start) # total credit and liabilities of households, NOT SA
totcred <- ts_cansim("v1231415582", start = date.start) # total credit and liabilities of households, NOT SA

ovrnght <- ts_cansim("v39050", start = date.start) # Overnight money market rate, daily

plot(cbind(cpi, gdp, unemp, wage, totcred))

# inspect for seasonality
obj.decomp.totcred <- decompose(totcred, type = "multiplicative")
plot(obj.decomp.totcred)

obj.decomp.wage <- decompose(wage, type = "multiplicative")
plot(obj.decomp.wage)

obj.decomp.unemp <- decompose(unemp, type = "multiplicative")
plot(obj.decomp.unemp)

ggAcf(wage)
ggAcf(unemp)

# Make variables stationary (economic intuition)
CPI <- diff(log(cpi), 12)
GDP <- diff(log(gdp), 12)
UNEMP <- diff(log(unemp),12)
WAGE <- diff(log(wage), 12)
TOTCRED <- diff(log(totcred), 12)

# plot the first-differenced data
plot(cbind(CPI, GDP, UNEMP, WAGE,TOTCRED))

ggAcf(WAGE)
ggAcf(TOTCRED)

# check for stationarity
ndiffs(CPI)
ndiffs(GDP)
ndiffs(UNEMP)
ndiffs(WAGE)
ndiffs(TOTCRED)

# KPSS: H0=stationary
ur.kpss(CPI) %>% summary() # no reject
adf.test(CPI)
ur.kpss(GDP) %>% summary() # crit for 5%: 0.463, actual: 0.4577, reject H0 at 10%
ur.kpss(UNEMP) %>% summary() # no reject
ur.kpss(TOTCRED) %>% summary() # reject for all 
ur.kpss(WAGE) %>% summary() # no reject

nsdiffs(CPI)
# apply second diff where needed
TOTCRED_2d <- diff(TOTCRED)
GDP_2d <- diff(GDP)

ndiffs(GDP_2d)
ndiffs(TOTCRED_2d)

data <- cbind(CPI, GDP_2d, UNEMP, WAGE, TOTCRED_2d)
plot(data)
# The last period for which all data is available needs to be the end
# date of our estimation sample!
tail(data)

date.est.start <- c(1998, 2)
date.est.end <- c(2022, 7)
data <- window(data, start = date.est.start, end = date.est.end)
tail(data)
head(data)

# a) Selecting the lag length
VARselect(data, lag.max = 11, type = "const")  
# Note 1: maximum lag length should never be longer than 1 year
# Note 2: SC stands for Schwarz information criterion and is the BIC

n.lag <- 11

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
# roots close to 1 - too much drift 
# need a transformation, then rerun the model/ varselect


# d) Granger Causality
causality(mod.var, cause = 'TOTCRED')
# sufficiently low p-value to reject the null hypothesis (that there is no
# causality) so keep the variable

# e) Forecasting
fc.var <- forecast(mod.var, h = 29)
plot(fc.var, include = 60)

# Converting first difference back to level
ten.year.diff <- c(TEN.YEAR,fc.var$forecast$TEN.YEAR$mean)
ten.year.fc <- ts(cumsum(c(ten.year[1], ten.year.diff)), start = start(ten.year), freq = 12)
plot(ten.year.fc)
lines(ten.year, col = 'blue')