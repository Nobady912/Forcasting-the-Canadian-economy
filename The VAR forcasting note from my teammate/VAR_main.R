#######################################
# ECON 485
# Quarterly Inflation Forecast

# Initialize
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

#setwd("G:/My Drive/Studies/Teaching/Classes/Econ 485/2022/R")
setwd("/Users/labuser/Desktop/temp_wd")
setwd("C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/Forecasting/temp_wd")

# Packages
library(lubridate)    # Easy date conversions
library(cansim)       # Get data from StatsCan
library(OECD)         # Get data from OECD
library(WDI)          # Get data from World Bank
library(fredr)        # Get data from FRED
library(forecast)     # Time Series Forecasting
library(vars)         # Vector Autoregression
library(plotrix)      # Draw a circle
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(writexl)

# Functions
source("functions/ts_cansim.R")
source("functions/ts_fred.R")
source("functions/ts_oecd.R")

######################################

## Reading in data and making stationary ----
date.start <- '1991-01-01'

# CPI
monthly <- ts_cansim("v41690914", start = date.start) # cpi, monthly, SA, 2002=100
cpi <- aggregate(monthly, nfrequency = 4, mean) # cpi quarterly average
CPI <- diff(log(cpi), 4) # inflation, yoy, quarterly

# GDP
gdp <- ts_cansim("v62305752", start = date.start) # gdp, quarterly, SA, 2012=100
GDP <- diff(log(gdp), 4) # gdp growth, yoy, quarterly

# Wages
monthly <- ts_cansim("v2132579", start = date.start) # average hourly wage, monthly, not SA, 2022 dollars
wage <- aggregate(monthly, nfrequency = 4, mean) # quarterly 
WAGE <- diff(log(wage), 4) # average hourly wage growth, yoy, quarterly

# Unemployment
monthly <- ts_cansim("v2062815", start = date.start) # monthly unemployment rate, SA
unemp <- aggregate(monthly, nfrequency = 4, mean) # quarterly unemployment rate
UNEMP <- diff(unemp, 1) # first difference


# Capacity Utilization 
# https://doi.org/10.25318/1610010901-eng
capu <- ts_cansim("v4331081", start = date.start) # quarterly Total Industrial Capacity Utilization rate
CAPU <- diff(capu, 1) # first diff

# Fixed Capital Formation
# https://doi.org/10.25318/3610010801-eng
fcap <- ts_cansim("v62143959", start = date.start) # quarterly, Total Business Gross fixed capital formation
FCAP <- diff(log(fcap), 4) # yoy growth

# Bryan's Boc Target (daily) conversion to quarterly
prate <- get_cansim_vector("v39079", start_time = date.start)
pratedrop  <- prate %>% drop_na()
dd<- pratedrop %>% mutate(Q=cut.Date(Date, "quarter", labels=F)) %>% 
  group_by(Q) %>% filter(Date==max(Date))
# Convert Into TS (avoid changing the dates!)
Prate.ts <- ts(dd$VALUE,
               start=c(1993,1),
               frequency = 4)
PolicyTS <- window(Prate.ts,
                   start = c(1993,1),
                   end = c(2022,4))

POL <- diff(PolicyTS, 1) # first difference, policy rate, quarterly

## compile data into useable form ----

# for main model
data <- cbind(CPI, GDP, UNEMP, POL)
plot(data)

# for investment model
data_inv <- cbind(GDP, CAPU, FCAP, WAGE)
plot(data_inv)

## Use modeled values for missing (Bryan's code), create date window ----

# CPI
mod.CPI <- auto.arima(CPI, seasonal = FALSE, stationary = TRUE, ic='bic')
summary(mod.CPI) #ARIMA(2,0,0)
ehatCPI <- mod.CPI$residuals
summary(Arima(ehatCPI, order=c(2,0,0), include.mean=FALSE))
# Forecast CPI
fc.CPI <- forecast(mod.CPI, h=2)
autoplot(mod.CPI, include=20)

# GDP
mod.GDP <- auto.arima(GDP, seasonal = FALSE, stationary = TRUE, ic='bic')
summary(mod.GDP) #ARIMA(2,0,2)
ehatGDP <- mod.GDP$residuals
summary(Arima(ehatGDP, order=c(2,0,2), include.mean=FALSE))
#Forecast GDP
fc.GDP <- forecast(mod.GDP, h=2)
autoplot(mod.GDP, include=20)

# UNEMP 
##testing if UNEMP is white noise
Box.test(unemp, lag = 1, type = "Ljung")
adf.test(unemp)
# UNEMP - getting ARIMA(1,0,0)
mod.UNEMP <- auto.arima(unemp, seasonal = FALSE, stationary = TRUE, ic='bic')
summary(mod.UNEMP) 
ehatUNEMP <- mod.UNEMP$residuals
summary(Arima(ehatUNEMP, order=c(1,0,0), include.mean=FALSE))
#Forecast UNEMP
fc.UNEMP <- forecast(mod.UNEMP, h=1)
autoplot(mod.UNEMP, include=20)

# add the missing values for use in VAR (Main)
df_data <- data.frame(data)
df_data[(nrow(df_data)-1),1:2] <- c(fc.CPI$mean[1], fc.GDP$mean[1])
df_data[(nrow(df_data)),1:3] <- c(fc.CPI$mean[2], fc.GDP$mean[2], fc.UNEMP$mean[1])

data2 <- ts(df_data, start = c(1991,2), freq = 4)
plot(data2)

# rm(df_data)

# FCAP
mod.FCAP <- auto.arima(FCAP, seasonal = FALSE, stationary = TRUE, ic='bic')
summary(mod.FCAP) #ARIMA(2,0,2)
ehatFCAP <- mod.FCAP$residuals
summary(Arima(ehatFCAP, order=c(2,0,2), include.mean=FALSE))
#Forecast FCAP
fc.FCAP <- forecast(mod.FCAP, h=1)
autoplot(mod.FCAP, include=20)

# CAPU
mod.CAPU <- auto.arima(CAPU, seasonal = FALSE, stationary = TRUE, ic='bic')
mod.CAPU <- Arima(CAPU, order=c(1,0,0), include.mean=FALSE)
summary(mod.CAPU) #ARIMA(1,0,0)
ehatCAPU <- mod.CAPU$residuals
summary(Arima(ehatCAPU, order=c(1,0,0), include.mean=FALSE))
#Forecast CAPU
fc.CAPU <- forecast(mod.CAPU, h=1)
autoplot(mod.CAPU, include=20)

mod.capu <- auto.arima(capu, seasonal = FALSE, stationary = TRUE, ic='bic')
summary(mod.capu) #ARIMA(1,0,0)
ehatcapu <- mod.capu$residuals
summary(Arima(ehatcapu, order=c(1,0,0), include.mean=FALSE))
#Forecast CAPU
fc.CAPU <- forecast(mod.capu, h=1)
autoplot(mod.capu, include=20)


## VAR Model - Main Macro Variables: CPI, GDP, UNEMP, POL ----

# align data with estimation interval
date.est.start <- c(1993, 3)
date.est.end <- c(2022, 4)
data2 <- window(data2, start = date.est.start, end = date.est.end)
tail(data2)
head(data2)

VARselect(data2, lag.max = 3, type = "const")  
# Note 1: maximum lag length should never be longer than 1 year
# Note 2: SC stands for Schwarz information criterion and is the BIC

n.lag <- 1

# b) Estimation
mod.var <- VAR(data2,  p = n.lag, type = c("const"))
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
# sufficiently low p-value to reject the null hypothesis (that there is no
# causality) so keep the variable
# p >= 0.05 do not reject H0: (do not Granger cause) = consider a replacement
# p < 0.05 reject H0 = keep variable

causality(mod.var, cause = 'POL') # do not reject H0: "no G cause", remove POL..

causality(mod.var, cause = 'CPI') # reject H0, keep CPI

causality(mod.var, cause = 'GDP') # do not reject H0: "no G cause", remove GDP..? Suggest replace POL first

causality(mod.var, cause = 'UNEMP') # reject H0, keep UNEMP



# Forecasting
fc.var <- forecast(mod.var, h = 12)
plot(fc.var, include = 20) # improve axis label detail, why the gaps? 

# Converting first difference back to level. 

# backtransform policy rate forecast 
pol.diff <- c(POL, fc.var$forecast$POL$mean)
pol.fc <- ts(cumsum(c(PolicyTS[1], pol.diff)), start = start(PolicyTS), freq = 4)
plot(pol.fc)
lines(PolicyTS, col = 'red') # model predicts a sudden, steep drop in policy rate AND 
# negative rates. Is this consistent with what we know? Something is off.

# CPI forecast
CPI.fc <- fc.var$forecast$CPI$mean # looks alright, shows high but decreasing inflation

# gdp growth forecast
GDP.fc <- fc.var$forecast$GDP$mean # also looks alright, but shows Q 3 positive growth, not consistent. 

# unemp forecast
unemp.fc <- fc.var$forecast$unemp$mean

# pol forecast
pol.fc <- fc.var$forecast$pol$mean

# plot all forecasts, may need to back transform log data?
data.fc <- cbind(CPI.fc, GDP.fc, pol.fc, unemp.fc)
plot(data.fc)

## VAR Model - Investment: GDP, WAGE, CAPU, CAPF ----

# align data with estimation interval

date.est.start <- c(1998, 1)
date.est.end <- c(2022, 2)
data_inv <- window(data_inv, start = date.est.start, end = date.est.end)
tail(data_inv)
head(data_inv)

# var model
VARselect(data_inv, lag.max = 3, type = "const")  
# Note 1: maximum lag length should never be longer than 1 year
# Note 2: SC stands for Schwarz information criterion and is the BIC

n.lag <- 3

# Estimation
mod.var2 <- VAR(data_inv,  p = n.lag, type = c("const"))

# The Coefficient Matrix
coef(mod.var2)

# Testing Stability
roots(mod.var2)  # unit root

#dev.new(width = 5, height = 5, unit="in")
plot(roots(mod.var2, modulus = FALSE), xlim = c(-2,2),  ylim = c(-2,2))
draw.circle(x = 0, y = 0, radius = 1)

# all pass
causality(mod.var2, cause = 'GDP') 
causality(mod.var2, cause = 'WAGE') 
causality(mod.var2, cause = 'FCAP') 
causality(mod.var2, cause = 'CAPU') 

# Forecasting
fc.var_inv <- forecast(mod.var2, h = 12)
plot(fc.var_inv, include = 20)

# backtransform
capu.diff <- c(CAPU, fc.var_inv$forecast$CAPU$mean)
capu.fc <- ts(cumsum(c(capu[1], capu.diff)), start = start(capu), freq = 4)
plot(capu.fc)
lines(capu, col = 'red')


# FCAP forecast
FCAP.fc <- fc.var_inv$forecast$FCAP$mean 
# WAGE forecast
WAGE.fc <- fc.var_inv$forecast$WAGE$mean 
# CAPU forecast
CAPU.fc <- fc.var_inv$forecast$CAPU$mean
# WAGE forecast
GDP.fc <- fc.var_inv$forecast$GDP$mean

## export dataframe ----

df_export_main <- data.frame(time(CPI.fc), CPI.fc, GDP.fc, unemp.fc)

df_export_invest <- data.frame(time(FCAP.fc), FCAP.fc, WAGE.fc, CAPU.fc, GDP.fc)







