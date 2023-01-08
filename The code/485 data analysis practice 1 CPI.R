#"“The sad truth is that most evil is done by people who never make up their minds to be good or evil.” Hannah Arendt

# Packages
library (lubridate)    # Easy date conversions
library (cansim)       # Get data from StatsCan
library (OECD)         # Get data from OECD
library (WDI)          # Get data from World Bank
library (fredr)        # Get data from FRED
library (mFilter)      # HP Filter
library (neverhpfilter)# Hamilton Filter
library (ggplot2)      # For Graphs
library (tsbox)        # Convert xts to ts

source("functions/ts_cansim.R")

CPI <-"v52673503"
CCPI <- get_cansim_vector (CPI, start_time = "1980-01-01")
  #CCPI stand for Canadian Consumer price index
View(CCPI)
plot(CCPI)

year.st <- year(CCPI$REF_DATE[1])
#the start day of the reference year which took out
#from the claims by using "$" 

month.st <- month(CCPI$REF_DATE[1])
#the start day of the reference month
#set a values of that......

day.st <- day((CCPI$REF_DATE[1]))

c(year.st, month.st,day.st)

CCPI.ts <- ts(CCPI$VALUE, start = c(year.st, month.st,day.st), freq = 52)
#ts time serisou
#the "claims#VALUE" put the time serious of the claim's value
#start at the first month and year 
# frequence: 12
#12 is for the monthly data
#1 is for the yearly data
#4 is for the quarterly data
#52 for the weekly data
#356.25 is for the daily data

plot(CCPI.ts)

#Note
#all the code upthere is how you code when you do not have the
#the function with his provide....

#ok, I will try the different frequence
#first, try the daily frequence

CCPI_daily.ts<- ts(CCPI$VALUE, start = c(year.st, month.st,day.st), freq =356.25 )
plot(CCPI_daily.ts)

CCPI_monthly.ts<- ts(CCPI$VALUE, start = c(year.st, month.st), freq =12)
plot(CCPI_monthly.ts)

CCPI_yearly.ts<- ts(CCPI$VALUE, start = c(year.st, month.st), freq =1)
plot(CCPI_yearly.ts)

summary(CCPI)

WDIsearch('infant mortality')
#the data bank search
#perhaps the most friendly one

dat <- WDI(indicator = 'SP.DYN.IMRT.FE.IN', country=c('MX','AF'), start = 2000, end = 2020)
View(dat)

ggplot(dat, aes(x = year, y = SP.DYN.IMRT.FE.IN, group = country)) + geom_line(aes(color = country))