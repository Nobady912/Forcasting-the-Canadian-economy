#"what needs justification by something else cannot be the essence of anything." - Hannah Arendt

# Packages
library(lubridate)    # Easy date conversions
library(cansim)       # Get data from StatsCan
library(OECD)         # Get data from OECD
library(WDI)          # Get data from World Bank
library(fredr)        # Get data from FRED
library(mFilter)      # HP Filter
library(neverhpfilter)# Hamilton Filter
library(ggplot2)      # For Graphs
library(tsbox)        # Convert xts to ts

# Functions
#evil!
source("functions/ts_cansim.R")

## 1. Reading in Data using APIs ----

#定义claim as the data sate from the state canada
#get_cansim_vector get the data from the the stata canada
#vector: the the specicif API the link oe the v?tt
#我的天呐

View(claims)
#view the data form long ass boring one

plot(claims$VALUE)
#画点图
#the things after $ is the one on the Yaxe

year.st <- year(CCPI$REF_DATE[1])
#the start day of the reference year which took out
#from the claims by using "$" 

month.st <- month(claims$REF_DATE[1])
#the start day of the reference month
#set a values of that......

c(year.st, month.st)

claims.ts <- ts(claims$VALUE, start = c(year.st, month.st), freq = 12)
#ts time serisou
#the "claims#VALUE" put the time serious of the claim's value
#start at the first month and year 
# frequence: 12
#12 is for the monthly data
#1 is for the yearly data
#4 is for the quarterly data
#52 for the weekly data
#356.25 is for the daily data

plot(claims.ts)

#Note
#all the code upthere is how you code when you do not have the
#the function with his provide....

# my function for convenience
gdp <- ts_cansim("v65201210", start = "1950-01-01")
plot(gdp, main = 'Canadian GDP', xlab = '', ylab = 'GDP in thousands')

# 1.2 FRED
# My API Key = '7b15ffc9ff456a8d5e3e579d2b04a9f8'
fredr_set_key('7b15ffc9ff456a8d5e3e579d2b04a9f8')
#get the data from the fredr 
ucan <- fredr(series_id = "LRUNTTTTCAM156S",
              observation_start = as.Date('1950-01-01'), 
              frequency = 'q' )
#q quarter
View(ucan)
plot(ucan$date, ucan$value, type = 'l', xlab = '')

# 1.3 stats.oecd.org

# a) Go to stats.oecd.org and put together the data you want. Then get developer API
# https://stats.oecd.org/SDMX-JSON/data/SNA_TABLE1/AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+OECD+NMEC+ARG+BRA+BGR+CHN+CRI+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+ZAF.B1_GE.HCPC/all?startTime=1980&endTime=2020&dimensionAtObservation=allDimensions

data.gdp1 <- get_dataset("SNA_TABLE1", "AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+OECD+NMEC+ARG+BRA+BGR+CHN+CRI+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+ZAF.B1_GE.HCPC/all?startTime=1980&endTime=2020&dimensionAtObservation=allDimensions")

data.EPS <- get_dataset("EPS","AUS+AUT+BEL+CAN+CZE+DNK+FIN+FRA+DEU+GRC+HUN+IRL+ITA+JPN+KOR+NLD+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+IND+IDN+RUS+ZAF.EPS+EPS_MKT+TAXES+TAXCO2+TAXDIESEL+TAXNOX+TAXSOX+TRADESCH+TRADESCH_REC+TRADESCH_CO2+TRADESCH_EEF+FIT+FIT_WIND+FIT_SOLAR+EPS_NMKT+STD+ELV_NOX+ELV_SOX+ELV_PM+ELV_DIESELSO+RD_SUB+RD_RE/all?startTime=2012&endTime=2012&dimensionAtObservation=allDimensions")

data.x <- get_dataset("EDU_DEM","AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA._T+F+M.Y_LT2+Y2+Y3+Y4+Y5+Y6+Y7+Y8+Y9+Y10+Y11+Y12+Y13+Y14+Y15+Y16+Y17+Y18+Y19+Y20+Y21+Y22+Y23+Y24+Y25+Y26+Y27+Y28+Y29+Y30+Y31+Y32+Y33+Y34+Y35+Y36+Y37+Y38+Y39+Y40+Y41+Y42+Y43+Y44+Y45+Y46+Y47+Y48+Y49+Y50T54+Y55T59+Y60T64+Y_GE65/all?startTime=2019&endTime=2019&dimensionAtObservation=allDimensions")

ggplot(data = data.gdp1, aes(x = time, y = ObsValue, group = LOCATION)) + geom_line(aes(color = LOCATION))

# Note: The way the data is organized has changed, need to reorganize first..

# # b) alternative: Search through the OECD api
# dataset_list <- get_datasets()
# search_dataset("gdp", data = dataset_list)
# 
# countries <- c("AUS","AUT","BEL","CAN","CHL","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA",
#                "JPN","KOR","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA")
# t.start <- 1980
# t.end <- 2020
# 
# # Get GDP growth
# info.gdp <- get_data_structure('SNA_TABLE1')  
# info.gdp$VAR_DESC 
# list.gdp <- list(LOCATION = countries, TRANSACT = 'B1_GE', MEASURE = 'G') 
# data.gdp <- get_dataset('SNA_TABLE1', filter = list.gdp, start_time = t.start, end_time = t.end)
#  
# View(data.gdp)

# 1.4 databank.worldbank.org

WDIsearch('infant mortality')
#the data bank search
#perhaps the most friendly one
dat <- WDI(indicator = 'SP.DYN.IMRT.FE.IN', country=c('MX','AF','CN'), start = 1980, end = 2020)
View(dat)

ggplot(dat, aes(x = year, y = SP.DYN.IMRT.FE.IN, group = country)) + geom_line(aes(color = country))

## 2. Variable Transformations for Time Series

# 2.1 Trends and Seasonality

var = 'v2091072'     # Canadian Monthly Employment
begin = '1970-01-01'
emp <- ts_cansim(var,begin)

plot(emp)

emp.season <- decompose(emp) # type = "multiplicative"
plot(emp.season)

?decompose

# 2.2 Growth Rates

# a) Regular Growth Rates
gdp.g <- diff(log(emp))
plot(gdp.g)  # small grwoth rates month to month

# b) YoY Growth Rates
gdp.yoy <- diff(log(gdp), 12)
#Compare the diifferent with the month in the last year.
plot(gdp.yoy, col = 'blue')
lines(12*gdp.g, col = 'gray')
#it is the dely, it is more soomth but with thew dely


# 2.3 Filters

# a) HP Filter
#Hodrick-Prescott (HP) 过滤器是一种数据平滑技术。
# HP 过滤器通常在分析过程中应用，以消除与商业周期相关的短期波动。
# 消除这些短期波动揭示了长期趋势。这有助于与商业周期相关的经济或其他预测。
# much like the average amnoubt o the time. 
# the result will impact by the future (filter itself)
# suggested: lambda = 1600 for quarterly, 100 for yearly, 129,600 for monthly

gdp.hp <- hpfilter(log(gdp), type="lambda", freq = 129600)

plot(gdp.hp)
plot(log(gdp))
lines(gdp.hp$trend, col = 'red')
plot(gdp.hp$cycle)
gdp.hp <- gdp.hp$cycle

# b) Hamilton Filter

gdp.xts <- as.xts(log(gdp)) # unfortunately, we need to convert from ts to class xts
names(gdp.xts) <- "LOGGDP"  # and name the variable

gdp.hamilton <- yth_filter(gdp.xts, h = 24, p = 12)

head(gdp.hamilton, 30)
plot(gdp.hamilton$LOGGDP)
lines(gdp.hamilton$LOGGDP.trend, col = 'red')

plot(gdp.hamilton$LOGGDP.cycle + gdp.hamilton$LOGGDP.random)
lines(gdp.hamilton$LOGGDP.cycle, col = 'blue')
gdp.ham <- ts_ts(gdp.hamilton$LOGGDP.cycle)

plot(cbind(gdp.g, gdp.yoy, gdp.hp, gdp.ham), main = 'Comparison of Different Variable Transformations', xlab = '')


#####################################################################################################
## learning the GG plot
###################################################################################################

data("mpg")
ggplot(data = mpg) + gemo_point(mapping = aes(x= displ, y= hwy))
#gemo_point create a layer of point   to your plot which create a acatterplot.
#the mapping argument always working with aes(x= variable, y= variable)

ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, color= class))
  #we can add the third class 种类 in the graphy by using either color or size or shape.

ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, alpha= class))
  #we can use the alpha the transparency of the point to do it

ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, shape= class))
  #or the shape of the point.
  #The shape palette can deal with a maximum of 6 discrete values because more than 6 becomes difficult

ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, shape= class), color = "blue")
  #we can set the color of the point we will sing
  # it cannot show us the graphy which itc an only do is to make more good looking? 

ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, color = displ < 5))
  #by setting the different color of the value of the variable, we can classicied the difference 
  #or in other world, we can picture the class in to the differnet group

# the other way to show the third variable is to use facets function
  #draw many small things and united by the common x and y
  #based and stupid!

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy))+
             facet_wrap(~class, nrow = 2)

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy))+
  facet_grid(drv ~ .)

# 横着的

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy))+
  facet_grid( . ~ cyl )

#竖着的 






