#"“The sad truth is that most evil is done by people who never make up their minds to be good or evil.” Hannah Arendt

# Cleaning the enviroment
rm(list = ls())


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


#setting the working direction

# Functions
#evil!
source("functions/ts_cansim.R")
#remmbert to run this lovely cute line first.
#Now it's time to running the function

source("functions/ts_cansim.R")
source("functions/ts_fred.R")
source("functions/ts_oecd.R")
#This function transer the data from to the ts series
#if you want to see some cute handadjust, go to The old R note file/485 data analysis practice 1.
#professor be like: Why you do not using my function?????

#now, is the data time!
data_start <- "2018-01-01"

