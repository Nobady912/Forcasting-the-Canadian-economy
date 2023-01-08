library(AER)
library(lubridate)
library(RColorBrewer)
library(ggthemes)
library(plotly)
library(wesanderson)
library(cansim)     
library(OECD)         
library(WDI)          
library(fredr)     
library(mFilter)      
library(neverhpfilter)
library(ggplot2)    
library(tsbox)

# 1.3 stats.oecd.org

# a) Go to stats.oecd.org and put together the data you want. Then get developer API
# https://stats.oecd.org/SDMX-JSON/data/SNA_TABLE1/AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+OECD+NMEC+ARG+BRA+BGR+CHN+CRI+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+ZAF.B1_GE.HCPC/all?startTime=1980&endTime=2020&dimensionAtObservation=allDimensions

data.gdp1 <- get_dataset("SNA_TABLE1", "AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+OECD+NMEC+ARG+BRA+BGR+CHN+CRI+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+ZAF.B1_GE.HCPC/all?startTime=1980&endTime=2020&dimensionAtObservation=allDimensions")

data.EPS <- get_dataset("EPS","AUS+AUT+BEL+CAN+CZE+DNK+FIN+FRA+DEU+GRC+HUN+IRL+ITA+JPN+KOR+NLD+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+IND+IDN+RUS+ZAF.EPS+EPS_MKT+TAXES+TAXCO2+TAXDIESEL+TAXNOX+TAXSOX+TRADESCH+TRADESCH_REC+TRADESCH_CO2+TRADESCH_EEF+FIT+FIT_WIND+FIT_SOLAR+EPS_NMKT+STD+ELV_NOX+ELV_SOX+ELV_PM+ELV_DIESELSO+RD_SUB+RD_RE/all?startTime=2012&endTime=2012&dimensionAtObservation=allDimensions")

data.x <- get_dataset("EDU_DEM","AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA._T+F+M.Y_LT2+Y2+Y3+Y4+Y5+Y6+Y7+Y8+Y9+Y10+Y11+Y12+Y13+Y14+Y15+Y16+Y17+Y18+Y19+Y20+Y21+Y22+Y23+Y24+Y25+Y26+Y27+Y28+Y29+Y30+Y31+Y32+Y33+Y34+Y35+Y36+Y37+Y38+Y39+Y40+Y41+Y42+Y43+Y44+Y45+Y46+Y47+Y48+Y49+Y50T54+Y55T59+Y60T64+Y_GE65/all?startTime=2019&endTime=2019&dimensionAtObservation=allDimensions")

ggplot(data = data.gdp1, aes(x = time, y = ObsValue, group = LOCATION)) + geom_line(aes(color = LOCATION))

# Note: The way the data is organized has changed, need to reorganize first..

# # b) alternative: Search through the OECD api
# data

evil <- get_dataset("KEI", "XTEXVA01.CAN+EU27_2020+G-20+OECD.GY.M/all?startTime=2020-06&endTime=2022-09&dimensionAtObservation=allDimensions")
set_list <- get_datasets()
# search_dataset("gdp", data = dataset_list)
#

evil <- get_dataset("KEI", "XTEXVA01.CAN+EU27_2020+G-20+OECD.GY.M/all?startTime=2020-06&endTime=2022-09&dimensionAtObservation=allDimensions")


https://stats.oecd.org/SDMX-JSON/data/KEI/XT+XTEXVA01.CAN+EU27_2020+G-20+OECD.GY.M/all?startTime=2020-06&endTime=2022-09&dimensionAtObservation=allDimensions