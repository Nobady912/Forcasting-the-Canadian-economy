##############################################################################################################################################################$
#Tie code 1.2
# The following code, including
  #1 Labor market 
    #1.1.4 The_Job_market_in_Canada_by_numbers_from_2018_to_2020
      #1.1.1 The unemployment rate 
      #1.1.2 The participation rate
      #1.1.3 Employment rate

    #1.2 The_Job_market_in_Canada_by_rate_from_2018_to_2020
      #1.2.1 the unemployment number
      #1.2.2 the employment number
      #1.2.3 the number of labor force
      #1.2.4 The Job market in Canada by numbers from 2018 to 2020

  #1.3 Job vacancies, payroll employees, and job vacancy rate, by economic regions, quarterly, adjusted for seasonality
  #1.4 Wages, salaries and employers' social contributions monthly from 2018-2022

# 2 export data The export change for CA, UK, US, EU, CN and JAN
  #2.1 the growth pervious period
  #2.2 the growth on the same period of the previous year

#3 the import change for CA, UK, US, EU, CN nd JAN
  #3.1 the growth previous period
  #3.2 the growth on the same period of the pervious years

#4 the data of deposit Disposable income
#5 Credit market debt to disposable income

#The following data was collected from the Bank of Canada
#The government bond yield 
#from short to long
#Can Consumer Expectations

#############################################################################################################################################################


rm(list = ls())
dev.off()
  
#make sure clean the environment


#create a new file for all the generated by the graphy
dir.create("Data")



#Step one Lode the all package that necessary. 
library (lubridate)    
library (cansim)       
library (OECD)        
library (WDI)          
library (fredr)        
library (tsbox)
library (RColorBrewer)
library(wesanderson)
library("writexl")

source("functions/ts_cansim.R")

DTS <-"2018-01-01"
# the data time start

#1 The labor data

#1.1 Labor force characteristics, monthly, seasonally adjusted and trend-cycle
#unemployment rate seasonally adjusted (2018-2022)
#https://www150.statcan.gc.ca/n1/daily-quotidien/220909/dq220909a-eng.htm?indid=3587-2&indgeo=0
#the report wrote by the statistics Canada
#get the data form statistic Canada

#1.1.1 The unemployment rate 
unemployment_rate.st <-get_cansim_vector ("v2062815", start = DTS)
The_unemployment_rate <- ts(unemployment_rate.st$VALUE)
The_Unemployment_time <-ts(unemployment_rate.st$REF_DATE)

#1.1.2 The participation rate
The_participation_rate.st <- get_cansim_vector ("v2062816", start = DTS)
The_participation_rate <- ts(The_participation_rate.st$VALUE)
The_participation_rate.st_time <-ts(The_participation_rate.st$REF_DATE)

#1.1.3 Employment rate
employment_rate.st <-get_cansim_vector ("v2062817", start = DTS)
The_employment_rate <- ts(employment_rate.st$VALUE)
The_employment_time <-ts(employment_rate.st$REF_DATE)

#1.1.4 The_Job_market_in_Canada_by_rate_from_2018_to_2020

The_Job_market_in_Canada_by_rates_from_2018_to_2020_raw <-
  cbind(The_Unemployment_time, The_unemployment_rate, The_participation_rate, The_employment_rate)


The_Job_market_in_Canada_by_rates_from_2018_to_2020 <- 
  as.data.frame(The_Job_market_in_Canada_by_rates_from_2018_to_2020_raw)

write_xlsx (The_Job_market_in_Canada_by_rates_from_2018_to_2020, 
            "Data/the_Job_market_in_Canada_by_rates_from_2018_to_2020.xlsx")




#1.2.1 the unemployment number
  
unemployment_number.st<- get_cansim_vector ("v2062811", start = DTS)
The_unemployment_number <- ts(unemployment_number.st$VALUE)
The_Unemployment_number_time <-ts(unemployment_number.st$REF_DATE)

#1.2.2 the employment number
  
employment_number.st<-get_cansim_vector ("v2062811", start = DTS)
The_employment_number <- ts(employment_number.st$VALUE)
The_employment_number_time <-ts(employment_number.st$REF_DATE)
                   
#1.2.3 the number of labor force

Canada_labor_force.st<-get_cansim_vector ("v2062810", start = DTS)
Canada_labor_force<- ts(Canada_labor_force.st$VALUE)
Canada_labor_force_time <-ts(Canada_labor_force.st$REF_DATE)

                            
#1.2.4 The Job market in Canada by numbers from 2018 to 2020

The_Job_market_in_Canada_by_numbers_from_2018_to_2020_raw <-
cbind(The_Unemployment_number_time, The_unemployment_number,The_employment_number, Canada_labor_force)

The_Job_market_in_Canada_by_numbers_from_2018_to_2020 <- 
  as.data.frame(The_Job_market_in_Canada_by_numbers_from_2018_to_2020_raw)

write_xlsx (The_Job_market_in_Canada_by_numbers_from_2018_to_2020, 
  "Data/The_Job_market_in_Canada_by_numbers_from_2018_to_2020.xlsx")




CA_Job_vacancy_rate.ts <-get_cansim_vector("v1374464762", start_time = "2018-01-01")
#collect the data
CA_Job_vacancy_rate <- ts(CA_Job_vacancy_rate.ts$VALUE)
CA_Job_vacancy_rate_time <-ts(CA_Job_vacancy_rate.ts$REF_DATE)
evil_1<-cbind(CA_Job_vacancy_rate_time, CA_Job_vacancy_rate)
pure_evil_1 <- data.frame(evil_1)
write_xlsx(pure_evil_1, "Data/The Job vacancy rate in Canada adjusted for seasonality.xlsx")


rm(list = ls())
dev.off()


#1.3 Wages, salaries and employers' social contributions monthly from 2018-2022

#1.3.1 Wages, salaries and employers' social contributions monthly from 2018-2022
Wages_salaries_and_employers.ts<- get_cansim_vector("v1996532", start_time = "2018-01-01")

Wages_salaries_and_employers_value<- ts(Wages_salaries_and_employers.ts$VALUE)
Wages_salaries_and_employers_date <- ts(Wages_salaries_and_employers.ts$REF_DATE)

 wased <- cbind (Wages_salaries_and_employers_date, Wages_salaries_and_employers_value)

write_xlsx(as.data.frame(wased),
           "data/Wages, salaries and employers social contributions monthly from 2018-2022.xlsx")



#1.3.2 the year of year growth rate of Wages, salaries and employers' social contributions monthly from 2018-2022
Wages_salaries_and_employers_wow <- ts_cansim("v1996532", start = "2018-01-01")
Wages_salaries_and_employers.yoy <- diff(log (Wages_salaries_and_employers_wow), 12)

write_xlsx( as.data.frame(Wages_salaries_and_employers.yoy),
           "Data/Wages_salaries_and_employers_yoy.xlsx")




rm(list = ls())
dev.off()

#The following data from OECD
#2 the change of exports in good (CA,UK,US,EU,JA.CN)

  #2.1 the growth pervious period
  the_export_growth_1<- get_dataset("KEI", "XT+XTEXVA01.CAN+JPN+GBR+USA+EU27_2020+CHN.GP.M/all?startTime=2018-01&endTime=2022-09&dimensionAtObservation=allDimensions")
  write_xlsx(the_export_growth_1,"Data/the_export_growth_1.xlsx")
  
  #2.2 the growth on the same period of the previous year
the_export_growth_2<- get_dataset("KEI", 
  "XT+XTEXVA01.CAN+JPN+GBR+USA+EU27_2020+BRA.GY.M/all?startTime=2018-01&endTime=2022-09&dimensionAtObservation=allDimensions")
write_xlsx(the_export_growth_1,"Data/the_export_growth_2.xlsx")

#3.0 The change of import in good (CA,UK,US,EU,JA.CN)

#3.1 the growth previous period
the_import_change_1<- get_dataset("KEI", 
 "XT+XTIMVA01.CAN+JPN+GBR+USA+EU27_2020+CHN.GP.M/all?startTime=2018-01&endTime=2022-09&dimensionAtObservation=allDimensions")
write_xlsx(the_import_change_1,"Data/the_import_change_1.xlsx")


#3.2 the growth on the same period of the pervious years
the_import_change_2<- get_dataset("KEI", 
"XT+XTIMVA01.CAN+JPN+GBR+USA+EU27_2020+CHN.GY.M/all?startTime=2018-01&endTime=2022-09&dimensionAtObservation=allDimensions")
write_xlsx(the_import_change_2,"Data/the_import_change_1.xlsx")


#4 the data of deposit Disposable income
write_xlsx(deposit_Disposable_income.st <- 
             get_cansim_vector("v1001696801", start_time = "2018-01-01"), 
           "data/deposit_Disposable_income.xlsx")

rm(list = ls())
dev.off()


#5 Household sector credit market summary table, seasonally adjusted estimates
write_xlsx(Credit_market_debt_to_disposable_income.st <- 
             get_cansim_vector("v1038036698", start_time = "2018-01-01"), 
           "data/Credit market debt to disposable income.xlsx")


