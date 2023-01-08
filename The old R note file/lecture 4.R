# ECON 403/603
# Lecture 4: Data Visualization


# Initialize
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Packages
library(AER)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(plotly)
library(wesanderson)
######################################

data()
data("Affairs")
?Affairs 
data("economics")
data("CreditCard")
data("Titanic")

## 1. Standard Plots in R ----

# 1.1 Categories of Graphs
# x-y plot
plot(CreditCard$age, CreditCard$income)
#the x轴 as the age poll off from the credit card and the y as the income pull off from the credit card data.
plot(income ~ age, data = CreditCard)
#The same, plot (x~y) data = where is the data come from. 

# categorical variables
plot(affairs ~ gender, data = Affairs)
plot(affairs ~ jitter(as.numeric(gender)), data = Affairs)

# line plots
plot(economics$unemploy, type = 'i')
  #p/l/b/o/s/h/n
  #Points plot (default)/Line plot/Both (points and line)/Both (overplotted)/Both (overplotted)/Histogram-like plot/No plotting
  # check this line for more graphic things.
  #https://r-coder.com/plot-r/
ts.plot(economics$unemploy)
  #time serious data for unmpolyment rate.
plot(as.ts(economics$unemploy))
  #as.ts time-series compatibility

# bar plots
barplot()
# need table and aggregate data
age <- c(17,18,18,17,18,19,18,16,18,18)
table(age)
barplot(table(age))

class(Titanic)
dim(Titanic)
View(Titanic)
margin.table(Titanic,1)
barplot(margin.table(Titanic,1))
  #by the vertical line of hte talbe in the table

View(table(Affairs))
barplot(margin.table(table(Affairs),2))
  #table () function in R language is used to create a categorical representation of data with variable name and the frequency in the form of a table.
  #margin.talbe(x, margin = null)
    # x : This is the input array. It is a required parameter.
    #margin : This is the index number representing either the row (1) or the column (2). It is a required parameter.


# dot charts
dotchart(margin.table(Titanic,1), type = 'i')
  # same as the the bar chart, need to turn the data dto the table() things...

# distributional 
hist(CreditCard$income)
plot(density(log(CreditCard$income)))
  #density 核密度估计（kernel density estimation）
    #turning the ugly graphy to good looking one
    #log 自然
boxplot(log(CreditCard$income))
?boxplot


# 1.2 Basic Graphical Parameters

u <- ts(economics$uempmed, start = c(year(economics$date[1]), month(economics$date[1])), freq = 12)
s <- ts(economics$psavert, start = c(year(economics$date[1]), month(economics$date[1])), freq = 12)


# type
plot(u, type = 'l')
plot(u, type = 'b')

plot(u, ylim = c(-5,30))
plot(u, ylim = c(-5,30), xlab = '', ylab = '', main = "US unemployment Rate in %")
  # put the data 标题
plot(u, ylim = c(-5,30), xlab = '', ylab = '', 
    main = "US unemployment Rate in %", 
    lwd = 1,#lwd:the line width
    type = 'b', #type: type of line
    pch = 13) # different style of doll, go checking the fucking following websit:
#http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

#1.3 Colours
  #Tardis bule #003865
  #https://www.flagcolorcodes.com/alberta the flag color of Alberta
  #https://www.designpieces.com/palette/university-of-alberta-color-palette-hex-and-rgb/ ualberta green and gold color
  #The color whale http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
plot(u)
plot(u, col = '#007C41')
plot(u, col = 4)
plot(u, col = 'cadetblue4', lwd = 2)


barplot(margin.table(Titanic,1), col = 1:4)
barplot(margin.table(Titanic,1), 
        col = c('slateblue1','slateblue2','slateblue3','slateblue4'))

# colour palettes
# the color whale of the graphy
display.brewer.all()

col.new <- brewer.pal(n = 10, 'Set3')
#name the different color
barplot(margin.table(Titanic,1), col = col.new)

# 1.4 Additional Elements
plot(u)
abline(h = mean(u))
  #drew the horizantally like of the avergate of variable u
abline(v = 2007+8/12, col = 'red')
  #drew the vertically line in the specific line (time) and set the coloe

plot(income ~ age, data = CreditCard)
abline(lm(income ~ age, data = CreditCard), col = 'steelblue4', lwd = 3)
  #lm() function is usedto fit linear models to data frames

ts.plot(u, col = 'maroon4', ylim = c(0,25))
lines(s, col = 'palegreen4')
  # we can add other line in the graphy by using lines

# Shapes
plot(u)
#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45,
#     col =NA, border = NULL, lty = par("lty"), lwd = par("lwd"), ...)
rect(1993, 0, 2007, max(u))
  #when it start, lowest point, rightest point and highest point
rect(1993, 0, 2007, max(u)*1.5, col = 'plum2', border = 'white')
lines(u)
lines(s, col = 'peachpuff3')
  #line(the object, col peachpuff3)
box()
  #put the box in the top layout
  #make the color tranparence

# Legends
legend('bottom', lty = 1, legend = c('unemployment rate','saving rate'), col = c('black','peachpuff3'))
  #标签
  #should be one of “bottomright”, “bottom”, “bottomleft”, “left”, “topleft”, “top”, “topright”, “right”, “center”
  #lines(u, col = my color [1], lty = 1)
  #lines(s, col = my color [2], lty = 2)
    #it set the different line type and tell the fucking R which one is which

# all together
mycols <- brewer.pal(3, 'Accent')

plot(u, main = 'US Macroeconomic variables during the Great Moderation', ylab = '', xlab = '')
rect(1993, 0, 2007, max(u)*1.5, col = 'seashell', border = 'white')
lines(u, col = mycols[1], lty = 1)
lines(s, col = mycols[2], lty = 2)
box()
legend('topleft', lty = c(1,2), legend = c('unemployment rate in %','savings rate in %'), col = mycols[1:2])
  
># 1.5 Set up the canvas
  
  par(mfrow = c(1, 2), mar = c(1,1,1,1), bg = 'lightskyblue3')
plot(u)
plot(s)

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

par(resetPar())
plot(s)


# 1.6 Saving

# note: remember to use setwd() to set the environment and create a file named "lovely_econ_graphic"
pdf("lovely_econ_graphic/USmacrovars.pdf", height = 6, width = 9) #ratio is importance eiter1 16:9, 4:3
  #open us the saving processe
plot(u, ylim = c(0,25), main = 'US Macroeconomic variables during the Great Moderation', ylab = '', xlab = '')
rect(1993, -10, 2007, max(u)*1.5, col = 'seashell', border = 'white')
lines(u, col = mycols[1], lty = 1) #evil line
lines(s, col = mycols[2], lty = 2) #evil line 
box() #put the box in the highest level
legend('topleft', lty = c(1,2), legend = c('unemployment rate in %','savings rate in %'), col = mycols[1:2], bty = 'n')
# add information
dev.off() # close the saving process, no closing no opeaning the econ_graphic


# Themes
ggplot(data = Guns, mapping  = aes(x = law, y = violent, col = state)) + 
  geom_jitter() + #width 用于调节点波动的宽度, height 用于调节点波动的高度
  theme(legend.position = "bottom")
# different theme you can choice
  #https://ggplot2.tidyverse.org/reference/ggtheme.html

# 2.1 Geometries
#a) geom_point()
ggplot(data = Guns, mapping  = aes(x = law, y = violent)) +
  geom_point()
# b) geom_jitter()
ggplot(data = Guns, mapping  = aes(x = law, y = violent)) + geom_jitter()
# c) geom_text()
ggplot(data = Guns, mapping  = aes(x = law, y = violent)) + 
  geom_text()
ggplot(data = Guns, mapping  = aes(x = law, y = violent)) + 
  geom_text(aes(label = violent))
ggplot(data = Guns, mapping  = aes(x = law, y = violent)) + 
  geom_text(aes(label = state))


# d) geom_line()
ggplot(data = economics, aes(x = date, y = unemploy)) + 
  geom_line()

# e) geom_bar()
ggplot(ChickWeight, aes(x = Diet, y = weight)) + 
  geom_bar()
ggplot(ChickWeight, aes(x = Diet, y = weight)) + 
  geom_bar(stat = 'summary')
ggplot(ChickWeight, aes(x = Diet, y = weight)) + 
  geom_bar(stat = 'summary', fun = median)

# Many More
# https://ggplot2.tidyverse.org/reference/
install.packages(c("rnaturalearth","rnaturalearthdata","sf","rgeos"))
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) 
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# 2.2 Filtering and Grouping

ggplot(data = Guns, mapping  = aes(x = law, y = violent, colour = state)) +
  geom_jitter()

ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point(aes(col = Diet))

ggplot(data = ChickWeight, aes(x = Time, y = weight, col = Diet)) +
  geom_point()

ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_line(aes(col = Chick))

ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_line(aes(group = Chick, col = Diet))

# 2.3 Additional Elements
# Add Regression Lines
ggplot(data = ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_point() +
  geom_smooth(method = 'lm')

# Jitter on Bars


library (lubridate)    # Easy date conversions
library (cansim)       # Get data from StatsCan
library (OECD)         # Get data from OECD
library (WDI)          # Get data from World Bank
library (fredr)        # Get data from FRED
library (mFilter)      # HP Filter
library (neverhpfilter)# Hamilton Filter
library (ggplot2)      # For Graphs
library (tsbox)        # Convert xts to ts




