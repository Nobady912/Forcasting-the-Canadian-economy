# clean up the evil enviroment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

#running the package
library(AER)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(plotly)
library(wesanderson)

####################################

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
  #the first is x, and the second is Y
plot(income ~ age, data = CreditCard)
  #income on age, the data set on credit

# categorical variables
data("Affairs")
plot(affairs ~ gender, data = Affairs)
plot(affairs ~ jitter(as.numeric(gender)), data = Affairs)

# line plots
plot(economics$unemploy, type = 'l')
ts.plot(economics$unemploy)
plot(as.ts(economics$unemploy))


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

View(table(Affairs))
barplot(margin.table(table(Affairs),2))

# dot charts
dotchart(margin.table(Titanic,1))

# distributional 
hist(CreditCard$income)
plot(density(log(CreditCard$income)))
boxplot(log(CreditCard$income))
?boxplot


# 1.2 Basic Graphical Parameters

u <- ts(economics$uempmed, start = c(year(economics$date[1]), month(economics$date[1])), freq = 12)
s <- ts(economics$psavert, start = c(year(economics$date[1]), month(economics$date[1])), freq = 12)

plot(u, type = 'l')
plot(u, type = 'b')
  #b is line and point

plot(u, ylim = c(-5,30))
plot(u, ylim = c(-5,30), xlab = '', ylab = '', main = "US unemployment Rate in %")

plot(u, ylim = c(-5,30), xlab = '', ylab = '', 
     main = "US unemployment Rate in %", 
     lwd = 1, type = 'b', 
     pch = 13)

# 1.3 Colours

# individual colours
plot(u)
plot(u, col = '#007C41')
plot(u, col = 4)
plot(u, col = 'cadetblue4', lwd = 2)

# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

barplot(margin.table(Titanic,1), col = 1:4)
barplot(margin.table(Titanic,1), 
        col = c('slateblue1','slateblue2','slateblue3','slateblue4'))

# colour palettes
display.brewer.all()

col.new <- brewer.pal(n = 10, 'Set3')
barplot(margin.table(Titanic,1), col = col.new)


# 1.4 Additional Elements

# Lines
plot(u)
abline(h = mean(u))
abline(v = 2007+8/12, col = 'red')

plot(income ~ age, data = CreditCard)
abline(lm(income ~ age, data = CreditCard), col = 'steelblue4', lwd = 3)

ts.plot(u, col = 'maroon4', ylim = c(0,25))
lines(s, col = 'palegreen4')

# Shapes
plot(u)
#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45,
#     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"), ...)
rect(1993, 0, 2007, max(u))
rect(1993, 0, 2007, max(u)*1.5, col = 'plum2', border = 'white')
lines(u)
lines(s, col = 'peachpuff3')
box()

# Legends
legend('bottom', lty = 1, legend = c('unemployment rate','saving rate'), col = c('black','peachpuff3'))

# all together
mycols <- brewer.pal(3, 'Accent')

plot(u, main = 'US Macroeconomic variables during the Great Moderation', ylab = '', xlab = '')
rect(1993, 0, 2007, max(u)*1.5, col = 'seashell', border = 'white')
lines(u, col = mycols[1], lty = 1)
lines(s, col = mycols[2], lty = 2)
box()
legend('topleft', lty = c(1,2), legend = c('unemployment rate in %','savings rate in %'), col = mycols[1:2])

# 1.5 Set up the canvas

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

pdf("pics/USmacrovars.pdf", height = 6, width = 9)
plot(u, ylim = c(0,25), main = 'US Macroeconomic variables during the Great Moderation', ylab = '', xlab = '')
rect(1993, -10, 2007, max(u)*1.5, col = 'seashell', border = 'white')
lines(u, col = mycols[1], lty = 1)
lines(s, col = mycols[2], lty = 2)
box()
legend('topleft', lty = c(1,2), legend = c('unemployment rate in %','savings rate in %'), col = mycols[1:2], bty = 'n')
dev.off()



## 2. GGplots ----

data(Guns)
data(ChickWeight)

# ggplot(data) + geometry() + other stuff

ggplot(data = Guns, mapping = aes(x = law, y = violent))

# 2.1 Geometries

# a) geom_point()
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
ggplot(data = world) +
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
ggplot(ChickWeight, aes(x = Diet, y = weight)) + 
  geom_bar(stat = 'summary') +
  geom_jitter()

# Add Labels
ggplot(data = Guns, mapping  = aes(x = law, y = violent)) + 
  geom_point() + 
  geom_label(aes(label = state))
ggplot(data = Guns, mapping  = aes(x = law, y = violent)) + 
  geom_point() + 
  geom_text(aes(label = state))
ggplot(data = Guns, mapping  = aes(x = law, y = violent)) + 
  geom_point() + 
  geom_text(aes(label = state), nudge_y = 50, nudge_x = 0.3)

# Change Axis Labels, Legends and Title

ggplot(data = ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_line(aes(group = Chick)) +
  ggtitle(label = "Chick Weight by Diet Type", subtitle = "Following 578 chicks over 21 days") +
  xlab("Age in Days") + 
  ylab("Weight in g") + 
  guides(color = guide_legend(title = "Type of Diet"))

ggplot(data = ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_line(aes(group = Chick)) +
  ggtitle(label = "Chick Weight by Diet Type", subtitle = "Following 578 chicks over 21 days") +
  labs(x = "Age in Days", y = "Weight in g") + 
  guides(color = "none")


# 2.4 Graphical Parameters
#shape, colour, fill, alpha

ggplot(data = ChickWeight, aes(x = Time, y = weight)) + 
  geom_point(aes(shape = Diet))
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + 
  geom_point(aes(shape = Diet, col = Chick))
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + 
  geom_point(aes(shape = Diet, col = Chick, size = weight))
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + 
  geom_point(aes(shape = Diet, col = Chick, size = weight, alpha = weight))

summary(Guns)
ggplot(data = Guns, mapping  = aes(x = law, y = violent, col = state)) +
  geom_jitter()
ggplot(data = Guns, mapping  = aes(x = law, y = violent, col = state, size = income, shape = year)) +
  geom_jitter()
# better?
ggplot(data = Guns, mapping  = aes(x = year, y = violent, group = state, col = law )) +
  geom_line()
ggplot(data = Guns, mapping  = aes(x = year, y = violent, col = year, group = state)) + 
  geom_line()

ggplot(ChickWeight, aes(x = Diet, y = weight)) + 
  geom_bar(stat = 'summary', aes(col = Diet))
ggplot(ChickWeight, aes(x = Diet, y = weight)) + 
  geom_bar(stat = 'summary', aes(fill = Diet))


ggplot(ChickWeight, aes(x = Diet, y = weight, col = Diet)) + 
  geom_bar(stat = 'summary') +
  geom_jitter(size = 2, shape = 21, width = 0.5)


# 2.5 Themes

# Themes
ggplot(data = Guns, mapping  = aes(x = law, y = violent, col = state)) + 
  geom_jitter() + 
  theme(legend.position = "bottom")

?theme

# List of themes: https://ggplot2.tidyverse.org/reference/ggtheme.html
ggplot(data = ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_line(aes(group = Chick)) +
  ggtitle(label = "Chick Weight by Diet Type", subtitle = "Following 578 chicks over 21 days") +
  labs(x = "Age in Days", y = "Weight in g") + 
  guides(color = guide_legend(title = "Type of Diet")) + 
  theme_gray()

# More themes: https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
ggplot(data = ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_line(aes(group = Chick)) +
  ggtitle(label = "Chick Weight by Diet Type", subtitle = "Following 578 chicks over 21 days") +
  xlab("Age in Days") + 
  ylab("Weight in g") + 
  guides(color = guide_legend(title = "Type of Diet")) + 
  theme_economist() +
  theme(panel.background = element_rect(fill = "lightgrey"))

ggplot(data = ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_line(aes(group = Chick)) +
  ggtitle(label = "Chick Weight by Diet Type", subtitle = "Following 578 chicks over 21 days") +
  xlab("Age in Days") + 
  ylab("Weight in g") + 
  guides(color = guide_legend(title = "Type of Diet")) + 
  theme_solarized()

# Colour Themes

# Can also change the colour palette
ggplot(data = ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_line(aes(group = Chick)) +
  ggtitle(label = "Chick Weight by Diet Type", subtitle = "Following 578 chicks over 21 days") +
  xlab("Age in Days") + 
  ylab("Weight in g") + 
  guides(color = guide_legend(title = "Type of Diet")) + 
  theme_light() + 
  scale_color_brewer(palette = "Dark2")

# Manual Colour themes
ggplot(data = ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_line(aes(group = Chick)) +
  ggtitle(label = "Chick Weight by Diet Type", subtitle = "Following 578 chicks over 21 days") +
  xlab("Age in Days") + 
  ylab("Weight in g") + 
  guides(color = guide_legend(title = "Type of Diet")) + 
  theme_light() + 
  #scale_fill_manual (for geom_bar)
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#54C3A3"))


# https://github.com/karthik/wesanderson
names(wes_palettes)
wes_palette('Zissou1')

x <- ggplot(data = ChickWeight, aes(x = Time, y = weight, col = Diet)) + 
  geom_line(aes(group = Chick), size = 1) +
  ggtitle(label = "Chick Weight by Diet Type", subtitle = "Following 578 chicks over 21 days") +
  xlab("Age in Days") + 
  ylab("Weight in g") + 
  guides(color = guide_legend(title = "Type of Diet")) + 
  theme_light() + 
  #scale_fill_manual (for geom_bar)
  scale_color_manual(values = wes_palette(n=4, name="Moonrise2"))

ggsave('pics/ourgraph.pdf', x, height  = 6, width = 9)

## 3. Interactive plots with Plotly ----

# 3.1 3D Graphs

x <- 70*(runif(70, 0, 1))
y <- 55*(runif(70, 0, 1))
z <- 40*(runif(70, 0, 1))

plot_ly(x = ~x, y = ~y, z = ~z, intensity = z, type = 'mesh3d') 
fig <- plot_ly(x = ~x, y = ~y, z = ~z, type = 'mesh3d') 
fig

plot_ly(x = ~Time, y = ~Diet, z = ~weight, intensity = ~weight, 
        data = ChickWeight, type = 'mesh3d')
# You can choose a color palette
fig <- plot_ly(x = ~Time, y = ~Diet, z = ~weight, intensity = ~weight,
               data = ChickWeight, type = 'mesh3d', colors = 'BuPu')
fig
# save interactive files
orca(fig, "surface-plot.svg")


