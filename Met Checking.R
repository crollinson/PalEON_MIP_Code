# ----------------------------------------------------
# Met Checking to figure out what's going on
# Christine R. Rollinson
# crollinson@gmail.com 
# 18 Jan 2015
# ----------------------------------------------------

library(ncdf4)
library(ggplot2)
library(grid)
library(nlme)

setwd("../")
outputs <- "phase1a_output_variables"
years <- 850:2010


# Met Checking
# temp <- nc_open(file.path(outputs, "Temp.annual.nc"))
# precip <- nc_open(file.path(outputs, "Precip.annual.nc"))

df1 <- read.csv(file.path(outputs, "MIP_Data_Ann_NACP2015.csv"))
summary(df1)

temp.overview <- df1[df1$Model=="ed2",]
summary(temp.overview)

for(s in unique(temp.overview$Site)){
	for(i in (min(temp.overview$Year)+5):(max(temp.overview$Year)-5)){
		temp.overview[temp.overview$Site==s & temp.overview$Year == i, "Temp.10smooth"] <- mean(temp.overview[temp.overview$Site==s & temp.overview$Year>=(i-5) & temp.overview$Year<=(i+5), "Temp"])
}}

plot(Temp.10smooth ~ Year, data=temp.overview[temp.overview$Site=="PHA",], type="l")