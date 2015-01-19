# ----------------------------------------------------
# Graphs & Analysis for Poster at 2015 NACP Meeting
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

# -------------------------------------------------------------------
# Reading in .nc files of variables created using script in MIP_formatting_ModelLoop_Yr.R
# -------------------------------------------------------------------
gpp <- nc_open(file.path(outputs, "GPP.annual.nc"))
agb <- nc_open(file.path(outputs, "AGB.annual.nc"))
temp <- nc_open(file.path(outputs, "Temp.annual.nc"))
precip <- nc_open(file.path(outputs, "Precip.annual.nc"))

summary(gpp$var)

GPP <- AGB <- TEMP <- PRECIP <- list()
for(i in 1:6){
	GPP[[i]] <- data.frame(t(ncvar_get(gpp, names(gpp$var)[i])))
	AGB[[i]] <- data.frame(t(ncvar_get(agb, names(agb$var)[i])))
	TEMP[[i]] <- data.frame(t(ncvar_get(temp, names(temp$var)[i])))
	PRECIP[[i]] <- data.frame(t(ncvar_get(precip, names(precip$var)[i])))

	names(GPP[[i]]) <- ncvar_get(gpp, "ModelNames")
	names(AGB[[i]]) <- ncvar_get(agb, "ModelNames")
	names(TEMP[[i]]) <- ncvar_get(temp, "ModelNames")
	names(PRECIP[[i]]) <- ncvar_get(precip, "ModelNames")		
	row.names(GPP[[i]]) <- years
	row.names(AGB[[i]]) <- years
	row.names(TEMP[[i]]) <- years
	row.names(PRECIP[[i]]) <- years
}
names(GPP) <- names(AGB) <- names(TEMP) <- names(PRECIP) <- names(gpp$var)[1:6]

nc_close(gpp); nc_close(agb); nc_close(temp); nc_close(precip)

df1 <- data.frame(array(dim=c(0,7)))
for(i in 1:6){
	gpp.df <- stack(GPP[[i]])
	names(gpp.df) <- c("GPP", "Model")
	gpp.df$Site <- as.factor(names(GPP)[i])
	gpp.df$Year <- years

	agb.df <- stack(AGB[[i]])
	names(agb.df) <- c("AGB", "Model")
	agb.df$Site <- as.factor(names(AGB)[i])

	temp.ed <- TEMP[[i]][,"ed2"]
	precip.ed <- PRECIP[[i]][,"ed2"]

	df1 <- rbind(df1, cbind(gpp.df, agb.df$AGB, temp.ed, precip.ed))

	}
names(df1) <- c("GPP", "Model", "Site", "Year", "AGB", "Temp", "Precip")
df1 <- df1[,c("Site", "Model", "Year", "GPP", "AGB", "Temp", "Precip")]
summary(df1)

write.csv(df1, file.path(outputs, "MIP_Data_Ann_NACP2015.csv"), row.names=F)
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Graphing Variables at PHA for Poster
# -------------------------------------------------------------------
df1 <- read.csv(file.path(outputs, "MIP_Data_Ann_NACP2015.csv"))
summary(df1)

large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=20), axis.text.y=element_text(color="black", size=20), axis.title.x=element_text(face="bold", size=24, vjust=-1),  axis.title.y=element_text(face="bold", size=24, vjust=2.5), plot.margin=unit(c(2,2,2,2), "lines"))

levels(df1$Model) <- c("CLM4.5", "ED2", "JULES", "LPJ-GUESS", "LPJ-WSL")

sec_2_yr <- 60*60*24*365
kgm2_2_MgHa <- (1/1000) * 10000

MgCHayr <- sec_2_yr*kgm2_2_MgHa

model.colors <- c("black", "blue", "red", "green3", "orange3")

pdf("Figures/NACP2015_GPP_PHA.pdf", width=10, height=6)
ggplot(data=df1[df1$Site=="PHA",]) + large.axes +
	geom_line(aes(x=Year, y=GPP*MgCHayr, color=Model), size=1) +
	scale_y_continuous(name=expression(bold(paste("GPP (MgC Ha"^"-1"," yr"^"-1",")")))) +
	scale_x_continuous(name="Year") +
	scale_color_manual(values=model.colors) + labs(color="Models") +
 	theme(legend.position=c(0.15,0.8), legend.text=element_text(size=18), legend.title=element_text(size=20), legend.key=element_rect(fill="white"), legend.key.width=unit(2, "line"))
dev.off()

pdf("Figures/NACP2015_AGB_PHA.pdf", width=10, height=6)
ggplot(data=df1[df1$Site=="PHA",]) + large.axes +
	geom_line(aes(x=Year, y=AGB*kgm2_2_MgHa, color=Model), size=1) +
	scale_y_continuous(name=expression(bold(paste("AGB (MgC Ha"^"-1",")")))) +
	scale_x_continuous(name="Year") +
	scale_color_manual(values=model.colors) + labs(color="Models") +
	guides(color=F)
dev.off()	

pdf("Figures/NACP2015_Temperature_ED2_PHA.pdf", width=10, height=3)
ggplot(data=df1[df1$Site=="PHA",]) + large.axes +
	geom_line(aes(x=Year, y=Temp-273.15), size=0.8, color="red") +
	scale_y_continuous(name=expression(bold(paste("Temp ("^"o","C)")))) +
	scale_x_continuous(name="Year")  +
	theme(axis.text.x=element_text(angle=0, color="black", size=16), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=24, vjust=-1),  axis.title.y=element_text(face="bold", size=18, vjust=2.5))
dev.off()	

pdf("Figures/NACP2015_Precip_Rate_ED2_PHA.pdf", width=10, height=3)
ggplot(data=df1[df1$Site=="PHA",]) + large.axes +
	geom_line(aes(x=Year, y=Precip*sec_2_yr), size=0.8, color="blue") +
	scale_y_continuous(name=expression(bold(paste("Precip (kg m"^"-2", " yr"^"-1",")")))) +
	scale_x_continuous(name="Year") +
	theme(axis.text.x=element_text(angle=0, color="black", size=16), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=24, vjust=-1),  axis.title.y=element_text(face="bold", size=18, vjust=2.5))
dev.off()	
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Finding Responses to Extreme Events
# -------------------------------------------------------------------
df1 <- read.csv(file.path(outputs, "MIP_Data_Ann_NACP2015.csv"))
summary(df1)

# Finding the 5th and 95th percentiles for extreme events
precip.extremes <- temp.extremes <- data.frame(array(NA, dim=c(2,6)))
names(precip.extremes) <- names(temp.extremes) <- unique(df1$Site)

for(i in names(temp.extremes)){
	temp.extremes[,i] <- quantile(df1[df1$Site==i, "Temp"], c(0.05, 0.95))
	precip.extremes[,i] <- quantile(df1[df1$Site==i, "Precip"], c(0.05, 0.95))
}
temp.extremes
precip.extremes


# Mean Centering the GPP & AGB Data
for(i in unique(df1$Model)){
	for(j in unique(df1$Site)){
		mean.gpp <- mean(df1[df1$Model==i & df1$Site==j,"GPP"], na.rm=T)
		mean.agb <- mean(df1[df1$Model==i & df1$Site==j,"AGB"], na.rm=T)
		mean.temp <- mean(df1[df1$Model==i & df1$Site==j,"Temp"], na.rm=T)
		mean.precip <- mean(df1[df1$Model==i & df1$Site==j,"Precip"], na.rm=T)
		df1[df1$Model==i & df1$Site==j,"GPP.centered"] <- (df1[df1$Model==i & df1$Site==j,"GPP"] - mean.gpp)/mean.gpp
		df1[df1$Model==i & df1$Site==j,"AGB.centered"] <- (df1[df1$Model==i & df1$Site==j,"AGB"] - mean.agb)/mean.agb
		df1[df1$Model==i & df1$Site==j,"Temp.centered"] <- (df1[df1$Model==i & df1$Site==j,"Temp"] - mean.temp)/mean.temp
		df1[df1$Model==i & df1$Site==j,"Precip.centered"] <- (df1[df1$Model==i & df1$Site==j,"Precip"] - mean.precip)/mean.precip
	}
}
summary(df1)


ggplot(data=df1) + large.axes + facet_grid(Model ~ .) +
	geom_point(aes(x=Temp.centered, y=GPP.centered, color=Site), size=0.8)

ggplot(data=df1) + large.axes + facet_grid(Model ~ .) +
	geom_point(aes(x=Precip.centered, y=GPP.centered, color=Site), size=0.8)

ggplot(data=df1) + large.axes + facet_grid(Model ~ .) +
	geom_point(aes(x=Temp.centered, y=AGB.centered, color=Site))

ggplot(data=df1) + large.axes + facet_grid(Model ~ .) +
	geom_point(aes(x=Precip.centered, y=AGB.centered, color=Site))


temp.max <- data.frame(array(dim=c(0,ncol(df1))))
temp.min <- data.frame(array(dim=c(0,ncol(df1))))
precip.max <- data.frame(array(dim=c(0,ncol(df1))))
precip.min <- data.frame(array(dim=c(0,ncol(df1))))
for(s in unique(df1$Site)){
	temp.max <- rbind(temp.max, df1[df1$Site==s & df1$Temp > temp.extremes[2,s],])
	temp.min <- rbind(temp.min, df1[df1$Site==s & df1$Temp < temp.extremes[1,s],])
	precip.max <- rbind(precip.max, df1[df1$Site==s & df1$Precip > precip.extremes[2,s],])
	precip.min <- rbind(precip.min, df1[df1$Site==s & df1$Precip < precip.extremes[1,s],])
}

summary(temp.max)
summary(temp.min)
summary(precip.max)
summary(precip.min)

by(temp.max$GPP.centered, temp.max$Model, FUN=mean)
by(temp.min$GPP.centered, temp.max$Model, FUN=mean)

lm.tmax <- lm(GPP.centered ~ Model -1, data=temp.max)
lm.tmin <- lm(GPP.centered ~ Model -1, data=temp.min)
summary(lm.tmax)
summary(lm.tmin)

lm.pmax <- lm(GPP.centered ~ Model - 1, data=precip.max)
lm.pmin <- lm(GPP.centered ~ Model - 1, data=precip.min)
summary(lm.pmax)
summary(lm.pmin)

dim(temp.max)
dim(temp.min)
dim(precip.max)
dim(precip.min)



ggplot() + large.axes +
	geom_boxplot(data=temp.max, aes(x=Model, y=GPP.centered), color="red", fill="red") +
	geom_boxplot(data=temp.min, aes(x=Model, y=GPP.centered), color="blue", fill="NA")

ggplot() + large.axes +
	geom_boxplot(data=precip.min, aes(x=Model, y=GPP.centered), color="red", fill="red") +
	geom_boxplot(data=precip.max, aes(x=Model, y=GPP.centered), color="blue", fill="NA")

t.test(abs(precip.min$GPP.centered), abs(precip.max$GPP.centered))