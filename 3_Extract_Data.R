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

setwd("~/Dropbox/PalEON CR/paleon_mip_site")
outputs <- "phase1a_output_variables"
years <- 850:2010

large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=20), axis.text.y=element_text(color="black", size=20), axis.title.x=element_text(face="bold", size=24, vjust=-1),  axis.title.y=element_text(face="bold", size=24, vjust=2.5), plot.margin=unit(c(2,2,2,2), "lines"))

# -------------------------------------------------------------------
# Reading in .nc files of variables created using script in MIP_formatting_ModelLoop_Yr.R
# -------------------------------------------------------------------
models.updated <- c("lpj.guess", "lpj.wsl", "ed2", "ed2.lu", "jules.stat", "jules.triffid", "linkages", "sibcasa", "clm.cn")

# Open the .nc files I saved from the two "MIP CODE" scripts
gpp <- nc_open(file.path(outputs, "GPP.annual.nc"))
agb <- nc_open(file.path(outputs, "AGB.annual.nc"))
lai <- nc_open(file.path(outputs, "LAI.annual.nc"))
npp <- nc_open(file.path(outputs, "NPP.annual.nc"))
nee <- nc_open(file.path(outputs, "NEE.annual.nc"))
temp <- nc_open(file.path(outputs, "Temp.annual.nc"))
precip <- nc_open(file.path(outputs, "Precip.annual.nc"))
auto.resp <- nc_open(file.path(outputs, "RespirationAuto.annual.nc"))
hetero.resp <- nc_open(file.path(outputs, "RespirationHetero.annual.nc"))
soilmoist <- nc_open(file.path(outputs, "SoilMoist.annual.nc"))
soilcarb <- nc_open(file.path(outputs, "TotSoilCarb.annual.nc"))
transp <- nc_open(file.path(outputs, "Transp.annual.nc"))
evap <- nc_open(file.path(outputs, "Evap.annual.nc"))

model.names <- c(ncvar_get(gpp, "ModelNames"))
years <- ncvar_get(gpp, "Year")-1000

sites <- names(gpp$var)[1:(length(names(gpp$var))-1)]

# extract the data into lists that are easy to use
#sites <- "PHA"
GPP <- AGB <- LAI <- NPP <- NEE <- TEMP <- PRECIP <- RA <- RH <- SOILMOIST <- SOILCARB <- EVAP <- TRANSP <- list()
for(i in 1:length(sites)){
	GPP[[i]]       <- data.frame(t(ncvar_get(gpp, sites[i])))
	AGB[[i]]       <- data.frame(t(ncvar_get(agb, sites[i])))
	LAI[[i]]       <- data.frame(t(ncvar_get(lai, sites[i])))
	NPP[[i]]       <- data.frame(t(ncvar_get(npp, sites[i])))
	NEE[[i]]       <- data.frame(t(ncvar_get(nee, sites[i])))
	TEMP[[i]]      <- data.frame(t(ncvar_get(temp, sites[i])))
	PRECIP[[i]]    <- data.frame(t(ncvar_get(precip, sites[i])))
	RA[[i]]        <- data.frame(t(ncvar_get(auto.resp, sites[i])))
	RH[[i]]        <- data.frame(t(ncvar_get(hetero.resp, sites[i])))
	SOILCARB[[i]]  <- data.frame(t(ncvar_get(soilcarb, sites[i])))
	SOILMOIST[[i]] <- data.frame(t(ncvar_get(soilmoist, sites[i])))
	# TRANSP[[i]]    <- data.frame(t(ncvar_get(transp, sites[i])))
	TRANSP[[i]]		   <- data.frame(array(NA, dim=dim(SOILMOIST[[i]])))
	EVAP[[i]]      <- data.frame(t(ncvar_get(evap, sites[i])))

	names(GPP[[i]]) <- names(AGB[[i]]) <- names(LAI[[i]]) <- names(NPP[[i]]) <- names(NEE[[i]]) <- names(TEMP[[i]]) <- names(PRECIP[[i]]) <- names(RA[[i]]) <- names(RH[[i]]) <- names(SOILCARB[[i]]) <- names(SOILMOIST[[i]]) <- names(EVAP[[i]]) <- names(TRANSP[[i]]) <- model.names
	
	row.names(GPP[[i]]) <- row.names(AGB[[i]]) <- row.names(TEMP[[i]]) <- row.names(PRECIP[[i]]) <- row.names(RA[[i]]) <- row.names(RH[[i]]) <- row.names(SOILCARB[[i]]) <- row.names(SOILMOIST[[i]]) <- row.names(EVAP[[i]]) <- row.names(TRANSP[[i]]) <- years

}
names(GPP) <- names(AGB) <- names(LAI) <- names(NPP) <- names(NEE) <- names(TEMP) <- names(PRECIP) <- names(RA) <- names(RH) <- names(SOILCARB) <- names(SOILMOIST) <- names(EVAP) <- names(TRANSP) <- sites

nc_close(gpp); nc_close(agb); nc_close(lai); nc_close(npp); nc_close(nee); nc_close(temp); nc_close(precip); nc_close(auto.resp); nc_close(hetero.resp); nc_close(soilcarb); nc_close(soilmoist); nc_close(evap); nc_close(transp)

# Bind everything together into a single data frame to write as a csv
for(i in 1:length(sites)){
	gpp.df <- stack(GPP[[i]])
	names(gpp.df) <- c("GPP", "Model")
	gpp.df$Site <- as.factor(names(GPP)[i])
	gpp.df$Year <- years
	gpp.df <- gpp.df[,c("Model", "Site", "Year", "GPP")]

	agb.df <- stack(AGB[[i]])
	names(agb.df) <- c("AGB", "Model")
	agb.df$Site <- as.factor(names(AGB)[i])

	lai.df <- stack(LAI[[i]])
	names(lai.df) <- c("LAI", "Model")
	lai.df$Site <- as.factor(names(LAI)[i])

	npp.df <- stack(NPP[[i]])
	names(npp.df) <- c("NPP", "Model")
	npp.df$Site <- as.factor(names(NPP)[i])

	nee.df <- stack(NEE[[i]])
	names(nee.df) <- c("NEE", "Model")
	nee.df$Site <- as.factor(names(NEE)[i])

	ra.df <- stack(RA[[i]])
	names(ra.df) <- c("AutoResp", "Model")
	ra.df$Site <- as.factor(names(RA)[i])

	rh.df <- stack(RH[[i]])
	names(rh.df) <- c("HeteroResp", "Model")
	rh.df$Site <- as.factor(names(RH)[i])

	soilcarb.df <- stack(SOILCARB[[i]])
	names(soilcarb.df) <- c("SoilCarb", "Model")
	soilcarb.df$Site <- as.factor(names(SOILCARB)[i])

	soilmoist.df <- stack(SOILMOIST[[i]])
	names(soilmoist.df) <- c("SoilMoist", "Model")
	soilmoist.df$Site <- as.factor(names(SOILMOIST)[i])

	evap.df <- stack(EVAP[[i]])
	names(evap.df) <- c("Evap", "Model")
	evap.df$Site <- as.factor(names(EVAP)[i])

	transp.df <- stack(TRANSP[[i]])
	names(transp.df) <- c("Transp", "Model")
	transp.df$Site <- as.factor(names(TRANSP)[i])
	
	temp.df <- stack(TEMP[[i]])
	names(temp.df) <- c("Temp", "Model")
	temp.df$Site <- as.factor(names(TEMP)[i])
	temp.df$Year <- as.numeric(row.names(TEMP[[i]]))

	precip.df <- stack(PRECIP[[i]])
	names(precip.df) <- c("Precip", "Model")
	precip.df$Site <- as.factor(names(PRECIP)[i])
	precip.df$Year <- as.numeric(row.names(PRECIP[[i]]))

	# write in the temp & precip data for models that didn't give it back and just assume it's fine
	for(y in years){
		temp.df[(temp.df$Model=="linkages" | temp.df$Model=="lpj.wsl") & temp.df$Year==y, "Temp"] <- temp.df[temp.df$Model=="lpj.guess" & temp.df$Year==y, "Temp"]
		precip.df[(precip.df$Model=="linkages" | precip.df$Model=="lpj.wsl") & precip.df$Year==y, "Precip"] <- precip.df[precip.df$Model=="lpj.guess" & precip.df$Year==y, "Precip"]
	}	

	if(i == 1) {
		df1 <- cbind(gpp.df, agb.df$AGB, lai.df$LAI, npp.df$NPP, nee.df$NEE, ra.df$AutoResp, rh.df$HeteroResp, soilcarb.df$SoilCarb, soilmoist.df$SoilMoist, evap.df$Evap, transp.df$Transp, temp.df$Temp, precip.df$Precip)
	} else {
		df1 <- rbind(df1, cbind(gpp.df, agb.df$AGB, lai.df$LAI, npp.df$NPP, nee.df$NEE, ra.df$AutoResp, rh.df$HeteroResp, soilcarb.df$SoilCarb, soilmoist.df$SoilMoist, evap.df$Evap, transp.df$Transp, temp.df$Temp, precip.df$Precip))
	}
}
names(df1) <- c("Model", "Site", "Year", "GPP", "AGB", "LAI", "NPP", "NEE", "AutoResp", "HeteroResp", "SoilCarb", "SoilMoist", "Evap", "Transp", "Temp", "Precip")
summary(df1)


df1$Updated <- as.factor(ifelse(df1$Model %in% models.updated, "Yes", "No"))
summary(df1)

write.csv(df1, file.path(outputs, "MIP_Data_Ann_2015.csv"), row.names=F)
# -------------------------------------------------------------------


