# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Yearly Output
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

# ---------------------------------------------- #
# Variables that exist for all models, but need to be aggregated to year for comparision with LPJ

AGB.y <- TotLivBiom <- TotSoilCarb <- LAI.y <- GPP.y <- NEE.y <- NPP.y <- HeteroResp.y <- AutoResp.y <- LAI.y  <- Qs.y <- SoilMoist.y <- SoilTemp.y <- Evap.y <- Transp.y <- Fcomp.y <- Dens.y <- BA.y <- list()

tair.y <- precipf.y <- wind.y <- lwdown.y <- swdown.y <- qair.y <- psurf.y <- list()

var1 <- c("NEE", "NPP", "HeteroResp", "AutoResp", "GPP", "LAI.m", "Evap", "Qs", "SoilMoist", "SoilTemp", "SWE", "Transp", "Fcomp", "Dens", "BA")
for(i in 1:length(var1)){
  assign(var1[i], list())
}

lpj.g.soilC <- data.frame(array(NA, dim=dim(lpj.w[["TotSoilCarb"]]))); names(lpj.g.soilC) <- site.list


for(s in 1:length(site.list)){
  yr.rows <- seq(1, length(ed[["AGB"]][,s]), by=12)

	#---------------------------------------------------------------------
	# DIVERSITY / STRUCTURE
	#---------------------------------------------------------------------

    #-----------------------------------
	# Fcomp (check)
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["Fcomp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["Fcomp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # jules.temp <- c(jules.temp, mean(jules.s[["Fcomp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["Fcomp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["Fcomp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    Fcomp.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["Fcomp"]][,s], lpj.g[["Fcomp"]][,s]))  
    names(Fcomp.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess")


    #-----------------------------------
	# Density (total)
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["Dens"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # clm.temp <- c(clm.temp, mean(clm[["Dens"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # jules.temp <- c(jules.temp, mean(jules.s[["Dens"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["Dens"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["Dens"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    Dens.y[[s]] <- data.frame(cbind(ed.temp, lpj.w[["Dens"]][,s], lpj.g[["Dens"]][,s]))  
    names(Dens.y[[s]]) <- c("ed2", "lpj.wsl", "lpj.guess")


    #-----------------------------------
	# Basal Area (total)
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["BA"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # clm.temp <- c(clm.temp, mean(clm[["BA"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # jules.temp <- c(jules.temp, mean(jules.s[["BA"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["BA"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["BA"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    BA.y[[s]] <- data.frame(cbind(ed.temp, lpj.w[["BA"]][,s], lpj.g[["BA"]][,s]))  
    names(BA.y[[s]]) <- c("ed2", "lpj.wsl", "lpj.guess")
	#---------------------------------------------------------------------
	# CARBON FLUXES
	#---------------------------------------------------------------------

    #-----------------------------------
	# GPP
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["GPP"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["GPP"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["GPP"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["GPP"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["GPP"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    GPP.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp, jules.temp))  
    names(GPP.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")

    #-----------------------------------
	# NPP
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["NPP"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["NPP"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["NPP"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["NPP"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["NPP"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    NPP.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["NPP"]][,s], lpj.g.temp, jules.temp))  
    names(NPP.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")

    #-----------------------------------
	# NEE
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["NEE"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["NEE"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["NPP"]][yr.rows[i]:(yr.rows[i]+11),s])-mean(jules.s[["HeteroResp"]][yr.rows[i]:(yr.rows[i]+11),s]*0.237))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["NEE"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["NEE"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    NEE.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp, jules.temp))  
    names(NEE.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")

    #-----------------------------------
	# AutoResp
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["AutoResp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["AutoResp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["AutoResp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["AutoResp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["AutoResp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    AutoResp.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp, jules.temp))  
    names(AutoResp.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    

    #-----------------------------------
	# HeteroResp
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["HeteroResp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["HeteroResp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["HeteroResp"]][yr.rows[i]:(yr.rows[i]+11),s]*0.237))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["HeteroResp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["HeteroResp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    HeteroResp.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp, jules.temp))  
    names(HeteroResp.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    

	#---------------------------------------------------------------------
	# CARBON POOLS
	#---------------------------------------------------------------------
	
	#-----------------------------------
	# AGB
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["AGB"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["AGB"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["TotLivBiom"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["AGB"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["AGB"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    AGB.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["TotLivBiom"]][,s], lpj.g[["AGB"]][,s], jules.temp))  
    names(AGB.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    

	#-----------------------------------
	# TotLivBiom
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["TotLivBiom"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["TotLivBiom"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["TotLivBiom"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["TotLivBiom"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["AGB"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    TotLivBiom[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["TotLivBiom"]][,s], lpj.g[["TotLivBiom"]][,s], jules.temp))  
    names(TotLivBiom[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    

	#-----------------------------------
	# TotSoilCarb
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["TotSoilCarb"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["TotSoilCarb"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["TotSoilCarb"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["TotLivBiom"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["AGB"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }

	# Extracting Soil from LPJ.G
	lpj.g.soilC <- vector()
    dir.lpj.g <- file.path(model.dir, "LPJ-GUESS.v2", paste(site.list[1], "LPJ-GUESS", sep="_"))
	files.lpj.g <- dir(dir.lpj.g)
  
	index <- gregexpr("month",files.lpj.g[2])[[1]][1] # LPJ-GUESS has separate annual and monthly files & we just want the monthly
	files.lpj.g.y <- files.lpj.g[substr(files.lpj.g, index, index+5)=="annual"]
  
	for(i in 1:length(files.lpj.g.y)){
   		ncMT <- nc_open(file.path(dir.lpj.g, files.lpj.g.y[i]))
        lpj.g.soilC <- c(lpj.g.soilC, colSums(ncvar_get(ncMT, "CarbPools")[3:4,]))
      	}    
    nc_close(ncMT)

    TotSoilCarb[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["TotSoilCarb"]][,s], lpj.g.soilC, jules.temp))  
    names(TotSoilCarb[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    

	#---------------------------------------------------------------------
	# OTHER
	#---------------------------------------------------------------------
    
	#-----------------------------------
	# LAI
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["LAI"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["LAI"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["LAI"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["LAI"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["LAI"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    LAI.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["LAI"]][,s], lpj.g.temp, jules.temp))  
    names(LAI.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    
        
	#-----------------------------------
	# Evap
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["Evap"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["Evap"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # jules.temp <- c(jules.temp, mean(jules.s[["Evap"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["Evap"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["Evap"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    Evap.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp))  
    names(Evap.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess")    
        
	#-----------------------------------
	# Transp
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["Transp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["Tranp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # jules.temp <- c(jules.temp, mean(jules.s[["Transp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["Tranp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["Transp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    Transp.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp))  
    names(Transp.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess")    

	#-----------------------------------
	# SoilMoist
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["SoilMoist"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["SoilMoist"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["SoilMoist"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["SoilMoist"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["SoilMoist"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    SoilMoist.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp, jules.temp))  
    names(SoilMoist.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    


	#-----------------------------------
	# SoilTemp
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["SoilTemp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["SoilTemp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["SoilTemp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["SoilTemp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["SoilTemp"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    SoilTemp.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.g.temp, jules.temp))  
    names(SoilTemp.y[[s]]) <- c("ed2", "clm45", "lpj.guess", "jules.stat")    


	#-----------------------------------
	# Qs
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["Qs"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["Qs"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["Qs"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["Qs"]][yr.rows[i]:(yr.rows[i]+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["Qs"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    Qs.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp, jules.temp))  
    names(Qs.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")        	


	#---------------------------------------------------------------------
	# MET CHECKING!
	#---------------------------------------------------------------------

	#-----------------------------------
	# tair
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["tair"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["tair"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["tair"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["tair"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["tair"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    tair.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(tair.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# precipf
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["precipf"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["precipf"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["precipf"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["precipf"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["precipf"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    precipf.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(precipf.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# wind
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["wind"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["wind"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["wind"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["wind"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["wind"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    wind.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(wind.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# lwdown
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["lwdown"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["lwdown"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["lwdown"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["lwdown"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["lwdown"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    lwdown.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(lwdown.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# swdown
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["swdown"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["swdown"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["swdown"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["swdown"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["swdown"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    swdown.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(swdown.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# qair
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["qair"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["qair"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["qair"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["qair"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["qair"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    qair.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(qair.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# psurf
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["psurf"]][yr.rows[i]:(yr.rows[i]+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["psurf"]][yr.rows[i]:(yr.rows[i]+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["psurf"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["psurf"]][yr.rows[i]:(yr.rows[i]+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["psurf"]][yr.rows[i]:(yr.rows[i]+11),s]))
      }
    psurf.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(psurf.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	


}


names(AGB.y) <- names(TotLivBiom) <- names(TotSoilCarb) <- names(LAI.y) <- names(GPP.y) <- names(NEE.y) <- names(NPP.y) <- names(HeteroResp.y) <- names(AutoResp.y) <- names(LAI.y)  <- names(Qs.y) <- names(SoilMoist.y) <- names(SoilTemp.y) <- names(Evap.y) <- names(Transp.y) <- names(Fcomp.y) <- names(Dens.y) <- names(BA.y) <- site.list




# ----------------------------------------------------------------------------
# Storing key variables as .nc files
dim.years <- ncdim_def(name="Year", units="Years", vals=1850:(1850+nrow(GPP.y[[1]])-1))
dim.allmods <- ncdim_def(name="Models", units="", vals=1:ncol(GPP.y[[1]]))
dim.3mods <- ncdim_def(name="Models", units="", vals=1:3)

dim.string <- ncdim_def("names", "", 1:24, create_dimvar=FALSE)
dim.allmods2 <- ncdim_def(name="Model Names", units="", vals=1:ncol(GPP.y[[1]]), create_dimvar=FALSE)
dim.3mods2 <- ncdim_def(name="Model Names", units="", vals=1:3, create_dimvar=FALSE)

units.fluxes <- "kg m-2 s-1"
models.all <- names(GPP.y[[1]])
models.3 <- names(tair.y[[1]])

GPP.vars <- AGB.vars <- LAI.vars <- NEE.vars <- NPP.vars <- Temp.vars <- Precip.vars <- list()
for(i in 1:length(site.list)){
	GPP.vars[[i]] <- ncvar_def(site.list[i], units=units.fluxes, dim=list(dim.allmods, dim.years))
	AGB.vars[[i]] <- ncvar_def(site.list[i], units=units.fluxes, dim=list(dim.allmods, dim.years))
	LAI.vars[[i]] <- ncvar_def(site.list[i], units=units.fluxes, dim=list(dim.allmods, dim.years))
	NEE.vars[[i]] <- ncvar_def(site.list[i], units=units.fluxes, dim=list(dim.allmods, dim.years))
	NPP.vars[[i]] <- ncvar_def(site.list[i], units=units.fluxes, dim=list(dim.allmods, dim.years))
	Temp.vars[[i]] <- ncvar_def(site.list[i], units=units.fluxes, dim=list(dim.3mods, dim.years))
	Precip.vars[[i]] <- ncvar_def(site.list[i], units=units.fluxes, dim=list(dim.3mods, dim.years))
}	
GPP.vars[[length(site.list)+1]] <- ncvar_def("ModelNames", units="", dim=list(dim.string, dim.allmods2), prec="char")
AGB.vars[[length(site.list)+1]] <- ncvar_def("ModelNames", units="", dim=list(dim.string, dim.allmods2), prec="char")
LAI.vars[[length(site.list)+1]] <- ncvar_def("ModelNames", units="", dim=list(dim.string, dim.allmods2), prec="char")
NPP.vars[[length(site.list)+1]] <- ncvar_def("ModelNames", units="", dim=list(dim.string, dim.allmods2), prec="char")
NEE.vars[[length(site.list)+1]] <- ncvar_def("ModelNames", units="", dim=list(dim.string, dim.allmods2), prec="char")
Temp.vars[[length(site.list)+1]] <- ncvar_def("ModelNames", units="", dim=list(dim.string, dim.3mods2), prec="char")
Precip.vars[[length(site.list)+1]] <- ncvar_def("ModelNames", units="", dim=list(dim.string, dim.3mods2), prec="char")

names(GPP.vars) <- names(AGB.vars) <- names(Temp.vars) <- names(Precip.vars) <- c(site.list, "ModelNames")
summary(GPP.vars)

output.location <- "phase1a_output_variables"
gpp <- nc_create(file.path(output.location, "GPP.annual.nc"), GPP.vars)
agb <- nc_create(file.path(output.location, "AGB.annual.nc"), AGB.vars)
lai <- nc_create(file.path(output.location, "LAI.annual.nc"), LAI.vars)
npp <- nc_create(file.path(output.location, "NPP.annual.nc"), NPP.vars)
nee <- nc_create(file.path(output.location, "NEE.annual.nc"), NEE.vars)
temp <- nc_create(file.path(output.location, "Temp.annual.nc"), Temp.vars)
precip <- nc_create(file.path(output.location, "Precip.annual.nc"), Precip.vars)

for(i in 1:length(site.list)){
	ncvar_put(gpp, GPP.vars[[i]], t(GPP.y[[i]]))
	ncvar_put(agb, AGB.vars[[i]], t(AGB.y[[i]]))
	ncvar_put(lai, LAI.vars[[i]], t(LAI.y[[i]]))
	ncvar_put(npp, NPP.vars[[i]], t(NPP.y[[i]]))
	ncvar_put(nee, NEE.vars[[i]], t(NEE.y[[i]]))
	ncvar_put(temp, Temp.vars[[i]], t(tair.y[[i]]))
	ncvar_put(precip, Precip.vars[[i]], t(precipf.y[[i]]))
}
ncvar_put(gpp, GPP.vars[[length(site.list)+1]], models.all)
ncvar_put(agb, AGB.vars[[length(site.list)+1]], models.all)
ncvar_put(lai, LAI.vars[[length(site.list)+1]], models.all)
ncvar_put(npp, NPP.vars[[length(site.list)+1]], models.all)
ncvar_put(nee, NEE.vars[[length(site.list)+1]], models.all)
ncvar_put(temp, Temp.vars[[length(site.list)+1]], models.3)
ncvar_put(precip, Precip.vars[[length(site.list)+1]], models.3)

nc_close(gpp); nc_close(agb); nc_close(temp); nc_close(precip) 

# nc <- nc_open(file.path(file.path(output.location, "GPP.annual.nc")))
# summary(nc$var)
# ncvar_get(nc, "ModelNames")
# ----------------------------------------------------------------------------


# --------------------------------------------------------

# GPP
summary(GPP.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/GPP_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(GPP.y[[s]][,"ed2"], ylim=range(GPP.y, na.rm=T), col="black", type="l", lwd=2, ylab="GPP KgC/m2/s", xlab="years since 850-01-01", main=paste(site.list[s], "GPP", sep=": "))
	lines(GPP.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(GPP.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(GPP.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(GPP.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(site.list[s]=="PHA") {
		# From Urbanski et al 2007
		points(x=2000-850, y=14*1e3*yr2sec*1e-4, cex=1.5, col="gray50", pch=19)
		# arrows(x0=2000-850, y0=(14-1.641137817)*1e-6*yr2sec*1e4, x1=2000-850, y1=(14+1.641137817)*1e-6*yr2sec*1e4, length=0, lwd=2, col="gray50") 
		}
	if(s==5) legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bty="n")
}
dev.off()


# NPP
summary(NPP.y[[1]]) 
pdf(width=8.5, height=11, file="PrelimGraphs/NPP_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(NPP.y[[s]][,"ed2"], ylim=range(NPP.y, na.rm=T), col="black", type="l", lwd=2, ylab="NPP KgC/m2/s", xlab="years since 850-01-01", main=paste(site.list[s], "NPP", sep=": "))
	lines(NPP.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(NPP.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(NPP.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(NPP.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==5) legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bty="n")
}
dev.off()

# NEE
summary(NEE.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/NEE_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(NEE.y[[s]][,"ed2"], ylim=range(NEE.y, na.rm=T), type="l", lwd=2, ylab="NEE KgC/m2/s", xlab="years since 850-01-01", main=paste(site.list[s], "NEE", sep=": "))
	lines(NEE.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(NEE.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(NEE.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(NEE.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(site.list[s]=="PHA") {
		# From Urbanski et al 2007
		points(x=2000-850, y=2.453846154*1e3*yr2sec*1e-4, cex=2, col="gray70", pch=19)
		# arrows(x0=2000-850, y0=(2.064461538)*1e-6*yr2sec*1e4, x1=2000-850, y1=(2.725384615)*1e-6*yr2sec*1e4, length=0, lwd=2, col="gray50") 
		}
	if(s==5) legend("bottomleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bty="n")
}
dev.off()

# AutoResp
summary(AutoResp.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/AutoResp_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(AutoResp.y[[s]][,"ed2"], ylim=range(AutoResp.y, na.rm=T), type="l", lwd=2, ylab="AutoResp KgC/m2/s", xlab="years since 850-01-01", main=paste(site.list[s], "AutoResp", sep=": "))
	lines(AutoResp.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(AutoResp.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(AutoResp.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(AutoResp.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==5) legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bty="n", cex=0.8)
}
dev.off()

# HeteroResp
summary(HeteroResp.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/HeteroResp_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(HeteroResp.y[[s]][,"ed2"], ylim=range(HeteroResp.y, na.rm=T), type="l", lwd=2, ylab="HeteroResp KgC/m2/s", xlab="years since 850-01-01", main=paste(site.list[s], "HeteroResp", sep=": "))
	lines(HeteroResp.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(HeteroResp.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(HeteroResp.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(HeteroResp.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==5) legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bty="n")
}
dev.off()

# AGB
summary(AGB.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/AGB_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(AGB.y[[s]][,"ed2"], ylim=range(AGB.y, na.rm=T), type="l", lwd=2, ylab="AGB KgC/m2", xlab="years since 850-01-01", main=paste(site.list[s], "AGB", sep=": "))
	lines(AGB.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(AGB.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(AGB.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(AGB.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(site.list[s]=="PHA") {
		# From Urbanski et al 2007
		points(x=2000-850, y=107.7942857*1e3*1e-4, cex=2, col="gray70", pch=19)
		}
	if(s==5) legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bty="n")
}
dev.off()

# TotLivBiom
summary(TotLivBiom[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/TotLivBiom_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(TotLivBiom[[s]][,"ed2"], ylim=range(TotLivBiom, na.rm=T), type="l", lwd=2, ylab="TotLivBiom KgC/m2", xlab="years since 850-01-01", main=paste(site.list[s], "TotLivBiom", sep=": "))
	lines(TotLivBiom[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(TotLivBiom[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(TotLivBiom[[s]][,"clm45"], col="green3", lwd=2)
	lines(TotLivBiom[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==5) legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bty="n")
}
dev.off()

# TotSoilCarb
summary(TotSoilCarb[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/TotSoilCarb_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(TotSoilCarb[[s]][,"ed2"], ylim=range(TotSoilCarb, na.rm=T), type="l", lwd=2, ylab="TotSoilCarb KgC/m2", xlab="years since 850-01-01", main=paste(site.list[s], "TotSoilCarb", sep=": "))
	lines(TotSoilCarb[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(TotSoilCarb[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(TotSoilCarb[[s]][,"clm45"], col="green3", lwd=2)
	lines(TotSoilCarb[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==5) legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bty="n")
}
dev.off()

# LAI
summary(LAI.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/LAI_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(LAI.y[[s]][,"ed2"], ylim=range(LAI.y, na.rm=T), col="black", type="l", lwd=2, ylab="LAI", xlab="years since 850-01-01", main=paste(site.list[s], "LAI", sep=": "))
	lines(LAI.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(LAI.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(LAI.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(LAI.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(site.list[s]=="PHA") {
		# From Urbanski et al 2007
		points(x=2000-850, y=5.225, cex=2, col="gray50", pch=19)
		}
	if(s==2) legend("bottomright", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bg="white", cex=0.6)
}
dev.off()

# Evap
summary(Evap.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/Evap_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(Evap.y[[s]][,"ed2"], ylim=range(Evap.y, na.rm=T), type="l", lwd=2, ylab="Evap kg/m2/s", xlab="years since 850-01-01", main=paste(site.list[s], "Evaporation", sep=": "))
	lines(Evap.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(Evap.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(Evap.y[[s]][,"clm45"], col="green3", lwd=2)
	# lines(Evap.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==1) legend(x=5, y=3e-5, legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45"), col=c("black", "blue", "red", "green3"), lwd=2, bty="n")
}
dev.off()


# Transp
summary(Transp.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/Transp_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(Transp.y[[s]][,"ed2"], ylim=range(Transp.y, na.rm=T), type="l", lwd=2, ylab="Transp kg/m2/s", xlab="years since 850-01-01", main=paste(site.list[s], "Transpiration", sep=": "))
	lines(Transp.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(Transp.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(Transp.y[[s]][,"clm45"], col="green3", lwd=2)
	# lines(Transp.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==5) legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45"), col=c("black", "blue", "red", "green3"), lwd=2, bty="n")
}
dev.off()

# SoilMoist
summary(SoilMoist.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/SoilMoist_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(SoilMoist.y[[s]][,"ed2"], ylim=range(SoilMoist.y, na.rm=T), type="l", lwd=2, ylab="SoilMoist", xlab="years since 850-01-01", main=paste(site.list[s], "Soil Moisture", sep=": "))
	lines(SoilMoist.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(SoilMoist.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(SoilMoist.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(SoilMoist.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==1) legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bty="n")
}
dev.off()

# SoilTemp
summary(SoilTemp.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/SoilTemp_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(SoilTemp.y[[s]][,"ed2"], ylim=range(SoilTemp.y, na.rm=T), type="l", lwd=2, ylab="SoilTemp K", xlab="years since 850-01-01", main=paste(site.list[s], "Soil Temperature", sep=": "))
	lines(SoilTemp.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	# lines(SoilTemp.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(SoilTemp.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(SoilTemp.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==1) legend("bottomleft", legend=c("ED2", "LPJ-GUESS", "CLM45", "JULES_STATIC"), col=c("black", "blue", "green3", "orange3"), lwd=2, bty="n")
}
dev.off()

# Qs
summary(Qs.y[[1]])
pdf(width=8.5, height=11, file="PrelimGraphs/Runoff_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(Qs.y[[s]][,"ed2"], ylim=range(Qs.y, na.rm=T), type="l", lwd=2, ylab="Runoff kg/m2/s", xlab="years since 850-01-01", main=paste(site.list[s], "Runoff", sep=": "))
	lines(Qs.y[[s]][,"lpj.guess"], col="blue", lwd=2)
	lines(Qs.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(Qs.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(Qs.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==5) legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "blue", "red", "green3", "orange3"), lwd=2, bty="n")
}
dev.off()


# Fcomp
summary(Fcomp.y[[1]])

# Dens
# BA


# --------------------------------------------------------------------------------
# Met Checking
# --------------------------------------------------------------------------------
# Tair

#pdf(width=8.5, height=11, file="PrelimGraphs/Tair_Annual_AllSites.pdf")
summary(tair.y[[1]])
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(tair.y[[s]][,"ed2"], ylim=range(tair.y, na.rm=T), type="l", lwd=2, ylab="Air Temp (k)", xlab="years since 850-01-01", main=paste(site.list[s], "Air Temp", sep=": "))
#	lines(tair.y[[s]][,"lpj.guess"], col="blue", lwd=2)
#	lines(tair.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(tair.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(tair.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==1) legend("topleft", legend=c("ED2", "CLM45", "JULES_STATIC"), col=c("black", "green3", "orange3"), lwd=2, bty="n")
}

summary(precipf.y[[1]])
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(precipf.y[[s]][,"ed2"], ylim=range(precipf.y, na.rm=T), type="l", lwd=2, ylab="Precip Rate", xlab="years since 850-01-01", main=paste(site.list[s], "Precip Rate", sep=": "))
#	lines(precipf.y[[s]][,"lpj.guess"], col="blue", lwd=2)
#	lines(precipf.y[[s]][,"lpj.wsl"], col="red", lwd=2)
	lines(precipf.y[[s]][,"clm45"], col="green3", lwd=2)
	lines(precipf.y[[s]][,"jules.stat"], col="orange3", lwd=2)
	if(s==5) legend("topleft", legend=c("ED2", "CLM45", "JULES_STATIC"), col=c("black", "green3", "orange3"), lwd=2, bty="n")
}

#------------------------------------------
# tair
pdf("PrelimGraphs/MetDrivers_Tair_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(tair.y[[s]][,"ed2"], ylim=range(tair.y[[s]][["ed2"]], na.rm=T)-273, type="l", lwd=1, ylab="Air Temp", xlab="years since 850-01-01", main=paste(site.list[s]))
}
dev.off()

# precipf
pdf("PrelimGraphs/MetDrivers_precipf_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(precipf.y[[s]][,"ed2"], ylim=range(precipf.y[[s]][["ed2"]], na.rm=T), type="l", lwd=1, ylab="Precip", xlab="years since 850-01-01", main=paste(site.list[s]))
}
dev.off()

# psurf
pdf("PrelimGraphs/MetDrivers_psurf_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(psurf.y[[s]][,"ed2"], ylim=range(psurf.y[[s]][["ed2"]], na.rm=T), type="l", lwd=1, ylab="Psurf", xlab="years since 850-01-01", main=paste(site.list[s]))
}
dev.off()

# qair
pdf("PrelimGraphs/MetDrivers_qair_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(qair.y[[s]][,"ed2"], ylim=range(qair.y[[s]][["ed2"]], na.rm=T), type="l", lwd=1, ylab="Qair", xlab="years since 850-01-01", main=paste(site.list[s]))
}
dev.off()

# wind
pdf("PrelimGraphs/MetDrivers_wind_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(wind.y[[s]][,"ed2"], ylim=range(wind.y[[s]][["ed2"]], na.rm=T), type="l", lwd=1, ylab="Wind", xlab="years since 850-01-01", main=paste(site.list[s]))
}
dev.off()

# co2
# pdf("PrelimGraphs/MetDrivers_co2_Annual_AllSites.pdf")
# par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
# for(s in 1:length(site.list)){
	# plot(co2.y[[s]][,"ed2"], ylim=range(co2.y[[s]][["ed2"]], na.rm=T), type="l", lwd=1, ylab="CO2", xlab="years since 850-01-01", main=paste(site.list[s]))
# }
# dev.off()

# lwdown
pdf("PrelimGraphs/MetDrivers_lwdown_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(lwdown.y[[s]][,"ed2"], ylim=range(lwdown.y[[s]][["ed2"]], na.rm=T), type="l", lwd=1, ylab="lwdown", xlab="years since 850-01-01", main=paste(site.list[s]))
}
dev.off()

# swdown
pdf("PrelimGraphs/MetDrivers_swdown_Annual_AllSites.pdf")
par(mfrow=c(3,2), mar=c(4,5,4,1)+0.1)
for(s in 1:length(site.list)){
	plot(swdown.y[[s]][,"ed2"], ylim=range(swdown.y[[s]][["ed2"]], na.rm=T), type="l", lwd=1, ylab="swdown", xlab="years since 850-01-01", main=paste(site.list[s]))
}
dev.off()
