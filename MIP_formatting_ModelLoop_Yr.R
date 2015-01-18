
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Yearly Output
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

# ---------------------------------------------- #
# Variables that exist for all models, but need to be aggregated to year for comparision with LPJ

AGB.y <- TotLivBiom <- TotSoilCarb <- LAI.y <- NPP.y <- NEE.y <- NPP.y <- HeteroResp.y <- AutoResp.y <- GPP.y <- LAI.y  <- Qs.y <- SoilMoist.y <- SoilTemp.y <- Evap.y <- Transp.y <- Fcomp.y <- Dens.y <- BA.y <- list()

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
      ed.temp <- c(ed.temp, mean(ed[["Fcomp"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["Fcomp"]][i:(i+11),s]))
      # jules.temp <- c(jules.temp, mean(jules.s[["Fcomp"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["Fcomp"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["Fcomp"]][i:(i+11),s]))
      }
    Fcomp.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["Fcomp"]][,s], lpj.g[["Fcomp"]][,s]))  
    names(Fcomp.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess")


    #-----------------------------------
	# Density (total)
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["Dens"]][i:(i+11),s]))
      # clm.temp <- c(clm.temp, mean(clm[["Dens"]][i:(i+11),s]))
      # jules.temp <- c(jules.temp, mean(jules.s[["Dens"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["Dens"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["Dens"]][i:(i+11),s]))
      }
    Fcomp.y[[s]] <- data.frame(cbind(ed.temp, lpj.w[["Dens"]][,s], lpj.g[["Dens"]][,s]))  
    names(Fcomp.y[[s]]) <- c("ed2", "lpj.wsl", "lpj.guess")


    #-----------------------------------
	# Basal Area (total)
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["BA"]][i:(i+11),s]))
      # clm.temp <- c(clm.temp, mean(clm[["BA"]][i:(i+11),s]))
      # jules.temp <- c(jules.temp, mean(jules.s[["BA"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["BA"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["BA"]][i:(i+11),s]))
      }
    Fcomp.y[[s]] <- data.frame(cbind(ed.temp, lpj.w[["BA"]][,s], lpj.g[["BA"]][,s]))  
    names(Fcomp.y[[s]]) <- c("ed2", "lpj.wsl", "lpj.guess")
	#---------------------------------------------------------------------
	# CARBON FLUXES
	#---------------------------------------------------------------------

    #-----------------------------------
	# GPP
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["GPP"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["GPP"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["GPP"]][i:(i+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["GPP"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["GPP"]][i:(i+11),s]))
      }
    GPP.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp, jules.temp))  
    names(GPP.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")

    #-----------------------------------
	# NPP
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["NPP"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["NPP"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["NPP"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["NPP"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["NPP"]][i:(i+11),s]))
      }
    NPP.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["NPP"]][,s], lpj.g.temp, jules.temp))  
    names(NPP.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")

    #-----------------------------------
	# NEE
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["NEE"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["NEE"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["NPP"]][i:(i+11),s])-mean(jules.s[["HeteroResp"]][i:(i+11),s]*0.237))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["NEE"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["NEE"]][i:(i+11),s]))
      }
    NEE.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp, jules.temp))  
    names(NEE.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")

    #-----------------------------------
	# AutoResp
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["AutoResp"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["AutoResp"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["AutoResp"]][i:(i+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["AutoResp"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["AutoResp"]][i:(i+11),s]))
      }
    AutoResp.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp, jules.temp))  
    names(AutoResp.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    

    #-----------------------------------
	# HeteroResp
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["HeteroResp"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["HeteroResp"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["HeteroResp"]][i:(i+11),s]*0.237))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["HeteroResp"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["HeteroResp"]][i:(i+11),s]))
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
      ed.temp <- c(ed.temp, mean(ed[["AGB"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["AGB"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["TotLivBiom"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["AGB"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["AGB"]][i:(i+11),s]))
      }
    AGB.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["TotLivBiom"]][,s], lpj.g[["AGB"]][,s], jules.temp))  
    names(AGB.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    

	#-----------------------------------
	# TotLivBiom
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["TotLivBiom"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["TotLivBiom"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["TotLivBiom"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["TotLivBiom"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["AGB"]][i:(i+11),s]))
      }
    TotLivBiom[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["TotLivBiom"]][,s], lpj.g[["TotLivBiom"]][,s], jules.temp))  
    names(TotLivBiom[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    

	#-----------------------------------
	# TotSoilCarb
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["TotSoilCarb"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["TotSoilCarb"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["TotSoilCarb"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["TotLivBiom"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["AGB"]][i:(i+11),s]))
      }

	# Extracting Soil from LPJ.G
	lpj.g.soilC <- vector()
	dir.lpj.g <- file.path(model.dir, "LPJ-GUESS", paste(site.list[s], "LPJ-GUESS", sep="_"))   
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
      ed.temp <- c(ed.temp, mean(ed[["LAI"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["LAI"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["LAI"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["LAI"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["LAI"]][i:(i+11),s]))
      }
    LAI.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w[["LAI"]][,s], lpj.g.temp, jules.temp))  
    names(LAI.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    
        
	#-----------------------------------
	# Evap
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["Evap"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["Evap"]][i:(i+11),s]))
      # jules.temp <- c(jules.temp, mean(jules.s[["Evap"]][i:(i+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["Evap"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["Evap"]][i:(i+11),s]))
      }
    Evap.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp))  
    names(Evap.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess")    
        
	#-----------------------------------
	# Transp
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["Transp"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["Tranp"]][i:(i+11),s]))
      # jules.temp <- c(jules.temp, mean(jules.s[["Transp"]][i:(i+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["Tranp"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["Transp"]][i:(i+11),s]))
      }
    Transp.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp))  
    names(Transp.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess")    

	#-----------------------------------
	# SoilMoist
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["SoilMoist"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["SoilMoist"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["SoilMoist"]][i:(i+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["SoilMoist"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["SoilMoist"]][i:(i+11),s]))
      }
    SoilMoist.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.w.temp, lpj.g.temp, jules.temp))  
    names(SoilMoist.y[[s]]) <- c("ed2", "clm45", "lpj.wsl", "lpj.guess", "jules.stat")    


	#-----------------------------------
	# SoilTemp
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["SoilTemp"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["SoilTemp"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["SoilTemp"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["SoilTemp"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["SoilTemp"]][i:(i+11),s]))
      }
    SoilTemp.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, lpj.g.temp, jules.temp))  
    names(SoilTemp.y[[s]]) <- c("ed2", "clm45", "lpj.guess", "jules.stat")    


	#-----------------------------------
	# Qs
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["Qs"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["Qs"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["Qs"]][i:(i+11),s]))
      lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["Qs"]][i:(i+11),s]))
      lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["Qs"]][i:(i+11),s]))
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
      ed.temp <- c(ed.temp, mean(ed[["tair"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["tair"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["tair"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["tair"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["tair"]][i:(i+11),s]))
      }
    tair.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(tair.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# precipf
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["precipf"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["precipf"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["precipf"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["precipf"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["precipf"]][i:(i+11),s]))
      }
    precipf.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(precipf.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# wind
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["wind"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["wind"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["wind"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["wind"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["wind"]][i:(i+11),s]))
      }
    wind.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(wind.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# lwdown
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["lwdown"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["lwdown"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["lwdown"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["lwdown"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["lwdown"]][i:(i+11),s]))
      }
    lwdown.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(lwdown.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# swdown
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["swdown"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["swdown"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["swdown"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["swdown"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["swdown"]][i:(i+11),s]))
      }
    swdown.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(swdown.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# qair
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["qair"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["qair"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["qair"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["qair"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["qair"]][i:(i+11),s]))
      }
    qair.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(qair.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	

	#-----------------------------------
	# psurf
    #-----------------------------------
    for(i in 1:length(yr.rows)){
     if(i==1) ed.temp <- clm.temp <- jules.temp <- lpj.w.temp <- lpj.g.temp <- vector()
      ed.temp <- c(ed.temp, mean(ed[["psurf"]][i:(i+11),s]))
      clm.temp <- c(clm.temp, mean(clm[["psurf"]][i:(i+11),s]))
      jules.temp <- c(jules.temp, mean(jules.s[["psurf"]][i:(i+11),s]))
      # lpj.w.temp <- c(lpj.w.temp, mean(lpj.w[["psurf"]][i:(i+11),s]))
      # lpj.g.temp <- c(lpj.g.temp, mean(lpj.g[["psurf"]][i:(i+11),s]))
      }
    psurf.y[[s]] <- data.frame(cbind(ed.temp, clm.temp, jules.temp))  
    names(psurf.y[[s]]) <- c("ed2", "clm45", "jules.stat")        	


}




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

