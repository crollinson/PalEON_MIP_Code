# Doing some EDA to make sure the ED runs are at least somewhat on par with others
library(ncdf4)
setwd("../")

# ------------------------------------------------
# Setting up to compare the inital data from the models
# ------------------------------------------------
#model.dir <- "~/Dropbox/PalEON CR/PalEON_MIP_Site/phase1a_model_output/"
model.dir <- "phase1a_model_output/"

#~/Dropbox/PalEON CR/PalEON_MIP_Site/phase1a_model_output
# Models for which we have data
model.list <- dir(model.dir)
model.list

# Sites
site.list <- c("PHA", "PHO", "PUN", "PBL", "PDL", "PMB")

# useful numbers
yr2sec <- 1/(365*24*60*60)
mo2sec <- 1/(12*24*60*60)

# ------------------------------------------------------------------------
# Extracting Variables names to make life easier
# ------------------------------------------------------------------------
# Setting up directories to pull an example file
dir.ed <- file.path(model.dir, "ED2.v1.2", site.list[1])
files.ed <- dir(dir.ed)

dir.clm <- file.path(model.dir, "CLM45.v2", paste(site.list[1], "CLM45", sep="."))
files.clm <- dir(dir.clm)

dir.lpj.g <- file.path(model.dir, "LPJ-GUESS.v2", paste(site.list[1], "LPJ-GUESS", sep="_"))
files.lpj.g <- dir(dir.lpj.g)
index <- gregexpr("month",files.lpj.g[2])[[1]][1] # LPJ-GUESS has separate annual and monthly files & we just want the monthly
files.lpj.g.m <- files.lpj.g[substr(files.lpj.g, index, index+4)=="month"]
files.lpj.g.y <- files.lpj.g[substr(files.lpj.g, index, index+5)=="annual"]

dir.lpj.w <- file.path(model.dir, "lpj-wsl.v4")
files.lpj.w <- dir(dir.lpj.w)

dir.jules.s <- file.path(model.dir, "JULES.v1", paste(site.list[1], "JULES_STATIC", sep="_"))
files.jules.s <- dir(dir.jules.s)


# Opening an example file from each model
ed <- nc_open(file.path(dir.ed, files.ed[1]))
clm <- nc_open(file.path(dir.clm, files.clm[2]))
lpj.g.m <- nc_open(file.path(dir.lpj.g, files.lpj.g.m[1]))
lpj.g.y <- nc_open(file.path(dir.lpj.g, files.lpj.g.y[1]))
lpj.w <- nc_open(file.path(dir.lpj.w, paste(site.list[1], "LPJ-wsl.850.nc", sep=".")))
jules.s <- nc_open(file.path(dir.jules.s, files.jules.s[1]))

# extracting variable names
ed.var <- names(ed$var)
clm.var <- names(clm$var)
lpj.g.var.m <- names(lpj.g.m$var)
lpj.g.var.y <- names(lpj.g.y$var)[!(names(lpj.g.y$var) %in% lpj.g.var.m)] # only take yearly what we can't get monthly
lpj.g.var <- c(lpj.g.var.m, lpj.g.var.y)
lpj.w.var <- names(lpj.w$var)
jules.s.var <- names(jules.s$var)[4:length(jules.s$var)]
jules.s.var2 <- c("TotLivBiom", jules.s.var[2:length(jules.s.var)])

# PFT-level variables need to be dealt with slightly differently than single-string variables
var.diversity <- c("BA", "Dens", "Fcomp", "PFT", "fpc", "pft-vegc", "pft-lai", "pft-npp", "pft-diam", "pft-height", "nind", "estrate")

summary(clm$var)
summary(ncvar_get(clm, "Fcomp"))
#ncvar_get(clm, "pft")

# -----------------------------------
# Soil variables have different layers and need to be indexed accordingly
# -----------------------------------
#   These indices lump things into the 0.5 and 1.5 m depths of lpj.g
soil.var <- c("SoilDepth", "SoilMoist", "SoilTemp")

soil.ed <- ncvar_get(ed, "SoilDepth")
soil.ed.5 <- which(abs(soil.ed)<=0.5); vol.ed <- vector(length=length(soil.ed))
for(i in 1:(length(soil.ed)-1)){
  vol.ed[length(soil.ed)] <- abs(soil.ed[length(soil.ed)])
  vol.ed[i] <- abs(abs(soil.ed[i]) - abs(soil.ed[i+1]))
  }

soil.clm <- ncvar_get(clm, "SoilDepth")
soil.clm.5 <- which(abs(soil.clm)<=0.5); vol.clm <- vector(length=length(soil.clm))
for(i in 1:(length(soil.clm)-1)){
  vol.clm[length(soil.clm)] <- abs(soil.clm[length(soil.clm)])
  vol.clm[i] <- abs(abs(soil.clm[i]) - abs(soil.clm[i+1]))
}

soil.lpj.g <- ncvar_get(lpj.g.m, "SoilDepth")
soil.lpj.g.5 <- which(abs(soil.lpj.g)<=0.5); vol.lpj.g <- vector(length=length(soil.lpj.g))
for(i in 1:(length(soil.lpj.g)-1)){
  vol.lpj.g[length(soil.lpj.g)] <- abs(soil.lpj.g[length(soil.lpj.g)])
  vol.lpj.g[i] <- abs(abs(soil.lpj.g[i]) - abs(soil.lpj.g[i+1]))
}

#soil.lpj.w <- ncvar_get(lpj.w, "soil.depths")
vol.jules.s <- c(0.1, 0.25, 0.65, 2)
soil.jules.s <- vol.jules.s[1]
for(i in 2:length(vol.jules.s)){
	soil.jules.s[i] <- soil.jules.s[i-1] + vol.jules.s[i]
} 
soil.jules.s.5 <- which(abs(soil.jules.s)<=0.5)

# Closing files
nc_close(ed); nc_close(clm); nc_close(lpj.g.m); nc_close(lpj.g.y); nc_close(lpj.w); nc_close(jules.s)

# ------------------------------------------------------------------------
# EXTRACTING MODEL OUTPUTS
# ------------------------------------------------------------------------
# -----------------------------------
# ED 2.1
# -----------------------------------
ed <- list()
ed.diversity <- list()
for(s in 1:length(site.list)){
  dir.ed <- file.path(model.dir, "ED2.v1.2", site.list[s])
  files.ed <- dir(dir.ed)
  
  #  nee.temp <- npp.temp <- rh.temp <- ah.temp <- gpp.temp <- vector()
  ed.var.list <- list()
  div.var.list <- list()
  #-----------------------------------
  # File loop extracting time series by variable group
  for(i in 1:length(files.ed)){
    ncMT <- nc_open(file.path(dir.ed, files.ed[i]))
    for(v in 1:length(ed.var)){
      if(i == 1) temp <- vector() else temp <- ed.var.list[[v]]
      if(ed.var[v] %in% var.diversity[1:3]){
      	temp <- c(temp, colSums(ncvar_get(ncMT, ed.var[[v]])))
      } else if(ed.var[v] %in% soil.var[2:3]){
        soil.temp <- t(ncvar_get(ncMT, ed.var[v]))[,soil.ed.5]
        for(q in 1:ncol(soil.temp)){
        	soil.temp[,q] <- soil.temp[,q]* vol.ed[soil.ed.5[q]]/sum(vol.ed[soil.ed.5])
        }
        temp <- c(temp, rowSums(soil.temp))
      } else {      
      temp <- c(temp, ncvar_get(ncMT, ed.var[v])) }
      ed.var.list[[v]] <- temp
    }
    nc_close(ncMT)      
  }
  names(ed.var.list) <- ed.var
  #-----------------------------------
  # Adding variable groups to master model list
  for(v in 1:length(ed.var)){
    if(s == 1){
      ed[[v]] <- data.frame(ed.var.list[[v]]) 
    } else {
      ed[[v]][,s] <- ed.var.list[[v]]
    }
  }
} # Close the model loop
# Adding site label to each variable
names(ed) <- c(ed.var)
for(i in 1:length(ed.var)){
  names(ed[[i]]) <- site.list
}

# -----------------------------------
# CLM 45
# -----------------------------------
clm <- list() 
for(s in 1:length(site.list)){
  dir.clm <- file.path(model.dir, "CLM45.v2", paste(site.list[s], "CLM45", sep="."))
  files.clm <- dir(dir.clm)
  clm.var.list <- list()
  #-----------------------------------
  # File loop extracting time series by variable group
  for(i in 1:length(files.clm)){
    ncMT <- nc_open(file.path(dir.clm, files.clm[i]))
    for(v in 1:length(clm.var)){
      if(i == 1) temp <- vector() else temp <- clm.var.list[[v]]
      if(clm.var[v] %in% var.diversity[1:3]){
      	temp <- c(temp, rowSums(ncvar_get(ncMT, clm.var[[v]])))
      } else if(clm.var[v] %in% soil.var[2:3]){
        soil.temp <- (ncvar_get(ncMT, clm.var[v]))[,soil.clm.5]
        for(q in 1:ncol(soil.temp)){
        	soil.temp[,q] <- soil.temp[,q]* vol.clm[soil.clm.5[q]]/sum(vol.clm[soil.clm.5])
        }
        temp <- c(temp, rowSums(soil.temp))
      } else {      
        temp <- c(temp, ncvar_get(ncMT, clm.var[v]))  }
      clm.var.list[[v]] <- temp
    }    
    nc_close(ncMT)      
  }
  names(clm.var.list) <- clm.var
  #-----------------------------------
  # Adding variable groups to master model list
  for(v in 1:length(clm.var)){
    if(s == 1){
      clm[[v]] <- data.frame(clm.var.list[[v]]) 
    } else {
      clm[[v]][,s] <- clm.var.list[[v]]
    }
  }
} # Close the model loop
# Adding site label to each variable
names(clm) <- c(clm.var)
for(i in 1:length(clm.var)){
  names(clm[[i]]) <- site.list
}


# -----------------------------------
# LPJ-GUESS
# -----------------------------------
lpj.g <- list()
lpj.pft <- c(which(lpj.g.var.y=="AGB"), which(lpj.g.var.y=="TotLivBiom"))
for(s in 1:length(site.list)){
  dir.lpj.g <- file.path(model.dir, "LPJ-GUESS.v2", paste(site.list[s], "LPJ-GUESS", sep="_"))
  files.lpj.g <- dir(dir.lpj.g)
  
  index <- gregexpr("month",files.lpj.g[2])[[1]][1] # LPJ-GUESS has separate annual and monthly files & we just want the monthly
  files.lpj.g.m <- files.lpj.g[substr(files.lpj.g, index, index+4)=="month"]
  files.lpj.g.y <- files.lpj.g[substr(files.lpj.g, index, index+5)=="annual"]
  
  lpj.g.var.list <- list()
  #-----------------------------------
  # Monthly Variables
  for(i in 1:length(files.lpj.g.m)){
    ncMT <- nc_open(file.path(dir.lpj.g, files.lpj.g.m[i]))
    for(v in 1:length(lpj.g.var.m)){
      if(i == 1) temp <- vector() else temp <- lpj.g.var.list[[v]]
      if(lpj.g.var.m[v] %in% var.diversity[1:3]){
      	temp <- c(temp, colSums(ncvar_get(ncMT, lpj.g.var.m[[v]])))
      } else if(lpj.g.var[v] %in% soil.var[2]){
        temp <- c(temp, ncvar_get(ncMT, lpj.g.var.m[v])[soil.lpj.g.5,])
      } else {      
        temp <- c(temp, ncvar_get(ncMT, lpj.g.var.m[v]))  }
      lpj.g.var.list[[v]] <- temp
    }    
    nc_close(ncMT)      
  }
  # Yearly Variables
  for(i in 1:length(files.lpj.g.y)){
    ncMT <- nc_open(file.path(dir.lpj.g, files.lpj.g.y[i]))
    for(v in 1:length(lpj.g.var.y)){
      if(i == 1) temp <- vector() else temp <- lpj.g.var.list[[v+length(lpj.g.var.m)]] # this tells us to go past monthly vars
      if(v %in% lpj.pft | lpj.g.var.y[v] %in% var.diversity[1:3]){
        temp <- c(temp, ncvar_get(ncMT, lpj.g.var.y[v])[13,])
      } else {
      temp <- c(temp, ncvar_get(ncMT, lpj.g.var.y[v]))  }
      lpj.g.var.list[[v+length(lpj.g.var.m)]] <- temp
    }    
    nc_close(ncMT)      
  }
  names(lpj.g.var.list) <- lpj.g.var
  #-----------------------------------
  # Adding variable groups to master model list
  for(v in 1:length(lpj.g.var)){
    if(s == 1){
      lpj.g[[v]] <- data.frame(lpj.g.var.list[[v]]) 
    } else {
      lpj.g[[v]][,s] <- lpj.g.var.list[[v]]
    }
  }
} # Close the model loop
# Adding site label to each variable
names(lpj.g) <- c(lpj.g.var)
for(i in 1:length(lpj.g.var)){
  names(lpj.g[[i]]) <- site.list
}


# -----------------------------------
# LPJ-WSL
# -----------------------------------
lpj.w <- list()
var.pft.lpj.w <- c("LAI", "NPP")
for(s in 1:length(site.list)){
  dir.lpj.w <- file.path(model.dir, "lpj-wsl.v4")
  files.lpj.w <- dir(dir.lpj.w)
  lpj.w.var.list <- list()
  #-----------------------------------
  # File loop extracting time series by variable group
  ncMT <- nc_open(file.path(dir.lpj.w, paste(site.list[s], "LPJ-wsl.850.nc", sep=".")))
  for(v in 1:length(lpj.w.var)){
	if(lpj.w.var[v] == "NPP" | lpj.w.var[v] %in% var.diversity[1:3]){
	lpj.w.var.list[[v]] <- rowSums(ncvar_get(ncMT, lpj.w.var[v]))
	} else {
    lpj.w.var.list[[v]] <- ncvar_get(ncMT, lpj.w.var[v])
    }    
  }
  nc_close(ncMT)      

  names(lpj.w.var.list) <- lpj.w.var
  #-----------------------------------
  # Adding variable groups to master model list
  for(v in 1:length(lpj.w.var)){
    if(s == 1){
      lpj.w[[v]] <- data.frame(lpj.w.var.list[[v]]) 
    } else {
      lpj.w[[v]][,s] <- lpj.w.var.list[[v]]
    }
  }
} # Close the model loop
# Adding site label to each variable
names(lpj.w) <- c(lpj.w.var)
for(i in 1:length(lpj.w.var)){
  names(lpj.w[[i]]) <- site.list
}

# -----------------------------------
# JULES_STATIC
# Notes for fixing the loop: 
#	- currently no soil depth
#	- NPP broken down by PFT
#	- BIOMASS = 0; see note in email
# -----------------------------------
pft.vars <- c("NPP", "LAI", "Qh", "Qle", "SnowDepth")
jules.s <- list() 
for(s in 1:length(site.list)){
  dir.jules.s <- file.path(model.dir, "JULES.v1", paste(site.list[s], "JULES_STATIC", sep="_"))
  files.jules.s <- dir(dir.jules.s)
  jules.s.var.list <- list()
  #-----------------------------------
  # File loop extracting time series by variable group
  for(i in 1:length(files.jules.s)){
    ncMT <- nc_open(file.path(dir.jules.s, files.jules.s[i]))
    for(v in 1:length(jules.s.var)){
      if(i == 1) temp <- vector() else temp <- jules.s.var.list[[v]]
      if(jules.s.var[v] %in% pft.vars){ 
      	temp <- c(temp, rowSums(t(ncvar_get(ncMT, jules.s.var[v]))))}
      else {
      if(jules.s.var[v] %in% soil.var[2:3]){
        soil.temp <- t(ncvar_get(ncMT, jules.s.var[v]))[,soil.jules.s.5]
        for(q in 1:ncol(soil.temp)){
        	soil.temp[,q] <- soil.temp[,q]*vol.jules.s[soil.jules.s.5[q]]/sum(vol.jules.s[soil.jules.s.5])
        }
        temp <- c(temp, rowSums(soil.temp))
      } else {      
        temp <- c(temp, ncvar_get(ncMT, jules.s.var[v]))  } }
      jules.s.var.list[[v]] <- temp
     }   
    nc_close(ncMT)      
  }
  names(jules.s.var.list) <- jules.s.var2
  #-----------------------------------
  # Adding variable groups to master model list
  for(v in 1:length(jules.s.var)){
    if(s == 1){
      jules.s[[v]] <- data.frame(jules.s.var.list[[v]]) 
    } else {
      jules.s[[v]][,s] <- jules.s.var.list[[v]]
    }
  }
} # Close the model loop
# Adding site label to each variable
names(jules.s) <- c(jules.s.var2)
for(i in 1:length(jules.s.var)){
  names(jules.s[[i]]) <- site.list
}



# ------------------------------------------------------------------------
# ORGANIZING MODEL OUTPUTS BY VARIABLE
# ------------------------------------------------------------------------
names(clm) 
names(ed) 
names(lpj.g) 
names(lpj.w)
names(jules.s)


# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Monthly Variables for all Models
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
NEE <- NPP <- HeteroResp <- AutoResp <- GPP <- LAI.m  <- Qs <- Qs <- SoilMoist <- SoilTemp <- SWE <- SnowDepth <- Transp <- list()
var1 <- c("NEE", "NPP", "HeteroResp", "AutoResp", "GPP", "LAI.m", "Evap", "Qs", "SoilMoist", "SoilTemp", "SWE", "SnowDepth", "Transp")
for(i in 1:length(var1)){
  assign(var1[i], list())
}

#dim(jules.s[["HeteroResp"]])
#dim(ed[["HeteroResp"]])

for(s in 1:length(site.list)){
  # -------------------------------------
  # Variables for which we have output from all models
  HeteroResp[[s]] <- data.frame(cbind(clm[["HeteroResp"]][,s], ed[["HeteroResp"]][,s], lpj.g[["HeteroResp"]][,s], lpj.w[["HeteroResp"]][,s], c(jules.s[["HeteroResp"]][,s]*exp(-1.3), rep(NA, 12))))  
  AutoResp[[s]] <- data.frame(cbind(clm[["AutoResp"]][,s], ed[["AutoResp"]][,s], lpj.g[["AutoResp"]][,s], lpj.w[["AutoResp"]][,s]), c(jules.s[["AutoResp"]][,s], rep(NA, 12)))  
  GPP[[s]] <- data.frame(cbind(clm[["GPP"]][,s], ed[["GPP"]][,s], lpj.g[["GPP"]][,s], lpj.w[["GPP"]][,s]), c(jules.s[["GPP"]][,s], rep(NA, 12)))  
  SoilMoist[[s]] <- data.frame(cbind(clm[["SoilMoist"]][,s], ed[["SoilMoist"]][,s], lpj.g[["SoilMoist"]][,s], lpj.w[["SoilMoist"]][,s], c(jules.s[["SoilMoist"]][,s], rep(NA, 12))))  
  Qs[[s]] <- data.frame(cbind(clm[["Qs"]][,s], ed[["Qs"]][,s], lpj.g[["Qs"]][,s], lpj.w[["Qs"]][,s], c(jules.s[["Qs"]][,s], rep(NA, 12))))  
    
  names(HeteroResp[[s]]) <- names(AutoResp[[s]]) <- names(GPP[[s]]) <- names(SoilMoist[[s]]) <- names(Qs[[s]]) <- c("clm45", "ed2", "lpj.guess", "lpj.wsl", "jules.stat")

  # -------------------------------------
  # Missing JULES_STATIC  
  NEE[[s]] <- data.frame(cbind(clm[["NEE"]][,s], ed[["NEE"]][,s], lpj.g[["NEE"]][,s], lpj.w[["NEE"]][,s]))  
  Evap[[s]] <- data.frame(cbind(clm[["Evap"]][,s], ed[["Evap"]][,s], lpj.g[["Evap"]][,s], lpj.w[["Evap"]][,s]))  
  Transp[[s]] <- data.frame(cbind(clm[["Tranp"]][,s], ed[["Transp"]][,s], lpj.g[["Transp"]][,s], lpj.w[["Tranp"]][,s]))  
   
  names(NEE[[s]]) <- names(Evap[[s]]) <- names(Transp[[s]]) <- c("clm45", "ed2", "lpj.guess", "lpj.wsl")
  
  # -------------------------------------
  # Missing LPJ-WSL
  NPP[[s]] <- data.frame(cbind(clm[["NPP"]][,s], ed[["NPP"]][,s], lpj.g[["NPP"]][,s], c(jules.s[["NPP"]][,s], rep(NA, 12))))   
  LAI.m[[s]] <- data.frame(cbind(clm[["LAI"]][,s], ed[["LAI"]][,s], lpj.g[["LAI"]][,s], c(jules.s[["LAI"]][,s], rep(NA, 12))))  
  SoilTemp[[s]] <- data.frame(cbind(clm[["SoilTemp"]][,s], ed[["SoilTemp"]][,s], lpj.g[["SoilTemp"]][,s], c(jules.s[["SoilTemp"]][,s], rep(NA, 12))))  
  SWE[[s]] <- data.frame(cbind(clm[["SWE"]][,s], ed[["SWE"]][,s], lpj.g[["SWE"]][,s], c(jules.s[["SWE"]][,s], rep(NA, 12))))  


  names(NPP[[s]]) <- names(LAI.m[[s]]) <- names(SoilTemp[[s]]) <- names(SWE[[s]]) <- c("clm45", "ed2", "lpj.guess", "jules.stat")
  
  # -------------------------------------
  # Missing LPJ-WSL and LPJ-GUESS
  SnowDepth[[s]] <- data.frame(cbind(clm[["SnowDepth"]][,s], ed[["SnowDepth"]][,s], c(jules.s[["SnowDepth"]][,s], rep(NA, 12))))  
  
  names(SnowDepth[[s]]) <- c("clm45", "ed2", "jules.stat")
  
}
names(NEE) <- names(NPP) <- names(HeteroResp) <- names(AutoResp) <- names(GPP) <- names(LAI.m) <- 
  names(Evap) <- names(Qs) <- names(SoilMoist) <- names(SoilTemp) <- names(SWE) <- names(SnowDepth) <- names(Transp) <- site.list
summary(NEE)
summary(NEE[[1]]); 
summary(NPP[[1]]); 
summary(HeteroResp[[1]]); 
summary(AutoResp[[1]]); 
summary(GPP[[1]]); 
summary(Evap[[1]]); 
summary(Transp[[1]])
summary(Qs[[1]]); 
summary(SoilMoist[[1]]); 
summary(LAI.m[[1]])
summary(SoilTemp[[1]]); 
summary(SWE[[1]]); 
summary(SnowDepth[[1]])


summary(NEE[[1]])
plot(NEE[[1]][,"ed2"], type="l", ylab="NEE KgC/m2/s", xlab="months since 850-01-01", main=paste(site.list[1], "NEE", sep=": "))
#plot(NEE[[1]][,"clm45"], type="l", ylim=c(-1,1), ylab="NEE KgC/m2", xlab="months since 850-01-01", main=site.list[1])
#lines(NEE[[1]][,"ed2"], col="green3")
lines(NEE[[1]][,"lpj.guess"], col="lightblue", lwd=.5)
lines(NEE[[1]][,"lpj.wsl"], col="red", lwd=.5)
lines(NEE[[1]][,"clm45"], col="green3", lwd=.5)
legend("bottomleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45"), col=c("black", "lightblue", "red", "green3"), lwd=2, bg="white")

summary(NPP[[1]])
plot(NPP[[1]][,"ed2"], type="l", ylab="NPP", xlab="months since 850-01-01", main=paste(site.list[1], "NPP", sep=": "))
lines(NPP[[1]][,"clm45"], col="green3", lwd=0.5)
lines(NPP[[1]][,"lpj.guess"], col="lightblue", lwd=0.5)
lines(NPP[[1]][,"jules.stat"], col="orange3", lwd=0.5)
legend("topleft", legend=c("ED2", "LPJ-GUESS", "CLM45", "JULES_STATIC"), col=c("black", "lightblue", "green3", "orange3"), lwd=2, bg="white")

summary(HeteroResp[[1]])
plot(HeteroResp[[1]][,"ed2"], type="l", ylab="Heterotrophic Respiration", xlab="months since 850-01-01", main=paste(site.list[1], "HeteroResp", sep=": "))
lines(HeteroResp[[1]][,"jules.stat"], col="orange3", lwd=0.5)
lines(HeteroResp[[1]][,"ed2"], col="black", lwd=0.5)
lines(HeteroResp[[1]][,"clm45"], col="green3", lwd=0.5)
lines(HeteroResp[[1]][,"lpj.wsl"], col="red", lwd=0.5)
lines(HeteroResp[[1]][,"lpj.guess"], col="lightblue", lwd=0.5)
legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "lightblue", "red", "green3", "orange3"), lwd=2, bg="white")

summary(AutoResp[[1]])
plot(AutoResp[[1]][,"ed2"], type="l", ylab="Autotrophic Respiration", xlab="months since 850-01-01", main=paste(site.list[1], "AutoResp", sep=": "))
lines(AutoResp[[1]][,"lpj.wsl"], col="red", lwd=0.3)
#lines(AutoResp[[1]][,"ed2"], col="black", lwd=0.5)
lines(AutoResp[[1]][,"lpj.guess"], col="lightblue", lwd=0.5)
lines(AutoResp[[1]][,"clm45"], col="green3", lwd=0.4)
lines(AutoResp[[1]][,"jules.stat"], col="orange3", lwd=0.4)
legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "lightblue", "red", "green3", "orange3"), lwd=2, bg="white")

summary(Evap[[1]])
plot(Evap[[1]][,"ed2"], type="l", ylab="Evap KgC/m2/s", xlab="months since 850-01-01", main=paste(site.list[1], "Evaporation", sep=": "), ylim=range(Evap[[1]]))
lines(-Evap[[1]][,"lpj.wsl"], col="red", lwd=.5)
lines(Evap[[1]][,"clm45"], col="green3", lwd=.5)
lines(Evap[[1]][,"lpj.guess"], col="lightblue", lwd=.5)
legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45"), col=c("black", "lightblue", "red", "green3"), lwd=2, bg="white")

summary(Transp[[1]])
plot(Transp[[1]][,"ed2"], type="l", ylab="Transp KgC/m2/s", xlab="months since 850-01-01", main=paste(site.list[1], "Transp", sep=": "))
lines(Transp[[1]][,"clm45"], col="green3", lwd=.5)
lines(Transp[[1]][,"lpj.wsl"], col="red", lwd=.5)
lines(Transp[[1]][,"lpj.guess"], col="lightblue", lwd=.5)
lines(Transp[[1]][,"ed2"], col="black", lwd=.5)
legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45"), col=c("black", "lightblue", "red", "green3"), lwd=2, bg="white")

summary(Qs[[1]]) # NOTE: ED is small because I had pretty much turned it off!
plot(Qs[[1]][,"ed2"], type="l", ylab="Qs KgC/m2/s", xlab="months since 850-01-01", ylim=range(Qs, na.rm=T), col="black", lwd=.5, main=paste(site.list[1], "Runoff", sep=": "))
lines(Qs[[1]][,"lpj.wsl"], col="red", lwd=.5)
lines(Qs[[1]][,"lpj.guess"], col="lightblue", lwd=.5)
lines(Qs[[1]][,"clm45"], col="green3", lwd=.3)
lines(Qs[[1]][,"jules.stat"], col="orange3", lwd=0.5)
lines(Qs[[1]][,"ed2"], col="black", lwd=0.5)
legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "lightblue", "red", "green3", "orange3"), lwd=2, bg="white")

summary(SoilMoist[[1]]) # NOTE: ED is wrong currently
#plot(SoilMoist[[1]][,"ed2"], type="l", ylim=c(0,1), ylab="SoilMoist Kg/m3", xlab="months since 850-01-01", main=site.list[1])
plot(SoilMoist[[1]][,"clm45"], type="l", ylim=range(SoilMoist[[1]], na.rm=T), ylab="Soil Moist KgC/m2", xlab="months since 850-01-01", col="green3", lwd=.5, main=paste(site.list[1], "SoilMoist", sep=": "))
lines(SoilMoist[[1]][,"jules.stat"], col="orange3", lwd=0.3)
lines(SoilMoist[[1]][,"clm45"], col="green3", lwd=.3)
lines(SoilMoist[[1]][,"lpj.guess"], col="lightblue", lwd=.5)
lines(SoilMoist[[1]][,"ed2"], col="black", lwd=.3)
lines(SoilMoist[[1]][,"lpj.wsl"], col="red", lwd=.3)
legend("topleft", legend=c("ED2", "LPJ-GUESS", "LPJ-WSL", "CLM45", "JULES_STATIC"), col=c("black", "lightblue", "red", "green3", "orange3"), lwd=2, bg="white")


summary(LAI.m[[1]])
plot(LAI.m[[1]][,"ed2"], type="l", ylab="Leaf Area Index", xlab="months since 850-01-01", main=paste(site.list[1], "Monthly LAI", sep=": "))
lines(LAI.m[[1]][,"clm45"], col="green3", lwd=0.5)
lines(LAI.m[[1]][,"ed2"], col="black", lwd=0.5)
lines(LAI.m[[1]][,"jules.stat"], col="orange3", lwd=0.4)
lines(LAI.m[[1]][,"lpj.guess"], col="lightblue", lwd=0.5)
legend("topleft", legend=c("ED2", "LPJ-GUESS", "CLM45", "JULES_STATIC"), col=c("black", "lightblue", "green3", "orange3"), lwd=2, bg="white")


summary(SoilTemp[[1]])
plot(SoilTemp[[1]][,"lpj.guess"], col="lightblue", type="l", ylab="SoilTemp K", xlab="months since 850-01-01", main=paste(site.list[1], "SoilTemp", sep=": "))
lines(SoilTemp[[1]][,"jules.stat"], col="orange3", lwd=.4)
lines(SoilTemp[[1]][,"ed2"], col="black", lwd=.35)
lines(SoilTemp[[1]][,"clm45"], col="green3", lwd=.3)
legend("topleft", legend=c("ED2", "LPJ-GUESS", "CLM45"), col=c("black", "lightblue", "green3"), lwd=2, bg="white")


summary(SWE[[1]])
plot(SWE[[1]][,"ed2"], col="black", type="l", ylab="SWE", xlab="months since 850-01-01", ylim=range(SWE[[1]][,c("ed2", "jules.stat")]*1.5, na.rm=T), main=paste(site.list[1], "SWE", sep=": "))
lines(SWE[[1]][,"clm45"], col="green3", lwd=.3)
lines(SWE[[1]][,"ed2"], col="black", lwd=.4)
lines(SWE[[1]][,"jules.stat"], col="orange3", lwd=.5)
legend("topleft", legend=c("ED2", "CLM45", "JULES_STATIC"), col=c("black", "green3", "orange3"), lwd=2, bg="white")

summary(SnowDepth[[1]])
plot(SnowDepth[[1]][,"ed2"], col="black", type="l", ylab="SnowDepth", xlab="months since 850-01-01", ylim=range(SnowDepth[[1]][,c("ed2", "jules.stat")]*1.5, na.rm=T), main=paste(site.list[1], "SnowDepth", sep=": "))
lines(SnowDepth[[1]][,"clm45"], col="green3", lwd=.3)
#lines(SnowDepth[[1]][,"ed2"], col="black", lwd=.4)
lines(SnowDepth[[1]][,"jules.stat"], col="orange3", lwd=.5)
legend("topleft", legend=c("ED2", "CLM45", "JULES_STATIC"), col=c("black", "green3", "orange3"), lwd=2, bg="white")


