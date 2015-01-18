
# Doing some EDA to make sure the ED runs are at least somewhat on par with others
library(ncdf4)
#setwd("~/Dropbox/PalEON CR/PalEON_MIP_Site/phase1a_model_output")

# ------------------------------------------------
# Setting up to compare the inital data from the models
# ------------------------------------------------
model.dir <- "~/Dropbox/PalEON CR/PalEON_MIP_Site/phase1a_model_output"

# Models for which we have data
model.list <- dir(model.dir)

# Sites
site.list <- c("PHA", "PHO", "PUN", "PBL", "PDL", "PMB") # Removing PHA for now until I get it working with ED

# useful numbers
yr2sec <- 1/(365*24*60*60)
mo2sec <- 1/(12*24*60*60)

# ------------------------------------------------------------------------
# Extracting Variables names to make life easier
# ------------------------------------------------------------------------
# Setting up directories to pull an example file
dir.ed <- file.path(model.dir, "ED2", site.list[1])
files.ed <- dir(dir.ed)                    

dir.clm <- file.path(model.dir, "CLM45", paste(site.list[1], "CLM45", sep="."))
files.clm <- dir(dir.clm)

dir.lpj.g <- file.path(model.dir, "LPJ-GUESS", paste(site.list[1], "LPJ-GUESS", sep="_"))
files.lpj.g <- dir(dir.lpj.g)
index <- gregexpr("month",files.lpj.g[2])[[1]][1] # LPJ-GUESS has separate annual and monthly files & we just want the monthly
files.lpj.g.m <- files.lpj.g[substr(files.lpj.g, index, index+4)=="month"]
files.lpj.g.y <- files.lpj.g[substr(files.lpj.g, index, index+5)=="annual"]

dir.lpj.w <- file.path(model.dir, "LPJ-WSL")

# Opening an example file from each model
ed <- nc_open(file.path(dir.ed, files.ed[1]))
clm <- nc_open(file.path(dir.clm, files.clm[2]))
lpj.g.m <- nc_open(file.path(dir.lpj.g, files.lpj.g.m[1]))
lpj.g.y <- nc_open(file.path(dir.lpj.g, files.lpj.g.y[1]))
lpj.w <- nc_open(file.path(dir.lpj.w, paste(site.list[1], "LPJ-wsl.850.nc", sep=".")))

# extracting variable names
ed.var <- names(ed$var)
clm.var <- names(clm$var)
lpj.g.var.m <- names(lpj.g.m$var)
lpj.g.var.y <- names(lpj.g.y$var)[!(names(lpj.g.y$var) %in% lpj.g.var.m)] # only take yearly what we can't get monthly
lpj.g.var <- c(lpj.g.var.m, lpj.g.var.y)
lpj.w.var <- names(lpj.w$var)
# lpj.w.var2 <- c("Fcomp", "AGB", "LAI-PFT", "NPP-PFT", "Diam-PFT", "Height-PFT", "Dens", "Estab-PFT", "TotSoilCarb", "LitterC", "NPP", "HeteroResp", "NEE") # note AGB bc I'm summing it in the loop

# PFT-level variables need to be dealt with slightly differently than single-string variables
var.diversity <- c("BA", "Dens", "Fcomp", "PFT", "fpc", "pft-vegc", "pft-lai", "pft-npp", "pft-diam", "pft-height", "nind", "estrate")

ed.pft <- ncvar_get(ed, "PFT")
lpj.g.pft <- ncvar_get(lpj.g.y, "PFT")
lpj.w.pft <- c("TrBE", "TrBR", "TeNE", "TeBE", "BES", "BNE", "BBS", "C3", "C4")
clm.pft <- c("bare", "TeNE", "BNE", "BNS", "TrBE", "TeBE", "TrBS", "TeBS", "BBS", "SBD", "TeSBS", "BSBS", "C3_arctic", "C3", "C4", "corn", "wheat")

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



# Closing files
nc_close(ed); nc_close(clm); nc_close(lpj.g.m); nc_close(lpj.g.y); nc_close(lpj.w)

# ------------------------------------------------------------------------
# EXTRACTING MODEL OUTPUTS
# ------------------------------------------------------------------------

ed.fcomp <- lpj.g.fcomp <- lpj.w.fcomp <- clm.fcomp <- list()
for(s in 1:length(site.list)){
  #-----------------------------------  
  # ED
  #-----------------------------------
  dir.ed <- file.path(model.dir, "ED2", site.list[s])   
  files.ed <- dir(dir.ed)
  #ed.var.list <- list()
  # File loop extracting time series by variable group
  for(i in 1:length(files.ed)){
    ncMT <- nc_open(file.path(dir.ed, files.ed[i]))
    npft <- length(ncvar_get(ncMT, "PFT"))
    if(i == 1) ed.fcomp[[s]] <- as.data.frame(t(ncvar_get(ncMT, "Fcomp"))) 
    else ed.fcomp[[s]] <- rbind(ed.fcomp[[s]], t(ncvar_get(ncMT, "Fcomp")))
    nc_close(ncMT)      
  }
  
  
   #-----------------------------------
  # CLM45
  #-----------------------------------
  dir.clm <- file.path(model.dir, "CLM45", paste(site.list[s], "CLM45", sep="."))
  files.clm <- dir(dir.clm)  
#  clm.var.list <- list()
  for(i in 1:length(files.clm)){
    ncMT <- nc_open(file.path(dir.clm, files.clm[i]))
    npft <- nrow(ncvar_get(ncMT, "Fcomp"))
    if(i == 1) clm.fcomp[[s]] <- as.data.frame(ncvar_get(ncMT, "Fcomp"))
    else clm.fcomp[[s]] <- rbind(clm.fcomp[[s]], ncvar_get(ncMT, "Fcomp"))
    nc_close(ncMT)      
  }
 
  #-----------------------------------
  # LPJ-Guess
  #-----------------------------------
  dir.lpj.g <- file.path(model.dir, "LPJ-GUESS", paste(site.list[s], "LPJ-GUESS", sep="_"))   
  files.lpj.g <- dir(dir.lpj.g)
  index <- gregexpr("month",files.lpj.g[2])[[1]][1] # LPJ-GUESS has separate annual and monthly files & we just want the monthly
  #files.lpj.g.m <- files.lpj.g[substr(files.lpj.g, index, index+4)=="month"]
  files.lpj.g.y <- files.lpj.g[substr(files.lpj.g, index, index+5)=="annual"]
  
  #lpj.g.var.list <- list()
  for(i in 1:length(files.lpj.g.y)){
    ncMT <- nc_open(file.path(dir.lpj.g, files.lpj.g.y[i]))
    npft <- nrow(ncvar_get(ncMT, "Fcomp"))
    if(i == 1) lpj.g.fcomp[[s]] <- as.data.frame(t(ncvar_get(ncMT, "Fcomp")))
    else lpj.g.fcomp[[s]] <- rbind(lpj.g.fcomp[[s]], t(ncvar_get(ncMT, "Fcomp")))
    nc_close(ncMT)      
  }
  
  #-----------------------------------
  # LPJ-WSL
  #-----------------------------------
  dir.lpj.w <- file.path(model.dir, "LPJ-WSL")
  files.lpj.w <- dir(dir.lpj.w)
#  lpj.w.var.list <- list()
  #-----------------------------------
  # File loop extracting time series by variable group
  ncMT <- nc_open(file.path(dir.lpj.w, paste(site.list[s], "LPJ-wsl.850.nc", sep=".")))
  lpj.w.fcomp[[s]] <- data.frame(ncvar_get(ncMT, "Fcomp"))
  nc_close(ncMT)
  
  
  } # Close the model loop

# Adding site label to each variable
names(ed.fcomp) <- names(lpj.g.fcomp) <- names(lpj.w.fcomp) <- names(clm.fcomp) <- site.list
for(i in 1:length(site.list)){
  names(ed.fcomp[[i]]) <- ed.pft
  names(clm.fcomp[[i]]) <- clm.pft
  names(lpj.g.fcomp[[i]]) <- lpj.g.pft
  names(lpj.w.fcomp[[i]]) <- lpj.w.pft
}
summary(ed.fcomp[[1]])
summary(clm.fcomp[[1]])
summary(lpj.g.fcomp[[1]])
summary(lpj.w.fcomp[[1]])

summary(rowSums(lpj.w.fcomp[[1]]))
summary(rowSums(lpj.g.fcomp[[1]][,1:(ncol(lpj.g.fcomp[[1]])-1)]))
summary(rowSums(clm.fcomp[[1]]))
summary(rowSums(ed.fcomp[[1]]))


# -----------------------------------------------------
# Exploratory Graphing
# -----------------------------------------------------
#-----------------------------------  
# ED
#-----------------------------------
pft.colors.ed <- c("green3", "darkgreen", "darkgoldenrod3", "darkorange3", "red3")
pfts.ed <- c("6-North Pine", "8-Late Conifer", "9-Early Hardwood", "10-Mid Hardwood", "11-Late Hardwood")

par(mfrow=c(1,1))
plot(ed.fcomp[[1]][,6], type="l", ylim=c(0,1), col=pft.colors.ed[1], lwd=3, main=site.list[1])
lines(ed.fcomp[[1]][,c(8)], col=pft.colors.ed[2], lwd=3)
lines(ed.fcomp[[1]][,c(9)], col=pft.colors.ed[3], lwd=3)
lines(ed.fcomp[[1]][,c(10)], col=pft.colors.ed[4], lwd=3)
lines(ed.fcomp[[1]][,c(11)], col=pft.colors.ed[5], lwd=3)
legend("topleft", legend=pfts.ed, col=pft.colors.ed, bg="white", lwd=3)

pdf(width=11, height=8.5, file="ED_Paleon_Prelim_Fcomp.pdf")
par(mfrow=c(3,2), mar=c(3,5,1,1)+.1)
for(i in 1:length(site.list)){
plot(ed.fcomp[[i]][,6], type="l", ylim=c(0,1), col=pft.colors.ed[1], lwd=2, xlab="", ylab="Fraction of AGB")
lines(ed.fcomp[[i]][,c(8)], col=pft.colors.ed[2], lwd=1.5)
lines(ed.fcomp[[i]][,c(9)], col=pft.colors.ed[3], lwd=1.5)
lines(ed.fcomp[[i]][,c(10)], col=pft.colors.ed[4], lwd=1.5)
lines(ed.fcomp[[i]][,c(11)], col=pft.colors.ed[5], lwd=1.5)
text(x=500, y=.95, site.list[i], cex=1.5, font=2)
#legend("topleft", legend=pfts.ed, col=pft.colors.ed, bty="n", lwd=3)
}
plot(-5, type="l", ylim=c(0,1), col=pft.colors.ed[1], lwd=0.0001, xlab="", ylab="Fraction of AGB")
legend("topleft", legend=pfts.ed, col=pft.colors.ed, bty="n", lwd=5, cex=2)
dev.off()

#-----------------------------------  
# LPJ-GUESS
#-----------------------------------
summary(lpj.g.fcomp[[1]])
summary(lpj.g.fcomp[[2]])
summary(lpj.g.fcomp[[3]])
summary(lpj.g.fcomp[[4]])
summary(lpj.g.fcomp[[5]])
summary(lpj.g.fcomp[[6]])

pft.colors.lpj.g <- c("darkgreen", "green3", "black", "darkorange3", "darkgoldenrod3", "red3")
pfts.lpj.g <- c("BNE", "BINE", "BIBS", "TeBS", "TeIBS", "C3G")

par(mfrow=c(1,1))
plot(lpj.g.fcomp[[1]][,"BNE"], type="l", ylim=c(0,1), col=pft.colors.lpj.g[1], lwd=3, main=site.list[1])
lines(lpj.g.fcomp[[1]][,"BINE"], col=pft.colors.lpj.g[2], lwd=3)
lines(lpj.g.fcomp[[1]][,"BIBS"], col=pft.colors.lpj.g[3], lwd=3)
lines(lpj.g.fcomp[[1]][,"TeBS"], col=pft.colors.lpj.g[4], lwd=3)
lines(lpj.g.fcomp[[1]][,"TeIBS"], col=pft.colors.lpj.g[5], lwd=3)
lines(lpj.g.fcomp[[1]][,"C3G"], col=pft.colors.lpj.g[6], lwd=3)
#legend("topleft", legend=pfts.lpj.g, col=pft.colors.lpj.g, bty="n", lwd=3)

pdf(width=11, height=8.5, file="LPJ-GUESS_Paleon_Prelim_Fcomp.pdf")
par(mfrow=c(3,2), mar=c(3,5,1,1)+.1)
for(i in 1:length(site.list)){
  plot(lpj.g.fcomp[[i]][,"BNE"], type="l", ylim=c(0,1), col=pft.colors.lpj.g[1], lwd=3, ylab="Fraction of AGB")
  lines(lpj.g.fcomp[[i]][,"BINE"], col=pft.colors.lpj.g[2], lwd=3)
  lines(lpj.g.fcomp[[i]][,"BIBS"], col=pft.colors.lpj.g[3], lwd=3)
  lines(lpj.g.fcomp[[i]][,"TeBS"], col=pft.colors.lpj.g[4], lwd=3)
  lines(lpj.g.fcomp[[i]][,"TeIBS"], col=pft.colors.lpj.g[5], lwd=3)
  lines(lpj.g.fcomp[[i]][,"C3G"], col=pft.colors.lpj.g[6], lwd=3)
  text(x=500, y=.95, site.list[i], cex=1.5, font=2)
  #legend("topleft", legend=pfts.lpj.g, col=pft.colors.lpj.g, bty="n", lwd=3)
}
plot(-5, type="l", ylim=c(0,1), col=pft.colors.lpj.g[1], lwd=0.0001, xlab="", ylab="Fraction of AGB")
legend("topleft", legend=pfts.lpj.g, col=pft.colors.lpj.g, bty="n", lwd=5, cex=2)
dev.off()



#-----------------------------------  
# LPJ-WSL
#-----------------------------------
summary(lpj.w.fcomp[[1]])
summary(lpj.w.fcomp[[2]])
summary(lpj.w.fcomp[[3]])
summary(lpj.w.fcomp[[4]])
summary(lpj.w.fcomp[[5]])
#summary(lpj.w.fcomp[[6]])

pft.colors.lpj.w <- c("green3", "darkgreen", "black","red3")
pfts.lpj.w <- c("BES", "BNE", "BBS", "C3")

par(mfrow=c(1,1))
plot(lpj.w.fcomp[[1]][,"BES"], type="l", ylim=c(0,1), col=pft.colors.lpj.w[1], lwd=3, main=site.list[1])
lines(lpj.w.fcomp[[1]][,"BNE"], col=pft.colors.lpj.w[2], lwd=3)
lines(lpj.w.fcomp[[1]][,"BBS"], col=pft.colors.lpj.w[3], lwd=3)
lines(lpj.w.fcomp[[1]][,"C3"], col=pft.colors.lpj.w[4], lwd=3)
legend("topleft", legend=pfts.lpj.w, col=pft.colors.lpj.w, bty="n", lwd=3)

pdf(width=11, height=8.5, file="LPJ-GUESS_Paleon_Prelim_Fcomp.pdf")
par(mfrow=c(3,2), mar=c(3,5,1,1)+.1)
for(i in 1:length(site.list)){
  plot(lpj.w.fcomp[[i]][,"BES"], type="l", ylim=c(0,1), col=pft.colors.lpj.w[1], lwd=3, ylab="Fraction of AGB")
  lines(lpj.w.fcomp[[i]][,"BNE"], col=pft.colors.lpj.w[2], lwd=3)
  lines(lpj.w.fcomp[[i]][,"BBS"], col=pft.colors.lpj.w[3], lwd=3)
  lines(lpj.w.fcomp[[i]][,"C3"], col=pft.colors.lpj.w[4], lwd=3)
  text(x=50, y=.95, site.list[i], cex=1.5, font=2)
  #legend("topleft", legend=pfts.lpj.w, col=pft.colors.lpj.w, bty="n", lwd=3)
}
plot(-5, type="l", ylim=c(0,1), col=pft.colors.lpj.w[1], lwd=0.0001, xlab="", ylab="Fraction of AGB")
legend("topleft", legend=pfts.lpj.w, col=pft.colors.lpj.w, bty="n", lwd=5, cex=2)
dev.off()


#-----------------------------------  
# CLM
#-----------------------------------
summary(clm.fcomp[[1]]); summary(rowSums(clm.fcomp[[1]]))
summary(clm.fcomp[[2]]); summary(rowSums(clm.fcomp[[2]]))
summary(clm.fcomp[[3]]); summary(rowSums(clm.fcomp[[3]]))
summary(clm.fcomp[[4]]); summary(rowSums(clm.fcomp[[4]]))
summary(clm.fcomp[[5]]); summary(rowSums(clm.fcomp[[5]]))
summary(clm.fcomp[[6]]); summary(rowSums(clm.fcomp[[6]]))

pft.colors.clm <- c("darkgreen", "green3")
pfts.clm <- c("TeNE", "TeBS")

par(mfrow=c(1,1))
plot(clm.fcomp[[1]][,"TeNE"], type="l", ylim=c(0,1), col=pft.colors.clm[1], lwd=3, main=site.list[1])
lines(clm.fcomp[[1]][,"TeBS"], col=pft.colors.clm[2], lwd=3)
legend("topleft", legend=pfts.clm, col=pft.colors.clm, bty="n", lwd=3)

#pdf(width=11, height=8.5, file="CLM45_Paleon_Prelim_Fcomp.pdf")
par(mfrow=c(3,2), mar=c(3,5,1,1)+.1)
for(i in 1:length(site.list)){
  plot(clm.fcomp[[i]][,"TeNE"], type="l", ylim=c(0,1), col=pft.colors.clm[1], lwd=3, ylab="Fraction of AGB")
  lines(clm.fcomp[[i]][,"TeBS"], col=pft.colors.clm[2], lwd=3)
  text(x=700, y=.9, site.list[i], cex=1.5, font=2)
  #legend("topleft", legend=pfts.clm, col=pft.colors.clm, bty="n", lwd=3)
}
#plot(-5, type="l", ylim=c(0,1), col=pft.colors.clm[1], lwd=0.0001, xlab="", ylab="Fraction of AGB")
legend(x=9000, y=.90, legend=pfts.clm, col=pft.colors.clm, bty="n", lwd=5, cex=2)
#dev.off()
