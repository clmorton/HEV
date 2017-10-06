#Load Packages; use the function instal.package() if a first instal is needed
library(spdep)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(tmap)

#Load Data
setwd("~/GitHub/HEV")
SpatDat <- readOGR("Shapefiles","LAD_DEC_2012_GB_BFE_linked")
summary(SpatDat)

#Choropleth Map of Hybrid Electric Vehicles per '000 cars (ln)
col.ramp <- brewer.pal(n = 6, name = "OrRd")
spplot(SpatDat,'HEVln', col = "transparent", main = "Hybrid Electric Vehicles per '000 cars (ln)")

tm_shape(SpatDat) +
  tm_polygons("HEVln", style="quantile", title="Hybrid Electric Vehicles per '000 cars (ln)", n = 10)
tmap_mode("view")
last_map()

#Create Spatial Weights
continuity.nb <- poly2nb(SpatDat, queen = TRUE)
plot(SpatDat, border = "grey")
plot(continuity.nb, coordinates(SpatDat), add = TRUE, col = "blue")
continuity.listw <- nb2listw(continuity.nb)
summary(continuity.listw)

#Autocorrelation Analysis
moran.test(SpatDat$HEVln,continuity.listw, randomisation=FALSE, alternative="two.sided")
moran.plot(SpatDat$HEVln, continuity.listw)
LMIresult <- localmoran(SpatDat$HEVln, continuity.listw)
LMImap <- spCbind(SpatDat, as.data.frame(LMIresult))
spplot(LMImap, "Z.Ii")

#Spatial log-log Regression Models

#Spatial Lag Model
mod7SLM <- lagsarlm(HEVln ~ MeanAgeln + Level4ln + MeanY1kln + OneCarln + DrivWorkln + PopDln
                    + MeanHSizel + CCars1kln + PropDLCCln, 
                    data = SpatDat, continuity.listw)
summary(mod7SLM)

#Spatial Error Model
mod7SEM <- errorsarlm(HEVln ~ MeanAgeln + Level4ln + MeanY1kln + OneCarln + DrivWorkln + PopDln
                      + MeanHSizel + CCars1kln + PropDLCCln, 
                      data = SpatDat, continuity.listw)
summary(mod7SEM)

#Spatial Durbin Model
mod7SDM <- lagsarlm(HEVln ~ MeanAgeln + Level4ln + MeanY1kln + OneCarln + DrivWorkln + PopDln
                    + MeanHSizel + CCars1kln + PropDLCCln, 
                    data = SpatDat, continuity.listw, type = "mixed")
summary(mod7SDM)
W <- as(continuity.listw, "CsparseMatrix")
trMatc <- trW(W, type = "mult")
summary(impacts(mod7SDM, tr=trMatc, R=200), zstats=TRUE, short = TRUE)

#Spatial Durbin Error Model
mod7SDEM <- errorsarlm(HEVln ~ MeanAgeln + Level4ln + MeanY1kln + OneCarln + DrivWorkln + PopDln
                       + MeanHSizel + CCars1kln + PropDLCCln, 
                      data = SpatDat, continuity.listw, etype = "emixed")
summary(mod7SDEM)
summary(impacts(mod7SDEM))

#Simultaneous Autoregressive Model
mod7SAR <- spautolm(HEVln ~ MeanAgeln + Level4ln + MeanY1kln + OneCarln + DrivWorkln + PopDln
                    + MeanHSizel + CCars1kln + PropDLCCln, 
                    data = SpatDat, listw = continuity.listw, family = "SAR", method = "eigen")
summary(mod7SAR)
mod7resSAR <- MCMCsamp(mod7SAR, mcmc = 5000, burnin = 500, listw = continuity.listw)
summary(mod7resSAR)

#Conditional Autoregressive Model
mod7CAR <- spautolm(HEVln ~ MeanAgeln + Level4ln + MeanY1kln + OneCarln + DrivWorkln + PopDln
                    + MeanHSizel + CCars1kln + PropDLCCln, 
                    data = SpatDat, listw = continuity.listw, family = "CAR", method = "eigen")
summary(mod7CAR)
mod7resCAR <- MCMCsamp(mod7CAR, mcmc = 5000, burnin = 500, listw = continuity.listw)
summary(mod7resCAR)
