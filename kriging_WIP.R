rm(list=ls())


# set directory
# Get the path to the directory containing the current script
# Set the working directory to the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load libraries
library(npsp)
library(sf)
library(sp)

final_data <- read.csv("Data/final_data.csv")
map <- read_sf("Data/poblacion-upz-bogota.geojson")

map <- as(st_as_sf(map), "Spatial")
summary(map)

attr <- attributes(map)
poly_list <- attr$polygons
proj4string <- attr$proj4string

# Create SpatialPolygons object
border <- SpatialPolygons(poly_list, proj4string = proj4string)
# trend estimation

x <- final_data[,c("X","Y")]
x <- as.matrix(x)
y <- final_data$P_Puntaje_2019
lp <- locpol(x, y, nbin = c(120, 120), h = diag(c(3, 3)))


simage(lp,  slim = range(y), main = "Trend estimates",
       col = jet.colors(256), xlab = "Longitude", ylab = "Latitude", asp = 1)


# bandwidth selection

bin <- binning(x, y)
lp0.h <- h.cv(bin)$h
lp0 <- locpol(bin, h = lp0.h, hat.bin = TRUE)


# Compute semivariogram
svar.bin <- svariso(x, residuals(lp0), nlags = 600, maxlag = 20)

# Perform bandwidth selection
svar.h <- h.cv(svar.bin)$h

svar.np <- np.svar(svar.bin, h = svar.h)
svar.np2 <- np.svariso.corr(lp0, nlags = 600, maxlag = 200, 
                            h = svar.h, plot = FALSE) 

geomod <- np.fitgeo(x = x, y = y,  nbin = c(30, 30), maxlag = 600, svm.resid = T)




















# ------------------------------------------------------------------------------

library(npsp)
library(sp)
summary(precipitation)

spoints(precipitation)

scattersplot(precipitation)

x <- coordinates(precipitation)
y <- precipitation$y
lp <- locpol(x, y, nbin = c(120, 120), h = diag(c(5, 5)))

attr <- attributes(precipitation)
border <- attr$border 

lp <- mask(lp, window = border)

interior <- attr$interior 
slim <- range(y)
col <- jet.colors(256)
simage(lp,  slim = slim, main = "Trend estimates",
       col = col, xlab = "Longitude", ylab = "Latitude", asp = 1)
plot(border, border = "darkgray", lwd = 2, add = TRUE)
plot(interior, border = "lightgray", lwd = 1, add = TRUE)


bin <- binning(x, y, nbin = c(120, 120), window = border)
# lp <- locpol(bin, h = diag(c(5, 5)))
lp2 <- locpol(bin, h = diag(c(2, 2)))
simage(lp2, slim = slim, main = "Trend estimates",
       col = col, xlab = "Longitude", ylab = "Latitude", asp = 1)
plot(border, border = "darkgray", lwd = 2, add = TRUE)
plot(interior, border = "lightgray", lwd = 1, add = TRUE)

bin <- binning(x, y, window = border)
lp0.h <- h.cv(bin)$h
lp0 <- locpol(bin, h = lp0.h, hat.bin = TRUE) 

svar.bin <- svariso(x, residuals(lp0), nlags = 60, maxlag = 20)  
svar.h <- h.cv(svar.bin)$h

svar.np <- np.svar(svar.bin, h = svar.h)
svar.np2 <- np.svariso.corr(lp0, nlags = 60, maxlag = 20, 
                            h = svar.h, plot = FALSE) 

svm0 <- fitsvar.sb.iso(svar.np, dk = 0) 
svm1 <- fitsvar.sb.iso(svar.np2, dk = 0) 
# plot...
plot(svm1, main = "Nonparametric bias-corrected semivariogram\nand fitted models", 
     legend = FALSE, xlim = c(0,max(coords(svar.np2))), 
     ylim = c(0,max(svar.np2$biny, na.rm = TRUE)))
plot(svm0, lwd = c(1, 1), add = TRUE)
plot(svar.np, type = "p", pch = 2, add = TRUE)
# abline(h = c(svm1$nugget, svm1$sill), lty = 3)
# abline(v = 0, lty = 3)
legend("bottomright", legend = c("corrected", 'biased'),
       lty = c(1, 1), pch = c(1, 2), lwd = c(1, 1))



geomod <- np.fitgeo(x, y, nbin = c(30, 30), maxlag = 20, svm.resid = TRUE, window = border)
plot(geomod)





krig.grid <- np.kriging(geomod, ngrid = c(120, 120))
# Plot kriging predictions and kriging standard deviations
old.par <- par(mfrow = c(1,2), omd = c(0.0, 0.98, 0.0, 1))
simage(krig.grid, 'kpred', main = 'Kriging predictions', slim = slim,
       xlab = "Longitude", ylab = "Latitude" , col = col, asp = 1, reset = FALSE)
plot(border, border = "darkgray", lwd = 2, add = TRUE)
plot(interior, border = "lightgray", lwd = 1, add = TRUE)
simage(krig.grid, 'ksd', main = 'Kriging sd', xlab = "Longitude", 
       ylab = "Latitude" , col = hot.colors(256), asp = 1, reset = FALSE)
plot(border, border = "darkgray", lwd = 2, add = TRUE)
plot(interior, border = "lightgray", lwd = 1, add = TRUE)
par(old.par)
















library(geoR)
map <- as.geodata(final_data)
trend <- trend.spatial(trend = "cte", geodata = map)
variog(geodata = map, coords = map , data = final_data$P_Puntaje_2019, trend = trend)
