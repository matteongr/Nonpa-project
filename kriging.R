rm(list = ls())


# set directory
# Get the path to the directory containing the current script
# Set the working directory to the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load libraries
library(npsp)
library(sf)
library(sp)


extended_data <- read.csv("Data/extended_data_clean.csv")
map <- read_sf("Data/poblacion-upz-bogota.geojson")

map <- as(st_as_sf(map), "Spatial")

# trying to get a sample of my data to reduce singularity
num_rows <- 150
sampled_rows <-
  sample(nrow(extended_data), num_rows, replace = FALSE)
final_data <- extended_data[sampled_rows,]


spatial_data <-
  SpatialPointsDataFrame(
    coords = final_data[, c("X", "Y")],
    data = final_data[, c("P_Puntaje_2019",
                          "P_Puntaje_2020",
                          "P_Puntaje_2021",
                          "P_Puntaje_2022")],
    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
  )

attr <- attributes(spatial_data)
coord <- attr$coords
data <- attr$data


#trend estimation


lp <-
  locpol(
    x = coord,
    y = data$P_Puntaje_2019,
    nbin = c(60, 60),
    h = diag(c(5, 5))
  )
lp <- mask(lp, window = map)

simage(
  lp,
  slim = range(data$P_Puntaje_2019),
  main = "Trend estimates",
  col = jet.colors(256),
  xlab = "Longitude",
  ylab = "Latitude",
  asp = 1
)


# bandwidth selection

bin <- binning(coord, data$P_Puntaje_2019)
lp0.h <- h.cv(bin)$h
lp0 <- locpol(bin, h = lp0.h, hat.bin = TRUE)


# Compute semivariogram
svar.bin <-
  svariso(coord, residuals(lp0), nlags = 60, maxlag = 0.25)

# Perform bandwidth selection
svar.h <- h.cv(svar.bin)$h

svar.np <- np.svar(svar.bin, h = svar.h)
svar.np2 <- np.svariso.corr(
  lp0,
  nlags = 60,
  maxlag = 0.25,
  h = svar.h,
  plot = F
)


svm0 <- fitsvar.sb.iso(svar.np, dk = 0)
svm1 <- fitsvar.sb.iso(svar.np2, dk = 0)
# plot...
plot(
  svm1,
  main = "Nonparametric bias-corrected semivariogram\nand fitted models",
  legend = FALSE,
  xlim = c(0, max(coords(svar.np2))),
  ylim = c(0, max(svar.np2$biny, na.rm = TRUE))
)
plot(
  svar.np,
  type = "p",
  pch = 2,
  add = TRUE,
  col = "gray"
)
plot(svm0,
     lwd = c(1, 1),
     add = TRUE,
     col = "red")
abline(h = c(svm1$nugget, svm1$sill), lty = 3)
abline(v = 0, lty = 3)
legend(
  "bottomright",
  legend = c("corrected", 'biased'),
  lty = c(1, 1),
  pch = c(1, 2),
  lwd = c(1, 1)
)


# automatic modelling
execution_time <- system.time({
  geomod <-
    np.fitgeo(
      x = coord,
      y = data$P_Puntaje_2019,
      nbin = c(60, 60),
      maxlag = 0.25,
      svm.resid = T,
      window = map,
      h = lp0.h
    )
})[["elapsed"]]

print(paste("Execution time:", execution_time, "seconds"))

plot(geomod)


# kriging

krig.grid <- np.kriging(geomod, ngrid = c(150, 150))
# Plot kriging predictions and kriging standard deviations
old.par <- par(mfrow = c(1, 2), omd = c(0.0, 0.98, 0.0, 1))
simage(
  krig.grid,
  'kpred',
  main = 'Kriging predictions',
  slim = range(data$P_Puntaje_2019),
  xlab = "Longitude",
  ylab = "Latitude" ,
  col = jet.colors(256),
  asp = 1,
  reset = FALSE
)
simage(
  krig.grid,
  'ksd',
  main = 'Kriging sd',
  xlab = "Longitude",
  ylab = "Latitude" ,
  col = hot.colors(256),
  asp = 1,
  reset = FALSE
)
par(old.par)
