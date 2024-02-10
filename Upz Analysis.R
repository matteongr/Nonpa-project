rm(list = ls())

library(splines)
library(ggpubr)
library(sf)


data_years <- read.csv("data_years.csv")
colegios <- read.csv("colegios.csv")
map <- read_sf("poblacion-upz-bogota.geojson")

bogota.map <- ggplot(map) +
  geom_sf(fill = "white") + theme_void()


# Initialize a vector to store the values of upz
upz_values <- numeric(nrow(data_years))

# Loop over each row of data_years
for (i in 1:nrow(data_years)) {
  # Check if COD_DANE12 is in colegios$DANE12_EST
  if (data_years$COD_DANE12[i] %in% colegios$DANE12_EST) {
    # If it is, get the index where the condition is true
    idx <- which(colegios$DANE12_EST == data_years$COD_DANE12[i])
    # Assign the corresponding COD_UPZ value to the correct index in upz_values
    upz_values[i] <- as.numeric(colegios$COD_UPZ[idx])
  }
}


# Add the upz_values vector as a new column named "upz" to data_years
data_years <- cbind(data_years, upz = upz_values)

final_data <-
  data_years[, c(2, 3, 73, 74, 9, 75, 10:19, 76, 21, 64, 72)]

upz_means <-
  aggregate(final_data[, 3:6], by = list(final_data$upz), FUN = mean)
colnames(upz_means)[1] <- "Upz"


# plot the puntaje per upz for each year
par(mfrow = c(2, 2))
for (i in 2:5) {
  plot(
    upz_means$Upz,
    upz_means[, i],
    xlab = "Upz",
    ylab = "Puntaje",
    main = colnames(upz_means)[i]
  )
}

bogota.map.bad <-
  list(
    "2019" = bogota.map,
    "2020" = bogota.map,
    "2021" = bogota.map,
    "2022" = bogota.map
  )



performance <- function(year) {
  if (year < 2019 | year > 2022) {
    stop("Year must be between 2019 and 2022")
  }
  # Define variables based on the year
  Puntaje <- switch(
    as.character(year),
    "2019" = upz_means$P_Puntaje_2019,
    "2020" = upz_means$P_Puntaje_2020,
    "2021" = upz_means$P_Puntaje_2021,
    "2022" = upz_means$P_Puntaje_2022
  )
  
  
  map_name <- as.character(year)
  # spline interpolation
  knots <-
    quantile(as.numeric(upz_means$Upz), probs = c(0.25, 0.5, 0.75))
  boundary_knots <-
    quantile(as.numeric(upz_means$Upz), probs = c(0.05, 0.95))
  
  new_data <-
    with(upz_means, data.frame(Upz = seq(range(Upz)[1], range(Upz)[2], by = 0.1)))
  
  model_ns = lm(Puntaje ~ ns(Upz, knots = knots, Boundary.knots = boundary_knots),
                data = upz_means)
  preds = predict(model_ns, new_data, se = T)
  se.bands = cbind(preds$fit + 2 * preds$se.fit , preds$fit - 2 * preds$se.fit)
  
  plot(
    upz_means$Upz ,
    Puntaje,
    xlim = range(upz_means$Upz),
    cex = 0.5,
    col = "darkgrey",
    xlab = "Upz",
    ylab = "Puntaje",
    main = paste("Puntaje per upz in", year)
  )
  lines(new_data$Upz, preds$fit , lwd = 2, col = " blue")
  matlines(new_data$Upz,
           se.bands ,
           lwd = 1,
           col = " blue",
           lty = 3)
  
  
  # gettin upz codes for school under 280 in scores according to model_ns
  # initialize upz_low
  upz_low <- c()
  for (i in 1:nrow(upz_means)) {
    fit <- model_ns$fitted.values[i]
    if (model_ns$fitted.values[i] < 280) {
      upz_low <- c(upz_low, upz_means[i, 1])
    }
  }
  

    # plot the average puntaje per upz in a map with color gradient from blu for upz_lim to red for the others
  bogota.map.bad[[map_name]] <- bogota.map.bad[[map_name]] +
    geom_point(data = final_data,
               aes(
                 x = X,
                 y = Y,
                 color = ifelse(!(upz %in% upz_low), "blue", "red")
               ),
               alpha = 1) +
    scale_color_manual(name = "Upz", values = c("blue", "red"))
  
  
  
}

par(mfrow = c(2, 2))
for (i in 2019:2022) {
  performance(i)
}

ggarrange(
  bogota.map.bad[["2019"]],
  bogota.map.bad[["2020"]],
  bogota.map.bad[["2021"]],
  bogota.map.bad[["2022"]],
  ncol = 2,
  nrow = 2
)
