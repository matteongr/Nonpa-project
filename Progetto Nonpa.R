### PROGETTO NONPARAMETRICA
rm(list = ls())

# CAMBIARE LA WORKING DIRECTORY!
# VEDERE SE QUESTI COMANDI FUNZIONANO AUTOMATICAMENTE
setwd(
  "C:/Users/lucat/OneDrive - Politecnico di Milano/Documenti/universita/HPC/corsi/nonparametric statistics/Project/Nonpa-project"
)


raw_data <- read.csv('PruebasSaber_2021_12.csv')
head(raw_data)
names <- colnames(raw_data)

plot(raw_data$X, raw_data$P_Puntaje_)
plot(raw_data$Y, raw_data$P_Puntaje_)
plot(raw_data$X, raw_data$Y) # plot of the coordinates (vorrei fare una cosa
# più carina)

#Keep only variables of interest
indexes = c(1, 2, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 64)
data <- raw_data[, indexes]

# There are some categorical variables encoded with some numbers:
# GENERO (scuola maschile/femminile/mista):
# 1: female
# 3: male
# 5: misto

# CALENDARIO (quando la scuola inizia)
# 1: A(gennaio)
# 3: B(agosto)
# 2: entrambi
# 5: ?

# SECTOR (non mi ricordo cosa volesse dire)
# 1: No oficial
# 2: Oficial

# CLASE_TIPO
# 1 : comunale
# 2 : comunale
# 6 : privato
# 4 : privato
# 3 : comunale (esercito)
# 5 : privato

# Categoria
# 0 : non applica
# 1 : A+
# 2 : A
# 3 : B
# 4 : C
# 5 : D

# COD_LOCA
# Codificazione delle zone della città
#-----------------------------------------------------------------------------
# Objectives:
# Understand the spatial dependence of schools with the students tests' scores
# Analyise if there is a difference between schools' categories
#

# Workflow:
# Data exploration (boxplots, outliers)
# Nonparametric tests for differences between schools
# Regression (coordinates, numero di studenti, ...) -> prediction

#--------------------------------------------------------------------------
#Provo a disegnare mappa di Bogotà + plot dei punti dove sono situate le scuole
#Carino plottare la città di bogotà ed evidenziare con dei puntini le scuole.

library(sf)
library(ggplot2)

# Import a geojson or shapefile
map <- read_sf("poblacion-upz-bogota.geojson")
# potrebbero non essere sufficienti, perché ci sono scuole fuori dal confine della mappa
bogota.map <- ggplot(map) +
  geom_sf(fill = "white") + theme_void()

x <- data$X
y <- data$Y
points <- cbind(x, y)

# Function to convert EPSG:3857 to WGS84
convert_to_wgs84 <- function(x, y) {
  df <-
    data.frame(x = x, y = y) # Create a data frame with the input coordinates
  points <-
    st_as_sf(df, coords = c("x", "y"), crs = 3857) # Create a simple feature (sf) object
  points_wgs84 <-
    st_transform(points, 4326) # Transform the coordinates to WGS84 (EPSG:4326)
  lonlat <-
    st_coordinates(points_wgs84) # Extract the longitude and latitude
  
  return(lonlat)
}

coord <- convert_to_wgs84(data$X, data$Y)

# Sostituisco al quello che abbiamo
data <- cbind(data.frame(coord), data[,-c(1, 2)])

bbox <-
  st_bbox(map) # vedo quali sono le scuole fuori dal range del file geojson
xmin <- as.numeric(bbox[1])
xmax <- as.numeric(bbox[3])
ymin <- as.numeric(bbox[2])
ymax <- as.numeric(bbox[4])

# RICORDATI CHE STAI SOVRASCRIVENDO QUA DELLA ROBA, STAI TOGLIENDO DEGLI OUTLIERS "SPAZIALI"
# CHE SONO FUORI DAI CONFINI DEL FILE DI GEOJSON --> stiamo levando osservazioni a caso, just per avere plot carini
data <- subset(data, X >= xmin & X <= xmax & Y >= ymin & Y <= ymax)
coord <- as.matrix(data[, 1:2])

library(readr)
write_csv(data, "data.csv") #save "clean" dataset


bogota.map.density <- ggplot() +
  geom_sf(data = map, aes(fill = densidad_urbana)) +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_void()
bogota.map.density

# PLOT OF SCHOOLS
final_map <- bogota.map +
  geom_point(
    data = as.data.frame(coord),
    aes(x = X, y = Y),
    color = "red",
    size = 1
  )
final_map

# PLOT OF SCHOOLS ACCORDING TO A FACTOR


plot.factor.genero <- function(factor) {
  # Create a color palette based on the unique levels of the factor
  col.ramp <- rainbow(length(levels(factor)))
  
  # Create a named vector of colors for each level of the factor
  col.lab <- setNames(col.ramp, levels(factor))
  
  # Create a named vector of labels for each level of the factor
  label_lab <-
    setNames(c("female", "male", "mixed"), c("1", "3", "5"))  # Assigning labels
  
  final_map <- bogota.map +
    geom_point(data = as.data.frame(coord),
               aes(x = X, y = Y, color = factor),
               size = 1) +
    scale_color_manual(name = "GENERO",
                       values = col.lab,
                       labels = label_lab) # Specify labels
  
  final_map
}

data$GENERO <- factor(data$GENERO, levels = c("1", "3", "5"))


plot.factor.calendario <- function(factor) {
  # Create a color palette based on the unique levels of the factor
  col.ramp <- rainbow(length(levels(factor)))
  
  # Create a named vector of colors for each level of the factor
  col.lab <- setNames(col.ramp, levels(factor))
  
  # Create a named vector of labels for each level of the factor
  label_lab <-
    setNames(c("A", "B", "both", "unknown"), c("1", "3", "2", "5"))  # Assigning labels
  
  final_map <- bogota.map +
    geom_point(data = as.data.frame(coord),
               aes(x = X, y = Y, color = factor),
               size = 1) +
    scale_color_manual(name = "CALENDARIO",
                       values = col.lab,
                       labels = label_lab) # Specify labels
  
  final_map
}

data$CALENDARIO <-
  factor(data$CALENDARIO, levels = c("1", "3", "2", "5"))


# Recode values in clase_tipo column
data$CLASE_TIPO <- ifelse(data$CLASE_TIPO %in% c(1, 2, 3), 1, 3)



plot.factor.clase_tipo <- function(factor) {
  # Create a color palette based on the unique levels of the factor
  col.ramp <- rainbow(length(levels(factor)))
  
  # Create a named vector of colors for each level of the factor
  col.lab <- setNames(col.ramp, levels(factor))
  
  # Create a named vector of labels for each level of the factor
  label_lab <-
    setNames(c("public", "private"), c("1", "3"))  # Assigning labels
  
  final_map <- bogota.map +
    geom_point(data = as.data.frame(coord),
               aes(x = X, y = Y, color = factor),
               size = 1) +
    scale_color_manual(name = "CLASE_TIPO",
                       values = col.lab,
                       labels = label_lab) # Specify labels
  
  final_map
}

data$CLASE_TIPO <- factor(data$CLASE_TIPO, levels = c("1", "3"))



plot.factor.cod_loca <- function(factor) {
  # Create a color palette based on the unique levels of the factor
  col.ramp <- rainbow(length(levels(factor)))
  
  # Create a named vector of colors for each level of the factor
  col.lab <- setNames(col.ramp, levels(factor))
  
  final_map <- bogota.map +
    geom_point(data = as.data.frame(coord),
               aes(x = X, y = Y, color = factor),
               size = 1) +
    scale_color_manual(name = "COD_LOCA", values = col.lab) # Specify labels
  final_map
}


data$COD_LOCA <- factor(data$COD_LOCA)

library(ggpubr)
ggarrange(
  plot.factor.genero(data$GENERO),
  plot.factor.calendario(data$CALENDARIO),
  plot.factor.clase_tipo(data$CLASE_TIPO),
  plot.factor.cod_loca(data$COD_LOCA),
  ncol = 2,
  nrow = 2
)



data_years <- read.csv("data_years.csv")
final_data <-
  data_years[, c(2, 3, 73, 74, 9, 75, 10:19, 21, 64, 72)]



# i tried to plot the change between years in puntaje for each school in different localidades

years <- c(2019, 2020, 2021, 2022)
plot.codloca <-
  function(codloca) {
    # function to plot the change in puntaje for each school in a localidad
    plot(
      x = NULL,
      y = NULL,
      xlab = "Year",
      ylab = "Puntaje",
      main = "Puntaje 2019-2022",
      xlim = c(min(years), max(years)),
      ylim = c(
        min(final_data$P_Puntaje_2020) - 10,
        max(final_data$P_Puntaje_2022) + 10
      )
    )
    for (i in 1:nrow(data)) {
      if (data$COD_LOCA[i] == codloca) {
        lines(x = years,
              y = final_data[i, 3:6],
              col = rainbow(nrow(final_data))[i])
      }
    }
  }


plot.codloca(1) # plot of the change in puntaje for each school in localidad 1


# variation between 2019 and 2022
diff_2019_2022 <-
  final_data$P_Puntaje_2022 - final_data$P_Puntaje_2019
hist(diff_2019_2022,
     main = "Difference in puntaje between 2019 and 2022",
     xlab = "Difference in puntaje",
     breaks = 20)

# test to see if the difference in puntaje between 2019 and 2022 is different from 0
wilcox.test(diff_2019_2022, mu = 0, alternative = "two.sided")

# Calculate the differences between consecutive years
diff_2019_2020 <-
  final_data$P_Puntaje_2020 - final_data$P_Puntaje_2019
diff_2020_2021 <-
  final_data$P_Puntaje_2021 - final_data$P_Puntaje_2020
diff_2021_2022 <-
  final_data$P_Puntaje_2022 - final_data$P_Puntaje_2021
diff_tot <- diff_2019_2020 + diff_2020_2021 + diff_2021_2022


wilcox.test(diff_2019_2020, mu = 0, alternative = "two.sided")
wilcox.test(diff_2020_2021, mu = 0, alternative = "two.sided")
wilcox.test(diff_2021_2022, mu = 0, alternative = "two.sided")

#most improved schools
top_schools <-
  final_data[order(diff_tot, decreasing = TRUE),][1:20,]

#most worsened schools
bad_schools <- final_data[order(diff_tot, decreasing = F),][1:20,]


# plot the change in puntaje for the most improved and worsened schools
par(mfrow = c(1, 2))
plot(
  x = NULL,
  y = NULL,
  xlab = "Year",
  ylab = "Puntaje",
  main = "Most improved schools",
  xlim = c(min(years), max(years)),
  ylim = c(
    min(final_data$P_Puntaje_2020) - 10,
    max(final_data$P_Puntaje_2022) + 10
  )
)
for (i in 1:nrow(top_schools)) {
  lines(x = years,
        y = top_schools[i, 3:6],
        col = rainbow(nrow(top_schools))[i])
}
plot(
  x = NULL,
  y = NULL,
  xlab = "Year",
  ylab = "Puntaje",
  main = "Most worsened schools",
  xlim = c(min(years), max(years)),
  ylim = c(
    min(final_data$P_Puntaje_2020) - 10,
    max(final_data$P_Puntaje_2022) + 10
  )
)
for (i in 1:nrow(bad_schools)) {
  lines(x = years,
        y = bad_schools[i, 3:6],
        col = rainbow(nrow(bad_schools))[i])
}
par(mfrow = c(1,1))


#plot top schools and bad schools on the map with points big according to the difference in puntaje
bogota.map +
  geom_point(
    data = top_schools,
    aes(
      x = X,
      y = Y,
      size = P_Puntaje_2022 - P_Puntaje_2019,
      color = "Most improved"
    ),
    alpha = 1
  ) +
  geom_point(
    data = bad_schools,
    aes(
      x = X,
      y = Y,
      size = P_Puntaje_2019 - P_Puntaje_2022,
      color = "Most worsened"
    ),
    alpha = 1
  ) +
  scale_color_manual(
    name = "Schools Performance",
    values = c("blue", "red"),
    labels = c("Most improved", "Most worsened")
  ) +
  scale_size_continuous(name = "Difference in puntaje")



# getting the upz code in our dataset
data_years <- read.csv("data_years.csv")
colegios <- read.csv("colegios.csv")
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


# plot according to upz in a map, every upz value is a different color without legend
plot.factor.upz <- function(factor) {
  # Create a color palette based on the unique levels of the factor
  col.ramp <- rainbow(length(levels(factor)))
  
  # Create a named vector of colors for each level of the factor
  col.lab <- setNames(col.ramp, levels(factor))
  
  final_map <- bogota.map +
    geom_point(data = as.data.frame(data_years[, 2:3]),
               aes(x = X, y = Y, color = factor),
               size = 1) +
    scale_color_manual(name = "UPZ", values = col.lab) +
    theme(legend.position = "none") # Specify labels
  final_map
}

data_years$upz <- factor(data_years$upz)
plot.factor.upz(data_years$upz)

#final data
final_data <- cbind(final_data, data_years$upz)
colnames(final_data)[ncol(final_data)] <- "Upz"



# plot puntaje in years for every school
plot(
  x = NULL,
  y = NULL,
  xlab = "Year",
  ylab = "Puntaje",
  main = ("Puntaje for school"),
  xlim = c(min(years), max(years)),
  ylim = c(
    min(final_data$P_Puntaje_2020) - 10,
    max(final_data$P_Puntaje_2022) + 10
  )
)
for (i in 1:nrow(final_data)) {
  points(x = years,
         y = final_data[i, 3:6])
}

# i think there is anything we can do with this

# plot puntaje in years for every upz
plot(
  x = NULL,
  y = NULL,
  xlab = "Year",
  ylab = "Puntaje",
  main = ("Puntaje for upz"),
  xlim = c(min(years), max(years)),
  ylim = c(
    min(final_data$P_Puntaje_2020) - 10,
    max(final_data$P_Puntaje_2022) + 10
  )
)
for (i in 1:length(levels(data_years$upz))) {
  lines(x = years,
        y = colMeans(final_data[data_years$upz == levels(data_years$upz)[i], 3:6]),
        col = rainbow(length(levels(data_years$upz)))[i])
}


# plot in a map the schools where the means in puntaje per upz is the highest, with the size of the point depending on the average score
upz_means <-
  aggregate(final_data[, 3:6], by = list(final_data$Upz), FUN = mean)
bogota.map +
  geom_point(data = final_data,
             aes(
               x = X,
               y = Y,
               size = rowMeans(final_data[, 3:6]),
               color = Upz
             ),
             alpha = 1) +
  scale_size_continuous(name = "Average puntaje")

# maybe there is a connection between upz and puntaje
# plot the average puntaje per upz
upz_means <- cbind(upz_means, rowMeans(upz_means[, 2:5]))
colnames(upz_means)[ncol(upz_means)] <- "Average_puntaje"
colnames(upz_means)[1] <- "Upz"
plot(
  upz_means[, 1],
  upz_means[, 6],
  xlab = "Upz",
  ylab = "Average puntaje",
  main = "Average puntaje per upz",
  pch = 19
)

# spline interpolation
library(splines)
knots <-
  quantile(as.numeric(upz_means$Upz), probs = c(0.25, 0.5, 0.75))
boundary_knots <-
  quantile(as.numeric(upz_means$Upz), probs = c(0.1, 0.9))

upz_means[, 1] <- as.numeric(upz_means[, 1])
new_data <-
  with(upz_means, data.frame(Upz = seq(range(Upz)[1], range(Upz)[2], by = 0.05)))

model_ns = lm(Average_puntaje ~ ns(Upz, knots = knots, Boundary.knots = boundary_knots),
              data = upz_means)
preds = predict(model_ns, new_data, se = T)
se.bands = cbind(preds$fit + 2 * preds$se.fit , preds$fit - 2 * preds$se.fit)

with(upz_means,
     plot(
       Upz ,
       Average_puntaje,
       xlim = range(upz_means$Upz) ,
       cex = .5,
       col = " darkgrey "
     ))
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
  if (model_ns$fitted.values[i] < 280) {
    upz_low <- c(upz_low, upz_means[i, 1])
  }
}


# plot the average puntaje per upz in a map with color gradient from blu for upz_lim to red for the others

bogota.map +
  geom_point(data = final_data,
             aes(
               x = X,
               y = Y,
               color = ifelse(!(Upz %in% upz_low), "blue", "red")
             ),
             alpha = 1) +
  scale_color_manual(name = "Upz", values = c("blue", "red"))


