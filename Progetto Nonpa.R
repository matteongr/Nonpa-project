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
data <- cbind(data.frame(coord), data[, -c(1, 2)])

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



final_data <- read.csv("data_years.csv")
final_data <- final_data[, c(2, 3, 73, 74, 9, 75, 10:19, 21, 64, 72)]



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
top_schools <- final_data[order(diff_tot, decreasing = TRUE), ][1:20, ]

#most worsened schools
bad_schools <- final_data[order(diff_tot, decreasing = F), ][1:20, ]


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


# plot the most improved and worsened schools on the map of Bogotà
bogota.map +
  geom_point(
    data = top_schools,
    aes(x = X, y = Y, color = "Most improved"),
    size = 1
) +
  geom_point(
    data = bad_schools,
    aes(x = X, y = Y, color = "Most worsened"),
    size = 1
) +
  scale_color_manual(
    name = "Schools Performance",
    values = c("blue", "red"),
    labels = c("Most improved", "Most worsened")
  )


#plot top schools and bad schools on the map with points big according to the difference in puntaje
bogota.map +
  geom_point(
    data = top_schools,
    aes(x = X, y = Y, size = P_Puntaje_2022 - P_Puntaje_2019, color = "Most improved"),
    alpha = 1
  ) +
  geom_point(
    data = bad_schools,
    aes(x = X, y = Y, size = P_Puntaje_2019 - P_Puntaje_2022, color = "Most worsened"),
    alpha = 1
  ) +
  scale_color_manual(
    name = "Schools Performance",
    values = c("blue", "red"),
    labels = c("Most improved", "Most worsened")
  ) +
  scale_size_continuous(
    name = "Difference in puntaje"
  )

