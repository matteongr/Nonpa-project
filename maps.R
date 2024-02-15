rm(list = ls())


# set directory
# Get the path to the directory containing the current script
# Set the working directory to the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries
library(ggplot2)
library(ggpubr)
library(sf)


data <- read.csv("Data/extended_data_clean.csv")
map <- read_sf("Data/poblacion-upz-bogota.geojson")
# potrebbero non essere sufficienti, perché ci sono scuole fuori dal confine della mappa
bogota.map <- ggplot(map) +
  geom_sf(fill = "white") + theme_void()

bogota.map.density <- ggplot() +
  geom_sf(data = map, aes(fill = densidad_urbana)) +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_void()
bogota.map.density

# PLOT OF SCHOOLS ACCORDING TO GENERO
plot.factor.genero <- function(factor) {
  # Create a color palette based on the unique levels of the factor
  col.ramp <- rainbow(length(levels(factor)))
  
  # Create a named vector of colors for each level of the factor
  col.lab <- setNames(col.ramp, levels(factor))
  
  # Create a named vector of labels for each level of the factor
  label_lab <-
    setNames(c("female", "male", "mixed"), c("1", "3", "5"))  # Assigning labels
  
  final_map <- bogota.map +
    geom_point(data = as.data.frame((data[, 3:4])),
               aes(x = X, y = Y, color = factor),
               size = 1) +
    scale_color_manual(name = "GENERO",
                       values = col.lab,
                       labels = label_lab) # Specify labels
  
  final_map
}

data$GENERO <- factor(data$GENERO, levels = c("1", "3", "5"))

# PLOT OF SCHOOLS ACCORDING TO CALENDARIO
plot.factor.calendario <- function(factor) {
  # Create a color palette based on the unique levels of the factor
  col.ramp <- rainbow(length(levels(factor)))
  
  # Create a named vector of colors for each level of the factor
  col.lab <- setNames(col.ramp, levels(factor))
  
  # Create a named vector of labels for each level of the factor
  label_lab <-
    setNames(c("A", "B", "both", "unknown"), c("1", "3", "2", "5"))  # Assigning labels
  
  final_map <- bogota.map +
    geom_point(data = as.data.frame((data[, 3:4])),
               aes(x = X, y = Y, color = factor),
               size = 1) +
    scale_color_manual(name = "CALENDARIO",
                       values = col.lab,
                       labels = label_lab) # Specify labels
  
  final_map
}

data$CALENDARIO <-
  factor(data$CALENDARIO, levels = c("1", "3", "2", "5"))


# PLOT OF SCHOOLS ACCORDING TO MANAGEMENT TYPE (PUBLIC/PRIVATE)

plot.factor.clase_tipo <- function(factor) {
  # Create a color palette based on the unique levels of the factor
  col.ramp <- rainbow(length(levels(factor)))
  
  # Create a named vector of colors for each level of the factor
  col.lab <- setNames(col.ramp, levels(factor))
  
  # Create a named vector of labels for each level of the factor
  label_lab <-
    setNames(c("public", "private"), c("0", "1"))  # Assigning labels
  
  final_map <- bogota.map +
    geom_point(data = as.data.frame((data[, 3:4])),
               aes(x = X, y = Y, color = factor),
               size = 1) +
    scale_color_manual(name = "CLASE_TIPO",
                       values = col.lab,
                       labels = label_lab) # Specify labels
  
  final_map
}

data$CLASE_TIPO <- factor(data$CLASE_TIPO, levels = c("0", "1"))

# PLOT OF SCHOOLS ACCORDING TO LOCALIDAD (MACROSCOPIC AREAS)
plot.factor.cod_loca <- function(factor) {
  # Create a color palette based on the unique levels of the factor
  col.ramp <- rainbow(length(levels(factor)))
  
  # Create a named vector of colors for each level of the factor
  col.lab <- setNames(col.ramp, levels(factor))
  
  final_map <- bogota.map +
    geom_point(data = as.data.frame((data[, 3:4])),
               aes(x = X, y = Y, color = factor),
               size = 1) +
    scale_color_manual(name = "COD_LOCA", values = col.lab) # Specify labels
  final_map
}

data$COD_LOCA <- factor(data$COD_LOCA)


# plot all the maps
ggarrange(
  plot.factor.genero(data$GENERO),
  plot.factor.calendario(data$CALENDARIO),
  plot.factor.clase_tipo(data$CLASE_TIPO),
  plot.factor.cod_loca(data$COD_LOCA),
  ncol = 2,
  nrow = 2
)

# plot the maps one by one
plot.factor.genero(data$GENERO)
plot.factor.calendario(data$CALENDARIO)
plot.factor.clase_tipo(data$CLASE_TIPO)
plot.factor.cod_loca(data$COD_LOCA)
