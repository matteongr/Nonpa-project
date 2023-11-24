### PROGETTO NONPARAMETRICA
# CAMBIARE LA WORKING DIRECTORY!
setwd("C:/Users/HP/Desktop/Progetto Nonpa")

raw_data <- read.csv('PruebasSaber_2021_12.csv')
head(raw_data)
names <- colnames(raw_data)

plot(raw_data$X, raw_data$P_Puntaje_)
plot(raw_data$Y, raw_data$P_Puntaje_)
plot(raw_data$X, raw_data$Y) # plot of the coordinates (vorrei fare una cosa 
                             # più carina)

#Remove some useless columns
unique(raw_data$Fecha) # there is only one date --> remove it
data <- raw_data[-c(4,5,6)]

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
# 3 : comunale (essercito)
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
#Trovare shapefiles and geojson files. una cosa carina da fare è plottare la città di bogotà ed evidenziare con dei puntini le scuole.
#Problema: non so in che formato sono scritte le coordinate!!!!

#install.packages(c("sf", "ggplot2"))
library(sf)
library(ggplot2)

# Import a geojson or shapefile
map <- read_sf("map.geojson")
ggplot(map) +
  geom_sf(fill = "white")

x <- data$X
y <- data$Y   
points <- cbind(x, y)

# Function to convert EPSG:3857 to WGS84
convert_to_wgs84 <- function(x, y) {
  df <- data.frame(x = x, y = y) # Create a data frame with the input coordinates
  points <- st_as_sf(df, coords = c("x", "y"), crs = 3857) # Create a simple feature (sf) object
  points_wgs84 <- st_transform(points, 4326) # Transform the coordinates to WGS84 (EPSG:4326)
  lonlat <- st_coordinates(points_wgs84) # Extract the longitude and latitude
  
  return(lonlat)
}

coord <- convert_to_wgs84(data$X, data$Y)

# Sostituisco al quello che abbiamo
data <- cbind(data.frame(coord), data[,-c(1,2)])

#-------------------------------------------------------------------------------
# Some plots

