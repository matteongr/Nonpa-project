### PROGETTO NONPARAMETRICA
rm(list = ls())

# remember to change the working directory
setwd(
  "C:/Users/lucat/OneDrive - Politecnico di Milano/Documenti/universita/HPC/corsi/nonparametric statistics/Project/Nonpa-project"
)

raw_data <- read.csv('PruebasSaber_2021_12.csv')
head(raw_data)
names <- colnames(raw_data)

#Keep only variables of interest
indexes = c(1, 2, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 64)
data <- raw_data[, indexes]
# in particular these variables are
# (1,2) --> latitude and longitude
# 7 --> name of the school
# 8 --> P_Puntaje_
# 9 --> Number of graduates
# (11,12,13,14,15) --> average grades of individual subjects
# 16 --> Sector
# 17 --> Calendario
# 18 --> Genero
# 19 --> Cod_loca
# 21 --> Clase_Tipo
# 64 --> Categoria

# There are some categorical variables encoded with some numbers:
# GENERO:
# 1: all-girls school
# 3: all-boys school
# 5: mixed genders school

# CALENDARIO (when the school begins)
# 1: A (in January)
# 3: B (in August)
# 2: both
# 5: ?

# SECTOR (which program does the school follow)
# 1: Not official program
# 2: Official program

# CLASE_TIPO
# 1,2,3: public school
# 4,5,6: private school

# Categoria
# 0 : non applica
# 1 : A+
# 2 : A
# 3 : B
# 4 : C
# 5 : D

# COD_LOCA: codes referring to one of the neighborhood in the city

# plot(raw_data$X, raw_data$P_Puntaje_)
# plot(raw_data$Y, raw_data$P_Puntaje_)
# plot(raw_data$X, raw_data$Y) # plot of the coordinates (vorrei fare una cosa
# più carina)

#--------------------------------------------------------------------------
# How are the schools located in the city of Bogotà?
# We retrieved from a github repo the file .geojson in order to plot a nice plot
# Superimposed to this plot we are plotting as dots the schools using the coordinates
# Following up there is a function converting the raw coordinates into coordinates compatible with the .geojson file

library(sf)
library(ggplot2)

# AGGIUNGERE MAPPA MACRO
map_macro <- read_sf("map.geojson")

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

coord <- convert_to_wgs84(data$X, data$Y) # coordinates in degrees

# Substitute
data <- cbind(data.frame(coord), data[,-c(1, 2)])


# RICORDATI CHE STAI SOVRASCRIVENDO QUA DELLA ROBA, STAI TOGLIENDO DEGLI OUTLIERS "SPAZIALI"
# CHE SONO FUORI DAI CONFINI DEL FILE DI GEOJSON 

bbox <- st_bbox(map) # vedo quali sono le scuole fuori dal range del file geojson
xmin <- as.numeric(bbox[1])
xmax <- as.numeric(bbox[3])
ymin <- as.numeric(bbox[2])
ymax <- as.numeric(bbox[4])

data <- subset(data, X >= xmin & X <= xmax & Y >= ymin & Y <= ymax)
coord <- as.matrix(data[, 1:2])

bogota.map <- ggplot(map_macro) +
  geom_sf(fill = "white") + theme_void()

final_map <- bogota.map +
  geom_point(data = as.data.frame(coord), aes(x = X, y = Y, color= data$P_Puntaje_), size = 0.15)+
  scale_color_gradient(low="red", high="green", name="Scores")+
  theme(legend.position="right", axis.text.x = element_text(size = 7))
final_map

# We noticed some spatial dependence, so we would like to investigate further.
# Localidades are too wide areas, then we would like to narrow them down --> UPZ CODES
# In colegios.csv we have this information, which we ad to the dataset. 

# ADD SOME SOCIO-ECONOMICAL VARIABLES (dropout rate--> per upz; estrato --> per school)
# other plots of maps using UPZ codes + aggiungiamo density population by upz
data_dropout <- read.csv('Desercion.csv') #dropout rate
data_estrato <- read.csv('Colegios.csv') #estrato
# data_density <- read.csv('poblacion-upz-bogota.xlsx') --> DA AGGIUNGERE EVENTUALMENTE

# CODICE DA SEB 

data_1 <- 
library(readr)
write_csv(data_1, "data_1.csv") #save "clean" dataset for first tests

#-----------------------------------------------------------------------------
# maps with narrower "grid"
# Import a geojson or shapefile
map <- read_sf("poblacion-upz-bogota.geojson")
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
    geom_point(data = as.data.frame(coord),
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


# PLOT OF SCHOOLS ACCORDING TO MANAGEMENT TYPE (PUBLIC/PRIVATE)
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

# PLOT OF SCHOOLS ACCORDING TO LOCALIDAD (MACROSCOPIC AREAS)
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

# END FIRST FILE 

# BEGIN NEW FILE
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


# plot in a map the schools where the means in puntaje per upz is the highest,
#with the size of the point depending on the average score
upz_means <-
  aggregate(final_data[, 3:6], by = list(final_data$Upz), FUN = mean)
#change name of first column
colnames(upz_means)[1] <- "Upz"

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

