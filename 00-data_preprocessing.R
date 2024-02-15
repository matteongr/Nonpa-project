###================= Merge new variables to the original dataset =================###
###===============================================================================###

rm(list = ls())


library(sf)

# Import the necessary datasets
raw_data <- read.csv('Data/data_2021.csv')
colegios <- read.csv('Data/Colegios.csv')
aprobacion <- read.csv('Data/taprobacionofupz.csv')
desercion <- read.csv("Data/tdesercionofupz.csv")
density <- read.csv("Data/poblacion-upz-bogota.csv", sep = ";")

# Private and public schools (transform variable into binary)
Private <- ifelse(raw_data$CLASE_TIPO %in% c(4, 5, 6), 1, 0) #now 1 --> private; 0--> public
raw_data$CLASE_TIPO <- Private

# Select only CALENDARIO A and B
raw_data <- raw_data[which(raw_data$CALENDARIO %in% c(1,3)),]

# Convert coordinates to Lat Long
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

coords <- raw_data[,c(1,2)]
coords <- convert_to_wgs84(coords[,1], coords[,2])
raw_data$X <- coords[,1]
raw_data$Y <- coords[,2]

## Passing rate ##
# Create a data frame with relevant columns to merge for passing rate
aprobacion <- aprobacion[which(!duplicated(aprobacion$COD_UPZ)),]

aprobacion$COD_UPZ <- gsub("[^0-9.-]", "", aprobacion$COD_UPZ) #take only the numeric part of COD_UPZ
df_aprobacion <- aprobacion[c("Thombre_UP", "Tmujer_UPZ", "COD_UPZ")]
colnames(df_aprobacion) <- c("Thombre_aprob", "Tmujer_aprob", "COD_UPZ")

# Extend the colegios dataset to include passing rate
colegios <- merge(colegios, df_aprobacion, by = "COD_UPZ")

df_aprobacion <- colegios[c("Thombre_aprob", "Tmujer_aprob", "DANE12_SED")]

# Merge the new columns
raw_data <- merge(raw_data, df_aprobacion,
                by.x = "COD_DANE12", by.y = "DANE12_SED")

## Dropout rate ##
# Create a data frame with relevant columns to merge for dropout rate
desercion <- desercion[which(!duplicated(desercion$COD_UPZ)),]

desercion$COD_UPZ <- gsub("[^0-9.-]", "", desercion$COD_UPZ) #take only the numeric part of COD_UPZ
df_desercion <- desercion[c("Thombre_UP", "Tmujer_UPZ", "COD_UPZ")]
colnames(df_desercion) <- c("Thombre", "Tmujer", "COD_UPZ")

# Extend the colegios dataset to include dropout rate
colegios <- merge(colegios, df_desercion, by = "COD_UPZ")

df_desercion <- colegios[c("Thombre", "Tmujer", "DANE12_SED")]

# Merge the new columns
raw_data <- merge(raw_data, df_desercion,
                  by.x = "COD_DANE12", by.y = "DANE12_SED", all.x = F)

## Socioeconomic status ##
colegios <- colegios[which(!duplicated(colegios$DANE12_SED)),]

# Create a data frame with relevant columns to merge
df_colegios <- colegios[c("DANE12_SED", "ESTRATO")]

# Merge the new columns
raw_data <- merge(raw_data, df_colegios,
                by.x = "COD_DANE12", by.y = "DANE12_SED")

## Population density per UPZ ##
density <- density[which(!duplicated(density$Código.UPZ)),]

# Create a data frame with relevant columns to merge
df_density <- data.frame("COD_UPZ" = density$Código.UPZ,
                         "DENSITY" = density$Densidad.urbana)

# Extend the colegios dataset to include density
colegios <- merge(colegios, df_density, by = "COD_UPZ")

df_density <- colegios[c("DENSITY", "DANE12_SED")]

# Merge the new columns
raw_data <- merge(raw_data, df_density,
                  by.x = "COD_DANE12", by.y = "DANE12_SED")

## Years ##
# Import datasets
data_2022 <- read.csv("Data/data_2022.csv")
data_2020 <- read.csv("Data/data_2020.csv")
data_2019 <- read.csv("Data/data_2019.csv")
data_2021 <- raw_data

data_2022 <- data_2022[which(!duplicated(data_2022$DANE12_SED)),]
data_2020 <- data_2020[which(!duplicated(data_2020$COD_DANE12)),]
data_2019 <- data_2019[which(!duplicated(data_2019$COD_DANE12)),]
data_2021 <- data_2021[which(!duplicated(data_2021$COD_DANE12)),]

# Change column names of original dataset to differentiate them
colnames(data_2021)[which(colnames(data_2021)== "P_Puntaje_")] <- "P_Puntaje_2021"
colnames(data_2021)[which(colnames(data_2021)== "EVALUADOS")] <- "EVALUADOS_2021"

# Merge all years together
# Variables included: P_Puntaje, EVALUADOS

# Merge 2021 (original) with 2019
data_full <- merge(x = data_2021,
                   y = data_2019[c("COD_DANE12", "P_PUNTAJE", "EVALUADOS")],
                   by = "COD_DANE12")
# Change new column names as to include the year
colnames(data_full)[which(colnames(data_full)== "P_PUNTAJE")] <- "P_Puntaje_2019"
colnames(data_full)[which(colnames(data_full)== "EVALUADOS")] <- "EVALUADOS_2019"

# Merge with 2020
data_full <- merge(x = data_full,
                   y = data_2020[c("COD_DANE12", "P_Puntaje_", "EVALUADOS")],
                   by = "COD_DANE12")
# Change new column names as to include the year
colnames(data_full)[which(colnames(data_full)== "P_Puntaje_")] <- "P_Puntaje_2020"
colnames(data_full)[which(colnames(data_full)== "EVALUADOS")] <- "EVALUADOS_2020"

# Merge with 2022
data_full <- merge(x = data_full,
                   y = data_2022[c("COLEGIO_SE", "P_Puntaje_", "EVALUADOS")],
                   by.x = "COD_DANE12",
                   by.y = "COLEGIO_SE")
# Change new column names as to include the year
colnames(data_full)[which(colnames(data_full)== "P_Puntaje_")] <- "P_Puntaje_2022"
colnames(data_full)[which(colnames(data_full)== "EVALUADOS")] <- "EVALUADOS_2022"

# Finally remove duplicates and export the extended data set
data_full <- data_full[!duplicated(data_full$COD_DANE12),]
# write.csv(data_full, file = "Data/extended_data.csv", row.names = F)

# Remove some columns to have a clean data and export it
clean_cols <- c("COD_DANE12", "COLEGIO_SE", "X", "Y", "P_Puntaje_2021", "EVALUADOS_2021",
                "CALENDARIO", "GENERO", "COD_LOCA", "CLASE_TIPO",
                "Thombre_aprob", "Tmujer_aprob", "Thombre", "Tmujer",
                "DENSITY", "P_Puntaje_2019", "EVALUADOS_2019",
                "P_Puntaje_2020", "EVALUADOS_2020","P_Puntaje_2022", "EVALUADOS_2022")
clean_data <- data_full[clean_cols]
write.csv(clean_data, file = "Data/extended_data_clean.csv", row.names = F)



###===============================Description of variables========================###
###===============================================================================###


# COD_DANE12 --> unique identifier of the school
# COLEGIO_SE --> name of the school
# X,Y --> latitude and longitude
# P_Puntaje_ --> average score of the school
# EVALUADOS --> number of students evaluated
# CALENDARIO --> when the school begins (A FOR JANUARY or B FOR AUGUST)
# GENERO --> what type of school (1 FOR ALL-GIRLS, 3 FOR ALL-BOYS, 5 FOR MIXED)
# COD_LOCA --> code referring to one of the neighborhood in the city
# CLASE_TIPO --> type of school (1 FOR PRIVATE, 0 FOR PUBLIC)
# Thombre_aprob, Tmujer_aprob --> passing rate of the school
# Thombre, Tmujer --> dropout rate of the school
# DENSITY --> population density of the neighborhood per upz code

