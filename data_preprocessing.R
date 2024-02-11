###================= Merge new variables to the original dataset =================###
###===============================================================================###

# Import the necessary datasets
raw_data <- read.csv('PruebasSaber_2021_12.csv')
colegios <- read.csv('Colegios.csv')
aprobacion <- read.csv('taprobacionofupz.csv')
desercion <- read.csv("tdesercionofupz.csv")
density <- read.csv("poblacion-upz-bogota.csv", sep = ";")

## Passing rate ##
# Create a data frame with relevant columns to merge for passing rate
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
desercion$COD_UPZ <- gsub("[^0-9.-]", "", desercion$COD_UPZ) #take only the numeric part of COD_UPZ
df_desercion <- desercion[c("Thombre_UP", "Tmujer_UPZ", "COD_UPZ")]
colnames(df_desercion) <- c("Thombre", "Tmujer", "COD_UPZ")

# Extend the colegios dataset to include dropout rate
colegios <- merge(colegios, df_desercion, by = "COD_UPZ")

df_desercion <- colegios[c("Thombre", "Tmujer", "DANE12_SED")]

# Merge the new columns
raw_data <- merge(raw_data, df_desercion,
                  by.x = "COD_DANE12", by.y = "DANE12_SED")

## Socioeconomic status ##
# Create a data frame with relevant columns to merge
df_colegios <- colegios[c("DANE12_SED", "ESTRATO")]

# Merge the new columns
raw_data <- merge(raw_data, df_colegios,
                by.x = "COD_DANE12", by.y = "DANE12_SED")

## Population density per UPZ ##
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
data_2022 <- read.csv("data_2022.csv")
data_2020 <- read.csv("data_2020.csv")
data_2019 <- read.csv("data_2019.csv")
data_2021 <- raw_data

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
write.csv(data_full, file = "Data/extended_data.csv", row.names = F)

# Remove some columns to have a clean data and export it
clean_cols <- c("COD_DANE12", "COLEGIO_SE", "X", "Y", "P_Puntaje_2021", "EVALUADOS_2021",
                "Sector", "CALENDARIO", "GENERO", "COD_LOCA", "NATU_JUR", "CLASE_TIPO",
                "Categoria", "Thombre_aprob", "Tmujer_aprob", "Thombre", "Tmujer",
                "ESTRATO", "DENSITY", "P_Puntaje_2019", "EVALUADOS_2019",
                "P_Puntaje_2020", "EVALUADOS_2020","P_Puntaje_2022", "EVALUADOS_2022")
clean_data <- data_full[clean_cols]
write.csv(clean_data, file = "Data/extended_data_clean.csv", row.names = F)

###===============================================================================###
###===============================================================================###



