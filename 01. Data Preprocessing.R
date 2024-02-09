### DATA PREPROCESSING
rm(list=ls())
setwd(getwd()) #setting the working directory

raw_data <- read.csv('PruebasSaber_2021_12.csv')
head(raw_data)

#Keep only variables of interest
indexes=c(1,2,7,8,9,11,12,13,14,15,16,17,18,19,21,64)
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

data <- raw_data[,indexes]



