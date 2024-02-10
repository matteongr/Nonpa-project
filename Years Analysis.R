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

# maybe there is a connection between upz and puntaje
# plot the average puntaje per upz
upz_means <- cbind(upz_means, rowMeans(upz_means[, 2:5]))
colnames(upz_means)[ncol(upz_means)] <- "Average_puntaje"

plot(
  upz_means[, 1],
  upz_means[, 6],
  xlab = "Upz",
  ylab = "Average puntaje",
  main = "Average puntaje per upz",
  pch = 19
)


# ROW 517