rm(list = ls())


# set directory
# Get the path to the directory containing the current script
# Set the working directory to the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# libraries for the script
library(splines)
library(ggpubr)
library(sf)
library(gridExtra)
library(sf)
library(spdep)
library(sfdep)
library(tidyr)
library(ggplot2)
library(dplyr)


# import the data
extended_data <- read.csv("Data/Extended_data_clean.csv")
colegios <- read.csv("Data/colegios.csv")
map <- read_sf("Data/poblacion-upz-bogota.geojson")

bogota.map <- ggplot(map) +
  geom_sf(fill = "white") + theme_void()


# Initialize a vector to store the values of upz
upz_values <- numeric(nrow(extended_data))


# Loop over each row of extended_data
for (i in 1:nrow(extended_data)) {
  # Check if COD_DANE12 is in colegios$DANE12_EST
  if (extended_data$COD_DANE12[i] %in% colegios$DANE12_EST) {
    # If it is, get the index where the condition is true
    idx <-
      which(colegios$DANE12_EST == extended_data$COD_DANE12[i])[1]
    # Assign the corresponding COD_UPZ value to the correct index in upz_values
    upz_values[i] <- as.numeric(colegios$COD_UPZ[idx])
  }
}


# Add the upz_values vector as a new column named "upz" to extended_data
extended_data <- cbind(extended_data, upz = upz_values)

final_data <-
  extended_data[, c(
    "X",
    "Y",
    "P_Puntaje_2019",
    "P_Puntaje_2020",
    "P_Puntaje_2021",
    "P_Puntaje_2022",
    "EVALUADOS_2019",
    "EVALUADOS_2020",
    "EVALUADOS_2021",
    "EVALUADOS_2022",
    "upz",
    "CALENDARIO",
    "GENERO",
    "COD_LOCA",
    "CLASE_TIPO",
    "DENSITY",
    "Thombre",
    "Thombre_aprob",
    "Tmujer",
    "Tmujer_aprob"
  )]

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


update_map <- function(map, year, data, upz_low) {
  map <- map +
    geom_point(data = data,
               aes(
                 x = X,
                 y = Y,
                 color = ifelse(!(upz %in% upz_low), "Best Performing", "Worse Performing")
               ),
               alpha = 1) +
    scale_color_manual(
      name = "Performance per upz",
      values = c(
        "Best Performing" = "blue",
        "Worse Performing" = "red"
      )
    ) +
    labs(title = paste("Puntaje per upz in", year))
  return(map)
}


# add a line at the 25th percentile of the scores
par(mfrow = c(2, 2))
for (i in 2:5) {
  plot(
    upz_means$Upz,
    upz_means[, i],
    xlab = "Upz",
    ylab = "Puntaje",
    main = colnames(upz_means)[i]
  )
  abline(h = quantile(upz_means[, i], prob = 0.25),
         col = 'red')
}

  
# getting the upz with the lowest 25th percentile for each year
upz_low_list <- list()
for (year in 2019:2022) {
  col_index <- year - 2017  # Calculate the column index based on the year
  upz_low_list[[as.character(year)]] <- 
    upz_means$Upz[upz_means[, col_index] < quantile(upz_means[, col_index], prob = 0.25)]
}





# Find the maximum length of the upz_low lists
max_length <- max(sapply(upz_low_list, length))

# Pad the shorter lists with zeros
upz_low_padded <- lapply(upz_low_list, function(x) {
  if (length(x) < max_length) {
    c(x, rep(0, max_length - length(x)))
  } else {
    x
  }
})

# Combine the padded lists into a dataframe
upz_low <- as.data.frame(do.call(cbind, upz_low_padded))

# Plot the maps
ggarrange(
  update_map(bogota.map, "2019", final_data, upz_low[, 1]),
  update_map(bogota.map, "2020", final_data, upz_low[, 2]),
  update_map(bogota.map, "2021", final_data, upz_low[, 3]),
  update_map(bogota.map, "2022", final_data, upz_low[, 4]),
  ncol = 2,
  nrow = 2
)


# if you want to plot each map in a separate window
for (i in 1:4) {
  plot(update_map(bogota.map, as.character(2019 + i - 1), final_data, upz_low[, i]))
}



# ---------------------------------------------------------

# HOTSPOT ANALYSIS


perform_hotspot_analysis <- function(year_data, year) {
  colnames(year_data)[2] <- "P_Puntaje"
  # Add column average puntaje to map
  map <-
    cbind(map, year_data[match(map$cod_upz, year_data$Upz), "P_Puntaje"])
  st_geometry(map) <- "geometry"
  colnames(map)[ncol(map) - 1] <- "P_Puntaje"
  
  # Eliminate NA values
  map_na <- map[is.na(map$P_Puntaje),]
  map <- map[!is.na(map$P_Puntaje),]
  
  # Create a neighbor list based on queen contiguity
  list_nb <- poly2nb(map, queen = TRUE)
  
  # Remove polygons with empty neighbor sets from the data
  map_subset <- map[card(list_nb) > 0, ]
  
  # Now that we removed empty neighbor sets (map_subset)
  # Identify neighbors with queen contiguity (edge/vertex touching)
  map_nb <- poly2nb(map_subset, queen = TRUE)
  
  # Binary weighting assigns a weight of 1 to all neighboring features
  # and a weight of 0 to all other features
  map_w_binary <- nb2listw(map_nb, style = "B")
  
  # Calculate spatial lag of P_Puntaje
  map_lag <- lag.listw(map_w_binary, map_subset$P_Puntaje)
  
  # Test for global G statistic of P_Puntaje
  globalG_test_result <-
    globalG.test(map_subset$P_Puntaje, map_w_binary)
  
  # Identify neighbors, create weights, calculate spatial lag
  map_nbs <- map_subset |>
    mutate(
      nb = st_contiguity(geometry),
      # neighbors share border/vertex
      wt = st_weights(nb),
      # row-standardized weights
      tes_lag = st_lag(P_Puntaje, nb, wt)    # calculate spatial lag of P_Puntaje
    )
  
  # Calculate the Gi using local_g_perm
  map_hot_spots <- map_nbs |>
    mutate(Gi = local_g_perm(P_Puntaje, nb, wt, nsim = 9999)
           # nsim = number of Monte Carlo simulations (999 is default)
           ) |>
           unnest(Gi)
           
           # Cursory visualization
           # Plot looks at gi values for all locations
           cursory_plot <- ggplot(map_hot_spots, aes(fill = gi)) +
             geom_sf(color = "black", lwd = 0.15) +
             scale_fill_gradient2() + # makes the value 0 (random) be the middle
             theme_void() +
             labs(title = paste("Scores Spatial Analysis for", year))
           
           # Create a new data frame called 'tes_hot_spots"
           classification_plot <- map_hot_spots |>
             # with the columns gi and p_folded_sim
             # p_folded_sim is the p-value of a folded permutation test
             select(gi, p_folded_sim) |>
             mutate(
               # Add a new column called "classification"
               classification = case_when(
                 # Classify based on the following criteria:
                 gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
                 gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
                 gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
                 gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
                 gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
                 gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
                 TRUE ~ "Insignificant"
               ),
               # Convert 'classification' into a factor for easier plotting
               classification = factor(
                 classification,
                 levels = c(
                   "Very hot",
                   "Hot",
                   "Somewhat hot",
                   "Insignificant",
                   "Somewhat cold",
                   "Cold",
                   "Very cold"
                 )
               )
             ) |>
             # Visualize the results with ggplot2
             ggplot(aes(fill = classification)) +
             geom_sf(color = "black", lwd = 0.1) +
             scale_fill_brewer(type = "div", palette = 5) +
             theme_void() +
             labs(fill = "Hot Spot Classification",
                  title = paste("Scores Hot Spots for", year))
           
           return(list(cursory_plot, classification_plot, globalG_test_result))
}

# Apply hotspot analysis for each year separately
hotspot_results <- lapply(list(
  list(data = upz_means[, c("Upz", "P_Puntaje_2019")], year = 2019),
  list(data = upz_means[, c("Upz", "P_Puntaje_2020")], year = 2020),
  list(data = upz_means[, c("Upz", "P_Puntaje_2021")], year = 2021),
  list(data = upz_means[, c("Upz", "P_Puntaje_2022")], year = 2022)
), function(x)
  perform_hotspot_analysis(x$data, x$year))

# report p.value of global G statistic for each year
G_results <- lapply(hotspot_results, `[[`, 3)
for (i in 1:4) {
  print(paste("Year", 2019 + i - 1, ":", G_results[[i]]$p.value))
}
# verify if the p-value is less than 0.05 for each year
# if it is, then the global G statistic is significant
is_significant <- sapply(G_results, function(x)
  x$p.value < 0.05)
is_significant


# Arrange plots in a 2x2 grid for each type of plot
cursory_plots <- lapply(hotspot_results, `[[`, 1)
classification_plots <- lapply(hotspot_results, `[[`, 2)

grid.arrange(grobs = cursory_plots, ncol = 2)
grid.arrange(grobs = classification_plots, ncol = 2)


# if you want each plot in a separate window
# Plot each plot separately
for (i in seq_along(hotspot_results)) {
  plot(hotspot_results[[i]][[1]])
  plot(hotspot_results[[i]][[2]])
}
