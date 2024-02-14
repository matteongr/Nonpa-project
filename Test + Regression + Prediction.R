rm(list = ls())

## ---- SOME REGRESSION MODELS

library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(knitr)
library(kableExtra)
library(tidyr)
library(dplyr)

# load data
extended_data = read.csv("Data/extended_data_clean.csv")

gam_models <- function(year, plot, flag.pred) {
  if (year < 2019 | year > 2022) {
    stop("Year must be between 2019 and 2022")
  } else {
    puntaje_col <- paste("P_Puntaje_", year, sep = "")
    evaluados_col <- paste("EVALUADOS_", year, sep = "")
    
    data <-
      extended_data[, c("X",
                        "Y",
                        puntaje_col,
                        evaluados_col,
                        "CALENDARIO",
                        "GENERO")]
  }
  
  ## MODEL 1: coordinates + interaction
  model_gam_inter <-
    gam(as.formula(
      paste(
        puntaje_col,
        "~ s(X, bs = 'cr') + s(Y, bs = 'cr') + s(I(X * Y), bs = 'cr')"
      )
    ), data = extended_data)
  
  # grid
  X.grid <- seq(range(extended_data$X)[1],
                range(extended_data$X)[2],
                length.out = 100)
  Y.grid <- seq(range(extended_data$Y)[1],
                range(extended_data$Y)[2],
                length.out = 100)
  
  grid <- expand.grid(X.grid, Y.grid)
  names(grid) <- c('X', 'Y')
  
  pred_inter <- predict(model_gam_inter,
                        newdata = data.frame(grid, inter = grid$X * grid$Y))
  
  if (plot) {
    persp3d(X.grid, Y.grid, pred_inter, col = 'yellow')
    with(extended_data,
         points3d(
           X,
           Y,
           get(puntaje_col),
           col = 'black',
           size = 5
         ))
  }
  
  ## no interaction
  model_gam <-
    gam(as.formula(paste(
      puntaje_col, "~ s(X, bs = 'cr') + s(Y, bs = 'cr')"
    )), data = extended_data)
  
  ## coordinates + interaction + evaluados
  full_model_gam_inter <-
    gam(as.formula(
      paste(
        puntaje_col,
        "~ s(X, bs = 'cr') + s(Y, bs = 'cr') + s(I(X * Y), bs = 'cr') + s(get(evaluados_col), bs = 'cr')"
      )
    ), data = extended_data)
  
  # semiparametric
  model_gam_reduced <-
    gam(as.formula(paste(
      puntaje_col, "~ X + Y + s(get(evaluados_col), bs = 'cr')"
    )), data = extended_data)
  
  ## full model + some categorical (linear)
  full_model_gam_inter_2 <-
    gam(as.formula(
      paste(
        puntaje_col,
        "~ s(X, bs = 'cr') + s(Y, bs = 'cr') + s(I(X * Y), bs = 'cr') + s(get(evaluados_col), bs = 'cr') + as.factor(CALENDARIO) + as.factor(GENERO)"
      )
    ), data = extended_data)
  
  if (flag.pred) {
    return(full_model_gam_inter)
  } else {
    return(
      list(
        model_gam_inter,
        model_gam,
        full_model_gam_inter,
        model_gam_reduced,
        full_model_gam_inter_2
      )
    )
  }
  
}


# Define model names
model_names <- c(
  "Model 1: Coordinates + Interaction",
  "Model 2: No Interaction",
  "Model 3: Coordinates + Interaction + Evaluados",
  "Model 4: Semiparametric",
  "Model 5: Full Model + categorical"
)

# Create an empty data frame to store model characteristics
model_table <- data.frame(
  Model = character(),
  R_squared = numeric(),
  Year = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each year
for (year in 2019:2022) {
  results <- gam_models(year, plot = FALSE, flag.pred = FALSE)
  year_models <- data.frame(
    Model = model_names,
    R_squared = sapply(results, function(model)
      summary(model)$r.sq),
    Year = year
  )
  model_table <- bind_rows(model_table, year_models)
}

# Reshape the data to have years as columns
model_table_wide <- spread(model_table, Year, R_squared)

# Convert the data frame to a formatted table
formatted_table <- kable(model_table_wide, "html") %>%
  kable_styling(full_width = FALSE)
# Print the table
print(formatted_table)



# ------------------------------------------------------------------------


#FULL CONFORMAL: Number of students vs puntaje

library(ggplot2)
library(broom)
pboptions(type = 'none')
library(dbscan)
library(gridExtra)
library(conformalInference)
library(MASS)
library(DepthProc)
library(hexbin)
library(aplpack)
library(robustbase)
library(MDBED)
library(parallel)
library(sf)


# Function to perform the analysis for a specific year
perform_analysis <- function(year, data_predict) {
  n_grid <- 20
  grid_factor <- 0.25
  alpha <- 0.1
  n <- nrow(data_predict)
  range_x <-
    range(data_predict[, 1])[2] - range(data_predict[, 1])[1]
  range_y <-
    range(data_predict[, 2])[2] - range(data_predict[, 2])[1]
  test_grid_x <- seq(
    min(data_predict[, 1]) - grid_factor * range_x,
    max(data_predict[, 1]) + grid_factor * range_x,
    length.out = n_grid
  )
  test_grid_y <- seq(
    min(data_predict[, 2]) - grid_factor * range_y,
    max(data_predict[, 2]) + grid_factor * range_y,
    length.out = n_grid
  )
  xy_surface <- expand.grid(test_grid_x, test_grid_y)
  colnames(xy_surface) <- colnames(data_predict)
  
  wrapper_multi_conf <- function(test_point) {
    newdata <- rbind(test_point, data_predict)
    newmedian <-
      depthMedian(newdata, depth_params = list(method = 'Tukey'))
    depth_surface_vec <- rowSums(t(t(newdata) - newmedian) ^ 2)
    sum(depth_surface_vec[-1] >= depth_surface_vec[1]) / (n + 1)
  }
  
  cl <- makeCluster(parallel::detectCores())
  clusterExport(cl = cl, list('data_predict', 'depthMedian', 'n'))
  
  pval_surf <- pbapply(xy_surface, 1, wrapper_multi_conf, cl = cl)
  data_plot <- cbind(pval_surf, xy_surface)
  p_set <- xy_surface[pval_surf > alpha, ]
  poly_points <- p_set[chull(p_set), ]
  
  stopCluster(cl)
  
  ggplot() +
    geom_tile(data = data_plot, aes_string(
      x = names(data_predict)[1],
      y = names(data_predict)[2],
      fill = "pval_surf"
    )) +
    geom_point(data = data.frame(data_predict), aes_string(x = names(data_predict)[1], y = names(data_predict)[2])) +
    geom_polygon(
      data = poly_points,
      aes_string(x = names(data_predict)[1], y = names(data_predict)[2]),
      color = 'red',
      size = 1,
      alpha = 0.01
    )
}

# Loop through each year and perform the analysis
for (year in 2019:2022) {
  data_predict <-
    extended_data[, c(paste0("EVALUADOS_", year), paste0("P_Puntaje_", year))]
  print(perform_analysis(year, data_predict))
}
# ------------------------------------------------------------------------

# prediction with 25 evaluados in 4 different locations in bogota



# Load map data
map <- read_sf("Data/poblacion-upz-bogota.geojson")

# Create ggplot object for the map
bogota.map <- ggplot(map) +
  geom_sf(fill = "white") + theme_void()


# Function to perform prediction and create plot for a specific year
perform_prediction <-
  function(year,
           full_model_gam_inter,
           evaluados_col) {
    # Calculate the median of X and Y coordinates
    median_x <- median(extended_data$X)
    median_y <- median(extended_data$Y)
    
    # Create a data frame for the median point
    median_point <- data.frame(X = median_x, Y = median_y)
    
    new_school_median <-
      data.frame(X = median_x,
                 Y = median_y,
                 evaluados_col = 25,
                 CALENDARIO = 3,
                 GENERO = 5
      )
    names(new_school_median)[3] <- evaluados_col
    
    # Prediction for the median point
    median_prediction <-
      predict(full_model_gam_inter, newdata = new_school_median)
    
    # Create points for est, sud, and nord locations
    est_point <- data.frame(X = -74.19, Y = 4.62)
    sud_point <- data.frame(X = -74.11, Y = 4.48)
    nord_point <- data.frame(X = -74.07, Y = 4.75)
    
    # Create data frames for est, sud, and nord locations with 25 evaluados
    new_school_est <-
      data.frame(X = est_point$X,
                 Y = est_point$Y,
                 EVALUADOS = 25)
    new_school_sud <-
      data.frame(X = sud_point$X,
                 Y = sud_point$Y,
                 EVALUADOS = 25)
    new_school_nord <-
      data.frame(X = nord_point$X,
                 Y = nord_point$Y,
                 EVALUADOS = 25)
    
    # Predictions for est, sud, and nord locations
    est_prediction <-
      predict(full_model_gam_inter, newdata = new_school_est)
    sud_prediction <-
      predict(full_model_gam_inter, newdata = new_school_sud)
    nord_prediction <-
      predict(full_model_gam_inter, newdata = new_school_nord)
    
    
    # Add the points to the final map
    final_map <- bogota.map +
      geom_point(data = extended_data,
                 aes(x = X, y = Y),
                 color = "gray",
                 size = 0.1) +
      geom_point(
        data = median_point,
        aes(x = X, y = Y),
        color = "yellow",
        size = 3
      ) +
      geom_point(
        data = est_point,
        aes(x = X, y = Y),
        color = "orange",
        size = 3
      ) +
      geom_point(data = sud_point,
                 aes(x = X, y = Y),
                 color = "red",
                 size = 3) +
      geom_point(
        data = nord_point,
        aes(x = X, y = Y),
        color = "green",
        size = 3
      )
    
    # Print the final map
    print(final_map)
    
    # Return the predictions
    return(
      list(
        median = median_prediction,
        est = est_prediction,
        sud = sud_prediction,
        nord = nord_prediction
      )
    )
  }

# Create an empty list to store the predictions
predictions_list <- list()

# Loop through each year and perform the prediction
for (year in 2019:2022) {
  # Extract the GAM model for the current year
  model <- gam_models(year, plot = FALSE, flag.pred = TRUE)
  # Get evaluados_col for the current year
  evaluados_col <- paste("EVALUADOS_", year, sep = "")
  # Perform prediction and create plot
  predictions <- perform_prediction(year, model, evaluados_col)
  # Store the predictions in the list
  predictions_list[[as.character(year)]] <- predictions
}

# Print the plots in a 2x2 grid
par(mfrow = c(2, 2))
for (year in 2019:2022) {
  cat("Year:", year, "\n")
  print(final_map)
}

# plot one by one
for (year in 2019:2022) {
  cat("Year:", year, "\n")
  print(final_map)
}