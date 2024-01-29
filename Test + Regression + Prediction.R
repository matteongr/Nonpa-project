## ------- TEST TO SEE IF THE NUMBER OF STUDENTS MAKES A DIFFERENCE

quantile(data$EVALUADOS,probs=c(0.25,0.5,0.75))
#25,49,75
#tenere la mediana mi sembrava un brutto treshold, quindi ho messo
#100 per dividere tra scuole piccole e grandi, se avete
#un treshold migliore modificate il codice

small_schools = data[which(data$EVALUADOS<=100),]
big_schools = data[which(data$EVALUADOS>100),]

set.seed(123)

perm_t_test=function(x,y,iter=1e3){
  
  T0=abs(mean(x)-mean(y))  # define the test statistic
  T_stat=numeric(iter) # a vector to store the values of each iteration
  x_pooled=c(x,y) # pooled sample
  n=length(x_pooled)
  n1=length(x)
  
  for(perm in 1:iter){ # loop for conditional MC
    # permutation:
    permutation <- sample(1:n)
    x_perm <- x_pooled[permutation]
    x1_perm <- x_perm[1:n1]
    x2_perm <- x_perm[(n1+1):n]
    # test statistic:
    T_stat[perm] <- abs(mean(x1_perm) - mean(x2_perm))
    
  }
  
  # p-value
  p_val <- sum(T_stat>=T0)/iter
  return(p_val)
}

B2 = 1e4

perm_t_test(small_schools$P_Puntaje_,big_schools$P_Puntaje_)
#p-value is 0

hist(small_schools$P_Puntaje_,col = "green")
hist(big_schools$P_Puntaje_,col="blue")

#small schools perform better


## ---- SOME REGRESSION MODELS

library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)

#pulizia = tolgo tutte quelle covariate col ? che non capivamo cosa fossero
#tipo i calendari che appaiono una volta o cose simili
step1 = data[-which(data$CALENDARIO == 2),]
clean_data = step1[-which(step1$CALENDARIO == 5),]

## MODEL 1: coordinates + interaction 
model_gam_inter = gam(P_Puntaje_ ~ s(X, bs = 'cr') + s(Y, bs ='cr') + 
                        s(I(X*Y), bs = 'cr'), data = clean_data)

#grid
X.grid=seq(range(clean_data$X)[1],range(clean_data$X)[2],length.out = 100)
Y.grid=seq(range(clean_data$Y)[1],range(clean_data$Y)[2],length.out = 100)

grid=expand.grid(X.grid,Y.grid)
names(grid)=c('X','Y')

pred_inter = predict(model_gam_inter,
                     newdata = data.frame(grid, inter = grid$X * grid$Y))

persp3d(X.grid, Y.grid, pred_inter, col = 'yellow')
with(data,
     points3d(X, Y, P_Puntaje_, col = 'black', size = 5))

summary(model_gam_inter)
#R2 = 24.6%

## no interaction
model_gam = gam(P_Puntaje_ ~ s(X, bs = 'cr') + s(Y, bs ='cr'), data = clean_data)

summary(model_gam)
#R2 = 23.3%


## coordinates + interaction + evaluados
full_model_gam_inter = gam(P_Puntaje_ ~ s(X, bs = 'cr') + s(Y, bs ='cr') + 
                             s(I(X*Y), bs = 'cr') + s(EVALUADOS,bs='cr'), data = clean_data)

summary(full_model_gam_inter)
#R2 = 27.1%

#semiparametric
model_gam_reduced=gam(P_Puntaje_ ~ X + Y + s(EVALUADOS,bs='cr'),data = clean_data) 
summary = summary(model_gam_reduced)
summary$r.sq

#R2 18% -> no

##full model + some categorical (linear)
full_model_gam_inter_2 = gam(P_Puntaje_ ~ s(X, bs = 'cr') + s(Y, bs ='cr') + 
                               s(I(X*Y), bs = 'cr') + s(EVALUADOS,bs='cr') + as.factor(CALENDARIO) +
                               as.factor(GENERO) , data = clean_data)

summary(full_model_gam_inter_2)
#R2 = 35.6%
#all smoothing terms are significant
#GENERO = 3 is not significant (the males??)
#CALENDARIO is always significant

#calendario 3 has a positive coefficient: starting in summer helps in performing better
#genero 5 has a negative sign: mixed schools tend to perform worse

#FULL CONFORMAL: Number of students vs puntaje

library(dplyr)
library(ggplot2)
library(knitr)
library(broom)
library(tidyr)
library(progress)
library(pbapply)
pboptions(type='none')
library(dbscan)
library(gridExtra)
library(conformalInference)

data_predict = clean_data[, c(3,4)]
n_grid = 20
grid_factor = 0.25
alpha = .1
n = nrow(data_predict)
range_x = range(data_predict[, 1])[2] - range(data_predict[, 1])[1]
range_y = range(data_predict[, 2])[2] - range(data_predict[, 2])[1]
test_grid_x = seq(
  min(data_predict[, 1]) - grid_factor * range_x,
  max(data_predict[, 1]) + grid_factor * range_x,
  length.out = n_grid
)
test_grid_y = seq(
  min(data_predict[, 2]) - grid_factor * range_y,
  max(data_predict[, 2]) + grid_factor * range_y,
  length.out = n_grid
)
xy_surface = expand.grid(test_grid_x, test_grid_y)
colnames(xy_surface) = colnames(data_predict)

library(MASS)
library(rgl)
library(DepthProc)
library(hexbin)
library(aplpack)
library(robustbase)
library(MDBED)
library(pbapply)
library(parallel)


wrapper_multi_conf = function(test_point) {
  newdata = rbind(test_point, data_predict)
  newmedian = depthMedian(newdata, depth_params = list(method = 'Tukey'))
  depth_surface_vec = rowSums(t(t(newdata) - newmedian) ^ 2) #In this case I am using the Lˆ2 norm...
  sum(depth_surface_vec[-1] >= depth_surface_vec[1]) / (n + 1)
}

#codice parallelizzato
cl=makeCluster(parallel::detectCores())
clusterExport(cl=cl,list('data_predict', 'depthMedian', 'n'))


pval_surf = pbapply(xy_surface, 1, wrapper_multi_conf, cl=cl)
data_plot = cbind(pval_surf, xy_surface)
p_set = xy_surface[pval_surf > alpha, ]
poly_points = p_set[chull(p_set), ]
ggplot() +
  geom_tile(data = data_plot, aes(EVALUADOS, P_Puntaje_, fill = pval_surf)) +
  geom_point(data = data.frame(data_predict), aes(EVALUADOS, P_Puntaje_)) +
  geom_polygon(
    data = poly_points,
    aes(EVALUADOS, P_Puntaje_),
    color = 'red',
    size = 1,
    alpha = 0.01
  )

stopCluster(cl)



#ho trovato un dataset con le densita di popolazione di tutta la colombia
#l'ho ristretto a bogota, ma non è abbastanza preciso da darmi una variabilità evidente
#infatti mi dice che la densita di popolazione è uguale in tutta bogota
#quindi l'idea di provare a fare una scuola dove c'è piu popolazione e una dove ce n'è meno non si può fare

density <- read_csv("col_general_2020.csv")

head(density)

summary(density)

library(dplyr)
library(ggplot2)
library(sf)

# Your data filtering
density_bogota_filt <- density %>%
  filter(latitude >= 4.45 & latitude <= 4.80 & longitude >= -74.23 & longitude <= -74)

#10000 points
density_bogota_filt <- density_bogota_filt[sample(nrow(density_bogota_filt), 10000), ]

# Adjusted color scale
ggplot(density_bogota_filt, aes(x = longitude, y = latitude, fill = col_general_2020)) +
  geom_point(shape = 21, size = 2) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom")



# prediction con 25 evaluados e diverse location
#ho scelto 25 evaluados perche possiamo dire che il sindaco voleva creare una scuola di eccellenze
#e come abbiamo visto le scuole piccole vanno meglio
#ora proviamo a posizionare la scuola in qualche zona random e vediamo come performa

# Calculate the median of X and Y coordinates
median_x <- median(data$X)
median_y <- median(data$Y)

# Create a data frame for the median point
median_point <- data.frame(X = median_x, Y = median_y)

new_school_median <- data.frame(X=median_x, Y=median_y, EVALUADOS=25)

#use fitted model to predict the response value for the new observation
predict(full_model_gam_inter, newdata=new_school_median)


#est point
est_x <- -74.19
est_y <- 4.62

# Create a data frame for the est point
est_point <- data.frame(X = est_x, Y = est_y)

new_school_est <- data.frame(X=est_x, Y=est_y, EVALUADOS=25)

#predict for the est point
predict(full_model_gam_inter, newdata=new_school_est)

#sud point
sud_x <- -74.11
sud_y <- 4.48

# Create a data frame for the sud point
sud_point <- data.frame(X = sud_x, Y = sud_y)

new_school_sud <- data.frame(X=sud_x, Y=sud_y, EVALUADOS=25)

#predict for the sud point
predict(full_model_gam_inter, newdata=new_school_sud)

# nord point
nord_x <- -74.07
nord_y <- 4.75

# Create a data frame for the nord point
nord_point <- data.frame(X = nord_x, Y = nord_y)

new_school_nord <- data.frame(X=nord_x, Y=nord_y, EVALUADOS=25)

#predict for the nord point
predict(full_model_gam_inter, newdata=new_school_nord)

# Add the points to the final map
final_map_with_median <- final_map +
  geom_point(data = as.data.frame(coord), aes(x = X, y = Y), color = "blue", size = 0.1) +
  geom_point(data = median_point, aes(x = X, y = Y), color = "red", size = 2) + 
  geom_point(data = est_point, aes(x = X, y = Y), color = "green", size = 2) +
  geom_point(data = sud_point, aes(x = X, y = Y), color = "yellow", size = 2) +
  geom_point(data = nord_point, aes(x = X, y = Y), color = "orange", size = 2)


# Display the final map
final_map_with_median


#con questo abbiamo visto che a nord andrebbe meglio



#SOME ROBUST METHODS??? In my opinion it's useless