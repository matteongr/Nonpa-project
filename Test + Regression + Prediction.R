## ------- TEST TO SEE IF THE NUMBER OF STUDENTS MAKES A DIFFERENCE

quantile(data$EVALUADOS,probs=c(0.25,0.5,0.75))
#25,49,75
#tenere la mediana mi sembrava un brutto treshold, quindi ho messo
#100 per dividere tra scuole piccole e grandi, se avete
#un treshold migliore modificate il codice

small_schools = data[which(data$EVALUADOS<=100),]
big_schools = data[which(data$EVALUADOS>100),]

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


wrapper_multi_conf = function(test_point) {
  newdata = rbind(test_point, data_predict)
  newmedian = depthMedian(newdata, depth_params = list(method = 'Tukey'))
  depth_surface_vec = rowSums(t(t(newdata) - newmedian) ^ 2) #In this case I am using the Lˆ2 norm...
  sum(depth_surface_vec[-1] >= depth_surface_vec[1]) / (n + 1)
}


pval_surf = pbapply(xy_surface, 1, wrapper_multi_conf)
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

#IDEA: PREDICTION: let's suppose the major plans to build a new, big school (e.g 150 students)
#He has 2 ideas
#one in the "centre" #(take the medians of the coordinates)
#one far away from the centre (e.g max of Y?)
#how will the schools perform?
new_school <- data.frame(X=median(data$X), Y=median(data$Y), EVALUADOS=150)

#use fitted model to predict the response value for the new observation
predict(full_model_gam_inter, newdata=new_school)
#276, not bad

#It would be interesting to plot the point into the map, can you do it??

#far_school = ? (#I have no idea of which coordinates I should insert)


#SOME ROBUST METHODS??? In my opinion it's useless