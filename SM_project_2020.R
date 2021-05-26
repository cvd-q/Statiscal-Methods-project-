##############################################
###   STATISTICAL METHODS PROJECT 2020     ###
##############################################


##############################################
###      PREPARARE DATA TEST0 + TEST1      ###
##############################################

### TEST0 ###
library(stringr)
library(rlist)
setwd('/Users/jiaweima/R/STATISTICAL METHODS/PARKINSON_HW/')
files <- list.files(recursive = TRUE)
l_control <- list() # controls list 
l_parkinson <- list()
for (f in files){
  if (str_detect(f, '/C_')){
    if (str_detect(f, '.txt')){
      t <- read.table(f, header = FALSE, sep = ';' ,
                      col.names = c('X','Y','Z','Pressure','GripAngle'
                                    ,'Timestamp','TestID'))
      l_control <- list.append(l_control, t)
    }
  }
  if (str_detect(f, '/P_|/H_')){
    if (str_detect(f, '.txt')){
      t <- read.table(f, header = FALSE, sep = ';' ,
                      col.names = c('X','Y','Z','Pressure','GripAngle'
                                    ,'Timestamp','TestID'))
      l_parkinson <- list.append(l_parkinson, t)
    }
  }
}

# ONLY TEST0
l_control_test0 <- list() # control list for test0
l_parkinson_test0 <- list()
f_test0 <- function(fr){
  fr[fr[,'TestID']==0,c('X','Y','Timestamp')] #only data related to test0
}
l_control_test0 <- lapply(l_control, f_test0)
l_parkinson_test0 <- lapply(l_parkinson, f_test0)

# time translation test0 data
transl_test0 <- function(fr){
  cbind(fr[-which(colnames(fr)=='Timestamp')], Timestamp=fr$Timestamp - fr$Timestamp[1])
}
l_control_test0_trans <- lapply(l_control_test0, transl_test0)
l_parkinson_test0_trans <- lapply(l_parkinson_test0, transl_test0)

# length check 
len_control_test0 <- sapply(l_control_test0_trans, nrow)
len_parkinson_test0 <- sapply(l_parkinson_test0_trans, nrow)
boxplot(len_control_test0, len_parkinson_test0,
        names = c('control','parkinson'))
summary(len_control_test0)
summary(len_parkinson_test0)
#healthy people are more slow in general!
#take off the patient with no data
l_parkinson_test0_trans_ <- l_parkinson_test0_trans[-which.min(len_parkinson_test0)]
len_parkinson_test0_ <- sapply(l_parkinson_test0_trans_, nrow)
summary(len_parkinson_test0_)

# interpolation X,Y (spline)
f_cutoff <- function(fr){
  cutoff <- 8000 #from plot
  if (length(fr$X)<cutoff){
    end <- length(fr$X) 
    r <- cutoff - end #n. rows to add. LAST VALUES
    c <- length(fr)
    f <- as.data.frame(matrix(rep(c(fr$X[end],fr$Y[end],NaN),r),r,c,byrow = TRUE))
    colnames(f) <- colnames(fr)
    rbind(fr,f)
  }
  else{
    fr[1:cutoff,]
  }
}
new_control_test0 <- lapply(l_control_test0_trans, f_cutoff)
new_parkinson_test0_ <- lapply(l_parkinson_test0_trans_, f_cutoff)

times_test0 <- seq(0, 8000, 80) # timestamps in which values will be predicted
n_times_test0 <- length(times_test0)
f_interp <- function(fr){ #interpolation function for each df (patient)
  s_x <- smooth.spline(fr$X)
  s_y <- smooth.spline(fr$Y)
  f <- fr[1:n_times_test0,]
  f$X <- predict(s_x, times_test0)$y
  f$Y <- predict(s_y, times_test0)$y
  f$Timestamp <- 1:n_times_test0
  f
}
new_control_test0 <- lapply(new_control_test0, f_interp)
new_parkinson_test0_ <- lapply(new_parkinson_test0_, f_interp)

# add velocities 
f_v <- function(fr){ #add velocity column; diff(x)^2+diff(y)^2
  fr$V <- rep(0, length(fr$Timestamp))
  fr$V[-1] <- sqrt(diff(fr$X)^2 + diff(fr$Y)^2)
  fr
}
new_control_test0_v <- lapply(new_control_test0, f_v)
new_parkinson_test0_v <- lapply(new_parkinson_test0_, f_v)


### TEST1 ###
files <- list.files(recursive = TRUE)
l_control <- list() # controls list 
l_parkinson <- list()
for (f in files){
  if (str_detect(f, '/C_')){
    if (str_detect(f, '.txt')){
      t <- read.table(f, header = FALSE, sep = ';' ,
                      col.names = c('X','Y','Z','Pressure','GripAngle'
                                    ,'Timestamp','TestID'))
      l_control <- list.append(l_control, t)
    }
  }
  if (str_detect(f, '/P_|/H_')){
    if (str_detect(f, '.txt')){
      t <- read.table(f, header = FALSE, sep = ';' ,
                      col.names = c('X','Y','Z','Pressure','GripAngle'
                                    ,'Timestamp','TestID'))
      l_parkinson <- list.append(l_parkinson, t)
    }
  }
}
# ONLY TEST1
l_control_test1 <- list() # control list for test1
l_parkinson_test1 <- list()
f_test1 <- function(fr){
  fr[fr[,'TestID']==1,c('X','Y','Timestamp')] #test1
}
f_name <- function(fr){
  n_row <- nrow(fr)
  if (n_row>0){ #pay attention to missing data
    rownames(fr) <- seq(nrow(fr)) #IMPORTANT LATER
  }
  return(fr)
}
l_control_test1 <- lapply(l_control, f_test1)
l_parkinson_test1 <- lapply(l_parkinson, f_test1)
l_control_test1 <- lapply(l_control_test1, f_name)
l_parkinson_test1 <- lapply(l_parkinson_test1, f_name)

# time translation test1 data 
transl_test1 <- function(fr){
  cbind(fr[-which(colnames(fr)=='Timestamp')], Timestamp=fr$Timestamp - fr$Timestamp[1])
}
l_control_test1_trans <- lapply(l_control_test1, transl_test1)
l_parkinson_test1_trans <- lapply(l_parkinson_test1, transl_test1)

#length check (check if there are some patients with incomplete data)
len_control_test1 <- sapply(l_control_test1_trans, nrow)
len_parkinson_test1 <- sapply(l_parkinson_test1_trans, nrow)
boxplot(len_control_test1, len_parkinson_test1,
        names = c('control','parkinson'))
summary(len_control_test1)
summary(len_parkinson_test1)
#healthy people are more slow in general!
#take off the patient with no data 
no_data <- c(39,40,41,43,44)
l_parkinson_test1_trans_ <- l_parkinson_test1_trans[-no_data]
len_parkinson_test1_ <- sapply(l_parkinson_test1_trans_, nrow)
summary(len_parkinson_test1_)

# interpolation X,Y (spline) 
f_cutoff <- function(fr){
  cutoff <- 3000 #from plot
  if (length(fr$X)<cutoff){
    end <- length(fr$X) 
    r <- cutoff - end #n. rows to add. LAST VALUES
    c <- length(fr)
    f <- as.data.frame(matrix(rep(c(fr$X[end],fr$Y[end],NaN),r),r,c,byrow = TRUE))
    colnames(f) <- colnames(fr)
    rbind(fr,f)
  }
  else{
    fr[1:cutoff,]
  }
}
new_control_test1 <- lapply(l_control_test1_trans, f_cutoff)
new_parkinson_test1_ <- lapply(l_parkinson_test1_trans_, f_cutoff)

times_test1 <- seq(0, 3000, 30) # timestamps in which values will be predicted
n_times_test1 <- length(times_test1)
f_interp <- function(fr){ #interpolation function for each df (patient)
  s_x <- smooth.spline(fr$X)
  s_y <- smooth.spline(fr$Y)
  f <- fr[1:n_times_test1,]
  f$X <- predict(s_x, times_test1)$y
  f$Y <- predict(s_y, times_test1)$y
  f$Timestamp <- 1:n_times_test1
  f
}
new_control_test1 <- lapply(new_control_test1, f_interp)
new_parkinson_test1_ <- lapply(new_parkinson_test1_, f_interp)

# add velocities 
f_v <- function(fr){ #add velocity column; diff(x)^2+diff(y)^2
  fr$V <- rep(0, length(fr$Timestamp))
  fr$V[-1] <- sqrt(diff(fr$X)^2 + diff(fr$Y)^2)
  fr
}
new_control_test1_v <- lapply(new_control_test1, f_v)
new_parkinson_test1_v <- lapply(new_parkinson_test1_, f_v)

################ SAVED ALL DATA ########################


################  START HERE    ########################
setwd('/Users/jiaweima/Desktop/Unipd/SM project/PARKINSON_HW')
load('SM_project_2020.RData')

##############################################
###               ONLY TEST0               ###
##############################################

###   PREPARE DATA    ###
# add degree column (TRANSLATED TO ZERO) and label column
library(rlist)
library(tidyverse)
# translation to zero
i <- 1
for (c in new_control_test0){
  c$X <- c$X - c$X[1]
  c$Y <- c$Y - c$Y[1]
  new_control_test0[[i]] <- c
  i <- i +1
}

i <- 1
for (p in new_parkinson_test0_){
  p$X <- c$X - c$X[1]
  p$Y <- c$Y - c$Y[1]
  new_parkinson_test0_[[i]] <- p
  i <- i +1
}
# arctan with values in [0,2pi]
f_atan <- function(y,x){
  if (y<0){
    if (x<0){
      return(atan(y/x)+pi)
    }
    if (x>0){
      return(atan(y/x)+2*pi)
    }
    if (x==0){
      print('ERROR: x==0')
    }
  }
  else{
    if (x<0){
      return(atan(y/x)+pi)
    }
    if (x>0){
      return(atan(y/x))
    }
    if (x==0){
      print('ERROR: x==0')
    }
  }
}
# compute degrees using translated coordinates
f_degree <- function(fr){
  tol = 1
  l <- length(fr$Y)
  fr$degree[1] <- 0
  for (i in 2:l){
    y <- fr$Y[i]
    x <- fr$X[i]
    fr$degree[i] <- f_atan(y,x)
  }
  l <- length(fr$degree)
  for (i in 2:l){
    if (fr$degree[(i)] - fr$degree[i-1]>tol){ # CLOCKWISE!!!
      fr$degree[i:l] <- fr$degree[i:l] - 2*pi
    }
  }
  fr[,c('X', 'Y', 'degree','Timestamp')]
}
f_add_control_label <- function(fr){ #label=0
  cbind(fr, label=rep(0,length(fr[,1])))
}
f_add_parkinson_label <- function(fr){ #label=1
  cbind(fr, label=rep(1,length(fr[,1])))}
l_control_degree_test0 <- lapply(new_control_test0, f_degree)
l_parkinson_degree_test0_ <- lapply(new_parkinson_test0_, f_degree)
l_control_degree_test0 <- lapply(l_control_degree_test0, f_add_control_label)
l_parkinson_degree_test0_ <- lapply(l_parkinson_degree_test0_, f_add_parkinson_label)

### FIT SPIRAL MODEL (ONE PARAMETER, THAT IS NORM, LINEAR MODEL)  ###
x <- as.data.frame(list(l_control_degree_test0, l_parkinson_degree_test0_)) %>% select(starts_with('degree'))
x_degree <- c() #degrees vector
for (d in x){
  x_degree <- c(x_degree, d)
}
X <- as.data.frame(list(new_control_test0, new_parkinson_test0_)) %>% select(starts_with('X'))
Y <- as.data.frame(list(new_control_test0, new_parkinson_test0_)) %>% select(starts_with('Y'))
norm <- sqrt(X**2 + Y**2) 
y_norm <- c() #response variable; distances from the center
for (n in norm){
  y_norm <- c(y_norm, n)
}

spiral_model <- lm(y_norm ~ x_degree)
summary(spiral_model) #R^2 = 0.8696
spiral_a <- spiral_model$coefficients[1]
spiral_b <- spiral_model$coefficients[2]

### check degree data ###
draw_spiral <- function(d, c_x, c_y, point=FALSE){
  x <- (spiral_a + spiral_b*d) * cos(d) + c_x
  y <- (spiral_a + spiral_b*d) * sin(d) + c_y
  if (point==TRUE){
      points(x, y, col=2, pch=8)
    }
  else{
      lines(x, y, col=2, lwd=3)
    }
}
p <- l_parkinson_test0_trans_[[30]]   #parkinson patient with real data
plot(p$X, p$Y, pch = '.')
d <- l_parkinson_degree_test0_[[30]]$degree #same patient with degree column
draw_spiral(rev(seq(-6*pi, 0, 0.1)), p$X[1], p$Y[1]) #ideal spiral
draw_spiral(d, p$X[1], p$Y[1], point = TRUE) #set center coordinates is necessary!

#same for a control subject
c <- l_control_test0_trans[[3]]   
plot(c$X, c$Y, pch = '.')
d <- l_control_degree_test0[[3]]$degree 
draw_spiral(rev(seq(-6*pi, 0, 0.1)), c$X[1], c$Y[1])
draw_spiral(d, c$X[1], c$Y[1], point = TRUE) 

### CLASSIFICATION(GROUP LASSO) ####
#create distance matrix using the fitted spiral model
m <- rbind(list.rbind(l_control_degree_test0), list.rbind(l_parkinson_degree_test0_))
y_dist <- matrix(m$label, ncol = n_times_test0, byrow = T)[,1]
x_dist <- matrix(spiral_model$residuals, ncol = n_times_test0, byrow = T) 
# START HERE TO ANALYZE PREDICTIVE POWER
library(gglasso)
group.n <- 4
group <- rep(1:(length(x_dist[1,])/group.n+1),each=group.n, length.out=length(x_dist[1,]))
fit <- gglasso(x=x_dist, y=ifelse(y_dist==0,1,-1), group=group, loss='logit')
plot(fit)
cv.fit.gglasso <- cv.gglasso(x=x_dist,y=ifelse(y_dist==0,1,-1),group = group, loss='logit',
                             pred.loss = 'misclass')
plot(cv.fit.gglasso)
#load('cv.fit.gglasso_30%dataset.RData')
lambda_min <- cv.fit.gglasso$lambda.min
lambda_1se <- cv.fit.gglasso$lambda.1se
fit_min <- gglasso(x=x_dist, y=ifelse(y_dist==0,1,-1),
                   lambda = lambda_min, group=group, loss='logit')
coef(fit_min)
fit_1se <- gglasso(x=x_dist, y=ifelse(y_dist==0,1,-1),
                   lambda = lambda_1se, group=group, loss='logit')
coef(fit_1se)

# variables visualization
draw_spiral_var <- function(d, c_x, c_y, color, pch=8){
  x <- (spiral_a + spiral_b*d) * cos(d) + c_x
  y <- (spiral_a + spiral_b*d) * sin(d) + c_y
  points(x, y, pch = pch, col=color, lwd=3)
  return(data.frame(x = x, y=y))
}
c <- l_control_test0_trans[[3]]   
plot(c$X, c$Y, pch = '.')
c_interpolated <- l_control_degree_test0[[3]]
d_c <- c_interpolated$degree 
draw_spiral(rev(seq(-6*pi, 0, 0.1)), c$X[1], c$Y[1])
draw_spiral(d_c, c$X[1], c$Y[1], point = TRUE)
pos <- (which(coef(fit_min)!=0)-1)[-1] #position non-zero coeff. (take off the intercept)
f <- draw_spiral_var(d_c[pos], c$X[1], c$Y[1], 1, pch='P')
# same for a parkinson patient
p <- l_parkinson_test0_trans_[[30]]   
plot(p$X, p$Y, pch = '.')
p_interpolated <- l_parkinson_degree_test0_[[30]]
d_p <- p_interpolated$degree 
draw_spiral(rev(seq(-6*pi, 0, 0.1)), p$X[1], p$Y[1])
draw_spiral(d_p, p$X[1], p$Y[1], point = T)
f <- draw_spiral_var(d_p[pos], p$X[1], p$Y[1], 1, pch = 'P') #returns a dataframe with X;Y
# distance variation in the selected positions
center <- c(p$X[1], p$Y[1]) #useful later for drawing
p$X <- p$X - center[1] #translate first!
p$Y <- p$Y - center[2]
p <- p[-which(p$X==0),] #take off data which is with X=0
p_with_degree <- f_degree(p)
p_intersect_x <- c()
p_intersect_y <- c()
p_intersect_dist <- c()
for (sing_pos in c(pos)){
    # only one row (the first one) 
    # digits = 2 is ok for patient 30
    row_pos <- which(round(p_with_degree$degree,digits = 2)==round(d_p[sing_pos],
                                                                   digits = 2))[1]
    p_intersect_x <- c(p_intersect_x, p_with_degree$X[row_pos])
    p_intersect_y <- c(p_intersect_y, p_with_degree$Y[row_pos])
    p_intersect_dist <- c(p_intersect_dist,
                          sqrt(p_interpolated$X[sing_pos]^2+p_interpolated$Y[sing_pos]^2)
                          - sqrt(p_with_degree$X[row_pos]^2+p_with_degree$Y[row_pos]^2))
}
# add intersection points
points(p_intersect_x+center[1], p_intersect_y+center[2], pch=9, col=1)
# plot distances between real points and points on ideal spiral
plot(p_intersect_dist, type='l')
points(p_intersect_dist)

# same thing for the control subject
p <- c
d_p <- d_c
p_interpolated <- c_interpolated # RERUN CODES ABOVE ALONG WITH PLOT!
# USEFUL PART FOR WRITING REPORT

### classification with velocities ###
m <- rbind(list.rbind(new_control_test0_v), list.rbind(new_parkinson_test0_v))
x <- matrix(m$V, ncol = n_times_test0, byrow = T)
x_dist_v <- cbind(x_dist, x)[,c(-101,-202)]

group.n <- 4
group <- rep(1:(length(x_dist_v[1,])/group.n+1),each=group.n, length.out=length(x_dist_v[1,]))
cv.fit.gglasso <- cv.gglasso(x=x_dist_v,y=ifelse(y_dist==0,1,-1),group = group, loss='logit',
                             pred.loss = 'misclass')
plot(cv.fit.gglasso)
fit <- gglasso(x=x_dist_v, y=ifelse(y_dist==0,1,-1), group=group, loss='logit')
plot(fit)

lambda_min <- cv.fit.gglasso$lambda.min
lambda_1se <- cv.fit.gglasso$lambda.1se
fit_min <- gglasso(x=x_dist_v, y=ifelse(y_dist==0,1,-1),
                   lambda = lambda_min, group=group, loss='logit')
coef(fit_min)
fit_1se <- gglasso(x=x_dist_v, y=ifelse(y_dist==0,1,-1),
                   lambda = lambda_1se, group=group, loss='logit')
coef(fit_1se)

# compare variable selected here and those selected before added velocities
pos
pos_v <- (which(coef(fit_min)!=0)-1)[-1]
pos_v # pos<pos_v !!!

# variables visualization
pos_v <- pos_v[pos_v>100] - 100 #selected velocity variables positions
p <- l_parkinson_test0_trans_[[30]]   
plot(p$X, p$Y, pch = '.')
p_interpolated <- l_parkinson_degree_test0_[[30]]
d_p <- p_interpolated$degree 
draw_spiral(rev(seq(-6*pi, 0, 0.1)), p$X[1], p$Y[1])
f <- draw_spiral_var(d_p[pos], p$X[1], p$Y[1], 3, pch = 'P') #returns a dataframe with X;Y
f <- draw_spiral_var(d_p[pos_v], p$X[1], p$Y[1], 33, pch = 'V')

# plot velocities
plot(new_parkinson_test0_v[[10]]$V, type='l')
points(pos_v, new_parkinson_test0_v[[10]]$V[pos_v], pch=20, col=10)

###
##############################################
###         ANALYSE RESULTS TEST0          ###
##############################################
### INTERPRETING RESULTS ###
library(ggplot2)
df_dist <- matrix(nrow = length(l_parkinson_test0_trans_), ncol = length(pos))
df_xy <- matrix(ncol = length(l_parkinson_test0_trans_)*2, nrow = length(pos))
for (i in seq(length(l_parkinson_test0_trans_))){
  p <- l_parkinson_test0_trans_[[i]]   
  p_interpolated <- l_parkinson_degree_test0_[[i]]
  d_p <- p_interpolated$degree 
  center <- c(p$X[1], p$Y[1]) #useful later for drawing
  p$X <- p$X - center[1] #translate first!
  p$Y <- p$Y - center[2]
  p <- p[-which(p$X==0),] #take off data which is with X=0
  p_with_degree <- f_degree(p)
  p_intersect_x <- c()
  p_intersect_y <- c()
  p_intersect_dist <- c()
  for (sing_pos in c(pos)){
    row_pos <- which(round(p_with_degree$degree,digits = 1)==round(d_p[sing_pos],
                                                                 digits = 1))[1]
    p_intersect_x <- c(p_intersect_x, p_with_degree$X[row_pos])
    p_intersect_y <- c(p_intersect_y, p_with_degree$Y[row_pos])    
    p_intersect_dist <- c(p_intersect_dist,
                          sqrt(p_interpolated$X[sing_pos]^2+p_interpolated$Y[sing_pos]^2)-
                          sqrt(p_with_degree$X[row_pos]^2+p_with_degree$Y[row_pos]^2))
  }
  df_dist[i,] <- p_intersect_dist
  df_xy[,2*(i-1)+1] <- p_intersect_x
  df_xy[,2*i] <- p_intersect_y
  i <- i+1
}
df_xy <- data.frame(df_xy)
park_id_xy <- paste('park',paste(rep(1:(ncol(df_xy)/2),each=2),
                                 rep(c('X','Y'), ncol(df_xy)/2), sep='.'),sep='.')
colnames(df_xy) <- park_id_xy
df_dist <- data.frame(t(df_dist))
park_id <- paste('park', 1:ncol(df_dist),sep='.')
colnames(df_dist) <- park_id
pos_id <- paste('pos', 1:nrow(df_dist), seq='')
df_dist$pos <- pos_id

dist_plot <- ggplot(df_dist)+labs(y='distance', title='Parkinson_test0')
col = 1
for (p in park_id){
  dist_plot <- dist_plot + geom_point(aes_string(x="pos", y=p), color=col)
  col <- col+1
}
dist_plot

library(reshape2)
flat_df_dist <- df_dist
flat_df_dist$pos <- paste('', 1:length(flat_df_dist$pos),sep='')
flat_df_dist <- melt(flat_df_dist)
dist_plot_box <- ggplot(flat_df_dist) + labs(y='distance', x='variables',
                                             title='Parkinson_test0')
dist_plot_box <- dist_plot_box+geom_boxplot(aes(x=pos, y=value), na.rm = T, fill=pos)
dist_plot_box

# plot points
points_plot <- ggplot(df_xy) + labs(title='Parkinson(centered)_test0', x='X',y='Y')
col = 1
for (i in seq(length(park_id_xy)/2)){
  points_plot <-  points_plot + geom_point(aes_string(x=park_id_xy[(2*(i-1)+1)], y=park_id_xy[2*i]), color=col)
  points_plot <-  points_plot + geom_path(aes_string(x=park_id_xy[(2*(i-1)+1)], y=park_id_xy[2*i]), color=col)
  col <- col + 1
}
points_plot

# CONTROLL
df_dist <- matrix(nrow = length(l_control_test0_trans), ncol = length(pos))
df_xy <- matrix(ncol = length(l_control_test0_trans)*2, nrow = length(pos))
for (i in seq(length(l_control_test0_trans))){
  c <- l_control_test0_trans[[i]]   
  c_interpolated <- l_control_degree_test0[[i]]
  d_c <- c_interpolated$degree 
  center <- c(c$X[1], c$Y[1]) #useful later for drawing
  c$X <- c$X - center[1] #translate first!
  c$Y <- c$Y - center[2]
  c <- c[-which(c$X==0),] #take off data which is with X=0
  c_with_degree <- f_degree(c)
  c_intersect_x <- c()
  c_intersect_y <- c()
  c_intersect_dist <- c()
  for (sing_pos in c(pos)){
    row_pos <- which(round(c_with_degree$degree,digits = 1)==round(d_c[sing_pos],
                                                                   digits = 1))[1]
    c_intersect_x <- c(c_intersect_x, c_with_degree$X[row_pos])
    c_intersect_y <- c(c_intersect_y, c_with_degree$Y[row_pos])
    c_intersect_dist <- c(c_intersect_dist,
                          sqrt(c_interpolated$X[sing_pos]^2+c_interpolated$Y[sing_pos]^2)-
                            sqrt(c_with_degree$X[row_pos]^2+c_with_degree$Y[row_pos]^2))
  }
  df_dist[i,] <- c_intersect_dist
  df_xy[,2*(i-1)+1] <- c_intersect_x
  df_xy[,2*i] <- c_intersect_y
  i <- i+1
}
df_xy <- data.frame(df_xy)
control_id_xy <- paste('control',paste(rep(1:(ncol(df_xy)/2),each=2),
                                 rep(c('X','Y'), ncol(df_xy)/2), sep='.'),sep='.')
colnames(df_xy) <- control_id_xy
df_dist <- data.frame(t(df_dist))
control_id <- paste('control', 1:ncol(df_dist),sep='.')
colnames(df_dist) <- control_id
pos_id <- paste('pos', 1:nrow(df_dist), seq='')
df_dist$pos <- pos_id

dist_plot <- ggplot(df_dist)+labs(y='distance', title = 'Controll_test0')
col = 1
for (p in control_id){
  dist_plot <- dist_plot + geom_point(aes_string(x="pos", y=p), color=col)
  col <- col+1
}
dist_plot

library(reshape2)
flat_df_dist <- df_dist
flat_df_dist$pos <- paste('', 1:length(flat_df_dist$pos),sep='')
flat_df_dist <- melt(flat_df_dist)
dist_plot_box <- ggplot(flat_df_dist) + labs(y='distance', x='variables', title='Control_test0')
dist_plot_box <- dist_plot_box+geom_boxplot(aes(x=pos, y=value), na.rm = T, fill=pos)
dist_plot_box

# plot points
points_plot <- ggplot(df_xy) + labs(title='Control(centered)_test0', x='X',y='Y')
col = 1
for (i in seq(length(control_id_xy)/2)){
  points_plot <-  points_plot + geom_point(aes_string(x=control_id_xy[(2*(i-1)+1)], y=control_id_xy[2*i]), color=col)
  points_plot <-  points_plot + geom_path(aes_string(x=control_id_xy[(2*(i-1)+1)], y=control_id_xy[2*i]), color=col)
  
  col <- col + 1
}
points_plot



##############################################
###     PREDICTIVE POWER OF THE ALG.       ###
###     USED IN TEST0                      ###
##############################################
### FIT SPIRAL MODEL WITH ONLY 30% OF DATASET (BALANCED) ###
percentage <- 0.3
n_controls <- round(0.3 * length((l_control_degree_test0)))
n_parkinson <- round(0.3 * length((l_parkinson_degree_test0_)))
#seed?
sel_controls <- rdunif(n_controls, length((l_control_degree_test0)))
sel_parkinson <- rdunif(n_parkinson, length((l_parkinson_degree_test0_)))
x <- as.data.frame(list(l_control_degree_test0[sel_controls],
                        l_parkinson_degree_test0_[sel_parkinson])) %>% select(starts_with('degree'))
x_degree <- c() #degrees vector
for (d in x){
  x_degree <- c(x_degree, d)
}
X <- as.data.frame(list(new_control_test0[sel_controls],
                        new_parkinson_test0_[sel_parkinson])) %>% select(starts_with('X'))
Y <- as.data.frame(list(new_control_test0[sel_controls],
                        new_parkinson_test0_[sel_parkinson])) %>% select(starts_with('Y'))
norm <- sqrt(X**2 + Y**2) 
y_norm <- c() #response variable; distances from the center
for (n in norm){
  y_norm <- c(y_norm, n)
}

spiral_model <- lm(y_norm ~ x_degree)
summary(spiral_model) #R^2 = 0.985!
spiral_a <- spiral_model$coefficients[1]
spiral_b <- spiral_model$coefficients[2]

### check degree data ###
draw_spiral <- function(d, c_x, c_y, point=FALSE){
  x <- (spiral_a + spiral_b*d) * cos(d) + c_x
  y <- (spiral_a + spiral_b*d) * sin(d) + c_y
  if (point==TRUE){
    points(x, y, col=2, pch=8)
  }
  else{
    lines(x, y, col=2, lwd=3)
  }
}
p <- l_parkinson_test0_trans_[[30]]   #parkinson patient with real data
plot(p$X, p$Y, pch = '.')
d <- l_parkinson_degree_test0_[[30]]$degree #same patient with degree column
draw_spiral(rev(seq(-6*pi, 0, 0.1)), p$X[1], p$Y[1]) #ideal spiral
draw_spiral(d, p$X[1], p$Y[1], point = TRUE) #set center coordinates is necessary!

#same for a control subject
c <- l_control_test0_trans[[10]]   
plot(c$X, c$Y, pch = '.')
d <- l_control_degree_test0[[10]]$degree 
draw_spiral(rev(seq(-6*pi, 0, 0.1)), c$X[1], c$Y[1])
draw_spiral(d, c$X[1], c$Y[1], point = TRUE) 

m <- rbind(list.rbind(l_control_degree_test0), list.rbind(l_parkinson_degree_test0_))
y_dist <- matrix(m$label, ncol = n_times_test0, byrow = T)[,1]
# distances have to be computed
residuals <- sqrt(m$X^2 + m$Y^2) - spiral_a + spiral_b*m$degree
x_dist <- matrix(residuals, ncol = n_times_test0, byrow = T) 
### RERUN CLASSIFICATION BLOCK ###







################  START HERE    ########################
setwd('/Users/jiaweima/R/STATISTICAL METHODS/PARKINSON_HW/')
load('SM_project_2020.RData')

##############################################
###               ONLY TEST1               ###
##############################################
###   PREPARE DATA    ###
# add degree column (TRANSLATED TO ZERO) and label column
library(rlist)
library(tidyverse)
# translation to zero
i <- 1
for (c in new_control_test1){
  c$X <- c$X - c$X[1]
  c$Y <- c$Y - c$Y[1]
  new_control_test1[[i]] <- c
  i <- i +1
}

i <- 1
for (p in new_parkinson_test1_){
  p$X <- c$X - c$X[1]
  p$Y <- c$Y - c$Y[1]
  new_parkinson_test1_[[i]] <- p
  i <- i +1
}
# arctan with values in [0,2pi]
f_atan <- function(y,x){
  if (y<0){
    if (x<0){
      return(atan(y/x)+pi)
    }
    if (x>0){
      return(atan(y/x)+2*pi)
    }
    if (x==0){
      print('ERROR: x==0')
    }
  }
  else{
    if (x<0){
      return(atan(y/x)+pi)
    }
    if (x>0){
      return(atan(y/x))
    }
    if (x==0){
      print('ERROR: x==0')
    }
  }
}
# compute degrees using translated coordinates
f_degree <- function(fr){
  tol = 1
  l <- length(fr$Y)
  fr$degree[1] <- 0
  for (i in 2:l){
    y <- fr$Y[i]
    x <- fr$X[i]
    fr$degree[i] <- f_atan(y,x)
  }
  l <- length(fr$degree)
  for (i in 2:l){
    if (fr$degree[(i)] - fr$degree[i-1]>tol){ # CLOCKWISE!!!
      fr$degree[i:l] <- fr$degree[i:l] - 2*pi
    }
  }
  fr[,c('X', 'Y', 'degree','Timestamp')]
}
f_add_control_label <- function(fr){ #label=0
  cbind(fr, label=rep(0,length(fr[,1])))
}
f_add_parkinson_label <- function(fr){ #label=1
  cbind(fr, label=rep(1,length(fr[,1])))}
l_control_degree_test1 <- lapply(new_control_test1, f_degree)
l_parkinson_degree_test1_ <- lapply(new_parkinson_test1_, f_degree)
l_control_degree_test1 <- lapply(l_control_degree_test1, f_add_control_label)
l_parkinson_degree_test1_ <- lapply(l_parkinson_degree_test1_, f_add_parkinson_label)

### FIT SPIRAL MODEL (ONE PARAMETER, THAT IS NORM, LINEAR MODEL)  ###
x <- as.data.frame(list(l_control_degree_test1, l_parkinson_degree_test1_)) %>% select(starts_with('degree'))
x_degree <- c() #degrees vector
for (d in x){
  x_degree <- c(x_degree, d)
}
X <- as.data.frame(list(new_control_test1, new_parkinson_test1_)) %>% select(starts_with('X'))
Y <- as.data.frame(list(new_control_test1, new_parkinson_test1_)) %>% select(starts_with('Y'))
norm <- sqrt(X**2 + Y**2) 
y_norm <- c() #response variable; distances from the center
for (n in norm){
  y_norm <- c(y_norm, n)
}

spiral_model <- lm(y_norm ~ x_degree)
summary(spiral_model) #R^2 = 0.9196!
spiral_a <- spiral_model$coefficients[1]
spiral_b <- spiral_model$coefficients[2]

### check degree data ###
draw_spiral <- function(d, c_x, c_y){
  x <- (spiral_a + spiral_b*d) * cos(d) + c_x
  y <- (spiral_a + spiral_b*d) * sin(d) + c_y
  lines(x, y, col=2, lwd=3)
}
p <- l_parkinson_test1_trans_[[30]]   #parkinson patient with real data
plot(p$X, p$Y, pch = '.')
d <- l_parkinson_degree_test1_[[30]]$degree #same patient with degree column
draw_spiral(rev(seq(-6*pi, 0, 0.1)), p$X[1], p$Y[1]) #set center coordinates is necessary!

#same for a control subject
c <- l_control_test1_trans[[1]]   
plot(c$X, c$Y, pch = '.')
d <- l_control_degree_test1[[1]]$degree 
draw_spiral(rev(seq(-6*pi, 0, 0.1)), c$X[1], c$Y[1])

### CLASSIFICATION(GROUP LASSO) ####
#create distance matrix using the fitted spiral model
m <- rbind(list.rbind(l_control_degree_test1), list.rbind(l_parkinson_degree_test1_))
y_dist <- matrix(m$label, ncol = n_times_test1, byrow = T)[,1]
x_dist <- matrix(spiral_model$residuals, ncol = n_times_test1, byrow = T) 
library(gglasso)
group.n <- 4
group <- rep(1:(length(x_dist[1,])/group.n+1),each=group.n, length.out=length(x_dist[1,]))
fit <- gglasso(x=x_dist, y=ifelse(y_dist==0,1,-1), group=group, loss='logit')
plot(fit)
cv.fit.gglasso <- cv.gglasso(x=x_dist,y=ifelse(y_dist==0,1,-1),group = group, loss='logit',
                             pred.loss = 'misclass')
plot(cv.fit.gglasso)

lambda_min <- cv.fit.gglasso$lambda.min
lambda_1se <- cv.fit.gglasso$lambda.1se
fit_min <- gglasso(x=x_dist, y=ifelse(y_dist==0,1,-1),
                   lambda = lambda_min, group=group, loss='logit')
coef(fit_min)
fit_1se <- gglasso(x=x_dist, y=ifelse(y_dist==0,1,-1),
                   lambda = lambda_1se, group=group, loss='logit')
coef(fit_1se)

# variables visualization
draw_spiral_var <- function(d, c_x, c_y, color, pch=8){
  x <- (spiral_a + spiral_b*d) * cos(d) + c_x
  y <- (spiral_a + spiral_b*d) * sin(d) + c_y
  points(x, y, pch = pch, col=color, lwd=3)
  return(data.frame(x = x, y=y))
}
c <- l_control_test1_trans[[3]]   
plot(c$X, c$Y, pch = '.')
c_interpolated <- l_control_degree_test1[[3]]
d_c <- c_interpolated$degree 
draw_spiral(rev(seq(-6*pi, 0, 0.1)), c$X[1], c$Y[1])
pos <- (which(coef(fit_min)!=0)-1)[-1] #position non-zero coeff. (take off the intercept)
f <- draw_spiral_var(d_c[pos], c$X[1], c$Y[1], 3)
# same for a parkinson patient
p <- l_parkinson_test1_trans_[[10]]   
plot(p$X, p$Y, pch = '.')
p_interpolated <- l_parkinson_degree_test1_[[10]]
d_p <- p_interpolated$degree 
draw_spiral(rev(seq(-6*pi, 0, 0.1)), p$X[1], p$Y[1])
f <- draw_spiral_var(d_p[pos], p$X[1], p$Y[1], 3) #returns a dataframe with X;Y
# distance variation in the selected positions
center <- c(p$X[1], p$Y[1]) #useful later for drawing
p$X <- p$X - center[1] #translate first!
p$Y <- p$Y - center[2]
p <- p[-which(p$X==0),] #take off data which is with X=0
p_with_degree <- f_degree(p)
p_intersect_x <- c()
p_intersect_y <- c()
p_intersect_dist <- c()
for (sing_pos in c(pos)){
  # only one row (the first one) 
  # 
  row_pos <- which(round(p_with_degree$degree, digits = 1)==round(d_p[sing_pos],digits=1))
  p_intersect_x <- c(p_intersect_x, p_with_degree$X[row_pos])
  p_intersect_y <- c(p_intersect_y, p_with_degree$Y[row_pos])
  p_intersect_dist <- c(p_intersect_dist,
                        sqrt(p_interpolated$X[sing_pos]^2+p_interpolated$Y[sing_pos])
                        - sqrt(p_with_degree$X[row_pos]^2+p_with_degree$Y[row_pos]))
}
# add intersection points
points(p_intersect_x+center[1], p_intersect_y+center[2], pch='P', col=6)
# plot distances between real points and points on ideal spiral
plot(p_intersect_dist, type='l')

# same thing for the control subject
p <- c
d_p <- d_c
p_interpolated <- c_interpolated # RERUN CODES ABOVE WITH PLOT!
# USEFUL PART FOR WRITING REPORT

### classification with velocities ###
m <- rbind(list.rbind(new_control_test1_v), list.rbind(new_parkinson_test1_v))
x <- matrix(m$V, ncol = n_times_test1, byrow = T)
x_dist_v <- cbind(x_dist, x)[,c(-101,-202)]

group.n <- 4
group <- rep(1:(length(x_dist_v[1,])/group.n+1),each=group.n, length.out=length(x_dist_v[1,]))
cv.fit.gglasso <- cv.gglasso(x=x_dist_v,y=ifelse(y_dist==0,1,-1),group = group, loss='logit',
                             pred.loss = 'misclass')
plot(cv.fit.gglasso)
fit <- gglasso(x=x_dist_v, y=ifelse(y_dist==0,1,-1), group=group, loss='logit')
plot(fit)

lambda_min <- cv.fit.gglasso$lambda.min
lambda_1se <- cv.fit.gglasso$lambda.1se
fit_min <- gglasso(x=x_dist_v, y=ifelse(y_dist==0,1,-1),
                   lambda = lambda_min, group=group, loss='logit')
coef(fit_min)
fit_1se <- gglasso(x=x_dist_v, y=ifelse(y_dist==0,1,-1),
                   lambda = lambda_1se, group=group, loss='logit')
coef(fit_1se)

# compare variable selected here and those selected before added velocities
pos
pos_v <- (which(coef(fit_min)!=0)-1)[-1]
pos_v # pos not < pos_v !!!

# variables visualization
pos_v <- pos_v[pos_v>100] - 100 #selected velocity variables positions
p <- l_parkinson_test1_trans_[[30]]   
plot(p$X, p$Y, pch = '.')
p_interpolated <- l_parkinson_degree_test1_[[30]]
d_p <- p_interpolated$degree 
draw_spiral(rev(seq(-6*pi, 0, 0.1)), p$X[1], p$Y[1])
f <- draw_spiral_var(d_p[pos], p$X[1], p$Y[1], 3, pch = 'P') #returns a dataframe with X;Y
f <- draw_spiral_var(d_p[pos_v], p$X[1], p$Y[1], 33, pch='V') #SAME POSITIONS!

# plot velocities
plot(new_parkinson_test1_v[[30]]$V, type='l')
points(pos_v, new_parkinson_test1_v[[30]]$V[pos_v], pch=20, col=10)


##############################################
###         ANALYSE RESULTS TEST1          ###
##############################################
### INTERPRETING RESULTS ###
library(ggplot2)
df_dist <- matrix(nrow = length(l_parkinson_test1_trans_), ncol = length(pos))
df_xy <- matrix(ncol = length(l_parkinson_test1_trans_)*2, nrow = length(pos))
for (i in seq(length(l_parkinson_test1_trans_))){
  p <- l_parkinson_test1_trans_[[i]]   
  p_interpolated <- l_parkinson_degree_test1_[[i]]
  d_p <- p_interpolated$degree 
  center <- c(p$X[1], p$Y[1]) #useful later for drawing
  p$X <- p$X - center[1] #translate first!
  p$Y <- p$Y - center[2]
  p <- p[-which(p$X==0),] #take off data which is with X=0
  p_with_degree <- f_degree(p)
  p_intersect_x <- c()
  p_intersect_y <- c()
  p_intersect_dist <- c()
  for (sing_pos in c(pos)){
    row_pos <- which(round(p_with_degree$degree,digits = 1)==round(d_p[sing_pos],
                                                                   digits = 1))[1]
    p_intersect_x <- c(p_intersect_x, p_with_degree$X[row_pos])
    p_intersect_y <- c(p_intersect_y, p_with_degree$Y[row_pos])    
    p_intersect_dist <- c(p_intersect_dist,
                          sqrt(p_interpolated$X[sing_pos]^2+p_interpolated$Y[sing_pos]^2)-
                            sqrt(p_with_degree$X[row_pos]^2+p_with_degree$Y[row_pos]^2))
  }
  df_dist[i,] <- p_intersect_dist
  df_xy[,2*(i-1)+1] <- p_intersect_x
  df_xy[,2*i] <- p_intersect_y
  i <- i+1
}
df_xy <- data.frame(df_xy)
park_id_xy <- paste('park',paste(rep(1:(ncol(df_xy)/2),each=2),
                                 rep(c('X','Y'), ncol(df_xy)/2), sep='.'),sep='.')
colnames(df_xy) <- park_id_xy
df_dist <- data.frame(t(df_dist))
park_id <- paste('park', 1:ncol(df_dist),sep='.')
colnames(df_dist) <- park_id
pos_id <- paste('pos', 1:nrow(df_dist), seq='')
df_dist$pos <- pos_id

dist_plot <- ggplot(df_dist)+labs(y='distance', x='variables', title='Parkinson_test1')
col = 1
for (p in park_id){
  dist_plot <- dist_plot + geom_point(aes_string(x="pos", y=p), color=col)
  col <- col+1
}
dist_plot

library(reshape2)
flat_df_dist <- melt(df_dist)
dist_plot_box <- ggplot(flat_df_dist) + labs(y='distance', x='variables', title='Parkinson_test1')
dist_plot_box <- dist_plot_box+geom_boxplot(aes(x=pos, y=value), na.rm = T,fill=pos)
dist_plot_box

# plot points
points_plot <- ggplot(df_xy) + labs(title='Parkinson(centered)_test1', x='X',y='Y')
col = 1
for (i in seq(length(park_id_xy)/2)){
  points_plot <-  points_plot + geom_point(aes_string(x=park_id_xy[(2*(i-1)+1)], y=park_id_xy[2*i]), color=col)
  points_plot <-  points_plot + geom_path(aes_string(x=park_id_xy[(2*(i-1)+1)], y=park_id_xy[2*i]), color=col)
  col <- col + 1
}
points_plot

# CONTROLL
df_dist <- matrix(nrow = length(l_control_test1_trans), ncol = length(pos))
df_xy <- matrix(ncol = length(l_control_test1_trans)*2, nrow = length(pos))
for (i in seq(length(l_control_test1_trans))){
  c <- l_control_test1_trans[[i]]   
  c_interpolated <- l_control_degree_test1[[i]]
  d_c <- c_interpolated$degree 
  center <- c(c$X[1], c$Y[1]) #useful later for drawing
  c$X <- c$X - center[1] #translate first!
  c$Y <- c$Y - center[2]
  c <- c[-which(c$X==0),] #take off data which is with X=0
  c_with_degree <- f_degree(c)
  c_intersect_x <- c()
  c_intersect_y <- c()
  c_intersect_dist <- c()
  for (sing_pos in c(pos)){
    row_pos <- which(round(c_with_degree$degree,digits = 1)==round(d_c[sing_pos],
                                                                   digits = 1))[1]
    c_intersect_x <- c(c_intersect_x, c_with_degree$X[row_pos])
    c_intersect_y <- c(c_intersect_y, c_with_degree$Y[row_pos])
    c_intersect_dist <- c(c_intersect_dist,
                          sqrt(c_interpolated$X[sing_pos]^2+c_interpolated$Y[sing_pos]^2)-
                            sqrt(c_with_degree$X[row_pos]^2+c_with_degree$Y[row_pos]^2))
  }
  df_dist[i,] <- c_intersect_dist
  df_xy[,2*(i-1)+1] <- c_intersect_x
  df_xy[,2*i] <- c_intersect_y
  i <- i+1
}
df_xy <- data.frame(df_xy)
control_id_xy <- paste('control',paste(rep(1:(ncol(df_xy)/2),each=2),
                                       rep(c('X','Y'), ncol(df_xy)/2), sep='.'),sep='.')
colnames(df_xy) <- control_id_xy
df_dist <- data.frame(t(df_dist))
control_id <- paste('control', 1:ncol(df_dist),sep='.')
colnames(df_dist) <- control_id
pos_id <- paste('pos', 1:nrow(df_dist), seq='')
df_dist$pos <- pos_id

dist_plot <- ggplot(df_dist)+labs(y='distance')
col = 1
for (p in control_id){
  dist_plot <- dist_plot + geom_point(aes_string(x="pos", y=p), color=col)
  col <- col+1
}
dist_plot

library(reshape2)
flat_df_dist <- melt(df_dist)
dist_plot_box <- ggplot(flat_df_dist) + labs(y='distance', x='variables', title='Control_test1')
dist_plot_box <- dist_plot_box+geom_boxplot(aes(x=pos, y=value), na.rm = T,fill=pos)
dist_plot_box

# plot points
points_plot <- ggplot(df_xy) + labs(title='Control(centered)_test1', x='X',y='Y')
col = 1
for (i in seq(length(control_id_xy)/2)){
  points_plot <-  points_plot + geom_point(aes_string(x=control_id_xy[(2*(i-1)+1)], y=control_id_xy[2*i]), color=col)
  points_plot <-  points_plot + geom_path(aes_string(x=control_id_xy[(2*(i-1)+1)], y=control_id_xy[2*i]), color=col)
  col <- col + 1
}
points_plot






