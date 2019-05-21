
rm(list = ls())
options(scipen = 999)

library(data.table)
library(ggplot2)
library(partykit)
library(plyr)
library(reshape2)
library(stats)
library(Rcpp)
library(scam)
library(Matrix)
library(Rcgmin)
library(doParallel)

av <- "/home_app2/U832473/A++/functions/"
av <- "C:/Users/JAME/Documents/A++/functions/"
source(paste0(av, "/constrOptim_custom.R"))

# Define functions for optimizing the lower riemann sum
get_w <- function(mat = numeric()){
  ind <- which(mat != 0)
  mat <- diag(mat)
  
  # Adding a zero row 
  mat_tmp <- mat[ind, ind]
  mat_tmp <- rbind(matrix(0, 1, length(ind)), mat_tmp)
  mat_diff <- diff(diag(length(ind) + 1), differences = 1)
  w_g <- mat_diff %*% mat_tmp
  
  # pushing the matrix of differences into the matrix of all the price-points
  w <- matrix(0, ncol(mat), nrow(mat))
  w[ind,ind] <- w_g
  w
}

mat_prod <- function(mat = numeric(),
                       x = numeric()) {
  mat %*% x
}

target <- function(x = numeric(),
                   arguments = list()) {
  function.env <- environment()
  list2env(arguments, envir = function.env)

  p <- c(Min_Price, x, Max_Price)
  X <- Matrix(c(rep(1, length(p)), p), length(p), 2)

  # Calculate the sum of the rectangles under demand-curve(s)
  eta <- X %*% PAR
  lambda <- exp(eta)
  W_g <- lapply(W, get_w)
  p <- matrix(p, length(p), 1)
  p_diff <- do.call("cbind", lapply(W_g, mat_prod, x = p))
  rev <- lambda * p_diff
  sum(rev)
}

gradient <- function(x = numeric(),
                     arguments = list()) {
  function.env <- environment()
  list2env(arguments, envir = function.env)

  p <- c(Min_Price, x, Max_Price)
  X <- Matrix(c(rep(1, length(p)), p), length(p), 2)

  # Calculate the sum of the rectangles under demand-curve(s)
  eta <- X %*% PAR
  lambda <- exp(eta)
  W_g <- lapply(W, get_w)
  p <- matrix(p, length(p), 1)
  lambda_deriv <- list()
  for (g in 1:no_seg) {
    lambda_deriv[[g]] <- t(W_g[[g]]) %*% lambda[,g] + (W_g[[g]] %*% p) * PAR[2,g] * lambda[,g]
  }

  lambda_deriv <- do.call("cbind", lambda_deriv)
  lambda_deriv <- matrix(rowSums(lambda_deriv), length(p), 1)
  norm_gradient <- sqrt(sum(lambda_deriv ^ 2))
  if (VERBOSE) {
    print(paste0("Gradient L2-Norm = ", round(norm_gradient, 2)))
  }
  lambda_deriv[2:(length(p) - 1)]
}

int <- c(4, 3, 2)
slope <- c(-0.003, -0.001, -0.0001)
data <- data.table(p = seq(50, 1500, 150))
data[,demand_a := exp(int[1] + slope[1] * p)]
data[,demand_b := exp(int[2] + slope[2] * p)]
data[,demand_c := exp(int[3] + slope[3] * p)]

# Determination of restrictions
Number_Price_Points <- nrow(data)
Min_Price <- data[,min(p)]
Max_Price <- data[,max(p)]
A2 <- diag(Number_Price_Points - 2)
A3 <- diff(diag(Number_Price_Points - 1), differences = 1)
A3 <- rbind(A3, t(matrix(c(1, rep(0, Number_Price_Points - 2)))))
A3 <- A3[,-ncol(A3)]

A <- rbind(A2, A3)
b <- seq(length = dim(A)[1], from = 0, by = 0)
b[length(b) - 1] = - Max_Price
b[length(b)] = Min_Price
b <- as.vector(b)

# Enter constraint for split-point between price-sequence (protection point for segment)
Mid_Price1 <- 450
k <- 4
#A4 <- matrix(rep(0, Number_Price_Points - 2), 1, )
#A4[k] <- 1
#A <- rbind(A2, A3, A4)
#b <- c(b, Mid_Price1)

# Starting points for the optimizing algorithm 
# (have to be between Min_Price and Max_Price)
x1 <- seq(max(Min_Price + 0.0001, Min_Price * 1.0001), 
          Mid_Price1 * 1.01, 
          length.out = k)
x2 <- seq(Mid_Price1 * 1.1, 
          Max_Price * 0.99, 
          length.out = Number_Price_Points - 2 - k)
x <- sort(c(x1, x2))
x <- x[x > Min_Price]

# test
b %*% A
A %*% sort(x) - b <= 0


###
# Matrices defining what segment can buy which price-point

# Number of segments
no_seg <- 2

# Matrices of segment-parameters
PAR <- list()
PAR[[1]] <- matrix(c(int[1], slope[1]), 2, 1)
PAR[[2]] <- matrix(c(int[2], slope[2]), 2, 1)
if (no_seg > 2) {
  PAR[[3]] <- matrix(c(int[3], slope[3]), 2, 1)
}
PAR <- do.call("cbind", PAR) 

# Generate possible positions of segment b able to buy the price point
# There is no restriction that every segment is able to buy the highest price-point!
Tot_Number_Price_Points <- nrow(data)
sets <- data.table(t(combn(Tot_Number_Price_Points, 6)))

# Start optimization
Gen <- results <- Wl <- list()
i <- 1
for (i in 1:nrow(sets)) {
  print(paste0("Calculation of set = ", i))
  # Matrix defining which group may buy what price
  W <- list()
  W[[1]] <- rep(1, Tot_Number_Price_Points)
  w <- rep(0, Tot_Number_Price_Points)
  w[unlist(sets[i])[-1]] <- 1
  W[[2]] <- w
  if (no_seg > 2) {
    w <- rep(0, Tot_Number_Price_Points)
    w[unlist(sets[i])[1]] <- 1  
    W[[3]] <- w
  }
  Wl[[i]] <- W
  
  # List of arguments for actual optimization
  this_arguments <- 
  list(VERBOSE = FALSE,
       PAR = PAR,
       W = W,
       no_seg = no_seg, 
       Min_Price = Min_Price, 
       #Mid_Price1 = Mid_Price1,
       Max_Price = Max_Price)

  Gen[[i]] <- constrOptim_custom(theta = sort(x), 
                                 f = target, 
                                 f_grad = gradient, 
                                 ui = A,
                                 ci = b,
                                 method = "BFGS",
                                 control = list(fnscale = -1), 
                                 outer.eps = 1e-05, 
                                 outer.iterations = 2000,
                                 arguments = this_arguments)
  results[[i]] <- data.table(value = Gen[[i]]$value, t(Gen[[i]]$par))
}

# Save
av <- "C:/Users/JAME/Documents/A++/Proposal/"
setwd(av)
results <- rbindlist(results)
#saveRDS(results, "brute_force.RDS")
#saveRDS(data, "data_for_brute_force.RDS")

ind_max <- results[,which(value == max(value))]
ind_max <- ind_max[1]
par_max <- results[ind_max, as.numeric(.SD), .SDcol = paste0("V", 1:8)]
this_arguments$W = Wl[[ind_max]]
w <- rep(0, 10)
w[unlist(sets[ind_max])] <- 1

# Plot results
rect_data_0 <- data.table(Price.xmin = c(0, Min_Price, par_max),
                          Price.xmax = c(Min_Price, par_max, Max_Price))
rect_data_0[,Demand.Predict.ymax := exp(int[1] + slope[1] * Price.xmax)]
rect_data_0[,Demand.Predict.ymin := 0]
p_1 <- c(Min_Price, par_max, Max_Price) 
p_1 <- p_1[w == 1]
rect_data_1 <- data.table(Price.xmin = c(0, p_1[-length(p_1)]),
                          Price.xmax = p_1)
rect_data_1[,Demand.Predict.ymax := exp(int[2] + slope[2] * Price.xmax)]
rect_data_1[,Demand.Predict.ymin := 0]

text_data <- data.table(cat = w, p = c(Min_Price, par_max, Max_Price))
text_data[,demand_a := exp(int[1] + slope[1] * p)]
text_data[,demand_b := exp(int[2] + slope[2] * p)]

plot_data <- data.table(p = seq(Min_Price, Max_Price, 1))
plot_data[,demand_a := exp(int[1] + slope[1] * p)]
plot_data[,demand_b := exp(int[2] + slope[2] * p)]
plot_data <- melt(plot_data, id.vars = "p")
plot_data[,segment := as.factor(variable)]
plot_data[,variable := NULL]
plot_data[,segment := as.factor(gsub("demand_", "", segment))]

# definition of standard colors
n <- 2
hues = seq(15, 375, length = n + 1)
colors <- hcl(h = hues, l = 65, c = 100)[1:n]

base_size <- 10
text_size <- 10
u1 <- ggplot()
u1 <- u1 + theme_bw(base_size = base_size)
u1 <- u1 + theme(text = element_text(size = text_size), 
                 strip.text.x = element_text(size = text_size, angle = 360, colour = "black"), 
                 strip.text.y = element_text(size = text_size, angle = 360, colour = "black"), 
                 axis.text.x = element_text(size = text_size, angle = 30, hjust = 1), 
                 axis.text.y = element_text(size  = text_size, angle = 360, hjust = 1), 
                 axis.title.y = element_text(size = text_size, angle = 90))
u1 <- u1 + geom_line(data = plot_data, aes(x = p, y = value, col = segment), lty = 1) 
u1 <- u1 + ylab("demand")
u1

u1 <- u1 + geom_rect(data = rect_data_0, aes(xmin = Price.xmin, 
                                             xmax = Price.xmax, 
                                             ymin = Demand.Predict.ymin,
                                             ymax = Demand.Predict.ymax), 
                                             lwd = 0.3,
                                             alpha = 0.3,
                                             col = "black",
                                             fill = colors[1])
u1 <- u1 + geom_rect(data = rect_data_1, aes(xmin = Price.xmin, 
                                             xmax = Price.xmax, 
                                             ymin = Demand.Predict.ymin,
                                             ymax = Demand.Predict.ymax), 
                                             lwd = 0.3,
                                             alpha = 0.3,
                                             col = "black",
                                             fill = colors [2])
offset <- 0
u1 <- u1 + geom_label(data = text_data, aes(x = (1 + offset ) * p, 
                                            y = (1 + offset) * demand_a, 
                                            label = round(p, 2)), 
                                            size = 3, 
                                            angle = 30,
                                            fontface = "bold")
u1 <- u1 + geom_label(data = text_data[cat == 1], aes(x = (1 + offset ) * p, 
                                                        y = (1 + offset) * demand_b, 
                                            		  label = round(p, 2)), 
                                            		  size = 3, 
                                            		  angle = 30,
                                            		  fontface = "bold")
u1







# debug
theta = sort(x)
f = target
f_grad = gradient
ui = A
ci = b
mu = 1e-04
method = "BFGS"
control = list(fnscale = -1)
outer.eps = 1e-05
outer.iterations = 2000
arguments <- this_arguments
hessian = FALSE



approx_grad <- function(x = numeric(),
                        fn = NULL,
                        h = 1e-3,
                        arguments = list()) {
  if (class(x) == "numeric") {
    x <- Matrix(x, ncol = 1)
  }
  g <- foreach(k = 1:nrow(x)) %do% {
    dx <- (Matrix(1:nrow(x), ncol = 1) == k) * h
    f.p <- fn(x = as.numeric(x + dx), arguments = arguments)
    f.m <- fn(x = as.numeric(x - dx), arguments = arguments)
    gk <- (f.p - f.m) / (2 * h)
    return(gk)
  }
  do.call("rbind", g)
}

