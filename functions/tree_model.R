#screen -S A++ R
screen -S A++ /home/U832473/R-3.5.3/bin/R

options(scipen = 999)





###
# load functions and packages
library(data.table)
library(ggplot2)
library(partykit)
library(plyr)
library(reshape2)
library(stats)
library(Rcpp)
library(scam)
#library(Rcgmin)
library(Matrix)
library(doParallel)

#prefix <- "/home_app2"
prefix <- "/home"

# load from exchange-folder used in production
dir.project <- paste0(prefix, "/U832473/GIT/unified_code_agam/")
source(paste0(dir.project, "target/knots.R"))

av <- paste0(prefix, "/U832473/A++/functions/")
av_data <- paste0(prefix, "/U832473/A++/data/")
av_plot <- paste0(prefix, "/U832473/A++/")

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

process_sets <- function(i = numeric(),
                         Number_Price_Points = numeric(),
                         sets = list(),
                         VERBOSE = logical(),
                         PAR = list(),
                         A = numeric(),
                         b = numeric(),
                         Min_Price = numeric(),
                         Max_Price = numeric(),
                         no_seg = numeric()) {
  output <- list()
  print(paste0("Calculation of set = ", i, " / ", nrow(sets)))
  # Matrix defining which group may buy what price-point
  W <- list()
  W[[1]] <- rep(1, Number_Price_Points)
  name_sets <- names(sets)
  if (no_seg > 1) {
    for (g in 2:no_seg) {
      ind_g <- grep(paste0("seg", g - 1), name_sets)
      w <- rep(0, Number_Price_Points)
      buy_g <- unlist(sets[i, .SD, .SDcol = ind_g])
      w[buy_g] <- 1
      W[[g]] <- w
    }
  }
  output$W <- W

  # Starting points for the optimizing algorithm 
  # (have to be between Min_Price and Max_Price)
  x <- seq(max(Min_Price + 0.0001, Min_Price * 1.0001), 
           Max_Price * 0.99, 
           length.out = Number_Price_Points - 2)
  x <- x[x > Min_Price]

  # test
  #b %*% A
  #A %*% sort(x) - b <= 0

  # List of arguments for actual optimization
  this_arguments <- 
  list(VERBOSE = VERBOSE,
       PAR = PAR,
       W = W,
       no_seg = no_seg, 
       Min_Price = Min_Price,
       Max_Price = Max_Price)

  Gen <- constrOptim_custom(theta = sort(x), 
                            f = target, 
                            f_grad = gradient, 
                            ui = A,
                            ci = b,
                            method = "BFGS",
                            control = list(fnscale = -1), 
                            outer.eps = 1e-05, 
                            outer.iterations = 2000,
                            arguments = this_arguments)
  output$value = Gen$value
  output$par <- t(Gen$par)
  return(output)
}

# Define helper function
comb <- function(x, ...) {
  lapply(seq_along(x),
    function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
} 

mat_prod <- function(mat = numeric(),
                       x = numeric()) {
  mat %*% x
}





# Read data
setwd(av_data)

files <- list.files()
index <- grep("merged_tickets_", files)
files <- files[index]

data <- list()
for (i in 1:length(files)) {
  data[[i]] <- readRDS(paste0(av_data, files[i]))
}
data[[1]][,ADVANCED_PURCHASE_NUM := NULL]
data <- rbindlist(data)
data[,CPN := 1]
data[,MIN_STAY := as.character(MIN_STAY)]
data[,DEP_DAY := weekdays(DEP_DATE, abbreviate = TRUE)]
data[,DEP_MONTH := month(DEP_DATE)]
data[,DEP_YEAR := year(DEP_DATE)]
n_TTD <- 1
d_TTD <- 1
k_TTD  <- knots(n_TTD, d_TTD, 0, 366, kind = "quantile",
                input = c(dep = "CPN", var = "TKT_TTD"),
                data = data)
segments_t <- k_TTD[k_TTD <= 366 & k_TTD >= 0]
data[,T_int := cut(TKT_TTD, segments_t,
                   include.lowest = TRUE,
                   ordered_result = FALSE,
                   dig.lab = 6)]


# Only select specific compartment
data <- data[COMP_booked == "Economy"]


# Definition of keys
id.key <- c("sales_airport", "compartment", "MIN_STAY", "T_int")
flight.key <- c("OPT_FLT_NUM", "DEP_DAY", "DEP_MONTH", "DEP_YEAR")


# If price optimization should be performed for normalized values (TRUE) or not (FALSE)
normalized <- TRUE


# Aggregate min-stay levels with low frequency
this_key <- c("sales_airport", "compartment", "MIN_STAY")
low_levels_of_min_stay <- names(which(data[,prop.table(table(MIN_STAY))] < 0.005))
data[MIN_STAY %in% low_levels_of_min_stay, MIN_STAY := "not specified"]
data[MIN_STAY %in% low_levels_of_min_stay, MIN_STAY := "not specified"]
data[,MIN_STAY := gsub("/", "_", MIN_STAY)]
data[,MIN_STAY := as.factor(as.character(MIN_STAY))]


# Split up conditions of MIN_STAY in terms of days- and day-conditions
min_stay_day_categories <- "SUN01"
ind_ <- data[,grep("_", MIN_STAY)]
data[-ind_, MIN_STAY := ifelse(MIN_STAY %in% min_stay_day_categories, paste0("unspecified_", MIN_STAY), paste0(MIN_STAY, "_unspecified"))]
data[,c("MIN_STAY_LENGTH", "MIN_STAY_DAY") := as.list(strsplit(as.character(MIN_STAY), "_")[[1]]), by = "MIN_STAY"]
data[,list(MIN_STAY, MIN_STAY_LENGTH, MIN_STAY_DAY)]


# Calculate the cumulative booking-process
data[,FLIGHT := .GRP, by = flight.key]
this_key <- c("ENDPREIS", 
              "RBD", 
              "T_int",
              "MIN_STAY",
              "MIN_STAY_LENGTH",
              "MIN_STAY_DAY",
              "COMP_booked",  
              "SALES_AIRPT_OD",
              "FLIGHT", 
              flight.key)
tmp <- data[,list(Coupons = sum(CPN)), by = this_key]
setnames(tmp, c("ENDPREIS", "COMP_booked", "SALES_AIRPT_OD"), 
              c("Price", "compartment", "sales_airport"))
this_key <- c("MIN_STAY", "compartment", "sales_airport", "FLIGHT", flight.key)
tmp <- tmp[order(-Price), Coupons.cumsum := cumsum(Coupons), by = this_key]
tmp[,ID := .GRP, by = c(flight.key, id.key)]


# Check length of curves
tmp[,n := .N, by = "ID"]
u1 <- ggplot(tmp, aes(n)) + geom_histogram() + theme_bw()
u1 <- u1 + geom_vline(xintercept = 1, lty = 2, col = "red")
pdf(width = 13, "Distribution_of_length_of_curves.pdf")
  print(u1)
dev.off()
tmp <- tmp[n >= 2]


# Plot of data
dday_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
tmp[,DEP_DAY := ordered(DEP_DAY, levels = dday_order)]
tmp[,title := paste0("Comp = ", compartment, ", OD = ", sales_airport)]
titles <- tmp[,unique(title)]
pdf (width = 13, "Empirical_booking_curves.pdf")
  for (i in seq_along(titles)) {
    u3 <- ggplot(tmp[title == titles[[i]]], aes(x = Price, y = Coupons.cumsum, group = ID))
    u3 <- u3 + geom_line(alpha = 0.5) + theme_bw()
    u3 <- u3 + ggtitle(titles[i])
    u3 <- u3 + ylab("Sold coupons")
    u3 <- u3 + facet_grid(DEP_DAY ~ MIN_STAY, scale = "free_y")
    print(u3)
  }
dev.off()
tmp[,title := NULL]


# Normalize prices
min_max_key <- c("sales_airport", "compartment")
Price_min_max <- tmp[,list(Price_min = min(Price), Price_max = max(Price)), by = min_max_key]
Price_min_max[,GRP := .GRP, by = min_max_key]
Price_min_max[,GRP := as.factor(GRP)]

if (normalized) {
  setkeyv(Price_min_max, min_max_key)
  setkeyv(tmp, min_max_key)
  tmp <- Price_min_max[tmp]
  tmp[,Price_norm := (Price - Price_min)/(Price_max - Price_min)]
}
tmp[,compartment := as.factor(compartment)]
tmp[,sales_airport := as.factor(sales_airport)]
dday_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
tmp[,DEP_DAY := ordered(DEP_DAY, levels = dday_order)]
tmp[,DEP_MONTH := as.factor(DEP_MONTH)]
tmp[,DEP_YEAR := as.factor(DEP_YEAR)]
tmp[,MIN_STAY := as.factor(as.character(MIN_STAY))]
tmp[,MIN_STAY_DAY := as.factor(as.character(MIN_STAY_DAY))]
tmp[,MIN_STAY_LENGTH := as.factor(as.character(MIN_STAY_LENGTH))]


# Save factor-levels for prediction
tmp[,DEP_DAY := as.factor(as.character(DEP_DAY))]
min_stay_levels <- tmp[,levels(MIN_STAY)]
compartment_levels <- tmp[,levels(compartment)]
sales_airport_levels <- tmp[,levels(sales_airport)]
dep_day_levels <- tmp[,levels(DEP_DAY)]
dep_month_levels <- tmp[,levels(DEP_MONTH)]
dep_year_levels <- tmp[,levels(DEP_YEAR)]
min_stay_day_levels <- tmp[,levels(MIN_STAY_DAY)]
min_stay_length_levels <- tmp[,levels(MIN_STAY_LENGTH)]
T_int_levels <- tmp[,levels(T_int)]


# Single booking-curve
# Test of plotting a single group 
mod_glm <- glm(Coupons.cumsum ~ Price, family = poisson(), data = tmp)
mod_scam <- scam(Coupons.cumsum ~ s(Price, k = 10, bs = "mpd"), family = poisson, data = tmp)
Price_min <- tmp[,min(Price)]
Price_max <- tmp[,max(Price)]
fit_data <- CJ(Price = seq(Price_min, Price_max, length.out = 200))
fit_data[,glm := predict(mod_glm, newdata = fit_data, type = "response")]
fit_data[,scam := predict(mod_scam, newdata = fit_data, type = "response")]
fit_data <- melt(fit_data, id.vars = c("Price"))
setnames(fit_data, "variable", "method")
u <- ggplot(tmp, aes(x = Price, y = Coupons.cumsum)) + geom_point(alpha = 0.5, shape = 4)
u <- u + geom_line(data = fit_data, aes(x = Price, y = value, col = method, group = method), lty = 2)
u <- u + xlab("Price")
u <- u + ylab("Demand")
u <- u + theme_bw()
pdf(width = 13, "single_booking_curve_not_aggregated.pdf")
  print(u)
dev.off()


# Tree-Model
if (normalized) {
  class(eq_mob <- Coupons.cumsum ~ Price_norm | compartment + sales_airport + MIN_STAY_DAY + MIN_STAY_LENGTH + DEP_DAY + DEP_MONTH + DEP_YEAR)
} else {
  class(eq_mob <- Coupons.cumsum ~ Price | compartment + sales_airport + MIN_STAY_DAY + MIN_STAY_LENGTH + DEP_DAY + DEP_MONTH + DEP_YEAR)
}
min_size <- sqrt(nrow(tmp))
mod <- glmtree(eq_mob, 
               data = tmp,
               family = poisson(), 
               alpha = 0.05,
               bonferroni = TRUE,
               verbose = TRUE,
               prune = "BIC",
               #maxdepth = 5,
               minsize = min_size,
               breakties = TRUE,
               restart = TRUE,
               cores = 50)


# Set wd
av <- paste0(prefix, "/U832473/A++/")
setwd(av)


# Plot and save tree
pdf (height = 30, width = 100, file = "Tree.pdf")
  plot(mod, terminal_panel = NULL)
dev.off()
saveRDS(mod, paste0(av, "mod_tree.RDS"))
mod <- readRDS(paste0(av, "mod_tree.RDS"))
tmp[,node := predict(mod, newdata = tmp, type = "node")]



# Create table of model coefficients and splitting-rules
coef_table <- list()
number.of.nodes <- length(mod)
coef_names <- names(coef(mod, node = 1))
for (i.node in c(1:number.of.nodes)) {
  print(paste0("Node = ", i.node,"."))     
  coef_mod <- as.data.table(t(coef(mod, node = i.node)))  
  if (is.terminal(mod[[i.node]]$node)) {
    kids <- rep(NA, 2)
  } else {
    kids.ind <- c(kids_node(mod[[i.node]]$node)[[1]]$id,
                  kids_node(mod[[i.node]]$node)[[2]]$id)
    kids.labels <- names(mod[[i.node]])
    kids <- as.numeric(kids.labels[kids.ind])
  }
  coef_mod[,c("kid1", "kid2") := as.list(kids)]
  # Check if the labels of the coefficients are accurate given that
  # a category may be missing after a sequence of splits
  split <- partykit:::.list.rules.party(mod, i.node)
  
  coef_mod[,node := i.node]
  coef_mod[,terminal := ifelse(any(is.na(kids)), TRUE, FALSE)]
  coef_mod[,split := split]
  coef_table[[i.node]] <- coef_mod
}
coef_table <- rbindlist(coef_table)
coef_names_new <- gsub("\\(", "", coef_names)
coef_names_new <- gsub("\\)", "", coef_names_new)
setnames(coef_table , coef_names, coef_names_new)


# Ensure that all price-coefficients are negative
# If there is a positive price-coefficient in one of the trees leaf,
# go to its parent node, i.e., one level higher until a non-positive value 
# is reached
if (normalized) {
  nodes <- coef_table[Price_norm > 0 & terminal == TRUE, node]
  coef_names <- c("Intercept", "Price_norm")
} else {
  nodes <- coef_table[Price > 0 & terminal == TRUE, node]
  coef_names <- c("Intercept", "Price")
}
for (k in seq_along(nodes)) {
  ind_parent <- coef_table[,which(nodes[k] %in% c(kid1, kid2))]
  coef_values_parent <- as.numeric(coef_table[ind_parent, .SD, .SDcol = coef_names])
  this_node <- coef_table[ind_parent, node]
  if (coef_values_parent[2] > 0) {
    while (coef_values_parent[2] > 0) {
      ind_parent <- coef_table[,which(this_node %in% c(kid1, kid2))]
      if (length(ind_parent) == 0) {
        break
      } else {
        coef_values_parent <- as.numeric(coef_table[ind_parent, .SD, .SDcol = coef_names])
        this_node <- coef_table[ind_parent, node]
      }
    }
  } else {
    coef_table[node == nodes[k], c(coef_names_new) := as.list(coef_values_parent)]
  }
}
if (normalized){
  price_name <- "Price_norm"
} else {
  price_name <- "Price"
}
quantile(unlist(coef_table[, .SD, .SDcols = price_name]), seq(0, 1, 0.01))
quantile(unlist(coef_table[terminal == TRUE, .SD, .SDcols = price_name]), seq(0, 1, 0.01))

coef_table_terminal <- coef_table[terminal == TRUE]
if (normalized) {
  setnames(coef_table_terminal, c("Intercept", "Price_norm"), c("b0_norm", "b1_norm"))
} else {
  setnames(coef_table_terminal, c("Intercept", "Price"), c("b0", "b1"))
}

# If MIN_STAY is part of the model-equation
ind_min_stay <- grep("MIN_STAY", coef_names_new)
if (length(ind_min_stay) > 0) {
  min_stay_cats <- paste0("MIN_STAY", tmp[,unique(MIN_STAY)])
  missing_cat <- min_stay_cats[!min_stay_cats %in% names(coef_table_terminal)]
  coef_table_terminal[,c(missing_cat) := 0]
  columns <- names(coef_table_terminal)[!names(coef_table_terminal) %in% min_stay_cats]
  coef_table_terminal <- melt(coef_table_terminal, id.vars = columns)
  if (normalized) {
    coef_table_terminal[!is.na(value), b0_norm := b0_norm + value]
  } else {
    coef_table_terminal[!is.na(value), b0 := b0 + value]
  }
  coef_table_terminal[,c("value") := NULL]
  coef_table_terminal[,variable := gsub("MIN_STAY", "", variable)]
  setnames(coef_table_terminal, "variable", "MIN_STAY")
}


# Create data for prediction
min_stay_vals <- tmp[,unique(MIN_STAY)]
if (normalized) {
  data_tmp <- CJ(Price_norm = seq(0, 1, 0.01), MIN_STAY = min_stay_vals)
} else {
  Price_min <- tmp[,min(Price)]
  Price_max <- tmp[,max(Price)]
  data_tmp <- CJ(Price = seq(Price_min, Price_max, length.out = 200), MIN_STAY = min_stay_vals)
}
if (normalized) {
  this_key <- intersect(names(data_tmp), names(Price_min_max))
  if (length(this_key) == 0) {
    data_tmp[,k := 1]
    Price_min_max[,k := 1]
    this_key <- "k"
  }
  setkeyv(Price_min_max, this_key)
  setkeyv(data_tmp, this_key)
  data_tmp <- Price_min_max[data_tmp, allow.cartesian = TRUE]
  Price_min_max[,k := NULL]
}
this_key <- intersect(names(data_tmp), names(coef_table_terminal))
if (length(this_key) == 0) {
  data_tmp[,k := 1]
  coef_table_terminal[,k := 1]
  this_key <- "k"
}
setkeyv(data_tmp, this_key)
setkeyv(coef_table_terminal, this_key)
fit_tree <- data_tmp[coef_table_terminal, allow.cartesian = TRUE]
if (length(this_key) == 0) {
  fit_tree[,k := NULL]
  coef_table_terminal[,k := NULL]
}
fit_tree[,Price := Price_min + Price_norm * (Price_max - Price_min)]
if (normalized) {
  fit_tree[,b0 := b0_norm - b1_norm * (Price_min/(Price_max - Price_min))]
  fit_tree[,b1 := b1_norm/(Price_max - Price_min)]
  fit_tree[,demand := exp(b0_norm + b1_norm * Price_norm)]
}
fit_tree[,demand := exp(b0 + b1 * Price)]
to_remove <- c("terminal")
fit_tree[,c(to_remove) := NULL]
fit_tree <- unique(fit_tree)


# Plotting of groups
nodes <- fit_tree[,unique(node)]
pdf (width = 13, "Tree_booking_curves.pdf")
  for (i in 1:length(nodes)) {
  #for (i in 1:1) {
    node_content <- unique(tmp[node == nodes[i], .SD, .SDcol = min_max_key])
    node_content[,dummy := "yes"]
    setkeyv(node_content, min_max_key)
    setkeyv(fit_tree, min_max_key)
    fit_tree <- node_content[fit_tree]
    ind_remove <- fit_tree[,which(node == nodes[i] & is.na(dummy))]
    if (length(ind_remove) > 0) {
      fit_tree <- fit_tree[-ind_remove]
      fit_tree[,dummy := NULL]
    }
    this_node <- unique(fit_tree[node == nodes[i], node])
    u1 <- ggplot(tmp[node == this_node], aes(x = Price, y = Coupons.cumsum, group = GRP, col = GRP)) + geom_point(shape = 4) 
    u1 <- u1 + geom_line(data = unique(fit_tree[node == nodes[i], list(GRP, Price, demand)]), aes(x = Price, y = demand, group = GRP, col = GRP), lty = 2)
    u1 <- u1 + xlab("Price")
    u1 <- u1 + theme_bw()
    u1 <- u1 + theme(plot.title = element_text(size = 10))
    
    # These operations for title only work if tree only contains set operations
    this_title <- unique(fit_tree[node == nodes[i], split])
    this_title <- strsplit(this_title, "&")[[1]]
    var <- values <- list()
    for (ii in seq_along(this_title)) {
      this_title_tmp <- strsplit(this_title[ii], " %in% ")[[1]]
      var[[ii]] <- gsub(" ", "", this_title_tmp[1])
      values_tmp <- this_title_tmp[2]
      values_tmp <- gsub("c\\(", "", values_tmp)
      values_tmp <- gsub("\\)", "", values_tmp)
      values_tmp <- gsub("\"", "", values_tmp)   
      values_tmp <- strsplit(values_tmp , ", ")[[1]]
      values[[ii]] <- gsub(" ", "", values_tmp[values_tmp != "NA"])
    }
    vars <- unique(unlist(var))
    this_title <- list()
    for (ii in seq_along(vars)) {
      ind_var <- which(unlist(var) == vars[ii])
      this_title[[ii]] <- paste0(vars[ii], " = ", paste0(Reduce(intersect, values[ind_var]), collapse = ", "))
    }
    this_title <- paste0(unlist(this_title), collapse = ", ")
    this_title <- paste0(this_title, ", node = ", this_node)
    u1 <- u1 + ggtitle(this_title)
    u1 <- u1 + ylab("Demand")
    print(u1)
  }
dev.off()


###
# Optimisation of Price-Points (incl. entry-level)
cost_table <- data.table(sales_airport = c("ZRH-YUL", "YUL-ZRH", "ZRH-YUL", "YUL-ZRH", "ZRH-EWR", "EWR-ZRH", "ZRH-EWR", "EWR-ZRH"), 
                         compartment = c("Economy", "Economy", "Business", "Business", "Economy", "Economy", "Business", "Business"),
                         currency = c("CHF", "CAD", "CHF", "CAD", "CHF", "USD", "CHF", "USD"),
                         cost = c(80.05, 107.44, 80.05, 107.44, 98.75, 99.3, 98.75, 99.3))
cost_table[,sales_airport := factor(sales_airport, levels = sales_airport_levels, labels = sales_airport_levels)]
cost_table[,compartment := factor(compartment, levels = compartment_levels, labels = compartment_levels)]

sales_airport <- c("ZRH-YUL", "YUL-ZRH", "ZRH-EWR", "EWR-ZRH")
compartment <- compartment_levels
DEP_MONTH <- dep_month_levels
DEP_DAY <- dep_day_levels
dep_year <- "2018"
rbd_structure <- unique(data[SALES_AIRPT_OD %in% sales_airport & DEP_YEAR %in% dep_year, list(SALES_AIRPT_OD, RBD, MIN_STAY, MIN_STAY_LENGTH, MIN_STAY_DAY, DEP_DATE)])
rbd_structure[,DEP_DATE_MAX := max(DEP_DATE), by = c("SALES_AIRPT_OD", "RBD")]
rbd_structure <- rbd_structure[DEP_DATE == DEP_DATE_MAX]
rbd_structure[,c("DEP_DATE", "DEP_DATE_MAX") := NULL]
PATTERN_RBD_ECONOMY <- c("K", "L", "T", "S", "W", "V",
                         "Q", "H", "U", "M", "B", "Y")
PATTER_RBD_BUSINESS <- c("P", "Z", "D", "C", "J")
rbd_order <- c(PATTERN_RBD_ECONOMY, PATTER_RBD_BUSINESS)
rbd_structure[,compartment := ifelse(RBD %in% PATTERN_RBD_ECONOMY, "Economy", "Business")]
rbd_structure[,RBD := ordered(RBD, levels = rbd_order)]
rbd_structure[,RBD_order := as.numeric(RBD)]
setkeyv(rbd_structure, c("RBD"))
length(rbd_order)
setnames(rbd_structure, "SALES_AIRPT_OD", "sales_airport")
predict_data <- CJ(sales_airport = sales_airport, 
                   compartment = compartment, 
                   DEP_MONTH = DEP_MONTH, 
                   DEP_DAY = DEP_DAY, 
                   DEP_YEAR = dep_year)

this_key <- intersect(names(rbd_structure), names(predict_data))
setkeyv(rbd_structure, this_key)
setkeyv(predict_data, this_key)
predict_data <- rbd_structure[predict_data, allow.cartesian = TRUE]
ind_remove <- predict_data[,which(compartment == "Economy" & RBD %in% PATTER_RBD_BUSINESS)]
if (length(ind_remove) > 0) {
  predict_data <- predict_data[-ind_remove]
}
ind_remove <- predict_data[,which(compartment == "Business" & RBD %in% PATTERN_RBD_ECONOMY)]
if (length(ind_remove) > 0) {
  predict_data <- predict_data[-ind_remove]
}
predict_data[,sales_airport := factor(sales_airport, levels = sales_airport_levels, labels = sales_airport_levels)]
predict_data[,MIN_STAY := factor(MIN_STAY, levels = min_stay_levels, labels = min_stay_levels)]
predict_data[,MIN_STAY_LENGTH := factor(MIN_STAY_LENGTH, levels = min_stay_length_levels, labels = min_stay_length_levels)]
predict_data[,MIN_STAY_DAY := factor(MIN_STAY_DAY, levels = min_stay_day_levels, labels = min_stay_day_levels)]
predict_data[,compartment := factor(compartment, levels = compartment_levels, labels = compartment_levels)]
predict_data[,DEP_MONTH := factor(DEP_MONTH, levels = dep_month_levels, labels = dep_month_levels)]
predict_data[,DEP_DAY := factor(DEP_DAY, levels = dep_day_levels, labels = dep_day_levels)]
predict_data[,DEP_YEAR := factor(DEP_YEAR, levels = dep_year_levels, labels = dep_year_levels)]
predict_data[,Price_norm := 0]
predict_data[,node := predict(mod, newdata = predict_data, type = "node")]
predict_data[,Price_norm := NULL]

predict_data[,DEP_DAY := factor(DEP_DAY, levels = dday_order, labels = dday_order)]
predict_data[,DEP_MONTH := as.numeric(DEP_MONTH)]

ods <- sales_airport
pdf (width = 13, height = 20, "node_distribution.pdf")
  for (i in seq_along(ods)) {
    u1 <- ggplot(predict_data[sales_airport == ods[i]], aes(x = DEP_MONTH, y = node)) + geom_line()
    u1 <- u1 + theme_bw()
    u1 <- u1 + facet_grid(RBD ~ DEP_DAY)
    u1 <- u1 + scale_x_continuous(breaks = 1:12)
    u1 <- u1 + xlab("departure month (1 = january, 12 = december)")
    u1 <- u1 + ylab("pricing cluster")
    u1 <- u1 + ggtitle(paste0("sales_airport = ", ods[i]))
    print(u1)
  }
dev.off()
#predict_data[,DEP_MONTH := factor(DEP_MONTH, levels = dep_month_levels, labels = dep_month_levels)]
#predict_data[,DEP_DAY := factor(DEP_DAY, levels = dep_day_levels, labels = dep_day_levels)]


# Merge cost data to price prediction-data
this_key <- intersect(names(cost_table), names(predict_data))
setkeyv(cost_table, this_key)
setkeyv(predict_data, this_key)
predict_data <- cost_table[predict_data]
predict_data[,cost := cost/2]


# Add Price_min, Price_max if normalized
if (normalized) {
  setkeyv(predict_data, min_max_key)
  setkeyv(Price_min_max, min_max_key)
  predict_data <- Price_min_max[predict_data]
}


# register backend
no.cores <- 100
#no.cores <- 1
registerDoParallel(cores = no.cores)


# Calculate optimal vertical upsells
setkeyv(predict_data, NULL)
predict_data[,GRP := .GRP, by = c("sales_airport", "compartment", "DEP_MONTH", "DEP_DAY")]
predict_data[,GRP := as.numeric(GRP)]
predict_data[,No_segments := length(unique(node)), by = "GRP"]
predict_data[,table(No_segments)]

grps <- predict_data[,unique(GRP)]
base_size <- 20
text_size <- 20
k <- 1
pdf (height = 20, width = 30, file = "Result_of_Price_Optimisation_Tree.pdf")
  for (k in grps) {
  #for (k in grps) {
  #for (k in 1:10) {
    print(paste0("Processing grp = ", k, " / ", length(grps)))
  
    process_data <- predict_data[GRP == k]
    process_data <- process_data[order(RBD)]

    # Determination of restrictions
    Number_Price_Points <- nrow(process_data)
    this_od <- process_data[,unique(sales_airport)]
    this_comp <- process_data[,unique(compartment)]
    Min_Price <- Price_min_max[sales_airport %in% this_od & compartment == this_comp, Price_min]
    Max_Price <- Price_min_max[sales_airport %in% this_od & compartment == this_comp, Price_max]
    A2 <- diag(Number_Price_Points - 2)
    A3 <- diff(diag(Number_Price_Points - 1), differences = 1)
    A3 <- rbind(A3, t(matrix(c(1, rep(0, Number_Price_Points - 2)))))
    A3 <- A3[,-ncol(A3), drop = FALSE]

    A <- rbind(A2, A3)
    b <- seq(length = dim(A)[1], from = 0, by = 0)
    b[length(b) - 1] = - Max_Price
    b[length(b)] = Min_Price
    b <- as.vector(b)


    ###
    # Matrices defining what segment can buy which price-point

    # Number of segments
    this_nodes <- process_data[,unique(node)]
    no_seg <- length(this_nodes)

    # Matrices of segment-parameters
    if (normalized) {
      coef_names <- c("b0_norm", "b1_norm")
    } else {
      coef_names <- c("b0", "b1")
    }
    PAR <- coef_table_terminal[node %in% this_nodes, .SD, .SDcol = coef_names]
    PAR <- t(PAR)
    
    if (normalized) {
      b0 <- PAR[1,,drop = FALSE] - PAR[2,,drop = FALSE] * (Min_Price/(Max_Price - Min_Price))
      b1 <- PAR[2,,drop = FALSE]/(Max_Price - Min_Price)
      PAR <- rbind(b0, b1)
      dimnames(PAR)[[2]] <- this_nodes
    }
    
    # Define every possible position of price-points the restrictive segments is able to buy.
    ind_lowest_rbd <- process_data[,which(RBD_order == min(RBD_order))]
    node_unrestr <- process_data[ind_lowest_rbd, node]
    number_restr <- process_data[node != node_unrestr, table(node)]
    restr_nodes <- names(number_restr)
    sets <- data.table(k = 1)
    # Check if there is only segment. If so, simple price-optimization applies
    add_highest_price <- TRUE
    adj <- 1
    if (no_seg > 1) {
      for (g in 1:length(restr_nodes)) {
        sets_tmp <- list()
        if (number_restr[g] > 1) {
          for (n in (number_restr[g] - adj):(number_restr[g] - adj)) {
            if (add_highest_price) {
              sets_tmp[[n]] <- data.table(cbind(t(combn(Number_Price_Points - adj, n)), as.numeric(Number_Price_Points)))
              setnames(sets_tmp[[n]], names(sets_tmp[[n]]), paste0("seg", g, "_", 1:ncol(sets_tmp[[n]])))
            }
          }
          sets_tmp <- rbindlist(sets_tmp, fill = TRUE, use.names = TRUE)
        } else {
          sets_tmp <- data.table(as.numeric(Number_Price_Points))
          setnames(sets_tmp, names(sets_tmp), paste0("seg", g, "_", 1:ncol(sets_tmp)))
        }
        sets_tmp[,k := 1]
        setkeyv(sets, "k")
        setkeyv(sets_tmp, "k")
        sets <- sets_tmp[sets, allow.cartesian = TRUE]
      }
      sets[,k := NULL]
    } else {
      sets <- data.table(cbind(t(combn(Number_Price_Points - adj, Number_Price_Points - adj)), as.numeric(Number_Price_Points)))
    }

    # Start optimization (possibly in parallel)
    results_optimization <- foreach (i = 1:nrow(sets), 
                                     .packages = c("data.table"),
                                     .verbose = TRUE,
                                     #.errorhandling = "pass",
                                     .init = rep(list(list()), 3),
                                     .multicombine = TRUE,
                                     .combine = 'comb'
                                     ) %dopar% {
      process_sets(i = i, 
                   Number_Price_Points = Number_Price_Points,
                   sets = sets,
                   VERBOSE = FALSE,
                   PAR = PAR,
                   A = A,
                   b = b,
                   Min_Price = Min_Price,
                   Max_Price = Max_Price,
                   no_seg = no_seg)
    }
    
    Wl <- results_optimization[[1]]
    value <- do.call("rbind", results_optimization[[2]])
    par <- do.call("rbind", results_optimization[[3]])
    results <- as.data.table(cbind(value, par))
    setnames(results, names(results), c("value",  paste0("V", 1:(Number_Price_Points - 2))))
  
    ind_max <- results[,which(value == max(value))]
    ind_max <- ind_max[1]
    par_max <- results[ind_max, as.numeric(.SD), .SDcol = paste0("V", 1:(Number_Price_Points - 2))]
    W = Wl[[ind_max]]
    p <- c(Min_Price, par_max, Max_Price) 

    # Gather results
    rect_data <- text_data <- plot_data <- list()
    for (g in 1:no_seg) {
      p_g <- p
      p_g <- p_g[Wl[[ind_max]][[g]] == 1]
      rect_data[[g]] <- data.table(Price.xmin = c(0, p_g[-length(p_g)]),
                                   Price.xmax = p_g)
      rect_data[[g]][,Demand.Predict.ymax := exp(PAR[1,g] + PAR[2,g] * Price.xmax)]
      rect_data[[g]][,Demand.Predict.ymin := 0]
      rect_data[[g]][,segment := g]
      
      text_data[[g]] <- data.table(p = p_g)
      text_data[[g]][,demand := exp(PAR[1,g] + PAR[2,g] * p)]
      text_data[[g]][,segment := g]
      
      plot_data[[g]] <- data.table(p = seq(Min_Price, Max_Price, 1))
      plot_data[[g]][,demand := exp(PAR[1,g] + PAR[2,g] * p)]
      plot_data[[g]][,segment := g]
    }
    rect_data <- rbindlist(rect_data)
    rect_data[,segment := as.factor(segment)]

    text_data <- rbindlist(text_data)
    text_data[,segment := as.factor(segment)]

    plot_data <- rbindlist(plot_data)
    plot_data[,segment := as.factor(segment)]

    this_title <- paste0("Sales Airport = ", this_od)
    this_title <- paste0(this_title, ", compartment = ", this_comp)
    this_title <- paste0(this_title, ", dep. month = ", process_data[,unique(DEP_MONTH)])
    this_title <- paste0(this_title, ", dep. day = ", process_data[,unique(DEP_DAY)])
    
    u1 <- ggplot() 
    u1 <- u1 + ggtitle(this_title)
    u1 <- u1 + geom_line(data = plot_data, aes(x = p, y = demand, col = segment), lty = 1) 
    u1 <- u1 + geom_rect(data = rect_data, aes(xmin = Price.xmin, 
                                               xmax = Price.xmax, 
                                               ymin = Demand.Predict.ymin,
                                               ymax = Demand.Predict.ymax,
                                               fill = segment), 
                                               col = "black",
                                               lwd = 0.3,
                                               alpha = 0.3)

    u1 <- u1 + theme_bw(base_size = base_size)
    u1 <- u1 + theme(text = element_text(size = text_size), 
                     strip.text.x = element_text(size = text_size, angle = 360, colour = "black"), 
                     strip.text.y = element_text(size = text_size, angle = 360, colour = "black"), 
                     axis.text.x = element_text(size = text_size, angle = 30, hjust = 1), 
                     axis.text.y = element_text(size = text_size, angle = 360, hjust = 1), 
                     axis.title.y = element_text(size = text_size, angle = 90))

    offset <- 0
    u1 <- u1 + geom_label(data = text_data, aes(x = (1 + offset ) * p, 
                                                y = (1 + offset) * demand, 
                                                label = round(p, 2)), 
                                                size = 10, 
                                                angle = 45,
                                                fontface = "bold")
    print(u1)
    
    # Add prices to data
    if (no_seg > 1) {
      predict_data[GRP == k, paste0("segment", 1:no_seg) := W]
    } else {
      predict_data[GRP == k, segment1 := W[[1]]]    
    }
    predict_data[GRP == k, Price := p]
  }
dev.off()


# Save result
write.table(predict_data,
            file = paste0(av_data, "optimal_prices.csv"),
            sep = ";",
            dec = ".",
            col.names = TRUE,
            row.names = FALSE)
predict_data <- fread(paste0(av_data, "optimal_prices.csv"))


# Plot distribution of optimal prices
setwd(av_plot)

ods <- predict_data[,unique(sales_airport)]
pdf (width = 13, "price_distribution.pdf")
  for (i in seq_along(ods)) {
    u1 <- ggplot(predict_data[sales_airport == ods[i]], aes(x = DEP_MONTH, y = Price, col = RBD)) + geom_line()
    u1 <- u1 + theme_bw()
    u1 <- u1 + facet_grid(compartment ~ DEP_DAY)
    u1 <- u1 + scale_x_continuous(breaks = 1:12)
    u1 <- u1 + xlab("departure month (1 = january, 12 = december)")
    u1 <- u1 + ylab("Price")
    u1 <- u1 + ggtitle(paste0("sales_airport = ", ods[i]))
    print(u1)
  }
dev.off()

ind_seg_names <- grep("segment", names(predict_data))
seg_names <- names(predict_data)[ind_seg_names]
seg_names <- seg_names[seg_names != "No_segments"]
columns <- c("sales_airport", 
             "DEP_MONTH", 
             "RBD", 
             "compartment",
             "DEP_DAY", 
             seg_names)
tmp <- predict_data[,.SD,.SDcol = columns]
tmp <- melt(tmp, id.vars = columns[!columns %in% seg_names])
tmp <- tmp[!is.na(value)]
tmp <- tmp[value > 0]
tmp[,value := NULL]
tmp <- tmp[,variable := as.numeric(gsub("segment", "", variable))]
dday_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
tmp[,DEP_DAY := ordered(DEP_DAY, levels = dday_order)]
PATTERN_RBD_ECONOMY <- c("K", "L", "E", "T", "S", "W", "V",
                         "Q", "G", "H", "U", "M", "B", "Y")
PATTERN_RBD_ECONOMY <- rev(PATTERN_RBD_ECONOMY)
tmp[,RBD := ordered(RBD, levels = PATTERN_RBD_ECONOMY)]
tmp[,variable := max(as.numeric(variable)), by = c("sales_airport", "DEP_MONTH", "RBD", "compartment", "DEP_DAY")]
tmp <- unique(tmp)

pdf (width = 13, "segment_distribution.pdf")
  for (i in seq_along(ods)) {
    u1 <- ggplot(tmp[sales_airport == ods[i]], aes(x = DEP_MONTH, y = variable)) + geom_line()
    u1 <- u1 + theme_bw()
    u1 <- u1 + facet_grid(RBD ~ DEP_DAY)
    u1 <- u1 + scale_y_continuous(breaks = 1:3)
    u1 <- u1 + scale_x_continuous(breaks = 1:12)
    u1 <- u1 + xlab("departure month (1 = january, 12 = december)")
    u1 <- u1 + ylab("No. segments")
    u1 <- u1 + ggtitle(paste0("sales_airport = ", ods[i]))
    print(u1)
  }
dev.off()




this_key <- intersect(names(predict_data), names(Price_min_max))
setkeyv(Price_min_max, this_key)
setkeyv(predict_data, this_key)
predict_data <- Price_min_max[predict_data]
predict_data[,Price_norm := (Price - Price_min)/(Price_max - Price_min), by = "Price"]
process_data <- predict_data[sales_airport == "ZRH-EWR" & compartment == "Economy" & DEP_MONTH == 9 & DEP_DAY == "Wed"]




# debug
theta = sort(x)
f = target
grad = gradient
ui = A
ci = b
mu = 1e-04
control = list(fnscale = -1)
outer.eps = 1e-05
outer.iterations = 1000
arguments <- 
  list(VERBOSE = VERBOSE,
       PAR = PAR,
       W = W,
       no_seg = no_seg, 
       Min_Price = Min_Price,
       Max_Price = Max_Price)
hessian = FALSE

