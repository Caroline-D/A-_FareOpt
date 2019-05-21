
rm(list = ls())
options(scipen = 999)

# Loading of packages
library(data.table)
library(ggplot2)
library(mgcv)
library(partykit)


# Generation of data
normalized <- FALSE
version <- "a"
flight <- 100:200
t <- seq(0, 100, 20)
min_stay_levels <- c(0:4)
N_p <- 5
data <- list()
for (i in seq_along(min_stay_levels)) {
	set.seed(1337)
	if (version == "a") {
		data[[i]] <- CJ(flight = flight, t = t, Price = rnorm(N_p, 1000 - min_stay_levels[i] * 200, 150), min_stay = min_stay_levels[i])
	}
	if (version == "b") {
		data[[i]] <- CJ(flight = flight, t = t, Price = seq(50, 1000, 50), min_stay = min_stay_levels[i])
	}
}
data <- rbindlist(data)


# Setting up the parameters
base_slope <- -0.001
min_stay_slopes <- base_slope * (min_stay_levels + 1)
t_par <- -0.02
int <- 1


# Create theoretical values and actual bookings 
# (this is already the cumsum version)
data[,lambda := exp(int + t_par * t + base_slope * (min_stay + 1) * Price)]
this_key <- c("min_stay", "t", "flight")
setkeyv(data, this_key)
data[order(-Price), lambda_incr := diff(c(0, lambda)), by = this_key]
this_key <- c("min_stay", "t", "flight", "Price")
data[,bkd_incr := rpois(1,lambda_incr), by = this_key]
this_key <- c("min_stay", "t", "flight")
data[order(-Price), bkd := cumsum(bkd_incr), by = this_key]
data[order(-Price), lambda := cumsum(lambda_incr), by = this_key]


# Normalize
if (normalized) {
	data[,Price := (Price - min(Price))/(max(Price) - min(Price)), by = "min_stay"]
}


# Plotting of data
data[,min_stay := as.factor(min_stay)]
mod_bkd <- glm(bkd ~ t + Price * min_stay, family = poisson(), data = data)
summary(mod_bkd)
data[,bkd_fit := predict.glm(mod_bkd, type = "response")]
u1 <- ggplot(data, aes(x = Price, y = bkd, col = min_stay)) + geom_point(size = 2) + theme_bw()
u1 <- u1 + geom_line(aes(x = Price, y = bkd_fit, col = min_stay), lty = 2)
u1 <- u1 + geom_line(aes(x = Price, y = lambda, col = min_stay))
u1 <- u1 + facet_wrap(~t)
u1


###
# Generate aggregated data by min_stay

# Model for sum
tmp <- data[,list(bkd_sum = sum(bkd), lambda_sum = sum(lambda)), by = c("Price", "min_stay", "flight")]
mod_sum <- glm(bkd_sum ~ Price* min_stay, family = poisson(), data = tmp)
summary(mod_sum)
tmp[,bkd_sum_fit := predict.glm(mod_sum, type = "response")]
u2 <- ggplot(tmp, aes(x = Price, y = bkd_sum, col = min_stay)) + geom_point(size = 2) + theme_bw()
u2 <- u2 + geom_line(aes(x = Price, y = bkd_sum_fit, col = min_stay), lty = 2)
u2 <- u2 + geom_line(aes(x = Price, y = lambda_sum, col = min_stay))
u2
tmp[,grp := .GRP, by = c("flight", "min_stay")]
u3 <- ggplot(tmp, aes(x = Price, y = bkd_sum, group = flight)) + geom_line(alpha = 0.3,col = "grey") + theme_bw()
u3 <- u3 + facet_wrap(~min_stay)
u3 <- u3 + geom_line(aes(x = Price, y = bkd_sum_fit), lty = 2, col = "red")
u3 <- u3 + geom_line(aes(x = Price, y = lambda_sum), col = "red")
u3


# Estimation with glmtree
class(eq_mob <- bkd_sum ~ Price | min_stay)
mod <- glmtree(eq_mob, 
               data = tmp,
               family = poisson(), 
               alpha = 0.05,
               bonferroni = TRUE,
               verbose = TRUE,
               prune = "BIC",
               maxdepth = 7,
               #minsize = min.size,
               breakties = TRUE,
               restart = TRUE,
               cores = 1)
plot(mod)
tmp[,node := predict(mod, newdata = tmp, type = "node")]


# Create table of model coefficients and splitting-rules
coef_table <- list()
number.of.nodes <- length(mod)
for (i.node in c(1:number.of.nodes)) {
  print(paste0("Node = ", i.node, "."))      
  if (is.terminal(mod[[i.node]]$node)) {
    kids <- NULL
  } else {
    kids.ind <- c(kids_node(mod[[i.node]]$node)[[1]]$id,
                  kids_node(mod[[i.node]]$node)[[2]]$id)
    kids.labels <- names(mod[[i.node]])
    kids <- as.numeric(kids.labels[kids.ind])
  }
  coef_mod <- as.data.table(t(coef(mod, node = i.node)))
  # Check if the labels of the coefficients are accurate given that
  # a category may be missing after a sequence of splits
  split <- partykit:::.list.rules.party(mod, i.node)
  
  coef_mod[,node := i.node]
  coef_mod[,terminal := ifelse(is.null(kids), TRUE, FALSE)]
  coef_mod[,split := split]
  coef_table[[i.node]] <- coef_mod
}
coef_table <- rbindlist(coef_table)
coef_table_terminal <- coef_table[terminal == TRUE]
new.names <- names(coef_table)
new.names <- gsub("\\(", "", new.names)
new.names <- gsub("\\)", "", new.names)
setnames(coef_table_terminal , names(coef_table_terminal), new.names)
if (normalized) {
  setnames(coef_table_terminal, c("Intercept", "Price_norm"), c("b0_norm", "b1_norm"))
} else {
  setnames(coef_table_terminal, c("Intercept", "Price"), c("b0", "b1"))
}
coef_table_terminal[,GRP := .GRP, by = "split"]


# If MIN_STAY is part of the model-equation
min_stay_cats <- paste0("min_stay", tmp[,unique(min_stay)])
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
coef_table_terminal[,variable := gsub("min_stay", "", variable)]
setnames(coef_table_terminal, "variable", "min_stay")


# Create data for prediction
id.key <- c("min_stay")
flight.key <- c("flight")
tmp[,ID := .GRP, by = c(flight.key, id.key)]
min_stay_vals <- tmp[,unique(min_stay)]
ids <- tmp[,unique(ID)]
if (normalized) {
  data_tmp <- CJ(ID = ids, Price_norm = seq(0, 1, 0.01), min_stay = min_stay_vals)
} else {
  Price_min <- tmp[,min(Price)]
  Price_max <- tmp[,max(Price)]
  data_tmp <- CJ(ID = ids, Price = seq(Price_min, Price_max, length.out = 25), min_stay = min_stay_vals)
}
info <- unique(tmp[,.SD,.SDcols = c("ID", id.key)])
this_key <- intersect(names(data_tmp), names(info))
setkeyv(data_tmp, this_key)
setkeyv(info, this_key)
data_tmp <- info[data_tmp]
if (normalized) {
  setkeyv(Price_min_max, "ID")
  setkeyv(data_tmp, "ID")
  data_tmp <- Price_min_max[data_tmp]
}
this_key <- intersect(names(data_tmp), names(coef_table_terminal))
if (length(this_key) == 0) {
  data_tmp[,k := 1]
  coef_table_terminal[,k := 1]
  this_key <- "k"
}
this_key <- intersect(names(data_tmp), names(coef_table_terminal))
setkeyv(data_tmp, this_key)
setkeyv(coef_table_terminal, this_key)
fit_tree <- data_tmp[coef_table_terminal, allow.cartesian = TRUE]
if (length(this_key) == 0) {
  fit_tree[,k := NULL]
}
if (normalized) {
  fit_tree[,demand := exp(b0_norm + b1_norm * Price_norm)]
  #fit_tree[,Price := Price_min + Price_norm * (Price_max - Price_min)]
  #fit_tree[,b0 := b0_norm - b1_norm * (Price_min/(Price_max - Price_min))]
  #fit_tree[,b1 := b1_norm/(Price_max - Price_min)
} else {
  fit_tree[,demand := exp(b0 + b1 * Price)]
}
to_remove <- c("terminal")
fit_tree[,c(to_remove) := NULL]
fit_tree <- unique(fit_tree)


# Plotting of groups
grps <- fit_tree[,unique(GRP)]
pdf (width = 13, "Demand_curves.pdf")
  for (i in 1:length(grps)) {
  #for (i in 1:1) {
    this_node <- unique(fit_tree[GRP == grps[i], node])
    if (normalized) {
    	u1 <- ggplot(tmp[node == this_node], aes(x = Price_norm, y = bkd_sum, col = min_stay, group = min_stay)) + geom_point(shape = 4) 
    	u1 <- u1 + geom_line(data = unique(fit_tree[GRP == grps[i], list(Price_norm, demand, min_stay)]), aes(x = Price_norm, y = demand, col = min_stay, group = min_stay))
    	u1 <- u1 + xlab("Price normalized")
    } else {
    	u1 <- ggplot(tmp[node == this_node], aes(x = Price, y = bkd_sum, col = min_stay, group = min_stay)) + geom_point(shape = 4) 
    	u1 <- u1 + geom_line(data = unique(fit_tree[GRP == grps[i], list(Price, demand, min_stay)]), aes(x = Price, y = demand, col = min_stay, group = min_stay))
    	u1 <- u1 + xlab("Price")
    }  
    #u1 <- u1 + facet_wrap(~min_stay)
    u1 <- u1 + theme_bw()
    u1 <- u1 + theme(plot.title = element_text(size = 8))
    u1 <- u1 + ggtitle(unique(fit_tree[GRP == grps[i], split]))
    u1 <- u1 + ylab("Demand")
    print(u1)
  }
dev.off()




















# Get slope value of smooth via numerical integration
min_price <- data_tmp[,min(price)]
max_price <- data_tmp[,max(price)]
tmp <- CJ(price = seq(min_price, max_price, length.out = 100), min_stay = as.factor(min_stay_levels))
tmp <- tmp[order(min_stay)]
tmp[,fit := predict(mod_cumsum, newdata = tmp)]
eps <- 0.00001
tmp[,price := price + eps]
tmp[,fit_eps := predict(mod_cumsum, newdata = tmp)]
tmp[,fit_deriv := (fit_eps - fit)/eps]
tmp[,price := price - eps]
tmp
u3 <- ggplot(tmp, aes(x = price, y = fit_deriv, col = min_stay, group = min_stay)) + geom_line()
u3 <- u3 + theme_bw()
u3




