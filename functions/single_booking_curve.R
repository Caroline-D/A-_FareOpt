#screen -S A++ R
screen -S A++ /home/U832473/R-3.5.3/bin/R

options(scipen = 999)

library(data.table)
library(ggplot2)
library(partykit)
library(plyr)
library(reshape2)
library(stats)
library(Rcpp)
library(scam)

av <- "/home_app2/U832473/A++/functions/"
source(paste0(av, "/constrOptim_custom.R"))

av <- "/home_app2/U832473/A++/data/"
setwd(av)





# Read data
files <- list.files()
index <- grep("merged_tickets_", files)
files <- files[index]

data <- list()
for (i in 1:length(files)) {
  data[[i]] <- readRDS(paste0(av, files[i]))
}
data <- rbindlist(data)
data[,CPN := 1]


# Set wd
av <- "/home_app2/U832473/A++/"
setwd(av)


# AC methodology (one booking-curve)
setnames(data, "ENDPREIS", "Price")
tmp <- data[,list(Coupons = sum(CPN)), by = c("Price", "RBD", "SALES_AIRPT_OD")]
tmp <- tmp[order(-Price), Coupons.cumsum := cumsum(Coupons), by = "SALES_AIRPT_OD"]
tmp[,GRP := .GRP, by = "SALES_AIRPT_OD"]


# Estimation
tmp[,SALES_AIRPT_OD := as.factor(SALES_AIRPT_OD)]
mod_glm <- glm(Coupons.cumsum ~ Price * SALES_AIRPT_OD, family = poisson(), data = tmp)
tmp[,fit_glm := predict.glm(mod_glm, type = "response")]
mod_scam <- scam(Coupons.cumsum ~ s(Price, k = 20, bs = "mpd", by = SALES_AIRPT_OD) + SALES_AIRPT_OD, family = poisson(), data = tmp)
tmp[,fit_scam := predict.scam(mod_scam, type = "response")]


# Plot
u1 <- ggplot(tmp, aes(x = Price, y = Coupons.cumsum)) + geom_line()
u1 <- u1 + facet_wrap(~SALES_AIRPT_OD, scale = "free_y") + theme_bw()
u1 <- u1 + geom_line(aes(x = Price, y = fit_glm), col = "red", lty = 2)
u1 <- u1 + geom_line(aes(x = Price, y = fit_scam), col = "green", lty = 2)
u1 <- u1 + xlab("Price")
u1 <- u1 + ylab("Demand")
pdf (width = 13, "single_booking_curve.pdf")
  print(u1)
dev.off()


###
# Optimisation of Price-Points (incl. entry-level)
fkt <- function(x = numeric(),
                arguments = list()) {
  function.env <- environment()
  list2env(arguments, envir = function.env)
  val <- sum(demand(x = x, arguments) * diff(c(Min_Price, x)))
  val <- val + Min_Price * demand(x = Min_Price, arguments)
  val <- val + (Max_Price - x[length(x)]) * demand(x = Max_Price, arguments)
  val
}

demand <- function(x = numeric(),
                   arguments = list()) {
  function.env <- environment()
  list2env(arguments, envir = function.env)
  dat <- data.table(Price = x, SALES_AIRPT_OD = sales_airpt_od)
  predict(mod, newdata = dat, type = "response")
}

Number_Price_Points_data <- tmp[,list(Number_Price_Points = length(unique(RBD))), by = "SALES_AIRPT_OD"]

text_size <- 15
base_size <- 18
alpha <- 0.01
this.k <- 5
results <- rect_data <- fitted <- text_data <- list()
i <- j <- 1
grps <- tmp[,unique(GRP)]
pdf (height = 20, width = 30, file = "Result_of_Price_Optimisation_single_curve.pdf")
  for (i in seq_along(grps)) {
    print(paste0("GRP = ", grps[i]))
    
    plot_data <- tmp[GRP == i]
    sales_airpt_od <- tmp[GRP == i, unique(SALES_AIRPT_OD)]
    Number_Price_Points <- Number_Price_Points_data[SALES_AIRPT_OD == sales_airpt_od, sum(Number_Price_Points)]
    # Starting points for the optimizing algorithm 
    # (have to be between Min_Price and Max_Price)
    Min_Price <- plot_data[,min(Price)]
    Max_Price <- plot_data[,max(Price)] 
    x <- seq(Min_Price * 1.0001, Max_Price/2, length.out = Number_Price_Points - 2)

    # Determination of restrictions
    A2 <- diag(Number_Price_Points - 2)
    A3 <- diag(-1, Number_Price_Points - 1, Number_Price_Points - 2)
    for (m in 1:(Number_Price_Points - 3)) {A3[m, m + 1] <- 1}
    A3[Number_Price_Points - 1, 1] <- 1
    A <- rbind(A2, A3)
    b <- seq(length = dim(A)[1], from = 0, by = 0)
    b[length(b) - 1] = -Max_Price
    b[length(b)] = Min_Price
    b <- as.vector(b)

    arguments <- list(mod = mod_scam,
                      SALES_AIRPT_OD = sales_airpt_od,
                      Min_Price = Min_Price, 
                      Max_Price = Max_Price)
                      
    Gen <- constrOptim_custom(theta = sort(x), 
                              f = fkt, 
                              grad = NULL, 
                              ui = A,
                              ci = b,
                              control = list(fnscale = -1), 
                              outer.eps = 1e-05, 
                              outer.iterations = 1000,
                              arguments = arguments)
    
    results[[i]] <- data.table(Price = Gen$par, GRP = grps[i])

    # Results
    rect_data[[i]] <- data.table(Price.xmin = c(0, Min_Price, Gen$par), Price.xmax = c(Min_Price, Gen$par, Max_Price))
    rect_data[[i]][,Demand.Predict.ymax := demand(x = Price.xmax, arguments = arguments), by = "Price.xmax"]
    rect_data[[i]][,Demand.Predict.ymin := 0]
    rect_data[[i]][,GRP := grps[i]]
    fitted[[i]] <- data.table(Price = seq(Min_Price - 0.1 * Min_Price, Max_Price, length.out = 100))
    fitted[[i]][,Demand.Predict := demand(x = Price, arguments = arguments), by = "Price"]
    fitted[[i]][,GRP := grps[i]]
    text_data[[i]] <- data.table(optimal_price = c(Min_Price, Gen$par, Max_Price))
    text_data[[i]][,optimal_demand := demand(x = optimal_price, arguments = arguments)]
    text_data[[i]][,GRP := grps[i]]    
    
    u1 <- ggplot() 
    this_title <- paste0("SALES_AIRPT_OD = ", sales_airpt_od)
    u1 <- u1 + geom_line(data = plot_data, aes(x = Price, y = Coupons.cumsum))
    u1 <- u1 + ggtitle(this_title)
    u1 <- u1 + ylab("Demand")
    u1 <- u1 + geom_line(data = fitted[[i]], aes(x = Price, y = Demand.Predict), lwd = 0.3, col = "red")
    u1 <- u1 + geom_rect(data = rect_data[[i]], aes(xmin = Price.xmin, 
                                                    xmax = Price.xmax, 
                                                    ymin = Demand.Predict.ymin,
                                                    ymax = Demand.Predict.ymax), 
                                                    lwd = 0.3, 
                                                    fill = "blue", 
                                                    alpha = 0.03, 
                                                    color = "darkblue")
    u1 <- u1 + theme_bw(base_size = base_size)
    u1 <- u1 + theme(text = element_text(size = text_size), 
                         strip.text.x = element_text(size = text_size, angle = 360, colour = "black"), 
                         strip.text.y = element_text(size = text_size, angle = 360, colour = "black"), 
                         axis.text.x = element_text(size = text_size, angle = 30, hjust = 1), 
                         axis.text.y = element_text(size = text_size, angle = 360, hjust = 1), 
                         axis.title.y = element_text(size = text_size, angle = 90))
    u1 <- u1 + geom_text(data = text_data[[i]], aes(x = optimal_price + optimal_price * 0.11, 
                                                    y = optimal_demand + optimal_demand * 0.11, 
                                                    label = round(optimal_price, 2)), 
                                                    size = 7, angle = 30)
    print(u1)
  }
dev.off()


results <- do.call("rbind", results)
write.table(results,
            file = "optimal_prices.csv",
            sep = ";",
            dec = ".",
            col.names = TRUE,
            row.names = FALSE)





# debug
theta = sort(x)
f = fkt
grad = NULL
ui = A
ci = b
mu = 1e-04
control = list(fnscale = -1)
outer.eps = 1e-05
outer.iterations = 1000
arguments = list(mod = mod_scam,
                 SALES_AIRPT_OD = sales_airpt_od,
                 Min_Price = Min_Price, 
                 Max_Price = Max_Price)
hessian = FALSE

