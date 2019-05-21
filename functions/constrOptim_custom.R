constrOptim_custom <- function (theta = numeric(), 
                                f, 
                                f_grad, 
                                ui = numeric(), 
                                ci = numeric(), 
                                mu = 1e-04, 
                                control = list(), 
                                method = "BFGS", 
                                outer.iterations = 100, 
                                outer.eps = 1e-08, 
                                arguments = list(), 
                                hessian = FALSE) {
  if (!is.null(control$fnscale) && control$fnscale < 0) {
    mu <- -mu
  }

  control$maxit <- 2000
  
  R <- function(theta, theta.old, arguments) {
    ui.theta <- ui %*% theta
    gi <- ui.theta - ci
    if (any(gi < 0)) {
      return(NaN)
    }
    gi.old <- ui %*% theta.old - ci
    bar <- sum(gi.old * log(gi) - ui.theta)
    if (!is.finite(bar)) {
      bar <- -Inf
    }
    val <- f(theta, arguments = arguments) - mu * bar
    val
  }
  
  dR <- function(theta, theta.old, arguments) {
    ui.theta <- ui %*% theta
    gi <- drop(ui.theta - ci)
    gi.old <- drop(ui %*% theta.old - ci)
    dbar <- colSums(ui * gi.old/gi - ui)
    f_grad(x = theta, arguments = arguments) - mu * dbar
  }
  
  if (any(ui %*% theta - ci <= 0)) {
    stop("Initial value is not in the interior of the feasible region.")
  }
  
  obj <- f(theta, arguments)
  r <- R(theta, theta, arguments)
  
  totCounts <- 0
  s.mu <- sign(mu)
  for (i in seq_len(outer.iterations)) {
    obj.old <- obj
    r.old <- r
    theta.old <- theta
    a <- optim(par = theta.old, 
               fn = R, 
               gr = dR,
               control = control, 
               method = method, 
               hessian = hessian, 
               arguments = arguments,
               theta.old = theta.old)
    r <- a$value
    if (is.finite(r) && is.finite(r.old) && abs(r - r.old) < (0.001 + abs(r)) * outer.eps) {
      break
    }
    theta <- a$par
    totCounts <- totCounts + a$counts
    obj <- f(theta, arguments = arguments)
    if (s.mu * obj > s.mu * obj.old) {
      break
    }
  }
  
  if (i == outer.iterations) {
    a$convergence <- 7
    a$message <- gettext("Barrier algorithm ran out of iterations and did not converge")
  }
  if (mu > 0 && obj > obj.old) {
    a$convergence <- 11
    a$message <- gettextf("Objective function increased at outer iteration %d", i)
  }
  if (mu < 0 && obj < obj.old) {
    a$convergence <- 11
    a$message <- gettextf("Objective function decreased at outer iteration %d", i)
  }
  a$outer.iterations <- i
  a$counts <- totCounts
  a$barrier.value <- a$value
  a$value <- f(a$par, arguments)
  a$barrier.value <- a$barrier.value - a$value
  a
}