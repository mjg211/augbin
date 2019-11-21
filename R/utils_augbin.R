augbin_ci           <- function(params, cont_comp, dichot_thresh, cov) {
  log_est  <- augbin_prob_success(params, cont_comp, dichot_thresh)
  params   <- matrix(params, 6, 6, byrow = T) + diag(1e-5, 6, 6)
  partials <- sapply(1:6,
                     function(i) {
                       1e5*(augbin_prob_success(params[i, ], cont_comp,
                                                dichot_thresh) - log_est)
                     })
  exp(log_est +
        stats::qnorm(0.975)*as.numeric(sqrt(partials%*%cov%*%partials))*(-1:1))
}

augbin_loglik       <- function(par, cont_comp, bin_comp, treatment) {
  sigma1                  <- sqrt(exp(par[5]))
  rho12                   <- transformation(par[6])
  mu1                     <- par[1] + treatment*par[2]
  cond_mean               <- par[3] + treatment*par[4] +
                               (cont_comp - mu1)*rho12/sigma1
  func_val                <- stats::pnorm(-cond_mean/sqrt(1 - rho12^2))
  func_val[bin_comp == 1] <- 1 - func_val[bin_comp == 1]
  -sum(log(stats::dnorm(cont_comp, mu1, sigma1))) - sum(log(func_val))
}

augbin_model        <- function(cont_comp, bin_comp, treatment) {
  model <- stats::optim(par       = numeric(6),
                        fn        = augbin_loglik,
                        cont_comp = cont_comp,
                        bin_comp  = bin_comp,
                        treatment = treatment,
                        method    = "BFGS",
                        hessian   = T)
  list(convergence = model$convergence,
       cov         = MASS::ginv(model$hessian),
       params      = model$par)
}

augbin_prob_success <- function(params, cont_comp, dichot_thresh) {
  n           <- length(cont_comp)
  param_mat   <- matrix(params[1:4], 2, 2)
  means_trt   <- matrix(1, n, 2)%*%param_mat
  sigma1_sq   <- exp(params[5])
  fact        <- transformation(params[6])*sqrt(sigma1_sq)
  means_untrt <- matrix(1:0, n, 2, T)%*%param_mat
  lower       <- c(-Inf, -Inf)
  upper       <- c(dichot_thresh, 0)
  cov         <- matrix(c(sigma1_sq, fact, fact, 1), 2, 2)
  value_trt   <- apply(means_trt, 1,
                       function(x) {
                         mvtnorm::pmvnorm(lower     = lower,
                                          upper     = upper,
                                          mean      = x,
                                          sigma     = cov,
                                          algorithm =
                                            mvtnorm::TVPACK(abseps = 1e-7))[1]
                       })
  value_untrt <- apply(means_untrt, 1,
                       function(x) {
                         mvtnorm::pmvnorm(lower     = lower,
                                          upper     = upper,
                                          mean      = x,
                                          sigma     = cov,
                                          algorithm =
                                            mvtnorm::TVPACK(abseps = 1e-7))[1]
                       })
  mean(stats::qlogis(value_trt)) - mean(stats::qlogis(value_untrt))
}