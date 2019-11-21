augbin_dose_ci           <- function(doses, params, baseline_tumour_size,
                                     dichot_thresh, cov) {
  value    <- augbin_dose_prob_success(doses, params, baseline_tumour_size,
                                       dichot_thresh)
  params   <- matrix(params, 13, 13, byrow = T) + diag(1e-5, 13, 13)
  partials <- sapply(1:13,
                     function(i) {
                       1e5*(augbin_dose_prob_success(doses, params[i, ],
                                                     baseline_tumour_size,
                                                     dichot_thresh) - value)
                     })
  stats::plogis(temp + stats::qnorm(0.975)*
                         as.numeric(sqrt(partials%*%cov%*%partials))*(-1:1))
}

augbin_dose_ci           <- Vectorize(augbin_dose_ci, vectorize.args = "doses")

augbin_dose_loglik       <- function(par, log_tumour_ratio, new_lesions,
                                     toxicity, dose, baseline_tumour_size) {
  sigma1            <- sqrt(exp(par[10]))
  rho12             <- transformation(par[11])
  rho13             <- transformation(par[12])
  rho23             <- transformation(par[13])
  mu1               <- par[1] + dose*par[2] + baseline_tumour_size*par[3]
  fact              <- (log_tumour_ratio - mu1)/sigma1
  cond_mean         <-
    cbind(par[4] + dose*par[5] + baseline_tumour_size*par[6] + fact*rho12,
          par[7] + dose*par[8] + baseline_tumour_size*par[9] + fact*rho13)
  cond_cov          <- matrix(c(1 - rho12^2, rho23 - rho12*rho13,
                                rho23 - rho12*rho13, 1 - rho13^2), 2, 2)
  n                 <- length(baseline_tumour_size)
  func_val          <- numeric(n)
  inf_inf           <- c(Inf, Inf)
  inf_zero          <- c(Inf, 0)
  minusinf_minusinf <- c(-Inf, -Inf)
  minusinf_zero     <- c(-Inf, 0)
  zero_inf          <- c(0, Inf)
  zero_minusinf     <- c(0, -Inf)
  zero_zero         <- c(0, 0)
  for (i in 1:n) {
    if (all(new_lesions[i] == 1, toxicity[i] == 1)) {
      func_val[i]   <- mvtnorm::pmvnorm(lower = zero_zero,
                                        upper = inf_inf,
                                        mean  = cond_mean[i, ],
                                        sigma = cond_cov)[1]
    } else if (all(new_lesions[i] == 1, toxicity[i] == 0)) {
      func_val[i]   <- mvtnorm::pmvnorm(lower = zero_minusinf,
                                        upper = inf_zero,
                                        mean  = cond_mean[i, ],
                                        sigma = cond_cov)[1]
    } else if (all(new_lesions[i] == 0, toxicity[i] == 1)) {
      func_val[i]   <- mvtnorm::pmvnorm(lower = minusinf_zero,
                                        upper = zero_inf,
                                        mean  = cond_mean[i, ],
                                        sigma = cond_cov)[1]
    } else if (all(new_lesions[i] == 0, toxicity[i] == 0)) {
      func_val[i]   <- mvtnorm::pmvnorm(lower = minusinf_minus_inf,
                                        upper = zero_zero,
                                        mean  = cond_mean[i, ],
                                        sigma = cond_cov)[1]
    }
  }
  -sum(log(stats::dnorm(log_tumour_ratio, mu1, sigma1))) - sum(log(func_val))
}

augbin_dose_model        <- function(log_tumour_ratio, new_lesions, toxicity,
                                     dose, baseline_tumour_size) {
  model <- stats::optim(par                  = numeric(13),
                        fn                   = augbin_dose_loglik,
                        log_tumour_ratio     = log_tumour_ratio,
                        new_lesions          = new_lesions,
                        toxicity             = toxicity,
                        dose                 = dose,
                        baseline_tumour_size = baseline_tumour_size,
                        method               = "BFGS",
                        hessian              = T)
  list(convergence = model$convergence,
       cov         = MASS::ginv(model$hessian),
       params      = model$par)
}

augbin_dose_prob_success <- function(dose, params, baseline_tumour_size,
                                     dichot_thresh) {
  n      <- length(baseline_tumour_size)
  mean   <- matrix(c(rep(c(1, dose), each = n),
                     baseline_tumour_size), n, 3)%*%matrix(params[1:9], 3, 3)
  sigma1 <- sqrt(exp(parameters[10]))
  rho12  <- transformation(parameters[11])
  rho13  <- transformation(parameters[12])
  rho23  <- transformation(parameters[13])
  lower  <- rep(-Inf, 3)
  upper  <- c(dichot_thresh, 0, 0)
  cov    <- matrix(c(sigma1^2, sigma1*rho12, sigma1*rho13, sigma1*rho12, 1,
                        rho23, sigma1*rho13, rho23, 1), 3, 3)
  temp   <- apply(mean, 1,
                  function(x) {
                    mvtnorm::pmvnorm(lower     = lower,
                                     upper     = upper,
                                     mean      = x,
                                     sigma     = cov,
                                     algorithm =
                                       mvtnorm::TVPACK(abseps = 1e-7))[1] })
  mean(stats::qlogis(temp))
}