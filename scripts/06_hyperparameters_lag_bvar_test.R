if(F){
  "
  
  the series has few data point (88) and has been interpolated i don't trust
  the result, so be careful with estimates
  
  "
}

#load library ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               BVAR,
               coda #for the Geweke convergence test of mcmc
)

# import data if it doesn't exist in the environment

if (!exists("quarterly_log_dummy")) {
  if (file.exists("csv/timor_quaiterly_log_and_dummys.csv")) {
    quarterly_log_dummy <- readr::read_csv("csv/timor_quaiterly_log_and_dummys.csv")
  } else {
    source("scripts/02_dessagregation.R")
  }
}

#series ----

endogenous <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("ln_")) %>%
  as.matrix()

exogenous <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("shock_")) %>%
  as.matrix()

#prior 1 lag selection and initial hyperparameters ----

#it does 2 diferences, because the series has a unit root so arima can't with na
var_base <- as.numeric(apply(diff(endogenous), 2, var, na.rm = TRUE))

#pmax() force the variance to be less than 1e-5
var_base <- pmax(var_base, 1e-5, na.rm = TRUE)

priors_spec <- BVAR::bv_priors(
  hyper = "auto",
  mn = BVAR::bv_minnesota(
    lambda = BVAR::bv_lambda(mode = 0.2, min = 0.0001, max = 1.5),
    alpha = BVAR::bv_alpha(mode = 2, min = 1, max = 3),
    # psi = BVAR::bv_psi() 
    psi = BVAR::bv_psi(
      mode = sqrt(var_base),
      min = sqrt(var_base) / 100,
      max = sqrt(var_base) * 100
    )
    )
)

#the lag criterion is based on maximum likelihood
table_lags_bvar <- data.frame()

for (p_i in 1:12) {
  set.seed(54973997)
  
  mod_tmp <- BVAR::bvar(
    data = endogenous,
    lags = p_i,
    exogen = exogenous,
    priors = priors_spec,
    n_draw = 10000,
    n_burn = 1000,
    verbose = FALSE
  )
  
  #get the log-marginal likelihood
  lml_val <- logLik(mod_tmp)
  
  table_lags_bvar <- rbind(table_lags_bvar, data.frame(
    lag_p = p_i,
    Log_Marginal_Likelihood = round(as.numeric(lml_val), 2)
  ))
}

print(table_lags_bvar)

#results
# lag_p Log_Marginal_Likelihood
# 1      1                  950.98
# 2      2                 1073.00
# 3      3                 1079.32 winner
# 4      4                 1072.07
# 5      5                 1076.60
# 6      6                 1071.05
# 7      7                 1060.43
# 8      8                 1041.40
# 9      9                 1021.53
# 10    10                  999.05
# 11    11                  989.87
# 12    12                  964.77

#prior 2 lambda sensitivity----

lambda_grid <- c(0.1 ,0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.4)
table_lambda_sensitivity <- data.frame()

for (lam in lambda_grid) {
  priors_tmp <- BVAR::bv_priors(
    hyper = "auto",
    mn = BVAR::bv_minnesota(
      lambda = BVAR::bv_lambda(mode = lam, min = 0.0001, max = 1.5),
      alpha = BVAR::bv_alpha(mode = 2, min = 1, max = 3),
      psi = BVAR::bv_psi(
        mode = sqrt(var_base),
        min = sqrt(var_base) / 100,
        max = sqrt(var_base) * 100      )
    )
  )
  
  set.seed(54973997)
  mod_tmp <- BVAR::bvar(
    data = endogenous, 
    lags = 3, 
    exogen = exogenous,
    priors = priors_tmp,
    n_draw = 10000,
    n_burn = 1000, 
    verbose = FALSE
  )
  
  table_lambda_sensitivity <- rbind(table_lambda_sensitivity, data.frame(
    lambda_mode = lam,
    Log_Marginal_Likelihood = round(as.numeric(logLik(mod_tmp)), 2)
  ))
}

print(table_lambda_sensitivity)

#it doesn't make sence, the winner is 1.4, i don't trust so this is waekness ;(

# lambda_mode Log_Marginal_Likelihood
# 1          0.1                 1073.00
# 2          0.2                 1073.00
# 3          0.3                 1073.00
# 4          0.4                 1073.00
# 5          0.5                 1073.00
# 6          0.6                 1073.00
# 7          0.7                 1073.00
# 8          0.8                 1073.00
# 9          0.9                 1073.00
# 10         1.0                 1073.00
# 11         1.4                 1073.01whyyy????????


#prior 3 lag sensitivity ----

priors_spec_2 <- BVAR::bv_priors(
  hyper = "auto",
  mn = BVAR::bv_minnesota(
    lambda = BVAR::bv_lambda(mode = 1.3, #lambda is the overall tightness
                             #Balance how much freedom the prior gives the 
                             #model to let the data speak for itself against 
                             #how much you force it to rely on the prior
                             min = 0.0001, 
                             max = 3),
    
    alpha = BVAR::bv_alpha(mode = 2, #alpha is the lag decay controls the rate 
                           #at which the coefficients shrink toward zero as 
                           #the lag is reduced
                           min = 1, 
                           max = 3),
    
    psi = BVAR::bv_psi(#the psi evaluate the contraction using the intrinsic 
      #volatility of each variable
      mode = sqrt(var_base),
      min = sqrt(var_base) / 100,  
      max = sqrt(var_base) * 100
    )
  )
)

#the lag criterion is based on maximum likelihood
table_lags_bvar_2 <- data.frame()

for (p_i in 1:12) {
  set.seed(54973997)
  
  mod_tmp_2 <- BVAR::bvar(
    data = endogenous,
    lags = p_i,
    exogen = exogenous,
    priors = priors_spec_2,
    n_draw = 10000,
    n_burn = 1000,
    verbose = FALSE
  )
  
  #get the log-marginal likelihood
  lml_val_2 <- logLik(mod_tmp_2)
  
  table_lags_bvar_2 <- rbind(table_lags_bvar_2, data.frame(
    lag_p = p_i,
    Log_Marginal_Likelihood_2 = round(as.numeric(lml_val_2), 2)
  ))
}

print(table_lags_bvar_2)

#resuls
# lag_p Log_Marginal_Likelihood_2
# 1      1                    959.85
# 2      2                   1117.54 winner
# 3      3                   1097.57
# 4      4                   1073.80
# 5      5                   1073.19
# 6      6                   1073.63
# 7      7                   1069.78
# 8      8                   1057.24
# 9      9                   1041.41
# 10    10                   1021.53
# 11    11                   1008.92
# 12    12                    990.76

#final hiperparameters ----

priors_spec_3 <- BVAR::bv_priors(
  hyper = "lambda",
  mn = BVAR::bv_minnesota(
    # Literature standard for small economies with interpolated quarterly data
    #Giannone, D., Lenza, M., & Primiceri, G. E. (2015), "Prior selection..."
    # Fixing lambda around 0.2 avoids overfitting the smoothness of Denton-Cholette
    lambda = BVAR::bv_lambda(mode = 0.2,
                             min = 0.001, 
                             max = 3),
    
    alpha = BVAR::bv_alpha(mode = 2, 
                           min = 1, 
                           max = 3),
    
    psi = BVAR::bv_psi(
      mode = sqrt(var_base),
      min = sqrt(var_base) / 100,  
      max = sqrt(var_base) * 100
    )
  )
)

set.seed(54973997)
mod_tmp_3 <- BVAR::bvar(
  data = endogenous,
  lags = 2,
  exogen = exogenous,
  priors = priors_spec_3,
  n_draw = 10000,
  n_burn = 1000,
  verbose = TRUE
)

summary(mod_tmp_3)
# if Error en 1:k: argument of length 0 
#just type ctrl + shift + f10 
plot(mod_tmp_3)
print(vcov(mod_tmp_3), digits = 6)

#mcmc convergence test (GEWEKE & ESS) ----

# Assessing convergence across Hyperparameters, Coefficients (beta), and Covariance (sigma)
hypers_mcmc <- coda::as.mcmc(mod_tmp_3$hyper) 
geweke_test <- coda::geweke.diag(hypers_mcmc)
geweke_p_values <- 2 * stats::pnorm(-abs(geweke_test$z))
ess_hypers <- coda::effectiveSize(hypers_mcmc)

geweke_table <- data.frame(
  Hyperparameter = names(geweke_p_values),
  P_Value_Geweke = round(geweke_p_values, 4),
  ESS_Sample_Size = round(ess_hypers, 1),
  Pass_Convergence = geweke_p_values > 0.05
)

print(geweke_table)

#mcmc convergence, all 
beta_mat <- matrix(mod_tmp_3$beta, nrow = dim(mod_tmp_3$beta)[1])
beta_mcmc <- coda::as.mcmc(beta_mat)
ess_beta <- coda::effectiveSize(beta_mcmc)
geweke_beta <- coda::geweke.diag(beta_mcmc)
geweke_p_beta <- 2 * stats::pnorm(-abs(geweke_beta$z))

beta_table <- data.frame(
  Parameter = paste0("beta_", 1:ncol(beta_mat)),
  Geweke_P_Value = round(geweke_p_beta, 4),
  Eff_Sample_Size = round(ess_beta, 1),
  Pass_Convergence = geweke_p_beta > 0.05
)
print(beta_table)

#mcmc convergence variance covariance (sigma)
beta_mat <- matrix(mod_tmp_3$beta, nrow = dim(mod_tmp_3$beta)[1])
beta_mcmc <- coda::as.mcmc(beta_mat)
ess_beta <- coda::effectiveSize(beta_mcmc)
geweke_beta <- coda::geweke.diag(beta_mcmc)
geweke_p_beta <- 2 * stats::pnorm(-abs(geweke_beta$z))

beta_table <- data.frame(
  Parameter = paste0("beta_", 1:ncol(beta_mat)),
  Geweke_P_Value = round(geweke_p_beta, 4),
  Eff_Sample_Size = round(ess_beta, 1),
  Pass_Convergence = geweke_p_beta > 0.05
)
print(beta_table)


#BVAR stability 
comp_mat <- BVAR::companion(mod_tmp_3)
eigen_vals <- eigen(comp_mat)$values
max_modulus <- max(Mod(eigen_vals))

stability_df <- data.frame(
  Max_Eigenvalue_Modulus = round(max_modulus, 4),
  Is_Stable = max_modulus < 1
)
print(stability_df)


rm(list = ls())
