#load library ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               dplyr,
               BVAR
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
# 1      1                  949.59
# 2      2                 1122.87 winner
# 3      3                 1097.40
# 4      4                 1067.32
# 5      5                 1064.50
# 6      6                 1065.60
# 7      7                 1062.01
# 8      8                 1048.09
# 9      9                 1025.80
# 10    10                 1005.00
# 11    11                  989.75
# 12    12                  965.02

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
    lags = 2, 
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

#it doesn't make sence, the winner is 5, i don't trust so this is waekness ;(

# lambda_mode Log_Marginal_Likelihood
# 1          0.1                 1123.72
# 2          0.2                 1122.87
# 3          0.3                 1122.02
# 4          0.4                 1121.19
# 5          0.5                 1120.42
# 6          0.6                 1119.71
# 7          0.7                 1119.09
# 8          0.8                 1118.55
# 9          0.9                 1118.14
# 10         1.0                 1117.83
# 11         2.0                 1120.13
# 12         3.0                 1128.08
# 13         4.0                 1136.81
# 14         5.0                 1144.12 whyyy????????


#prior 3 lag sensitivity ----

priors_spec_2 <- BVAR::bv_priors(
  hyper = "auto",
  mn = BVAR::bv_minnesota(
    lambda = BVAR::bv_lambda(mode = 1.3, #lambda is the overall tightness
                             #Balance how much freedom the prior gives the 
                             #model to let the data speak for itself against 
                             #how much you force it to rely on the prior
                             min = 0.0001, 
                             max = 5),
    
    alpha = BVAR::bv_alpha(mode = 2, #alpha is the lag decay controls the rate 
                           #at which the coefficients shrink toward zero as 
                           #the lag is reduced
                           min = 1, 
                           max = 3),
    
    psi = BVAR::bv_psi(#the psi evaluate the contraction using the intrinsic 
      #volatility of each variable
      mode = sqrt(var_base),
      min = sqrt(var_base) / 10000,  
      max = sqrt(var_base) * 10000
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
#lag_p Log_Marginal_Likelihood_2
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


