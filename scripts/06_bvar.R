#load library ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
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
  hyper = "full",
  mn = BVAR::bv_minnesota(
    lambda = BVAR::bv_lambda(mode = 0.2, min = 0.0001, max = 5),
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

for (p_i in 1:8) {
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
# 1     1                  428.62
# 2     2                  291.42
# 3     3                  369.36
# 4     4                  379.95
# 5     5                  356.64
# 6     6                  487.63
# 7     7                  316.74
# 8     8                  335.79

#prior 2 lambda sensitivity----

lambda_grid <- c(0.1, 0.3, 0.5, 1, 2, 3, 5)
table_lambda_sensitivity <- data.frame()

for (lam in lambda_grid) {
  priors_tmp <- BVAR::bv_priors(
    hyper = "full",
    mn = BVAR::bv_minnesota(
      lambda = BVAR::bv_lambda(mode = lam, min = 0.0001, max = 5),
      alpha = BVAR::bv_alpha(mode = 2, min = 1, max = 3),
      psi = BVAR::bv_psi(
        mode = sqrt(var_base),
        min = sqrt(var_base) / 10000,
        max = sqrt(var_base) * 10000
      )
    )
  )
  
  set.seed(54973997)
  mod_tmp <- BVAR::bvar(
    data = endogenous, 
    lags = 6, 
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

#results
# lambda_mode Log_Marginal_Likelihood
# 1         0.1                  486.96
# 2         0.3                  488.48
# 3         0.5                  489.98
# 4         1.0                  486.69
# 5         2.0                  479.08
# 6         3.0                  482.99
# 7         5.0                  480.86

#prior 3 lag sensitivity ----

priors_spec_2 <- BVAR::bv_priors(
  hyper = "full",
  mn = BVAR::bv_minnesota(
    lambda = BVAR::bv_lambda(mode = 0.5, min = 0.0001, max = 5),
    alpha = BVAR::bv_alpha(mode = 2, min = 1, max = 3),
    psi = BVAR::bv_psi(
      mode = sqrt(var_base),
      min = sqrt(var_base) / 10000,  
      max = sqrt(var_base) * 10000
    )
  )
)

#the lag criterion is based on maximum likelihood
table_lags_bvar_2 <- data.frame()

for (p_i in 1:8) {
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
# 1     1                    429.09
# 2     2                    291.34
# 3     3                    370.55
# 4     4                    381.39
# 5     5                    357.68
# 6     6                    489.98
# 7     7                    317.60
# 8     8                    337.23

#out-of-sample cross validation ----

T_total <- nrow(endogenous)
T_val <- 8 #8 validation quarters (2022-Q1 through 2023-Q4)
T_train <- T_total - T_val #80 quarters of training

train_endo <- matrix(as.numeric(endogenous[1:T_train, ]), ncol = ncol(endogenous))
train_exo <- matrix(as.numeric(exogenous[1:T_train, ]), ncol = ncol(exogenous))
val_endo <- matrix(as.numeric(endogenous[(T_train + 1):T_total, ]), ncol = ncol(endogenous))
val_exo <- matrix(as.numeric(exogenous[(T_train + 1):T_total, ]), ncol = ncol(exogenous))

colnames(train_endo) <- colnames(endogenous)
colnames(train_exo) <- colnames(exogenous)
colnames(val_endo) <- colnames(endogenous)
colnames(val_exo) <- colnames(exogenous)

#prior -

set.seed(54973997)
mod_p6 <- BVAR::bvar(
  data = train_endo,
  lags = 6,
  exogen = train_exo,
  priors = priors_spec_2,
  n_draw = 20000,
  n_burn = 5000,
  thin = 10,
  verbose = FALSE
)

#out-of-sample forecast
pred_p6 <- BVAR:::predict.bvar(mod_p6, horizon = as.integer(T_val))
pred_summary <- summary(pred_p6)

#extract the median (50%)
pred_median <- pred_summary[, , "0.5"]
pred_median_t <- t(pred_median)

rmse_by_var <- sqrt(colMeans((val_endo - pred_median_t)^2, na.rm = TRUE))
rmse_total <- mean(rmse_by_var, na.rm = TRUE)

cat("Total system RMSE:", round(rmse_total, 6), "\n\n")
cat("RMSE by variable:\n")
print(round(rmse_por_var, 6))

# bvar with p=6 ----

timor <- BVAR::bvar(
  data = endogenous,
  lags = 6,
  exogen = exogenous,
  priors = priors_spec,
  n_draw = 300000, #mcmc totals
  n_burn = 50000, #warm-up period (initial discard)
  thin = 50, #keep 1 out of every 50 samples, remove autocorrelation  MCMC
  verbose  = TRUE
)

summary(timor)
