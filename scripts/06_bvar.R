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

#lag selection ----

endogenous <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("ln_")) %>%
  as.matrix()

exogenous <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("shock_")) %>%
  as.matrix()

#prior

#it does 2 diferences, because the series has a unit root so arima can't with na
var_base <- as.numeric(apply(diff(endogenous), 2, var, na.rm = TRUE))

#pmax() force the variance to be less than 1e-5
var_base <- pmax(var_base, 1e-5, na.rm = TRUE)

priors_spec <- BVAR::bv_priors(
  mn = BVAR::bv_minnesota(
    lambda = BVAR::bv_lambda(mode = 0.2, min = 0.0001, max = 5),
    alpha = BVAR::bv_alpha(mode = 2, min = 1, max = 3),
    psi = BVAR::bv_psi(mode = sqrt(var_base))
  )
)

#the lag criterion is based on maximum likelihood
table_lags_bvar <- data.frame()

for (p_i in 1:6) {
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


# bvar with p=2 ----

timor <- BVAR::bvar(
  data = endogenous,
  lags = 2,
  exogen = exogenous,
  priors = priors_spec,
  n_draw = 300000, #mcmc totals
  n_burn = 50000, #warm-up period (initial discard)
  thin = 50, #keep 1 out of every 50 samples, remove autocorrelation  MCMC
  verbose  = TRUE
)

summary(timor)
