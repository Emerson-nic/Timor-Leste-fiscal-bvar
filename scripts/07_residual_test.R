#load library ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               dplyr,
               BVAR,
               coda, #for the Geweke convergence test of MCMC
               FinTS #for the ARCH test of heteroscedasticity
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
  dplyr::select(
    ln_gov_exp, # 1 initial shock
    ln_gdp_non, # 2 real response
    ln_imports, # 3 fiscal leakage
    ln_cpi, # 4 price adjustment
    ln_credit # 5 financial liquidity
  ) %>%
  as.matrix()

exogenous <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("shock_")) %>%
  as.matrix()

#prior lag selection and initial hyperparameters ----

#it does 1 diferences, because the series has a unit root so arima can't with na
var_base <- as.numeric(apply(diff(endogenous), 2, var, na.rm = TRUE))

#pmax() force the variance to be less than 1e-5
var_base <- pmax(var_base, 1e-5, na.rm = TRUE)

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

# bvar with p=2 ----

timor <- BVAR::bvar(
  data = endogenous,
  lags = 2,
  exogen = exogenous,
  priors = priors_spec_2,
  n_draw = 500000, #mcmc totals
  n_burn = 150000, #warm-up period (initial discard)
  thin = 50, #keep 1 out of every 50 samples, remove autocorrelation  MCMC
  verbose = TRUE #the console displays a progress bar 
)

#hiperparameters
timor$hyper

#coefficients
coef(timor, type = "mean")

#extract residuals
res_array <- stats::residuals(timor)
res_median <- residuals(timor, type = "quantile", conf_bands = 0.5)
res_median <- as.matrix(res_median)
colnames(res_median) <- colnames(endogenous)

#ARCH and Ljung-Box test ----
diagnostics_table <- data.frame()

for (i in 1:ncol(res_median)) {
  var_name <- colnames(res_median)[i]
  var_resid <- res_median[, i]
  
  #autocorrelation (Ljung-Box test)
  #H0: there is no autocorrelation (p > 0.05 and true)
  lb_test <- stats::Box.test(var_resid, lag = 12, type = "Ljung-Box")
  p_lb <- lb_test$p.value
  
  #heteroscedasticity (ARCH test)
  #H0: No ARCH effects(p > 0.05 and true)
  arch_test <- FinTS::ArchTest(var_resid, lags = 12)
  p_arch <- arch_test$p.value
  
  diagnostics_table <- rbind(diagnostics_table, data.frame(
    Variable = var_name,
    P_Value_Autocorr = round(p_lb, 4),
    Pass_Autocorr = p_lb > 0.05,
    P_Value_Heterosc = round(p_arch, 4),
    Pass_Heterosc = p_arch > 0.05
  ))
}

print(diagnostics_table)

#mcmc convergence test (GEWEKE test) ----

#H0: the markov chains have converged (p > 0.05 )
#convert hyperparameters trace into an mcmc object
hypers_mcmc <- coda::as.mcmc(timor$hyper) 
geweke_test <- coda::geweke.diag(hypers_mcmc)

#p-values
geweke_p_values <- 2 * stats::pnorm(-abs(geweke_test$z))

geweke_table <- data.frame(
  Hyperparameter = names(geweke_p_values),
  P_Value_Geweke = round(geweke_p_values, 4),
  Pass_Convergence = geweke_p_values > 0.05
)

print(geweke_table)


