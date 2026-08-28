#load library ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(BVAR,
               dplyr,
               coda, #for the Geweke convergence test of mcmc
               FinTS #for the ARCH test of heteroscedasticity
)

# import data if it doesn't exist in the environment

#restartR in case of an error
#.rs.restartR()

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
    ln_imports, # 2 fiscal leaka
    ln_gdp_non, # 3 real response
    ln_cpi, # 4 price adjustment
    ln_credit # 5 financial liquidity
  ) %>%
  stats::na.omit() %>%
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
    # Literature standard for small/emerging economies with interpolated quarterly data:
    # Fixing/tightening lambda around 0.2 avoids overfitting the smoothness of Denton-Cholette
    lambda = BVAR::bv_lambda(mode = 0.2,
                             min = 0.0001, 
                             max = 5),
    
    alpha = BVAR::bv_alpha(mode = 2, 
                           min = 1, 
                           max = 5),
    
    psi = BVAR::bv_psi(
      mode = sqrt(var_base),
      min = sqrt(var_base) / 100,  
      max = sqrt(var_base) * 100
    )
  )
)


# bvar with p=2 ----

set.seed(54973997)
timor <- BVAR::bvar(
  data = endogenous,
  lags = 2,
  exogen = exogenous,
  priors = priors_spec_2,
  n_draw = 50000, #mcmc totals
  n_burn = 1000, #warm-up period (initial discard)
  thin = 5, #keep 1 out of every 5 samples, remove autocorrelation  mcmc
  #thin kills the geweke test ;(
  verbose = TRUE #the console displays a progress bar 
)

summary(timor)
#if Error en 1:k: argument of length 0 
#just type ctrl + shift + f10 
plot(timor)

#hiperparameters
#timor$hyper

#coefficients
#coef(timor, type = "mean")

#extract residuals
# Note on Denton-Cholette disaggregation:
# The quarterly series were interpolated from annual data without guide variables
# This introduces deterministic smoothness and intrinsic serial correlation
# Therefore, Ljung-Box test rejections for autocorrelation should be interpreted
# with caution as a property of the temporal disaggregation method, not solely 
# BVAR misspecification.

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
  #H0: no ARCH effects(p > 0.05 and true)
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

cat("Residual Diagnostics (Ljung-Box & ARCH)\n")
print(diagnostics_table)

#mcmc convergence test (GEWEKE & ESS) ----

# Assessing convergence across Hyperparameters, Coefficients (beta), and Covariance (sigma)
hypers_mcmc <- coda::as.mcmc(timor$hyper) 
geweke_test <- coda::geweke.diag(hypers_mcmc)
geweke_p_values <- 2 * stats::pnorm(-abs(geweke_test$z))
ess_hypers <- coda::effectiveSize(hypers_mcmc)

geweke_table <- data.frame(
  Hyperparameter = names(geweke_p_values),
  P_Value_Geweke = round(geweke_p_values, 4),
  Eff_Sample_Size = round(ess_hypers, 1),
  Pass_Convergence = geweke_p_values > 0.05
)

print(geweke_table)

#mcmc convergence, coefficients Beta, ESS and Geweke 
beta_mat <- matrix(timor$beta, nrow = dim(timor$beta)[1])
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
sigma_mat <- matrix(timor$sigma, nrow = dim(timor$sigma)[1])
sigma_mcmc <- coda::as.mcmc(sigma_mat)
ess_sigma <- coda::effectiveSize(sigma_mcmc)
geweke_sigma <- coda::geweke.diag(sigma_mcmc)
geweke_p_sigma <- 2 * stats::pnorm(-abs(geweke_sigma$z))

sigma_table <- data.frame(
  Parameter = paste0("sigma_", 1:ncol(sigma_mat)),
  Geweke_P_Value = round(geweke_p_sigma, 4),
  Eff_Sample_Size = round(ess_sigma, 1),
  Pass_Convergence = geweke_p_sigma > 0.05
)
print(sigma_table)


#BVAR stability 
comp_mat <- BVAR::companion(timor)
eigen_vals <- eigen(comp_mat)$values
max_modulus <- max(Mod(eigen_vals))

stability_df <- data.frame(
  Max_Eigenvalue_Modulus = round(max_modulus, 4),
  Is_Stable = max_modulus < 1
)
print(stability_df)


