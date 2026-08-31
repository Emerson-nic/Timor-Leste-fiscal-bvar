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
  dplyr::select(
    ln_gov_exp, # 1 initial shock
    ln_imports, # 2 fiscal leaka
    ln_gdp_non, # 3 real response
    ln_cpi, # 4 price adjustment
    ln_credit # 5 financial liquidity
  ) %>%
  as.matrix()
#note: the cpi is expressed as an index
#the other variables are in millions of dollars. 

exogenous <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("shock_")) %>%
  as.matrix()

#prior lag selection and initial hyperparameters ----

#it does 1 diferences, because the series has a unit root so arima can't with na
var_base <- as.numeric(apply(diff(endogenous), 2, var, na.rm = TRUE))

#pmax() force the variance to be less than 1e-5
var_base <- pmax(var_base, 1e-5, na.rm = TRUE)

priors_spec_2 <- BVAR::bv_priors(
  hyper = "lambda",
  mn = BVAR::bv_minnesota(
    # Literature standard for small/emerging economies with interpolated quarterly data:
    # Fixing/tightening lambda around 0.2 avoids overfitting the smoothness of Denton-Cholette
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

# order cholesky 1 ----

#order 1
endogenous_order_1 <- quarterly_log_dummy %>%
  dplyr::select(
    ln_gov_exp, # 1 initial shock
    ln_imports, # 2 fiscal leaka
    ln_gdp_non, # 3 real response
    ln_cpi, # 4 price adjustment
    ln_credit # 5 financial liquidity
  ) %>%
  as.matrix()

# professional names 
prof_names_order_1 <- c("Gov Exp", 
                        "Imports", 
                        "N.O. GDP", 
                        "CPI", 
                        "Credit")

colnames(endogenous_order_1) <- prof_names_order_1

timor_order_1 <- BVAR::bvar(
  data = endogenous_order_1,
  lags = 2,
  exogen = exogenous,
  priors = priors_spec_2,
  n_draw = 300000, 
  n_burn = 100000,
  n_thin = 50,
  verbose = TRUE 
)

#irf
opt_irf_order_1 <- BVAR::bv_irf(
  horizon = 20,
  identification = TRUE, # order cholesky
  fevd = TRUE # calculated fevd 
)

irf_timor_order_1 <- BVAR::irf(timor_order_1,
                               opt_irf_order_1, 
                               conf_bands = c(0.10, 0.16, 0.84, 0.90))

pdf("graphics/irf_full_matrix_order_1.pdf", width = 12, height = 10)
plot(irf_timor_order_1)
dev.off()

pdf("graphics/irf_gov_shock_order_1.pdf", width = 10, height = 8)
plot(irf_timor_order_1, 
     vars_impulse = "Gov Exp", 
     vars_response = c("Imports", "N.O. GDP", "CPI", "Credit"))
dev.off()

#irf table ----

names(irf_timor_order_1)

irf_raw_order_1 <- irf_timor_order_1$quants
irf_df_order_1 <- as.data.frame(as.table(irf_raw_order_1))

colnames(irf_df_order_1) <- c("Quantile", "Response", "Horizon", "Impulse", "Value")
print(irf_df_order_1)

dimnames(irf_raw_order_1) <- list(
  Quantile = c("10%", "16%", "50%", "84%", "90%"), # Computes 68% (0.16-0.84) 
  #and 80% (0.10-0.90) intervals
  Response = prof_names_order_1,
  Horizon = 1:20,
  Impulse = prof_names_order_1
)

irf_df_order_1 <- as.data.frame(as.table(irf_raw_order_1))
print(irf_df_order_1)

irf_df_order_1 <- irf_df_order_1 %>%
  tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
  dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
  dplyr::arrange(Response, Horizon)

readr::write_csv(irf_df_order_1, "csv/irf_full_order_1.csv")

#gov shock table
irf_gov_shock_df_order_1 <- irf_df_order_1 %>%
  dplyr::filter(Impulse == "Gov Exp") %>%
  dplyr::arrange(Response, Horizon)

readr::write_csv(irf_gov_shock_df_order_1, "csv/irf_gov_shock_full_order_1.csv")
