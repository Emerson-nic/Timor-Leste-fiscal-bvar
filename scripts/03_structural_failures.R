#load library ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               strucchange,
               lubridate
)

# import data if it doesn't exist in the environment

if (!exists("quarterly_log") & !exists("quarterly")) {
  if (file.exists("csv/timor_quarterly_log.csv") & file.exists("csv/timor_quarterly_data.csv")) {
    quarterly_log <- readr::read_csv("csv/timor_quarterly_log.csv")
    quarterly <- readr::read_csv("csv/timor_quarterly_data.csv")
  } else {
    source("scripts/02_dessagregation.R")
  }
}

#break ----

vars_to_break <- c(
  "ln_gov_exp", 
  "ln_gdp_non", 
  "ln_imports", 
  "ln_cpi", 
  "ln_credit"
)

for (v in vars_to_break) {
  cat("\nvariable:", v , "\n")
  #flush.console() allows to print whithin the loop
  utils::flush.console()
  y <- stats::ts(quarterly_log[[v]], start = c(2002, 1), frequency = 4)
  
  #h=0.15 is a trimming parameter 
  #minimum allowed length for each time segment between breaks, 
  #according to the methodology of Bai & Perron (1998, 2003)
  bp_model <- strucchange::breakpoints(y ~ stats::time(y), h = 0.15)
  
  print(summary(bp_model))
}

message("criterion bic will be used")

message("ln_gov_exp - m = 4 with bic = -254.3870 - m = 4   2005(4) 2009(2) 2012(4) 2016(3)")

message("ln_gdp_non - m = 4 with bic = -395.69580  - m = 4   2006(1) 2010(2)         2016(3) 2019(4)")

message("ln_imports - m = 4 with bic = -186.2522 - m = 4   2005(4) 2009(1) 2012(3)         2020(3)")

message("ln_cpi - m = 5 with bic = -461.84006 - m = 5   2005(2) 2009(1) 2012(4) 2016(1) 2020(3)")

message("lc_credit - m = 3 with bic = -97.5200 - m = 3   2005(1) 2011(1)                 2020(3)")


dummys <- quarterly_log %>%
  dplyr::mutate(
    # 2006 political crisis  
    shock_2006 = dplyr::if_else(
      date >= as.Date("2005-10-01") & date <= as.Date("2007-09-30"), 1, 0
    ),
    
    #2009 global financial crisis with withdrawals from the Petroleum Fund
    shock_2009 = dplyr::if_else(
      date >= as.Date("2009-01-01") & date <= as.Date("2010-12-31"), 1, 0
    ),
    
    #post-onu
    shock_2012 = dplyr::if_else(
      date >= as.Date("2012-10-01") & date <= as.Date("2014-09-30"), 1, 0
    ),
    
    #political crisis 2016–2018 
    shock_2016 = dplyr::if_else(
      date >= as.Date("2016-01-01") & date <= as.Date("2018-12-31"), 1, 0
    ),
    
    #covid-19
    shock_covid = dplyr::if_else(
      date >= as.Date("2020-01-01") & date <= as.Date("2021-12-31"), 1, 0
    )
  )
  
print(dummys)
print(dummys, n = 78)

readr::write_csv(dummys, "csv/timor_quaiterly_log_and_dummys.csv")

#add dummys to timor_quaiterly
quarterly <- quarterly %>%
  dplyr::left_join(
    dplyr::select(dummys, 
                  date, 
                  shock_2006, 
                  shock_2009, 
                  shock_2012, 
                  shock_2016, 
                  shock_covid),
    by = "date"
  )

readr::write_csv(quarterly, "csv/timor_quaiterly_and_dummys.csv")

if(F){
  "
  
  Break windows correspond to optimal breakpoint clusters identified via 
  Bai & Perron (2003) tests under the BIC criterion across the 5 serie
  
  Defined as closed temporal windows (pulse/regime blocks) rather than 
  cumulative step dummies. This keeps the indicators mutually exclusive,
  preventing severe collinearity in the deterministic matrix
  
  Captures temporary volatility from major historical shocks (dummys)) 
  without inducing artificial deterministic trends into the Minnesota prior
  or distorting the irfs
  
  "
}

rm(list = ls())
