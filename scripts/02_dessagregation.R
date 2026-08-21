
#load library ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               tempdisagg,
               zoo,
               timeseries
               )

# import data if it doesn't exist in the environment

if (!exists("annual_data")) {
  if (file.exists("csv/timor_annual_macro.csv")) {
    annual_data <- readr::read_csv("csv/timor_annual_macro.csv")
  } else {
    source("scripts/01_cleaning.R")
  }
}

#time-series ----

start_year <- min(annual_data$year)

# gov_exp <- ts(annual_data$gov_expenditure_real, 
#                  start = start_year, 
#                  frequency = 1)
# 
# gdp_non <- ts(annual_data$gdp_non_oil_real,
#                  start = 2002, 
#                  frequency = 1)
# 
# imports <- ts(annual_data$imports_real,
#                  start = 2002,
#                  frequency = 1)
# 
# cpi <- ts(annual_data$cpi,
#                  start = 2002, 
#                  frequency = 1)
# 
# credit <- ts(annual_data$private_credit,
#                  start = 2002, 
#                  frequency = 1)

var_names <- c(
  "gov_expenditure_real", 
  "gdp_non_oil_real", 
  "imports_real", 
  "cpi", 
  "private_credit"
)

#note: assign creates string
for (v in var_names) {
  assign(v, 
         ts(annual_data[[v]], 
            start = start_year, 
            frequency = 1))
}

#make the time-series in to quarterly ----

#note: ~1 indicates that it has no external quarterly indicators
#method is the type of smoothing algorithm
#conversion = sum fallow this form 
#$$\sum_{q=1}^{4} x_{t,q} = X_t^{anual}$$
#to average
#$$\frac{1}{4} \sum_{q=1}^{4} x_{t,q} = X_t^{anual}$$


q_gov_exp <- predict(td(gov_expenditure_real ~ 1, to = "quarterly", 
                        method = "denton-cholette", 
                        conversion = "sum"))

q_gdp_non <- predict(td(gdp_non_oil_real ~ 1, to = "quarterly", 
                        method = "denton-cholette", 
                        conversion = "sum"))

q_imports <- predict(td(imports_real ~ 1, 
                        to = "quarterly", 
                        method = "denton-cholette", 
                        conversion = "sum"))

q_cpi <- predict(td(cpi ~ 1, to = "quarterly", 
                    method = "denton-cholette", 
                    conversion = "average"))

q_credit <- predict(td(private_credit ~ 1, 
                       to = "quarterly", 
                       method = "denton-cholette", 
                       conversion = "average"))

#quarter dataset ----

quarterly_data <- tibble::tibble(
  date = seq(from = as.Date(paste0(start_year, "-01-01")), 
             by= "quarter", 
             #the length of dataframe
             length.out = length(q_gdp_non)
  ),
  gov_expenditure_real = as.numeric(q_gov_exp),
  gdp_non_oil_real = as.numeric(q_gdp_non),
  imports_real = as.numeric(q_imports),
  cpi = as.numeric(q_cpi),
  private_credit = as.numeric(q_credit)
)

#log dataset

quarterly_log <- quarterly_data %>%
  #also it can use transmute, delate everything less the new variables
  dplyr::mutate(
    ln_gov_exp = log(gov_expenditure_real),
    ln_gdp_non = log(gdp_non_oil_real),
    ln_imports = log(imports_real),
    ln_cpi = log(cpi),
    ln_credit = log(private_credit)
  )

quarterly_log <- quarterly_log %>%
  dplyr::select(
    date, dplyr::starts_with("ln_")
  )

tibble::as_tibble(quarterly_data)
tibble::as_tibble(quarterly_log)


#export to csv
readr::write_csv(quarterly_data, "csv/timor_quarterly_data.csv")
readr::write_csv(quarterly_log,  "csv/timor_quarterly_log.csv")
