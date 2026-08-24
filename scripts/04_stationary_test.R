#load library ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               tseries,
               urca
)

# import data if it doesn't exist in the environment

if (!exists("quarterly_log_dummy")) {
  if (file.exists("csv/timor_quaiterly_log_and_dummys.csv")) {
    quarterly_log_dummy <- readr::read_csv("csv/timor_quaiterly_log_and_dummys.csv")
  } else {
    source("scripts/02_dessagregation.R")
  }
}

#stationary test ----

#notes: map_dfr, it takes each column of the table and applies a 
#function to it, and stacks the results row by row
#.id create a new column
unit_root_pvalues <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("ln_")) %>%
  purrr::map_dfr(\(x) {
    tibble::tibble(
      p_val_adf = round(tseries::adf.test(x)$p.value, 4),
      p_val_kpss = round(tseries::kpss.test(x)$p.value, 4)
    )
  }, .id = "variables")

message("adf test h0: the series is non-stacionary\nkpss test h0: the series is stacionary")
print(unit_root_pvalues)

#za test ----

za_results <- quarterly_log_dummy %>%
  dplyr::select(dplyr::starts_with("ln_")) %>%
  purrr::map_dfr(\(x) {
    za_test <- urca::ur.za(x, model = "both", lag = 2)
    tibble::tibble(
      t_stat = round(za_test@teststat, 4),
      crit_val_5pct = za_test@cval[2], 
      break_point = za_test@bpoint,
      break_date = as.character(quarterly_log_dummy$date[za_test@bpoint]),
      is_stationary = za_test@teststat < za_test@cval[2]
    )
  }, .id = "variables")

print(za_results)
message("is_stationary meaning that h0 is rejacted, h1 is accepted ")
message("za - h0: the series has unit root")



