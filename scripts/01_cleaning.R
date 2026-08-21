#question to answer

if(F){
  "
  
  How much does each dollar spent by the government actually contribute to 
  non-oil GDP, and how many dollars go directly toward imports 
  (fiscal leakage)?
  
  
  "
}

#load library ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(readxl,
               tidyverse,
               janitor,
               dplyr,
)

data_timor <- readxl::read_excel("dataset/tim-key-indicators-2025.xlsx", 
                         sheet = "TIM", 
                         range = "B7:AA424", 
                         col_names = FALSE)



print(data_timor)

#cleaning the dataframe ----

#transpose
data_timor <- t(data_timor)

#the t() funcion converts it to a matrix, convert it to a dataframe
data_timor <- as.data.frame(data_timor)

#header
data_timor <- data_timor %>%
  janitor::row_to_names(row_number = 1)

#rename the first row year
colnames(data_timor)[1] <- "year"

#cleanig the names 
colnames(data_timor) <- janitor::make_clean_names(colnames(data_timor))

#dalate na columns
#note: to use matches, it need to be a dataframe
data_timor <- data_timor %>%
  dplyr::select(!dplyr::matches("^na(_\\d+)?$")) 

#convert '…' and '-' to NA 
#note: across allows apply a funcion to each column,
#everything is a filter without exception,
#\(columns) is the same that funcion(columns) 
data_timor <- data_timor %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), \(columns) dplyr::na_if(columns, "…"))) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), \(columns) dplyr::na_if(columns, "–")))

#delate the columns that all there obs are na
data_timor <- data_timor %>%
  dplyr::select(dplyr::where(\(columns) !all(is.na(columns))))

#fix row header
rownames(data_timor) <- NULL

#save timor-leste's dataset
#note: it's not completely cleaned, but it's in dataframe format

readr::write_csv(data_timor, "csv/timor_leste_uncleaned.csv")

#select the variables ----

names(data_timor)

annual_data <- data_timor %>%
  dplyr::select(
  year,
  gov_expenditure_real = government_final_consumptionh_2,
  gdp_non_oil_real = gdp_by_industrial_origin_at_constant_2015_market_prices,
  imports_real = imports_of_goods_and_services_2,
  cpi  = consumer_dili_december_2001_december_2012_august_2018_100,
  private_credit = claims_on_private_sector
) %>%
  dplyr::arrange(year)

annual_data <- annual_data %>%
  tidyr::drop_na()

print(annual_data)

tibble::as_tibble(annual_data)

readr::write_csv(annual_data, "csv/timor_annual_macro.csv")

