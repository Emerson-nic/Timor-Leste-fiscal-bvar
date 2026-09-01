#load library ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               formattable,
               DT
               
)

#load csv ----
irf_order_1_view <- readr::read_csv("csv/irf_full_order_1.csv")
irf_order_2_view <- readr::read_csv("csv/irf_full_order_2.csv")

irf_order_1_view <- irf_order_1_view %>%
  dplyr::select(Impulse, Response, Horizon, dplyr::everything()) %>%
  dplyr::arrange(Impulse, Response, Horizon)

irf_order_2_view <- irf_order_2_view %>%
  dplyr::select(Impulse, Response, Horizon, dplyr::everything()) %>%
  #everything() select all the other columns
  dplyr::arrange(Impulse, Response, Horizon)

#csv rule ---
negative_color <- formattable::formatter("span", 
                                         style = x ~ formattable::style(color = ifelse(x < 0, "red", "black")))

numeric_cols_1 <- names(irf_order_1_view)[sapply(irf_order_1_view, is.numeric)]

format_list_1 <- lapply(stats::setNames(numeric_cols_1, numeric_cols_1), function(x) negative_color)

numeric_cols_2 <- names(irf_order_2_view)[sapply(irf_order_2_view, is.numeric)]

format_list_2 <- lapply(stats::setNames(numeric_cols_2, numeric_cols_2), function(x) negative_color)



## aplicated rule ----

formattable::formattable(irf_order_1_view, format_list_1)

formattable::formattable(irf_order_2_view, format_list_2)

#html ----
DT::datatable(irf_order_1_view, 
              options = list(pageLength = 20),
              filter = 'top', 
              rownames = FALSE) %>%
  DT::formatStyle(
    columns = numeric_cols_1, 
    color = DT::styleInterval(0, c('red', 'black')) 
  )

DT::datatable(irf_order_2_view, 
              options = list(pageLength = 20),
              filter = 'top', 
              rownames = FALSE) %>%
  DT::formatStyle(
    columns = numeric_cols_2, 
    color = DT::styleInterval(0, c('red', 'black')) 
  )

#automation ----

print(irf_order_1_view)
# A tibble: 500 × 8
# Impulse Response Horizon  `10%`  `16%`  `50%`  `84%`  `90%`
# <chr>   <chr>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#   1 CPI     CPI            1 0.0114 0.0116 0.0125 0.0134 0.0138
# 2 CPI     CPI            2 0.0159 0.0164 0.0182 0.0203 0.0210
# 3 CPI     CPI            3 0.0173 0.0180 0.0210 0.0245 0.0256
# 4 CPI     CPI            4 0.0177 0.0186 0.0223 0.0271 0.0287
# 5 CPI     CPI            5 0.0176 0.0187 0.0230 0.0292 0.0313
# 6 CPI     CPI            6 0.0172 0.0184 0.0235 0.0308 0.0336
# 7 CPI     CPI            7 0.0165 0.0179 0.0237 0.0323 0.0354
# 8 CPI     CPI            8 0.0156 0.0172 0.0238 0.0338 0.0373
# 9 CPI     CPI            9 0.0146 0.0164 0.0236 0.0350 0.0393
# 10 CPI     CPI           10 0.0135 0.0154 0.0234 0.0362 0.0411
# ℹ 490 more rows


# vars_names <- unique(irf_order_1_view$Impulse)
# sink("significant_irf_automatic.md")
# 
# #loop
# for (imp in vars_names) {
#   base::cat(base::paste0("# Impulse ", imp, "\n\n"))
#   
#   for (resp in vars_names) {
#     base::cat(base::paste0("## Response ", resp, "\n"))
#     
#     #filter the data in order 1
#     df_sub_1 <- irf_order_1_view %>%
#       dplyr::filter(Impulse == imp, Response == resp) %>%
#       dplyr::arrange(Horizon)
#     
#     #filter the data in order 2
#     df_sub_2 <- irf_order_2_view %>%
#       dplyr::filter(Impulse == imp, Response == resp) %>%
#       dplyr::arrange(Horizon)
#     
#     #significant order 1 (Excluyendo los ceros estructurales con != 0)
#     #68% CI
#     sig_68_1 <- df_sub_1$Horizon[base::sign(df_sub_1$`16%`) == base::sign(df_sub_1$`84%`) & df_sub_1$`16%` != 0]
#     #90% CI 
#     sig_90_1 <- df_sub_1$Horizon[base::sign(df_sub_1$`10%`) == base::sign(df_sub_1$`90%`) & df_sub_1$`10%` != 0]
#     
#     #significant order 2 
#     #68% CI
#     sig_68_2 <- df_sub_2$Horizon[base::sign(df_sub_2$`16%`) == base::sign(df_sub_2$`84%`) & df_sub_2$`16%` != 0]
#     #90% CI 
#     sig_90_2 <- df_sub_2$Horizon[base::sign(df_sub_2$`10%`) == base::sign(df_sub_2$`90%`) & df_sub_2$`10%` != 0]
#     
#     #irf 1 logic
#     if (base::length(sig_68_1) == 0) {
#       base::cat("IRF 1: No horizons are statistically significant - 0\n")
#     } else if (base::length(sig_68_1) == base::nrow(df_sub_1) && base::length(sig_90_1) == base::nrow(df_sub_1)) {
#       base::cat(base::paste0("IRF 1: All horizons are statistically significant - 1:", base::max(df_sub_1$Horizon), " Both credible intervals\n"))
#     } else {
#       base::cat("IRF 1:\n")
#       if (base::length(sig_68_1) > 0) {
#         base::cat(base::paste0("From horizon ", base::min(sig_68_1), " to ", base::max(sig_68_1), 
#                                " are significant 68% credible intervals - ", base::min(sig_68_1), ":", base::max(sig_68_1), "\n"))
#       }
#       if (base::length(sig_90_1) > 0) {
#         base::cat(base::paste0("From horizon ", base::min(sig_90_1), " to ", base::max(sig_90_1), 
#                                " are significant 90% credible intervals - ", base::min(sig_90_1), ":", base::max(sig_90_1), "\n"))
#       }
#     }
#     
#     #irf 2 logic
#     if (base::length(sig_68_2) == 0) {
#       base::cat("IRF 2: No horizons are statistically significant - 0\n\n")
#     } else if (base::length(sig_68_2) == base::nrow(df_sub_2) && base::length(sig_90_2) == base::nrow(df_sub_2)) {
#       base::cat(base::paste0("IRF 2: All horizons are statistically significant - 1:", base::max(df_sub_2$Horizon), " Both credible intervals\n\n"))
#     } else {
#       base::cat("IRF 2:\n")
#       if (base::length(sig_68_2) > 0) {
#         base::cat(base::paste0("From horizon ", base::min(sig_68_2), " to ", base::max(sig_68_2), 
#                                " are significant 68% credible intervals - ", base::min(sig_68_2), ":", base::max(sig_68_2), "\n"))
#       }
#       if (base::length(sig_90_2) > 0) {
#         base::cat(base::paste0("From horizon ", base::min(sig_90_2), " to ", base::max(sig_90_2), 
#                                " are significant 90% credible intervals - ", base::min(sig_90_2), ":", base::max(sig_90_2), "\n"))
#       }
#       base::cat("\n") 
#     }
#   }
# }
# 
# #close file
# sink()
