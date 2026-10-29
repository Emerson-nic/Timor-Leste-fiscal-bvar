rm(list = ls())

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-10-19"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               BVAR)

#First-difference BVAR: the same three-variable system as the baseline, but
#estimated in log differences. Cumulative IRFs are rescaled to a 1% increase
#in the level of government expenditure.

data <- readr::read_csv(
  "Data/csv/timor_quarterly_official_log_and_dummys.csv",
  show_col_types = FALSE
)

exo <- data %>%
  dplyr::select(shock_2016, shock_covid) %>%
  as.matrix()

H <- 12
N_DRAW <- 100000
N_BURN <- 20000
N_THIN <- 20

run_first_difference <- function(vars, names, seed, order_label) {
  levels <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    as.matrix()
  colnames(levels) <- names

  Y <- diff(levels)
  exo_diff <- exo[-1, , drop = FALSE]

  var_base <- as.numeric(apply(Y, 2, var, na.rm = TRUE))
  var_base <- pmax(var_base, 1e-5, na.rm = TRUE)

  priors <- BVAR::bv_priors(
    hyper = "lambda",
    mn = BVAR::bv_minnesota(
      lambda = BVAR::bv_lambda(mode = 0.2, min = 0.001, max = 3),
      alpha = BVAR::bv_alpha(mode = 2, min = 1, max = 3),
      psi = BVAR::bv_psi(
        mode = sqrt(var_base),
        min = sqrt(var_base) / 100,
        max = sqrt(var_base) * 100
      )
    )
  )

  set.seed(seed)
  model <- BVAR::bvar(
    data = Y,
    lags = 2,
    exogen = exo_diff,
    priors = priors,
    n_draw = N_DRAW,
    n_burn = N_BURN,
    n_thin = N_THIN,
    verbose = FALSE
  )

  irf_obj <- BVAR::irf(
    model,
    BVAR::bv_irf(horizon = H, identification = TRUE, fevd = FALSE),
    conf_bands = c(0.10, 0.16, 0.84, 0.90)
  )

  q <- irf_obj$quants
  dimnames(q) <- list(
    Quantile = c("10%", "16%", "50%", "84%", "90%"),
    Response = names,
    Horizon = seq_len(H),
    Impulse = names
  )

  irf_df <- as.data.frame(as.table(q)) %>%
    tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
    dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
    dplyr::arrange(Response, Horizon)

  readr::write_csv(
    irf_df,
    paste0(
      "Data/csv/irf_full_order_", order_label,
      "_official_3var_diff.csv"
    )
  )

  draws <- irf_obj$irf
  dimnames(draws) <- list(
    Draw = as.character(seq_len(dim(draws)[1])),
    Response = names,
    Horizon = seq_len(H),
    Impulse = names
  )

  cum_draws <- apply(draws, MARGIN = c(1, 2, 4), FUN = cumsum)
  cum_draws <- aperm(cum_draws, c(2, 3, 1, 4))

  cum_q <- apply(
    cum_draws,
    MARGIN = c(2, 3, 4),
    FUN = quantile,
    probs = c(0.10, 0.16, 0.50, 0.84, 0.90),
    na.rm = TRUE
  )

  dimnames(cum_q) <- list(
    Quantile = c("10%", "16%", "50%", "84%", "90%"),
    Response = names,
    Horizon = seq_len(H),
    Impulse = names
  )

  cum_df <- as.data.frame(as.table(cum_q)) %>%
    tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
    dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
    dplyr::arrange(Response, Horizon)

  readr::write_csv(
    cum_df,
    paste0(
      "Data/csv/irf_cumulative_order_", order_label,
      "_official_3var_diff.csv"
    )
  )

  gov_cum_impact <- stats::median(
    cum_draws[, "Gov Exp", 1, "Gov Exp"],
    na.rm = TRUE
  )
  scale <- 0.01 / gov_cum_impact
  cum_scaled <- cum_draws * scale

  cum_scaled_q <- apply(
    cum_scaled,
    MARGIN = c(2, 3, 4),
    FUN = quantile,
    probs = c(0.10, 0.16, 0.50, 0.84, 0.90),
    na.rm = TRUE
  )

  dimnames(cum_scaled_q) <- list(
    Quantile = c("10%", "16%", "50%", "84%", "90%"),
    Response = names,
    Horizon = seq_len(H),
    Impulse = names
  )

  cum_scaled_df <- as.data.frame(as.table(cum_scaled_q)) %>%
    tidyr::pivot_wider(names_from = Quantile, values_from = Freq) %>%
    dplyr::mutate(Horizon = as.numeric(as.character(Horizon))) %>%
    dplyr::arrange(Response, Horizon)

  readr::write_csv(
    cum_scaled_df,
    paste0(
      "Data/csv/irf_cumulative_scaled_order_", order_label,
      "_official_3var_diff.csv"
    )
  )

  comp <- BVAR::companion(model, type = "mean")
  max_mod <- max(Mod(eigen(comp)$values))

  readr::write_csv(
    tibble::tibble(
      sample = "official_3var_diff",
      order = order_label,
      max_modulus = max_mod,
      n_obs = nrow(Y),
      horizon = H
    ),
    paste0(
      "Data/csv/stability_order_", order_label,
      "_official_3var_diff.csv"
    )
  )

  summary_rows <- cum_scaled_df %>%
    dplyr::filter(
      Impulse == "Gov Exp",
      Response %in% c("Imports", "N.O. GDP"),
      Horizon %in% c(1, 3, 6, 12)
    ) %>%
    dplyr::transmute(
      sample = "official_3var_diff",
      order = order_label,
      response = Response,
      horizon = Horizon,
      median_pct = `50%` * 100,
      lower68_pct = `16%` * 100,
      upper68_pct = `84%` * 100,
      lower90_pct = `10%` * 100,
      upper90_pct = `90%` * 100,
      max_modulus = max_mod,
      n_obs = nrow(Y)
    )

  invisible(summary_rows)
}

order_1 <- run_first_difference(
  vars = c("ln_gov_exp", "ln_imports", "ln_gdp_non"),
  names = c("Gov Exp", "Imports", "N.O. GDP"),
  seed = 54973997,
  order_label = "1"
)

order_2 <- run_first_difference(
  vars = c("ln_gdp_non", "ln_gov_exp", "ln_imports"),
  names = c("N.O. GDP", "Gov Exp", "Imports"),
  seed = 54973998,
  order_label = "2"
)

readr::write_csv(
  dplyr::bind_rows(order_1, order_2),
  "Data/csv/bvar_diff_summary.csv"
)

message("First-difference BVAR finished.")
