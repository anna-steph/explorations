#' Calc change
#'
#' Calculate change in a df relative to a one-period lag
#' Change defined as the simple difference of current period less prior period
#'
#' Must arrange df as
#' a) long: meta_vars, series_id, date, current
#' b) long: series_id, date, current
#' c) wide: meta_vars, date, series1, series2
#' d) wide: date, series1, series2
#'
#' Dependencies: dplyr, tidyr, stringr, data.table
#'
#' Function uses arrange_at(c(stable_vars, "date")) to pass a string of vars in quotes
#' Alternative syntax: arrange(!!! rlang::syms(stable_vars), date)
#'
#' @param df df; df for change calcs: meta_vars, series_id, date, current
#' @param is_long boolean; TRUE if df = c(series_id, date, current), FALSE if df = c(date, series1, series2)
#' @param meta_vars string; list of meta vars to remain unchanged, in quotes
#'
#' @return df, formatted
calc_change <- function(df,
                        is_long = FALSE,
                        meta_vars) {

  if ((is_long == TRUE) & (missing(meta_vars))) {

    stable_vars <- "series_id"

    set <- df %>%
      arrange(series_id, date)

  } else if ((is_long == TRUE) & (!missing(meta_vars))) {

    stable_vars <- c(meta_vars, "series_id")

    set <- df %>%
      arrange_at(c(stable_vars, "date"))

  } else if ((is_long == FALSE) & (missing(meta_vars))) {

    stable_vars <- "series_id"

    set <- df %>%
      pivot_longer(-date, names_to = "series_id", values_to = "current") %>%
      arrange(series_id, date)

  } else if ((is_long == FALSE) & (!missing(meta_vars))) {

    stable_vars <- c(meta_vars, "series_id")

    set <- df %>%
      pivot_longer(-c(date, meta_vars), names_to = "series_id", values_to = "current") %>%
      arrange_at(c(stable_vars, "date"))

  }

  dt <- as.data.table(set)

  lag_vals <- dt[, lag := shift(current, 1, fill = NA, type = "lag"),
                 by = c(stable_vars)]

  chg_vals <- lag_vals[, chg := sum(current, -lag, na.rm = FALSE),
                       by = seq_len(nrow(lag_vals))]

  return_set <- chg_vals %>%
    as.data.frame() %>%
    relocate(any_of(stable_vars))

  return(return_set)

}
