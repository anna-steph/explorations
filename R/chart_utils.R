#' Format df
#'
#' Uses libraries: dplyr, tidyr, data.table, stringr
#'
#' @param input_df df
#' @param group string; line_item var
#' @param meta_df df; line item desc lookup
#'
#' @return df, formatted
format_df <- function(input_df, line_item, meta_df) {
  
  set <- input_df %>%
    select(date, contains(line_item)) %>%
    pivot_longer(-date, names_sep = 5, names_to = c("pre", "suff")) %>%
    mutate(
      group = case_when(
        stringr::str_detect(suff, "ncb") == TRUE ~ "All commercial banks",
        stringr::str_detect(suff, "ndm") == TRUE ~ "Domestic banks",
        stringr::str_detect(suff, "nlg") == TRUE ~ "Large banks",
        stringr::str_detect(suff, "nsm") == TRUE ~ "Small banks",
        stringr::str_detect(suff, "nfr") == TRUE ~ "Foreign-related institutions"
      ),
      line_item = as.numeric(line_item),
      series_id = toupper(trimws(paste0(pre, suff)))
    ) %>%
    left_join(meta_df %>% select(series_name, short_desc),
              by = c("series_id" = "series_name")) %>%
    select(line_item, group, desc = short_desc, date, current = value)

  dt <- as.data.table(set)

  lag_vals <- dt[, lag := shift(current, 1, fill = NA, type = "lag"),
                 by = c("desc", "line_item", "group")]

  chg_vals <- lag_vals[, chg := sum(current, -lag, na.rm = FALSE),
                       by = seq_len(nrow(lag_vals))]

  return_set <- chg_vals %>%
    as.data.frame() %>%
    relocate(desc, line_item)

  return(return_set)

}


#' Plot stacked bar
#'
#' Dependencies: dplyr, tidyr, data.table, ggplot2, stringr, scales
#'
#' Plot colors: https://www.r-bloggers.com/2020/03/how-to-standardize-group-colors-in-data-visualizations-in-r/
#'
#' @param df df; formatted desc, line_item, group, date, current, lag, chg
#' @param date_start string; earliest date of chart, formatted "yyyymmdd"
#' @param date_end string; latest date of chart, formatted "yyyymmdd", optional
#' @param level_or_change string; whether to chart balance sheet levels or simple diff
#' @param domestic boolean; true if chart only shows domestic rather than all groups
#' @param weekly boolean; true if df represents weekly rather than monthly data
#' @param labelsize numeric; size of chart labels, set in params
#' @param titlesize numeric; size of chart title, set in params
#'
#' @return stacked bar chart
plot_stacked_bar <- function(df, date_start, date_end,
                             level_or_change = "change",
                             domestic = FALSE,
                             weekly = FALSE,
                             labelsize = params$label_size,
                             titlesize = params$title_size) {

  # balance sheet levels or changes (simple diff)
  var_of_interest <- dplyr::if_else(
    stringr::str_detect(stringr::str_to_lower(level_or_change), "level"),
    "current",
    "chg")

  id_pal <- RColorBrewer::brewer.pal(n = 3, name = "Blues")

  id_colors <- c(
    "Foreign-related institutions" = id_pal[1],
    "Large banks" = id_pal[2],
    "Small banks" = id_pal[3]
  )

  id_labs <- c(
    "Foreign-related institutions" = "Foreign-related institutions",
    "Large banks" = "Large banks",
    "Small banks" = "Small banks"
  )

  if (missing(domestic)) {
    id_include <- "(Foreign|Large|Small)"
    id_colors <- id_colors
    id_labs <- id_labs
  } else if (isTRUE(domestic)) {
    id_include <- "(Large|Small)"
    id_colors <- id_colors[2:3]
    id_labs <- id_labs[2:3]
  } else {
    id_include <- "(Foreign|Large|Small)"
    id_colors <- id_colors
    id_labs <- id_labs
  }

  dt_start <- as.Date(date_start, format = "%Y%m%d")
  dt_end <- if (!missing(date_end)) as.Date(date_end, format = "%Y%m%d") else Sys.Date()

  chart_vars <- c("group", "desc", "date", paste0(var_of_interest))

  if (missing(weekly)) {

    long <- df %>%
      filter(
        str_detect(group, id_include) == TRUE
      ) %>%
      select(all_of(chart_vars)) %>%
      mutate(
        date_chart = as.Date(paste(year(date), month(date), "01", sep = "-"),
                             format = "%Y-%m-%d")
      ) %>%
      filter(date_chart >= dt_start & date_chart <= dt_end) %>%
      select(-date) %>%
      rename(date = date_chart) %>%
      relocate(all_of(chart_vars)) %>%
      rename("chart_val" = var_of_interest)

  } else if (isTRUE(weekly)) {

    long <- df %>%
      filter(
        str_detect(group, id_include) == TRUE
      ) %>%
      select(all_of(chart_vars)) %>%
      filter(date >= dt_start & date <= dt_end) %>%
      relocate(all_of(chart_vars)) %>%
      rename("chart_val" = var_of_interest)

  } else {

    long <- df %>%
      filter(
        str_detect(group, id_include) == TRUE
      ) %>%
      select(all_of(chart_vars)) %>%
      mutate(
        date_chart = as.Date(paste(year(date), month(date), "01", sep = "-"),
                             format = "%Y-%m-%d")
      ) %>%
      filter(date_chart >= dt_start & date_chart <= dt_end) %>%
      select(-date) %>%
      rename(date = date_chart) %>%
      relocate(all_of(chart_vars)) %>%
      rename("chart_val" = var_of_interest)

  }

  plot <- ggplot2::ggplot(long) +
    geom_col(aes(fill = group,
                 x = date,
                 y = chart_val / 1000),
             color = "black", linewidth = 0.15) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    scale_colour_manual(values = "black") +
    scale_fill_manual(name = "Group",
                      labels = id_labs,
                      values = id_colors) +
    theme_bw() +
    labs(x = "",
         y = "Billions",
         fill = "",
         title = paste0(long$desc, ", ",
                        tolower(level_or_change),
                        ", ",
                        format(min(long$date), "%b %Y"),
                        " to ",
                        format(max(long$date), "%b %Y")
         )) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = labelsize)) +
    theme(axis.title.x = element_text(size = labelsize),
          axis.title.y = element_text(size = labelsize),
          axis.text.x = element_text(size = labelsize),
          axis.text.y = element_text(size = labelsize)) +
    theme(plot.title = element_text(size = titlesize))

  plot

}
