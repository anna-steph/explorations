#' Format sa comps
#' 
#' Format df for comparisons between nsa and sa level values
#'
#' Dependencies: dplyr, tidyr, stringr, data.table, R/calc_change.R
#'
#' @param df1 df; first df
#' @param df2 df; second df
#' @param bg string; bank group, one of c("lg", "sm", "fr", "dm", "cb")
#' @param line_item string; line_item var
#' @param meta df; line item desc lookup
#'
#' @return df, formatted
format_sa_comps <- function(df1 = nsa_data,
                            df2 = sa_data,
                            bg = "cb",
                            line_item,
                            meta = h8_meta) {

  series_mnem <- paste0(line_item, "n", bg)

  set <- df1 %>%
    select(date, contains(series_mnem)) %>%
    full_join(df2 %>% select(date, contains(series_mnem)),
              by = "date") %>%
    pivot_longer(-date, names_to = "series_id", values_to = "current") %>%
    mutate(
      line_item = as.numeric(stringr::str_extract(series_id, "\\d{4}"))
    ) %>%
    left_join(meta %>% select(series_name, desc = short_desc),
              by = c("series_id" = "series_name")) %>%
    relocate(desc, line_item, series_id, date) %>%
    arrange(series_id, date)

  change <- calc_change(df = set,
                        is_long = TRUE,
                        meta_vars = c("desc", "line_item")) %>%
    mutate(
      sa = dplyr::if_else(stringr::str_detect(series_id, "am$"), "sa", "nsa")
    )

  return(change)

}



#' Plot sa line
#'
#' Chart comparisons between nsa and sa level values 
#'
#' Dependencies: dplyr, tidyr, ggplot2, stringr, scales, RColorBrewer
#'
#' Plot colors: https://www.r-bloggers.com/2020/03/how-to-standardize-group-colors-in-data-visualizations-in-r/
#'
#' @param df df; formatted desc, line_item, group, date, current, lag, chg
#' @param date_start string; earliest date of chart, formatted "yyyymmdd"
#' @param date_end string; latest date of chart, formatted "yyyymmdd", optional
#' @param level_or_change string; whether to chart balance sheet levels or simple diff
#' @param weekly boolean; true if df represents weekly rather than monthly data
#' @param labelsize numeric; size of chart labels, set in params
#' @param titlesize numeric; size of chart title, set in params
#'
#' @return line chart
plot_sa_line <- function(df, date_start, date_end,
                        level_or_change = "change",
                        weekly = FALSE,
                        labelsize = params$label_size,
                        titlesize = params$title_size) {
  
  # balance sheet levels or changes (simple diff)
  var_of_interest <- dplyr::if_else(
    stringr::str_detect(tolower(level_or_change), "level"),
    "current",
    "chg")
  
  id_pal <- RColorBrewer::brewer.pal(n = 3, name = "Blues")
  
  id_colors <- c(
    "nsa" = id_pal[2],
    "sa" = id_pal[3]
  )
  
  id_labs <- c(
    "nsa" = "not seasonally adjusted",
    "sa" = "seasonally adjusted"
  )

  dt_start <- as.Date(date_start, format = "%Y%m%d")
  dt_end <- if (!missing(date_end)) as.Date(date_end, format = "%Y%m%d") else Sys.Date()

  chart_vars <- c("sa", "desc", "date", paste0(var_of_interest))

  if (missing(weekly)) {
    
    long <- df %>%
      select(all_of(chart_vars)) %>%
      mutate(
        date_chart = as.Date(paste(year(date), month(date), "01", sep = "-"),
                             format = "%Y-%m-%d")
      ) %>%
      select(-date) %>%
      rename(date = date_chart) %>%
      filter(date >= dt_start & date <= dt_end) %>%
      relocate(all_of(chart_vars)) %>%
      rename("chart_val" = var_of_interest)
    
  } else if (isTRUE(weekly)) {
    
    long <- df %>%
      select(all_of(chart_vars)) %>%
      filter(date >= dt_start & date <= dt_end) %>%
      relocate(all_of(chart_vars)) %>%
      rename("chart_val" = var_of_interest)
    
  } else {

    long <- df %>%
      select(all_of(chart_vars)) %>%
      mutate(
        date_chart = as.Date(paste(year(date), month(date), "01", sep = "-"),
                             format = "%Y-%m-%d")
      ) %>%
      select(-date) %>%
      rename(date = date_chart) %>%
      filter(date >= dt_start & date <= dt_end) %>%
      relocate(all_of(chart_vars)) %>%
      rename("chart_val" = var_of_interest)

  }

  plot <- ggplot2::ggplot(long) +
    geom_line(aes(color = sa,
                  x = date,
                  y = chart_val / 1000),
              lwd = 0.5) +
    scale_color_manual(name = "Group",
                       labels = id_labs,
                       values = id_colors) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
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
    scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
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
