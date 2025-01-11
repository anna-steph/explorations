#' Pull pub meta
#'
#' @param pub_file string; pub filepath
#'
#' dependencies: dplyr, tidyr, xml2, stringr
#'
#' @returns df; pub metadata
#'
#' @export
pull_pub_meta <- function(pub_file) {
  
  pub <- xml2::read_xml(pub_file)
  series_nodes <- xml2::xml_find_all(pub, "//kf:Series")
  
  # Get series metadata
  attr_names <- c("BG", "CATEGORY", "CURRENCY", "FREQ", "H8_UNITS", "ITEM",
                  "SA", "SERIES_NAME", "UNIT", "UNIT_MULT")

  grab_attr <- function(attr, xml_file) {
    as.vector(xml2::xml_find_all(xml_file, "//kf:Series") %>% xml2::xml_attr(attr))
  }

  attrs_list <- list()

  for (n in seq_along(attr_names)) {
    attrs_list[[n]] <- grab_attr(attr = attr_names[n], xml_file = pub)
  }

  names(attrs_list) <- tolower(attr_names)

  pub_meta <- bind_rows(attrs_list) %>%
    mutate(order = seq_along(series_name))

  return(pub_meta)
}


#' Pull single pub series
#'
#' @param series_id string; id
#' @param pub_file string; the pub filepath
#'
#' dependencies: dplyr, tidyr, xml2, stringr
#'
#' @returns a dataset containing the given pub series
pull_single_pub_series <- function(series_id, pub_file) {
  
  pub <- xml2::read_xml(pub_file)
  series_nodes <- xml2::xml_find_all(pub, "//kf:Series")
  
  # Get series metadata
  attr_names <- c("BG", "CATEGORY", "CURRENCY", "FREQ", "H8_UNITS", "ITEM",
                  "SA", "SERIES_NAME", "UNIT", "UNIT_MULT")
  
  grab_attr <- function(attr, xml_file) {
    as.vector(xml2::xml_find_all(xml_file, "//kf:Series") %>% xml2::xml_attr(attr))
  }
  
  attrs_list <- list()

  for (n in seq_along(attr_names)) {
    attrs_list[[n]] <- grab_attr(attr = attr_names[n], xml_file = pub)
  }

  names(attrs_list) <- tolower(attr_names)

  # full pub metadata
  order_in_pub <- bind_rows(attrs_list) %>%
    mutate(order = seq_along(series_name)) %>%
    filter(series_name == series_id) %>%
    pull(order)

  # get values
  search_node <- series_nodes[order_in_pub]
  obs <- xml2::xml_find_all(search_node, "frb:Obs")

  prod_date <- file.mtime(pub_file)

  df <- data.frame(
    series_name = series_id,
    pub = pub_file,
    status = as.vector(xml2::xml_attr(obs, "OBS_STATUS")),
    value = as.numeric(stringr::str_squish(as.vector(xml2::xml_attr(obs, "OBS_VALUE")))),
    time_period = as.Date(as.vector(xml2::xml_attr(obs, "TIME_PERIOD")),
                          "%Y-%m-%d"),
    output_date = prod_date
  ) %>%
    relocate(series_name, time_period, value, status, output_date, pub)

  return(df)
}



#' Pull all pub vals
#'
#' @param pub_file string; the pub filepath
#'
#' Dependencies: dplyr, tidyr, xml2, stringr, purrr
#'
#' @returns a list of dfs
pull_pub_vals <- function(pub_file) {
  
  t <- Sys.time()
  
  pub <- xml2::read_xml(pub_file)
  
  # Get series metadata
  grab_attr <- function(attr, xml_file) {
    as.vector(xml2::xml_find_all(xml_file, "//kf:Series") %>% xml2::xml_attr(attr))
  }
  
  series_ids <- grab_attr(attr = "SERIES_NAME", xml_file = pub)
  
  # Split by frequency for correct dates
  
  order_freq <- data.frame(
    series_id = series_ids
  ) %>%
    mutate(
      freq = case_when(
        stringr::str_detect(series_id, "(B$|D$)") == TRUE ~ "weekly_nsa",
        stringr::str_detect(series_id, "(A$|C$)") == TRUE ~ "weekly_sa",
        stringr::str_detect(series_id, "(BM$|DM$)") == TRUE ~ "monthly_nsa",
        stringr::str_detect(series_id, "(AM$|CM$)") == TRUE ~ "monthly_sa",
        stringr::str_detect(series_id, "(CMG$)") == TRUE ~ "monthly_g",
        stringr::str_detect(series_id, "(CQG$)") == TRUE ~ "quarterly_g",
        stringr::str_detect(series_id, "(CAG$)") == TRUE ~ "annual_g"
      ),
      order = seq_along(series_id)
    )

  series_nodes <- xml2::xml_find_all(pub, "//kf:Series")
  
  # Pull pub series obs
  get_obs <- function(order, series_name) {
    
    search_node <- series_nodes[order]
    
    obs <- xml2::xml_find_all(search_node, "frb:Obs")
    
    df <- data.frame(
      date = as.Date(as.vector(xml2::xml_attr(obs, "TIME_PERIOD")),
                     "%Y-%m-%d"),
      val = as.numeric(stringr::str_squish(as.vector(xml2::xml_attr(obs, "OBS_VALUE"))))
    )
    
    names(df) <- c("date", tolower(series_name))
    
    long <- df %>%
      pivot_longer(-date, names_to = "series_id", values_to = "value")
    
    return(long)
  }
  
  message("Pull series vals")

  t1 <- Sys.time()

  series_list <- list()

  for (i in seq_along(series_ids)) {
    series_list[[i]] <- get_obs(order = i, series_name = series_ids[i])
  }

  message(paste0("Series loop time elapsed: ", Sys.time() - t1))

  # Group list of pub dfs by freq
  subset_freq <- function(freq_name) {

    order <- order_freq %>%
      filter(freq == freq_name) %>%
      pull(order)

    sub <- series_list[c(order)] %>%
      bind_rows()

    return(sub)
  }

  freq_range <- unique(order_freq$freq)
  pub_dfs <- list()

  for (f in seq_along(freq_range)) {
    pub_dfs[[f]] <- subset_freq(freq_range[f])
  }

  names(pub_dfs) <- freq_range

  message(paste0("Time elapsed: ", Sys.time() - t))

  return(pub_dfs)
}



#' Pull series names
#'
#' @param pub_file string; the pub filepath
#'
#' Dependencies: dplyr, tidyr, xml2, stringr, purrr
#'
#' @returns a list of series names dfs
pull_series_names <- function(pub_file) {
  
  t <- Sys.time()
  
  pub <- xml2::read_xml(pub_file)
  
  # Get series metadata
  grab_attr <- function(attr, xml_file) {
    as.vector(xml2::xml_find_all(xml_file, "//kf:Series") %>% xml2::xml_attr(attr))
  }
  
  series_ids <- grab_attr(attr = "SERIES_NAME", xml_file = pub)
  
  # Split by frequency for correct dates
  
  order_freq <- data.frame(
    series_id = series_ids
  ) %>%
    mutate(
      freq = case_when(
        stringr::str_detect(series_id, "(B$|D$)") == TRUE ~ "weekly_nsa",
        stringr::str_detect(series_id, "(A$|C$)") == TRUE ~ "weekly_sa",
        stringr::str_detect(series_id, "(BM$|DM$)") == TRUE ~ "monthly_nsa",
        stringr::str_detect(series_id, "(AM$|CM$)") == TRUE ~ "monthly_sa",
        stringr::str_detect(series_id, "(CMG$)") == TRUE ~ "monthly_g",
        stringr::str_detect(series_id, "(CQG$)") == TRUE ~ "quarterly_g",
        stringr::str_detect(series_id, "(CAG$)") == TRUE ~ "annual_g"
      ),
      order = seq_along(series_id)
    )
  
  series_nodes <- xml2::xml_find_all(pub, "//kf:Series")
  
  # Pull pub series names
  get_names <- function(order, series_name) {
    
    search_node <- series_nodes[order]
    
    obs <- xml2::xml_find_all(search_node, "frb:Annotations")
    
    df <- data.frame(
      order = order,
      series_name = series_name,
      series_desc = stringr::str_extract(xml2::xml_text(obs), "(?<=Short Description).*?(?=Long Description)")
    )

    return(df)
  }

  message("Pull series names")
  
  t1 <- Sys.time()
  
  series_list <- list()
  
  for (i in seq_along(series_ids)) {
    series_list[[i]] <- get_names(order = i, series_name = series_ids[i])
  }
  
  message(paste0("Series loop time elapsed: ", Sys.time() - t1))
  
  # Group list of pub dfs by freq
  subset_freq <- function(freq_name) {
    
    order <- order_freq %>%
      filter(freq == freq_name) %>%
      pull(order)
    
    sub <- series_list[c(order)] %>%
      bind_rows()
    
    return(sub)
  }
  
  freq_range <- unique(order_freq$freq)
  pub_dfs <- list()
  
  for (f in seq_along(freq_range)) {
    pub_dfs[[f]] <- subset_freq(freq_range[f])
  }
  
  names(pub_dfs) <- freq_range
  
  message(paste0("Time elapsed: ", Sys.time() - t))
  
  return(pub_dfs)
}

