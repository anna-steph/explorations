# Format and save data
# Data saved in intermediate feather files for ease of use

# Set paths -----------------------------------------------------------------

library(rmarkdown)
library(here)
proj_path <- here::here()
download_path <- here("data")

# Setup -----------------------------------------------------------------

time_start <- Sys.time()

options(scipen = 999)

sdmx_name <- "FRB_H8.zip"
sdmx_url <- "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H8&filetype=zip"

dependencies <- c("dplyr", "tidyr", "xml2", "stringr", "purrr", "httr", "feather")
lapply(dependencies, library, character.only = T)

pgm_list <- list.files(here("R"), full.names = T)
sapply(pgm_list, source)

# Convert -------------------------------------------------

# loops below take a couple minutes
# convert xml vals to df list
h8_sdmx <- pull_pub_vals(pub_file = here("data", "H8_data.xml"))

# names
h8_names <- pull_series_names(pub_file = here("data", "H8_data.xml")) %>%
  bind_rows()

# metadata
h8_meta <- pull_pub_meta(pub_file = here("data", "H8_data.xml")) %>%
  left_join(h8_names, by = c("order", "series_name")) %>%
  meta_shorthand() %>%
  bank_group_descs()

# Save --------------------------------------------------------------

# reads out dfs as intermediate feather files if needed
intermediate_df_list <- names(h8_sdmx)

for (i in seq_along(intermediate_df_list)) {
  feather::write_feather(h8_sdmx[[i]],
                         path = paste0(here("data"), "/", intermediate_df_list[i], ".feather"))
}

feather::write_feather(h8_meta,
                       path = paste0(here("data"), "/", "h8_meta.feather"))

message(paste0("Total time elapsed: ", Sys.time() - time_start))
