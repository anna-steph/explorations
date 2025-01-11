# Download, format, and save data

# Setup -----------------------------------------------------------------

download_path <- "[path]"
sdmx_name <- "FRB_H8.zip"
sdmx_url <- "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H8&filetype=zip"

dependencies <- c("dplyr", "tidyr", "xml2", "stringr", "purrr", "httr")
lapply(dependencies, library, character.only = T)

pgm_list <- list.files("R", full.names = T)
sapply(pgm_list, source)

# Download and convert -------------------------------------------------

download.file(sdmx_url, paste0(download_path, sdmx_name), mode = "wb")

# unzip SDMX xml file
unzip_call <- paste0("unzip data/FRB_H8.zip 'H8_data.xml' -d data")
system(unzip_call)

# loops below take a couple minutes
# convert xml vals to df list
h8_sdmx <- pull_pub_vals(pub_file = "data/H8_data.xml")

# names
h8_names <- pull_series_names(pub_file = "data/H8_data.xml") %>%
  bind_rows()

# metadata
h8_meta <- pull_pub_meta(pub_file = "data/H8_data.xml") %>%
  left_join(h8_names, by = c("order", "series_name"))

# Save --------------------------------------------------------------


