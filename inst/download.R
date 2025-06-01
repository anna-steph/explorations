# Download data
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

# Download and convert -------------------------------------------------

download.file(sdmx_url, paste0(download_path, sdmx_name))

