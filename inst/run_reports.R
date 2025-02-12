# Reports

# Set paths -----------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  message("Missing project filepath")
  stop("Project filepath missing")
}

proj_path <- args[1]
setwd(proj_path)

# Reports --------------------------------------------------------------------

# simple diffs
rmarkdown::render(
  input = "reports/simple_diffs.Rmd",
  params = list(
    program_path = proj_path,
    date_max = Sys.Date() - 7
  )
)

# sa comps
rmarkdown::render(
  input = "reports/sa_comps.Rmd",
  params = list(
    program_path = proj_path,
    date_max = Sys.Date() - 7
  )
)
