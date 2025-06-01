# Reports

# Set paths -----------------------------------------------------------------

library(rmarkdown)
library(here)
proj_path <- here::here()

# Reports --------------------------------------------------------------------

# simple diffs
xfun::Rscript_call(
  rmarkdown::render,
  list(
    input = here("reports", "simple_diffs.Rmd"),
    envir = parent.frame(),
    output_file = here("reports", "simple_diffs.html"),
    params = list(
      program_path = proj_path,
      date_max = Sys.Date() - 7
    )
  )
)

# sa comps
xfun::Rscript_call(
  rmarkdown::render,
  list(
    input = here("reports", "sa_comps.Rmd"),
    envir = parent.frame(),
    output_file = here("reports", "sa_comps.html"),
    params = list(
      program_path = proj_path,
      date_max = Sys.Date() - 7
    )
  )
)

