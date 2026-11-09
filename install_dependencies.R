# Installs the R packages TaxaCurate (worms_pipeline_app_v8) needs.
# Run from the app folder:  Rscript install_dependencies.R
cran <- c("shiny", "bslib", "dplyr", "tidyr", "stringr", "readr", "purrr", "tibble", "ggplot2",
          "DT", "worrms", "future", "promises", "shinyjs", "httr2", "jsonlite", "data.table")
need <- setdiff(cran, rownames(installed.packages()))
if (length(need)) install.packages(need, repos = "https://cloud.r-project.org")
# Optional, only for Module D (contaminant removal):
#   decontam  : BiocManager::install(c("decontam", "phyloseq"))   # phyloseq is also needed by the decontam method
#   microDecon: remotes::install_github("donaldtmcknight/microDecon")
cat("R", as.character(getRversion()), "- packages installed:", length(cran) - length(setdiff(cran, rownames(installed.packages()))), "of", length(cran), "\n")
cat("Optional decontam:", requireNamespace("decontam", quietly = TRUE), " microDecon:", requireNamespace("microDecon", quietly = TRUE), "\n")
