library(tidyverse)

# Create a temporary file to download data to
tf <- tempfile()
download.file("https://github.com/mrc-ide/cepi_retrospective_analysis/raw/refs/heads/main/analysis/data_raw/vsly.rds",
              tf)

# read it in and save it in data-derived as it is data that we have created with code
vsly <- readRDS(tf)
saveRDS(vsly, "analysis/data-derived/vsly.rds")
