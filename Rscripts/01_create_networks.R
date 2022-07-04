library(data.table)
library(tidyverse)
library(igraph)
library(backbone)
source("Rscripts/helpers.R")

if (!dir.exists("processed_data/networks")) dir.create("processed_data/networks")

# parameters ----
cutoffs <- c(3, 10, 20, 60, 120)
reaches <- c(0.01, 0.03, 0.05)
fixN <- FALSE

# create all networks ----
fl <- list.files("processed_data/tracking", full.names = TRUE)
res <- tibble(
  country = character(0),
  type = character(0),
  reach = numeric(0),
  cutoff = numeric(0),
  political = logical(0),
  fixN = logical(0),
  network = list()
)
for (f in fl) {
  ctry <- str_remove(str_extract(f, "[a-z]{2}\\."), "\\.")
  dt <- data.table::fread(f)
  for (cval in cutoffs) {
    for (reach in reaches) {
      gnews <- create_networks(dt, political = FALSE, weights = FALSE, fixN = fixN, reach = reach, cutoff = cval)
      gpols <- create_networks(dt, political = TRUE, weights = FALSE, fixN = fixN, reach = reach, cutoff = cval)

      tmp <- tibble(
        country = rep(ctry, 6), type = c(names(gnews), names(gpols)),
        reach = reach, cutoff = cval, political = rep(c(FALSE, TRUE), each = 3), fixN = fixN,
        network = c(unname(gnews), unname(gpols))
      )

      res <- bind_rows(res, tmp)
    }
  }
}
saveRDS(res, "processed_data/networks.RDS")
