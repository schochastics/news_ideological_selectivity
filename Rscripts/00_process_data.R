# script to process raw data (needs access to raw data)

# needed libraries
library(data.table)

# load auxiliary data
domain_lists <- readRDS("data/domain_lists.rds")
political_urls <- as.data.table(readRDS("data/political_URLs.rds"))
news_types <- as.data.table(readRDS("data/news_types.rds"))

# get file list of raw data
fl <- list.files("data/tracking/data_parsed", full.names = T)
countries <- stringr::str_extract(fl, "_[a-z]{2}[_\\.]") |> stringr::str_remove_all("[_\\.]")

for (i in seq_along(fl)) {
  cat(countries[i], "\n")
  dt <- fread(fl[i], verbose = FALSE)

  dt <- dt[, c("panel", "panelist_id", "url", "domain", "duration", "used_at")]
  dt[, timestamp := used_at]
  dt[, used_at := NULL]
  setorder(dt, panelist_id, timestamp)
  
  dt[, visit := cumsum(url != shift(url, n = 1, type = "lag", fill = 0)), by = .(panelist_id)]
  dt[, day := as.Date(timestamp)]

  dt1 <- dt[, .(visits = .N, duration = sum(as.numeric(duration), na.rm = TRUE)),
    by = .(panelist_id, visit, url, domain, day)
  ]
  dt1 <- dt1[!is.na(duration)]
  dt1[, newsportal := grepl(paste(domain_lists$newsportals, collapse = "|"), url)]
  dt1[, domain := fcase(newsportal, paste0(domain, "/NEWS"), !newsportal, domain)]
  dt1[political_urls, on = .(url), political := political]
  dt1[news_types, on = .(domain), type := type]
  dt1[, type := fcase(newsportal, "Portal news", !newsportal, type)]

  if (!dir.exists("processed_data")) dir.create("processed_data")
  if (!dir.exists("processed_data/tracking")) dir.create("processed_data/tracking")

  fwrite(dt1, paste0("processed_data/tracking/", countries[i], ".csv"))
}
