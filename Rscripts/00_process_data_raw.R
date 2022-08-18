# script to process raw data (needs access to raw data)

# needed libraries
library(data.table)

# load auxiliary data
domain_lists <- readRDS("data/domain_lists.rds")
doms <- paste(domain_lists$newsportals, collapse = "|")
political_urls <- as.data.table(readRDS("data/political_URLs.rds"))
news_types <- as.data.table(readRDS("data/news_types.rds"))

#setkeys
setkey(political_urls,url)
setkey(news_types,domain)

# get file list of raw data
countries <- c("ESP","FRA","GER","ITA","UK","USA")
fl <- paste0("data/tracking/data_raw/",countries,"/2019-06-16_all.rda")
out_files <- c("es.csv","fr.csv","de.csv","it.csv","uk.csv" ,"us.csv")

for (i in c(5,6)) {
  cat(countries[i], "\n")
  load(fl[i])
  cat("-loaded\n")
  urls_final <- as.data.table(urls_final)
  urls_final <- urls_final[, c("panel", "panelist_id", "url", "domain", "duration", "used_at")]
  urls_final[, timestamp := used_at]
  urls_final[, used_at := NULL]
  setorder(urls_final, panelist_id, timestamp)
  
  urls_final[, visit := cumsum(url != shift(url, n = 1, type = "lag", fill = 0)), by = .(panelist_id)]
  urls_final[, day := as.Date(timestamp)]
  # setkey(urls_final,panelist_id, visit, url, domain, day)
  dt1 <- suppressWarnings(urls_final[, .(visits = .N, duration = sum(as.numeric(duration), na.rm = TRUE)),
                             by = .(panelist_id, visit, url, domain, day)
  ])
  cat("-done summarize\n")
  dt1 <- dt1[!is.na(duration)]
  dt1[,visit:=NULL]
  
  dt1[, newsportal := grepl(doms,url,perl = TRUE)]
  cat("-found news portals\n")
  dt1[, domain := fcase(newsportal, paste0(domain, "/NEWS"), !newsportal, domain)]
  # setkey(dt1,url)
  dt1[,political:=fifelse(url%in%political_urls$url,"political","")]
  # dt1[political_urls, on = .(url), political := political]
  setkey(dt1,domain)
  dt1[news_types, on = .(domain), type := type]
  dt1[, type := fcase(newsportal, "Portal news", !newsportal, type)]
  dt1[,newsportal:=NULL]
  
  if (!dir.exists("processed_data")) dir.create("processed_data")
  if (!dir.exists("processed_data/tracking")) dir.create("processed_data/tracking")
  
  fwrite(dt1, paste0("processed_data/tracking/", out_files[i]))
}
