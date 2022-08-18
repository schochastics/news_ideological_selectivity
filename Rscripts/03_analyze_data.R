library(tidyverse)
library(data.table)
library(readxl)
library(patchwork)
library(overlapping)
fl <- list.files("processed_data/tracking/news_only/",pattern = "csv")
survey <- as.data.table(readRDS("data/survey_data_r.rds"))
parties <- read_xlsx("data/party_families.xlsx")
fixN <- FALSE
visits <- TRUE
cutoffs <- c(3,10,20,60,120)
boundaries <- list( from = -1, to = 1 )
# compute ----
## segregation scores ----

news <- lapply(cutoffs,function(y) {sapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
  if(fixN){
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!="" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  # dt[leftright!=6,`:=`(dem=fifelse(leftright<=5,1,0),rep=fifelse(leftright>=7,1,0))]
  dt[,`:=`(dem=fifelse(leftright<=4,1,0),rep=fifelse(leftright>=8,1,0),cen=fifelse(leftright%in%c(5,6,7),1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,dem,rep)])
  # peeps <- unique(dt1[,.(panelist_id,dem,rep)])
  nd <- sum(dt1$dem,na.rm = TRUE)
  nr <- sum(dt1$rep,na.rm = TRUE)
  
  dt2 <- dt1[,.(demvisits=sum(dem,na.rm = TRUE),repvisits=sum(rep,na.rm = TRUE)),by=.(domain)]
  dt2[,segscore:=(repvisits/nr-demvisits/nd) * repvisits/(repvisits+demvisits)]
  sum(dt2$segscore,na.rm = TRUE)
  
})
})


pol <- lapply(cutoffs,function(y) {sapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
  if(fixN){
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!="" & political=="political"& duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  # dt[leftright!=6,`:=`(dem=fifelse(leftright<=5,1,0),rep=fifelse(leftright>=7,1,0))]
  dt[,`:=`(dem=fifelse(leftright<=4,1,0),rep=fifelse(leftright>=8,1,0),cen=fifelse(leftright%in%c(5,6,7),1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,dem,rep)])
  nd <- sum(dt1$dem,na.rm = TRUE)
  nr <- sum(dt1$rep,na.rm = TRUE)
  
  dt2 <- dt1[,.(demvisits=sum(dem,na.rm = TRUE),repvisits=sum(rep,na.rm = TRUE)),by=.(domain)]
  dt2[,segscore:=(repvisits/nr-demvisits/nd) * repvisits/(repvisits+demvisits)]
  sum(dt2$segscore,na.rm = TRUE)
  
})
})

do.call("rbind",news) %>% 
  as_tibble() %>% 
  mutate(cutoff=cutoffs,type="news") %>% 
  bind_rows(
    do.call("rbind",pol) %>% 
      as_tibble() %>% 
      mutate(cutoff=cutoffs,type="pol")
  ) %>% 
  pivot_longer(all_of(fl)) %>% 
  pivot_wider(names_from = type) %>% 
  mutate(case=str_extract(name,"^[a-z]{2}")) %>% 
  saveRDS("processed_data/segregation_scores.RDS")

## dissimilarity index ----
news <- lapply(cutoffs,function(y) {sapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
  if(fixN){
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!="" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  # dt[leftright!=6,`:=`(dem=fifelse(leftright<=5,1,0),rep=fifelse(leftright>=7,1,0))]
  dt[,`:=`(dem=fifelse(leftright<=4,1,0),rep=fifelse(leftright>=8,1,0),cen=fifelse(leftright%in%c(5,6,7),1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,dem,rep)])
  # peeps <- unique(dt1[,.(panelist_id,dem,rep)])
  nd <- sum(dt1$dem,na.rm = TRUE)
  nr <- sum(dt1$rep,na.rm = TRUE)
  
  dt2 <- dt1[,.(demvisits=sum(dem,na.rm = TRUE),repvisits=sum(rep,na.rm = TRUE)),by=.(domain)]
  dt2[,segscore:=abs((repvisits/nr-demvisits/nd))]
  0.5 * sum(dt2$segscore,na.rm = TRUE)
  
})
})


pol <- lapply(cutoffs,function(y) {sapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
  if(fixN){
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!="" & political=="political"& duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[leftright!=6,`:=`(dem=fifelse(leftright<=5,1,0),rep=fifelse(leftright>=7,1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,dem,rep)])
  nd <- sum(dt1$dem,na.rm = TRUE)
  nr <- sum(dt1$rep,na.rm = TRUE)
  
  dt2 <- dt1[,.(demvisits=sum(dem,na.rm = TRUE),repvisits=sum(rep,na.rm = TRUE)),by=.(domain)]
  dt2[,segscore:=abs((repvisits/nr-demvisits/nd))]
  0.5 * sum(dt2$segscore,na.rm = TRUE)
  
})
})

do.call("rbind",news) %>% 
  as_tibble() %>% 
  mutate(cutoff=cutoffs,type="news") %>% 
  bind_rows(
    do.call("rbind",pol) %>% 
      as_tibble() %>% 
      mutate(cutoff=cutoffs,type="pol")
  ) %>% 
  pivot_longer(all_of(fl)) %>% 
  pivot_wider(names_from = type) %>% 
  mutate(case=str_extract(name,"^[a-z]{2}")) %>% 
  saveRDS("processed_data/dissimilarity_scores.RDS")


## average diversity of news diets ----------

news <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
  if(fixN){
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  if(!visits){
    dt1 <- unique(dt[,.(domain,panelist_id,ideo)])  
  } else{
    dt1 <- dt[,.(domain,panelist_id,ideo)]
  }
  dt3 <- dt[,.(domain,panelist_id,ideo)]
  dt3 <- dt3[!is.na(ideo)]
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt3[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  dt3[,lcr:=fifelse(avg_ideo<=-0.2,"l",fifelse(avg_ideo>=0.2,"r","c"))]
  tibble(country = x,cutoff = y,
         popsd = mean(dt3[,.N,.(panelist_id,lcr)][,.(frac=N*(N)/(sum(N)*(sum(N)))),.(panelist_id)][,.(Diversity=1-sum(frac,na.rm = TRUE)),.(panelist_id)][["Diversity"]]), 
         entropy= mean(dt3[,.N,.(panelist_id,lcr)][,.(frac=N/sum(N)),.(panelist_id)][,.(Diversity=-sum(frac*log(frac))),.(panelist_id)][["Diversity"]]), 
         type="all news")
  
})
})

pol <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
  if(fixN){
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& political=="political" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  if(!visits){
    dt1 <- unique(dt[,.(domain,panelist_id,ideo)])  
  } else{
    dt1 <- dt[,.(domain,panelist_id,ideo)]
  }
  dt3 <- dt[,.(domain,panelist_id,ideo)]
  dt3 <- dt3[!is.na(ideo)]
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt3[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  dt3[,lcr:=fifelse(avg_ideo<=-0.2,"l",fifelse(avg_ideo>=0.2,"r","c"))]
  tibble(country = x,cutoff = y,
         popsd = mean(dt3[,.N,.(panelist_id,lcr)][,.(frac=N*(N)/(sum(N)*(sum(N)))),.(panelist_id)][,.(Diversity=1-sum(frac,na.rm = TRUE)),.(panelist_id)][["Diversity"]]), 
         entropy= mean(dt3[,.N,.(panelist_id,lcr)][,.(frac=N/sum(N)),.(panelist_id)][,.(Diversity=-sum(frac*log(frac))),.(panelist_id)][["Diversity"]]), 
         type="political news")
})
})

tbl <- bind_rows(news,pol) |> 
  mutate(cases_long = case_when(country=="de.csv" ~ "Germany",
                                country=="es.csv" ~ "Spain",
                                country=="fr.csv" ~ "France",
                                country=="it.csv" ~ "Italy",
                                country=="uk.csv" ~ "United Kingdom",
                                country=="us.csv" ~ "USA"))

tbl|> select(cases_long,political=type,cutoff,value=popsd) |> mutate(type = "average news diet diversity") |>
  saveRDS("processed_data/diversity_scores.RDS")

## mean of absolute individual news diet ----
fixN <- FALSE
visits <- FALSE
news <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
  if(fixN){
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  if(!visits){
    dt1 <- unique(dt[,.(domain,panelist_id,ideo)])  
  } else{
    dt1 <- dt[,.(domain,panelist_id,ideo)]
  }
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt1[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  tibble(country = x,cutoff = y,
         popsd = mean(dt1[,.(avg_ideo_aud=mean(abs(avg_ideo))),.(panelist_id)][["avg_ideo_aud"]]), 
         type="all news")
  
})
})

pol <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
  if(fixN){
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& political=="political" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  if(!visits){
    dt1 <- unique(dt[,.(domain,panelist_id,ideo)])  
  } else{
    dt1 <- dt[,.(domain,panelist_id,ideo)]
  }
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt1[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  tibble(country = x,cutoff = y,
         popsd = mean(dt1[,.(avg_ideo_aud=mean(abs(avg_ideo))),.(panelist_id)][["avg_ideo_aud"]]), 
         type="political news")
})
})

bind_rows(news,pol) |> 
  mutate(cases_long = case_when(country=="de.csv" ~ "Germany",
                                country=="es.csv" ~ "Spain",
                                country=="fr.csv" ~ "France",
                                country=="it.csv" ~ "Italy",
                                country=="uk.csv" ~ "United Kingdom",
                                country=="us.csv" ~ "USA")) |> 
  select(cases_long,political=type,cutoff,value=popsd) |> 
  mutate(type="mean of absolute individual news diet") |> 
  saveRDS("processed_data/mean_of_abs_indiv_news_diet.RDS")
