library(tidyverse)
library(data.table)
library(readxl)
library(patchwork)
source("Rscripts/helpers.R")
fl <- list.files("processed_data/tracking/")
survey <- as.data.table(readRDS("data/survey_data_r.rds"))
fixN <- TRUE
cutoffs <- c(3,10,20,60,120)
boundaries <- list( from = -1, to = 1 )
visits <- TRUE

news <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
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
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
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
                                country=="us.csv" ~ "USA"))|>
  ggplot(aes(x=cutoff,y=popsd,col=type)) +
  geom_point() +
  geom_line()+
  scale_color_manual(values=c("political news" = "#AA8939","all news" = "#303C74"),name="")+
  theme_minimal()+
  facet_wrap(cases_long~.)+
  theme(legend.position = "bottom")+
  labs(x="cutoff",y="mean of absolute individual news diet")

ggsave("figures/old/explore/abs_indiv_scores_120.png",width = 10,height=6,bg = "white")


news <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- unique(dt[political=="political"][,.(panelist_id)])[["panelist_id"]]
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  tibble(country = x,cutoff = y,
         popsd = mean(dt[,.(avg_ideo_aud=sd(avg_ideo)),.(panelist_id)][["avg_ideo_aud"]],na.rm=TRUE), 
         type="all news")
  
})
})

pol <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- unique(dt[political=="political"][,.(panelist_id)])[["panelist_id"]]
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& political=="political" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  tibble(country = x,cutoff = y,
         popsd = mean(dt[,.(avg_ideo_aud=sd(avg_ideo)),.(panelist_id)][["avg_ideo_aud"]],na.rm=TRUE),
         type="political news")
})
})

bind_rows(news,pol) |> 
  mutate(cases_long = case_when(country=="de.csv" ~ "Germany",
                                country=="es.csv" ~ "Spain",
                                country=="fr.csv" ~ "France",
                                country=="it.csv" ~ "Italy",
                                country=="uk.csv" ~ "United Kingdom",
                                country=="us.csv" ~ "USA"))|>
  ggplot(aes(x=cutoff,y=popsd,col=type)) +
  geom_point() +
  geom_line()+
  scale_color_manual(values=c("political news" = "#AA8939","all news" = "#303C74"),name="")+
  theme_minimal()+
  facet_wrap(cases_long~.)+
  theme(legend.position = "bottom")+
  labs(x="cutoff",y="sd of individual news diet")

ggsave("figures/old/explore/var_indiv_scores.png",width = 10,height=6,bg = "white")


news <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
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
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
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

bind_rows(news,pol) |> 
  mutate(cases_long = case_when(country=="de.csv" ~ "Germany",
                                country=="es.csv" ~ "Spain",
                                country=="fr.csv" ~ "France",
                                country=="it.csv" ~ "Italy",
                                country=="uk.csv" ~ "United Kingdom",
                                country=="us.csv" ~ "USA"))|>
  ggplot(aes(x=cutoff,y=popsd,col=type)) +
  geom_point() +
  geom_line()+
  scale_color_manual(values=c("political news" = "#AA8939","all news" = "#303C74"),name="")+
  theme_minimal()+
  facet_wrap(cases_long~.)+
  theme(legend.position = "bottom")+
  labs(x="cutoff",y="average diversity")

ggsave("figures/old/explore/avg_diversity_scores_120.png",width = 10,height=6,bg = "white")

bind_rows(news,pol) |> 
  mutate(cases_long = case_when(country=="de.csv" ~ "Germany",
                                country=="es.csv" ~ "Spain",
                                country=="fr.csv" ~ "France",
                                country=="it.csv" ~ "Italy",
                                country=="uk.csv" ~ "United Kingdom",
                                country=="us.csv" ~ "USA"))|>
  ggplot(aes(x=cutoff,y=entropy,col=type)) +
  geom_point() +
  geom_line()+
  scale_color_manual(values=c("political news" = "#AA8939","all news" = "#303C74"),name="")+
  theme_minimal()+
  facet_wrap(cases_long~.)+
  theme(legend.position = "bottom")+
  labs(x="cutoff",y="average entropy")

ggsave("figures/old/explore/avg_entropy.png",width = 10,height=6,bg = "white")



news <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- unique(dt[political=="political"][,.(panelist_id)])[["panelist_id"]]
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt1[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  dt1[,lcr:=fifelse(avg_ideo<=-0.2,"l",fifelse(avg_ideo>=0.2,"r","c"))]
  centrists <- dt1[lcr=="c"][["panelist_id"]]
  dt3 <- dt1[,.N,.(panelist_id,lcr)][,.N,.(panelist_id)]
  tibble(country = x,cutoff = y,
         popsd = sum(dt3[["N"]]==1 & !dt3[["panelist_id"]]%in%centrists)/nrow(dt3),
         type="all news")
  
})
})

pol <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- unique(dt[political=="political"][,.(panelist_id)])[["panelist_id"]]
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& political=="political" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt1[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  dt1[,lcr:=fifelse(avg_ideo<=-0.2,"l",fifelse(avg_ideo>=0.2,"r","c"))]
  centrists <- dt1[lcr=="c"][["panelist_id"]]
  dt3 <- dt1[,.N,.(panelist_id,lcr)][,.N,.(panelist_id)]
  tibble(country = x,cutoff = y,
         popsd = sum(dt3[["N"]]==1 & !dt3[["panelist_id"]]%in%centrists)/nrow(dt3),
         type="political news")
})
})

bind_rows(news,pol) |> 
  mutate(cases_long = case_when(country=="de.csv" ~ "Germany",
                                country=="es.csv" ~ "Spain",
                                country=="fr.csv" ~ "France",
                                country=="it.csv" ~ "Italy",
                                country=="uk.csv" ~ "United Kingdom",
                                country=="us.csv" ~ "USA"))|>
  ggplot(aes(x=cutoff,y=popsd,col=type)) +
  geom_point() +
  geom_line()+
  scale_color_manual(values=c("political news" = "#AA8939","all news" = "#303C74"),name="")+
  theme_minimal()+
  facet_wrap(cases_long~.)+
  theme(legend.position = "bottom")+
  labs(x="cutoff",y="fraction of people in echo chambers")

ggsave("figures/old/explore/echo_chamber_scores.png",width = 10,height=6,bg = "white")


news <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- dtr <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- unique(dt[political=="political"][,.(panelist_id)])[["panelist_id"]]
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt1[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  dt1[,lcr:=fifelse(avg_ideo<=-0.2,"l",fifelse(avg_ideo>=0.2,"r","c"))]
  centrists <- dt1[lcr=="c"][["panelist_id"]]
  dt3 <- dt1[,.N,.(panelist_id,lcr)][,.N,.(panelist_id)]
  tibble(country = x,cutoff = y,
         popsd = sum(dt3[["N"]]==1 & !dt3[["panelist_id"]]%in%centrists)/length(unique(dtr[["panelist_id"]])),
         type="all news")
  
})
})

pol <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- dtr <-  data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- unique(dt[political=="political"][,.(panelist_id)])[["panelist_id"]]
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& political=="political" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt1[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  dt1[,lcr:=fifelse(avg_ideo<=-0.2,"l",fifelse(avg_ideo>=0.2,"r","c"))]
  centrists <- dt1[lcr=="c"][["panelist_id"]]
  dt3 <- dt1[,.N,.(panelist_id,lcr)][,.N,.(panelist_id)]
  tibble(country = x,cutoff = y,
         popsd = sum(dt3[["N"]]==1 & !dt3[["panelist_id"]]%in%centrists)/length(unique(dtr[["panelist_id"]])),
         type="political news")
})
})

bind_rows(news,pol) |> 
  mutate(cases_long = case_when(country=="de.csv" ~ "Germany",
                                country=="es.csv" ~ "Spain",
                                country=="fr.csv" ~ "France",
                                country=="it.csv" ~ "Italy",
                                country=="uk.csv" ~ "United Kingdom",
                                country=="us.csv" ~ "USA"))|>
  ggplot(aes(x=cutoff,y=popsd,col=type)) +
  geom_point() +
  geom_line()+
  scale_color_manual(values=c("political news" = "#AA8939","all news" = "#303C74"),name="")+
  theme_minimal()+
  facet_wrap(cases_long~.)+
  theme(legend.position = "bottom")+
  labs(x="cutoff",y="fraction of people in echo chambers")

ggsave("figures/old/explore/echo_chamber_scores1.png",width = 10,height=6,bg = "white")



# diversity tests ----
pol <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  dt <- dt[type!=""& political=="political" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt1[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  dt1[,lcr:=fifelse(avg_ideo<=-0.2,"left",fifelse(avg_ideo>=0.2,"right","center"))]
  tibble(country = x,cutoff = y,
         popsd = mean(dt1[,.N,.(panelist_id,lcr)][,.(frac=N*(N)/(sum(N)*(sum(N)))),.(panelist_id)][,.(Diversity=1-sum(frac,na.rm = TRUE)),.(panelist_id)][["Diversity"]]), 
         entropy= mean(dt1[,.N,.(panelist_id,lcr)][,.(frac=N/sum(N)),.(panelist_id)][,.(Diversity=-sum(frac*log(frac))),.(panelist_id)][["Diversity"]]), 
         type="political news")
})
})

tbl <- dt1[,.N,.(panelist_id,lcr)]
tbl <- tbl |> group_by(panelist_id) |> mutate(tot=sum(N))

tbl |> 
  ggplot(aes(x=reorder(panelist_id,tot),y=N,fill=lcr))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("yellow","blue","red"))+
  theme(legend.position = "bottom",axis.text.x = element_blank(),axis.ticks.x = element_blank())

tbl |> 
  # filter(tot>2) |> 
  mutate(frac = N/tot) |> 
  group_by(panelist_id) |> 
  summarise(div = 1-sum(frac^2)) |> 
  pull(div) |> mean()


tbl |> 
  filter(tot>2) |>
  mutate(frac = N*(N-1)/(tot*(tot-1))) |> 
  group_by(panelist_id) |> 
  summarise(div = 1-sum(frac)) |> 
  pull(div) |> mean(na.rm=TRUE)


news <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  peeps <- dt[type!=""& political=="political" & duration>=120]
  peeps <- unique(peeps[["panelist_id"]])
  dt <- dt[type!=""& duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt1 <- dt1[panelist_id%in%peeps]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt1[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  dt1[,lcr:=fifelse(avg_ideo<=-0.2,"l",fifelse(avg_ideo>=0.2,"r","c"))]
  tibble(country = x,cutoff = y,
         popsd = mean(dt1[,.N,.(panelist_id,lcr)][,.(frac=N*(N-1)/(sum(N)*(sum(N)-1))),.(panelist_id)][,.(Diversity=1-sum(frac,na.rm = TRUE)),.(panelist_id)][["Diversity"]]), 
         entropy= mean(dt1[,.N,.(panelist_id,lcr)][,.(frac=N/sum(N)),.(panelist_id)][,.(Diversity=-sum(frac*log(frac))),.(panelist_id)][["Diversity"]]), 
         type="all news")
  
})
})

pol <- map_dfr(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  peeps <- dt[type!=""& political=="political" & duration>=120]
  peeps <- unique(peeps[["panelist_id"]])
  dt <- dt[type!=""& political=="political" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,ideo:=fifelse(leftright<=4,-1,fifelse(leftright>=8,1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt1 <- dt1[panelist_id%in%peeps]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  ideo_scores <- dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  dt1[ideo_scores,on = .(domain), avg_ideo := cor_avg_ideo]
  dt1[,lcr:=fifelse(avg_ideo<=-0.2,"l",fifelse(avg_ideo>=0.2,"r","c"))]
  tibble(country = x,cutoff = y,
         popsd = mean(dt1[,.N,.(panelist_id,lcr)][,.(frac=N*(N-1)/(sum(N)*(sum(N)-1))),.(panelist_id)][,.(Diversity=1-sum(frac,na.rm = TRUE)),.(panelist_id)][["Diversity"]]), 
         entropy= mean(dt1[,.N,.(panelist_id,lcr)][,.(frac=N/sum(N)),.(panelist_id)][,.(Diversity=-sum(frac*log(frac))),.(panelist_id)][["Diversity"]]), 
         type="political news")
})
})

bind_rows(news,pol) |> 
  mutate(cases_long = case_when(country=="de.csv" ~ "Germany",
                                country=="es.csv" ~ "Spain",
                                country=="fr.csv" ~ "France",
                                country=="it.csv" ~ "Italy",
                                country=="uk.csv" ~ "United Kingdom",
                                country=="us.csv" ~ "USA"))|>
  ggplot(aes(x=cutoff,y=popsd,col=type)) +
  geom_point() +
  geom_line()+
  scale_color_manual(values=c("political news" = "#AA8939","all news" = "#303C74"),name="")+
  theme_minimal()+
  facet_wrap(cases_long~.)+
  theme(legend.position = "bottom")+
  labs(x="cutoff",y="average diversity")


bind_rows(news,pol) |> 
  mutate(cases_long = case_when(country=="de.csv" ~ "Germany",
                                country=="es.csv" ~ "Spain",
                                country=="fr.csv" ~ "France",
                                country=="it.csv" ~ "Italy",
                                country=="uk.csv" ~ "United Kingdom",
                                country=="us.csv" ~ "USA"))|>
  ggplot(aes(x=cutoff,y=entropy,col=type)) +
  geom_point() +
  geom_line()+
  scale_color_manual(values=c("political news" = "#AA8939","all news" = "#303C74"),name="")+
  theme_minimal()+
  facet_wrap(cases_long~.)+
  theme(legend.position = "bottom")+
  labs(x="cutoff",y="average diversity")
ggsave("figures/old/explore/avg_diversity_scores4.png",width = 10,height=6,bg = "white")
