library(tidyverse)
library(data.table)
library(readxl)

fl <- list.files("processed_data/tracking/")
survey <- as.data.table(readRDS("data/survey_data_r.rds"))
parties <- read_xlsx("data/party_families.xlsx")
fixN <- FALSE
cutoffs <- c(3,10,20,60,120)

# compute ----
## segregation scores ----

news <- lapply(cutoffs,function(y) {sapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- unique(dt[political=="political"][,.(panelist_id)])[["panelist_id"]]
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!="" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[leftright!=6,`:=`(dem=fifelse(leftright<=5,1,0),rep=fifelse(leftright>=7,1,0))]
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
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- unique(dt[political=="political"][,.(panelist_id)])[["panelist_id"]]
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!="" & political=="political"& duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[leftright!=6,`:=`(dem=fifelse(leftright<=5,1,0),rep=fifelse(leftright>=7,1,0))]
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
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- unique(dt[political=="political"][,.(panelist_id)])[["panelist_id"]]
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!="" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[leftright!=6,`:=`(dem=fifelse(leftright<=5,1,0),rep=fifelse(leftright>=7,1,0))]
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
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- unique(dt[political=="political"][,.(panelist_id)])[["panelist_id"]]
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

# plotting ----
## segregation scores ----
seg_scores <- readRDS("processed_data/segregation_scores.RDS")
seg_scores_long <- seg_scores %>% 
  pivot_longer(cols=news:pol,names_to="political") %>% 
  mutate(political=if_else(political=="news","non-political","political"))


ggplot(seg_scores_long) +
  geom_line(aes(x=cutoff,y=value,color=case),size=0.5)+
  geom_point(aes(x=cutoff,y=value,color=case),size=2)+
  facet_wrap(political~.) +
  theme_minimal()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size=12))+
  scale_color_manual(values=pal,name="sample")+
  scale_y_continuous(limits=c(0,0.3),breaks=c(0,0.1,0.2,0.3))+
  labs(x = "cutoff (in sec)",y = "segregation score")+
  guides(colour = guide_legend(override.aes = list(size=2),nrow = 1))

ggsave("figures/segregation_scores.pdf",width = 10,height=6)


