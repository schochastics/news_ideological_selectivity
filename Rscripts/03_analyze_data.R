library(tidyverse)
library(data.table)
library(readxl)
library(patchwork)
library(overlapping)
fl <- list.files("processed_data/tracking/")
survey <- as.data.table(readRDS("data/survey_data_r.rds"))
parties <- read_xlsx("data/party_families.xlsx")
fixN <- TRUE
cutoffs <- c(3,10,20,60,120)
boundaries <- list( from = -1, to = 1 )
# compute ----
## segregation scores ----

news <- lapply(cutoffs,function(y) {sapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
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
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
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
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
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
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
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


## individual news exposure (testing) ----
news <- lapply(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,`:=`(dem=fifelse(leftright<=4,1,0),rep=fifelse(leftright>=8,1,0),cen=fifelse(leftright%in%c(5,6,7),1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,dem,rep,cen)])
  # dt1 <- dt
  dt1 <- dt1[!is.na(dem)]
  # rep_scores <- dt1[,.(rep_frac=sum(rep)/sum(dem+rep+cen)),by=.(domain)]
  rep_scores <- dt1[,.(rep_frac=sum(rep-dem)/sum(dem+rep)),by=.(domain)]
  dt1[rep_scores,on = .(domain), rep_frac := rep_frac]
  dt2 <- dt1[,.(avg_rep_exp=mean(rep_frac,na.rm=TRUE)),by = .(panelist_id,dem,cen,rep)]
  dt2[,`:=`(ideo=-dem+rep,cutoff=y,case=x)]
  dt2[,.(panelist_id,avg_rep_exp,ideo,cutoff,case)]
})
})

pol <- lapply(cutoffs,function(y) { lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  if(fixN){
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dt <- dt[panelist_id%in%peeps]
  }
  dt <- dt[type!=""& political=="political" & duration>=y]
  dt[survey, on = .(panelist_id), leftright := leftright]
  dt[,`:=`(dem=fifelse(leftright<=4,1,0),rep=fifelse(leftright>=8,1,0),cen=fifelse(leftright%in%c(5,6,7),1,0))]
  dt1 <- unique(dt[,.(domain,panelist_id,dem,rep,cen)])
  # dt1 <- dt
  dt1 <- dt1[!is.na(dem)]
  # rep_scores <- dt1[,.(rep_frac=sum(rep)/sum(dem+rep+cen)),by=.(domain)]
  rep_scores <- dt1[,.(rep_frac=sum(rep-dem)/sum(dem+rep)),by=.(domain)]
  dt1[rep_scores,on = .(domain), rep_frac := rep_frac]
  dt2 <- dt1[,.(avg_rep_exp=mean(rep_frac,na.rm=TRUE)),by = .(panelist_id,dem,cen,rep)]
  dt2[,`:=`(ideo=-dem+rep,cutoff=y,case=x)]
  dt2[,.(panelist_id,avg_rep_exp,ideo,cutoff,case)]
})
})

p1 <- map_dfr(news,identity,.id="id") |>
  mutate(case=str_remove(case,"\\.csv")) |>
  # mutate(cutoff=cutoffs[as.numeric(id)]) |>
  # mutate(strip_lab = paste0("cutoff = ",cutoff)) |>
  # mutate(strip_lab = fct_reorder(strip_lab,cutoff))|>
  ggplot(aes(avg_rep_exp,group=ideo,fill=as.factor(ideo)))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values=c("blue","grey66","red"),labels=c("left","center","right"),name="individual alignment")+
  labs(x="favorability scores")+
  facet_grid(case~cutoff,scales = "free_y")+
  theme_minimal()+
  theme(legend.position = "bottom")

p2 <- map_dfr(pol,identity,.id="id") |>
  mutate(case=str_remove(case,"\\.csv")) |>
  # mutate(cutoff=cutoffs[as.numeric(id)]) |>
  # mutate(strip_lab = paste0("cutoff = ",cutoff)) |>
  # mutate(strip_lab = fct_reorder(strip_lab,cutoff))|>
  ggplot(aes(avg_rep_exp,group=ideo,fill=as.factor(ideo)))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values=c("blue","grey66","red"),labels=c("left","center","right"),name="individual alignment")+
  labs(x="favorability scores",y="")+
  facet_grid(case~cutoff,scales = "free_y")+
  theme_minimal()+
  theme(legend.position = "bottom")

p1+p2+
  plot_annotation(tag_levels = "a",tag_suffix = ")")+
  plot_layout(ncol=2,guides = "collect") & theme(legend.position = 'bottom')

ggsave("figures/old/explore/indiv_exposure6.png",width = 14*1.5,height=5*1.5,bg = "white")  

dens_news <- map_dfr(news,identity,.id="id") |>
  group_by(cutoff,case,ideo)|>
  summarise(d=list(avg_rep_exp),.groups="drop")|>
  mutate(case=str_remove_all(case,"\\.csv"))|>
  pivot_wider(names_from = ideo,values_from = d)|>
  rename(left=3,center=4,right=5)|>
  rowwise()|>
  mutate(coef=unname(overlap(list(X1=left,X2=right),boundaries = boundaries)$OV),
         political="non-political")



dens_pol <- map_dfr(pol,identity,.id="id") |>
  group_by(cutoff,case,ideo)|>
  summarise(d=list(avg_rep_exp),.groups="drop") |>
  mutate(case=str_remove_all(case,"\\.csv"))|>
  pivot_wider(names_from = ideo,values_from = d)|>
  rename(left=3,center=4,right=5)|>
  rowwise()|>
  mutate(coef=unname(overlap(list(X1=left,X2=right),boundaries = boundaries)$OV),
         political="political")

bind_rows(dens_news,dens_pol)|>
  ggplot(aes(x=cutoff,y=coef,col=case))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=pal,name="sample")+
  theme_minimal()+
  facet_wrap(political~.) +
  theme_minimal()+
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size=12))+
  labs(x = "cutoff (in sec)",y = "overlap coefficient")+
  guides(colour = guide_legend(override.aes = list(size=2),nrow = 1))

ggsave("figures/old/explore/overlap_coef.png",width = 10,height=6,bg = "white")


## pop sd from fletcher ----
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
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  tibble(country = x,cutoff = y,popsd = psd(dt2$cor_avg_ideo,n=dt2$N), type="all news")
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
  dt1 <- unique(dt[,.(domain,panelist_id,ideo)])
  dt1 <- dt1[!is.na(ideo)]
  dt2 <- dt1[,.(avg_ideo=mean(ideo),N=.N),by=.(domain)]
  dt2[,cor_avg_ideo := avg_ideo-mean(dt1$ideo)]
  tibble(country = x,cutoff = y,popsd = psd(dt2$cor_avg_ideo,n=dt2$N), type="political news")
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
  labs(x="cutoff",y="polarization score")

ggsave("figures/old/explore/polarization_scores.png",width = 10,height=6,bg = "white")

# plotting ----
pal <- c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", "#244579FF", "#C6242DFF")
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
  scale_y_continuous(limits=c(0,0.35),breaks=c(0,0.1,0.2,0.3))+
  labs(x = "cutoff (in sec)",y = "segregation score")+
  guides(colour = guide_legend(override.aes = list(size=2),nrow = 1))

ggsave("figures/segregation_scores.pdf",width = 10,height=6)


