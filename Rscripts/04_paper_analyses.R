library(tidyverse)
library(data.table)
library(patchwork)
fl <- list.files("processed_data/tracking/news_only/",pattern = "csv")
cutoffs <- c(3,10,30,60,120)
short_cases <- c("de","es","fr","it","uk","us")
long_cases <- c("Germany","Spain","France","Italy","United Kingdom","USA")
survey <- as.data.table(readRDS("data/survey_data_r.rds"))
fixN <- TRUE
##----------------------------------------------------------------------------##
# Overall news descriptives ----
##----------------------------------------------------------------------------##
if(!dir.exists("processed_data/tracking/figure1/")){
  dir.create("processed_data/tracking/figure1/")
}

for(i in seq_along(fl)){
  cat(countries[i],sep="\n")
  dt <- fread(paste0("processed_data/tracking/",fl[i]))
  tst1 <- dt[,.(news_vis=sum(type!="")),by=.(panelist_id)]
  val1 <- sum(tst1$news_vis!=0)/nrow(tst1)
  
  tst1 <- dt[,.(news_vis=sum(type!="" & political=="")),by=.(panelist_id)]
  val2 <- sum(tst1$news_vis!=0)/nrow(tst1)
  
  tst1 <- dt[,.(news_vis=sum(type!="" & political!="")),by=.(panelist_id)]
  val3 <- sum(tst1$news_vis!=0)/nrow(tst1)
  dat1 <- data.frame(type=c("news","non_news"),value=c(sum(dt[["type"]]!="")/nrow(dt),sum(dt[["type"]]=="")/nrow(dt)))
  
  dat2 <- data.frame(type=c("News in general","Non-political news","political news"),
                     value=c(val1,val2,val3)) 
  
  
  dat3 <- data.frame(
    type = c("political news","Non-political news"),
    value = c(sum(dt[["political"]]!="")/sum(dt[["type"]]!=""),1-sum(dt[["political"]]!="")/sum(dt[["type"]]!="")))
  
  dat3$ypos <- cumsum(dat3$value)- 0.5*dat3$value 
  
  saveRDS(list(dat1,dat2,dat3),paste0("processed_data/tracking/figure1/",short_cases[i],".RDS"))
}

vis_cnt_lst <- map(seq_along(fl),function(i){
  res_visitors <- res_visits <- data.table(cutoff=cutoffs,
                                           non_pol=numeric(length(cutoffs)),
                                           pol=numeric(length(cutoffs)))
  df <- fread(paste0("processed_data/tracking/news_only/",fl[i]))
  n1 <- length(unique(df[["panelist_id"]][df[["political"]]=="political"]))
  n2 <- length(unique(df[["panelist_id"]]))
  fracs <- sapply(cutoffs,function(x){
    tmp1 <- length(unique(df[duration>=x & political=="political"][["panelist_id"]]))/n1
    tmp2 <- length(unique(df[duration>=x][["panelist_id"]]))/n2
    c(tmp1,tmp2)
  })
  res_visitors$pol <-  fracs[1,]
  res_visitors$non_pol <-  fracs[2,] 
  
  totals <- sapply(cutoffs,function(x){
    df[duration>=x,.(count=.N),by=.(political)][["count"]]
  })
  #wtf is wrong with the UK???
  # if(i==5){
  #   totals[1:2,1:2] <- totals[2:1,2:1]
  # }
  
  res_visits$non_pol <- totals[1,]/colSums(totals)
  res_visits$pol <- totals[2,]/colSums(totals)
  list(visits = res_visits,visitors = res_visitors)
})
saveRDS(vis_cnt_lst,"processed_data/tracking/figure1/vis_counts.RDS")

# Figure1 for paper ----
fl1 <- paste0("processed_data/tracking/figure1/",short_cases,".RDS")

plt_tbl1 <- map_dfr(seq_along(fl1),function(x){
  lst <- readRDS(fl1[x])
  lst[[2]]$panel <- "News domain visitors"
  lst[[3]]$panel <- "Visits of news articles"
  lst[[2]]$case <- long_cases[x]
  lst[[3]]$case <- long_cases[x]
  lst[[2]]$type <- str_to_lower(lst[[2]]$type)
  lst[[3]]$type <- str_to_lower(lst[[3]]$type)
  bind_rows(lst[[2]][-1,],lst[[3]][,-3])
})

vis_cnt_lst <- readRDS("processed_data/tracking/figure1/vis_counts.RDS")
plt_tbl2 <- map_dfr(seq_along(vis_cnt_lst),function(x){
  vis_cnt_lst[[x]][[1]]$case <- vis_cnt_lst[[x]][[2]]$case <- long_cases[x]
  vis_cnt_lst[[x]][[1]]$level <- "Proportion of news article visits"
  vis_cnt_lst[[x]][[2]]$level <- "Proportion of news domain visitors"
  bind_rows(vis_cnt_lst[[x]])
}) |> 
  pivot_longer(cols = c(non_pol,pol)) |> 
  mutate(name = ifelse(name=="pol","political news","non-political news"))

# TODO: fix UK
plt_tbl2$value[plt_tbl2$case=="United Kingdom" & 
               plt_tbl2$cutoff==3 & 
               plt_tbl2$level=="Proportion of news article visits"] <- 
  rev(plt_tbl2$value[plt_tbl2$case=="United Kingdom" & 
                       plt_tbl2$cutoff==3 & 
                       plt_tbl2$level=="Proportion of news article visits"])

p1 <- ggplot(plt_tbl1,aes(x=type,y=value,fill = type))+
  geom_col()+
  scale_fill_manual(values=c("political news" = "#AA8939","non-political news" = "#303C74"),
                    labels=c("political news","non-political news"),name="")+
  facet_grid(panel~case)+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family="sans", size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        strip.text = element_text(face = "bold"),
        text = element_text(family="sans", size=16))+
  scale_y_continuous(labels = scales::label_percent())+
  labs(x="",y="")

p2 <- ggplot(plt_tbl2,aes(x=as.factor(cutoff),y=value,color=name)) + 
  geom_point(position = position_dodge(0.6))+
  geom_hline(yintercept = 0, linetype = "dashed",color="transparent")+
  scale_color_manual(values=c("political news" = "#AA8939","non-political news" = "#303C74"),
                     labels=c("political news","non-political news"),name="")+
  facet_grid(level~case,scales = "free_y") +
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family="sans", size = 20),
        axis.text.x = element_text(family="sans", size = 12,angle = 45,vjust=1),
        strip.text = element_text(face = "bold"),
        text = element_text(family="sans", size=16))+
  labs(x = "cutoff (in sec)",y = "")+
  scale_y_continuous(labels = scales::label_percent())
  # guides(fill = guide_legend(override.aes = list(size=3)))

p <- p1 + p2

ggsave("figures/figure1.pdf",width=18,height=8)
 
##----------------------------------------------------------------------------##
# Comparison pol/nonpol ----
##----------------------------------------------------------------------------##
combine_results <- function(non_pol,pol){
  do.call("rbind",non_pol) |> 
    as_tibble() |> 
    mutate(cutoff=cutoffs,type="non_political") |> 
    bind_rows(
      do.call("rbind",pol) |> 
        as_tibble() |> 
        mutate(cutoff=cutoffs,type="political")
    ) |> 
    pivot_longer(all_of(fl)) |> 
    pivot_wider(names_from = type) |> 
    mutate(case=str_extract(name,"^[a-z]{2}"))|> 
    mutate(case=long_cases[match(case,short_cases)]) |>
    select(-name)
}

# Segregation score ----
# Operational decisions:
#   - based on unique visitors
#   - recode the ideology variable lefright [1-11] -> left: [1-6), center: 6, right: (6,11]
#   - delete cases with missing data from the survey data set (The deletion intends to make the samples consistent across the different types of analyses (including the regression analyses)

## isolation index -----

non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
    if(fixN){
      peeps <- dt[duration>=120]
      peeps <- unique(peeps[["panelist_id"]])
      dt <- dt[panelist_id%in%peeps]
    }
    dt <- dt[political=="" & duration>=y]
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(dem=fifelse(leftright<6,1,0),rep=fifelse(leftright>6,1,0),cen=fifelse(leftright==6,1,0))]
    dt1 <- unique(dt[,.(domain,panelist_id,dem,rep,cen)])
    
    dt2 <- dt1[,.(demvisits=sum(dem,na.rm = TRUE),repvisits=sum(rep,na.rm = TRUE)),by=.(domain)]
    dt2[,score:=(repvisits/sum(repvisits)-demvisits/sum(demvisits)) * repvisits/(repvisits+demvisits)]
    sum(dt2$score,na.rm = TRUE)
  })
})

pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
    if(fixN){
      peeps <- dt[duration>=120]
      peeps <- unique(peeps[["panelist_id"]])
      dt <- dt[panelist_id%in%peeps]
    }
    dt <- dt[political=="political" & duration>=y]
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(dem=fifelse(leftright<6,1,0),rep=fifelse(leftright>6,1,0),cen=fifelse(leftright==6,1,0))]
    dt1 <- unique(dt[,.(domain,panelist_id,dem,rep,cen)])
    
    dt2 <- dt1[,.(demvisits=sum(dem,na.rm = TRUE),repvisits=sum(rep,na.rm = TRUE)),by=.(domain)]
    dt2[,score:=(repvisits/sum(repvisits)-demvisits/sum(demvisits)) * repvisits/(repvisits+demvisits)]
    sum(dt2$score,na.rm = TRUE)
  })
})

combine_results(non_pol,pol) |> saveRDS("processed_data/segregation_scores.RDS")

## dissimilarity index -----
non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
    if(fixN){
      peeps <- dt[duration>=120]
      peeps <- unique(peeps[["panelist_id"]])
      dt <- dt[panelist_id%in%peeps]
    }
    dt <- dt[political=="" & duration>=y]
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(dem=fifelse(leftright<6,1,0),rep=fifelse(leftright>6,1,0),cen=fifelse(leftright==6,1,0))]
    dt1 <- unique(dt[,.(domain,panelist_id,dem,rep,cen)])
    
    dt2 <- dt1[,.(demvisits=sum(dem,na.rm = TRUE),repvisits=sum(rep,na.rm = TRUE)),by=.(domain)]
    dt2[,score:=abs(repvisits/sum(repvisits)-demvisits/sum(demvisits))]
    0.5 * sum(dt2$score,na.rm = TRUE)
  })
})

pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
    if(fixN){
      peeps <- dt[duration>=120]
      peeps <- unique(peeps[["panelist_id"]])
      dt <- dt[panelist_id%in%peeps]
    }
    dt <- dt[political=="political" & duration>=y]
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(dem=fifelse(leftright<6,1,0),rep=fifelse(leftright>6,1,0),cen=fifelse(leftright==6,1,0))]
    dt1 <- unique(dt[,.(domain,panelist_id,dem,rep,cen)])
    
    dt2 <- dt1[,.(demvisits=sum(dem,na.rm = TRUE),repvisits=sum(rep,na.rm = TRUE)),by=.(domain)]
    dt2[,score:=abs(repvisits/sum(repvisits)-demvisits/sum(demvisits))]
    0.5 * sum(dt2$score,na.rm = TRUE)
  })
})

combine_results(non_pol,pol) |> saveRDS("processed_data/dissimilarity_scores.RDS")


## Atkinson index -----
non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
    if(fixN){
      peeps <- dt[duration>=120]
      peeps <- unique(peeps[["panelist_id"]])
      dt <- dt[panelist_id%in%peeps]
    }
    dt <- dt[political=="" & duration>=y]
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(dem=fifelse(leftright<6,1,0),rep=fifelse(leftright>6,1,0),cen=fifelse(leftright==6,1,0))]
    dt1 <- unique(dt[,.(domain,panelist_id,dem,rep,cen)])
    
    dt2 <- dt1[,.(demvisits=sum(dem,na.rm = TRUE),repvisits=sum(rep,na.rm = TRUE)),by=.(domain)]
    dt2[,score:=(repvisits/sum(repvisits))^(1/2)*(demvisits/sum(demvisits))^(1/2)]
    1 - sum(dt2$score,na.rm = TRUE)
  })
})

pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
    if(fixN){
      peeps <- dt[duration>=120]
      peeps <- unique(peeps[["panelist_id"]])
      dt <- dt[panelist_id%in%peeps]
    }
    dt <- dt[political=="political" & duration>=y]
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(dem=fifelse(leftright<6,1,0),rep=fifelse(leftright>6,1,0),cen=fifelse(leftright==6,1,0))]
    dt1 <- unique(dt[,.(domain,panelist_id,dem,rep,cen)])
    
    dt2 <- dt1[,.(demvisits=sum(dem,na.rm = TRUE),repvisits=sum(rep,na.rm = TRUE)),by=.(domain)]
    dt2[,score:=(repvisits/sum(repvisits))^(1/2)*(demvisits/sum(demvisits))^(1/2)]
    1 - sum(dt2$score,na.rm = TRUE)
  })
})

combine_results(non_pol,pol) |> saveRDS("processed_data/atkinson_scores.RDS")

# Partisan slant/news diets ----
# The ideology variable is centered around the respective country mean
# The centering takes int account that the news audience in some countries as a whole more strongly lean to 
# the left or right than in others
# Technically, the centering makes the analyses more compatible with the regression 
# analyses and the estimates more stable
#
# Following Flaxman et al., we use the standard deviation of news diets (consequences are neglible,
# simply produces a little higher values by giving more extreme news diets a little more weight from the outset)
non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
    if(fixN){
      peeps <- dt[duration>=120]
      peeps <- unique(peeps[["panelist_id"]])
      dt <- dt[panelist_id%in%peeps]
    }
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(leftright=fcase(leftright < 6,-1,leftright > 6,1,default = 0))]
    mean_ideo <- mean(unique(dt[,.(panelist_id,leftright)])[["leftright"]])
    
    dt <- dt[political=="" & duration>=y]
    
    # calculate the ideological slant of the individual participants news diets
    dom_align <- dt[,.(align=mean(leftright,na.rm=TRUE) - mean_ideo),by = .(domain)]
    dt[dom_align, on = .(domain), dom_align:= align]
    dt1 <- dt[,.(diet_slant=mean(dom_align,na.rm = TRUE)), by = .(panelist_id)]
    sd(dt1[["diet_slant"]]) #TODO: mean or SD?
  })
})

pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
    if(fixN){
      peeps <- dt[duration>=120]
      peeps <- unique(peeps[["panelist_id"]])
      dt <- dt[panelist_id%in%peeps]
    }
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(leftright=fcase(leftright < 6,-1,leftright > 6,1,default = 0))]
    mean_ideo <- mean(unique(dt[,.(panelist_id,leftright)])[["leftright"]])
    
    dt <- dt[political=="political" & duration>=y]
    
    # calculate the ideological slant of the individual participants news diets
    dom_align <- dt[,.(align=mean(leftright,na.rm=TRUE) - mean_ideo),by = .(domain)]
    dt[dom_align, on = .(domain), dom_align:= align]
    dt1 <- dt[,.(diet_slant=mean(dom_align,na.rm = TRUE)), by = .(panelist_id)]
    sd(dt1[["diet_slant"]]) #TODO: mean or SD?
  })
})

combine_results(non_pol,pol) |> saveRDS("processed_data/news_diet_slant.RDS")

# Simpson Diversity ----
non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
    if(fixN){
      peeps <- dt[duration>=120]
      peeps <- unique(peeps[["panelist_id"]])
      dt <- dt[panelist_id%in%peeps]
    }
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(leftright=fcase(leftright < 6,-1,leftright > 6,1,default = 0))]
    mean_ideo <- mean(unique(dt[,.(panelist_id,leftright)])[["leftright"]])
    
    dt <- dt[political=="" & duration>=y]
    
    dom_align <- dt[,.(align=mean(leftright,na.rm=TRUE) - mean_ideo),by = .(domain)]
    dom_align[, ideo_cat := fcase(align < -.2,-1, align > .2,1, default = 0)]
    dt[dom_align, on = .(domain), ideo_cat:= ideo_cat]
    dt1 <- dt[,.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat)]
    dt2 <- dt1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
    1.5 * mean(dt2[["ideo_div"]])
  })
})

pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/tracking/news_only/",x))
    if(fixN){
      peeps <- dt[duration>=120]
      peeps <- unique(peeps[["panelist_id"]])
      dt <- dt[panelist_id%in%peeps]
    }
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(leftright=fcase(leftright < 6,-1,leftright > 6,1,default = 0))]
    mean_ideo <- mean(unique(dt[,.(panelist_id,leftright)])[["leftright"]])
    
    dt <- dt[political=="political" & duration>=y]
    
    dom_align <- dt[,.(align=mean(leftright,na.rm=TRUE) - mean_ideo),by = .(domain)]
    dom_align[, ideo_cat := fcase(align < -.2,-1, align > .2,1, default = 0)]
    dt[dom_align, on = .(domain), ideo_cat:= ideo_cat]
    dt1 <- dt[,.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat)]
    dt2 <- dt1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
    1.5 * mean(dt2[["ideo_div"]])
  })
})

combine_results(non_pol,pol) |> saveRDS("processed_data/simpson_diversity.RDS")

# Figure 2 for Paper----
result_files <- c("segregation_scores.RDS","simpson_diversity.RDS","news_diet_slant.RDS")
types <- c("(A) Ideological segregation","(B) News Diet Diversity","(C) Partisanship in News Diets")

res_tbl <- map_dfr(seq_along(result_files),function(x){
  readRDS(paste0("processed_data/",result_files[x])) |> mutate(type=types[x]) |> 
  pivot_longer(cols=c(non_political,political),names_to = "news_type",values_to = "score")
})


ggplot(res_tbl,aes(x=as.factor(cutoff),y=score,color = news_type)) +
  geom_point(position = position_dodge(0.6))+
  geom_hline(yintercept = 0, linetype = "dashed",color="transparent")+
  scale_color_manual(values=c("political" = "#AA8939","non_political" = "#303C74"),
                     labels=c("political news","non-political news"),name="")+
  facet_grid(type~case,scales = "free_y") +
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family="sans", size = 20),
        axis.text.x = element_text(family="sans", size = 12,angle = 45,vjust=1),
        strip.text = element_text(face = "bold"),
        text = element_text(family="sans", size=16))+
  labs(x = "cutoff (in sec)",y = "score")+
  guides(colour = guide_legend(override.aes = list(size=3)))

ggsave("figures/figure2.pdf",width=16,height=10)
