# packages ----
library(tidyverse)
library(data.table)
library(patchwork)
library(lme4)

# one of "desktop", "mobile" or "both"
# main paper is "both", appendix is "desktop"
platform <- "both"

fl <- list.files(paste0("processed_data/",platform,"/news_only"),pattern = "csv")
cutoffs <- c(3,10,30,60,120)
short_cases <- c("de","es","fr","it","uk","us")
long_cases <- c("Germany","Spain","France","Italy","United Kingdom","USA")
survey <- as.data.table(readRDS("data/survey_data_r.rds"))
fixN <- TRUE

# some helper function to get the regression estimates into the graphs
lmer_to_tidy <- function(res) {
  res <- summary(res) |> coef() |> as_tibble(rownames = "term")
  res$CI_lower <- res$Estimate - 1.96 * res$`Std. Error`
  res$CI_upper <- res$Estimate + 1.96 * res$`Std. Error`
  return(res)
}

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

##----------------------------------------------------------------------------##
# Overall news descriptives ----
##----------------------------------------------------------------------------##
if(!dir.exists("processed_data/stats/")){
  dir.create("processed_data/stats/")
}

vis_cnt_lst <- map(seq_along(fl),function(i){
  res_visitors <- data.table(cutoff=cutoffs,
                             non_pol=numeric(length(cutoffs)),
                             pol=numeric(length(cutoffs)))
  df <- fread(paste0("processed_data/",platform,"/news_only/",fl[i]))
  n <- length(unique(df[duration>= 3][["panelist_id"]]))
  # n1 <- length(unique(df[["panelist_id"]][df[["political"]]=="political"]))
  fracs <- sapply(cutoffs,function(x){
    tmp1 <- length(unique(df[duration>=x & political=="political"][["panelist_id"]]))/n
    tmp2 <- length(unique(df[duration>=x][["panelist_id"]]))/n
    c(tmp1,tmp2)
  })
  res_visitors$pol <-  fracs[1,]
  res_visitors$non_pol <-  fracs[2,] 
  
  res_visitors
})
saveRDS(vis_cnt_lst,paste0("processed_data/stats/",platform,"_vis_counts.RDS"))

vis_cnt_lst <- readRDS(paste0("processed_data/stats/",platform,"_vis_counts.RDS"))
plt_tbl <- map_dfr(seq_along(vis_cnt_lst),function(x){
  vis_cnt_lst[[x]]$case <- long_cases[x]
  # vis_cnt_lst[[x]]$level <- "Proportion of news article visits"
  vis_cnt_lst[[x]]$level <- "Proportion of news domain visitors"
  bind_rows(vis_cnt_lst[[x]])
}) |> 
  pivot_longer(cols = c(non_pol,pol)) |> 
  mutate(name = ifelse(name=="pol","political news","non-political news"))

p <- ggplot(plt_tbl,aes(x=as.factor(cutoff),y=value,color=name)) + 
  geom_point(position = position_dodge(0.6))+
  geom_hline(yintercept = 0, linetype = "dashed",color="transparent")+
  scale_color_manual(values=c("political news" = "#AA8939","non-political news" = "#303C74"),
                     labels=c("political news","non-political news"),name="")+
  facet_grid(.~case,scales = "free_y") +
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family="sans", size = 20),
        axis.text.x = element_text(family="sans", size = 12),
        strip.text = element_text(face = "bold"),
        text = element_text(family="sans", size=16))+
  labs(x = "cutoff (in sec)",y = "")+
  scale_y_continuous(labels = scales::label_percent())
  # guides(fill = guide_legend(override.aes = list(size=3)))

ggsave(paste0("figures/",platform,"_figure1.pdf"),p,width=10,height=4)
 
##----------------------------------------------------------------------------##
# Comparison pol/nonpol ----
##----------------------------------------------------------------------------##
# Segregation score ----
# Operational decisions:
#   - based on unique visitors
#   - recode the ideology variable lefright [1-11] -> left: [1-6), center: 6, right: (6,11]
#   - delete cases with missing data from the survey data set (The deletion intends to make the samples consistent across the different types of analyses (including the regression analyses)

## isolation index -----

non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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

combine_results(non_pol,pol) |> 
  saveRDS(paste0("processed_data/",platform,"_segregation_scores.RDS"))

## dissimilarity index -----
non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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

combine_results(non_pol,pol) |> 
  saveRDS(paste0("processed_data/stats/",platform,"_dissimilarity_scores.RDS"))


## Atkinson index -----
non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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

combine_results(non_pol,pol) |> 
  saveRDS(paste0("processed_data/stats/",platform,"_atkinson_scores.RDS"))

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
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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

combine_results(non_pol,pol) |> 
  saveRDS(paste0("processed_data/stats/",platform,"_news_diet_slant.RDS"))

# Diversity measures ----
## Simpson Diversity ----
non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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

combine_results(non_pol,pol) |> 
  saveRDS(paste0("processed_data/stats/",platform,"_simpson_diversity.RDS"))

## Shannon ----
non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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
    dt[dom_align, on = .(domain), lcr:= ideo_cat]
    mean(dt[,.N,.(panelist_id,lcr)][,.(frac=N/sum(N)),.(panelist_id)][,.(Diversity=-sum(frac*log(frac))),.(panelist_id)][["Diversity"]]) 
  })
})

pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
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
    dt[dom_align, on = .(domain), lcr:= ideo_cat]
    mean(dt[,.N,.(panelist_id,lcr)][,.(frac=N/sum(N)),.(panelist_id)][,.(Diversity=-sum(frac*log(frac))),.(panelist_id)][["Diversity"]]) 
  })
})

combine_results(non_pol,pol) |> 
  saveRDS(paste0("processed_data/stats/",platform,"_entropy.RDS"))


# Figure 2 for Paper----
result_files <- paste0(platform,c("_segregation_scores.RDS",
                                  "_simpson_diversity.RDS",
                                  "_news_diet_slant.RDS"))
types <- c("(A) Ideological Segregation","(B) News Diet Diversity","(C) Partisanship in News Diets")

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

ggsave(paste0("figures/",platform,"_figure2.pdf"),width=16,height=10)


# Prepare Regression Data ----
# prepare Data
survey_lm <- survey[,.(panelist_id, leftright,  polinterest, extremism, age, female, edu, log_total_visits)]  
survey_lm <- na.omit(survey_lm)
survey_lm[,extreme:=(abs(leftright-6)/5)]
survey_lm[, leftright := fcase(leftright < 6,-1,leftright == 6, 0, leftright > 6, 1)]
survey_lm[, polinterest := polinterest * -1]
survey_lm[, age := (age-30)/35]

lm_dt <- lapply(seq_along(fl),function(i){
  dt <- fread(paste0("processed_data/",platform,"/news_only/",fl[i]))
  dt[ ,country := long_cases[i]]
  dt[, prev_type:=fcase(prev_type == "direct","direct",
                        prev_type == "ebay","direct",
                        prev_type == "news","direct",
                        prev_type == "other","direct",
                        prev_type == "facebook","facebook",
                        prev_type == "twitter","twitter",
                        prev_type == "search","search",
                        prev_type == "portal","portal")]
  dt[,prev_type:=as.factor(prev_type)]
  dt[,political:=ifelse(political=="","non-political",political)]
}) |> rbindlist()

lm_dt <- lm_dt[!is.na(duration)]
lm_dt <- lm_dt[survey_lm, on = .(panelist_id)]
keep <- lm_dt[,.(max_visit=max(duration,na.rm=TRUE)), by=.(panelist_id)][max_visit>=120][["panelist_id"]]
lm_dt <- lm_dt[panelist_id%in%keep]
lm_dt[,ideo_over:=mean(leftright,na.rm=TRUE),by=country]

# In the following, we conduct the substantive regression analyses. 
# We need to run one regression set per moderator. 
# Although We do not plot the respective coefficients, We additionally control for logged(total visits), as
# is common with most previous research 

# Regressions I-----
## Countries (Moderation Analysis) ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + 
         extreme+ country:as.factor(leftright) + country + prev_type + 
         polinterest + age + female + as.factor(edu) + log_total_visits + (1|panelist_id),data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + 
                extreme+ country:as.factor(leftright) + country + prev_type + 
                polinterest + age + female + as.factor(edu) + log_total_visits + (1|panelist_id),data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_country_inter <- bind_rows(non_pol,pol)

# extract, recode, and save interaction terms for later graphing
to_keep <- paste0("as.factor(leftright)1:country",long_cases)

tidy_toplot_country_inter <- tidy_toplot_country_inter |> 
  dplyr::filter(term%in%to_keep) |> 
  mutate(header = "(A) Country \n (Reference: France)",
         term = str_replace(term,"as.factor\\(leftright\\)1:country","\\(A\\) Country: "))

## News Access (Moderation Analysis) ----

non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + 
                extreme + prev_type:as.factor(leftright) + country + prev_type + 
                as.factor(polinterest) + age + female + as.factor(edu) + 
                log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + 
                 extreme + prev_type:as.factor(leftright) + country + prev_type + 
                 as.factor(polinterest) + age + female + as.factor(edu) + 
                 log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_access_inter <- bind_rows(non_pol,pol)

to_keep <- paste0("as.factor(leftright)1:prev_type",c("facebook","twitter","search","portal"))

tidy_toplot_access_inter <- tidy_toplot_access_inter |> 
  dplyr::filter(term%in%to_keep) |> 
  mutate(header = "(B) Access \n (Reference: Direct)",
         term = str_replace(term,"as.factor\\(leftright\\)1:prev_type","\\(B\\) Access: ") |> str_to_title())

## Political Interest (Moderation Analysis) ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                as.factor(polinterest):as.factor(leftright) + country + prev_type + 
                as.factor(polinterest) + age + female + as.factor(edu) + 
                log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                as.factor(polinterest):as.factor(leftright) + country + prev_type + 
                as.factor(polinterest) + age + female + as.factor(edu) + 
                log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_interest_inter <- bind_rows(non_pol,pol)

to_keep <- "as.factor(leftright)1:as.factor(polinterest)-1"

tidy_toplot_interest_inter <- tidy_toplot_interest_inter |> 
  dplyr::filter(term%in%to_keep) |> 
  mutate(header = "(C) Political Interest \n(Reference: Low Political Interest)",
         term = str_replace(term,"as.factor\\(leftright\\)1:as.factor\\(polinterest\\)-1",
                            "\\(C\\) Political Interest: High"))

# Change order 
# TODO: check that this really switches ref point from high to low
tidy_toplot_interest_inter <- tidy_toplot_interest_inter |> 
  mutate(Estimate = -1 * Estimate,CI_lower = -1 * CI_lower, CI_upper = -1 * CI_upper)

## Extremity (Moderation Analysis) ----
# (This part of the analyses produces error messages; these can be ignored. They simply reflect that 
# very low extremity is identical with centrist ideology; the procedure automatically drop the corresponding interaction terms from the analysis)

non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + extreme + extreme:as.factor(leftright) +
              country + prev_type + as.factor(polinterest) + age + female + 
              as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + extreme + extreme:as.factor(leftright) +
                country + prev_type + as.factor(polinterest) + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_extremity_inter <- bind_rows(non_pol, pol)

to_keep <- c("as.factor(leftright)1:extreme")

tidy_toplot_extremity_inter <- tidy_toplot_extremity_inter |> 
  dplyr::filter(term%in%to_keep) |> 
  mutate(header = "(D) Extremity \n(Reference: Low Extremity)",
         term = str_replace(term,"as.factor\\(leftright\\)1:extreme",
                            "\\(D\\) Extremity: High"))

## Generation (Moderation Analysis) ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + extreme + age:as.factor(leftright) + 
                country + prev_type + as.factor(polinterest) + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- res <- lmer(align ~ 1  + as.factor(leftright) + extreme + age:as.factor(leftright) + 
                       country + prev_type + as.factor(polinterest) + age + female + 
                       as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_generation_inter <- bind_rows(non_pol, pol)

to_keep <- c("as.factor(leftright)1:age")

tidy_toplot_generation_inter <- tidy_toplot_generation_inter |> 
  dplyr::filter(term%in%to_keep) |> 
  mutate(header = "(E) Generation \n(Reference: Millenial)",
         term = str_replace(term,"as.factor\\(leftright\\)1:age",
                            "\\(E\\) Generation: Boomer"))

## Gender (Moderation Analysis) ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + extreme + female:as.factor(leftright) + 
                country + prev_type + as.factor(polinterest) + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- res <- lmer(align ~ 1  + as.factor(leftright) + extreme + female:as.factor(leftright) + 
                       country + prev_type + as.factor(polinterest) + age + female + 
                       as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_gender_inter <- bind_rows(non_pol, pol)

to_keep <- c("as.factor(leftright)1:female")

tidy_toplot_gender_inter <- tidy_toplot_gender_inter |> 
  dplyr::filter(term%in%to_keep) |> 
  mutate(header = "(F) Gender \n(Reference: Female)",
         term = str_replace(term,"as.factor\\(leftright\\)1:female",
                            "\\(F\\) Gender: Male"))

# Change order 
# TODO: check that this really switches ref point from male to female
tidy_toplot_gender_inter <- tidy_toplot_gender_inter |> 
  mutate(Estimate = -1 * Estimate,CI_lower = -1 * CI_lower, CI_upper = -1 * CI_upper)

## Education (Moderation Analysis) ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + extreme + as.factor(edu):as.factor(leftright) + 
                country + prev_type + as.factor(polinterest) + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  res <- res <- lmer(align ~ 1  + as.factor(leftright) + extreme + as.factor(edu):as.factor(leftright) + 
                       country + prev_type + as.factor(polinterest) + age + female + 
                       as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_education_inter <- bind_rows(non_pol, pol)

to_keep <- c("as.factor(leftright)1:as.factor(edu)3")

tidy_toplot_education_inter <- tidy_toplot_education_inter |> 
  dplyr::filter(term%in%to_keep) |> 
  mutate(header = "(G) Education \n(Reference: Low Education)",
         term = str_replace(term,"as.factor\\(leftright\\)1:as.factor\\(edu\\)3",
                            "\\(G\\) Education: High"))

tidy_toplot_integrated <- bind_rows(tidy_toplot_country_inter, tidy_toplot_access_inter, tidy_toplot_interest_inter, tidy_toplot_extremity_inter,
                                tidy_toplot_generation_inter, tidy_toplot_gender_inter, tidy_toplot_education_inter)

if(!dir.exists("processed_data/regression")){
  dir.create("processed_data/regression")
}
write_csv(tidy_toplot_integrated, paste0("processed_data/regression/",platform,"_interaction_terms.csv"))
## Plotting ----
tidy_toplot_integrated <- read_csv(paste0("processed_data/regression/",platform,"_interaction_terms.csv"))
tidy_toplot_integrated <- tidy_toplot_integrated |> mutate(header = as.factor(header))
level_order <- rev(levels(tidy_toplot_integrated$header))

tidy_toplot_integrated <- tidy_toplot_integrated |> 
  mutate(type = as.factor(type),
         type = dplyr::recode_factor(type,
                                     "non_political" = "Non-Political News",
                                     "political" = "Political News"))

tidy_toplot_integrated |> 
  mutate(term=str_replace_all(term,"\\(B\\) Access","\\(C\\) News Access")) |> 
  mutate(term=str_replace_all(term,"\\(C\\) Political Interest","\\(B\\) Political Interest")) |> 
  mutate(term=str_replace_all(term,"Direct","Non-referred")) |> 
  mutate(header=str_replace_all(header,"\\(B\\) Access","\\(C\\) News Access")) |> 
  mutate(header=str_replace_all(header,"Reference: Direct","Reference: Non-referred")) |> 
  mutate(header=str_replace_all(header,"\\(C\\) Political Interest","\\(B\\) Political Interest")) |>   ggplot(aes(y = Estimate, x = factor(threshold))) +
  geom_pointrange(
    aes(ymin = CI_lower, ymax = CI_upper, color = term,shape=term), size=0.32,
    position = position_dodge2(w = 0.5)) +
  coord_flip() +
  theme_bw() + 
  scale_color_manual(values = c(
    "#E69F00", "#009E73", "#0072B2", "#D55E00", "grey25",
    "#E69F00",
    "#E69F00", "#009E73", "#0072B2", "#D55E00",
    "#009E73", "#0072B2", "#D55E00", "grey25"
  ),name = "")+
  scale_shape_manual(values = c(
    15,15,15,15,15,
    17,
    16,16,16,16,
    17,17,17,17
  ), name = "")+
  facet_grid(type~header, scales = "free_x") +
  theme(axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size=14)) +
  labs(title="",
       x = "cutoff (in sec)") +
  geom_hline(yintercept = 0, linetype = "dashed")+
  guides(color = guide_legend(override.aes = list(size=0.75)))

ggsave(paste0("figures/",platform,"_regression_interaction.pdf"), width = 15, height = 7)

# Regressions II-----
## Country (Conditional Effects) ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(long_cases,function(ref){
    dat[country==ref][["country"]] <- "A_reference"
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                country:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(long_cases,function(ref){
    dat[country==ref][["country"]] <- "A_reference"
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  country:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

tidy_toplot_country <- bind_rows(non_pol,pol) %>% 
  mutate(header = "(A) Country",
         level = as.factor(level),
         level = dplyr::recode_factor(level,
                                      "France" = "(A) Country: France",
                                      "Germany" = "(A) Country: Germany",
                                      "Italy" = "(A) Country: Italy",
                                      "Spain" = "(A) Country: Spain",
                                      "United Kingdom" = "(A) Country: United Kingdom",
                                      "USA" = "(A) Country: USA"))


## Access (Conditional Effects Analysis)----
reference <- c("direct", "facebook", "twitter", "search", "portal") 
lm_dt$prev_type <- as.character(lm_dt$prev_type)

non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[prev_type==ref][["prev_type"]] <- "A_reference"
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  prev_type:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "non_political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[prev_type==ref][["prev_type"]] <- "A_reference"
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  prev_type:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

tidy_toplot_access <- bind_rows(non_pol,pol) %>% 
  mutate(header = "(B) Access",
         level = as.factor(level),
         level = dplyr::recode_factor(level,
                                      "direct" = "(B) Access: Direct",
                                      "facebook" = "(B) Access: Facebook",
                                      "twitter" = "(B) Access: Twitter",
                                      "search" = "(B) Access: Search engines",
                                      "portal" = "(B) Access: Portals"))

## Political interest (Conditional Effects Analysis) ----
reference <- c("1", "4")
lm_dt$polinterest <- -1*lm_dt$polinterest

non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[polinterest==ref][["polinterest"]] <- 0
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  as.factor(polinterest):as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "non_political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[polinterest==ref][["polinterest"]] <- 0
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  as.factor(polinterest):as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

tidy_toplot_interest <- bind_rows(non_pol,pol) |> 
  mutate(header = "(C) Political Interest",
         level = as.factor(level),
         level = dplyr::recode_factor(level,
                                      "1" = "(C) Political Interest: Low",
                                      "4" = "(C) Political Interest: High"))

## Extremity(Conditional Effects Analysis) ----
# because We treat extremity as a continuous moderator, this requires a little bit different coding
# We use .2 as a reference level, because zero does not make so much sense (because it corresponds to a centrist ideology)
reference <- c(".2", "1") 

non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[["extreme"]] <- dat[["extreme"]] - as.numeric(ref)
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  extreme:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "non_political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[["extreme"]] <- dat[["extreme"]] - as.numeric(ref)
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  extreme:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})


tidy_toplot_extreme <- bind_rows(non_pol,pol) |> 
  mutate(header = "(D) Political Extremity",
         level = as.factor(level),
         level = dplyr::recode_factor(level,
                                      ".2" = "(D) Political Extremity: Low",
                                      "1" = "(D) Political Extremity: High"))

## Generation (Conditional Effects Analysis) ----
tidy_toplot <- data.frame()
reference <- c("30", "60") #TODO: 65?
lm_dt[, age := age*35+30] #undo recode from before

non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[["age"]] <- dat[["age"]] - as.numeric(ref)
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  age:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "non_political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[["age"]] <- dat[["age"]] - as.numeric(ref)
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  age:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

tidy_toplot_age <- bind_rows(non_pol,pol)|> 
  mutate(header = "(E) Generation",
         level = as.factor(level),
         level = dplyr::recode_factor(level,
                                      "30" = "(E) Generation: Millennial",
                                      "60" = "(E) Generation: Boomer"))

## Gender (Conditional Effects Analysis) ----
reference <- c("0", "1")

non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[["female"]] <- dat[["female"]] - as.numeric(ref)
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  female:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "non_political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[["female"]] <- dat[["female"]] - as.numeric(ref)
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  female:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})


tidy_toplot_female <- bind_rows(non_pol,pol) |> 
  mutate(header = "(F) Gender",
         level = as.factor(level),
         level = dplyr::recode_factor(level,
                                      "0" = "(F) Gender: Male",
                                      "1" = "(F) Gender: Female"))

## Education (Conditional Effects Analysis) ----
reference <- c("1", "3")

non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[edu==ref][["edu"]] <- 0
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  as.factor(edu):as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "non_political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-ideo_over,by=c("country","domain")]
  map_dfr(reference,function(ref){
    dat[edu==ref][["edu"]] <- 0
    res <- lmer(align ~ 1  + as.factor(leftright) + extreme + 
                  as.factor(edu):as.factor(leftright) + country + prev_type + as.factor(polinterest) + age + 
                  female + as.factor(edu) + log_total_visits + (1|panelist_id), data = dat) 
    lmer_to_tidy(res) |> 
      mutate(type = "political", threshold = x,level = ref) |> 
      dplyr::filter(term == "as.factor(leftright)1")
  })
})


tidy_toplot_edu <- bind_rows(non_pol,pol) |> 
  mutate(header = "(G) Education",
         level = as.factor(level),
         level = dplyr::recode_factor(level,
                                      "1" = "(G) Education: Low",
                                      "3" = "(G) Education: High"))

# Build single concise plot
tidy_toplot_integrated <- bind_rows(tidy_toplot_country, tidy_toplot_access, 
                                    tidy_toplot_interest, tidy_toplot_extreme,
                                    tidy_toplot_age, tidy_toplot_female, tidy_toplot_edu)

level_order <- rev(levels(tidy_toplot_integrated$header))
tidy_toplot_integrated <- tidy_toplot_integrated  %>% 
  mutate(type = as.factor(type),
         type = dplyr::recode_factor(type,
                                     "non_political" = "Non-Political News",
                                     "political" = "Political News"))

write_csv(tidy_toplot_integrated, paste0("processed_data/regression/",platform,"_conditional_effects.csv"))

## Plotting ----
tidy_toplot_integrated <- read_csv(paste0("processed_data/regression/",platform,"_conditional_effects.csv"))
tidy_toplot_integrated |> 
  mutate(level=str_replace_all(level,"\\(B\\) Access","\\(C\\) News Access")) |> 
  mutate(level=str_replace_all(level,"\\(C\\) Political Interest","\\(B\\) Political Interest")) |> 
  mutate(level=str_replace_all(level,"Direct","Non-referred")) |> 
  mutate(header=str_replace_all(header,"\\(B\\) Access","\\(C\\) News Access")) |> 
  mutate(header=str_replace_all(header,"\\(C\\) Political Interest","\\(B\\) Political Interest")) |> 
  dplyr::filter(str_detect(level,"\\(A\\)|\\(B\\)|\\(C\\)")) |> 
  ggplot(aes(y = Estimate, x = factor(threshold))) +
  geom_pointrange(
    aes(ymin = CI_lower, ymax = CI_upper, color = level,shape=level), size=0.32,
    position = position_dodge2(w = 0.4)) +
  coord_flip() +
  theme_bw() + 
  theme_bw() + 
  scale_color_manual(values = c(
    "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7","grey25",
    "#E69F00", "#009E73",
    "#E69F00", "#009E73", "#0072B2", "#D55E00","#CC79A7",
    "#E69F00", "#009E73","#E69F00", "#009E73",
    "#E69F00", "#009E73","#E69F00", "#009E73","#E69F00", "#009E73"
  ),name = "")+
  scale_shape_manual(values = c(
    15,15,15,15,15,
    16,16,16,16,
    17,17,7,7,8,8,9,9,10,10,11,11
  ), name = "")+
  facet_grid(type~header, scales = "free_x") +
  theme(axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  labs(y = "Ideological Selectivity",x="Threshold") +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave(paste0("figures/",platform,"_regression_conditional_reduced.pdf"), width = 10, height = 7)

tidy_toplot_integrated <- read_csv(paste0("processed_data/regression/",platform,"_conditional_effects.csv"))
tidy_toplot_integrated |> 
  mutate(level=str_replace_all(level,"\\(B\\) Access","\\(C\\) News Access")) |> 
  mutate(level=str_replace_all(level,"\\(C\\) Political Interest","\\(B\\) Political Interest")) |> 
  mutate(level=str_replace_all(level,"Direct","Non-referred")) |> 
  mutate(header=str_replace_all(header,"\\(B\\) Access","\\(C\\) News Access")) |> 
  mutate(header=str_replace_all(header,"\\(C\\) Political Interest","\\(B\\) Political Interest")) |> 
  # dplyr::filter(str_detect(level,"\\(A\\)|\\(B\\)|\\(C\\)")) |> 
  ggplot(aes(y = Estimate, x = factor(threshold))) +
  geom_pointrange(
    aes(ymin = CI_lower, ymax = CI_upper, color = level,shape=level), size=0.32,
    position = position_dodge2(w = 0.4)) +
  coord_flip() +
  theme_bw() + 
  theme_bw() + 
  scale_color_manual(values = c(
    "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7","grey25",
    "#E69F00", "#009E73",
    "#E69F00", "#009E73", "#0072B2", "#D55E00","#CC79A7",
    "#E69F00", "#009E73","#E69F00", "#009E73",
    "#E69F00", "#009E73","#E69F00", "#009E73","#E69F00", "#009E73"
  ),name = "")+
  scale_shape_manual(values = c(
    15,15,15,15,15,
    16,16,16,16,
    17,17,7,7,8,8,9,9,10,10,11,11
  ), name = "")+
  facet_grid(type~header, scales = "free_x") +
  theme(axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  labs(y = "Ideological Selectivity",x="Threshold") +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave(paste0("figures/",platform,"_regression_conditional.pdf"), width = 10, height = 7)


# Appendix ----

## Seg score comparison ----
bind_rows(
  readRDS(paste0("processed_data/stats/",platform,"_segregation_scores.RDS")) |> 
    mutate(type="Isolation index"),
  readRDS(paste0("processed_data/stats/",platform,"_dissimilarity_scores.RDS")) |> 
    mutate(type="Dissimilarity index"),
  readRDS(paste0("processed_data/stats/",platform,"_atkinson_scores.RDS")) |> 
    mutate(type="Atkinson scores")
) |> 
  pivot_longer(cols = c(non_political,political)) |> 
  mutate(name = ifelse(name=="political","political news","non-political news")) |> 
  ggplot(aes(x=factor(cutoff),y=value,col=name))+
    geom_point()+
    geom_hline(yintercept = 0, linetype = "dashed",color="transparent")+
    scale_color_manual(values=c("political news" = "#AA8939",
                                "non-political news" = "#303C74"),
                       labels=c("political news","non-political news"),name="")+
    facet_grid(type~case,scales = "free_y") +
    theme_bw()+
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          legend.text = element_text(family="sans", size = 20),
          axis.text.x = element_text(family="sans", size = 12),
          strip.text = element_text(face = "bold"),
          text = element_text(family="sans", size=16))+
    labs(x = "cutoff (in sec)",y = "")

ggsave(paste0("figures/",platform,"_seg_score_comparison.pdf"),width=16,height=10)

## diversity comparison ----  
bind_rows(
  readRDS(paste0("processed_data/stats/",platform,"_simpson_diversity.RDS")) |> 
            mutate(type="Simpson's D"),
  readRDS(paste0("processed_data/stats/",platform,"_entropy.RDS")) |> 
            mutate(type="Shannon's H")
) |> 
  pivot_longer(cols = c(non_political,political)) |> 
  mutate(name = ifelse(name=="political","political news","non-political news")) |> 
  ggplot(aes(x=factor(cutoff),y=value,col=name))+
  geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed",color="transparent")+
  scale_color_manual(values=c("political news" = "#AA8939",
                              "non-political news" = "#303C74"),
                     labels=c("political news","non-political news"),name="")+
  facet_grid(type~case,scales = "free_y") +
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family="sans", size = 20),
        axis.text.x = element_text(family="sans", size = 12),
        strip.text = element_text(face = "bold"),
        text = element_text(family="sans", size=16))+
  labs(x = "cutoff (in sec)",y = "")

ggsave(paste0("figures/",platform,"_diversity_comparison.pdf"),width=16,height=10)

##news diet slant comparison ----
non_pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(leftright=fcase(leftright < 6,-1,leftright > 6,1,default = 0))]
    mean_ideo <- mean(unique(dt[,.(panelist_id,leftright)])[["leftright"]])
    
    dt <- dt[political=="" & duration>=y]
    
    dom_align <- dt[,.(align=mean(leftright,na.rm=TRUE)),by = .(domain)]
    dt[dom_align, on = .(domain), dom_align:= align]
    dt1 <- dt[,.(diet_slant=mean(dom_align,na.rm = TRUE)), by = .(panelist_id)]
    mean(abs(dt1[["diet_slant"]]-mean_ideo))
  })
})

pol <- lapply(cutoffs,function(y) {
  sapply(fl,function(x){
    dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
    dt[survey, on = .(panelist_id), leftright := leftright]
    dt <- dt[!is.na(leftright)]
    dt[,`:=`(leftright=fcase(leftright < 6,-1,leftright > 6,1,default = 0))]
    mean_ideo <- mean(unique(dt[,.(panelist_id,leftright)])[["leftright"]])
    
    dt <- dt[political=="political" & duration>=y]
    
    dom_align <- dt[,.(align=mean(leftright,na.rm=TRUE)),by = .(domain)]
    dt[dom_align, on = .(domain), dom_align:= align]
    dt1 <- dt[,.(diet_slant=mean(dom_align,na.rm = TRUE)), by = .(panelist_id)]
    mean(abs(dt1[["diet_slant"]]-mean_ideo))
  })
})

combine_results(non_pol,pol) |> 
  saveRDS(paste0("processed_data/stats/",platform,"_news_diet_slant_fletcher.RDS"))

flaxman  <- readRDS(paste0("processed_data/stats/",platform,"_news_diet_slant.RDS")) |> 
  mutate(meta="(A) Flaxman et al.")
fletcher <- readRDS(paste0("processed_data/stats/",platform,"_news_diet_slant_fletcher.RDS")) |> 
  mutate(meta="(B) Fletcher et al.")

bind_rows(flaxman,fletcher) |> 
  pivot_longer(non_political:political,names_to = "type",values_to = "score") |> 
  ggplot(aes(x=factor(cutoff),y=score,col=type))+
  geom_point()+
  scale_color_manual(values=c("political" = "#AA8939","non_political" = "#303C74"),
                     labels=c("Political news","Non-political news"),name="")+
  theme_bw() + 
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family="sans", size = 20),
        axis.text.x = element_text(family="sans", size = 12),
        strip.text = element_text(face = "bold"),
        text = element_text(family="sans", size=16))+
  labs(x = "cutoff (in sec)",y = "")+
  facet_grid(meta~case,scales = "free_y")

ggsave(paste0("figures/",platform,"_diet_slant_comparison.pdf"),width=15,height=8)

## alt scores regression ----
###prepare data ----
bakshy <- fread("data/bakshy_top500.txt")
bakshy[,domain := str_replace_all(domain,"www\\.","")]
bakshy[,domain := fifelse(domain=="news.yahoo.com","news.yahoo.com/NEWS",domain)]
bakshy[,domain := fifelse(domain=="news.msn.com", "msn.com/NEWS",domain)]
bakshy[,domain := fifelse(domain=="aol.com", "aol.com/NEWS",domain)]
bakshy[,domain := fifelse(domain=="huffingtonpost.com", "huffpost.com",domain)]
bakshy[,domain := fifelse(domain=="westernjournalism.com", "westernjournal.com",domain)]
bakshy <- bakshy[!domain%in%c("msn.com", "twitter.com", "amazon.com", "youtube.com")]
bakshy <- bakshy[,c("domain","avg_align")]
robertson_data <- fread("data/bias_scores.csv")

survey_lm <- survey[country=="USA",.(panelist_id, leftright,  polinterest, extremism, age, female, edu, log_total_visits)]  
survey_lm <- na.omit(survey_lm)
survey_lm[, leftright := fcase(leftright < 6,-1,leftright == 6, 0, leftright > 6, 1)]

lm_dt <- fread(paste0("processed_data/",platform,"/news_only/","us.csv"))
lm_dt[ ,country := "USA"]
lm_dt[,political:=ifelse(political=="","non-political",political)]
lm_dt <- lm_dt[!is.na(duration)]
lm_dt <- lm_dt[survey_lm, on = .(panelist_id)]
keep <- lm_dt[,.(max_visit=max(duration,na.rm=TRUE)), by=.(panelist_id)][max_visit>=120][["panelist_id"]]
lm_dt <- lm_dt[panelist_id%in%keep]
overall_ideo <- mean(lm_dt[["leftright"]],na.rm = TRUE)

lm_dt <- bakshy[lm_dt, on = .(domain)]
lm_dt <- robertson_data[lm_dt, on = .(domain)]
tidy_toplot <- tibble()
### present data ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-overall_ideo,by=c("domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-overall_ideo,by=c("domain")]
  res <- lmer(align ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol,pol) |> mutate(meta = "(A) Present data")
tidy_toplot <- bind_rows(tidy_toplot,tmp)

### Bakshy et al ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  res <- lmer(avg_align ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  res <- lmer(avg_align ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol,pol) |> mutate(meta = "(B) Bakshy et al. scores")
tidy_toplot <- bind_rows(tidy_toplot,tmp)

### Robertson et al. ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  res <- lmer(score ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  res <- lmer(score ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol,pol) |> mutate(meta = "(C) Robertson et al. scores")
tidy_toplot <- bind_rows(tidy_toplot,tmp)

### Budak et al. ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,budak_score := budak_score * 5]
  res <- lmer(budak_score ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,budak_score := budak_score * 5]
  res <- lmer(budak_score ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol,pol) |> mutate(meta = "(D) Budak et al. scores")
tidy_toplot <- bind_rows(tidy_toplot,tmp)

### AllSides scores ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  res <- lmer(allsides_score ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  res <- lmer(allsides_score ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol,pol) |> mutate(meta = "(F) AllSides controlled scores")
tidy_toplot <- bind_rows(tidy_toplot,tmp)

### Allsides community ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  res <- lmer(allsides_score_community ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  res <- lmer(allsides_score_community ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol,pol) |> mutate(meta = "(E) AllSides community scores")
tidy_toplot <- bind_rows(tidy_toplot,tmp)

### PEW ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,pew_score := pew_score * 2.5]
  res <- lmer(pew_score ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,pew_score := pew_score * 2.5]
  res <- lmer(pew_score ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol,pol) |> mutate(meta = "(G) PEW scores")
tidy_toplot <- bind_rows(tidy_toplot,tmp)


### MTURK scores ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  res <- lmer(mturk_score ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  res <- lmer(mturk_score ~ 1  + as.factor(leftright) + prev_type + polinterest + age + female + 
                as.factor(edu) + log_total_visits + (1|panelist_id), data = dat)
  lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol,pol) |> mutate(meta = "(H) MTurk scores")
tidy_toplot <- bind_rows(tidy_toplot,tmp)

write_csv(tidy_toplot,paste0("processed_data/regression/",platform,"_compare_other_scores.csv"))

###Plotting ----
tidy_toplot <- read_csv(paste0("processed_data/regression/",platform,"_compare_other_scores.csv"))
to_keep <- c("as.factor(leftright)1")

tidy_toplot <- tidy_toplot  |>  
  mutate(type = as.factor(type),
         type = dplyr::recode_factor(type,
                                     "non_political" = "Non-political News",
                                     "political" = "Political News"))

tidy_toplot |>  
  filter((term %in% to_keep)) |>  
  ggplot(aes(y = Estimate, x = factor(threshold))) +
  geom_pointrange(
    aes(ymin = CI_lower, ymax = CI_upper, color = type),
    position = position_dodge(0.6)) +
  scale_color_manual(values=c("Political News" = "#AA8939","Non-political News" = "#303C74"),
                     labels=c("Political news","Non-political news"),name="")+
  coord_flip() +
  theme_bw() + 
  facet_wrap(~meta,scales = "free_x",nrow=2)+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size=14)) +
  labs(x="cutoff (in sec)")+
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave(paste0("figures/",platform,"_compare_other_scores.pdf"), width = 12, height = 8)

## alt scores partisanship ----
### diversity ----
non_pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="non-political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-overall_ideo,by=c("domain")]
  dat[, `:=`(ideo_cat_A = fcase(is.na(align),NA_real_,align < -.2,-1, align > .2,1, default = 0),
             ideo_cat_B = fcase(is.na(avg_align),NA_real_,avg_align < -.2,-1, avg_align > .2,1, default = 0),
             ideo_cat_C = fcase(is.na(score),NA_real_,score < -.2,-1, score > .2,1, default = 0),
             ideo_cat_D = fcase(is.na(budak_score),NA_real_,budak_score < -.04,-1, budak_score > .04,1, default = 0),
             ideo_cat_E = fcase(is.na(allsides_score_community),NA_real_,allsides_score_community < -.2,-1, allsides_score_community > .2,1, default = 0),
             ideo_cat_F = fcase(is.na(allsides_score),NA_real_,allsides_score < -.2,-1, allsides_score > .2,1, default = 0),
             ideo_cat_G = fcase(is.na(pew_score),NA_real_,pew_score < -.2/2.5,-1, pew_score > .2/2.5,1, default = 0),
             ideo_cat_H = fcase(is.na(mturk_score),NA_real_,mturk_score < -.2,-1, mturk_score > .2,1, default = 0))]
  
  datA1 <- dat[!is.na(ideo_cat_A),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_A)]
  datB1 <- dat[!is.na(ideo_cat_B),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_B)]
  datC1 <- dat[!is.na(ideo_cat_C),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_C)]
  datD1 <- dat[!is.na(ideo_cat_D),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_D)]
  datE1 <- dat[!is.na(ideo_cat_E),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_E)]
  datF1 <- dat[!is.na(ideo_cat_F),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_F)]
  datG1 <- dat[!is.na(ideo_cat_G),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_G)]
  datH1 <- dat[!is.na(ideo_cat_H),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_H)]
  
  datA2 <- datA1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datB2 <- datB1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datC2 <- datC1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datD2 <- datD1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datE2 <- datE1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datF2 <- datF1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datG2 <- datG1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datH2 <- datH1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  
  tibble(
    country = "USA",
    cutoff = x,
    type = "non-political",
    score = c(1.5 * mean(datA2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datB2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datC2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datD2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datE2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datF2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datG2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datH2[["ideo_div"]],na.rm = TRUE)),
    meta = c("(A) Present data", "(B) Bakshy et al. scores", "(C) Robertson et al. scores", 
             "(D) Budak et al. scores", "(E) AllSides community scores",
             "(F) AllSides controlled scores", "(G) PEW scores","(H) MTurk scores"
             ) 
  )
  
})

pol <- map_dfr(cutoffs,function(x){
  dat <- lm_dt[political=="political" & duration >= x]
  dat[,align := mean(leftright,na.rm = TRUE)-overall_ideo,by=c("domain")]
  dat[, `:=`(ideo_cat_A = fcase(is.na(align),NA_real_,align < -.2,-1, align > .2,1, default = 0),
             ideo_cat_B = fcase(is.na(avg_align),NA_real_,avg_align < -.2,-1, avg_align > .2,1, default = 0),
             ideo_cat_C = fcase(is.na(score),NA_real_,score < -.2,-1, score > .2,1, default = 0),
             ideo_cat_D = fcase(is.na(budak_score),NA_real_,budak_score < -.04,-1, budak_score > .04,1, default = 0),
             ideo_cat_E = fcase(is.na(allsides_score_community),NA_real_,allsides_score_community < -.2,-1, allsides_score_community > .2,1, default = 0),
             ideo_cat_F = fcase(is.na(allsides_score),NA_real_,allsides_score < -.2,-1, allsides_score > .2,1, default = 0),
             ideo_cat_G = fcase(is.na(pew_score),NA_real_,pew_score < -.2/2.5,-1, pew_score > .2/2.5,1, default = 0),
             ideo_cat_H = fcase(is.na(mturk_score),NA_real_,mturk_score < -.2,-1, mturk_score > .2,1, default = 0))]
  
  datA1 <- dat[!is.na(ideo_cat_A),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_A)]
  datB1 <- dat[!is.na(ideo_cat_B),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_B)]
  datC1 <- dat[!is.na(ideo_cat_C),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_C)]
  datD1 <- dat[!is.na(ideo_cat_D),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_D)]
  datE1 <- dat[!is.na(ideo_cat_E),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_E)]
  datF1 <- dat[!is.na(ideo_cat_F),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_F)]
  datG1 <- dat[!is.na(ideo_cat_G),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_G)]
  datH1 <- dat[!is.na(ideo_cat_H),.(visit_by_ideo=.N), by = .(panelist_id,ideo_cat_H)]
  
  datA2 <- datA1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datB2 <- datB1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datC2 <- datC1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datD2 <- datD1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datE2 <- datE1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datF2 <- datF1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datG2 <- datG1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  datH2 <- datH1[,.(ideo_div = vegan::diversity(visit_by_ideo,index = "simpson")), by = .(panelist_id)]
  
  tibble(
    country = "USA",
    cutoff = x,
    type = "political",
    score = c(1.5 * mean(datA2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datB2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datC2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datD2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datE2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datF2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datG2[["ideo_div"]],na.rm = TRUE),
              1.5 * mean(datH2[["ideo_div"]],na.rm = TRUE)),
    meta = c("(A) Present data", "(B) Bakshy et al. scores", "(C) Robertson et al. scores", 
             "(D) Budak et al. scores", "(E) AllSides community scores",
             "(F) AllSides controlled scores", "(G) PEW scores","(H) MTurk scores"
    ) 
  )
  
})

sum_stat_int <- bind_rows(non_pol,pol) |> mutate(meta2 = "(A) News Diet Diversity")
saveRDS(sum_stat_int,paste0("processed_data/stats/",platform,"_diversity_other_scores.RDS"))


### partisanship ----
non_pol <- map_dfr(cutoffs,function(y){
  dat <- lm_dt
  if(fixN){
    peeps <- dat[duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dat <- dat[panelist_id%in%peeps]
  }
  dat <- dat[political=="non-political" & duration>=y]
  dom_align <- dat[,.(align_A = mean(leftright,na.rm = TRUE)-overall_ideo,
                      align_B = mean(avg_align,na.rm = TRUE),
                      align_C = mean(score,na.rm = TRUE),
                      align_D = mean(5 * budak_score,na.rm = TRUE),
                      align_E = mean(allsides_score_community,na.rm = TRUE),
                      align_F = mean(allsides_score,na.rm = TRUE),
                      align_G = mean(8/3 * pew_score,na.rm = TRUE),
                      align_H = mean(mturk_score,na.rm = TRUE)),
                   by = .(domain)]
  
  dat <- dat[dom_align, on = .(domain)]
  dat1 <- dat[,.(diet_slant_A=mean(align_A,na.rm = TRUE),
                 diet_slant_B=mean(align_B,na.rm = TRUE),
                 diet_slant_C=mean(align_C,na.rm = TRUE),
                 diet_slant_D=mean(align_D,na.rm = TRUE),
                 diet_slant_E=mean(align_E,na.rm = TRUE),
                 diet_slant_F=mean(align_F,na.rm = TRUE),
                 diet_slant_G=mean(align_G,na.rm = TRUE),
                 diet_slant_H=mean(align_H,na.rm = TRUE)), 
              by = .(panelist_id)]
  tibble(
    country = "USA",
    cutoff = y,
    type = "non-political",
    score = c(sd(dat1$diet_slant_A,na.rm = TRUE),
              sd(dat1$diet_slant_B,na.rm = TRUE),
              sd(dat1$diet_slant_C,na.rm = TRUE),
              sd(dat1$diet_slant_D,na.rm = TRUE),
              sd(dat1$diet_slant_E,na.rm = TRUE),
              sd(dat1$diet_slant_F,na.rm = TRUE),
              sd(dat1$diet_slant_G,na.rm = TRUE),
              sd(dat1$diet_slant_H,na.rm = TRUE)),
    meta = c("(A) Present data", "(B) Bakshy et al. scores", "(C) Robertson et al. scores", 
             "(D) Budak et al. scores", "(E) AllSides community scores",
             "(F) AllSides controlled scores", "(G) PEW scores", "(H) MTurk scores") 
  )
})

pol <- map_dfr(cutoffs,function(y){
  dat <- lm_dt
  if(fixN){
    peeps <- dat[duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    dat <- dat[panelist_id%in%peeps]
  }
  dat <- dat[political=="political" & duration>=y]
  dom_align <- dat[,.(align_A = mean(leftright,na.rm = TRUE)-overall_ideo,
                      align_B = mean(avg_align,na.rm = TRUE),
                      align_C = mean(score,na.rm = TRUE),
                      align_D = mean(5 * budak_score,na.rm = TRUE),
                      align_E = mean(allsides_score_community,na.rm = TRUE),
                      align_F = mean(allsides_score,na.rm = TRUE),
                      align_G = mean(8/3 * pew_score,na.rm = TRUE),
                      align_H = mean(mturk_score,na.rm = TRUE)),
                   by = .(domain)]
  
  dat <- dat[dom_align, on = .(domain)]
  dat1 <- dat[,.(diet_slant_A=mean(align_A,na.rm = TRUE),
                 diet_slant_B=mean(align_B,na.rm = TRUE),
                 diet_slant_C=mean(align_C,na.rm = TRUE),
                 diet_slant_D=mean(align_D,na.rm = TRUE),
                 diet_slant_E=mean(align_E,na.rm = TRUE),
                 diet_slant_F=mean(align_F,na.rm = TRUE),
                 diet_slant_G=mean(align_G,na.rm = TRUE),
                 diet_slant_H=mean(align_H,na.rm = TRUE)), 
              by = .(panelist_id)]
  tibble(
    country = "USA",
    cutoff = y,
    type = "political",
    score = c(sd(dat1$diet_slant_A,na.rm = TRUE),
              sd(dat1$diet_slant_B,na.rm = TRUE),
              sd(dat1$diet_slant_C,na.rm = TRUE),
              sd(dat1$diet_slant_D,na.rm = TRUE),
              sd(dat1$diet_slant_E,na.rm = TRUE),
              sd(dat1$diet_slant_F,na.rm = TRUE),
              sd(dat1$diet_slant_G,na.rm = TRUE),
              sd(dat1$diet_slant_H,na.rm = TRUE)),
    meta = c("(A) Present data", "(B) Bakshy et al. scores", "(C) Robertson et al. scores", 
             "(D) Budak et al. scores", "(E) AllSides community scores",
             "(F) AllSides controlled scores", "(G) PEW scores", "(H) MTurk scores") 
  )
})

sum_stat_int <- bind_rows(non_pol,pol) |> mutate(meta2 = "(B) Partisanship in News Diets")
saveRDS(sum_stat_int,paste0("processed_data/stats/",platform,"_partisan_other_scores.RDS"))

### plotting ----
sum_stat_partisan <- readRDS(paste0("processed_data/stats/",platform,"_partisan_other_scores.RDS"))
sum_stat_diverse  <- readRDS(paste0("processed_data/stats/",platform,"_diversity_other_scores.RDS"))

summary_scores <- bind_rows(sum_stat_diverse, sum_stat_partisan) |> 
  mutate(type = as.factor(type),
         type = dplyr::recode_factor(type,
                                     "non-political" = "Non-political News",
                                     "political" = "Political News"))


ggplot(summary_scores,aes(y = score, x = factor(cutoff))) +
  geom_point(
    aes(color = type)) +
  scale_color_manual(values=c("Political News" = "#AA8939","Non-political News" = "#303C74"),
                     labels=c("Political news","Non-political news"),name="")+
  theme_bw() + 
  facet_grid(meta2~meta, scales = "free_y") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.position = "bottom",
        strip.text = element_text(size=10),
        legend.text = element_text(size=14)) +
  ylim(0, .6)+
  labs(x="cutoff (in sec)",y= "scores")

ggsave(paste0("figures/",platform,"_divpart_other_scores.pdf"), width = 18, height = 10)

## prevalences ----
system("Rscripts/freq_count.sh")

### news proportion all visits ----
fl <- list.files("processed_data/stats",patter="type_freq",full.names = TRUE)
map_dfr(fl,function(f){
  df <- read_csv(f,show_col_types = FALSE)
  df$country <- long_cases[which(fl==f)]
  df
}) |> 
  group_by(country) |> 
  dplyr::summarise(news=count[value=="news"],all=sum(count)) |> 
  mutate(frac=round(news/all*100,2),all=format(all,big.mark=",")) |> 
  select(country,all,news=frac) |> 
  knitr::kable(format="latex",booktabs=TRUE)

### pol non pol visits  ----
fl <- list.files(paste0("processed_data/",platform,"/news_only"),pattern = "csv")

stats <- map_dfr(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
  map_dfr(cutoffs,function(y){
    tibble(
      country = long_cases[short_cases==str_sub(x,1,2)],
      cutoff=y,
      non_political=nrow(dt[political=="" & duration>=y]),
      political=nrow(dt[political=="political" & duration>=y]))
  })
})

stats |> 
  mutate(
    all = non_political + political,
    frac_pol = political/all
  ) |> 
  dplyr::filter(country=="United Kingdom") |> 
  mutate(frac_all = all/all[1]) |> 
  select(-country)

## misc descriptives ----
survey[, leftright := fcase(leftright < 6,-1,leftright == 6, 0, leftright > 6, 1)]
survey[,.(mean=mean(leftright,na.rm=TRUE),
          sd=sd(leftright,na.rm=TRUE),
          min=min(leftright,na.rm=TRUE),
          max=max(leftright,na.rm=TRUE),
          N=sum(!is.na(leftright))),by=.(country)]

# ## networks ----#
# source("Rscripts/helpers.R")
# library(igraph)
# ### create networks ----#
# fl <- list.files("processed_data/",platform,"/news_only", full.names = TRUE,pattern = "csv")
# res <- tibble(
#   country = character(0),
#   type = character(0),
#   cutoff = numeric(0),
#   political = logical(0),
#   fixN = logical(0),
#   network = list()
# )
# for (f in fl) {
#   cat(f,"\n")
#   ctry <- str_remove(str_extract(f, "[a-z]{2}\\."), "\\.")
#   dt <- data.table::fread(f)
#   for (cval in cutoffs) {
#     gnews <- create_networks(dt, political = FALSE, weights = FALSE, fixN = fixN, reach = 0, cutoff = cval)
#     gpols <- create_networks(dt, political = TRUE, weights = FALSE, fixN = fixN, reach = 0, cutoff = cval)
#     
#     tmp <- tibble(
#       country = rep(ctry, 6), type = c(names(gnews), names(gpols)),
#       cutoff = cval, political = rep(c(FALSE, TRUE), each = 3), fixN = fixN,
#       network = c(unname(gnews), unname(gpols))
#     )
#     
#     res <- bind_rows(res, tmp)
#   }
# }
# saveRDS(res, "processed_data/stats/networks.RDS")
# 
# tbl <- readRDS("processed_data/networks.RDS")
# tbl[["density"]] <- sapply(tbl[["network"]], graph.density)
# tbl[["political"]] <- if_else(tbl[["political"]],"political news","non-political news")
# tbl[["case"]] <- long_cases[match(tbl$country,short_cases)]
# # plot density for all networks ----#
# ggplot(tbl,aes(x=factor(cutoff),y=density,col=political))+
#   geom_point()+
#   geom_hline(yintercept = 0, linetype = "dashed",color="transparent")+
#   scale_color_manual(values=c("political news" = "#AA8939",
#                               "non-political news" = "#303C74"),
#                      labels=c("political news","non-political news"),name="")+
#   facet_grid(type~case,scales = "free_y") +
#   theme_bw()+
#   theme(legend.position = "none",
#         panel.grid.minor = element_blank(),
#         legend.text = element_text(family="sans", size = 20),
#         axis.text.x = element_text(family="sans", size = 12),
#         strip.text = element_text(face = "bold"),
#         text = element_text(family="sans", size=16))+
#   labs(x = "cutoff (in sec)",y = "")
# 
# ggsave("figures/network_densities.pdf",width=16,height=10)

# fl <- list.files(paste0("processed_data/",platform,"/news_only"),pattern = "csv")
# non_pol <- map_dfr(cutoffs,function(y) {
#   map_dfr(fl,function(x){
#     dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
#     dt <- dt[political=="" & duration>=y]
#     dt1 <- dt[,.(count=.N),by=.(panelist_id)]
#     tibble(
#       country = long_cases[short_cases==str_sub(x,1,2)],
#       cutoff = y,
#       value = mean(dt1[["count"]],na.rm=TRUE),
#       type = "non-political"
#     )
#   })
# })
# 
# pol <- map_dfr(cutoffs,function(y) {
#   map_dfr(fl,function(x){
#     dt <- data.table::fread(paste0("processed_data/",platform,"/news_only/",x))
#     dt <- dt[political=="political" & duration>=y]
#     dt1 <- dt[,.(count=.N),by=.(panelist_id)]
#     tibble(
#       country = long_cases[short_cases==str_sub(x,1,2)],
#       cutoff = y,
#       value = mean(dt1[["count"]],na.rm=TRUE),
#       type = "political"
#     )
#   })
# })
# 
# write_csv(bind_rows(non_pol,pol),paste0("processed_data/stats/",platform,"_avg_visits_news.csv"))
# 
# ### Plotting ----
# dat <- read_csv(paste0("processed_data/stats/",platform,"_avg_visits_news.csv"))
# 
# ggplot(dat,aes(x=factor(cutoff),y=value,col=type))+
#   geom_point(size = 1.5)+
#   scale_color_manual(values=c("political" = "#AA8939","non-political" = "#303C74"),
#                      labels=c("Political news","Non-political news"),name="")+
#   theme_bw() + 
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(size = 14),
#         strip.text = element_text(size = 12),
#         legend.position = "bottom",
#         legend.text = element_text(size=14)) +
#   labs(x="cutoff (in sec)",y="average visits per user")+
#   facet_wrap(country~.,scales="free_y")
# 
# ggsave(paste0("figures/",platform,"_avg_visits_news.pdf"),width=9,height=6)
