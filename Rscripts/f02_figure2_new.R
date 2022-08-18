library(data.table)
library(ggplot2)
library(patchwork)
fl <- list.files("processed_data/tracking/news_only",full.names = TRUE,pattern = "csv")
countries <- c("Germany","Spain","France","Italy","United Kingdom","USA")
countries_short <- c("GER","ESP","FRA","ITA","UK","USA")
cutoffs <- c(0,3,10,20,60,120)
p1 <- vector("list",length(fl))
p2 <- vector("list",length(fl))
for(i in seq_along(fl)){
  res_visitors <- res_visits <- data.table(cutoff=cutoffs,
                                           non_pol=numeric(length(cutoffs)),
                                           pol=numeric(length(cutoffs)))
  df <- fread(fl[i])
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
  if(i==5){
    totals[1:2,1:2] <- totals[2:1,2:1]
  }
  
  res_visits$non_pol <- totals[1,]/colSums(totals)
  res_visits$pol <- totals[2,]/colSums(totals)
  
p1[[i]] <- ggplot(melt(res_visitors,id.vars = "cutoff"),
             aes(x=cutoff,y=value,col=variable)) + 
  geom_line()+ 
  geom_point()+
  theme_minimal()+
  labs(title = ifelse(i==1,"Proportion of news domain visitors",""),y=countries[i],x="")+
  coord_cartesian(clip="off")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "serif",size=16),
        axis.title.y = element_text(size=18,family = "serif",face = "bold"),
        plot.title = element_text(hjust=0.5,size = 18, lineheight = 1.5))+
  scale_y_continuous(limits=c(0,1),labels = scales::label_percent())+
  scale_color_manual(values=c("pol" = "#AA8939","non_pol" = "#303C74"),name="",labels=c("Political news","Non-political news"))

p2[[i]] <- ggplot(melt(res_visits,id.vars = "cutoff"),
             aes(x=cutoff,y=value,col=variable)) + 
  geom_line()+ 
  geom_point()+
  theme_minimal()+
  labs(title = ifelse(i==1,"Proportion of news article visits",""))+
  coord_cartesian(clip="off")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "serif",size=16),
        axis.title = element_blank(),
        plot.title = element_text(hjust=0.5,size = 18, lineheight = 1.5))+
  scale_y_continuous(limits=c(0,1),labels = scales::label_percent())+
  # scale_x_continuous(breaks = cutoffs)+
  scale_color_manual(values=c("pol" = "#AA8939","non_pol" = "#303C74"),name="",labels=c("Political news","Non-political news"))

# p1+p2+
#   plot_annotation(title = countries[i],
#                   theme = theme(plot.title = element_text(
#                     size = 22, lineheight = 1.2,color="black",family="serif")))+
#   plot_layout(guides = "collect") & theme(legend.position = 'bottom')
#   ggsave(paste0("figures/figure2_",countries_short[i],".pdf"),width = 15,height=5)
}
p <- c(p1,p2)
pfull <- wrap_plots(p,ncol = 2,nrow = length(fl),byrow = FALSE) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("figures/figure2_new.pdf",pfull,width = 10,height=25)
