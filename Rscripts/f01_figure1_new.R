library(data.table)
library(ggplot2)
library(patchwork)
library(ggtext)

fl <- list.files("processed_data/tracking",full.names = TRUE,pattern = "csv")
countries <- c("Germany","Spain","France","Italy","United Kingdom","USA")
countries_short <- c("GER","ESP","FRA","ITA","UK","USA")

if(!dir.exists("processed_data/tracking/figure1/")){
  dir.create("processed_data/tracking/figure1/")
}

for(i in seq_along(fl)){
  cat(countries[i],sep="\n")
  dt <- fread(fl[i])
  tst1 <- dt[,.(news_vis=sum(type!="")),by=.(panelist_id)]
  val1 <- sum(tst1$news_vis!=0)/nrow(tst1)
  
  tst1 <- dt[,.(news_vis=sum(type!="" & political=="")),by=.(panelist_id)]
  val2 <- sum(tst1$news_vis!=0)/nrow(tst1)
  
  tst1 <- dt[,.(news_vis=sum(type!="" & political!="")),by=.(panelist_id)]
  val3 <- sum(tst1$news_vis!=0)/nrow(tst1)
  dat1 <- data.frame(type=c("news","non_news"),value=c(sum(dt[["type"]]!="")/nrow(dt),sum(dt[["type"]]=="")/nrow(dt)))

  
 
  
  dat2 <- data.frame(type=c("News in general","Non-political news","Political news"),
                     value=c(val1,val2,val3)) 
  

  dat3 <- data.frame(
    type = c("political news","Non-political news"),
    value = c(sum(dt[["political"]]!="")/sum(dt[["type"]]!=""),1-sum(dt[["political"]]!="")/sum(dt[["type"]]!="")))
  
  dat3$ypos <- cumsum(dat3$value)- 0.5*dat3$value 
  
  saveRDS(list(dat1,dat2,dat3),paste0("processed_data/tracking/figure1/",countries_short[i],".RDS"))
  
}
# plot_title <- "News domain visits"
# p1 <- ggplot(dat1,aes(x="",y=value,fill=type))+ geom_bar(stat="identity", width=1) +
#   annotate("text",x=1.52,y=1.0,label=paste0(round(100*dat1$value[1],2),"%"),color="black",family="serif",size=6,hjust=0.5,vjust=0)+
#   coord_polar("y", start=0,direction = -1)+
#   scale_fill_manual(values=c("news" = "#AA8939","non_news" = "grey66"))+#"#303C74"))+
#   theme_void()+
#   labs(title = plot_title)  +
#   theme(legend.position = "none",
#         text = element_text(family = "serif"),
#         plot.title.position = "plot",
#         plot.title = element_markdown(hjust=0.5,size = 18, lineheight = 1.5))
# 
# p2 <- ggplot(dat2,aes(x=type,y=value,fill=type))+
#   geom_col(show.legend = FALSE)+
#   geom_text(aes(label=paste0(round(value*100),"%")),vjust=-1)+
#   theme_minimal()+
#   labs(title = "News domain visitors")+
#   coord_cartesian(clip="off")+
#   theme(panel.grid.major.x = element_blank(),
#         text = element_text(family = "serif",size=16),
#         axis.title = element_blank(),
#         plot.title = element_markdown(hjust=0.5,size = 18, lineheight = 1.5))+
#   scale_y_continuous(limits=c(0,1),labels = scales::label_percent())+
#   scale_fill_manual(values=c("Political news" = "#AA8939","News in general" = "grey66","Non-political news"="#303C74"))
# 
# p3 <- ggplot(dat3,aes(x="",y=value,fill=type))+ geom_bar(stat="identity", width=1) +
#   geom_text(aes(y=ypos,label=paste0(round(value*100),"%")),size=6,color="white",family="serif")+
#   coord_polar("y", start=0,direction = 1)+
#   scale_fill_manual(values=c("political news" = "#AA8939","Non-political news" = "#303C74"),
#                     name="",labels=c("Political news articles","Non-political new articles"))+
#   theme_void()+
#   labs(title = "Visits of <span style='color:#AA8939'>political</span> and <span style='color:#303C74'>non-political</span><br> news articles")  +
#   theme(legend.position = "none",
#         text = element_text(family = "serif",size=16),
#         plot.title.position = "plot",
#         plot.title = element_markdown(hjust=0.5,size = 18, lineheight = 1.2,color="black"))
# 
# p <- p1+p2+p3+plot_annotation(
#   title=countries[i],theme = theme(plot.title = element_markdown(size = 22, lineheight = 1.2,color="black",family="serif")))
# ggsave(paste0("figures/figure1_",countries_short[i],".pdf"),width = 15,height=5)
