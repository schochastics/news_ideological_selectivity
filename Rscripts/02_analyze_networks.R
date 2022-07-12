library(igraph)
library(tidyverse)
pal <- c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", "#244579FF", "#C6242DFF")

tbl <- readRDS("processed_data/networks.RDS")
tbl[["density"]] <- sapply(tbl[["network"]], graph.density)
tbl[["political"]] <- if_else(tbl[["political"]],"political","non-political")

# plot density for all networks ----
types <- unique(tbl$type)
sapply(types,function(x){
  ggplot(filter(tbl,reach==0.01 & type==x)) +
    geom_line(aes(x=cutoff,y=density,color=country),size=0.5)+
    geom_point(aes(x=cutoff,y=density,color=country),size=2)+
    facet_wrap(political~.) +
    theme_minimal()+
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(size=12))+
    scale_color_manual(values=pal,name="sample")+
    labs(x = "cutoff (in sec)",y = "network density")+
    guides(colour = guide_legend(override.aes = list(size=2),nrow = 1))
  
  ggsave(paste0("figures/",x,"_density.pdf"),width = 10,height=6)
})

plot(tbl$network[[8]],vertex.label=NA,vertex.size=0.5)
g1 <- netUtils::biggest_component(g)
levelnet::superbox_graph(g,3)
