library(igraph)
library(tidyverse)
pal <- c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", "#244579FF", "#C6242DFF")
# density of networks ----
tbl <- readRDS("processed_data/networks.RDS")
tbl[["density"]] <- sapply(tbl[["network"]], graph.density)
tbl[["political"]] <- if_else(tbl[["political"]],"political","non-political")

ggplot(filter(tbl,reach==0.01 & type=="pmi")) +
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
