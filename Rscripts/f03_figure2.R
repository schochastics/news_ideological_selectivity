library(igraph)
library(tidyverse)
library(patchwork)

net <- readRDS("processed_data/networks.RDS")
net[["value"]] <- sapply(net[["network"]], graph.density)
net[["political"]] <- if_else(net[["political"]],"political news","all news")
net <- net %>% 
  mutate(cases_long = case_when(country=="de" ~ "Germany",
                                country=="es" ~ "Spain",
                                country=="fr" ~ "France",
                                country=="it" ~ "Italy",
                                country=="uk" ~ "United Kingdom",
                                country=="us" ~ "USA")) %>% 
  dplyr::filter(reach==0.01,type=="pmi") %>% 
  mutate(type="density of pmi network") %>% 
  select(cases_long,type,political,cutoff,value)

seg_scores <- readRDS("processed_data/segregation_scores.RDS")
seg_scores_long <- seg_scores %>% 
  pivot_longer(cols=news:pol,names_to="political") %>% 
  mutate(political=if_else(political=="news","all news","political news")) %>% 
  mutate(cases_long = case_when(case=="de" ~ "Germany",
                                case=="es" ~ "Spain",
                                case=="fr" ~ "France",
                                case=="it" ~ "Italy",
                                case=="uk" ~ "United Kingdom",
                                case=="us" ~ "USA")) %>% 
  mutate(type="segregation score") %>% 
  select(cases_long,type,political,cutoff,value)



tbl <- bind_rows(net,seg_scores_long,readRDS("processed_data/diversity_scores.RDS"))

plots <- unique(tbl$type)
cases <- unique(tbl$cases_long)

pList <- lapply(plots,function(px){
  lapply(cases,function(cy){
    tbl|>
      dplyr::filter(type==px,cases_long==cy)|>
      ggplot(aes(x=cutoff,y=value,color=political))+
      geom_point()+
      geom_line()+
      scale_color_manual(values=c("political news" = "#AA8939","all news" = "#303C74"),name="")+
      theme_minimal()+
      theme(legend.position = "bottom",
            legend.margin = margin(t=20),
            axis.title = element_text(face = "bold",size=14),
            plot.title = element_text(face = "bold"),
            legend.text = element_text(size = 18))+
      labs(x="",
           y=ifelse(px==plots[1],cy,""),
           title=ifelse(cy==cases[1],px,""))
  })
})

tst <- unlist(pList,recursive = FALSE)

pdf("figures/figure2.pdf", height = 16, width = 16)
wrap_plots(tst)+
  plot_layout(ncol=length(plots),nrow=length(cases),byrow = FALSE,guides = "collect") &
  theme(legend.position = 'bottom') 
grid::grid.draw(grid::textGrob("cutoff (in sec)", y = 0.05,gp=grid::gpar(fontsize=18)))
dev.off()
