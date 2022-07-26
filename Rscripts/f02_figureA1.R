library(igraph)
library(tidyverse)
library(patchwork)

tbl <- readRDS("processed_data/networks.RDS")
tbl[["density"]] <- sapply(tbl[["network"]], graph.density)
tbl[["political"]] <- if_else(tbl[["political"]],"political news","all news")
tbl <- tbl %>% 
  mutate(cases_long = case_when(country=="de" ~ "Germany",
                                country=="es" ~ "Spain",
                                country=="fr" ~ "France",
                                country=="it" ~ "Italy",
                                country=="uk" ~ "United Kingdom",
                                country=="us" ~ "USA"))

plots <- unique(tbl$type)[c(1,3,2,4)]
cases <- unique(tbl$cases_long)
pList <- lapply(plots,function(px){
  lapply(cases,function(cy){
    tbl|>
      dplyr::filter(type==px,cases_long==cy,reach==0.01)|>
      ggplot(aes(x=cutoff,y=density,color=political))+
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

pdf("figures/figureA1.pdf", height = 16, width = 16)
wrap_plots(tst)+
  plot_layout(ncol=length(plots),nrow=length(cases),byrow = FALSE,guides = "collect") &
  theme(legend.position = 'bottom') 
grid::grid.draw(grid::textGrob("cutoff (in sec)", y = 0.05,gp=grid::gpar(fontsize=18)))
dev.off()
