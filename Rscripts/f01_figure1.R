library(data.table)
library(tidyverse)
library(patchwork)
fl <- list.files("processed_data/tracking/")
# survey <- as.data.table(readRDS("data/survey_data_r.rds"))
# parties <- read_xlsx("data/party_families.xlsx")
# fixN <- FALSE
cutoffs <- c(3,10,20,60,120)


news <- lapply(cutoffs,function(y) {lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  npeeps <- length(unique(dt[["panelist_id"]]))
  dt <- dt[type!="" & duration>=y]
  data.table(share_single_news_visit = length(unique(dt[["panelist_id"]]))/npeeps,
             avg_news_visits = mean(dt[,.(nvis=sum(visits)),.(panelist_id)][["nvis"]]),
             avg_outlet_visits = mean(dt[,.N,.(panelist_id,domain)][,.N,.(panelist_id)][["N"]]),
             avg_diversity = mean(dt[,.N,.(panelist_id,domain)][,.(frac=N/sum(N)),.(panelist_id)][,.(Diversity=1-sum(frac^2)),.(panelist_id)][["Diversity"]]),
             cutoff = y, case = x,type="all news"
  )
})
})


pol <- lapply(cutoffs,function(y) {lapply(fl,function(x){
  dt <- data.table::fread(paste0("processed_data/tracking/",x))
  npeeps <- length(unique(dt[["panelist_id"]]))
  dt <- dt[type!="" & political=="political"& duration>=y]
  data.table(share_single_news_visit = length(unique(dt[["panelist_id"]]))/npeeps,
             avg_news_visits = mean(dt[,.(nvis=sum(visits)),.(panelist_id)][["nvis"]]),
             avg_outlet_visits = mean(dt[,.N,.(panelist_id,domain)][,.N,.(panelist_id)][["N"]]),
             avg_diversity = mean(dt[,.N,.(panelist_id,domain)][,.(frac=N/sum(N)),.(panelist_id)][,.(Diversity=1-sum(frac^2)),.(panelist_id)][["Diversity"]]),
             cutoff = y, case = x,type="political news"
  )
})
})

tbl <- map_dfr(c(news,pol),identity) |>
  pivot_longer(1:4) |>
  mutate(name_long=case_when(name=="share_single_news_visit"~ "Share of participants\nwith at least a single news visit",
                        name=="avg_news_visits" ~ "Average number of news visit",
                        name=="avg_outlet_visits" ~ "Average number of visited news outlets",
                        name=="avg_diversity" ~ "Average source diversity"),
         cases_long = case_when(case=="de.csv" ~ "Germany",
                                case=="es.csv" ~ "Spain",
                                case=="fr.csv" ~ "France",
                                case=="it.csv" ~ "Italy",
                                case=="uk.csv" ~ "United Kingdom",
                                case=="us.csv" ~ "USA"))

# tbl %>% 
#   split(list(.$name,.$type)) %>%
#   map(~ lm(value ~ cutoff, data = .)) %>%
#   map(coef) %>%
#   map_dbl("cutoff") %>% 
#   matrix(ncol=2)


plots <- unique(tbl$name_long)
cases <- unique(tbl$cases_long)
pList <- lapply(plots,function(px){
  lapply(cases,function(cy){
  tbl|>
    dplyr::filter(name_long==px,cases_long==cy)|>
      ggplot(aes(x=cutoff,y=value,color=type))+
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

pdf("figures/figure1.pdf", height = 16, width = 16)
wrap_plots(tst)+
  plot_layout(ncol=length(plots),nrow=length(cases),byrow = FALSE,guides = "collect") &
  theme(legend.position = 'bottom') 
grid::grid.draw(grid::textGrob("cutoff (in sec)", y = 0.05,gp=grid::gpar(fontsize=18)))
dev.off()
