non_pola <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt <- dt[political == "" & duration >= y]
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(dem = fifelse(leftright < 6, 1, 0), rep = fifelse(leftright > 6, 1, 0), cen = fifelse(leftright == 6, 1, 0))]
        dt1 <- unique(dt[, .(domain, panelist_id, dem, rep, cen)])

        dt2 <- dt1[, .(demvisits = sum(dem, na.rm = TRUE), repvisits = sum(rep, na.rm = TRUE)), by = .(domain)]
        # dt2[, score := (repvisits / sum(repvisits) - demvisits / sum(demvisits)) * repvisits / (repvisits + demvisits)]
        dt2[, score := 1 / sum(repvisits) * repvisits * (repvisits - 1) / (repvisits + demvisits - 1) -
            1 / sum(demvisits) * demvisits * repvisits / (repvisits + demvisits - 1)]

        sum(dt2$score, na.rm = TRUE)
    })
})

pola <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt <- dt[political == "political" & duration >= y]
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(dem = fifelse(leftright < 6, 1, 0), rep = fifelse(leftright > 6, 1, 0), cen = fifelse(leftright == 6, 1, 0))]
        dt1 <- unique(dt[, .(domain, panelist_id, dem, rep, cen)])

        dt2 <- dt1[, .(demvisits = sum(dem, na.rm = TRUE), repvisits = sum(rep, na.rm = TRUE)), by = .(domain)]
        # dt2[, score := (repvisits / sum(repvisits) - demvisits / sum(demvisits)) * repvisits / (repvisits + demvisits)]
        dt2[, score := 1 / sum(repvisits) * repvisits * (repvisits - 1) / (repvisits + demvisits - 1) -
            1 / sum(demvisits) * demvisits * repvisits / (repvisits + demvisits - 1)]
        sum(dt2$score, na.rm = TRUE)
    })
})

bind_rows(
    combine_results(non_pol, pol) |> mutate(type = "non-adjusted"),
    combine_results(non_pola, pola) |> mutate(type = "adjusted")
) |>
    pivot_longer(cols = c(non_political, political), names_to = "news_type", values_to = "score") |>
    ggplot(aes(x = as.factor(cutoff), y = score, color = type)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "transparent") +
    facet_grid(news_type ~ case, scales = "free_y") +
    theme_bw() +
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(family = "sans", size = 14),
        axis.title = element_text(family = "sans", size = 20),
        strip.text = element_text(face = "bold"),
        text = element_text(family = "sans", size = 16)
    ) +
    labs(x = "threshold (in sec)", y = "score") +
    guides(colour = guide_legend(override.aes = list(size = 3)))
