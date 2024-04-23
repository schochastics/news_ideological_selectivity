# packages ----
library(tidyverse)
library(data.table)
library(patchwork)
library(lme4)


# one of "desktop", "mobile" or "both"
# main paper is "both", appendix is "desktop"
platform <- "both"

fl <- list.files(paste0("processed_data/", platform, "/news_only"), pattern = "csv")
cutoffs <- c(3, 10, 30, 60, 120)
short_cases <- c("de", "es", "fr", "it", "uk", "us")
long_cases <- c("Germany", "Spain", "France", "Italy", "United Kingdom", "USA")
survey <- as.data.table(readRDS("data/survey_data_r.rds"))
fixN <- TRUE

# some helper function to get the regression estimates into the graphs
lmer_to_tidy <- function(res) {
    res <- summary(res) |>
        coef() |>
        as_tibble(rownames = "term")
    res$CI_lower <- res$Estimate - 1.96 * res$`Std. Error`
    res$CI_upper <- res$Estimate + 1.96 * res$`Std. Error`
    return(res)
}

combine_results <- function(non_pol, pol) {
    do.call("rbind", non_pol) |>
        as_tibble() |>
        mutate(cutoff = cutoffs, type = "non_political") |>
        bind_rows(
            do.call("rbind", pol) |>
                as_tibble() |>
                mutate(cutoff = cutoffs, type = "political")
        ) |>
        pivot_longer(all_of(fl)) |>
        pivot_wider(names_from = type) |>
        mutate(case = str_extract(name, "^[a-z]{2}")) |>
        mutate(case = long_cases[match(case, short_cases)]) |>
        select(-name)
}

## ----------------------------------------------------------------------------##
# Overall news descriptives ----
## ----------------------------------------------------------------------------##
if (!dir.exists("processed_data/stats/")) {
    dir.create("processed_data/stats/")
}

vis_cnt_lst <- map(seq_along(fl), function(i) {
    res_visitors <- data.table(
        cutoff = cutoffs,
        non_pol = numeric(length(cutoffs)),
        pol = numeric(length(cutoffs))
    )
    df <- fread(paste0("processed_data/", platform, "/news_only/", fl[i]))
    n <- length(unique(df[duration >= 3][["panelist_id"]]))
    # n1 <- length(unique(df[["panelist_id"]][df[["political"]]=="political"]))
    # adjust_to_all <- c(0.8662,0.9405,0.9193,0.9064,0.9257,0.8667)[i]
    fracs <- sapply(cutoffs, function(x) {
        tmp1 <- length(unique(df[duration >= x & political == "political"][["panelist_id"]])) / n * adjust_to_all
        tmp2 <- length(unique(df[duration >= x][["panelist_id"]])) / n * adjust_to_all
        c(tmp1, tmp2)
    })
    res_visitors$pol <- fracs[1, ]
    res_visitors$non_pol <- fracs[2, ]

    res_visitors
})
saveRDS(vis_cnt_lst, paste0("processed_data/stats/", platform, "_vis_counts.RDS"))

vis_cnt_lst <- readRDS(paste0("processed_data/stats/", platform, "_vis_counts.RDS"))
plt_tbl <- map_dfr(seq_along(vis_cnt_lst), function(x) {
    vis_cnt_lst[[x]]$case <- long_cases[x]
    # vis_cnt_lst[[x]]$level <- "Proportion of news article visits"
    vis_cnt_lst[[x]]$level <- "Proportion of news domain visitors"
    bind_rows(vis_cnt_lst[[x]])
}) |>
    pivot_longer(cols = c(non_pol, pol)) |>
    mutate(name = ifelse(name == "pol", "political news", "non-political news"))

p <- ggplot(plt_tbl, aes(x = as.factor(cutoff), y = value, color = name, shape = name)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "transparent") +
    scale_color_manual(
        values = c("political news" = "#AA8939", "non-political news" = "#303C74"),
        labels = c("political news" = "Political news", "non-political news" = "Non-political news"), name = ""
    ) +
    scale_shape_manual(
        values = c("political news" = 16, "non-political news" = 17),
        labels = c("political news" = "Political news", "non-political news" = "Non-political news"), name = ""
    ) +
    facet_grid(. ~ case, scales = "free_y") +
    theme_bw() +
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family = "sans", size = 14),
        axis.text.x = element_text(family = "sans", size = 12),
        axis.title.x = element_text(family = "sans", size = 14),
        strip.text = element_text(face = "bold"),
        text = element_text(family = "sans", size = 12)
    ) +
    labs(x = "threshold (in sec)", y = "") +
    scale_y_continuous(labels = scales::label_percent()) +
    guides(fill = guide_legend(override.aes = list(size = 3)))

if (!dir.exists("figures")) {
    dir.create("figures")
}

ggsave(paste0("figures/", platform, "_figure1.pdf"), p, width = 10, height = 4)

## ----------------------------------------------------------------------------##
# Comparison pol/nonpol ----
## ----------------------------------------------------------------------------##
# Segregation score ----
# Operational decisions:
#   - based on unique visitors
#   - recode the ideology variable lefright [1-11] -> left: [1-6), center: 6, right: (6,11]
#   - delete cases with missing data from the survey data set (The deletion intends to make the samples consistent across the different types of analyses (including the regression analyses)

## isolation index -----

non_pol <- lapply(cutoffs, function(y) {
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
        dt2[, score := (repvisits / sum(repvisits) - demvisits / sum(demvisits)) * repvisits / (repvisits + demvisits)]
        sum(dt2$score, na.rm = TRUE)
    })
})

pol <- lapply(cutoffs, function(y) {
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
        dt2[, score := (repvisits / sum(repvisits) - demvisits / sum(demvisits)) * repvisits / (repvisits + demvisits)]
        sum(dt2$score, na.rm = TRUE)
    })
})

combine_results(non_pol, pol) |>
    saveRDS(paste0("processed_data/stats/", platform, "_segregation_scores.RDS"))

## dissimilarity index -----
non_pol <- lapply(cutoffs, function(y) {
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
        dt2[, score := abs(repvisits / sum(repvisits) - demvisits / sum(demvisits))]
        0.5 * sum(dt2$score, na.rm = TRUE)
    })
})

pol <- lapply(cutoffs, function(y) {
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
        dt2[, score := abs(repvisits / sum(repvisits) - demvisits / sum(demvisits))]
        0.5 * sum(dt2$score, na.rm = TRUE)
    })
})

combine_results(non_pol, pol) |>
    saveRDS(paste0("processed_data/stats/", platform, "_dissimilarity_scores.RDS"))


## Atkinson index -----
non_pol <- lapply(cutoffs, function(y) {
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
        dt2[, score := (repvisits / sum(repvisits))^(1 / 2) * (demvisits / sum(demvisits))^(1 / 2)]
        1 - sum(dt2$score, na.rm = TRUE)
    })
})

pol <- lapply(cutoffs, function(y) {
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
        dt2[, score := (repvisits / sum(repvisits))^(1 / 2) * (demvisits / sum(demvisits))^(1 / 2)]
        1 - sum(dt2$score, na.rm = TRUE)
    })
})

combine_results(non_pol, pol) |>
    saveRDS(paste0("processed_data/stats/", platform, "_atkinson_scores.RDS"))

# Partisan slant/news diets ----
# The ideology variable is centered around the respective country mean
# The centering takes int account that the news audience in some countries as a whole more strongly lean to
# the left or right than in others
# Technically, the centering makes the analyses more compatible with the regression
# analyses and the estimates more stable
#
# Following Flaxman et al., we use the standard deviation of news diets (consequences are neglible,
# simply produces a little higher values by giving more extreme news diets a little more weight from the outset)
non_pol <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "" & duration >= y]

        # calculate the ideological slant of the individual participants news diets
        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo), by = .(domain)]
        dt[dom_align, on = .(domain), dom_align := align]
        dt1 <- dt[, .(diet_slant = mean(dom_align, na.rm = TRUE)), by = .(panelist_id)]
        sd(dt1[["diet_slant"]])
    })
})

pol <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "political" & duration >= y]

        # calculate the ideological slant of the individual participants news diets
        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo), by = .(domain)]
        dt[dom_align, on = .(domain), dom_align := align]
        dt1 <- dt[, .(diet_slant = mean(dom_align, na.rm = TRUE)), by = .(panelist_id)]
        sd(dt1[["diet_slant"]])
    })
})

combine_results(non_pol, pol) |>
    saveRDS(paste0("processed_data/stats/", platform, "_news_diet_slant.RDS"))


# Sqrt 2 version for 2nd revision
non_pol <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, 0, leftright > 6, 1, default = 0.5))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "" & duration >= y]

        # calculate the ideological slant of the individual participants news diets
        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo), by = .(domain)]
        dt[dom_align, on = .(domain), dom_align := align]
        dt1 <- dt[, .(diet_slant = mean(dom_align, na.rm = TRUE)), by = .(panelist_id)]
        sd(dt1[["diet_slant"]])*sqrt(2)
    })
})

pol <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, 0, leftright > 6, 1, default = 0.5))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "political" & duration >= y]

        # calculate the ideological slant of the individual participants news diets
        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo), by = .(domain)]
        dt[dom_align, on = .(domain), dom_align := align]
        dt1 <- dt[, .(diet_slant = mean(dom_align, na.rm = TRUE)), by = .(panelist_id)]
        sd(dt1[["diet_slant"]])*sqrt(2)
    })
})

combine_results(non_pol, pol) |>
    saveRDS(paste0("processed_data/stats/", platform, "_news_diet_slant_01sqrt2.RDS"))


# Diversity measures ----
## Simpson Diversity ----
non_pol <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "" & duration >= y]

        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo), by = .(domain)]
        dom_align[, ideo_cat := fcase(align < -.2, -1, align > .2, 1, default = 0)]
        dt[dom_align, on = .(domain), ideo_cat := ideo_cat]
        dt1 <- dt[, .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat)]
        dt2 <- dt1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
        1.5 * mean(dt2[["ideo_div"]])
    })
})

pol <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "political" & duration >= y]

        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo), by = .(domain)]
        dom_align[, ideo_cat := fcase(align < -.2, -1, align > .2, 1, default = 0)]
        dt[dom_align, on = .(domain), ideo_cat := ideo_cat]
        dt1 <- dt[, .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat)]
        dt2 <- dt1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
        1.5 * mean(dt2[["ideo_div"]])
    })
})

combine_results(non_pol, pol) |>
    saveRDS(paste0("processed_data/stats/", platform, "_simpson_diversity.RDS"))

## Shannon ----
non_pol <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "" & duration >= y]

        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo), by = .(domain)]
        dom_align[, ideo_cat := fcase(align < -.2, -1, align > .2, 1, default = 0)]
        dt[dom_align, on = .(domain), lcr := ideo_cat]
        mean(dt[, .N, .(panelist_id, lcr)][, .(frac = N / sum(N)), .(panelist_id)][, .(Diversity = -sum(frac * log(frac))), .(panelist_id)][["Diversity"]])
    })
})

pol <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "political" & duration >= y]

        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo), by = .(domain)]
        dom_align[, ideo_cat := fcase(align < -.2, -1, align > .2, 1, default = 0)]
        dt[dom_align, on = .(domain), lcr := ideo_cat]
        mean(dt[, .N, .(panelist_id, lcr)][, .(frac = N / sum(N)), .(panelist_id)][, .(Diversity = -sum(frac * log(frac))), .(panelist_id)][["Diversity"]])
    })
})

combine_results(non_pol, pol) |>
    saveRDS(paste0("processed_data/stats/", platform, "_entropy.RDS"))


# Figure 2 for Paper----
result_files <- paste0(platform, c(
    "_segregation_scores.RDS",
    "_simpson_diversity.RDS",
    "_news_diet_slant.RDS"
))
types <- c("(A) Isolation", "(B) Simpson's D", "(C) News Diet Slant (SD)")

res_tbl <- map_dfr(seq_along(result_files), function(x) {
    readRDS(paste0("processed_data/stats/", result_files[x])) |>
        mutate(type = types[x]) |>
        pivot_longer(cols = c(non_political, political), names_to = "news_type", values_to = "score")
})


ggplot(res_tbl, aes(x = as.factor(cutoff), y = score, color = news_type, shape = news_type)) +
    geom_point(size=3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "transparent") +
    scale_color_manual(
        values = c("political" = "#AA8939", "non_political" = "#303C74"),
        labels = c("political" = "Political news", "non_political" = "Non-political news"), name = ""
    ) +
    scale_shape_manual(
        values = c("political" = 16, "non_political" = 17),
        labels = c("political" = "Political news", "non_political" = "Non-political news"), name = ""
    ) +
    facet_grid(type ~ case, scales = "free_y") +
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

ggsave(paste0("figures/", platform, "_figure2.pdf"), width = 16, height = 10)

# revision figure comparing News diet slant ----
# Figure 2 for Paper----
result_files <- paste0(platform, c(
    "_news_diet_slant.RDS",
    "_news_diet_slant_01sqrt2.RDS"
))
types <- c("(A) News Diet Slant (SD)", "(B) News Diet Slant (SD2)")

res_tbl <- map_dfr(seq_along(result_files), function(x) {
    readRDS(paste0("processed_data/stats/", result_files[x])) |>
        mutate(type = types[x]) |>
        pivot_longer(cols = c(non_political, political), names_to = "news_type", values_to = "score")
})


ggplot(res_tbl, aes(x = as.factor(cutoff), y = score, color = news_type, shape = news_type)) +
    geom_point(size=3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "transparent") +
    scale_color_manual(
        values = c("political" = "#AA8939", "non_political" = "#303C74"),
        labels = c("political" = "Political news", "non_political" = "Non-political news"), name = ""
    ) +
    scale_shape_manual(
        values = c("political" = 16, "non_political" = 17),
        labels = c("political" = "Political news", "non_political" = "Non-political news"), name = ""
    ) +
    facet_grid(type ~ case, scales = "free_y") +
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

ggsave(paste0("figures/", platform, "_news_diet_slants.pdf"), width = 16, height = 10)


# Prepare Regression Data ----
# prepare Data
survey_lm <- survey[, .(panelist_id, leftright, polinterest, extremism, age, female, edu, log_total_visits)]
survey_lm <- na.omit(survey_lm)
survey_lm[, extreme := (abs(leftright - 6) / 5)]
survey_lm[, leftright := fcase(leftright < 6, -1, leftright == 6, 0, leftright > 6, 1)]
survey_lm[, polinterest := polinterest * -1]
survey_lm[, age := (age - 30) / 35]

lm_dt <- lapply(seq_along(fl), function(i) {
    dt <- fread(paste0("processed_data/", platform, "/news_only/", fl[i]))
    dt[, country := long_cases[i]]
    dt[, prev_type := fcase(
        prev_type == "direct", "direct",
        prev_type == "ebay", "direct",
        prev_type == "news", "direct",
        prev_type == "other", "direct",
        prev_type == "facebook", "facebook",
        prev_type == "twitter", "twitter",
        prev_type == "search", "search",
        prev_type == "portal", "portal"
    )]
    dt[, prev_type := as.factor(prev_type)]
    dt[, political := ifelse(political == "", "non-political", political)]
}) |> rbindlist()

lm_dt <- lm_dt[!is.na(duration)]
lm_dt <- lm_dt[survey_lm, on = .(panelist_id)]
keep <- lm_dt[, .(max_visit = max(duration, na.rm = TRUE)), by = .(panelist_id)][max_visit >= 120][["panelist_id"]]
lm_dt <- lm_dt[panelist_id %in% keep]
lm_dt[, ideo_over := mean(leftright, na.rm = TRUE), by = country]

# In the following, we conduct the substantive regression analyses.
# We need to run one regression set per moderator.
# Although We do not plot the respective coefficients, We additionally control for logged(total visits), as
# is common with most previous research

# Regressions I-----
## Countries (Moderation Analysis) ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) +
        extreme + country:as.factor(leftright) + country + prev_type +
        polinterest + age + female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) +
        extreme + country:as.factor(leftright) + country + prev_type +
        polinterest + age + female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_country_inter <- bind_rows(non_pol, pol)

# extract, recode, and save interaction terms for later graphing
to_keep <- paste0("as.factor(leftright)1:country", long_cases)

tidy_toplot_country_inter <- tidy_toplot_country_inter |>
    dplyr::filter(term %in% to_keep) |>
    mutate(
        header = "(A) Country \n (Reference: France)",
        term = str_replace(term, "as.factor\\(leftright\\)1:country", "\\(A\\) Country: ")
    )

## News Access (Moderation Analysis) ----

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) +
        extreme + prev_type:as.factor(leftright) + country + prev_type +
        as.factor(polinterest) + age + female + as.factor(edu) +
        log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) +
        extreme + prev_type:as.factor(leftright) + country + prev_type +
        as.factor(polinterest) + age + female + as.factor(edu) +
        log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_access_inter <- bind_rows(non_pol, pol)

to_keep <- paste0("as.factor(leftright)1:prev_type", c("facebook", "twitter", "search", "portal"))

tidy_toplot_access_inter <- tidy_toplot_access_inter |>
    dplyr::filter(term %in% to_keep) |>
    mutate(
        header = "(B) Access \n (Reference: Direct)",
        term = str_replace(term, "as.factor\\(leftright\\)1:prev_type", "\\(B\\) Access: ") |> str_to_title()
    )

## Political Interest (Moderation Analysis) ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
        as.factor(polinterest):as.factor(leftright) + country + prev_type +
        as.factor(polinterest) + age + female + as.factor(edu) +
        log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
        as.factor(polinterest):as.factor(leftright) + country + prev_type +
        as.factor(polinterest) + age + female + as.factor(edu) +
        log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_interest_inter <- bind_rows(non_pol, pol)

to_keep <- "as.factor(leftright)1:as.factor(polinterest)-1"

tidy_toplot_interest_inter <- tidy_toplot_interest_inter |>
    dplyr::filter(term %in% to_keep) |>
    mutate(
        header = "(C) Political Interest \n(Reference: Low Political Interest)",
        term = str_replace(
            term, "as.factor\\(leftright\\)1:as.factor\\(polinterest\\)-1",
            "\\(C\\) Political Interest: High"
        )
    )

# Change order
tidy_toplot_interest_inter <- tidy_toplot_interest_inter |>
    mutate(Estimate = -1 * Estimate, CI_lower = -1 * CI_lower, CI_upper = -1 * CI_upper)

## Extremity (Moderation Analysis) ----
# (This part of the analyses produces error messages; these can be ignored. They simply reflect that
# very low extremity is identical with centrist ideology; the procedure automatically drop the corresponding interaction terms from the analysis)

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) + extreme + extreme:as.factor(leftright) +
        country + prev_type + as.factor(polinterest) + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) + extreme + extreme:as.factor(leftright) +
        country + prev_type + as.factor(polinterest) + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_extremity_inter <- bind_rows(non_pol, pol)

to_keep <- c("as.factor(leftright)1:extreme")

tidy_toplot_extremity_inter <- tidy_toplot_extremity_inter |>
    dplyr::filter(term %in% to_keep) |>
    mutate(
        header = "(D) Extremity \n(Reference: Low Extremity)",
        term = str_replace(
            term, "as.factor\\(leftright\\)1:extreme",
            "\\(D\\) Extremity: High"
        )
    )

## Generation (Moderation Analysis) ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) + extreme + age:as.factor(leftright) +
        country + prev_type + as.factor(polinterest) + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- res <- lmer(align ~ 1 + as.factor(leftright) + extreme + age:as.factor(leftright) +
        country + prev_type + as.factor(polinterest) + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_generation_inter <- bind_rows(non_pol, pol)

to_keep <- c("as.factor(leftright)1:age")

tidy_toplot_generation_inter <- tidy_toplot_generation_inter |>
    dplyr::filter(term %in% to_keep) |>
    mutate(
        header = "(E) Generation \n(Reference: Millenial)",
        term = str_replace(
            term, "as.factor\\(leftright\\)1:age",
            "\\(E\\) Generation: Boomer"
        )
    )

## Gender (Moderation Analysis) ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) + extreme + female:as.factor(leftright) +
        country + prev_type + as.factor(polinterest) + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- res <- lmer(align ~ 1 + as.factor(leftright) + extreme + female:as.factor(leftright) +
        country + prev_type + as.factor(polinterest) + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_gender_inter <- bind_rows(non_pol, pol)

to_keep <- c("as.factor(leftright)1:female")

tidy_toplot_gender_inter <- tidy_toplot_gender_inter |>
    dplyr::filter(term %in% to_keep) |>
    mutate(
        header = "(F) Gender \n(Reference: Female)",
        term = str_replace(
            term, "as.factor\\(leftright\\)1:female",
            "\\(F\\) Gender: Male"
        )
    )

# Change order
tidy_toplot_gender_inter <- tidy_toplot_gender_inter |>
    mutate(Estimate = -1 * Estimate, CI_lower = -1 * CI_lower, CI_upper = -1 * CI_upper)

## Education (Moderation Analysis) ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) + extreme + as.factor(edu):as.factor(leftright) +
        country + prev_type + as.factor(polinterest) + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    res <- res <- lmer(align ~ 1 + as.factor(leftright) + extreme + as.factor(edu):as.factor(leftright) +
        country + prev_type + as.factor(polinterest) + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tidy_toplot_education_inter <- bind_rows(non_pol, pol)

to_keep <- c("as.factor(leftright)1:as.factor(edu)3")

tidy_toplot_education_inter <- tidy_toplot_education_inter |>
    dplyr::filter(term %in% to_keep) |>
    mutate(
        header = "(G) Education \n(Reference: Low Education)",
        term = str_replace(
            term, "as.factor\\(leftright\\)1:as.factor\\(edu\\)3",
            "\\(G\\) Education: High"
        )
    )

tidy_toplot_integrated <- bind_rows(
    tidy_toplot_country_inter, tidy_toplot_access_inter, tidy_toplot_interest_inter, tidy_toplot_extremity_inter,
    tidy_toplot_generation_inter, tidy_toplot_gender_inter, tidy_toplot_education_inter
)

if (!dir.exists("processed_data/regression")) {
    dir.create("processed_data/regression")
}
write_csv(tidy_toplot_integrated, paste0("processed_data/regression/", platform, "_interaction_terms.csv"))
## Plotting ----
tidy_toplot_integrated <- read_csv(paste0("processed_data/regression/", platform, "_interaction_terms.csv"))
tidy_toplot_integrated <- tidy_toplot_integrated |> mutate(header = as.factor(header))
level_order <- rev(levels(tidy_toplot_integrated$header))

tidy_toplot_integrated <- tidy_toplot_integrated |>
    mutate(
        type = as.factor(type),
        type = dplyr::recode_factor(type,
            "non_political" = "Non-Political News",
            "political" = "Political News"
        )
    )

tidy_toplot_integrated |>
    mutate(term = str_replace_all(term, "\\(B\\) Access", "\\(C\\) News Access")) |>
    mutate(term = str_replace_all(term, "\\(C\\) Political Interest", "\\(B\\) Political Interest")) |>
    mutate(term = str_replace_all(term, "Direct", "Non-referred")) |>
    mutate(header = str_replace_all(header, "\\(B\\) Access", "\\(C\\) News Access")) |>
    mutate(header = str_replace_all(header, "Reference: Direct", "Reference: Non-referred")) |>
    mutate(header = str_replace_all(header, "\\(C\\) Political Interest", "\\(B\\) Political Interest")) |>
    ggplot(aes(y = Estimate, x = factor(threshold))) +
    geom_pointrange(
        aes(ymin = CI_lower, ymax = CI_upper, color = term, shape = term),
        size = 0.32,
        position = position_dodge2(w = 0.5)
    ) +
    coord_flip() +
    theme_bw() +
    scale_color_manual(values = c(
        "#E69F00", "#009E73", "#0072B2", "#D55E00", "grey25",
        "#E69F00",
        "#E69F00", "#009E73", "#0072B2", "#D55E00",
        "#009E73", "#0072B2", "#D55E00", "grey25"
    ), name = "") +
    scale_shape_manual(values = c(
        15, 15, 15, 15, 15,
        17,
        16, 16, 16, 16,
        17, 17, 17, 17
    ), name = "") +
    facet_grid(type ~ header, scales = "free_x") +
    theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18),
        legend.position = "bottom",
        legend.text = element_text(size = 14)
    ) +
    labs(
        title = "",
        x = "threshold (in sec)", y = "estimate"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(color = guide_legend(override.aes = list(size = 0.75)))

ggsave(paste0("figures/", platform, "_regression_interaction.pdf"), width = 15, height = 7)

# Regressions II-----
## Country (Conditional Effects) ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(long_cases, function(ref) {
        dat[country == ref][["country"]] <- "A_reference"
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            country:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "non_political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(long_cases, function(ref) {
        dat[country == ref][["country"]] <- "A_reference"
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            country:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

tidy_toplot_country <- bind_rows(non_pol, pol) %>%
    mutate(
        header = "(A) Country",
        level = as.factor(level),
        level = dplyr::recode_factor(level,
            "France" = "(A) Country: France",
            "Germany" = "(A) Country: Germany",
            "Italy" = "(A) Country: Italy",
            "Spain" = "(A) Country: Spain",
            "United Kingdom" = "(A) Country: United Kingdom",
            "USA" = "(A) Country: USA"
        )
    )


## Access (Conditional Effects Analysis)----
reference <- c("direct", "facebook", "twitter", "search", "portal")
lm_dt$prev_type <- as.character(lm_dt$prev_type)

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[prev_type == ref][["prev_type"]] <- "A_reference"
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            prev_type:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "non_political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[prev_type == ref][["prev_type"]] <- "A_reference"
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            prev_type:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

tidy_toplot_access <- bind_rows(non_pol, pol) %>%
    mutate(
        header = "(B) Access",
        level = as.factor(level),
        level = dplyr::recode_factor(level,
            "direct" = "(B) Access: Direct",
            "facebook" = "(B) Access: Facebook",
            "twitter" = "(B) Access: Twitter",
            "search" = "(B) Access: Search engines",
            "portal" = "(B) Access: Portals"
        )
    )

## Political interest (Conditional Effects Analysis) ----
reference <- c("1", "4")
lm_dt$polinterest <- -1 * lm_dt$polinterest

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[polinterest == ref][["polinterest"]] <- 0
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            as.factor(polinterest):as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "non_political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[polinterest == ref][["polinterest"]] <- 0
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            as.factor(polinterest):as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

tidy_toplot_interest <- bind_rows(non_pol, pol) |>
    mutate(
        header = "(C) Political Interest",
        level = as.factor(level),
        level = dplyr::recode_factor(level,
            "1" = "(C) Political Interest: Low",
            "4" = "(C) Political Interest: High"
        )
    )

## Extremity(Conditional Effects Analysis) ----
# because We treat extremity as a continuous moderator, this requires a little bit different coding
# We use .2 as a reference level, because zero does not make so much sense (because it corresponds to a centrist ideology)
reference <- c(".2", "1")

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[["extreme"]] <- dat[["extreme"]] - as.numeric(ref)
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            extreme:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "non_political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[["extreme"]] <- dat[["extreme"]] - as.numeric(ref)
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            extreme:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})


tidy_toplot_extreme <- bind_rows(non_pol, pol) |>
    mutate(
        header = "(D) Political Extremity",
        level = as.factor(level),
        level = dplyr::recode_factor(level,
            ".2" = "(D) Political Extremity: Low",
            "1" = "(D) Political Extremity: High"
        )
    )

## Generation (Conditional Effects Analysis) ----
tidy_toplot <- data.frame()
reference <- c("30", "60")
lm_dt[, age := age * 35 + 30] # undo recode from before

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[["age"]] <- dat[["age"]] - as.numeric(ref)
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            age:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "non_political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[["age"]] <- dat[["age"]] - as.numeric(ref)
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            age:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

tidy_toplot_age <- bind_rows(non_pol, pol) |>
    mutate(
        header = "(E) Generation",
        level = as.factor(level),
        level = dplyr::recode_factor(level,
            "30" = "(E) Generation: Millennial",
            "60" = "(E) Generation: Boomer"
        )
    )

## Gender (Conditional Effects Analysis) ----
reference <- c("0", "1")

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[["female"]] <- dat[["female"]] - as.numeric(ref)
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            female:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "non_political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[["female"]] <- dat[["female"]] - as.numeric(ref)
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            female:as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})


tidy_toplot_female <- bind_rows(non_pol, pol) |>
    mutate(
        header = "(F) Gender",
        level = as.factor(level),
        level = dplyr::recode_factor(level,
            "0" = "(F) Gender: Male",
            "1" = "(F) Gender: Female"
        )
    )

## Education (Conditional Effects Analysis) ----
reference <- c("1", "3")

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[edu == ref][["edu"]] <- 0
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            as.factor(edu):as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "non_political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[edu == ref][["edu"]] <- 0
        res <- lmer(align ~ 1 + as.factor(leftright) + extreme +
            as.factor(edu):as.factor(leftright) + country + prev_type + as.factor(polinterest) + age +
            female + as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})


tidy_toplot_edu <- bind_rows(non_pol, pol) |>
    mutate(
        header = "(G) Education",
        level = as.factor(level),
        level = dplyr::recode_factor(level,
            "1" = "(G) Education: Low",
            "3" = "(G) Education: High"
        )
    )

# Build single concise plot
tidy_toplot_integrated <- bind_rows(
    tidy_toplot_country, tidy_toplot_access,
    tidy_toplot_interest, tidy_toplot_extreme,
    tidy_toplot_age, tidy_toplot_female, tidy_toplot_edu
)

level_order <- rev(levels(tidy_toplot_integrated$header))
tidy_toplot_integrated <- tidy_toplot_integrated %>%
    mutate(
        type = as.factor(type),
        type = dplyr::recode_factor(type,
            "non_political" = "Non-Political News",
            "political" = "Political News"
        )
    )

write_csv(tidy_toplot_integrated, paste0("processed_data/regression/", platform, "_conditional_effects.csv"))

## Plotting ----
### reduced ----
tidy_toplot_integrated <- read_csv(paste0("processed_data/regression/", platform, "_conditional_effects.csv"))
dat <- tidy_toplot_integrated |>
    mutate(level = str_replace_all(level, "\\(B\\) Access", "\\(C\\) News Access")) |>
    mutate(level = str_replace_all(level, "\\(C\\) Political Interest", "\\(B\\) Political Interest")) |>
    mutate(level = str_replace_all(level, "Direct", "Non-referred")) |>
    dplyr::filter(str_detect(level, "\\(A\\)|\\(B\\)|\\(C\\)")) |>
    mutate(level1 = str_remove_all(level, "\\(.*\\).*\\:\\s")) |>
    mutate(header = str_replace_all(header, "\\(B\\) Access", "\\(C\\) News Access")) |>
    mutate(header = str_replace_all(header, "\\(C\\) Political Interest", "\\(B\\) Political Interest"))

labels <- unique(dat$level1)
names(labels) <- labels
ggplot(dat, aes(y = Estimate, x = factor(threshold))) +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(A\\)"), ], shape = 15,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_shape_manual(values = c(
        15, 15, 15, 15, 15,
        16, 16, 16, 16,
        17, 17, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11
    ), name = "") +
    scale_color_manual(
        values = c(
            "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "grey25"
        ),
        name = "(A) Country",
        guide = guide_legend(title.position = "left", order = 1, nrow = 2)
    ) +
    ggnewscale::new_scale_color() +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(B\\)"), ], shape = 16,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c("#E69F00", "#009E73"),
        name = "(B) Political Interest",
        guide = guide_legend(title.position = "left", order = 2, nrow = 2)
    ) +
    ggnewscale::new_scale_color() +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(C\\)"), ], shape = 17,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
        name = "(C) News Access",
        guide = guide_legend(title.position = "left", order = 3, nrow = 2)
    ) +
    facet_grid(type ~ header, scales = "free_x") +
    coord_flip() +
    theme_bw() +
    theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal"
    ) +
    # legend.title = element_blank()) +
    labs(y = "", x = "threshold (in sec)") +
    geom_hline(yintercept = 0, linetype = "dashed")

ggsave(paste0("figures/", platform, "_regression_conditional_reduced.pdf"), width = 12, height = 7)

### full ----
tidy_toplot_integrated <- read_csv(paste0("processed_data/regression/", platform, "_conditional_effects.csv"))
dat <- tidy_toplot_integrated |>
    mutate(level = str_replace_all(level, "\\(B\\) Access", "\\(C\\) News Access")) |>
    mutate(level = str_replace_all(level, "\\(C\\) Political Interest", "\\(B\\) Political Interest")) |>
    mutate(level = str_replace_all(level, "Direct", "Non-referred")) |>
    mutate(level1 = str_remove_all(level, "\\(.*\\).*\\:\\s")) |>
    mutate(header = str_replace_all(header, "\\(B\\) Access", "\\(C\\) News Access")) |>
    mutate(header = str_replace_all(header, "\\(C\\) Political Interest", "\\(B\\) Political Interest"))

labels <- unique(dat$level1)
names(labels) <- labels
ggplot(dat, aes(y = Estimate, x = factor(threshold))) +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(A\\)"), ], shape = 15,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c(
            "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "grey25"
        ),
        name = "(A) Country",
        guide = guide_legend(title.position = "left", order = 1, nrow = 3)
    ) +
    ggnewscale::new_scale_color() +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(B\\)"), ], shape = 16,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c("#E69F00", "#009E73"),
        name = "(B) Political Interest",
        guide = guide_legend(title.position = "left", order = 2, nrow = 2)
    ) +
    ggnewscale::new_scale_color() +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(C\\)"), ], shape = 17,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
        name = "(C) News Access",
        guide = guide_legend(title.position = "left", order = 3, nrow = 3)
    ) +
    ggnewscale::new_scale_color() +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(D\\)"), ], shape = 7,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
        name = "(D) Political Extremity",
        guide = guide_legend(title.position = "left", order = 4, nrow = 2)
    ) +
    ggnewscale::new_scale_color() +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(E\\)"), ], shape = 8,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
        name = "(E) Generation",
        guide = guide_legend(title.position = "left", order = 5, nrow = 2)
    ) +
    ggnewscale::new_scale_color() +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(F\\)"), ], shape = 9,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
        name = "(F) Gender",
        guide = guide_legend(title.position = "left", order = 6, nrow = 2)
    ) +
    ggnewscale::new_scale_color() +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(G\\)"), ], shape = 10,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
        name = "(G) Education",
        guide = guide_legend(title.position = "left", order = 7, nrow = 2)
    ) +
    facet_grid(type ~ header, scales = "free_x") +
    coord_flip() +
    theme_bw() +
    theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal"
    ) +
    # legend.title = element_blank()) +
    labs(y = "", x = "threshold (in sec)") +
    geom_hline(yintercept = 0, linetype = "dashed")


ggsave(paste0("figures/", platform, "_regression_conditional.pdf"), width = 18, height = 10)

# Appendix ----

## Seg score comparison ----
bind_rows(
    readRDS(paste0("processed_data/stats/", platform, "_segregation_scores.RDS")) |>
        mutate(type = "Isolation index"),
    readRDS(paste0("processed_data/stats/", platform, "_dissimilarity_scores.RDS")) |>
        mutate(type = "Dissimilarity index"),
    readRDS(paste0("processed_data/stats/", platform, "_atkinson_scores.RDS")) |>
        mutate(type = "Atkinson scores")
) |>
    pivot_longer(cols = c(non_political, political)) |>
    mutate(name = ifelse(name == "political", "political news", "non-political news")) |>
    ggplot(aes(x = factor(cutoff), y = value, col = name, shape = name)) +
    geom_point(size=3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "transparent") +
    scale_color_manual(
        values = c(
            "political news" = "#AA8939",
            "non-political news" = "#303C74"
        ),
        labels = c("political news" = "Political news", "non-political news" = "Non-political news"), name = ""
    ) +
    scale_shape_manual(
        values = c("political news" = 16, "non-political news" = 17),
        labels = c("political news" = "Political news", "non-political news" = "Non-political news"), name = ""
    ) +
    facet_grid(type ~ case, scales = "free_y") +
    theme_bw() +
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(family = "sans", size = 12),
        axis.title = element_text(family = "sans", size = 18),
        strip.text = element_text(face = "bold"),
        text = element_text(family = "sans", size = 16)
    ) +
    labs(x = "threshold (in sec)", y = "score")

ggsave(paste0("figures/", platform, "_seg_score_comparison.pdf"), width = 16, height = 10)

## diversity comparison ----
bind_rows(
    readRDS(paste0("processed_data/stats/", platform, "_simpson_diversity.RDS")) |>
        mutate(type = "Simpson's D"),
    readRDS(paste0("processed_data/stats/", platform, "_entropy.RDS")) |>
        mutate(type = "Shannon's H")
) |>
    pivot_longer(cols = c(non_political, political)) |>
    mutate(name = ifelse(name == "political", "political news", "non-political news")) |>
    ggplot(aes(x = factor(cutoff), y = value, col = name, shape = name)) +
    geom_point(size=3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "transparent") +
    scale_color_manual(
        values = c(
            "political news" = "#AA8939",
            "non-political news" = "#303C74"
        ),
        labels = c("political news" = "Political news", "non-political news" = "Non-political news"), name = ""
    ) +
    scale_shape_manual(
        values = c("political news" = 16, "non-political news" = 17),
        labels = c("political news" = "Political news", "non-political news" = "Non-political news"), name = ""
    ) +
    facet_grid(type ~ case, scales = "free_y") +
    theme_bw() +
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(family = "sans", size = 12),
        strip.text = element_text(face = "bold"),
        text = element_text(family = "sans", size = 16)
    ) +
    labs(x = "threshold (in sec)", y = "score")

ggsave(paste0("figures/", platform, "_diversity_comparison.pdf"), width = 16, height = 10)

## news diet slant comparison ----
non_pol <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "" & duration >= y]

        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE)), by = .(domain)]
        dt[dom_align, on = .(domain), dom_align := align]
        dt1 <- dt[, .(diet_slant = mean(dom_align, na.rm = TRUE)), by = .(panelist_id)]
        mean(abs(dt1[["diet_slant"]] - mean_ideo))
    })
})

pol <- lapply(cutoffs, function(y) {
    sapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "political" & duration >= y]

        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE)), by = .(domain)]
        dt[dom_align, on = .(domain), dom_align := align]
        dt1 <- dt[, .(diet_slant = mean(dom_align, na.rm = TRUE)), by = .(panelist_id)]
        mean(abs(dt1[["diet_slant"]] - mean_ideo))
    })
})

combine_results(non_pol, pol) |>
    saveRDS(paste0("processed_data/stats/", platform, "_news_diet_slant_fletcher.RDS"))

flaxman <- readRDS(paste0("processed_data/stats/", platform, "_news_diet_slant.RDS")) |>
    mutate(meta = "(A) Flaxman et al.")
fletcher <- readRDS(paste0("processed_data/stats/", platform, "_news_diet_slant_fletcher.RDS")) |>
    mutate(meta = "(B) Fletcher et al.")

bind_rows(flaxman, fletcher) |>
    pivot_longer(non_political:political, names_to = "type", values_to = "score") |>
    ggplot(aes(x = factor(cutoff), y = score, col = type)) +
    geom_point() +
    scale_color_manual(
        values = c("political" = "#AA8939", "non_political" = "#303C74"),
        labels = c("Political news", "Non-political news"), name = ""
    ) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family = "sans", size = 20),
        axis.text.x = element_text(family = "sans", size = 12),
        strip.text = element_text(face = "bold"),
        text = element_text(family = "sans", size = 16)
    ) +
    labs(x = "cutoff (in sec)", y = "") +
    facet_grid(meta ~ case, scales = "free_y")

ggsave(paste0("figures/", platform, "_diet_slant_comparison.pdf"), width = 15, height = 8)

## alt scores regression ----
### prepare data ----
bakshy <- fread("data/bakshy_top500.txt")
bakshy[, domain := str_replace_all(domain, "www\\.", "")]
bakshy[, domain := fifelse(domain == "news.yahoo.com", "news.yahoo.com/NEWS", domain)]
bakshy[, domain := fifelse(domain == "news.msn.com", "msn.com/NEWS", domain)]
bakshy[, domain := fifelse(domain == "aol.com", "aol.com/NEWS", domain)]
bakshy[, domain := fifelse(domain == "huffingtonpost.com", "huffpost.com", domain)]
bakshy[, domain := fifelse(domain == "westernjournalism.com", "westernjournal.com", domain)]
bakshy <- bakshy[!domain %in% c("msn.com", "twitter.com", "amazon.com", "youtube.com")]
bakshy <- bakshy[, c("domain", "avg_align")]
robertson_data <- fread("data/bias_scores.csv")

survey_lm <- survey[country == "USA", .(panelist_id, leftright, polinterest, extremism, age, female, edu, log_total_visits)]
survey_lm <- na.omit(survey_lm)
survey_lm[, leftright := fcase(leftright < 6, -1, leftright == 6, 0, leftright > 6, 1)]

lm_dt <- fread(paste0("processed_data/", platform, "/news_only/", "us.csv"))
lm_dt[, country := "USA"]
lm_dt[, political := ifelse(political == "", "non-political", political)]
lm_dt <- lm_dt[!is.na(duration)]
lm_dt <- lm_dt[survey_lm, on = .(panelist_id)]
keep <- lm_dt[, .(max_visit = max(duration, na.rm = TRUE)), by = .(panelist_id)][max_visit >= 120][["panelist_id"]]
lm_dt <- lm_dt[panelist_id %in% keep]
overall_ideo <- mean(lm_dt[["leftright"]], na.rm = TRUE)

lm_dt <- bakshy[lm_dt, on = .(domain)]
lm_dt <- robertson_data[lm_dt, on = .(domain)]
tidy_toplot <- tibble()
### present data ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - overall_ideo, by = c("domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - overall_ideo, by = c("domain")]
    res <- lmer(align ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol, pol) |> mutate(meta = "(A) Present data")
tidy_toplot <- bind_rows(tidy_toplot, tmp)

### Bakshy et al ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    res <- lmer(avg_align ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    res <- lmer(avg_align ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol, pol) |> mutate(meta = "(B) Bakshy et al. scores")
tidy_toplot <- bind_rows(tidy_toplot, tmp)

### Robertson et al. ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    res <- lmer(score ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    res <- lmer(score ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol, pol) |> mutate(meta = "(C) Robertson et al. scores")
tidy_toplot <- bind_rows(tidy_toplot, tmp)

### Budak et al. ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, budak_score := budak_score * 5]
    res <- lmer(budak_score ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, budak_score := budak_score * 5]
    res <- lmer(budak_score ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol, pol) |> mutate(meta = "(D) Budak et al. scores")
tidy_toplot <- bind_rows(tidy_toplot, tmp)

### AllSides scores ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    res <- lmer(allsides_score ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    res <- lmer(allsides_score ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol, pol) |> mutate(meta = "(F) AllSides controlled scores")
tidy_toplot <- bind_rows(tidy_toplot, tmp)

### Allsides community ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    res <- lmer(allsides_score_community ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    res <- lmer(allsides_score_community ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol, pol) |> mutate(meta = "(E) AllSides community scores")
tidy_toplot <- bind_rows(tidy_toplot, tmp)

### PEW ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, pew_score := pew_score * 2.5]
    res <- lmer(pew_score ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, pew_score := pew_score * 2.5]
    res <- lmer(pew_score ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol, pol) |> mutate(meta = "(G) PEW scores")
tidy_toplot <- bind_rows(tidy_toplot, tmp)


### MTURK scores ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    res <- lmer(mturk_score ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "non_political", threshold = x)
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    res <- lmer(mturk_score ~ 1 + as.factor(leftright) + prev_type + polinterest + age + female +
        as.factor(edu) + log_total_visits + (1 | panelist_id), data = dat)
    lmer_to_tidy(res) |> mutate(type = "political", threshold = x)
})

tmp <- bind_rows(non_pol, pol) |> mutate(meta = "(H) MTurk scores")
tidy_toplot <- bind_rows(tidy_toplot, tmp)

write_csv(tidy_toplot, paste0("processed_data/regression/", platform, "_compare_other_scores.csv"))

### Plotting ----
tidy_toplot <- read_csv(paste0("processed_data/regression/", platform, "_compare_other_scores.csv"))
to_keep <- c("as.factor(leftright)1")

tidy_toplot <- tidy_toplot |>
    mutate(
        type = as.factor(type),
        type = dplyr::recode_factor(type,
            "non_political" = "Non-political News",
            "political" = "Political News"
        )
    ) |> 
    mutate(meta=str_replace_all(meta,"Bakshy et al.","Bakshy et al. [54]")) |> 
    mutate(meta=str_replace_all(meta,"Budak et al.","Budak et al. [43]")) |> 
    mutate(meta=str_replace_all(meta,"Robertson et al.","Robertson et al. [80]"))

tidy_toplot |>
    filter((term %in% to_keep)) |>
    ggplot(aes(y = Estimate, x = factor(threshold))) +
    geom_pointrange(
        aes(ymin = CI_lower, ymax = CI_upper, color = type,shape = type),
        position = position_dodge(0.6),size=1
    ) +
    scale_color_manual(
        values = c("Political News" = "#AA8939", "Non-political News" = "#303C74"),
        labels = c("Political News" = "Political news", "Non-political News" = "Non-political news"), name = ""
    ) +
    scale_shape_manual(
        values = c("Political News" = 16, "Non-political News" = 17),
        labels = c("Political News" = "Political news", "Non-political News" = "Non-political news"), name = ""
    ) +
    coord_flip() +
    theme_bw() +
    facet_wrap(~meta, scales = "free_x", nrow = 2) +
    theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 18)
    ) +
    labs(x = "threshold (in sec)", y = "estimate") +
    geom_hline(yintercept = 0, linetype = "dashed")

ggsave(paste0("figures/", platform, "_compare_other_scores.pdf"), width = 12, height = 8)

## alt scores partisanship ----
### diversity ----
non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - overall_ideo, by = c("domain")]
    dat[, `:=`(
        ideo_cat_A = fcase(is.na(align), NA_real_, align < -.2, -1, align > .2, 1, default = 0),
        ideo_cat_B = fcase(is.na(avg_align), NA_real_, avg_align < -.2, -1, avg_align > .2, 1, default = 0),
        ideo_cat_C = fcase(is.na(score), NA_real_, score < -.2, -1, score > .2, 1, default = 0),
        ideo_cat_D = fcase(is.na(budak_score), NA_real_, budak_score < -.04, -1, budak_score > .04, 1, default = 0),
        ideo_cat_E = fcase(is.na(allsides_score_community), NA_real_, allsides_score_community < -.2, -1, allsides_score_community > .2, 1, default = 0),
        ideo_cat_F = fcase(is.na(allsides_score), NA_real_, allsides_score < -.2, -1, allsides_score > .2, 1, default = 0),
        ideo_cat_G = fcase(is.na(pew_score), NA_real_, pew_score < -.2 / 2.5, -1, pew_score > .2 / 2.5, 1, default = 0),
        ideo_cat_H = fcase(is.na(mturk_score), NA_real_, mturk_score < -.2, -1, mturk_score > .2, 1, default = 0)
    )]

    datA1 <- dat[!is.na(ideo_cat_A), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_A)]
    datB1 <- dat[!is.na(ideo_cat_B), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_B)]
    datC1 <- dat[!is.na(ideo_cat_C), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_C)]
    datD1 <- dat[!is.na(ideo_cat_D), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_D)]
    datE1 <- dat[!is.na(ideo_cat_E), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_E)]
    datF1 <- dat[!is.na(ideo_cat_F), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_F)]
    datG1 <- dat[!is.na(ideo_cat_G), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_G)]
    datH1 <- dat[!is.na(ideo_cat_H), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_H)]

    datA2 <- datA1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datB2 <- datB1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datC2 <- datC1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datD2 <- datD1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datE2 <- datE1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datF2 <- datF1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datG2 <- datG1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datH2 <- datH1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]

    tibble(
        country = "USA",
        cutoff = x,
        type = "non-political",
        score = c(
            1.5 * mean(datA2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datB2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datC2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datD2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datE2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datF2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datG2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datH2[["ideo_div"]], na.rm = TRUE)
        ),
        meta = c(
            "(A) Present data", "(B) Bakshy et al. scores", "(C) Robertson et al. scores",
            "(D) Budak et al. scores", "(E) AllSides community scores",
            "(F) AllSides controlled scores", "(G) PEW scores", "(H) MTurk scores"
        )
    )
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - overall_ideo, by = c("domain")]
    dat[, `:=`(
        ideo_cat_A = fcase(is.na(align), NA_real_, align < -.2, -1, align > .2, 1, default = 0),
        ideo_cat_B = fcase(is.na(avg_align), NA_real_, avg_align < -.2, -1, avg_align > .2, 1, default = 0),
        ideo_cat_C = fcase(is.na(score), NA_real_, score < -.2, -1, score > .2, 1, default = 0),
        ideo_cat_D = fcase(is.na(budak_score), NA_real_, budak_score < -.04, -1, budak_score > .04, 1, default = 0),
        ideo_cat_E = fcase(is.na(allsides_score_community), NA_real_, allsides_score_community < -.2, -1, allsides_score_community > .2, 1, default = 0),
        ideo_cat_F = fcase(is.na(allsides_score), NA_real_, allsides_score < -.2, -1, allsides_score > .2, 1, default = 0),
        ideo_cat_G = fcase(is.na(pew_score), NA_real_, pew_score < -.2 / 2.5, -1, pew_score > .2 / 2.5, 1, default = 0),
        ideo_cat_H = fcase(is.na(mturk_score), NA_real_, mturk_score < -.2, -1, mturk_score > .2, 1, default = 0)
    )]

    datA1 <- dat[!is.na(ideo_cat_A), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_A)]
    datB1 <- dat[!is.na(ideo_cat_B), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_B)]
    datC1 <- dat[!is.na(ideo_cat_C), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_C)]
    datD1 <- dat[!is.na(ideo_cat_D), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_D)]
    datE1 <- dat[!is.na(ideo_cat_E), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_E)]
    datF1 <- dat[!is.na(ideo_cat_F), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_F)]
    datG1 <- dat[!is.na(ideo_cat_G), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_G)]
    datH1 <- dat[!is.na(ideo_cat_H), .(visit_by_ideo = .N), by = .(panelist_id, ideo_cat_H)]

    datA2 <- datA1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datB2 <- datB1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datC2 <- datC1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datD2 <- datD1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datE2 <- datE1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datF2 <- datF1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datG2 <- datG1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]
    datH2 <- datH1[, .(ideo_div = vegan::diversity(visit_by_ideo, index = "simpson")), by = .(panelist_id)]

    tibble(
        country = "USA",
        cutoff = x,
        type = "political",
        score = c(
            1.5 * mean(datA2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datB2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datC2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datD2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datE2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datF2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datG2[["ideo_div"]], na.rm = TRUE),
            1.5 * mean(datH2[["ideo_div"]], na.rm = TRUE)
        ),
        meta = c(
            "(A) Present data", "(B) Bakshy et al. scores", "(C) Robertson et al. scores",
            "(D) Budak et al. scores", "(E) AllSides community scores",
            "(F) AllSides controlled scores", "(G) PEW scores", "(H) MTurk scores"
        )
    )
})

sum_stat_int <- bind_rows(non_pol, pol) |> mutate(meta2 = "(A) News Diet Diversity")
saveRDS(sum_stat_int, paste0("processed_data/stats/", platform, "_diversity_other_scores.RDS"))


### partisanship ----
non_pol <- map_dfr(cutoffs, function(y) {
    dat <- lm_dt
    if (fixN) {
        peeps <- dat[duration >= 120]
        peeps <- unique(peeps[["panelist_id"]])
        dat <- dat[panelist_id %in% peeps]
    }
    dat <- dat[political == "non-political" & duration >= y]
    dom_align <- dat[, .(
        align_A = mean(leftright, na.rm = TRUE) - overall_ideo,
        align_B = mean(avg_align, na.rm = TRUE),
        align_C = mean(score, na.rm = TRUE),
        align_D = mean(5 * budak_score, na.rm = TRUE),
        align_E = mean(allsides_score_community, na.rm = TRUE),
        align_F = mean(allsides_score, na.rm = TRUE),
        align_G = mean(8 / 3 * pew_score, na.rm = TRUE),
        align_H = mean(mturk_score, na.rm = TRUE)
    ),
    by = .(domain)
    ]

    dat <- dat[dom_align, on = .(domain)]
    dat1 <- dat[, .(
        diet_slant_A = mean(align_A, na.rm = TRUE),
        diet_slant_B = mean(align_B, na.rm = TRUE),
        diet_slant_C = mean(align_C, na.rm = TRUE),
        diet_slant_D = mean(align_D, na.rm = TRUE),
        diet_slant_E = mean(align_E, na.rm = TRUE),
        diet_slant_F = mean(align_F, na.rm = TRUE),
        diet_slant_G = mean(align_G, na.rm = TRUE),
        diet_slant_H = mean(align_H, na.rm = TRUE)
    ),
    by = .(panelist_id)
    ]
    tibble(
        country = "USA",
        cutoff = y,
        type = "non-political",
        score = c(
            sd(dat1$diet_slant_A, na.rm = TRUE),
            sd(dat1$diet_slant_B, na.rm = TRUE),
            sd(dat1$diet_slant_C, na.rm = TRUE),
            sd(dat1$diet_slant_D, na.rm = TRUE),
            sd(dat1$diet_slant_E, na.rm = TRUE),
            sd(dat1$diet_slant_F, na.rm = TRUE),
            sd(dat1$diet_slant_G, na.rm = TRUE),
            sd(dat1$diet_slant_H, na.rm = TRUE)
        ),
        meta = c(
            "(A) Present data", "(B) Bakshy et al. scores", "(C) Robertson et al. scores",
            "(D) Budak et al. scores", "(E) AllSides community scores",
            "(F) AllSides controlled scores", "(G) PEW scores", "(H) MTurk scores"
        )
    )
})

pol <- map_dfr(cutoffs, function(y) {
    dat <- lm_dt
    if (fixN) {
        peeps <- dat[duration >= 120]
        peeps <- unique(peeps[["panelist_id"]])
        dat <- dat[panelist_id %in% peeps]
    }
    dat <- dat[political == "political" & duration >= y]
    dom_align <- dat[, .(
        align_A = mean(leftright, na.rm = TRUE) - overall_ideo,
        align_B = mean(avg_align, na.rm = TRUE),
        align_C = mean(score, na.rm = TRUE),
        align_D = mean(5 * budak_score, na.rm = TRUE),
        align_E = mean(allsides_score_community, na.rm = TRUE),
        align_F = mean(allsides_score, na.rm = TRUE),
        align_G = mean(8 / 3 * pew_score, na.rm = TRUE),
        align_H = mean(mturk_score, na.rm = TRUE)
    ),
    by = .(domain)
    ]

    dat <- dat[dom_align, on = .(domain)]
    dat1 <- dat[, .(
        diet_slant_A = mean(align_A, na.rm = TRUE),
        diet_slant_B = mean(align_B, na.rm = TRUE),
        diet_slant_C = mean(align_C, na.rm = TRUE),
        diet_slant_D = mean(align_D, na.rm = TRUE),
        diet_slant_E = mean(align_E, na.rm = TRUE),
        diet_slant_F = mean(align_F, na.rm = TRUE),
        diet_slant_G = mean(align_G, na.rm = TRUE),
        diet_slant_H = mean(align_H, na.rm = TRUE)
    ),
    by = .(panelist_id)
    ]
    tibble(
        country = "USA",
        cutoff = y,
        type = "political",
        score = c(
            sd(dat1$diet_slant_A, na.rm = TRUE),
            sd(dat1$diet_slant_B, na.rm = TRUE),
            sd(dat1$diet_slant_C, na.rm = TRUE),
            sd(dat1$diet_slant_D, na.rm = TRUE),
            sd(dat1$diet_slant_E, na.rm = TRUE),
            sd(dat1$diet_slant_F, na.rm = TRUE),
            sd(dat1$diet_slant_G, na.rm = TRUE),
            sd(dat1$diet_slant_H, na.rm = TRUE)
        ),
        meta = c(
            "(A) Present data", "(B) Bakshy et al. scores", "(C) Robertson et al. scores",
            "(D) Budak et al. scores", "(E) AllSides community scores",
            "(F) AllSides controlled scores", "(G) PEW scores", "(H) MTurk scores"
        )
    )
})

sum_stat_int <- bind_rows(non_pol, pol) |> mutate(meta2 = "(B) Partisanship in News Diets")
saveRDS(sum_stat_int, paste0("processed_data/stats/", platform, "_partisan_other_scores.RDS"))

### plotting ----
sum_stat_partisan <- readRDS(paste0("processed_data/stats/", platform, "_partisan_other_scores.RDS"))
sum_stat_diverse <- readRDS(paste0("processed_data/stats/", platform, "_diversity_other_scores.RDS"))

summary_scores <- bind_rows(sum_stat_diverse, sum_stat_partisan) |>
    mutate(
        type = as.factor(type),
        type = dplyr::recode_factor(type,
            "non-political" = "Non-political News",
            "political" = "Political News"
        )
    ) |> 
    mutate(meta=str_replace_all(meta,"Bakshy et al.","Bakshy et al. [54]")) |> 
    mutate(meta=str_replace_all(meta,"Budak et al.","Budak et al. [43]")) |> 
    mutate(meta=str_replace_all(meta,"Robertson et al.","Robertson et al. [80]"))

summary_scores$meta2 <- ifelse(
    grepl("News Diet Div",summary_scores$meta2),
    "(A) Simpson's D", "(B) News Diet Slant (SD)"
)

ggplot(summary_scores, aes(y = score, x = factor(cutoff))) +
    geom_point(
        aes(color = type,shape=type), size = 3
    ) +
    scale_color_manual(
        values = c("Political News" = "#AA8939", "Non-political News" = "#303C74"),
        labels = c("Political News" = "Political news", "Non-political News" = "Non-political news"), name = ""
    ) +
    scale_shape_manual(
        values = c("Political News" = 16, "Non-political News" = 17),
        labels = c("Political News" = "Political news", "Non-political News" = "Non-political news"), name = ""
    ) +
    theme_bw() +
    facet_grid(meta2 ~ meta, scales = "free_y") +
    theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        legend.position = "bottom",
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 18)
    ) +
    ylim(0, .6) +
    labs(x = "threshold (in sec)", y = "score")

ggsave(paste0("figures/", platform, "_divpart_other_scores.pdf"), width = 18, height = 8)

## prevalences ----
system("Rscripts/freq_count.sh")

### news proportion all visits ----
fl <- list.files("processed_data/stats", patter = "type_freq", full.names = TRUE)
map_dfr(fl, function(f) {
    df <- read_csv(f, show_col_types = FALSE)
    df$country <- long_cases[which(fl == f)]
    df
}) |>
    group_by(country) |>
    dplyr::summarise(news = count[value == "news"], all = sum(count)) |>
    mutate(frac = round(news / all * 100, 2), all = format(all, big.mark = ",")) |>
    select(country, all, news = frac) |>
    knitr::kable(format = "latex", booktabs = TRUE)

### pol non pol visits  ----
fl <- list.files(paste0("processed_data/", platform, "/news_only"), pattern = "csv")

stats <- map_dfr(fl, function(x) {
    dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
    map_dfr(cutoffs, function(y) {
        tibble(
            country = long_cases[short_cases == str_sub(x, 1, 2)],
            cutoff = y,
            non_political = nrow(dt[political == "" & duration >= y]),
            political = nrow(dt[political == "political" & duration >= y])
        )
    })
})

stats |>
    mutate(
        all = non_political + political,
        frac_pol = political / all
    ) |>
    dplyr::filter(country == "United Kingdom") |>
    mutate(frac_all = all / all[1]) |>
    select(-country)

### all visitors/news visitors ----
fl <- list.files("processed_data/stats/", pattern = "uniq")
as.numeric(sapply(paste0("processed_data/stats/", short_cases, "_uniq_panelists_news.csv"), readLines)) /
    as.numeric(sapply(paste0("processed_data/stats/", short_cases, "_uniq_panelists.csv"), readLines))

## misc descriptives ----
survey[, leftright := fcase(leftright < 6, -1, leftright == 6, 0, leftright > 6, 1)]
survey[, .(
    mean = mean(leftright, na.rm = TRUE),
    sd = sd(leftright, na.rm = TRUE),
    min = min(leftright, na.rm = TRUE),
    max = max(leftright, na.rm = TRUE),
    N = sum(!is.na(leftright))
), by = .(country)]

fl <- list.files(paste0("processed_data/", platform), pattern = "csv")
visits <- map(fl, function(x) {
    dt <- data.table::fread(paste0("processed_data/", platform, "/", x), select = c(1, 3, 5, 7, 8))
    dt_sum <- dt[, .(
        news_visits = sum(type == "news"),
        pol_visits = sum(political == "political"),
        outlets = length(unique(domain[type == "news"])),
        fb_visits = sum(type == "facebook"),
        twitter_visits = sum(type == "twitter"),
        search_visits = sum(type == "search"),
        portal_visits = sum(type == "portal")
    ), by = .(panelist_id)]
    totals <- colSums(dt_sum[, -1])
    rbind(
        apply(dt_sum[, -1], 2, summary)[c(1, 3, 4, 6), ],
        totals
    )
})
names(visits) <- long_cases
saveRDS(visits, paste0("processed_data/stats/", platform, "_visit_stats.RDS"))

## correlation with existing scores ----
survey_data <- read_rds("data/survey_data_r.rds") %>%
    filter(country == "USA") %>%
    select(panelist_id, leftright, polinterest, extremism, age, female, edu, log_total_visits) %>%
    na.omit() %>%
    mutate(leftright = case_when(leftright < 6 ~ -1, leftright >= 6 & leftright <= 6 ~ 0, leftright > 6 ~ 1))

us <- read_csv(paste0("processed_data/", platform, "/news_only/us.csv")) %>% mutate(country = "USA")
us$political[us$political == ""] <- "non-political"
us <- us %>% left_join(survey_data, by = "panelist_id")
us <- us %>% filter(duration > 10)
overall_ideo <- mean(us$leftright, na.rm = TRUE)

robertson_data <- read.csv("data/bias_scores.csv") %>% mutate(domain = gsub("www.", "", domain))
us <- us %>% left_join(robertson_data, by = "domain")

# Bakshy et al. scores
k <- us %>%
    group_by(domain) %>%
    mutate(n_visit = n_distinct(panelist_id)) %>%
    filter(n_visit > 30)
k <- k %>%
    group_by(domain) %>%
    summarise(align = mean(leftright, na.rm = TRUE) - overall_ideo, avg_align = mean(fb_score, na.rm = TRUE), n_pop = n())
k$meta <- "(B) Bakshy et al. scores (r = .80)"

# Robertson et al. scores
k1 <- us %>%
    group_by(domain) %>%
    mutate(n_visit = n_distinct(panelist_id)) %>%
    filter(n_visit > 30)
k1 <- k1 %>%
    group_by(domain) %>%
    summarise(align = mean(leftright, na.rm = TRUE) - overall_ideo, avg_align = mean(score, na.rm = TRUE), n_pop = n())
k1$meta <- "(C) Robertson et al. scores (r = .64)"

# Budak et al. scores
k2 <- us %>%
    group_by(domain) %>%
    mutate(n_visit = n_distinct(panelist_id)) %>%
    filter(n_visit > 30)
k2 <- k2 %>% mutate(budak_score = budak_score * 5)
k2 <- k2 %>%
    group_by(domain) %>%
    summarise(align = mean(leftright, na.rm = TRUE) - overall_ideo, avg_align = mean(budak_score, na.rm = TRUE), n_pop = n())
k2$meta <- "(D) Budak et al. scores (r = .94)"

# allsides_score
k3 <- us %>%
    group_by(domain) %>%
    mutate(n_visit = n_distinct(panelist_id)) %>%
    filter(n_visit > 30)
k3 <- k3 %>%
    group_by(domain) %>%
    summarise(align = mean(leftright, na.rm = TRUE) - overall_ideo, avg_align = mean(allsides_score_community, na.rm = TRUE), n_pop = n())
k3$meta <- "(E) AllSides community scores (r = .93)"

# allsides_score_community
k4 <- us %>%
    group_by(domain) %>%
    mutate(n_visit = n_distinct(panelist_id)) %>%
    filter(n_visit > 30)
k4 <- k4 %>%
    group_by(domain) %>%
    summarise(align = mean(leftright, na.rm = TRUE) - overall_ideo, avg_align = mean(allsides_score, na.rm = TRUE), n_pop = n())
k4$meta <- "(F) AllSides controlled scores (r = .82)"

# pew_score
k6 <- us %>%
    group_by(domain) %>%
    mutate(n_visit = n_distinct(panelist_id)) %>%
    filter(n_visit > 30)
k6 <- k6 %>% mutate(pew_score = pew_score * (8 / 3) - 0.11)
k6 <- k6 %>%
    group_by(domain) %>%
    summarise(align = mean(leftright, na.rm = TRUE) - overall_ideo, avg_align = mean(pew_score, na.rm = TRUE), n_pop = n())
k6$meta <- "(G) PEW scores (r = .83)"

# mturk_scores
k5 <- us %>%
    group_by(domain) %>%
    mutate(n_visit = n_distinct(panelist_id)) %>%
    filter(n_visit > 30)
k5 <- k5 %>%
    group_by(domain) %>%
    summarise(align = mean(leftright, na.rm = TRUE) - overall_ideo, avg_align = mean(mturk_score, na.rm = TRUE), n_pop = n())
k5$meta <- "(H) Mturk scores (r = .72)"

# Build an integrated graph
k_int <- rbind(k, k1, k2, k3, k4, k5, k6)

ggplot(k_int, aes(x = avg_align, y = align, size = (n_pop^.8))) +
    geom_point() +
    geom_smooth(method = "glm", formula = y ~ x) +
    geom_text(label = k_int$domain, hjust = 0, nudge_x = 0.1, alpha = .3) +
    geom_point(
        color = "cornflowerblue",
        alpha = .85
    ) +
    theme_bw() +
    scale_x_continuous(
        limits = c(-1., 1.3)
    ) +
    scale_y_continuous(
        limits = c(-1., 1.)
    ) +
    scale_size(range = c(1, 8)) +
    facet_wrap(~meta, nrow = 4, scales = "free_y") +
    theme( # axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        legend.position = "none",
        legend.text = element_blank(),
        legend.title = element_blank()
    ) +
    xlab("(B-H) Reference scores") +
    ylab("(A) Present data")

ggsave(paste0("figures/", platform, "_align_comparison.pdf"), width = 10, height = 16)


# Density plots (new Figure 3) ----
cutoffs <- c(3,120)
non_pol <- lapply(cutoffs, function(y) {
    lapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "" & duration >= y]

        # calculate the ideological slant of the individual participants news diets
        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo), by = .(domain)]
        dt[dom_align, on = .(domain), dom_align := align]
        dt1 <- dt[, .(diet_slant = mean(dom_align, na.rm = TRUE),leftright = mean(leftright,na.rm = TRUE)), by = .(panelist_id)]
        dt1[,`:=`(political = "Non-political news",cutoff=y,country=x)]
        dt1
    })
})

pol <- lapply(cutoffs, function(y) {
    lapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "political" & duration >= y]

        # calculate the ideological slant of the individual participants news diets
        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo), by = .(domain)]
        dt[dom_align, on = .(domain), dom_align := align]
        dt1 <- dt[, .(diet_slant = mean(dom_align, na.rm = TRUE),leftright = mean(leftright,na.rm = TRUE)), by = .(panelist_id)]
        dt1[,`:=`(political = "Political news",cutoff=y,country=x)]
        dt1
    })
})

res <- rbind(
    rbind(rbindlist(non_pol[[1]]),rbindlist(non_pol[[2]])),
    rbind(rbindlist(pol[[1]]),rbindlist(pol[[2]]))
)

res[,country:=long_cases[match(str_remove(country,"\\.csv"),short_cases)]]
saveRDS(res,paste0("processed_data/stats/", platform, "density_plots.RDS"))

## plot
dat <- readRDS(paste0("processed_data/stats/", platform, "density_plots.RDS"))

outlets <- data.table(
    domain = c("Spiegel", "Focus","El Peridodico","El Mundo","Le Monde", "Le Figaro","Repubblica","Mediaset","Gurdian","Daily Mail", "CNN", "Fox News"),
    country= rep(long_cases,each = 2),
    x = c(-0.41,0.39,-0.27,0.3,-0.4,0.2,-0.38,0.31,-0.43,0.43,-0.26,0.81),
    y = 0
)

ggplot() + 
    geom_density(data = dat[leftright!=0 & cutoff == 120],aes(x=diet_slant,color = as.factor(leftright),fill=as.factor(leftright)), alpha = 0.7) + 
    geom_label(data=outlets,aes(x=x,y=y,label=domain),vjust = 0,size=3)+
    scale_fill_manual(values = c("red","blue"),name = "", labels = c("Conservative","Liberal"))+
    scale_colour_manual(values = c("red","blue"),name = "", labels = c("Conservative","Liberal"))+
    facet_grid(country~political,scale = "free_y")+
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
    labs(x="",y="") 

ggsave(paste0("figures/", platform, "_density_plot_main.pdf"), height = 16, width = 10)

ggplot() + 
    geom_density(data = dat[leftright!=0 & cutoff == 3],aes(x=diet_slant,color = as.factor(leftright),fill=as.factor(leftright)), alpha = 0.7) + 
    geom_label(data=outlets,aes(x=x,y=y,label=domain),vjust = 0,size=3)+
    scale_fill_manual(values = c("red","blue"),name = "", labels = c("Conservative","Liberal"))+
    scale_colour_manual(values = c("red","blue"),name = "", labels = c("Conservative","Liberal"))+
    facet_grid(country~political,scale = "free_y")+
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
    labs(x="",y="") 

ggsave(paste0("figures/", platform, "_density_plot_appendix.pdf"), height = 16, width = 10)

# density with alternative scores ----
bakshy <- fread("data/bakshy_top500.txt")
bakshy[, domain := str_replace_all(domain, "www\\.", "")]
bakshy[, domain := fifelse(domain == "news.yahoo.com", "news.yahoo.com/NEWS", domain)]
bakshy[, domain := fifelse(domain == "news.msn.com", "msn.com/NEWS", domain)]
bakshy[, domain := fifelse(domain == "aol.com", "aol.com/NEWS", domain)]
bakshy[, domain := fifelse(domain == "huffingtonpost.com", "huffpost.com", domain)]
bakshy[, domain := fifelse(domain == "westernjournalism.com", "westernjournal.com", domain)]
bakshy <- bakshy[!domain %in% c("msn.com", "twitter.com", "amazon.com", "youtube.com")]
bakshy <- bakshy[, c("domain", "avg_align")]
robertson_data <- fread("data/bias_scores.csv")
us <- data.table::fread(paste0("processed_data/", platform, "/news_only/", "us.csv"))
us[survey, on = .(panelist_id), leftright := leftright]
us[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
us <- us[duration>=120 & !is.na(leftright)]
overall_ideo <- mean(us[["leftright"]], na.rm = TRUE)

current <- readRDS(paste0("processed_data/stats/", platform, "density_plots.RDS"))[cutoff==120 & country =="USA"][,c("panelist_id","diet_slant","leftright","political")]

us <- bakshy[us, on = .(domain)]
us <- robertson_data[us, on = .(domain)]
dat <- us[,.(robertson = mean(score,na.rm=TRUE),
      bakshy = mean(fb_score,na.rm = TRUE),
      budak = mean(budak_score, na.rm = FALSE)*5,
      allside_com = mean(allsides_score_community, na.rm=FALSE),
      allside = mean(allsides_score, na.rm=FALSE),
      pew = mean(pew_score, na.rm=FALSE),
      mturk = mean(mturk_score, na.rm=FALSE)
        ),
    by=.(panelist_id,political)]

dat <- dat[current, on = .(panelist_id)]
dat[,i.political:=NULL]
dat_melt <- melt(dat,id.vars=c("panelist_id","political","leftright"))
dat_melt[,political:=fifelse(political=="","Non-political news","Political news")]
dat_melt <- dat_melt[!is.na(value)]

dat_melt[, variable:=fcase(
    variable=="diet_slant","present data",
    variable=="robertson","Robertson et al. [80] scores",
    variable=="bakshy","Bakshy et al. [54] scores",
    variable=="budak","Budak et al. [43] scores",
    variable=="allside_com","Allside community scores",
    variable=="allside","Allside controlled scores",
    variable=="robertson","Robertson et al. [80] scores",
    variable=="pew","PEW scores",
    variable=="mturk","Mturk scores"
)]
levs <- unique(dat_melt$variable)[c(8,2,1,3,4,5,6,7)]
dat_melt[, variable:=factor(variable,levels = levs)]

outlets <- data.table(
    variable = rep(c("present data", "Bakshy et al. [54] scores", "Robertson et al. [80] scores", 
"Budak et al. [43] scores", "Allside community scores", "Allside controlled scores", 
"PEW scores", "Mturk scores"), each = 2),
    domain = rep(c("CNN","Fox News"),8),
    value = c(-0.26,0.81,-0.27,0.78,-0.11,0.61,-0.03*5,0.13*5,-0.5,1.0,-0,0.5,-0.22,0.42,-0.66,0.25),
    y=0
)

outlets$variable <- factor(outlets$variable,levels=levs)

ggplot() + 
    geom_density(data = dat_melt[leftright!=0],aes(x=value,color = as.factor(leftright),fill=as.factor(leftright)), alpha = 0.7) + 
    geom_label(data=outlets,aes(x=value,y=y,label=domain),vjust = 0,size=3)+
    scale_fill_manual(values = c("red","blue"),name = "", labels = c("Conservative","Liberal"))+
    scale_colour_manual(values = c("red","blue"),name = "", labels = c("Conservative","Liberal"))+
    facet_grid(variable~political,scale = "free_y")+
    theme_bw() +
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(family = "sans", size = 14),
        axis.title = element_text(family = "sans", size = 20),
        strip.text = element_text(face = "bold",size = 8),
        text = element_text(family = "sans", size = 16)
    ) +
    labs(x="",y="") 

ggsave(paste0("figures/", platform, "_density_plot_altscore.pdf"), height = 16, width = 10)

## Regression less control

# Prepare Regression Data ----
# prepare Data
survey_lm <- survey[, .(panelist_id, leftright, polinterest, extremism, age, female, edu, log_total_visits)]
survey_lm <- na.omit(survey_lm)
survey_lm[, extreme := (abs(leftright - 6) / 5)]
survey_lm[, leftright := fcase(leftright < 6, -1, leftright == 6, 0, leftright > 6, 1)]
survey_lm[, polinterest := polinterest * -1]
survey_lm[, age := (age - 30) / 35]

lm_dt <- lapply(seq_along(fl), function(i) {
    dt <- fread(paste0("processed_data/", platform, "/news_only/", fl[i]))
    dt[, country := long_cases[i]]
    dt[, prev_type := fcase(
        prev_type == "direct", "direct",
        prev_type == "ebay", "direct",
        prev_type == "news", "direct",
        prev_type == "other", "direct",
        prev_type == "facebook", "facebook",
        prev_type == "twitter", "twitter",
        prev_type == "search", "search",
        prev_type == "portal", "portal"
    )]
    dt[, prev_type := as.factor(prev_type)]
    dt[, political := ifelse(political == "", "non-political", political)]
}) |> rbindlist()

lm_dt <- lm_dt[!is.na(duration)]
lm_dt <- lm_dt[survey_lm, on = .(panelist_id)]
keep <- lm_dt[, .(max_visit = max(duration, na.rm = TRUE)), by = .(panelist_id)][max_visit >= 120][["panelist_id"]]
lm_dt <- lm_dt[panelist_id %in% keep]
lm_dt[, ideo_over := mean(leftright, na.rm = TRUE), by = country]

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(long_cases, function(ref) {
        dat[country == ref][["country"]] <- "A_reference"
        res <- lmer(align ~ 1 + as.factor(leftright) + 
            country:as.factor(leftright) + country + prev_type + as.factor(polinterest) + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "non_political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(long_cases, function(ref) {
        dat[country == ref][["country"]] <- "A_reference"
        res <- lmer(align ~ 1 + as.factor(leftright) + 
            country:as.factor(leftright) + country + prev_type + as.factor(polinterest) + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

tidy_toplot_country <- bind_rows(non_pol, pol) %>%
    mutate(
        header = "(A) Country",
        level = as.factor(level),
        level = dplyr::recode_factor(level,
            "France" = "(A) Country: France",
            "Germany" = "(A) Country: Germany",
            "Italy" = "(A) Country: Italy",
            "Spain" = "(A) Country: Spain",
            "United Kingdom" = "(A) Country: United Kingdom",
            "USA" = "(A) Country: USA"
        )
    )


## Access (Conditional Effects Analysis)----
reference <- c("direct", "facebook", "twitter", "search", "portal")
lm_dt$prev_type <- as.character(lm_dt$prev_type)

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[prev_type == ref][["prev_type"]] <- "A_reference"
        res <- lmer(align ~ 1 + as.factor(leftright) + 
            prev_type:as.factor(leftright) + country + prev_type + as.factor(polinterest) + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "non_political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[prev_type == ref][["prev_type"]] <- "A_reference"
        res <- lmer(align ~ 1 + as.factor(leftright) + 
            prev_type:as.factor(leftright) + country + prev_type + as.factor(polinterest) + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

tidy_toplot_access <- bind_rows(non_pol, pol) %>%
    mutate(
        header = "(B) Access",
        level = as.factor(level),
        level = dplyr::recode_factor(level,
            "direct" = "(B) Access: Direct",
            "facebook" = "(B) Access: Facebook",
            "twitter" = "(B) Access: Twitter",
            "search" = "(B) Access: Search engines",
            "portal" = "(B) Access: Portals"
        )
    )

## Political interest (Conditional Effects Analysis) ----
reference <- c("1", "4")
lm_dt$polinterest <- -1 * lm_dt$polinterest

non_pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "non-political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[polinterest == ref][["polinterest"]] <- 0
        res <- lmer(align ~ 1 + as.factor(leftright) + 
            as.factor(polinterest):as.factor(leftright) + country + prev_type + as.factor(polinterest) + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "non_political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

pol <- map_dfr(cutoffs, function(x) {
    dat <- lm_dt[political == "political" & duration >= x]
    dat[, align := mean(leftright, na.rm = TRUE) - ideo_over, by = c("country", "domain")]
    map_dfr(reference, function(ref) {
        dat[polinterest == ref][["polinterest"]] <- 0
        res <- lmer(align ~ 1 + as.factor(leftright) + 
            as.factor(polinterest):as.factor(leftright) + country + prev_type + as.factor(polinterest) + (1 | panelist_id), data = dat)
        lmer_to_tidy(res) |>
            mutate(type = "political", threshold = x, level = ref) |>
            dplyr::filter(term == "as.factor(leftright)1")
    })
})

tidy_toplot_interest <- bind_rows(non_pol, pol) |>
    mutate(
        header = "(C) Political Interest",
        level = as.factor(level),
        level = dplyr::recode_factor(level,
            "1" = "(C) Political Interest: Low",
            "4" = "(C) Political Interest: High"
        )
    )

# Build single concise plot
tidy_toplot_integrated <- bind_rows(
    tidy_toplot_country, tidy_toplot_access,
    tidy_toplot_interest
)

level_order <- rev(levels(tidy_toplot_integrated$header))
tidy_toplot_integrated <- tidy_toplot_integrated %>%
    mutate(
        type = as.factor(type),
        type = dplyr::recode_factor(type,
            "non_political" = "Non-Political News",
            "political" = "Political News"
        )
    )

write_csv(tidy_toplot_integrated, paste0("processed_data/regression/", platform, "_conditional_effects_no_control.csv"))

## Plotting ----
### reduced ----
tidy_toplot_integrated <- read_csv(paste0("processed_data/regression/", platform, "_conditional_effects_no_control.csv"))
dat <- tidy_toplot_integrated |>
    mutate(level = str_replace_all(level, "\\(B\\) Access", "\\(C\\) News Access")) |>
    mutate(level = str_replace_all(level, "\\(C\\) Political Interest", "\\(B\\) Political Interest")) |>
    mutate(level = str_replace_all(level, "Direct", "Non-referred")) |>
    dplyr::filter(str_detect(level, "\\(A\\)|\\(B\\)|\\(C\\)")) |>
    mutate(level1 = str_remove_all(level, "\\(.*\\).*\\:\\s")) |>
    mutate(header = str_replace_all(header, "\\(B\\) Access", "\\(C\\) News Access")) |>
    mutate(header = str_replace_all(header, "\\(C\\) Political Interest", "\\(B\\) Political Interest"))

labels <- unique(dat$level1)
names(labels) <- labels
ggplot(dat, aes(y = Estimate, x = factor(threshold))) +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(A\\)"), ], shape = 15,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_shape_manual(values = c(
        15, 15, 15, 15, 15,
        16, 16, 16, 16,
        17, 17, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11
    ), name = "") +
    scale_color_manual(
        values = c(
            "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "grey25"
        ),
        name = "(A) Country",
        guide = guide_legend(title.position = "left", order = 1, nrow = 2)
    ) +
    ggnewscale::new_scale_color() +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(B\\)"), ], shape = 16,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c("#E69F00", "#009E73"),
        name = "(B) Political Interest",
        guide = guide_legend(title.position = "left", order = 2, nrow = 2)
    ) +
    ggnewscale::new_scale_color() +
    geom_pointrange(
        data = dat[str_detect(dat$level, "\\(C\\)"), ], shape = 17,
        aes(ymin = CI_lower, ymax = CI_upper, color = level1), size = 0.32,
        position = position_dodge2(w = 0.4)
    ) +
    scale_color_manual(
        values = c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
        name = "(C) News Access",
        guide = guide_legend(title.position = "left", order = 3, nrow = 2)
    ) +
    facet_grid(type ~ header, scales = "free_x") +
    coord_flip() +
    theme_bw() +
    theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal"
    ) +
    # legend.title = element_blank()) +
    labs(y = "", x = "threshold (in sec)") +
    geom_hline(yintercept = 0, linetype = "dashed")

ggsave(paste0("figures/", platform, "_regression_conditional_no_control.pdf"), width = 12, height = 7)

## average alignment top 15 outlets
cutoffs <- c(3, 10, 30, 60, 120)
non_pol <- lapply(cutoffs, function(y) {
    lapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "" & duration >= y]

        # calculate the ideological slant of the individual participants news diets
        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo,visits_tot = sum(visits)), by = .(domain)]
        dom_align[,country:=x]
        dom_align
    })
})

pol <- lapply(cutoffs, function(y) {
    lapply(fl, function(x) {
        dt <- data.table::fread(paste0("processed_data/", platform, "/news_only/", x))
        if (fixN) {
            peeps <- dt[duration >= 120]
            peeps <- unique(peeps[["panelist_id"]])
            dt <- dt[panelist_id %in% peeps]
        }
        dt[survey, on = .(panelist_id), leftright := leftright]
        dt <- dt[!is.na(leftright)]
        dt[, `:=`(leftright = fcase(leftright < 6, -1, leftright > 6, 1, default = 0))]
        mean_ideo <- mean(unique(dt[, .(panelist_id, leftright)])[["leftright"]])

        dt <- dt[political == "political" & duration >= y]

        # calculate the ideological slant of the individual participants news diets
        dom_align <- dt[, .(align = mean(leftright, na.rm = TRUE) - mean_ideo,visits_tot = sum(visits)), by = .(domain)]
        dom_align[,country:=x]
        dom_align
    })
})

res <- rbind(
    rbind(rbindlist(non_pol[[1]]),rbindlist(non_pol[[2]]),rbindlist(non_pol[[3]]),rbindlist(non_pol[[4]]),rbindlist(non_pol[[5]])),
    rbind(rbindlist(pol[[1]]),rbindlist(pol[[2]]),rbindlist(pol[[3]]),rbindlist(pol[[4]]),rbindlist(pol[[5]]))
)

res[,country:=long_cases[match(str_remove(country,"\\.csv"),short_cases)]]
res <- res[,.(align = mean(align,na.rm=TRUE),visits=max(visits_tot)),by=.(domain,country)]
res[,order:=rank(-visits),by=country]
res <- res[order<=15]
saveRDS(res,paste0("processed_data/stats/", platform, "top_outlet_align.RDS"))

## plot
dat <- readRDS(paste0("processed_data/stats/", platform, "top_outlet_align.RDS"))

ggplot(dat,aes(x=align,size=visits,label=domain))+
    geom_point(y=0)+
    geom_text(y=0,angle=45,hjust = 1.1, vjust = 1, nudge_x = 0, alpha = .5)+
    scale_x_continuous(limits=c(-1., 1)) +
    scale_y_continuous(limits=c(-1.75, 0.25)) +
    scale_size(range = c(3,10)) +
    coord_cartesian(clip="off")+
    facet_wrap(~country, nrow = 6, scales = "free_y")+
    theme_bw()+
    theme(#axis.title = element_blank(),
        axis.text = element_text(size = 9),
        legend.position = "none",
        strip.text = element_text(size = 14,face="bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  xlab("Ideological alignment") +
  ylab("")

ggsave(paste0("figures/", platform, "_domain_top15_align.pdf"), width = 10, height = 16)
