# Figure 10. Three pre-registered minimum-flip claims under two graders.

summary_specs <- data.frame(
  key = c("Harm", "Help", "Sig"),
  label = c("Harm direction\n(W/I M vs T)",
            "Help direction\n(L/I M vs T)",
            "Help verdict\n(L/T T vs N)"),
  estimand = c("direction", "direction", "verdict"),
  stringsAsFactors = FALSE
)

grader_values <- function(grader, spec) {
  rows <- audit_s2$per_grader[[grader]]$contrasts
  target <- c(
    Harm = "Famous, image | multimodal vs text",
    Help = "Long-tail, image | multimodal vs text",
    Sig = "Long-tail, text | text vs none"
  )[[spec$key]]
  row <- rows[rows$label == target, ]
  if (nrow(row) != 1L) stop("missing unique summary row for ", grader, "/", spec$key)
  value <- if (spec$estimand == "direction") row$kappa_dir else row$kappa_ver
  as.integer(value[[1]])
}

summary_rows <- do.call(rbind, lapply(seq_len(nrow(summary_specs)), function(i) {
  spec <- summary_specs[i, ]
  data.frame(label = spec$label,
             grader = c("Stored strict", "Numeric-tolerant"),
             kappa = c(grader_values("strict", spec), grader_values("numeric", spec)),
             stringsAsFactors = FALSE)
}))
summary_rows$label <- factor(summary_rows$label, levels = rev(summary_specs$label))
summary_rows$grader <- factor(summary_rows$grader,
                              levels = c("Stored strict", "Numeric-tolerant"))

p <- ggplot(summary_rows, aes(x = label, y = kappa, fill = grader)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62,
           colour = "grey25", linewidth = 0.2) +
  geom_text(aes(label = kappa), position = position_dodge(width = 0.72),
            vjust = -0.35, size = 2.8, colour = "grey20") +
  scale_fill_manual(values = c("Stored strict" = "#2166AC",
                               "Numeric-tolerant" = "#B2182B"), name = NULL) +
  scale_y_continuous(breaks = 0:12, limits = c(0, 13),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Minimum gradings to overturn",
       subtitle = "Three pre-registered claims; raw per-contrast distances (Holm survival is a separate stored condition)") +
  rtx_theme() +
  theme(axis.text.x = element_text(size = 7.2),
        axis.text.y = element_text(size = 7.4),
        legend.position = "bottom", legend.text = element_text(size = 7),
        legend.key.size = unit(9, "pt"), legend.margin = margin(t = -4),
        plot.subtitle = element_text(size = 7.1, colour = "grey25"))

save_fig(p, "fig10_flip_regrade_summary", FIGURE_TEXT_WIDTH_IN, 3.4)
