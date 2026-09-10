# Figure 10. Three pre-registered minimum-flip claims under two graders.

# The row labels use the same W/L, I/T/A and M/T/N key as the ladders.
summary_specs <- data.frame(
  key = c("Harm", "Help", "Sig"),
  label = c("Harm direction\nW/I M-T",
            "Help direction\nL/I M-T",
            "Long-tail text verdict\nL/T T-N"),
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
             grader = FIGURE_GRADER_LEVELS,
             kappa = c(grader_values("strict", spec), grader_values("numeric", spec)),
             stringsAsFactors = FALSE)
}))
# Rows read top to bottom in the pre-registration order, and within a row the
# stored grader is drawn above the numeric-tolerant one.
summary_rows$label <- factor(summary_rows$label, levels = rev(summary_specs$label))
summary_rows$grader <- factor(summary_rows$grader, levels = rev(FIGURE_GRADER_LEVELS))

# Six bars are a single-column figure. Gradings run along the horizontal axis,
# as they do in the flip ladder, and the two graders are the two colours the
# regrade figure uses for them. The framing is in the caption.
p <- ggplot(summary_rows, aes(y = label, x = kappa, fill = grader)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6,
           colour = "grey25", linewidth = FIGURE_HAIRLINE) +
  geom_text(aes(label = kappa), position = position_dodge(width = 0.7),
            hjust = -0.4, size = FIGURE_ANNOTATION_SIZE, colour = "grey20") +
  scale_fill_manual(values = FIGURE_GRADER_COLOURS, name = NULL,
                    breaks = FIGURE_GRADER_LEVELS) +
  scale_x_continuous(breaks = 0:8, limits = c(0, 8),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(y = NULL, x = "Minimum gradings to overturn") +
  rtx_theme() +
  legend_bottom(legend.margin = margin(t = -2)) +
  theme(panel.grid.major.y = element_blank())

save_fig(p, "fig10_flip_regrade_summary", FIGURE_SINGLE_COLUMN_WIDTH_IN, 2.35)
