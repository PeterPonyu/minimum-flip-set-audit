# Figure 4. Dropping one question at a time. A flip asks what a different grading
# would have done; a deletion asks what a different question set would have done.
# They are not the same measurement, and a conclusion can survive one and not the
# other, so both are reported for every contrast.

loo <- inventory
loo$row_label <- paste0(loo$label, ": ", CONDITION_NAMES[loo$arm],
                        " vs ", CONDITION_NAMES[loo$reference])
loo$sign_share <- loo$loo_sign / loo$n
loo$verdict_share <- loo$loo_verdict / loo$n
loo <- loo[order(loo$sign_share, loo$verdict_share, loo$row_label), ]
loo$pos <- seq_len(nrow(loo))

long <- rbind(
  data.frame(pos = loo$pos, share = loo$sign_share, kept = loo$loo_sign, n = loo$n,
             which = "Direction unchanged", stringsAsFactors = FALSE),
  data.frame(pos = loo$pos, share = loo$verdict_share, kept = loo$loo_verdict, n = loo$n,
             which = "Verdict unchanged", stringsAsFactors = FALSE))

annotated <- long[long$share < 1, ]

p <- ggplot(long, aes(x = share, y = pos, shape = which)) +
  geom_vline(xintercept = 1, linewidth = 0.4, colour = "grey25") +
  geom_point(size = 1.9, stroke = 0.5) +
  geom_text(data = annotated, aes(label = sprintf("%d of %d", kept, n)),
            hjust = 1.25, size = 2.2, show.legend = FALSE) +
  scale_shape_manual(values = c(19, 1), name = NULL) +
  scale_y_continuous(breaks = loo$pos, labels = loo$row_label, expand = c(0, 0.7)) +
  scale_x_continuous(limits = c(0.68, 1.02), expand = c(0, 0),
                     labels = function(x) formatC(x, format = "f", digits = 2)) +
  labs(x = "Share of single-question deletions leaving the conclusion in place", y = NULL,
       subtitle = "unlabelled points are unanimous") +
  rtx_theme() +
  theme(plot.subtitle = element_text(size = 6.6, colour = "grey25"),
        axis.text.y = element_text(size = 6.4),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom", legend.text = element_text(size = 7),
        legend.key.size = unit(9, "pt"), legend.margin = margin(t = -4))

save_fig(p, "fig7_loo", width = FIGURE_TEXT_WIDTH_IN, height = 4.1)
