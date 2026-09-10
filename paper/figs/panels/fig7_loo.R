# Figure 4. Dropping one question at a time. A flip asks what a different grading
# would have done; a deletion asks what a different question set would have done.
# They are not the same measurement, and a conclusion can survive one and not the
# other, so both are reported for every contrast.

loo <- inventory
loo$dataset_code <- c(famous = "W", longtail = "L")[loo$dataset]
loo$subset_code <- c(image_load_bearing = "I", text_only = "T", overall = "A")[loo$subset]
loo$sign_share <- loo$loo_sign / loo$n
loo$verdict_share <- loo$loo_verdict / loo$n
loo <- loo[order(loo$sign_share, loo$verdict_share, loo$dataset_code,
                 loo$subset_code, loo$arm, loo$reference), ]
loo$pos <- seq_len(nrow(loo))

# Use the same compact row key as the flip ladder. W/L are the widely and less
# documented datasets, I/T/A are image/text/all questions, and M/T/N are the
# multimodal/text/none retrieval conditions.
condition_code <- c(no_kg = "N", text_kg = "T", multimodal_kg = "M")
loo$row_label <- sprintf("%s/%s %s-%s", loo$dataset_code, loo$subset_code,
                         condition_code[loo$arm], condition_code[loo$reference])

long <- rbind(
  data.frame(pos = loo$pos, share = loo$sign_share, kept = loo$loo_sign, n = loo$n,
             which = "Direction unchanged", stringsAsFactors = FALSE),
  data.frame(pos = loo$pos, share = loo$verdict_share, kept = loo$loo_verdict, n = loo$n,
             which = "Verdict unchanged", stringsAsFactors = FALSE))

annotated <- long[long$share < 1, ]

# Four of the eighteen rows carry information and the rest sit at 1.00, so the
# figure is a single column: the row key and the axis are what a reader needs,
# and a text-width canvas would be mostly empty. The count is printed to the
# left of each marker that is not unanimous; the key is decoded in the caption.
p <- ggplot(long, aes(x = share, y = pos, shape = which)) +
  geom_vline(xintercept = 1, linewidth = 0.4, colour = "grey25") +
  geom_point(size = 1.9, stroke = 0.5) +
  geom_text(data = annotated, aes(label = sprintf("%d of %d", kept, n)),
            hjust = 1.3, size = FIGURE_ANNOTATION_SIZE, show.legend = FALSE) +
  scale_shape_manual(values = c(19, 1), name = NULL) +
  scale_y_continuous(breaks = loo$pos, labels = loo$row_label, expand = c(0, 0.7)) +
  scale_x_continuous(limits = c(0.66, 1.03), breaks = seq(0.7, 1, 0.1), expand = c(0, 0),
                     labels = function(x) formatC(x, format = "f", digits = 2)) +
  labs(x = "Share of single-question deletions\nleaving the conclusion in place", y = NULL) +
  rtx_theme() +
  legend_bottom(legend.margin = margin(t = -2)) +
  theme(panel.grid.major.y = element_blank())

save_fig(p, "fig7_loo", width = FIGURE_SINGLE_COLUMN_WIDTH_IN, height = 3.9)
