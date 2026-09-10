# Figure 8. Effect sizes and exact paired uncertainty for every contrast.
#
# The fragility panels report distances in grading units. This companion view
# puts the same contrasts back in their natural scale (percentage points) and
# attaches the exact interval implied by the discordant split. It is deliberately
# aggregate: no question text, answer, gold string or identifier is displayed.

forest <- inventory
forest$dataset_code <- c(famous = "W", longtail = "L")[forest$dataset]
forest$subset_code <- c(image_load_bearing = "I", text_only = "T", overall = "A")[forest$subset]
condition_code <- c(no_kg = "N", text_kg = "T", multimodal_kg = "M")
forest$row_label <- sprintf("%s/%s %s-%s", forest$dataset_code, forest$subset_code,
                            condition_code[forest$arm], condition_code[forest$reference])

intervals <- lapply(seq_len(nrow(forest)), function(i) {
  cells <- c(both = forest$both[i], arm_only = forest$arm_only[i],
              ref_only = forest$ref_only[i], neither = forest$neither[i])
  exact_paired_interval(cells)
})
forest$lower_pt <- 100 * vapply(intervals, function(x) x[1], numeric(1))
forest$upper_pt <- 100 * vapply(intervals, function(x) x[2], numeric(1))
forest$estimate_pt <- forest$diff_pt
if (any(!is.finite(c(forest$lower_pt, forest$upper_pt))) ||
    any(forest$lower_pt > forest$estimate_pt + 1e-10) ||
    any(forest$upper_pt < forest$estimate_pt - 1e-10)) {
  stop("an exact paired interval does not contain its recorded contrast")
}

# Keep the same stable ordering as the manuscript's tables -- the widely
# documented set first, then image / text / all questions, then the three
# contrasts in the order the record fixes them -- and put the first row at the
# top of the plot. The compact key is decoded in the caption.
CONTRAST_ORDER <- vapply(CONTRASTS, paste, character(1), collapse = " vs ")
forest <- forest[order(match(forest$dataset, c("famous", "longtail")),
                       match(forest$subset_code, c("I", "T", "A")),
                       match(paste(forest$arm, "vs", forest$reference), CONTRAST_ORDER)), ]
forest$y <- rev(seq_len(nrow(forest)))
# Use the paired accuracy counts themselves to encode a zero effect.  The
# fragility distance is a different estimand, and a zero effect can still have
# discordant questions (m > 0); coupling the marker to k_sign would make that
# distinction depend on an unrelated derived field.
forest$zero_difference <- (forest$arm_only == forest$ref_only)
if (any(forest$zero_difference != (abs(forest$estimate_pt) < 1e-10))) {
  stop("the zero-effect marker does not agree with the paired accuracy counts")
}
forest$shape <- ifelse(forest$zero_difference, "zero difference", "directional difference")
# Printed as m/n, which is how the caption names it.
forest$note <- sprintf("%d/%d", forest$discordant, forest$n)

lower <- min(forest$lower_pt)
upper <- max(forest$upper_pt)
right_limit <- upper + 9
left_limit <- lower - 6

# Rows come in blocks of three, one block per question set and split; a light
# rule separates the blocks and a darker one separates the two question sets.
block_edges <- seq(3.5, nrow(forest) - 0.5, by = 3)
dataset_edge <- nrow(forest) / 2 + 0.5
separators <- data.frame(
  y = block_edges,
  colour = ifelse(abs(block_edges - dataset_edge) < 1e-9, "grey55", "grey85"),
  stringsAsFactors = FALSE)

p <- ggplot(forest, aes(y = y)) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.4, colour = "grey25") +
  geom_hline(data = separators, aes(yintercept = y, colour = colour),
             linewidth = FIGURE_HAIRLINE, show.legend = FALSE) +
  scale_colour_identity() +
  geom_segment(aes(x = lower_pt, xend = upper_pt, yend = y),
               linewidth = 0.55, colour = "grey35") +
  geom_point(aes(x = estimate_pt, shape = shape), size = 2.1, stroke = 0.55,
             colour = FIGURE_PALETTE[["blue"]]) +
  geom_text(aes(x = upper_pt + 1.6, label = note), hjust = 0,
            size = FIGURE_ANNOTATION_SIZE, colour = "grey25") +
  scale_shape_manual(values = c("directional difference" = 16, "zero difference" = 1),
                     name = NULL) +
  scale_x_continuous(name = "Accuracy difference (percentage points; first minus second)",
                     limits = c(left_limit, right_limit),
                     breaks = seq(-60, 60, by = 20), expand = c(0, 0)) +
  scale_y_continuous(name = NULL, breaks = forest$y, labels = forest$row_label,
                     limits = c(0.3, nrow(forest) + 0.7), expand = c(0, 0)) +
  rtx_theme() +
  legend_bottom(legend.margin = margin(t = -3)) +
  theme(panel.grid.major.y = element_blank())

save_fig(p, "fig8_effect_forest", FIGURE_TEXT_WIDTH_IN, 4.0)
