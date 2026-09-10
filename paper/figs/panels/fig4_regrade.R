# Figure 2. The one grading three graders disagree about, and what the contrast
# does when it moves. Panel A is the accuracy of each condition on the
# image-load-bearing questions of the famous set under the stored grader and
# under the numeric-tolerant one; only the multimodal condition moves, and it
# moves because of a single answer. Panel B is the contrast those
# accuracies produce, before and after.
#
# Eight points and two bars do not need a text-width canvas. The two panels
# are stacked in a single column, and the grader is drawn in the same two
# colours the summary figure uses for it.

conditions <- c("no_kg", "text_kg", "multimodal_kg")

stored <- vapply(conditions, function(x) mean(famous_ilb[[x]]), numeric(1))
after <- vapply(conditions, function(x) mean(regraded[[x]]), numeric(1))

# Counts rather than accuracies. The subject is a single answer, and on a
# proportion axis a single answer is an eighteenth of the range; on a count axis
# it is one unit, which is what it is.
levels_out <- data.frame(
  condition = rep(unname(CONDITION_NAMES[conditions]), 2),
  grader = rep(FIGURE_GRADER_LEVELS, each = length(conditions)),
  correct = round(c(stored, after) * harm$n), stringsAsFactors = FALSE)
levels_out$grader <- factor(levels_out$grader, levels = FIGURE_GRADER_LEVELS)

moved <- levels_out[levels_out$condition == CONDITION_NAMES[["multimodal_kg"]], ]

# The conditions are ordered as every table orders them, none / text /
# multimodal. The order is fixed on the scale rather than on the data because
# a layer drawn from a one-row subset trains the discrete scale first and
# would otherwise leave it alphabetical.
# The stored grader is a filled dot and the numeric-tolerant one an open ring
# drawn over it, so a condition on which the two agree shows as a ringed dot and
# the one they disagree on shows as two separate markers joined by a rule.
counts <- ggplot(levels_out, aes(x = condition, y = correct)) +
  geom_segment(data = moved[1, ], aes(xend = condition, y = min(moved$correct),
                                      yend = max(moved$correct)),
               linewidth = 0.35, colour = "grey45", show.legend = FALSE) +
  geom_point(aes(shape = grader, colour = grader), size = 2.3, stroke = 0.7) +
  scale_x_discrete(limits = unname(CONDITION_NAMES[conditions])) +
  scale_y_continuous(breaks = seq(0, harm$n), limits = c(min(levels_out$correct) - 1.4,
                                                         max(levels_out$correct) + 1.4)) +
  scale_shape_manual(values = c(19, 1), name = NULL, breaks = FIGURE_GRADER_LEVELS) +
  scale_colour_manual(values = FIGURE_GRADER_COLOURS, name = NULL,
                      breaks = FIGURE_GRADER_LEVELS) +
  labs(x = "Retrieval condition", y = sprintf("Answers graded correct, of %d", harm$n)) +
  rtx_theme() +
  legend_bottom()

contrast <- data.frame(
  grader = factor(FIGURE_GRADER_LEVELS, levels = rev(FIGURE_GRADER_LEVELS)),
  diff = c(harm$diff_pt, REGRADED_DIFF))
# A typographic minus, as the axis prints it, rather than a hyphen.
contrast$label <- sub("-", "\u2212", sprintf("%+.1f pt", contrast$diff), fixed = TRUE)

difference <- ggplot(contrast, aes(x = diff, y = grader)) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey25") +
  geom_segment(aes(x = 0, xend = diff, yend = grader), linewidth = 0.4, colour = "grey45") +
  geom_point(aes(colour = grader), size = 2.4, show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -1.1, size = FIGURE_ANNOTATION_SIZE) +
  scale_colour_manual(values = FIGURE_GRADER_COLOURS, guide = "none") +
  scale_x_continuous(limits = c(min(contrast$diff) - 2.5, 2.5)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.9))) +
  labs(x = "Multimodal minus text\n(percentage points)", y = NULL) +
  rtx_theme() +
  theme(panel.grid.major.y = element_blank())

# Panel B's category labels are wide; freeing Panel A's axis title keeps it
# beside its own tick labels instead of aligned to B's.
p <- patchwork::wrap_plots(patchwork::free(panel_label(counts, "A"), type = "label", side = "l"),
                           panel_label(difference, "B"),
                           ncol = 1, heights = c(1.35, 1), guides = "collect") &
  legend_bottom()

save_fig(p, "fig4_regrade", width = FIGURE_SINGLE_COLUMN_WIDTH_IN, height = 3.9)
