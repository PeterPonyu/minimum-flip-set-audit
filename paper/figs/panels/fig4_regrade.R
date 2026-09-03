# Figure 2. The one grading three graders disagree about, and what the contrast
# does when it moves. Panel A is the accuracy of each condition on the
# image-load-bearing questions of the famous set under the stored grader and
# under the numeric-tolerant one; only the multimodal condition moves, and it
# moves because of a single answer. Panel B is the contrast those
# accuracies produce, before and after.

conditions <- c("no_kg", "text_kg", "multimodal_kg")

stored <- vapply(conditions, function(x) mean(famous_ilb[[x]]), numeric(1))
after <- vapply(conditions, function(x) mean(regraded[[x]]), numeric(1))

# Counts rather than accuracies. The subject is a single answer, and on a
# proportion axis a single answer is an eighteenth of the range; on a count axis
# it is one unit, which is what it is.
levels_out <- data.frame(
  condition = rep(CONDITION_NAMES[conditions], 2),
  grader = rep(c("Stored", "Numeric-tolerant"), each = length(conditions)),
  correct = round(c(stored, after) * harm$n), stringsAsFactors = FALSE)
levels_out$condition <- factor(levels_out$condition, levels = CONDITION_NAMES[conditions])
levels_out$grader <- factor(levels_out$grader, levels = c("Stored", "Numeric-tolerant"))

moved <- levels_out[levels_out$condition == CONDITION_NAMES[["multimodal_kg"]], ]

counts <- ggplot(levels_out, aes(x = condition, y = correct, shape = grader)) +
  geom_segment(data = moved[1, ], aes(xend = condition, y = min(moved$correct),
                                      yend = max(moved$correct)),
               linewidth = 0.35, colour = "grey45", show.legend = FALSE) +
  geom_point(size = 2.2, stroke = 0.5) +
  scale_y_continuous(breaks = seq(0, harm$n), limits = c(min(levels_out$correct) - 1.4,
                                                         max(levels_out$correct) + 1.4)) +
  scale_shape_manual(values = c(19, 1), name = NULL) +
  labs(x = "Retrieval condition", y = sprintf("Answers graded correct, of %d", harm$n),
       subtitle = sprintf("one of %d answers moves", regrade$grader_agreement$famous$n)) +
  rtx_theme() +
  theme(plot.subtitle = element_text(size = 7, colour = "grey25"),
        legend.position = "bottom", legend.text = element_text(size = 7),
        legend.key.size = unit(9, "pt"), legend.margin = margin(t = -4))

contrast <- data.frame(
  grader = factor(c("Stored", "Numeric-tolerant"),
                  levels = c("Stored", "Numeric-tolerant")),
  diff = c(harm$diff_pt, REGRADED_DIFF))

difference <- ggplot(contrast, aes(x = diff, y = grader)) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey25") +
  geom_segment(aes(x = 0, xend = diff, yend = grader), linewidth = 0.4, colour = "grey45") +
  geom_point(size = 2.4) +
  geom_text(aes(label = sprintf("%+.1f pt", diff)), vjust = -1.2, size = 2.6) +
  scale_x_continuous(limits = c(min(contrast$diff) - 2.5, 2.5)) +
  labs(x = "Multimodal minus text (percentage points)", y = NULL,
       subtitle = "the conclusion the direction carries") +
  rtx_theme() +
  theme(plot.subtitle = element_text(size = 7, colour = "grey25"),
        axis.text.y = element_text(size = 7.5),
        panel.grid.major.y = element_blank())

p <- patchwork::wrap_plots(panel_label(counts, "A"), panel_label(difference, "B"),
                           widths = c(1, 1.05))

save_fig(p, "fig4_regrade", width = FIGURE_TEXT_WIDTH_IN, height = 2.7)
