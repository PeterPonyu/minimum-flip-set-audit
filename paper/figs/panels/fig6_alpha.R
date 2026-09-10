# Figure 7. What survives if the threshold is moved.
#
# Everything else in this paper is read against 0.05, which is the level the
# records used. A reader is entitled to ask whether the observations are about
# the evaluation or about that number, so the whole family is recomputed across
# a range of thresholds. The three counts move as they must and the shape does
# not: at every level a substantial share of contrasts could not have been
# significant whatever the answers were, because that share is fixed by how
# rarely the two conditions disagreed and not by where the line is drawn.
#
# The sweep is affordable because the verdict distance has a closed form. The
# search that produces the reported numbers is run once, at the recorded level,
# and the build checks the two against each other there.

alpha_counts <- data.frame(
  alpha = rep(ALPHA_SWEEP$alpha, 3),
  count = c(ALPHA_SWEEP$incapable, ALPHA_SWEEP$significant, ALPHA_SWEEP$survives_holm),
  series = factor(rep(c("Could not have reached it", "Reached the threshold",
                        "Survived the family correction"),
                      each = nrow(ALPHA_SWEEP)),
                  levels = c("Could not have reached it", "Reached the threshold",
                             "Survived the family correction")))

# The two panels share the threshold axis, so the legend is collected once
# under the pair rather than printed between them; the vertical rule is decoded
# in the caption.
THRESHOLD_BREAKS <- c(0.001, 0.01, 0.05, 0.2)
THRESHOLD_LABELS <- c("0.001", "0.01", "0.05", "0.2")

counts <- ggplot(alpha_counts, aes(x = alpha, y = count, linetype = series)) +
  geom_vline(xintercept = ALPHA, linewidth = 0.4, colour = "grey25") +
  geom_step(direction = "hv", linewidth = 0.45) +
  scale_linetype_manual(values = c("solid", "42", "12"), name = NULL,
                        guide = guide_legend(ncol = 1)) +
  scale_x_log10(breaks = THRESHOLD_BREAKS, labels = THRESHOLD_LABELS) +
  scale_y_continuous(limits = c(0, nrow(inventory)), breaks = seq(0, 18, 6)) +
  labs(x = NULL, y = sprintf("Contrasts, of %d", nrow(inventory))) +
  rtx_theme() +
  theme(legend.key.width = unit(16, "pt"))

distances <- ggplot(ALPHA_SWEEP, aes(x = alpha, y = median_k)) +
  geom_vline(xintercept = ALPHA, linewidth = 0.4, colour = "grey25") +
  geom_step(direction = "hv", linewidth = 0.45) +
  scale_x_log10(breaks = THRESHOLD_BREAKS, labels = THRESHOLD_LABELS) +
  labs(x = "Threshold the verdict is read against",
       y = "Median gradings to\nmove the verdict") +
  rtx_theme()

p <- patchwork::wrap_plots(panel_label(counts, "A"), panel_label(distances, "B"),
                           ncol = 1, heights = c(1.45, 1), guides = "collect") &
  legend_bottom(legend.justification = "left")

save_fig(p, "fig6_alpha", width = FIGURE_SINGLE_COLUMN_WIDTH_IN, height = 4.1)
