# Figure 1. Every contrast in the study, measured by how many gradings would have
# to have come out the other way. The two distances are different questions asked
# of the same four counts: how far the direction is from reversing, and how far
# the verdict is from crossing the threshold. Points far apart on a row are
# contrasts where one is sturdy and the other is not.

flips <- inventory
flips$row_label <- paste0(flips$label, ": ", CONDITION_NAMES[flips$arm],
                          " vs ", CONDITION_NAMES[flips$reference])
flips <- flips[order(flips$k_sign, flips$k_verdict, flips$row_label), ]
flips$pos <- seq_len(nrow(flips))

# A difference of exactly zero has no direction to reverse, so its distance is
# zero by definition rather than by fragility. Marking them keeps a reader from
# reading the bottom of the axis as the most fragile end.
flips$degenerate <- flips$k_sign == 0L

segments <- data.frame(pos = flips$pos,
                       lower = pmin(flips$k_sign, flips$k_verdict),
                       upper = pmax(flips$k_sign, flips$k_verdict))

long <- rbind(
  data.frame(pos = flips$pos[!flips$degenerate], k = flips$k_sign[!flips$degenerate],
             which = "Direction reverses", stringsAsFactors = FALSE),
  data.frame(pos = flips$pos, k = flips$k_verdict,
             which = "Verdict crosses the threshold", stringsAsFactors = FALSE))

p <- ggplot() +
  annotate("rect", xmin = -Inf, xmax = KILL_THRESHOLD - 0.5, ymin = -Inf, ymax = Inf,
           fill = "grey88") +
  geom_segment(data = segments, aes(x = lower, xend = upper, y = pos, yend = pos),
               linewidth = 0.35, colour = "grey45") +
  geom_point(data = long, aes(x = k, y = pos, shape = which), size = 1.9, stroke = 0.5) +
  geom_point(data = flips[flips$degenerate, ], aes(x = k_sign, y = pos),
             shape = 4, size = 1.5, stroke = 0.5, colour = "grey30") +
  scale_shape_manual(values = c(19, 1), name = NULL) +
  scale_y_continuous(breaks = flips$pos, labels = flips$row_label, expand = c(0, 0.7)) +
  scale_x_continuous(breaks = seq(0, max(long$k), by = 2),
                     limits = c(-0.4, max(long$k) + 0.4), expand = c(0, 0)) +
  labs(x = "Gradings that would have to have come out otherwise", y = NULL,
       subtitle = sprintf("shaded: fewer than %d, the prespecified threshold. Cross: no direction to reverse.",
                          KILL_THRESHOLD)) +
  rtx_theme() +
  theme(plot.subtitle = element_text(size = 6.6, colour = "grey25"),
        axis.text.y = element_text(size = 6.4),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom", legend.text = element_text(size = 7),
        legend.key.size = unit(9, "pt"), legend.margin = margin(t = -4))

save_fig(p, "fig1_flips", width = 6.2, height = 4.1)
