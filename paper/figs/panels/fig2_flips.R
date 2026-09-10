# Figure 1. Every contrast in the study, measured by how many gradings would have
# to have come out the other way. The two distances are different questions asked
# of the same four counts: how far the direction is from reversing, and how far
# the verdict is from crossing the threshold. Points far apart on a row are
# contrasts where one is sturdy and the other is not.

flips <- inventory
flips$dataset_code <- c(famous = "W", longtail = "L")[flips$dataset]
flips$subset_code <- c(image_load_bearing = "I", text_only = "T", overall = "A")[flips$subset]
flips <- flips[order(flips$k_sign, flips$k_verdict, flips$dataset_code,
                    flips$subset_code, flips$arm, flips$reference), ]
flips$pos <- seq_len(nrow(flips))

# Keep the ladder legible at the printed width. The key is stated in the
# caption: W/L are the two recorded datasets, I/T/A are the image, text and
# all-question splits, and M/T/N are multimodal, text and no retrieval.
condition_code <- c(no_kg = "N", text_kg = "T", multimodal_kg = "M")
flips$row_label <- sprintf("%s/%s %s-%s", flips$dataset_code, flips$subset_code,
                           condition_code[flips$arm], condition_code[flips$reference])

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

# The row key, the shaded threshold and the cross are all decoded in the
# caption, which is the one place the figure is explained.
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
  labs(x = "Gradings that would have to have come out otherwise", y = NULL) +
  rtx_theme() +
  legend_bottom(legend.margin = margin(t = -1)) +
  theme(panel.grid.major.y = element_blank())

save_fig(p, "fig2_flips", width = FIGURE_TEXT_WIDTH_IN, height = 3.6)
