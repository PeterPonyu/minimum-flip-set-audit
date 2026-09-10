# Figure 3. What each test could have returned, against what it did return. The
# floor is the smallest p-value an exact test on that many discordant pairs can
# produce, which is fixed by the count before any answer is graded. A contrast
# whose floor sits above the threshold could not have been significant however the
# answers came out, so its non-significance is a fact about the question set
# rather than about the conditions being compared.

floors <- inventory

# Three states, not two. A contrast whose discordant pairs all fell the same way
# is at its floor and is the point the figure is about; a contrast with no
# discordant pairs at all also sits on the diagonal, but for a reason that says
# nothing about how anything fell, so it is marked apart rather than counted in.
floors$attained <- factor(
  ifelse(floors$discordant == 0L, "No discordant pairs",
         ifelse(floors$at_floor, "Every discordant pair fell one way",
                "Discordant pairs split")),
  levels = c("Discordant pairs split", "Every discordant pair fell one way",
             "No discordant pairs"))

# Both axes carry the same quantity, so they are given the same range: the
# diagonal is only readable as an equality if a unit on one is a unit on the
# other. The limits are finite because the shaded region is drawn in data
# coordinates, and a log axis has no finite bottom to anchor it to otherwise.
span <- range(c(floors$floor, floors$p))
LIMITS <- 10^(log10(span) + c(-0.3, 0.3))

# Decade labels are written out rather than produced by scales::label_log().
# That labeller returns a plotmath expression, and plotmath resolves its minus
# sign through the device's own symbol handling instead of the theme family, so
# Cairo answers by embedding a second font for the exponents alone. The ASCII
# exponent is the form the other papers already use on a log axis.
DECADES <- 10^seq(-6, 0)
DECADE_LABELS <- c("1e-6", "1e-5", "1e-4", "1e-3", "1e-2", "1e-1", "1")

# A single-column figure, matched to the decoupling figure so the two read as a
# pair. The shaded region, the diagonal and the threshold rules are decoded in
# the caption; the size key omits zero because a contrast with no discordant
# pair is the cross, not a small dot.
p <- ggplot(floors, aes(x = floor, y = p)) +
  annotate("rect", xmin = ALPHA, xmax = LIMITS[2], ymin = LIMITS[1], ymax = LIMITS[2],
           fill = "grey88") +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.4, colour = "grey35", linetype = "22") +
  geom_hline(yintercept = ALPHA, linewidth = 0.4, colour = "grey25") +
  geom_vline(xintercept = ALPHA, linewidth = 0.4, colour = "grey25") +
  geom_point(aes(shape = attained, size = discordant), stroke = 0.5, alpha = 0.9) +
  scale_shape_manual(values = c(19, 1, 4), name = NULL, drop = FALSE) +
  scale_size_continuous(range = c(1.1, 3.4), name = "Discordant pairs",
                        breaks = c(4, 8, 12, 20)) +
  scale_x_log10(limits = LIMITS, breaks = DECADES, labels = DECADE_LABELS,
                expand = c(0, 0)) +
  scale_y_log10(limits = LIMITS, breaks = DECADES, labels = DECADE_LABELS,
                expand = c(0, 0)) +
  annotation_logticks(sides = "bl", linewidth = FIGURE_HAIRLINE,
                      short = unit(2, "pt"), mid = unit(3, "pt"), long = unit(4, "pt")) +
  coord_fixed() +
  labs(x = "Smallest attainable p-value", y = "p-value returned") +
  guides(shape = guide_legend(order = 1, ncol = 1),
         size = guide_legend(order = 2, nrow = 1)) +
  rtx_theme() +
  legend_bottom(legend.box = "vertical", legend.box.just = "left",
                legend.justification = "left", legend.spacing.y = unit(3, "pt"),
                legend.margin = margin(t = -2))

save_fig(p, "fig5_floor", width = FIGURE_SINGLE_COLUMN_WIDTH_IN, height = 3.95)
