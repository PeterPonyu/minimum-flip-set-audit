# Figure 9. How often a significant verdict at these n rests on one grading.
#
# The closed form is exact, so the operating characteristic is a function of
# the sampling model rather than of a search. The bound simulation summary
# already pools 840,000 paired Bernoulli tables; this panel plots that pool
# rather than redrawing the lattice.

p_one <- unlist(audit_s4$headline$P_kappa_ver_eq_1_given_sig_by_n)
p_two <- unlist(audit_s4$headline$P_kappa_ver_le_2_given_sig_by_n)
if (is.null(names(p_one)) || !identical(names(p_one), names(p_two))) {
  stop("the simulation headline no longer reports P(kappa_ver = 1) and P(<= 2) on the same n grid")
}

op <- data.frame(
  n = as.integer(names(p_one)),
  p_one = as.numeric(p_one),
  p_two = as.numeric(p_two),
  stringsAsFactors = FALSE)
op <- op[order(op$n), ]
if (nrow(op) != length(audit_s4$design$n_grid) ||
    any(op$n != as.integer(audit_s4$design$n_grid))) {
  stop("the simulation headline n grid does not match the recorded design")
}

EVENT_LEVELS <- c("verdict distance = 1", "verdict distance \u2264 2")
long <- rbind(
  data.frame(n = op$n, probability = op$p_one,
             event = EVENT_LEVELS[[1]], stringsAsFactors = FALSE),
  data.frame(n = op$n, probability = op$p_two,
             event = EVENT_LEVELS[[2]], stringsAsFactors = FALSE))
long$event <- factor(long$event, levels = EVENT_LEVELS)

# The two design sizes of the evaluation being audited, so the curve can be read
# at the points where the manuscript quotes it. Both are sizes the study already
# carries, not new quantities.
design_sizes <- data.frame(n = c(harm$n, nrow(WIDE$famous)))
if (!all(design_sizes$n %in% op$n)) {
  stop("the simulation grid does not contain the evaluation's own design sizes")
}

# An operating characteristic in a single column: the curve is the object, so
# it is drawn in grey with the two design sizes marked by thin rules.
p <- ggplot(long, aes(x = n, y = probability, shape = event, linetype = event)) +
  geom_vline(data = design_sizes, aes(xintercept = n), linewidth = 0.35,
             colour = "grey35", linetype = "22", inherit.aes = FALSE) +
  geom_line(linewidth = 0.4, colour = "grey25") +
  geom_point(size = 2, colour = "grey15", fill = "white") +
  scale_x_log10(breaks = op$n) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_shape_manual(values = c(16, 21), name = NULL) +
  scale_linetype_manual(values = c("solid", "22"), name = NULL) +
  labs(x = "Paired items, n", y = "Share of significant tables") +
  rtx_theme() +
  legend_bottom(legend.key.width = unit(18, "pt"), legend.margin = margin(t = -2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p, "fig9_operating", width = FIGURE_SINGLE_COLUMN_WIDTH_IN, height = 2.55)
