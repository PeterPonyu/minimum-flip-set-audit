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

long <- rbind(
  data.frame(n = op$n, probability = op$p_one,
             event = "one grading", stringsAsFactors = FALSE),
  data.frame(n = op$n, probability = op$p_two,
             event = "at most two", stringsAsFactors = FALSE))
long$event <- factor(long$event, levels = c("one grading", "at most two"))

p <- ggplot(long, aes(x = n, y = probability, shape = event, linetype = event)) +
  geom_line(linewidth = 0.4, colour = "grey25") +
  geom_point(size = 2.1, colour = "grey15") +
  scale_x_log10(breaks = op$n) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_shape_manual(values = c(16, 1), name = NULL) +
  scale_linetype_manual(values = c("solid", "22"), name = NULL) +
  labs(x = "Paired items, n",
       y = "Share of significant tables") +
  rtx_theme() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = FIGURE_LEGEND_TEXT_SIZE),
        legend.key.width = unit(18, "pt"),
        legend.margin = margin(t = -2),
        axis.text.x = element_text(size = 7.2, angle = 45, hjust = 1))

save_fig(p, "fig9_operating", width = 0.72 * FIGURE_TEXT_WIDTH_IN, height = 2.85)
