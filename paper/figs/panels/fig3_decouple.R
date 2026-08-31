# Figure 6. The two distances against each other, for the whole family.
#
# Figure 1 gives each contrast its own row, which is the right way to read one
# of them. This is the same numbers with the rows dissolved, and it answers a
# different question: whether a reader who knows one distance knows the other.
# They do not. The points sit on both sides of the diagonal, so neither claim is
# systematically the sturdier one, and the study's recommendation to print both
# follows from the scatter rather than from the two cases the text names.

decoupled <- inventory
decoupled$family <- factor(
  ifelse(decoupled$degenerate, "No direction to reverse",
         ifelse(decoupled$decoupled, sprintf("Distances differ by %d or more", DECOUPLE_GAP),
                "Distances within a few gradings")),
  levels = c("Distances within a few gradings",
             sprintf("Distances differ by %d or more", DECOUPLE_GAP),
             "No direction to reverse"))

SPAN <- c(-0.6, max(decoupled$k_sign, decoupled$k_verdict) + 0.9)

# Several contrasts land on the same pair of integers, and a scatter that hides
# them would understate the pattern, so the count is printed where it exceeds one
# rather than the points being jittered apart.
stacked <- aggregate(list(count = decoupled$n),
                     by = list(k_sign = decoupled$k_sign, k_verdict = decoupled$k_verdict),
                     FUN = length)
stacked <- stacked[stacked$count > 1L, ]

p <- ggplot(decoupled, aes(x = k_verdict, y = k_sign)) +
  annotate("polygon", x = c(SPAN[1], SPAN[2], SPAN[2]),
           y = c(SPAN[1], SPAN[2], SPAN[1]), fill = "grey93") +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.4, colour = "grey35", linetype = "22") +
  geom_abline(slope = 1, intercept = c(-DECOUPLE_GAP, DECOUPLE_GAP),
              linewidth = 0.3, colour = "grey60") +
  geom_point(aes(shape = family), size = 2, stroke = 0.55, fill = "white") +
  geom_text(data = stacked, aes(label = count), size = 2.1, colour = "grey20",
            nudge_x = 0.55, nudge_y = 0.35) +
  scale_shape_manual(values = c(19, 21, 4), name = NULL, drop = FALSE,
                     guide = guide_legend(nrow = 2)) +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  coord_fixed(xlim = SPAN, ylim = SPAN, expand = FALSE) +
  labs(x = "Gradings that would move the verdict",
       y = "Gradings that would reverse the direction",
       subtitle = sprintf("below the diagonal: the verdict is the sturdier claim (%d of %d)",
                          sum(!decoupled$degenerate & decoupled$k_sign < decoupled$k_verdict),
                          sum(!decoupled$degenerate))) +
  rtx_theme() +
  theme(plot.subtitle = element_text(size = 7, colour = "grey25"),
        legend.position = "bottom", legend.text = element_text(size = 7),
        legend.key.size = unit(9, "pt"), legend.margin = margin(t = -4),
        legend.justification = "left")

save_fig(p, "fig3_decouple", width = 0.62 * FIGURE_TEXT_WIDTH_IN, height = 4.6)
