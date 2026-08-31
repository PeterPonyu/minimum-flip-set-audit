# Figure 5. The plane the whole method lives in.
#
# A paired contrast is four counts, and the two the tests read are the discordant
# pair: how many questions the arm alone got right, and how many the reference
# alone did. One grading change moves that pair by one unit along one axis and
# can do nothing else, so the tables reachable from the observed one form a
# lattice and the distance to a conclusion failing is a distance in the ordinary
# sense.
#
# The point of drawing it is that the two panels show the same lattice and the
# same observed point, with failure regions of completely different shape. The
# direction fails on a half-plane, so its distance is read off rather than
# searched for. The verdict fails on a region with a corner in it, which is why
# the two distances are not the same number and why only one of them is free.

# The ball of radius k is a diamond in the plane, and part of it can lie outside
# the triangle of tables that exist. Drawing the whole diamond would show a
# reader states that no grading of any answer could produce, so it is cut back to
# the triangle before it is drawn.
clip_to_triangle <- function(poly, n) {
  edges <- list(function(p) p$b, function(p) p$c,
                function(p) n - p$b - p$c)
  for (inside in edges) {
    if (!nrow(poly)) return(poly)
    kept <- poly[0, ]
    for (i in seq_len(nrow(poly))) {
      here <- poly[i, ]
      prev <- poly[if (i == 1L) nrow(poly) else i - 1L, ]
      d_here <- inside(here)
      d_prev <- inside(prev)
      if (d_here * d_prev < 0) {
        t <- d_prev / (d_prev - d_here)
        kept <- rbind(kept, data.frame(b = prev$b + t * (here$b - prev$b),
                                       c = prev$c + t * (here$c - prev$c)))
      }
      if (d_here >= 0) kept <- rbind(kept, here[, c("b", "c")])
    }
    poly <- kept
  }
  poly
}

plane_panel <- function(cells, n, kind, alpha, subject) {
  grid <- expand.grid(b = 0:n, c = 0:n)
  grid <- grid[grid$b + grid$c <= n, ]
  grid$p <- mapply(split_p, grid$b, grid$c)

  b0 <- cells[["arm_only"]]
  c0 <- cells[["ref_only"]]
  if (identical(kind, "sign")) {
    leading <- b0 > c0
    grid$fails <- if (leading) grid$b <= grid$c else grid$b >= grid$c
    k <- sign_distance(cells)
  } else {
    significant <- mcnemar_exact_p(cells) <= alpha
    grid$fails <- (grid$p <= alpha) != significant
    k <- verdict_distance(cells, alpha)
  }
  grid$state <- factor(ifelse(grid$fails, "conclusion fails here",
                              "conclusion still holds"),
                       levels = c("conclusion still holds", "conclusion fails here"))

  panel <- sprintf("%s\n%d grading%s away", subject, k, if (k == 1L) "" else "s")

  # The ball of radius k in the only metric a grading change can move in. Its
  # boundary touches the failure region without entering it, which is what
  # "minimum" means here, drawn rather than asserted.
  ball <- clip_to_triangle(
    data.frame(b = c(b0 + k, b0, b0 - k, b0),
               c = c(c0, c0 + k, c0, c0 - k)), n)

  list(grid = transform(grid, panel = panel),
       ball = transform(ball, panel = panel),
       observed = data.frame(b = b0, c = c0, panel = panel))
}

# The middle and right panels are the same contrast, the same four counts and
# the same point, read as two different claims. That is the paper's central
# observation and it is a fact about the shape of two regions, so it is drawn
# rather than argued.
plane_parts <- list(
  plane_panel(harm$cells, harm$n, "sign", ALPHA,
              "Widely-documented: direction"),
  plane_panel(help$cells, help$n, "sign", ALPHA,
              "Less-documented: direction"),
  plane_panel(help$cells, help$n, "verdict", ALPHA,
              "The same table: verdict"))

PANEL_ORDER <- vapply(plane_parts, function(x) x$grid$panel[[1]], character(1))
plane_part <- function(name) {
  out <- do.call(rbind, lapply(plane_parts, function(x) x[[name]]))
  out$panel <- factor(out$panel, levels = PANEL_ORDER)
  out
}

PLANE_SPAN <- max(plane_part("grid")$b, plane_part("grid")$c) + 0.7

p <- ggplot(plane_part("grid"), aes(x = b, y = c)) +
  geom_point(aes(colour = state), size = 0.8) +
  geom_polygon(data = plane_part("ball"), fill = NA, colour = "black",
               linewidth = 0.45, linetype = "22") +
  geom_point(data = plane_part("observed"), size = 2.2, shape = 21,
             fill = "white", colour = "black", stroke = 0.7) +
  facet_wrap(~ panel, nrow = 1) +
  scale_colour_manual(values = c("conclusion still holds" = "grey74",
                                 "conclusion fails here" = "grey20"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 20, 4)) +
  scale_y_continuous(breaks = seq(0, 20, 4)) +
  coord_fixed(xlim = c(-0.7, PLANE_SPAN), ylim = c(-0.7, PLANE_SPAN)) +
  labs(x = "Questions the arm alone answered correctly",
       y = "Questions the reference alone answered correctly") +
  rtx_theme() +
  theme(legend.position = "bottom", legend.text = element_text(size = 7),
        legend.key.size = unit(9, "pt"), legend.margin = margin(t = -4),
        strip.text = element_text(size = 8))

save_fig(p, "fig1_plane", width = FIGURE_TEXT_WIDTH_IN, height = 3.1)
