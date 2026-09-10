# Shared figure theme. R-first by workspace convention; do not add a
# matplotlib path to this tree.
#
# Keep one explicitly installed family for every glyph in the exported PDF.
# Arial is the manuscript's approved visual family.  The regular and bold
# files are resolved and checked before any panel is built; Cairo is then
# allowed to embed that family rather than asking fontconfig to substitute a
# second family for plot annotations.
FIGURE_FONT_FAMILY <- "Arial"

# Keep the visual hierarchy in one place.  The values are deliberately shared
# by every panel so a composed figure does not mix tiny inherited labels with
# larger ad-hoc annotations.  ggplot2 sizes are in points for theme elements
# and millimetres for geom text.
#
# These are printed sizes, not nominal ones.  Each canvas is emitted at its own
# printed width -- the text width times the \linewidth fraction the manuscript
# includes it at -- so TeX scales every figure by 1.0 and a value here reaches
# the page unchanged.  A panel that emits a different canvas width would
# silently rescale its own type and reintroduce the size drift these constants
# exist to prevent; figs/lib/emit.R refuses to write such a canvas.
#
# Two canvas widths exist, and they are the journal's two column widths rather
# than arbitrary fractions.  The manuscript text block is 6.5 in; a figure
# included at the full width prints at 165 mm, which the typesetter scales by
# about 1.05 to the 174 mm double column, so its type only grows.  A figure
# included at FIGURE_SINGLE_COLUMN_FRACTION prints at 3.315 in = 84.2 mm, the
# single column, so it is neither enlarged nor reduced when it is set.  A
# fraction between the two would be reduced by an unknown factor at typesetting
# and every size below would be a guess.
FIGURE_TEXT_WIDTH_IN <- 6.5
FIGURE_SINGLE_COLUMN_FRACTION <- 0.51
FIGURE_SINGLE_COLUMN_WIDTH_IN <- FIGURE_SINGLE_COLUMN_FRACTION * FIGURE_TEXT_WIDTH_IN

# Printed type sizes, in points.  The floor is 7 pt for anything a reader has
# to read, axis titles sit in the 8-9 pt band, and a panel label is a bold
# 10 pt letter.  Text drawn by a geom takes its size in millimetres, so the
# same floor is converted once here rather than in each panel.
FIGURE_MIN_TEXT_PT <- 7
FIGURE_BASE_SIZE <- 9
FIGURE_AXIS_TITLE_SIZE <- 8.5
FIGURE_AXIS_TEXT_SIZE <- 7.5
FIGURE_LEGEND_TITLE_SIZE <- 7.5
FIGURE_LEGEND_TEXT_SIZE <- 7.5
FIGURE_STRIP_TEXT_SIZE <- 8
FIGURE_TITLE_SIZE <- 9
FIGURE_SUBTITLE_SIZE <- 7.5
FIGURE_ANNOTATION_SIZE <- FIGURE_MIN_TEXT_PT / ggplot2::.pt
FIGURE_CELL_SIZE <- FIGURE_ANNOTATION_SIZE
FIGURE_PANEL_LABEL_SIZE <- 10

# The thinnest printed rule.  ggplot2 linewidths are millimetres; 0.2 mm is
# 0.57 pt, above the 0.5 pt a print workflow keeps.
FIGURE_HAIRLINE <- 0.2

# One palette for the whole set.  Most encodings are shape and grey level, which
# every reader sees alike.  Where a hue is needed it is the Okabe-Ito blue and
# vermilion, which remain distinct under the common colour-vision deficiencies.
# The two graders are the only categorical contrast drawn in colour, and they
# are drawn in the same two colours in every figure that shows them.
FIGURE_PALETTE <- c(blue = "#0072B2", vermilion = "#D55E00")
FIGURE_GRADER_LEVELS <- c("Stored strict", "Numeric-tolerant")
FIGURE_GRADER_COLOURS <- c("Stored strict" = FIGURE_PALETTE[["blue"]],
                           "Numeric-tolerant" = FIGURE_PALETTE[["vermilion"]])

# Resolve the family before any panel is built.  A `family` string alone is not
# enough: on a different host Cairo can silently substitute a fallback when a
# regular or bold face is absent, and it does so per glyph rather than per
# figure, so a single character can arrive in a different typeface than the
# label around it.  The generator therefore fails closed if the two Arial faces
# or the Unicode glyphs used by the annotations cannot be resolved.  The minus
# sign is on the list because ggplot2 sets negative axis breaks with U+2212
# rather than a hyphen, and the mu because a plotmath unit resolves to it.
FIGURE_REQUIRED_GLYPHS <- c("\u03c3", "\u00d7", "\u00b1", "\u00b0", "\u2192",
                            "\u03bc", "\u2212", "\u2013", "\u2264")

validate_figure_font <- function() {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    stop("systemfonts is required to resolve the embedded figure font")
  }
  matches <- suppressWarnings(systemfonts::match_fonts(
    family = FIGURE_FONT_FAMILY,
    weight = c("normal", "bold")
  ))
  if (nrow(matches) != 2L || any(!file.exists(matches$path))) {
    stop("figure font family must provide installed regular and bold faces: ",
         FIGURE_FONT_FAMILY)
  }
  font_info <- systemfonts::font_info(path = matches$path)
  if (nrow(font_info) != 2L || any(font_info$family != FIGURE_FONT_FAMILY) ||
      !identical(as.logical(font_info$bold), c(FALSE, TRUE)) ||
      !identical(as.character(font_info$name), c("ArialMT", "Arial-BoldMT"))) {
    stop("figure font resolver returned a fallback or unexpected Arial face")
  }
  for (weight in c("normal", "bold")) {
    glyphs <- systemfonts::glyph_info(FIGURE_REQUIRED_GLYPHS,
                                      family = FIGURE_FONT_FAMILY,
                                      weight = weight)
    if (nrow(glyphs) != length(FIGURE_REQUIRED_GLYPHS) ||
        any(is.na(glyphs$index)) || any(glyphs$width <= 0)) {
      stop("figure font lacks one or more required Unicode glyphs at weight ",
           weight, ": ", FIGURE_FONT_FAMILY)
    }
  }
  invisible(matches$path)
}

validate_figure_font()

# ggplot2's newer defaults inherit `family` from the theme, but older releases
# leave text geoms at the host default.  Set both common text geoms explicitly so
# annotations that do not repeat `family =` cannot reintroduce a fallback.
ggplot2::update_geom_defaults("text", list(family = FIGURE_FONT_FAMILY))
ggplot2::update_geom_defaults("label", list(family = FIGURE_FONT_FAMILY))

rtx_theme <- function(base_size = FIGURE_BASE_SIZE) {
  ggplot2::theme_bw(base_size = base_size, base_family = FIGURE_FONT_FAMILY) +
    ggplot2::theme(
      text = ggplot2::element_text(family = FIGURE_FONT_FAMILY, colour = "black"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = FIGURE_HAIRLINE,
                                               colour = "grey90"),
      panel.border = ggplot2::element_rect(colour = "black", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(linewidth = 0.3),
      axis.title = ggplot2::element_text(family = FIGURE_FONT_FAMILY,
                                         size = FIGURE_AXIS_TITLE_SIZE),
      axis.text = ggplot2::element_text(family = FIGURE_FONT_FAMILY,
                                        size = FIGURE_AXIS_TEXT_SIZE,
                                        colour = "black"),
      legend.title = ggplot2::element_text(family = FIGURE_FONT_FAMILY,
                                           size = FIGURE_LEGEND_TITLE_SIZE),
      legend.text = ggplot2::element_text(family = FIGURE_FONT_FAMILY,
                                          size = FIGURE_LEGEND_TEXT_SIZE),
      strip.text = ggplot2::element_text(family = FIGURE_FONT_FAMILY,
                                         size = FIGURE_STRIP_TEXT_SIZE),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4)
    )
}

# Every legend in the set sits under its panel in the same compact form, so a
# reader meets one legend style rather than ten.  The key size is fixed rather
# than inherited from the base size so a legend of open and filled markers does
# not grow with the type.
legend_bottom <- function(...) {
  defaults <- list(
    legend.position = "bottom",
    legend.key.size = ggplot2::unit(9, "pt"),
    legend.key.spacing.x = ggplot2::unit(6, "pt"),
    legend.key.spacing.y = ggplot2::unit(1, "pt"),
    legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.spacing = ggplot2::unit(4, "pt")
  )
  overrides <- list(...)
  do.call(ggplot2::theme, c(defaults[setdiff(names(defaults), names(overrides))], overrides))
}

# Apply a panel label to one member of a composed figure.  A ggplot plot tag
# with `plot.tag.location = "panel"` is anchored to the panel viewport itself.
# hjust=0 and vjust=0 put the left/bottom edges of the glyph on the panel's
# upper-left corner, so the letter sits just above the panel border, flush with
# its left spine, in the top margin.  Anchoring it there rather than to the left
# of the spine keeps it off the topmost y tick label, which is exactly where a
# leftward-extending tag lands when an axis ends on a labelled break.  This is
# deliberately not a data-space annotation: it stays outside the plotting
# region for linear, log, discrete and faceted scales alike, and patchwork
# keeps one tag per composed panel.
#
# A caption that says "left" describes where a panel happened to land in one
# composition; a caption that says "Panel A" describes the panel.  The label is
# what lets the caption stay self-contained, so every composed figure in this
# tree carries one.
#
# The title and subtitle are optional and no panel in this set uses them: the
# caption is the one place a figure is explained, and a heading printed inside
# the canvas would repeat it in smaller type.
panel_label <- function(plot, label, title = NULL, subtitle = NULL) {
  if (!is.null(subtitle)) {
    subtitle <- paste(strwrap(subtitle, width = 48), collapse = "\n")
  }
  plot +
    ggplot2::labs(title = title, subtitle = subtitle, tag = label) +
    ggplot2::theme(
      plot.title.position = "panel",
      plot.title = ggplot2::element_text(
        family = FIGURE_FONT_FAMILY, hjust = 0.5,
        size = FIGURE_TITLE_SIZE, face = "plain",
        margin = ggplot2::margin(b = 2.5)
      ),
      plot.subtitle = ggplot2::element_text(
        family = FIGURE_FONT_FAMILY, hjust = 0.5,
        size = FIGURE_SUBTITLE_SIZE, colour = "grey25",
        lineheight = 0.95, margin = ggplot2::margin(b = 3.5)
      ),
      plot.tag.location = "panel",
      plot.tag.position = c(0, 1),
      plot.tag = ggplot2::element_text(
        family = FIGURE_FONT_FAMILY, hjust = 0, vjust = 0,
        size = FIGURE_PANEL_LABEL_SIZE, face = "bold",
        margin = ggplot2::margin(t = 0, r = 0, b = 2, l = 0)
      ),
      # Room above the panel for the label's ascender.
      plot.margin = ggplot2::margin(t = 14, r = 6, b = 4, l = 4)
    )
}
