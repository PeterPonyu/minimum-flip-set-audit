# Turning per-answer rows into the paired tables the contrasts are computed on.
#
# Nothing here decides anything. It reshapes the rows, refuses a reshape that
# would silently drop or duplicate an answer, and hands back a table of matched
# pairs. Every contrast in this paper is a function of one such table.

CONDITIONS <- c("no_kg", "text_kg", "multimodal_kg")

# The rows arrive as one object per answer. A question is only usable if it was
# answered under all three conditions, because the contrasts are within-question;
# a question missing one condition would otherwise contribute to one arm of a
# pair and not the other.
as_wide <- function(rows, label) {
  needed <- c("question_id", "condition", "correct", "image_load_bearing")
  missing <- setdiff(needed, names(rows))
  if (length(missing)) stop(label, ": the records are missing ", paste(missing, collapse = ", "))
  if (anyNA(rows$correct)) stop(label, ": an answer is recorded without a grading")
  if (!all(rows$condition %in% CONDITIONS)) {
    stop(label, ": the records contain a condition this study does not know")
  }

  ids <- sort(unique(rows$question_id))
  wide <- data.frame(question_id = ids, stringsAsFactors = FALSE)
  for (condition in CONDITIONS) {
    part <- rows[rows$condition == condition, ]
    if (anyDuplicated(part$question_id)) {
      stop(label, ": ", condition, " answers the same question more than once")
    }
    idx <- match(ids, part$question_id)
    if (anyNA(idx)) stop(label, ": ", condition, " does not cover every question")
    wide[[condition]] <- as.integer(part$correct[idx])
  }

  # The subset flag is a property of the question, so the three conditions must
  # agree on it. If they did not, a question could sit in one subset for one
  # condition and another for the next, and the pairing would be across subsets.
  flags <- tapply(rows$image_load_bearing, rows$question_id, function(x) length(unique(x)))
  if (any(flags != 1L)) stop(label, ": the conditions disagree about which subset a question is in")
  wide$image_load_bearing <- rows$image_load_bearing[match(ids, rows$question_id)]
  wide
}

subset_rows <- function(wide, which) {
  keep <- if (identical(which, "image_load_bearing")) wide$image_load_bearing else !wide$image_load_bearing
  wide[keep, , drop = FALSE]
}

# The four cells of the paired table, in the order the tests below expect. This
# is the only representation the fragility search works on: two arms and n
# questions collapse to four counts, and every quantity this paper reports is a
# function of those four.
paired_cells <- function(wide, arm, reference) {
  a <- wide[[arm]]
  b <- wide[[reference]]
  c(both = sum(a & b), arm_only = sum(a & !b), ref_only = sum(!a & b), neither = sum(!a & !b))
}

accuracy <- function(cells, side = c("arm", "reference")) {
  side <- match.arg(side)
  hits <- if (side == "arm") cells[["both"]] + cells[["arm_only"]] else cells[["both"]] + cells[["ref_only"]]
  hits / sum(cells)
}

acc_diff_pt <- function(cells) 100 * (cells[["arm_only"]] - cells[["ref_only"]]) / sum(cells)

discordant <- function(cells) cells[["arm_only"]] + cells[["ref_only"]]

# A recorded contrast must be a restatement of the rows it claims to summarise.
# Both halves are checked: the size of the difference, and the exact p-value the
# record printed, recomputed here from the four cells.
assert_recorded_contrast <- function(cells, recorded, label, tol = 1e-3) {
  if (!identical(as.integer(recorded$n), as.integer(sum(cells)))) {
    stop(label, ": the record counts a different number of questions than the rows hold")
  }
  if (abs(acc_diff_pt(cells) - recorded$acc_diff_pt) > tol) {
    stop(label, ": the recorded difference does not come from the rows")
  }
  if (abs(mcnemar_exact_p(cells) - recorded$mcnemar_exact_p) > 1e-9) {
    stop(label, ": the recorded p-value does not come from the rows")
  }
  invisible(TRUE)
}
