# How many gradings a conclusion rests on.
#
# One answer is graded right or wrong. Change one grading and every quantity
# derived from it moves. This file asks the smallest version of that question:
# what is the fewest gradings that would have to have come out the other way for
# a stated conclusion to stop holding?
#
# The search is exact rather than approximate. A paired contrast collapses to
# four counts, one grading moves a question from one count to another, and the
# reachable states form a small graph, so a breadth-first walk over it returns
# the true minimum and not a bound on it.

# The eight ways one grading can move a question between cells. Each is one
# flip; the label is what a reader would have to accept for it to have happened.
FLIP_MOVES <- list(
  list(from = "both",     to = "ref_only", side = "arm",
       label = "an answer both conditions got right, marked wrong for this condition"),
  list(from = "arm_only", to = "neither",  side = "arm",
       label = "an answer only this condition got right, marked wrong"),
  list(from = "ref_only", to = "both",     side = "arm",
       label = "an answer only the reference got right, marked right for this condition too"),
  list(from = "neither",  to = "arm_only", side = "arm",
       label = "an answer neither got right, marked right for this condition"),
  list(from = "both",     to = "arm_only", side = "reference",
       label = "an answer both conditions got right, marked wrong for the reference"),
  list(from = "arm_only", to = "both",     side = "reference",
       label = "an answer only this condition got right, marked right for the reference too"),
  list(from = "ref_only", to = "neither",  side = "reference",
       label = "an answer only the reference got right, marked wrong"),
  list(from = "neither",  to = "ref_only", side = "reference",
       label = "an answer neither got right, marked right for the reference")
)

apply_move <- function(cells, move) {
  cells[[move$from]] <- cells[[move$from]] - 1L
  cells[[move$to]] <- cells[[move$to]] + 1L
  cells
}

# Breadth-first over cell states. The frontier is small -- the state is four
# non-negative counts summing to n -- so this terminates quickly and the first
# state satisfying the predicate is at the true minimum distance.
min_flips <- function(cells, holds) {
  if (holds(cells)) return(list(k = 0L, moves = character(0)))
  key <- function(x) paste(x, collapse = ",")
  seen <- new.env(hash = TRUE, parent = emptyenv())
  assign(key(cells), TRUE, envir = seen)
  frontier <- list(list(cells = cells, moves = character(0)))
  while (length(frontier)) {
    nxt <- list()
    for (node in frontier) {
      for (move in FLIP_MOVES) {
        if (node$cells[[move$from]] < 1L) next
        moved <- apply_move(node$cells, move)
        k <- key(moved)
        if (!is.null(seen[[k]])) next
        assign(k, TRUE, envir = seen)
        path <- c(node$moves, move$label)
        if (holds(moved)) return(list(k = length(path), moves = path))
        nxt[[length(nxt) + 1L]] <- list(cells = moved, moves = path)
      }
    }
    frontier <- nxt
  }
  stop("no sequence of grading changes reaches the stated condition")
}

# The two conclusions a contrast can carry, and what it takes to unseat each.
#
# They are separate questions and this paper's point is that they can have very
# different answers. The direction of a difference is unseated by moving the
# difference to zero; the verdict at the threshold is unseated by moving the
# p-value across it, in whichever direction it currently sits.
sign_fragility <- function(cells) {
  d <- cells[["arm_only"]] - cells[["ref_only"]]
  if (d == 0L) return(list(k = 0L, moves = character(0)))
  holds <- if (d > 0) function(x) x[["arm_only"]] <= x[["ref_only"]] else
    function(x) x[["arm_only"]] >= x[["ref_only"]]
  min_flips(cells, holds)
}

verdict_fragility <- function(cells, alpha) {
  significant <- mcnemar_exact_p(cells) <= alpha
  holds <- if (significant) function(x) mcnemar_exact_p(x) > alpha else
    function(x) mcnemar_exact_p(x) <= alpha
  min_flips(cells, holds)
}

# Which answers a one-grading conclusion is resting on.
#
# When the minimum is one, the paper should be able to name the answers rather
# than assert a count, so this enumerates every single grading whose change
# would do it. More than one candidate does not make the conclusion sturdier: it
# means there are several answers any one of which carries it alone.
single_flip_candidates <- function(wide, arm, reference, holds) {
  cells <- paired_cells(wide, arm, reference)
  found <- data.frame(question_id = character(0), condition = character(0),
                      stringsAsFactors = FALSE)
  for (i in seq_len(nrow(wide))) {
    for (condition in c(arm, reference)) {
      altered <- wide
      altered[[condition]][i] <- 1L - altered[[condition]][i]
      if (holds(paired_cells(altered, arm, reference))) {
        found <- rbind(found, data.frame(question_id = wide$question_id[i],
                                         condition = condition, stringsAsFactors = FALSE))
      }
    }
  }
  found
}

# Dropping one question at a time and asking whether the conclusion survives.
#
# This measures something different from the flip distance: a flip asks what a
# different grading would have done, a deletion asks what a different question
# set would have done. A conclusion can be robust to one and fragile to the other.
leave_one_out <- function(wide, arm, reference, alpha) {
  cells <- paired_cells(wide, arm, reference)
  d0 <- sign(cells[["arm_only"]] - cells[["ref_only"]])
  sig0 <- mcnemar_exact_p(cells) <= alpha
  same_sign <- 0L
  same_verdict <- 0L
  for (i in seq_len(nrow(wide))) {
    cut <- paired_cells(wide[-i, , drop = FALSE], arm, reference)
    if (sign(cut[["arm_only"]] - cut[["ref_only"]]) == d0) same_sign <- same_sign + 1L
    if ((mcnemar_exact_p(cut) <= alpha) == sig0) same_verdict <- same_verdict + 1L
  }
  list(n = nrow(wide), same_sign = same_sign, same_verdict = same_verdict)
}
