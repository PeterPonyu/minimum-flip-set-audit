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

# The same two distances, reached without searching.
#
# Everything below is a second route to the numbers the search already returns.
# It exists so the derivation in the methods section can be checked rather than
# believed: the two procedures share no code, the build runs both on every
# contrast, and it stops if they ever part. The search remains the definition
# and is what produces the reported values.

# The p-value depends on the discordant split and on nothing else, so it can be
# asked for directly rather than through a table that carries two counts the
# test will not read.
split_p <- function(b, c) {
  mcnemar_exact_p(c(both = 0L, arm_only = as.integer(b), ref_only = as.integer(c),
                    neither = 0L))
}

# The direction needs no search at all. Unseating it costs one flip per unit of
# the lead, and the lead is the numerator of the difference already reported.
sign_distance <- function(cells) {
  abs(cells[["arm_only"]] - cells[["ref_only"]])
}

# The smallest majority among m discordant pairs that the exact test calls
# significant, tabulated for every m up to n at once. The value is
# non-decreasing in m and rises by at most one at a time, so one pass suffices;
# both properties are checked rather than assumed, because they are what makes
# the pass linear.
critical_majority <- function(n, alpha) {
  out <- rep(NA_integer_, n + 1L)
  w <- 0L
  for (m in 0:n) {
    w <- max(w, as.integer(ceiling(m / 2)))
    while (w <= m && split_p(w, m - w) > alpha) w <- w + 1L
    if (w <= m) {
      previous <- if (m > 0L) out[[m]] else NA_integer_
      if (!is.na(previous) && (w < previous || w > previous + 1L)) {
        stop("the critical majority is not stepping by one; the linear pass is unsound")
      }
      out[[m + 1L]] <- w
    } else {
      w <- 0L
    }
  }
  out
}

# The verdict distance, as a minimum over one number rather than a walk over
# tables. For a target discordant count the cost is convex and piecewise linear
# in how the pairs split, and it is flat at |m - m'| between the two split
# points that preserve one coordinate, so the cheapest admissible split is
# whichever endpoint of that flat stretch survives clamping. NA where no
# reachable table carries the other verdict.
verdict_distance <- function(cells, alpha) {
  b <- cells[["arm_only"]]
  c_ <- cells[["ref_only"]]
  n <- sum(cells)
  significant <- mcnemar_exact_p(cells) <= alpha
  majority <- critical_majority(n, alpha)

  cost <- function(m2, b2) abs(b - b2) + abs(c_ - (m2 - b2))
  cheapest_in <- function(m2, lo, hi) {
    lo <- max(0L, as.integer(lo))
    hi <- min(as.integer(m2), as.integer(hi))
    if (lo > hi) return(NA_integer_)
    flat <- sort(c(b, m2 - c_))
    as.integer(min(cost(m2, min(max(flat[[1]], lo), hi)),
                   cost(m2, min(max(flat[[2]], lo), hi))))
  }

  best <- NA_integer_
  for (m2 in 0:n) {
    w <- majority[[m2 + 1L]]
    here <- if (significant) {
      # Any split the test would not call significant will do. Where no split
      # of m2 pairs could be significant, that is all of them.
      if (is.na(w)) cheapest_in(m2, 0L, m2) else cheapest_in(m2, m2 - w + 1L, w - 1L)
    } else if (is.na(w)) {
      NA_integer_
    } else {
      # Either side may carry the majority, so both rays are candidates.
      candidates <- c(cheapest_in(m2, w, m2), cheapest_in(m2, 0L, m2 - w))
      candidates <- candidates[!is.na(candidates)]
      if (length(candidates)) min(candidates) else NA_integer_
    }
    if (!is.na(here) && (is.na(best) || here < best)) best <- as.integer(here)
  }
  best
}

# The smallest number of discordant pairs on which the test can return anything
# below the threshold. A paired design with fewer questions than this cannot
# produce a significant result however its answers come out.
significance_needs <- function(alpha) {
  m <- 0L
  while (attainable_floor(m) > alpha) m <- m + 1L
  m
}

# The scan above stops at the first majority the test calls significant and
# treats every larger one as significant too. That is a property of the binomial
# tail and not of this code, so it is checked over the whole range in use rather
# than relied on.
assert_tail_monotone <- function(n) {
  for (m in 0:n) {
    previous <- NULL
    for (w in seq.int(ceiling(m / 2), max(m, ceiling(m / 2)))) {
      here <- split_p(w, m - w)
      if (!is.null(previous) && here > previous + 1e-12) {
        stop("the exact p-value is not decreasing in the majority at m = ", m)
      }
      previous <- here
    }
  }
  invisible(TRUE)
}

# Run both routes against each other. Called once per contrast, so a derivation
# that stops matching the search stops the build instead of reaching the page.
assert_closed_form <- function(cells, alpha, searched_sign, searched_verdict, label) {
  if (!identical(as.integer(searched_sign), as.integer(sign_distance(cells)))) {
    stop(label, ": the search and the closed form disagree about the direction")
  }
  if (!identical(as.integer(searched_verdict), as.integer(verdict_distance(cells, alpha)))) {
    stop(label, ": the search and the closed form disagree about the verdict")
  }
  invisible(TRUE)
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
