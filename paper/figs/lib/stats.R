# The tests this paper re-computes, and the two limits that bound what they
# could ever have said.
#
# The design is paired: the same questions are answered under every condition, so
# a contrast is decided by the questions the two conditions disagree on and by
# nothing else. Both limits below follow from that, and neither depends on how
# the answers came out.

# Two-sided exact McNemar: a binomial test at one half on the discordant pairs.
# Two conditions that never disagree carry no evidence either way, which is
# reported as a p-value of one rather than as an error.
mcnemar_exact_p <- function(cells) {
  disc <- discordant(cells)
  if (disc == 0L) return(1)
  stats::binom.test(cells[["arm_only"]], disc, 0.5)$p.value
}

# The smallest p-value the test could return on this many discordant pairs, which
# is what it returns when every one of them falls the same way. It is fixed by
# the count alone, so a contrast whose floor is above the threshold was never
# capable of being significant and its non-significance says nothing about the
# conditions being compared.
attainable_floor <- function(disc) if (disc == 0L) 1 else min(1, 2 * 0.5^disc)

# The threshold this paper reads the recorded verdicts against. It is the one the
# records themselves used; it is not chosen here.
holm_adjusted <- function(p) stats::p.adjust(p, method = "holm")

# Clopper--Pearson on the discordant split, mapped onto the paired difference in
# accuracy. This is a conditional interval given the observed discordant count;
# it is recomputed so the manuscript can print the construction it can state.
# When the observed discordant count is zero, the conditional split is empty and
# the returned [0, 0] is a bookkeeping marker rather than an unconditional
# interval for a population paired difference.
exact_paired_interval <- function(cells) {
  disc <- discordant(cells)
  n <- sum(cells)
  if (disc == 0L) return(c(0, 0))
  fit <- stats::binom.test(cells[["arm_only"]], disc, 0.5)$conf.int
  (2 * c(fit[1], fit[2]) - 1) * disc / n
}
