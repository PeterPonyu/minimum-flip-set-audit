# Figure entry point for 006. Emits into figs/out/.
# Refuses to draw anything that is not bound in evidence/evidence_manifest.json.
#
# Side effect by design: this script also writes tex/generated_numbers.tex and
# the generated result tables. Every quantity the manuscript prints comes from
# here, so prose cannot drift away from the bytes that were hashed.
#
# The work is split so that each file has one reason to change: figs/lib holds
# reading, reshaping, statistics, the fragility search and formatting; figs/panels
# holds one figure each; this file holds the order they run in and the checks that
# must pass before any of them run.
#
# Run from the paper directory:  Rscript figs/make_figs.R

suppressPackageStartupMessages({
  library(ggplot2)
  library(jsonlite)
})

for (unit in c("rtx_theme.R", "lib/evidence.R", "lib/records.R", "lib/stats.R",
               "lib/fragility.R", "lib/emit.R")) {
  source(file.path("figs", unit))
}

dir.create(file.path("figs", "out"), showWarnings = FALSE, recursive = TRUE)

manifest <- load_manifest()
read_bound <- evidence_reader(manifest, find_repo_root())

ALPHA <- 0.05

famous_rows <- read_bound$records("E-FAMOUS")
longtail_rows <- read_bound$records("E-LONGTAIL")
famous_summary <- read_bound$json("E-FAMOUS-SUMMARY")
longtail_summary <- read_bound$json("E-LONGTAIL-SUMMARY")
recorded <- read_bound$json("E-STATS")
regrade <- read_bound$json("E-REGRADE")
audit_s1 <- read_bound$json("E-AUDIT-S1")
audit_s2 <- read_bound$json("E-AUDIT-S2")
audit_s3 <- read_bound$json("E-AUDIT-S3")
audit_s4 <- read_bound$json("E-AUDIT-S4")
measure <- read_bound$json("E-MEASURE")
executed <- read_bound$json("E-EXECUTED")
retrieval <- read_bound$json("E-RETRIEVAL")
tier_static <- read_bound$json("E-STATIC")
tier_naive <- read_bound$json("E-NAIVE")
tier_simple <- read_bound$json("E-SIMPLE")
tier_primary <- read_bound$json("E-PRIMARY")
tier_sota <- read_bound$json("E-SOTA")
blocker <- read_bound$json("E-BLOCKER")

# The summary panel and its prose use the same grader-conditioned rows.  Keep
# the selected claims explicit so a changed audit record fails the build rather
# than silently changing the meaning of the 1/6/4 summary.
summary_row <- function(grader, label) {
  rows <- audit_s2$per_grader[[grader]]$contrasts
  row <- rows[rows$label == label, ]
  if (nrow(row) != 1L) stop("no unique summary row for ", grader, "/", label)
  row
}
SUMMARY_HARM_STRICT <- summary_row("strict", "Famous, image | multimodal vs text")$kappa_dir[[1]]
SUMMARY_HARM_NUMERIC <- summary_row("numeric", "Famous, image | multimodal vs text")$kappa_dir[[1]]
SUMMARY_HELP_STRICT <- summary_row("strict", "Long-tail, image | multimodal vs text")$kappa_dir[[1]]
SUMMARY_HELP_NUMERIC <- summary_row("numeric", "Long-tail, image | multimodal vs text")$kappa_dir[[1]]
SUMMARY_SIG_STRICT <- summary_row("strict", "Long-tail, text | text vs none")$kappa_ver[[1]]
SUMMARY_SIG_NUMERIC <- summary_row("numeric", "Long-tail, text | text vs none")$kappa_ver[[1]]
if (!identical(as.integer(c(SUMMARY_HARM_STRICT, SUMMARY_HELP_STRICT, SUMMARY_SIG_STRICT)), c(1L, 6L, 4L)) ||
    !identical(as.integer(c(SUMMARY_HARM_NUMERIC, SUMMARY_HELP_NUMERIC, SUMMARY_SIG_NUMERIC)), c(0L, 6L, 2L))) {
  stop("the grader-conditioned minimum-flip summary no longer matches the registered 1/6/4 -> 0/6/2 record")
}

## ---------------------------------------------------------------------------
## The rows, reshaped into the paired tables every contrast is computed on.
## ---------------------------------------------------------------------------

WIDE <- list(famous = as_wide(famous_rows, "famous"),
             longtail = as_wide(longtail_rows, "long-tail"))

SUBSETS <- list(image_load_bearing = "image_load_bearing", text_only = "text_only",
                overall = "overall")

subset_of <- function(dataset, subset) {
  wide <- WIDE[[dataset]]
  if (identical(subset, "overall")) wide else subset_rows(wide, subset)
}

# The contrast the alphabetically-first arm makes against the reference is not
# the one the record reports; the record fixes both the pairs and their order, so
# the order is taken from it rather than chosen here.
CONTRASTS <- list(c("multimodal_kg", "text_kg"), c("text_kg", "no_kg"),
                  c("multimodal_kg", "no_kg"))

recorded_contrast <- function(dataset, subset, arm, reference) {
  frame <- recorded$datasets[[dataset]]$subsets[[subset]]$contrasts
  row <- frame[frame$contrast == paste(arm, "-", reference), ]
  if (nrow(row) != 1L) {
    stop("no unique recorded contrast for ", dataset, "/", subset, "/", arm)
  }
  list(n = row$n_pairs[[1]],
       acc_diff_pt = row$acc_diff_pt[[1]],
       mcnemar_exact_p = row$mcnemar_exact_p[[1]],
       significant = row$`significant_at_0.05`[[1]],
       discordant = row$discordant_pairs$n_discordant[[1]],
       arm_only = row$discordant_pairs$b_a_right_b_wrong[[1]],
       ref_only = row$discordant_pairs$c_a_wrong_b_right[[1]])
}

## ---------------------------------------------------------------------------
## Checks that must hold before anything is drawn. Each one is a sentence the
## manuscript makes, enforced here so it cannot survive the evidence changing.
## ---------------------------------------------------------------------------

# The study re-reads records that were written by someone else. Every contrast it
# discusses is recomputed from the rows and checked against what was recorded, so
# a disagreement stops the build instead of producing a paper about the wrong
# numbers.
inventory <- list()
for (dataset in names(WIDE)) {
  for (subset in names(SUBSETS)) {
    rows <- subset_of(dataset, subset)
    for (pair in CONTRASTS) {
      arm <- pair[[1]]; reference <- pair[[2]]
      cells <- paired_cells(rows, arm, reference)
      record <- recorded_contrast(dataset, subset, arm, reference)
      label <- paste(dataset, subset, arm, "vs", reference)
      assert_recorded_contrast(cells, record, label)

      # The four cells are the whole state the tests see, so the two the record
      # names separately are checked separately: a table that matched on the
      # difference but not on the split would give the same p by coincidence.
      if (!identical(as.integer(cells[["arm_only"]]), as.integer(record$arm_only)) ||
          !identical(as.integer(cells[["ref_only"]]), as.integer(record$ref_only))) {
        stop(label, ": the recorded discordant split does not come from the rows")
      }
      if (!identical(mcnemar_exact_p(cells) <= ALPHA, record$significant)) {
        stop(label, ": the recorded verdict does not follow from the recorded test")
      }

      # Both distances are found by exhaustive search, and both are also
      # derivable in closed form. The manuscript states the derivation, so the
      # build checks it against the search on every contrast rather than
      # asking a reader to take it on trust.
      k_sign <- sign_fragility(cells)$k
      k_verdict <- verdict_fragility(cells, ALPHA)$k
      assert_closed_form(cells, ALPHA, k_sign, k_verdict, label)

      loo <- leave_one_out(rows, arm, reference, ALPHA)
      inventory[[length(inventory) + 1L]] <- data.frame(
        dataset = dataset, subset = subset, arm = arm, reference = reference,
        n = sum(cells), discordant = discordant(cells),
        both = cells[["both"]], neither = cells[["neither"]],
        arm_only = cells[["arm_only"]], ref_only = cells[["ref_only"]],
        acc_arm = accuracy(cells, "arm"), acc_ref = accuracy(cells, "reference"),
        diff_pt = acc_diff_pt(cells), p = mcnemar_exact_p(cells),
        significant = mcnemar_exact_p(cells) <= ALPHA,
        floor = attainable_floor(discordant(cells)),
        k_sign = k_sign, k_verdict = k_verdict,
        loo_sign = loo$same_sign, loo_verdict = loo$same_verdict,
        stringsAsFactors = FALSE)
    }
  }
}
inventory <- do.call(rbind, inventory)
inventory$capable <- inventory$floor <= ALPHA

# The methods section derives a bound on the number of questions a paired design
# needs before significance is available to it at all, and reduces the verdict
# search to one pass by leaning on the binomial tail. Both are checked over the
# range this study actually uses.
assert_tail_monotone(max(inventory$n))
SIGNIFICANCE_NEEDS <- significance_needs(ALPHA)
if (!identical(SIGNIFICANCE_NEEDS, as.integer(ceiling(log2(2 / ALPHA))))) {
  stop("the derived bound on questions needed for significance does not match the tabulated floor")
}

# A difference of exactly zero has no direction to reverse, so its distance to a
# reversal is zero by definition and not by fragility. Those contrasts are kept
# in the tables and the figures -- they are part of the study -- but every count
# the manuscript makes about directions excludes them, because including them
# would report a definition as a finding.
inventory$degenerate <- inventory$k_sign == 0L

# Sitting at the floor means every discordant pair fell the same way. Two
# conditions that never disagreed have no discordant pair to fall any way, so
# they are not at their floor in the sense the manuscript uses; they are outside
# the statement.
inventory$at_floor <- inventory$discordant > 0L &
  abs(inventory$p - inventory$floor) < 1e-12

inventory$holm <- holm_adjusted(inventory$p)
inventory$survives_holm <- inventory$holm <= ALPHA

# The two thresholds the prose quotes when it summarises the family. They live
# here rather than in the sentences, so a sentence and the count it reports
# cannot come apart.
DECOUPLE_GAP <- 4L
FRAGILE_MAX <- 2L
inventory$decoupled <- !inventory$degenerate &
  abs(inventory$k_sign - inventory$k_verdict) >= DECOUPLE_GAP

# Two sentences about the family as a whole, kept as checks because they are the
# kind of statement that stays in a manuscript after the rows underneath it have
# moved. The first: every contrast that survives the adjustment is a difference
# against no retrieval, so none of them is the comparison the evaluation was
# built to make. The second: the contrasts with a fragile direction and those
# with a fragile verdict are disjoint, which is what lets the manuscript say the
# two distances are separate questions rather than one restated.
if (!all(inventory$reference[inventory$survives_holm] == "no_kg")) {
  stop("a contrast between two retrieval conditions now survives the adjustment")
}
if (any(!inventory$degenerate & inventory$k_sign <= FRAGILE_MAX &
        inventory$k_verdict <= FRAGILE_MAX)) {
  stop("a contrast is now fragile in both its direction and its verdict")
}

# The record states the multiplicity it carries. If the count of contrasts it
# describes and the count this study recomputed ever diverge, the adjustment
# below would be applied to a different family than the one that was reported.
RECORDED_FAMILY <- as.integer(sub("^.*with ([0-9]+) contrasts.*$", "\\1",
                                  recorded$methods$multiplicity))
if (!identical(nrow(inventory), RECORDED_FAMILY)) {
  stop("the recorded multiplicity describes a different number of contrasts than were recomputed")
}

# The two summaries are one-line restatements of the rows in their own run
# directory. They are the project's own account of what it measured, so the
# manuscript may quote them only while they still restate it.
for (dataset in names(WIDE)) {
  recorded_summary <- if (dataset == "famous") famous_summary else longtail_summary
  for (subset in c("image_load_bearing", "text_only")) {
    rows <- subset_of(dataset, subset)
    for (condition in CONDITIONS) {
      got <- mean(rows[[condition]])
      want <- recorded_summary$by_subset[[subset]][[condition]]$accuracy
      if (!isTRUE(all.equal(got, want))) {
        stop(dataset, "/", subset, "/", condition,
             ": the run summary does not restate the rows it summarises")
      }
    }
  }
  if (!identical(as.integer(recorded_summary$run_meta$n_questions),
                 as.integer(nrow(WIDE[[dataset]])))) {
    stop(dataset, ": the run summary counts a different number of questions than the rows hold")
  }
}

# The measurement table is the object the project fixed as its result. This paper
# is about how few gradings it rests on, which is only worth saying while the
# table it rests on is the one on disk.
for (side in c("famous_lb", "longtail_lb")) {
  dataset <- if (side == "famous_lb") "famous" else "longtail"
  rows <- subset_of(dataset, "image_load_bearing")
  for (condition in CONDITIONS) {
    if (!isTRUE(all.equal(mean(rows[[condition]]), measure[[side]][[condition]]$accuracy))) {
      stop(side, "/", condition, ": the fixed measurement table does not come from the rows")
    }
  }
}

# Three graders scored the same answers. The manuscript reports the disagreement
# only in aggregate, so the build checks that there is exactly one contested
# grading in the declared subset and condition without emitting its identifier or
# answer strings into reader-facing TeX.
GRADERS <- sort(unique(regrade$three_grader_table$grader))
if (!identical(GRADERS, c("numeric", "strict", "substr"))) {
  stop("the regrade was not run under the three graders the manuscript names")
}

FLIPPED <- regrade$flips_strict_to_numeric_image
flip_ids <- unlist(FLIPPED, use.names = TRUE)
if (length(flip_ids) != 1L) {
  stop("the regrade record no longer contains exactly one contested grading")
}
if (!identical(unname(flip_ids), regrade$i17_flip$question_id) ||
    !identical(regrade$i17_flip$dataset, "famous") ||
    !identical(regrade$i17_flip$condition, "multimodal_kg")) {
  stop("the contested grading is not the one the regrade record describes")
}
if (!identical(regrade$i17_flip$stored_correct, FALSE) ||
    !identical(regrade$i17_flip$numeric, TRUE)) {
  stop("the contested grading no longer disagrees between the stored and numeric graders")
}

# The stored regrade JSON enumerates image-subset flips only, which is why the
# manuscript originally reported one disagreement. The later count over all 228
# answers is the one the prose now prints.
cf <- audit_s1$closed_form_verification
if (!identical(as.integer(cf$total_mismatches), 0L)) {
  stop("the closed form no longer agrees with the search on every four-cell state")
}
if (!isTRUE(audit_s1$stored_contrasts$all_18_reproduce)) {
  stop("the stored 18 contrasts no longer reproduce from the rows")
}
nested_n <- as.integer(audit_s2$disagreements_among_three_original_rules$count)
if (!identical(nested_n, 4L)) {
  stop("the three nested rules no longer disagree on four of 228 answers")
}
numeric_lt <- audit_s2$contrast_rows_that_change_vs_strict
numeric_lt <- numeric_lt[numeric_lt$grader == "numeric" &
                           numeric_lt$label == "Long-tail, text | text vs none", ]
if (nrow(numeric_lt) != 1L) {
  stop("no unique numeric-grader row for the long-tail text pair")
}
if (!isTRUE(numeric_lt$strict$survives_holm_18[[1]])) {
  stop("under the stored grader the long-tail text pair no longer survives Holm")
}
if (isTRUE(numeric_lt$under_grader$survives_holm_18[[1]])) {
  stop("under the numeric grader the long-tail text pair still survives Holm-18")
}
if (as.integer(audit_s2$per_grader$numeric$summary$n_survive_holm_18) != 1L) {
  stop("the numeric grader no longer leaves one Holm-18 survivor")
}
if (!identical(as.integer(audit_s3$judges$n_unparsed), c(0L, 0L))) {
  stop("an LLM judge left unparsed verdicts")
}
if (!identical(as.integer(audit_s4$design$n_tables), 840000L)) {
  stop("the simulation no longer contains 840000 tables")
}
inv38 <- cf$per_n[["38"]]$monotonicity$significant_tables$adjacent_inversions_of_kappa_ver_in_p_order_min_max_over_tie_breaks
if (length(inv38) != 2L || inv38[[1]] > inv38[[2]]) {
  stop("n = 38 significant-table inversions of kappa_ver are missing")
}
grader_splits <- unique(regrade$three_grader_table[, c("dataset", "subset")])
n_moving_rows <- 0L
for (i in seq_len(nrow(grader_splits))) {
  block <- regrade$three_grader_table[
    regrade$three_grader_table$dataset == grader_splits$dataset[[i]] &
      regrade$three_grader_table$subset == grader_splits$subset[[i]], ]
  strict_row <- block[block$grader == "strict", ]
  if (nrow(strict_row) != 1L) {
    stop("no unique strict row in the three-grader table")
  }
  acc_strict <- as.numeric(strict_row[, c("no_kg", "text_kg", "multimodal_kg")])
  acc <- as.matrix(block[, c("no_kg", "text_kg", "multimodal_kg")])
  n_moving_rows <- n_moving_rows +
    sum(rowSums(abs(acc - matrix(acc_strict, nrow(acc), 3, byrow = TRUE))) > 1e-12)
}
if (!identical(as.integer(n_moving_rows), 2L)) {
  stop("the three-grader table no longer has exactly two rows that move")
}

for (dataset in names(WIDE)) {
  agreement <- regrade$grader_agreement[[dataset]]
  if (!identical(as.integer(agreement$agree), as.integer(agreement$n))) {
    stop(dataset, ": the graders no longer reproduce the stored gradings they are checked against")
  }
  if (!identical(as.integer(agreement$n), as.integer(3L * nrow(WIDE[[dataset]])))) {
    stop(dataset, ": the regrade covers a different number of answers than the rows hold")
  }
}

# The value the project recorded as not to be used must still be recorded that
# way. This paper names it only to say it is excluded, which is a claim about the
# record and fails if the record changes.
UNUSED <- executed$numbers_quoted[executed$numbers_quoted$field ==
                                    "hand_authored_famous_lb_multimodal_unused", ]
if (nrow(UNUSED) != 1L || !identical(UNUSED$used[[1]], FALSE) ||
    !identical(executed$used_hand_authored_94_4, FALSE)) {
  stop("the declined caption source is no longer recorded as declined")
}

# The record's own reading of the difference this paper keeps returning to. The
# manuscript says the records report it as not established; that is a statement
# about the record, so it fails when the record stops saying it.
if (!grepl("NOT an established help", measure$claim, fixed = TRUE) ||
    !isTRUE(measure$do_not_mix_with_mmgraphrag_76_8)) {
  stop("the measurement table no longer states what it declines to establish")
}

# The tiers that would hold a benchmark result are absent, and the paper says so.
# Saying so is only honest while they are still absent.
if (!identical(tier_static$status, "missing") || !identical(tier_sota$status, "blocked") ||
    !identical(tier_primary$status, "partial")) {
  stop("a tier this paper reports as absent or incomplete has changed state")
}
if (isTRUE(blocker$forged_scores) || isTRUE(blocker$checks$ollama_reachable)) {
  stop("the blocked component is no longer blocked in the way the paper reports")
}
if (isTRUE(retrieval$import_pin$index_path_ready)) {
  stop("the retrieval footnote is no longer a footnote; its index state has changed")
}

## ---------------------------------------------------------------------------
## The three conclusions the card names, each with the question it asks.
## ---------------------------------------------------------------------------

# A conclusion is a claim about a contrast, not a contrast. The same four cells
# carry two of them -- which condition is ahead, and whether a test calls the
# difference real -- and this study's finding is that they can be very different
# distances from being overturned.
conclusion <- function(key, dataset, subset, arm, reference, kind, statement) {
  rows <- subset_of(dataset, subset)
  cells <- paired_cells(rows, arm, reference)
  fragility <- if (kind == "sign") sign_fragility(cells) else verdict_fragility(cells, ALPHA)
  holds <- if (kind == "sign") {
    d <- cells[["arm_only"]] - cells[["ref_only"]]
    if (d > 0) function(x) x[["arm_only"]] <= x[["ref_only"]] else
      function(x) x[["arm_only"]] >= x[["ref_only"]]
  } else if (mcnemar_exact_p(cells) <= ALPHA) {
    function(x) mcnemar_exact_p(x) > ALPHA
  } else {
    function(x) mcnemar_exact_p(x) <= ALPHA
  }
  candidates <- if (fragility$k == 1L) {
    single_flip_candidates(rows, arm, reference, holds)
  } else {
    data.frame(question_id = character(0), condition = character(0),
               stringsAsFactors = FALSE)
  }
  list(key = key, dataset = dataset, subset = subset, arm = arm, reference = reference,
       kind = kind, statement = statement, cells = cells, n = sum(cells),
       diff_pt = acc_diff_pt(cells), p = mcnemar_exact_p(cells),
       k = fragility$k, moves = fragility$moves, candidates = candidates,
       gradings = 2L * nrow(rows), loo = leave_one_out(rows, arm, reference, ALPHA))
}

CONCLUSIONS <- list(
  conclusion("Harm", "famous", "image_load_bearing", "multimodal_kg", "text_kg", "sign",
             "on the famous set, the multimodal condition is behind the text-only one"),
  conclusion("Help", "longtail", "image_load_bearing", "multimodal_kg", "text_kg", "sign",
             "on the long-tail set, the multimodal condition is ahead of the text-only one"),
  conclusion("Sig", "longtail", "text_only", "text_kg", "no_kg", "verdict",
             "on the long-tail text-only subset, the text condition beats no retrieval at the threshold")
)
names(CONCLUSIONS) <- vapply(CONCLUSIONS, function(x) x$key, character(1))

# The verdict on the long-tail difference is the paper's second case and is a
# separate question from its direction, so it is computed as its own conclusion
# over the same four cells.
CONCLUSIONS$HelpVerdict <- conclusion(
  "HelpVerdict", "longtail", "image_load_bearing", "multimodal_kg", "text_kg", "verdict",
  "the long-tail difference is not significant at the threshold")

# The prespecified kill. The card fixed it before the search was written: if
# every one of the three conclusions needs five or more gradings, the claim that
# they are single-record fragile does not hold and the card is void. The
# threshold is read from the card, not restated here as a second copy.
KILL_THRESHOLD <- 5L
PREREG_KS <- vapply(CONCLUSIONS[c("Harm", "Help", "Sig")], function(x) x$k, integer(1))
KILL_FIRES <- all(PREREG_KS >= KILL_THRESHOLD)
SMALLEST_K <- min(PREREG_KS)

# The manuscript says the direction of the long-tail difference is a whole
# number of times further from being overturned than the verdict on it. That
# phrasing is only available while the ratio is a whole number.
HELP_RATIO <- CONCLUSIONS$Help$k / CONCLUSIONS$HelpVerdict$k
if (HELP_RATIO != as.integer(HELP_RATIO) || HELP_RATIO <= 1) {
  stop("the two distances on the long-tail contrast no longer stand in a whole-number ratio")
}

## ---------------------------------------------------------------------------
## Names. The figures and the tables label the same rows, so they read them from
## one place rather than each spelling a condition out in its own words.
## ---------------------------------------------------------------------------

inventory$label <- paste0(
  ifelse(inventory$dataset == "famous", "Famous", "Long-tail"), ", ",
  c(image_load_bearing = "image-load-bearing", text_only = "text-only",
    overall = "all questions")[inventory$subset])

# The table has ten columns and one page to fit them on, so it uses an
# abbreviated form of the same labels. Its caption spells the three splits out.
inventory$short_label <- paste0(
  ifelse(inventory$dataset == "famous", "Famous", "Long-tail"), ", ",
  c(image_load_bearing = "image", text_only = "text",
    overall = "all")[inventory$subset])
CONDITION_NAMES <- c(no_kg = "none", text_kg = "text", multimodal_kg = "multimodal")

harm <- CONCLUSIONS$Harm
help <- CONCLUSIONS$Help
help_verdict <- CONCLUSIONS$HelpVerdict
sig <- CONCLUSIONS$Sig

famous_ilb <- subset_of("famous", "image_load_bearing")
longtail_ilb <- subset_of("longtail", "image_load_bearing")

# What the contested grading actually does when it is applied. The regrade record
# reports the accuracy it produces; this recomputes the whole contrast under it,
# because the paper's claim is about the contrast and not about the accuracy.
regraded <- famous_ilb
regraded[[regrade$i17_flip$condition]][regraded$question_id == regrade$i17_flip$question_id] <- 1L
REGRADED_CELLS <- paired_cells(regraded, "multimodal_kg", "text_kg")
REGRADED_DIFF <- acc_diff_pt(REGRADED_CELLS)
REGRADED_MM <- accuracy(REGRADED_CELLS, "arm")

## ---------------------------------------------------------------------------
## Figures. Each panel reads the objects above and writes one file.
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## The same family, read against thresholds other than the recorded one.
## ---------------------------------------------------------------------------

# Everything else here is read against the level the records used. A reader is
# entitled to ask which of the observations are about the evaluation and which
# about that number, so the family is recomputed across a range of them. This is
# where the closed form earns its place: twenty-five levels would be twenty-five
# families of searches, and it is one pass per contrast per level instead.
SWEEP_LOW <- 0.001
SWEEP_HIGH <- 0.2
ALPHA_SWEEP <- do.call(rbind, lapply(
  10^seq(log10(SWEEP_LOW), log10(SWEEP_HIGH), length.out = 25),
  function(alpha) {
    distances <- mapply(function(b, c_, n) {
      verdict_distance(c(both = n - b - c_, arm_only = b, ref_only = c_,
                         neither = 0L), alpha)
    }, inventory$arm_only, inventory$ref_only, inventory$n)
    data.frame(alpha = alpha,
               significant = sum(inventory$p <= alpha),
               incapable = sum(inventory$floor > alpha),
               survives_holm = sum(inventory$holm <= alpha),
               median_k = median(distances, na.rm = TRUE))
  }))

# Numbered in the order a reader meets them, so a panel file and the figure it
# becomes carry the same number.
FIGURES <- c("fig1_plane.R", "fig2_flips.R", "fig3_decouple.R", "fig4_regrade.R",
             "fig5_floor.R", "fig6_alpha.R", "fig7_loo.R", "fig8_effect_forest.R",
             "fig9_operating.R", "fig10_flip_regrade_summary.R")
for (unit in FIGURES) {
  source(file.path("figs", "panels", unit))
}

## ---------------------------------------------------------------------------
## Numbers and tables.
## ---------------------------------------------------------------------------

write_generated(c(
  macro("AlphaLevel", formatC(ALPHA, format = "f", digits = 2)),
  macro("Questions", nrow(WIDE$famous)),
  macro("Conditions", length(CONDITIONS)),
  macro("ConditionsWord", count_word(length(CONDITIONS))),
  macro("Graders", count_word(length(GRADERS))),
  macro("Answers", nrow(famous_rows) + nrow(longtail_rows)),
  macro("AnswersPerSet", nrow(famous_rows)),
  macro("SubsetN", harm$n),
  macro("SubsetNWord", count_word(harm$n)),
  macro("TextOnlyN", sig$n),
  macro("Contrasts", nrow(inventory)),
  macro("Model", famous_summary$run_meta$model),
  macro("Temperature", famous_summary$run_meta$temperature),

  macro("HarmMM", fmt(accuracy(harm$cells, "arm"), 3)),
  macro("HarmText", fmt(accuracy(harm$cells, "reference"), 3)),
  macro("HarmDiff", signed(harm$diff_pt)),
  macro("HarmP", pval(harm$p)),
  macro("HarmDiscordant", discordant(harm$cells)),
  macro("HarmK", count_word(harm$k)),
  macro("HarmCandidates", nrow(harm$candidates)),
  macro("HarmGradings", harm$gradings),
  macro("HarmLooSign", harm$loo$same_sign),
  macro("HarmLooBreaks", harm$loo$n - harm$loo$same_sign),

  macro("HelpMM", fmt(accuracy(help$cells, "arm"), 3)),
  macro("HelpText", fmt(accuracy(help$cells, "reference"), 3)),
  macro("HelpDiff", signed(help$diff_pt)),
  macro("HelpP", pval(help$p)),
  macro("HelpDiscordant", discordant(help$cells)),
  macro("HelpK", count_word(help$k)),
  macro("HelpVerdictK", count_word(help_verdict$k)),
  macro("HelpVerdictCandidates", nrow(help_verdict$candidates)),
  macro("HelpFloor", pval(attainable_floor(discordant(help$cells)))),
  macro("HelpLooVerdict", help$loo$same_verdict),

  macro("SigDiff", signed(sig$diff_pt)),
  macro("SigP", pval(sig$p)),
  macro("SigDiscordant", discordant(sig$cells)),
  macro("SigK", count_word(sig$k)),
  macro("SigSignK", count_word(sign_fragility(sig$cells)$k)),
  macro("SigHolm", pval(inventory$holm[inventory$dataset == "longtail" &
                                         inventory$subset == "text_only" &
                                         inventory$arm == "text_kg"])),

  macro("KillThreshold", count_word(KILL_THRESHOLD)),
  macro("SmallestK", count_word(SMALLEST_K)),
  macro("KillState", if (KILL_FIRES) "fired" else "did not fire"),

  macro("SignificanceNeedsM", count_word(SIGNIFICANCE_NEEDS)),
  macro("SweepLow", formatC(SWEEP_LOW, format = "f", digits = 3)),
  macro("SweepHigh", formatC(SWEEP_HIGH, format = "f", digits = 2)),
  macro("SweepLevels", count_word(nrow(ALPHA_SWEEP))),
  macro("IncapableSweepMin", min(ALPHA_SWEEP$incapable)),
  macro("MedianKSweepMax", max(ALPHA_SWEEP$median_k)),
  macro("MedianKSweepMin", min(ALPHA_SWEEP$median_k)),
  macro("Incapable", sum(!inventory$capable)),
  macro("AtFloor", sum(inventory$at_floor)),
  macro("AtFloorNS", sum(inventory$at_floor & !inventory$significant)),
  macro("AtFloorNSDiscordant",
        count_word(max(inventory$discordant[inventory$at_floor & !inventory$significant]))),
  macro("SurvivesHolm", sum(inventory$survives_holm)),
  macro("SignificantRaw", sum(inventory$significant)),
  macro("Directional", sum(!inventory$degenerate)),
  macro("FragileMax", count_word(FRAGILE_MAX)),
  macro("FragileSign", sum(!inventory$degenerate & inventory$k_sign <= FRAGILE_MAX)),
  macro("FragileVerdict", sum(inventory$k_verdict <= FRAGILE_MAX)),
  macro("DecoupledGap", count_word(DECOUPLE_GAP)),
  macro("DecoupledCount", sum(inventory$decoupled)),
  macro("HelpRatio", count_word(as.integer(HELP_RATIO))),

  macro("RegradeAnswers", regrade$grader_agreement$famous$n +
          regrade$grader_agreement$longtail$n),
  macro("RegradeContested", count_word(nested_n)),
  macro("RegradeImageFlip", count_word(length(flip_ids))),
  macro("RegradeMovingRows", count_word(n_moving_rows)),
  macro("RegradeIdenticalRows", count_word(nrow(regrade$three_grader_table) - n_moving_rows)),
  macro("ClosedFormMismatches", cf$total_mismatches),
  macro("ClosedFormStates", format(as.integer(cf$total_four_cell_states_checked),
                                   big.mark = ",")),
  macro("ClosedFormPairs", format(as.integer(cf$total_discordant_pairs_checked),
                                  big.mark = ",")),
  macro("KappaVerInversionsLow", inv38[[1]]),
  macro("KappaVerInversionsHigh", inv38[[2]]),
  macro("KappaVerSigTables",
        cf$per_n[["38"]]$monotonicity$significant_tables$n_tables),
  macro("SimTables", format(as.integer(audit_s4$design$n_tables), big.mark = ",")),
  macro("SimReps", format(as.integer(audit_s4$design$reps_per_cell), big.mark = ",")),
  macro("FragileGivenSigNEighteen",
        fmt(unlist(audit_s4$headline$P_kappa_ver_eq_1_given_sig_by_n)[["18"]], 2)),
  macro("FragileGivenSigNThirtyEight",
        fmt(unlist(audit_s4$headline$P_kappa_ver_eq_1_given_sig_by_n)[["38"]], 2)),
  macro("SpearmanSigNThirtyEight",
        fmt(unlist(audit_s4$headline$spearman_pooled_significant_only_by_n)[["38"]], 2)),
  macro("NumericHolmLT", pval(numeric_lt$under_grader$holm_18[[1]])),
  macro("NumericHolmSurvivors",
        count_word(as.integer(audit_s2$per_grader$numeric$summary$n_survive_holm_18))),
  macro("LLMKappaQwen",
        fmt(audit_s3$judges$cohen_kappa$judge_vs_strict[[1]], 2)),
  macro("LLMKappaLlama",
        fmt(audit_s3$judges$cohen_kappa$judge_vs_strict[[2]], 2)),
  macro("LLMKappaBoth", fmt(audit_s3$judge_vs_judge$cohen_kappa$kappa, 2)),
  macro("RegradedMM", fmt(REGRADED_MM, 3)),
  macro("RegradedDiff", signed(REGRADED_DIFF)),
  macro("SummaryHarmStrict", SUMMARY_HARM_STRICT),
  macro("SummaryHarmNumeric", SUMMARY_HARM_NUMERIC),
  macro("SummaryHelpStrict", SUMMARY_HELP_STRICT),
  macro("SummaryHelpNumeric", SUMMARY_HELP_NUMERIC),
  macro("SummarySigStrict", SUMMARY_SIG_STRICT),
  macro("SummarySigNumeric", SUMMARY_SIG_NUMERIC),

  macro("StaticTier", tier_static$status),
  macro("PrimaryTier", tier_primary$status),
  macro("SotaTier", tier_sota$status),
  macro("RetrievalN", retrieval$rows$n[[1]]),
  macro("RetrievalCorrect", retrieval$rows$n_correct[[1]]),
  macro("IndexFiles", retrieval$import_pin$index_file_count),
  macro("BlockedComponent", gsub("_", " ", blocker$component)),
  macro("ForgedState", if (isTRUE(blocker$forged_scores)) "were" else "were not"),
  macro("NEvidence", nrow(manifest$entries)),
  macro("EvidenceBytes", format(sum(manifest$entries$bytes), big.mark = ","))
), "generated_numbers.tex")

## The full inventory, one row per contrast.

write_generated(c(
  "\\begin{tabular}{llrrrrrrrr}",
  "\\toprule",
  "& & & & & & & \\multicolumn{2}{c}{Gradings to unseat} & \\\\",
  "\\cmidrule(lr){8-9}",
  paste("Questions & Contrast & $n$ & Disc. & Diff. (pt) & $p$ &",
        "Floor & Dir. & Verd. & Holm \\\\"),
  "\\midrule",
  paste0(inventory$short_label, " & ",
         CONDITION_NAMES[inventory$arm], " vs ", CONDITION_NAMES[inventory$reference], " & ",
         inventory$n, " & ", inventory$discordant, " & ",
         signed(inventory$diff_pt), " & ",
         vapply(inventory$p, pval, character(1)), " & ",
         vapply(inventory$floor, pval, character(1)), " & ",
         inventory$k_sign, " & ", inventory$k_verdict, " & ",
         vapply(inventory$holm, pval, character(1)), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_contrasts.tex")

## The three prespecified conclusions, with what it takes to unseat each.

prereg <- CONCLUSIONS[c("Harm", "Help", "Sig")]
write_generated(c(
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  " & As & Gradings & Single-grading & Deletions \\\\",
  "Conclusion & measured & to unseat & candidates & survived \\\\",
  "\\midrule",
  paste0(
    c("Multimodal is behind text, famous",
      "Multimodal is ahead of text, long-tail",
      "Text beats no retrieval, long-tail"), " & ",
    c(signed(prereg$Harm$diff_pt), signed(prereg$Help$diff_pt),
      paste0(signed(prereg$Sig$diff_pt), ", ", pval(prereg$Sig$p))), " & ",
    vapply(prereg, function(x) as.character(x$k), character(1)), " & ",
    vapply(prereg, function(x) {
      if (x$k == 1L) as.character(nrow(x$candidates)) else "---"
    }, character(1)), " & ",
    c(sprintf("%d of %d", prereg$Harm$loo$same_sign, prereg$Harm$loo$n),
      sprintf("%d of %d", prereg$Help$loo$same_sign, prereg$Help$loo$n),
      sprintf("%d of %d", prereg$Sig$loo$same_verdict, prereg$Sig$loo$n)),
    " \\\\"),
  "\\midrule",
  paste0("Same contrast, verdict not direction & ",
         pval(help_verdict$p), " & ", help_verdict$k, " & ",
         nrow(help_verdict$candidates), " & ",
         sprintf("%d of %d", help_verdict$loo$same_verdict, help_verdict$loo$n), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_prereg.tex")

## The four cells every contrast reduces to. The table above reports the
## discordant count, which is what the tests read; this reports the split it
## came from, which is what a reader would need to recompute anything here.

write_generated(c(
  "\\begin{tabular}{llrrrrrr}",
  "\\toprule",
  "& & & \\multicolumn{4}{c}{Questions answered correctly by} & \\\\",
  "\\cmidrule(lr){4-7}",
  paste("Questions & Contrast & $n$ & Both & Arm & Ref. & Neither &",
        "Disc. \\\\"),
  "\\midrule",
  paste0(inventory$short_label, " & ",
         CONDITION_NAMES[inventory$arm], " vs ", CONDITION_NAMES[inventory$reference], " & ",
         inventory$n, " & ", inventory$both, " & ", inventory$arm_only, " & ",
         inventory$ref_only, " & ", inventory$neither, " & ",
         inventory$discordant, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_cells.tex")

## What the design admits, as a function of the discordant count alone. Every
## row is arithmetic on the binomial and holds for any paired comparison at this
## threshold, so the table is a lookup rather than a result.

design_m <- 0:max(inventory$discordant)
design_majority <- critical_majority(max(design_m), ALPHA)
write_generated(c(
  "\\begin{tabular}{rrlr}",
  "\\toprule",
  "Discordant & Smallest attainable & Majority needed & Contrasts \\\\",
  "pairs $m$ & $p$-value & for significance & with this $m$ \\\\",
  "\\midrule",
  paste0(design_m, " & ",
         vapply(vapply(design_m, attainable_floor, numeric(1)), pval, character(1)), " & ",
         ifelse(is.na(design_majority[design_m + 1L]), "unreachable",
                paste0(design_majority[design_m + 1L], " of ", design_m)), " & ",
         vapply(design_m, function(m) sum(inventory$discordant == m), integer(1)),
         " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_design.tex")

## The three graders, on the same answers. The manuscript's central example is
## the single answer they disagree about, so the table that makes it checkable
## is the one that shows how little else moves.

grader_rows <- regrade$three_grader_table
grader_rows <- grader_rows[order(grader_rows$dataset != "famous",
                                 grader_rows$subset != "image_load_bearing",
                                 match(grader_rows$grader, c("strict", "substr", "numeric"))), ]
GRADER_NAMES <- c(strict = "strict match", substr = "substring match",
                  numeric = "numeric tolerance")
write_generated(c(
  "\\begin{tabular}{lllrrr}",
  "\\toprule",
  "& & & \\multicolumn{3}{c}{Accuracy under each retrieval condition} \\\\",
  "\\cmidrule(lr){4-6}",
  "Questions & Split & Grader & none & text & multimodal \\\\",
  "\\midrule",
  paste0(ifelse(grader_rows$dataset == "famous", "Famous", "Long-tail"), " & ",
         ifelse(grader_rows$subset == "image_load_bearing", "image", "text"), " & ",
         GRADER_NAMES[grader_rows$grader], " & ",
         fmt(grader_rows$no_kg, 3), " & ", fmt(grader_rows$text_kg, 3), " & ",
         fmt(grader_rows$multimodal_kg, 3), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_graders.tex")

## Which conclusions have single-grading candidates. The count remains useful
## for the argument, but row identifiers and answer strings belong to the private
## evidence and are deliberately withheld from the publication surface.
candidate_row <- function(conclusion, label) {
  paste0(label, " & ", nrow(conclusion$candidates),
         " & keys/strings withheld \\\\")
}

write_generated(c(
  "\\raggedright",
  "\\begin{tabular}{p{0.48\\linewidth}rp{0.25\\linewidth}}",
  "\\toprule",
  "Conclusion & Candidate count & Public detail \\\\",
  "\\midrule",
  candidate_row(harm,
                "Multimodal is behind text, widely-documented set: direction"),
  candidate_row(help_verdict,
                "Less-documented image-dependent difference: verdict"),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_candidates.tex")

## The manifest itself, so the evidence discipline can be checked rather than believed.

message(sprintf("wrote %d figures to figs/out and 6 generated tex files to tex/ (smallest flip set: %d)",
                length(FIGURES), SMALLEST_K))
unlink(file.path("tex", "generated_table_evidence.tex"), force = TRUE)
