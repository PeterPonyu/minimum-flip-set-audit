# Count the gradings, not the questions: how far each conclusion of a small paired evaluation is from being overturned

Per-answer question-answering records under three retrieval conditions, the recorded paired contrasts, an independent three-grader regrade, figure code and manuscript source for a study that computes, by exhaustive search, the smallest number of individual gradings that would overturn the direction and separately the verdict of every contrast the evaluation reported.

This repository has not been deposited in a public archive, so it has no persistent identifier yet. One will be recorded here when an archive exists.

## What is here

- `paper/tex/` — manuscript source. The abstract, the methods and the figure
  captions are separate files and each is self-contained.
- `paper/figs/` — the R code that draws every figure and emits every number the
  manuscript prints.
- `paper/evidence/` — the manifest binding each artifact to its SHA-256 digest.
- `data/` — the 15 artifacts the manifest names, at the bytes that
  were hashed.

## Not redistributed

The manuscript's evidence manifest binds 2 further artifacts that this archive does not carry. No number in the manuscript is derived from that material; they are bound because the manuscript refers to the content, and held back for the reason below.

- The project's own working record of this direction. It is an internal narrative that names other directions, planning decisions and process labels, and no number in the manuscript comes from it. Everything it contributes to the manuscript is stated in the methods section and is separately bound in the recorded-state artifacts that are redistributed.
- A display panel built from the measurement table, annotated with the workspace's internal disposition label for this direction. The two things the manuscript reads from it, that the difference is recorded as not established and that the higher hand-authored figure is recorded as declined, are each also carried by artifacts that are redistributed: the measurement table states the first and the execution record states the second.

## Rebuild

```bash
bash build.sh
```

The build re-hashes every artifact before reading it and stops if any byte has
moved. Figures and printed numbers are regenerated from those bytes rather than
transcribed, so the manuscript cannot quietly disagree with its own data.

Requires `python3`, `Rscript` with `digest`, `ggplot2`, `jsonlite`,
`patchwork`, `scales` and `systemfonts`, and a TeX distribution with `latexmk`.

## Status

Working draft. Not submitted to any venue.

## Licence

Code: MIT (`LICENSE`). Manuscript text, figures and recorded result data:
CC BY 4.0 (`LICENSE-CONTENT`).
