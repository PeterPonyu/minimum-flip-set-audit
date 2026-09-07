# Count the gradings, not the questions: how far each conclusion of a small paired evaluation is from being overturned

Per-answer question-answering records under three retrieval conditions, the recorded paired contrasts, an independent three-grader regrade, figure code and manuscript source for a study that computes, by exhaustive search, the smallest number of individual gradings that would overturn the direction and separately the verdict of every contrast the evaluation reported.

Archived at [10.5281/zenodo.22647028](https://doi.org/10.5281/zenodo.22647028).

Repository: https://github.com/PeterPonyu/minimum-flip-set-audit

## What is here

- `paper/tex/` — manuscript source
- `paper/figs/` — the R code that draws the figures and writes the printed numbers
- `paper/evidence/` — a file list with SHA-256 hashes
- `data/` — the 15 data files named in that list

## Not included

This archive leaves out 2 extra files named in the paper's evidence list. The paper does not take any number from them.

- A private working note. The paper does not use any number from it. Those facts are already in the methods and in the result files included here.
- A figure made from the measurement table. The two facts the paper uses from it are also in the measurement table and the execution record, both included here.

## Rebuild

```bash
bash build.sh
```

The build checks every data file against its hash and stops if a file has
changed. Figures and printed numbers are generated from those files, not typed
in by hand.

Requires `python3`, `Rscript` with `digest`, `ggplot2`, `jsonlite`,
`patchwork`, `scales` and `systemfonts`, and a TeX distribution with `latexmk`.

## Status

Working draft. Not submitted to any venue.

## Licence

Code: MIT (`LICENSE`). Manuscript text, figures and recorded result data:
CC BY 4.0 (`LICENSE-CONTENT`).
