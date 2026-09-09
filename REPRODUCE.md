# Reproducing the numbers

Every quantity printed in the manuscript is emitted by `paper/figs/make_figs.R`
from the artifacts listed below. None is typed into the prose. The figure code
re-hashes each artifact before reading it, so a modified or missing file stops
the build instead of producing a stale number.

## Bound artifacts

| path | role | bytes | sha256 |
|---|---|---|---|
| `data/e-famous/raw_responses.jsonl` | per_record | 78294 | `f83fef56548eb4ee…` |
| `data/e-longtail/raw_responses.jsonl` | per_record | 79206 | `7bca93ed4e23e8a3…` |
| `data/e-famous-summary/summary.json` | derived_table | 1390 | `c843d50a4955d606…` |
| `data/e-longtail-summary/summary.json` | derived_table | 1392 | `4f297d50dd3ddd85…` |
| `data/e-stats/inferential_stats.json` | derived_table | 26309 | `2aedebd44d74f4c5…` |
| `data/e-regrade/regrade_independent.json` | derived_table | 5390 | `093946749aae8e5c…` |
| `data/e-measure/paper_measurement_caption_condition.json` | recorded_state | 1005 | `3451160deab5dc31…` |
| `data/e-executed/executed.json` | recorded_state | 3914 | `3ef5e9fb574a9e62…` |
| `data/e-retrieval/p006b_retrieval_38_38.json` | derived_table | 1283 | `1039ef73cb149c32…` |
| `data/e-static/static.json` | recorded_state | 269 | `91227418f9eb99fa…` |
| `data/e-naive/naive.json` | recorded_state | 355 | `283ce11222a77f46…` |
| `data/e-simple/simple.json` | recorded_state | 386 | `33f67068f00a3e8c…` |
| `data/e-primary/paper_primary.json` | recorded_state | 1025 | `d6f6053440c0ce6b…` |
| `data/e-sota/sota_copy.json` | recorded_state | 315 | `f856273e0ef3202f…` |
| `data/e-blocker/llm_qa_blocker.json` | recorded_state | 484 | `aa0d722720e9a9a4…` |
| `data/e-audit-s1/closed_form_verification.json` | derived_table | 53152 | `c9054a7ae3bc11a7…` |
| `data/e-audit-s2/regrade_consequences.json` | derived_table | 112036 | `48a1b060a5d2a3e1…` |
| `data/e-audit-s3/judge_summary.json` | derived_table | 25331 | `2175c7b2d46594cc…` |
| `data/e-audit-s4/simulation_summary.json` | derived_table | 125422 | `7d0ff852cf53b094…` |

Some of these files recorded the paths of the machine that produced them. Those path strings and source links were refreshed before deposit; `REDACTION.md` states the rules, lists every file touched with both digests, and describes the check that proves no number changed.

## Not included

This archive leaves out 2 extra files named in the paper's evidence list. The paper does not take any number from them.

- A private working note. The paper does not use any number from it. Those facts are already in the methods and in the result files included here.
- A figure made from the measurement table. The two facts the paper uses from it are also in the measurement table and the execution record, both included here.

## Checking the archive without building it

```bash
python3 tools/bind_evidence.py paper --check
```

This re-hashes every path above against `paper/evidence/evidence_manifest.json`
and reports the first artifact that has drifted.

## Rebuilding

```bash
bash build.sh
```

The steps are check the files, redraw the figures, then typeset. Each step
must finish before the next one starts.
