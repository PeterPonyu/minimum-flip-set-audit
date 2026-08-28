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
| `data/e-executed/executed.json` | recorded_state | 3892 | `03f8556cf5627f6b…` |
| `data/e-retrieval/p006b_retrieval_38_38.json` | derived_table | 1283 | `1039ef73cb149c32…` |
| `data/e-static/static.json` | recorded_state | 269 | `91227418f9eb99fa…` |
| `data/e-naive/naive.json` | recorded_state | 355 | `283ce11222a77f46…` |
| `data/e-simple/simple.json` | recorded_state | 386 | `33f67068f00a3e8c…` |
| `data/e-primary/paper_primary.json` | recorded_state | 1025 | `d6f6053440c0ce6b…` |
| `data/e-sota/sota_copy.json` | recorded_state | 315 | `f856273e0ef3202f…` |
| `data/e-blocker/llm_qa_blocker.json` | recorded_state | 484 | `aa0d722720e9a9a4…` |

Some of these files recorded the paths of the machine that produced them. Those path strings were rewritten before deposit; `REDACTION.md` states the rules, lists every file touched with both digests, and describes the check that proves no number changed.

## Not redistributed

The manuscript's evidence manifest binds 2 further artifacts that this archive does not carry. No number in the manuscript is derived from that material; they are bound because the manuscript refers to the content, and held back for the reason below.

- The project's own working record of this direction. It is an internal narrative that names other directions, planning decisions and process labels, and no number in the manuscript comes from it. Everything it contributes to the manuscript is stated in the methods section and is separately bound in the recorded-state artifacts that are redistributed.
- A display panel built from the measurement table, annotated with the workspace's internal disposition label for this direction. The two things the manuscript reads from it, that the difference is recorded as not established and that the higher hand-authored figure is recorded as declined, are each also carried by artifacts that are redistributed: the measurement table states the first and the execution record states the second.

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

Stage order is verify, regenerate, typeset. Each stage is a hard gate on the
next.
