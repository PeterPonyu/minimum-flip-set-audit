# Local paths rewritten before deposit

Some result files recorded the machine they ran on, including local folder
names. Those strings are not published. The copies in this archive were
rewritten before deposit. The left column names each class of string rather
than quoting it.

The rewrite changes path strings only. Numbers and table structure stay the
same. Longer matches are applied first.

| replaced | with |
|---|---|
| the absolute filesystem prefix of the machine the archive was assembled on | removed |
| the checkout prefix of a rented machine a run executed on | removed |
| the remaining scratch-mount prefix of that rented machine | `<remote>/` |
| the recorded path of an artifact that is archived here | the path it now has in this archive |
| the home directory of the account the runs executed under | `~/` |
| any remaining directory prefix belonging to the private source tree | `source-tree/` |
| a branch of the private repository named in a recorded instruction | `<private-branch>` |
| a private project status word | the ordinary word it stands for |

Both machine prefixes are removed before a file is mapped to its place in this
archive, so the same path recorded on two machines becomes the same archived
string. A study that never left one machine will only show some of these
substitutions.

The last two rows rewrite recorded values, never keys. A reader comparing an
archived file with the original should see the same fields and the same
numbers; only a local name is changed.

## What was checked

Every rewritten file was read again after substitution and compared with the
original after all string values were blanked. A changed number, a dropped
field, a reordered list or a lost record stops the export. For line-oriented
files the line count is compared as well.

## Files rewritten

The hash on the left is the file as the run wrote it. The hash on the right is
the file in this archive, and it is the one the file list names and the build
checks.

| path | path substitutions | receipt-link refreshes | original sha256 | archived sha256 |
|---|---:|---:|---|---|
| `data/e-executed/executed.json` | 24 | 0 | `aa2ca54ecba38ccc…` | `3ef5e9fb574a9e62…` |
| `data/e-retrieval/p006b_retrieval_38_38.json` | 7 | 0 | `d1e213e98e87ca40…` | `1039ef73cb149c32…` |
| `data/e-naive/naive.json` | 1 | 0 | `18aaf726056bd651…` | `283ce11222a77f46…` |
| `data/e-simple/simple.json` | 1 | 0 | `1d85e81ec46761bd…` | `33f67068f00a3e8c…` |
| `data/e-primary/paper_primary.json` | 1 | 0 | `f9f6110a3dd5455a…` | `d6f6053440c0ce6b…` |
