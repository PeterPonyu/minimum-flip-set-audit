# Redaction of recorded paths and internal names

The result files in this archive were written by the runs that produced them,
and they recorded where they were running and how the work was coordinated.
Those strings describe a private machine and a private workspace and are not
published, so the archived copies were rewritten before deposit. Describing the
rules below without reproducing the strings they remove is the point of this
file, so the left column names each class of string rather than quoting it.

The rewrite is textual and total: it substitutes path strings and internal
names and refreshes the explicit source/hash links in derived receipts, without
changing a numeric or structural value. Rules are applied longest match first.

| replaced | with |
|---|---|
| the absolute filesystem prefix of the machine the archive was assembled on | removed |
| the checkout prefix of a rented machine a run executed on | removed |
| the remaining scratch-mount prefix of that rented machine | `<remote>/` |
| the recorded path of an artifact that is archived here | the path it now has in this archive |
| the home directory of the account the runs executed under | `~/` |
| any remaining directory prefix belonging to the private source tree | `source-tree/` |
| a branch of the private repository named in a recorded instruction | `<private-branch>` |
| a disposition label of the private workspace's own series | the plain word it stands for |

Both machine prefixes are removed before an archived artifact is mapped to its
new location, so a path recorded on the rented machine and the same path
recorded locally become the same archived string rather than two. The rules are
the full declared set; a direction whose runs never left one machine will show
substitutions for only some of them.

The last two classes rewrite recorded values, never keys. A reader comparing an
archived record against the original finds the same schema and the same numbers;
what changes is a name that only resolves inside the workspace that coined it.

## What was checked

Every rewritten file was reparsed after substitution and compared against the
original with all string leaves erased. A changed number, a dropped key, a
reordered list or a lost record fails the export rather than being deposited.
For line-oriented records the record count is compared as well.

## Files rewritten

The digest on the left is the file as the run wrote it; the digest on the right
is the file in this archive, and it is the one the manifest binds and the build
verifies.

| path | path substitutions | receipt-link refreshes | original sha256 | archived sha256 |
|---|---:|---:|---|---|
| `data/e-executed/executed.json` | 24 | 0 | `aa2ca54ecba38ccc…` | `3ef5e9fb574a9e62…` |
| `data/e-retrieval/p006b_retrieval_38_38.json` | 7 | 0 | `d1e213e98e87ca40…` | `1039ef73cb149c32…` |
| `data/e-naive/naive.json` | 1 | 0 | `18aaf726056bd651…` | `283ce11222a77f46…` |
| `data/e-simple/simple.json` | 1 | 0 | `1d85e81ec46761bd…` | `33f67068f00a3e8c…` |
| `data/e-primary/paper_primary.json` | 1 | 0 | `f9f6110a3dd5455a…` | `d6f6053440c0ce6b…` |
