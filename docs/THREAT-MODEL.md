# Integrity boundaries

The harness checks that a downstream task consumes the project, inputs, method bytes, and artifacts specified by its approved records. Preserve the exact files and ledger entries that describe the decision.

## Checks

- Method manifests bind reference code, entrypoints and declared dependencies by exact digest. Guarded methods require a valid local approval.
- Stage gates and the approval ledger bind the approved package and associated artifacts. Edited, missing or stale inputs need fresh review before their results can be promoted.
- The errata ledger records corrections separately and verifies their chain and relation to the accompanying approval record.
- The local runtime adapters can protect review and method paths from ordinary tool edits. Use the adapter's actual registration and runtime evidence to confirm a host enforces its protection.

## Operating assumptions

The project supplies authorized local reviewers, trustworthy storage and credentials, and its own qualified execution environment. Ledger hashes and exact-byte checks detect integrity failures within those assumptions. The machine's administrators and account holders control the files and runners directly. Keep restricted inputs and credentials in the organization's approved storage and use protected review processes for changes to the enforcement code.

The public CI exercises synthetic and malformed cases in controlled environments. Verify the applicable engine, toolchain and scientific methodology in your project before relying on results.
