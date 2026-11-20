# Copilot adapter runbook

Use a disposable copy to install or test the adapter. Use an isolated synthetic
project for gate transitions and provide a disposable reviewer identity.

## Prepare the workspace

1. Use Python 3.11 or newer. Install PyYAML in the Python interpreter the host
   uses for the write guard. Install R and the packages needed for the
   declared tasks if you plan to run the harness.
2. Run `python3 copilot-adapter/setup.py install` followed by
   `python3 copilot-adapter/setup.py check`. Review the ignored environment
   draft locally before using it. Run `python3 copilot-adapter/setup.py env`
   to check probe drift. The site independently provides any licensed tool.
3. Open the copy in Copilot. In VS Code, the generated settings request one
   set of agent cards by disabling discovery of the Claude copies. Use
   `@pmx-orchestrator` and see the
   [dispatch map](PORTABILITY.md#dispatch-map). Run independent review in an
   independent subagent or fresh chat.

## Verify the active host

- Check that the repository instructions and path-scoped rules load. Confirm
  the model reported by the runtime before using a specialist. Record
  `unreported` when the runtime does not expose a model.
- If the host permits hooks, capture the SessionStart mandate and try a
  harmless editor-tool write to a disposable gate fixture. Record the host,
  tool and outcome. The session and write hook tests exercise fixture
  envelopes. A real host claim needs its own receipt.
- Try an ordinary editable file in the same copy. Test independent reviewer
  dispatch and direct reviewer selection. Treat reviewer isolation as
  procedural whenever shell execution or unguarded edits are available.
- The terminal auto-approval rules are editor settings for prompts. Gate
  digests and journal verification provide independent integrity checks even
  when hooks are unavailable. Use `Rscript tools/ci_check.R --light` and
  `bash tools/tests/test_protect_paths.sh` for the portable local checks.

## Test the synthetic path

Follow the repository quick start. Verify that the ungated step produces its
declared artifact and that the downstream step refuses an unsigned gate.
Use the local preview and apply workflow from
[Portability](PORTABILITY.md#local-gate-signatures) in a separate copy.
For a negative test, alter a declared artifact there and confirm that
verification refuses the drift. Use the source command and decisive output
as evidence.

## Release recovery

A release is FAIL-LOUD, RECOVERABLE MULTI-STEP, not all-or-none. The
residual window is exactly this: between the ledger append and the two
renames that follow it, the ledger records the release while the gate
record on disk may still carry its signature and the archive may not
yet be in place. The append cannot be undone -- the ledger is
append-only, which is what makes the chain auditable -- so the staged
bytes are LEFT ON DISK, named, and the operator completes or revokes
the release by hand. Every release is serialised by an exclusive lock,
so the window is never entered by two releasers at once. A release may also
overwrite a concurrent editor save of the record: editing is unlocked, so a
save made between the release's last comparison and its rename of the cleared
record is replaced, and the ledger and archive stay consistent while that
save is lost.

Inspect the emitted diagnostics and keep staged recovery files for the owner.
