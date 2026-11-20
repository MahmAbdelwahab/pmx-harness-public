# PMx Harness

The SME owns scientific decisions and signatures. The orchestrator routes work
to specialists using the [dispatch map](docs/PORTABILITY.md#dispatch-map).
Each specialist reads its role in `roles/` and the corresponding skills.
Run independent review in a fresh context.

## Methods and gates

- Compose declared tasks with `pipeline/compose.R`. Tasks use targets for
  computation and typed evidence notes from `tools/evidence_notes.R`.
  The composer generates stage-gate targets.
- Public methods start unsigned. Inspect their manifests and use
  `tools/load_reference.R` for authenticated reference loading. Guarded methods
  require local review and matching file and dependency digests. Escalate a
  missing implementation or a failed criterion.
- Render drafts from a current targets store through `render_gate_from_store()`.
  The SME supplies the signature through the local gate workflow. The gate and
  ledger checks must pass before downstream computation. Use the release or
  errata commands for an approved record that needs correction.
- Keep real datasets and site credentials outside the source tree. Site
  operators provide local configuration and licensed engines independently.

## Work and handoffs

- Give each specialist an objective, scope, protected paths, evidence and
  expected output. Keep authors and verifiers separate.
- Attach the producing command and decisive result to verification claims.
  Use disposable copies for export, installation, reproduction and renders.
- Keep decisions and handoffs in local project records. Escalate unresolved
  scientific choices with evidence and options.
- Commit, publish or deploy only with the owner's express authorization.

The repository-local write guard protects gate, hook and trust-bound paths
when the host invokes it. See [Portability](docs/PORTABILITY.md) for adapter
setup, the synthetic example, and local signatures.
