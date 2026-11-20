#!/usr/bin/env bash
# NONMEM/PsN provision dispatcher. R resolves and records one immutable config;
# this wrapper validates its private handoff and launches the selected runner.
set -euo pipefail

if [ $# -lt 2 ]; then echo "Usage: $0 <workdir> <command...>" >&2; exit 2; fi
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=resolved_context.sh
source "$HERE/resolved_context.sh"
REPO_ROOT="$(cd "$HERE/.." && pwd)"
WORKDIR="$1"; shift
case "$WORKDIR" in /*) : ;; *) WORKDIR="$(pwd)/$WORKDIR" ;; esac
[ -d "$WORKDIR" ] || { echo "Workdir does not exist: $WORKDIR" >&2; exit 2; }
CONFIG="$REPO_ROOT/config/engine.yaml"
[ ! -f "$WORKDIR/run-config.yaml" ] || CONFIG="$WORKDIR/run-config.yaml"
[ -f "$CONFIG" ] || CONFIG=""
HARNESS="$REPO_ROOT/harness.yaml"; [ -f "$HARNESS" ] || HARNESS=""
resolved_context_new
trap resolved_context_cleanup EXIT

Rscript "$HERE/resolve_engine_config.R" \
  --workdir "$WORKDIR" --engine-config "$CONFIG" --harness "$HARNESS" \
  --handoff "$RESOLVED_CONTEXT" \
  --invocation "${PMX_INVOCATION_ID:-not-supplied}" -- "$@"

resolved_context_validate
resolved_context_read provision PROVISION
resolved_context_read env-file ENV_FILE

case "$PROVISION" in
  containerized)
    exec "$HERE/nonmem_container.sh" --resolved-context "$RESOLVED_CONTEXT" "$WORKDIR" "$@"
    ;;
  native)
    if [ -n "$ENV_FILE" ]; then
      [ -f "$ENV_FILE" ] || { echo "nonmem_env_file not found: $ENV_FILE" >&2; exit 2; }
      # Existing machine-local site profile; never generated configuration.
      # shellcheck disable=SC1090
      . "$ENV_FILE"
    fi
    TOOL="$1"
    command -v "$TOOL" >/dev/null 2>&1 || {
      echo "Tool not on PATH: $TOOL (native provision expects the site's validated NONMEM/PsN on PATH — launch from a login shell or set NONMEM_ENV_FILE / harness.yaml nonmem_env_file)." >&2
      exit 127
    }
    cd "$WORKDIR"; resolved_context_cleanup; trap - EXIT; exec "$@"
    ;;
  *) echo "Unknown NONMEM provision mode: $PROVISION (containerized|native)" >&2; exit 2 ;;
esac
