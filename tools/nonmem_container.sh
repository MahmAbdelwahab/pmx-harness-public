#!/usr/bin/env bash
# Containerized NONMEM/PsN runner. Direct calls resolve once; dispatcher calls
# consume its existing private context without reopening either YAML source.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=resolved_context.sh
source "$HERE/resolved_context.sh"
REPO_ROOT="${REPO_ROOT:-$(git -C "$(dirname "$0")" rev-parse --show-toplevel 2>/dev/null || echo "$(cd "$(dirname "$0")/.." && pwd)")}"
DELEGATED=false; DIRECT_WORKDIR=""
trap resolved_context_cleanup EXIT
if [ "${1:-}" = --resolved-context ]; then
  [ $# -ge 4 ] || { echo "Usage: $0 <workdir> <command...>" >&2; exit 2; }
  resolved_context_adopt "$2"; DELEGATED=true; shift 2
else
  SHOW=false; [ "${1:-}" != --show-config ] || SHOW=true
  if ! $SHOW && [ $# -lt 2 ]; then echo "Usage: $0 <workdir> <command...>" >&2; exit 2; fi
  if $SHOW; then WORKDIR="$(pwd)"; COMMAND=(--show-config)
  else
    WORKDIR="$1"; shift; case "$WORKDIR" in /*) : ;; *) WORKDIR="$(pwd)/$WORKDIR" ;; esac
    [ -d "$WORKDIR" ] || { echo "Workdir does not exist: $WORKDIR" >&2; exit 2; }
    DIRECT_WORKDIR="$WORKDIR"
    COMMAND=("$@")
  fi
  HARNESS="$REPO_ROOT/harness.yaml"; [ -f "$HARNESS" ] || HARNESS=""
  resolved_context_new
  NONMEM_PROVISION= NONMEM_PROVISION_OVERRIDE_ACK= \
    Rscript "$HERE/resolve_engine_config.R" --workdir "$WORKDIR" \
      --engine-config "" --harness "$HARNESS" --handoff "$RESOLVED_CONTEXT" \
      --invocation "${PMX_INVOCATION_ID:-not-supplied}" -- "${COMMAND[@]}"
fi

resolved_context_validate
resolved_context_read image IMAGE; resolved_context_read image-source IMAGE_SOURCE
resolved_context_read license LICENSE; resolved_context_read license-source LICENSE_SOURCE

if ! $DELEGATED && [ "${1:-}" = --show-config ]; then
  echo "image: $IMAGE ($IMAGE_SOURCE)"
  echo "license: ${LICENSE:-<not configured>} ($LICENSE_SOURCE)"
  exit 0
fi
if ! $DELEGATED; then set -- "$DIRECT_WORKDIR" "${COMMAND[@]}"; fi
WORKDIR="$1"; shift
case "$WORKDIR" in /*) : ;; *) WORKDIR="$(pwd)/$WORKDIR" ;; esac
[ -d "$WORKDIR" ] || { echo "Workdir does not exist: $WORKDIR" >&2; exit 2; }
[ -n "$LICENSE" ] || { echo "NONMEM license not configured: set NONMEM_LICENSE or nonmem_license: in harness.yaml" >&2; exit 2; }
[ -f "$LICENSE" ] || { echo "NONMEM license not found: $LICENSE (set NONMEM_LICENSE or harness.yaml nonmem_license:)" >&2; exit 2; }
MOUNT_ROOT="$(git -C "$WORKDIR" rev-parse --show-toplevel 2>/dev/null || echo "$WORKDIR")"
EXTRA_ARGS=()
if [ -n "${NONMEM_EXTRA_MOUNTS:-}" ]; then
  IFS=':' read -ra EXTRA <<< "$NONMEM_EXTRA_MOUNTS"
  for path in "${EXTRA[@]}"; do [ -z "$path" ] || EXTRA_ARGS+=(-v "$path":"$path":ro); done
fi
resolved_context_cleanup; trap - EXIT
exec docker run --rm -v "$MOUNT_ROOT":"$MOUNT_ROOT" \
  -v "$LICENSE":/nonmem/nm751/license/nonmem.lic:ro "${EXTRA_ARGS[@]}" \
  -w "$WORKDIR" --user "$(id -u):$(id -g)" "$IMAGE" "$@"
