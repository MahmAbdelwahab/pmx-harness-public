# Fixed-file protocol shared by NONMEM shell runners. This tracked code is safe
# to source; resolved data remains inert and is read only through `read -r`.
RESOLVED_CONTEXT_ROOT=""
RESOLVED_CONTEXT=""
RESOLVED_CONTEXT_OWNED=""

resolved_context_error() {
  echo "ERROR: malformed resolved configuration context." >&2
  exit 4
}

resolved_context_new() {
  RESOLVED_CONTEXT_ROOT="$(cd "${TMPDIR:-/tmp}" && pwd -P)"
  RESOLVED_CONTEXT="$(mktemp -d "$RESOLVED_CONTEXT_ROOT/pmx-resolved-config.XXXXXX")"
  RESOLVED_CONTEXT_OWNED="$RESOLVED_CONTEXT"
}

resolved_context_adopt() {
  local base
  RESOLVED_CONTEXT_ROOT="$(cd "${TMPDIR:-/tmp}" && pwd -P)"
  RESOLVED_CONTEXT="$1"
  base="${RESOLVED_CONTEXT##*/}"
  [ "$RESOLVED_CONTEXT" = "$RESOLVED_CONTEXT_ROOT/$base" ] &&
    [[ "$base" = pmx-resolved-config.* ]] || resolved_context_error
  RESOLVED_CONTEXT_OWNED="$RESOLVED_CONTEXT"
}

resolved_context_cleanup() {
  local status=$?
  [ -z "$RESOLVED_CONTEXT_OWNED" ] || rm -rf "$RESOLVED_CONTEXT_OWNED"
  return "$status"
}

resolved_context_read() {
  local key="$1" destination="$2" value file_bytes value_bytes
  [ "$(wc -l < "$RESOLVED_CONTEXT/$key")" = 1 ] || resolved_context_error
  IFS= read -r value < "$RESOLVED_CONTEXT/$key" || resolved_context_error
  file_bytes="$(wc -c < "$RESOLVED_CONTEXT/$key")"
  value_bytes="$(printf '%s' "$value" | wc -c)"
  [ "$file_bytes" = "$((value_bytes + 1))" ] || resolved_context_error
  case "$value" in *$'\r'*) resolved_context_error ;; esac
  printf -v "$destination" '%s' "$value"
}

resolved_context_validate() {
  local mode count=0 field key
  [ -d "$RESOLVED_CONTEXT" ] && [ ! -L "$RESOLVED_CONTEXT" ] &&
    [ -O "$RESOLVED_CONTEXT" ] || resolved_context_error
  mode="$(stat -c %a "$RESOLVED_CONTEXT" 2>/dev/null || stat -f %Lp "$RESOLVED_CONTEXT")"
  [ "${mode#0}" = 700 ] || resolved_context_error
  shopt -s nullglob dotglob
  for field in "$RESOLVED_CONTEXT"/*; do
    [ -f "$field" ] && [ ! -L "$field" ] || resolved_context_error
    case "${field##*/}" in
      env-file|image|image-source|license|license-source|provision|schema-version) : ;;
      *) resolved_context_error ;;
    esac
    resolved_context_read "${field##*/}" unused
    count=$((count + 1))
  done
  shopt -u nullglob dotglob
  [ "$count" = 7 ] || resolved_context_error
  resolved_context_read schema-version schema
  [ "$schema" = 1 ] || resolved_context_error
}
