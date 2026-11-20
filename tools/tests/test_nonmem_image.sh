#!/usr/bin/env bash
# Image and license resolution in tools/nonmem_container.sh.
#
# Exercises ONLY the resolution path, through the script's --show-config
# diagnostic: no container is created, docker is never invoked, and the
# repository's own harness.yaml is never read (REPO_ROOT is overridden to a
# temp fixture holding a fake harness.yaml and a dummy license file).
#
# Run with: bash tools/tests/test_nonmem_image.sh
# Exits non-zero if any assertion fails.
set -u

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOOLS="$(dirname "$HERE")"
SCRIPT="$TOOLS/nonmem_container.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/nonmem-image-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

fail=0
pass() { echo "PASS  $1"; }
flunk() { echo "FAIL  $1"; fail=1; }

CONTEXT_HELPER="$TOOLS/resolved_context.sh"
if bash -c 'source "$1" && declare -F resolved_context_new resolved_context_adopt resolved_context_validate resolved_context_read resolved_context_cleanup >/dev/null' \
     _ "$CONTEXT_HELPER"; then
  pass "0 shared resolved-context protocol helper is sourceable"
else
  flunk "0 shared resolved-context protocol helper is unavailable"
fi

FIX="$TMP/fixture"; mkdir -p "$FIX"
printf 'dummy license content, not a real license\n' > "$FIX/nonmem.lic"
cat > "$FIX/harness.yaml" <<YAML
environment_confirmed: true
nonmem_license: $FIX/nonmem.lic
nonmem_image: pmx-harness/nonmem:from-yaml
YAML

show() { env -u NONMEM_IMAGE -u NONMEM_LICENSE "$@" \
           REPO_ROOT="$FIX" bash "$SCRIPT" --show-config 2>&1; }

# --- 1. harness.yaml supplies the image when the environment does not --------
out="$(show env)"
if grep -q '^image: pmx-harness/nonmem:from-yaml (harness.yaml)$' <<<"$out"; then
  pass "1 image resolved from harness.yaml"
else
  flunk "1 image not resolved from harness.yaml"; echo "$out"
fi

# --- 2. NONMEM_IMAGE overrides harness.yaml ---------------------------------
out="$(REPO_ROOT="$FIX" NONMEM_IMAGE=pmx-harness/nonmem:from-env \
        bash "$SCRIPT" --show-config 2>&1)"
if grep -q '^image: pmx-harness/nonmem:from-env (NONMEM_IMAGE)$' <<<"$out"; then
  pass "2 NONMEM_IMAGE overrides the harness.yaml key"
else
  flunk "2 NONMEM_IMAGE did not win"; echo "$out"
fi

# --- 3. image selection reports the chosen source without a site image ---
BARE="$TMP/bare"; mkdir -p "$BARE"
printf 'environment_confirmed: true\nnonmem_license: %s\n' "$FIX/nonmem.lic" \
  > "$BARE/harness.yaml"
out="$(env -u NONMEM_IMAGE -u NONMEM_LICENSE REPO_ROOT="$BARE" \
        bash "$SCRIPT" --show-config 2>&1)"
if grep -Eq '^image: .+ [(]default[)]$' <<<"$out"; then
  pass "3 the built-in fallback is attributed to the default source"
else
  flunk "3 default image source not reported"
fi

# --- 4. the license keeps its own order, and is reported by source ----------
out="$(show env)"
if grep -q "^license: $FIX/nonmem.lic (harness.yaml)$" <<<"$out"; then
  pass "4 license resolved from harness.yaml"
else
  flunk "4 license not resolved from harness.yaml"; echo "$out"
fi
out="$(REPO_ROOT="$FIX" NONMEM_LICENSE="$FIX/nonmem.lic" \
        bash "$SCRIPT" --show-config 2>&1)"
if grep -q "^license: $FIX/nonmem.lic (NONMEM_LICENSE)$" <<<"$out"; then
  pass "4b NONMEM_LICENSE overrides the harness.yaml key"
else
  flunk "4b NONMEM_LICENSE did not win"; echo "$out"
fi

# --- 5. delegated execution consumes one existing context -------------------
CTX="$TMP/pmx-resolved-config.context"; mkdir -m 700 "$CTX"
for pair in \
  'schema-version|1' 'provision|containerized' 'image|local/nonmem:captured' \
  'image-source|harness.yaml' "license|$FIX/nonmem.lic" \
  'license-source|harness.yaml' 'env-file|'; do
  key="${pair%%|*}"; value="${pair#*|}"; printf '%s\n' "$value" > "$CTX/$key"
done
chmod 600 "$CTX"/*
printf 'nonmem_image: [malformed\n' > "$FIX/harness.yaml"
DOCKER_BIN="$TMP/bin"; mkdir "$DOCKER_BIN"
cat > "$DOCKER_BIN/docker" <<'SH'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$DOCKER_ARGS"
SH
chmod 755 "$DOCKER_BIN/docker"
WORK="$TMP/work"; mkdir "$WORK"
PATH="$DOCKER_BIN:$PATH" DOCKER_ARGS="$TMP/docker-args" REPO_ROOT="$FIX" TMPDIR="$TMP" \
  bash "$SCRIPT" --resolved-context "$CTX" "$WORK" true >/dev/null 2>&1
status=$?
if [ "$status" = 0 ] && grep -qx 'local/nonmem:captured' "$TMP/docker-args" && \
   [ ! -e "$CTX" ]; then
  pass "5 delegated container uses captured context, ignores newer harness, cleans context"
else
  flunk "5 delegated container did not consume one captured context"
fi

# --- 6. malformed delegated contexts refuse before Docker -------------------
BAD="$TMP/pmx-resolved-config.bad"; mkdir -m 700 "$BAD"
for pair in \
  'schema-version|1' 'provision|containerized' 'image|line one' 'image-source|harness.yaml' \
  "license|$FIX/nonmem.lic" 'license-source|harness.yaml' \
  'env-file|' 'unknown|value'; do
  key="${pair%%|*}"; value="${pair#*|}"; printf '%s\n' "$value" > "$BAD/$key"
done
rm -f "$TMP/docker-args"
PATH="$DOCKER_BIN:$PATH" DOCKER_ARGS="$TMP/docker-args" REPO_ROOT="$FIX" TMPDIR="$TMP" \
  bash "$SCRIPT" --resolved-context "$BAD" "$WORK" true >/dev/null 2>&1
status=$?
if [ "$status" != 0 ] && [ ! -e "$TMP/docker-args" ] && [ ! -e "$BAD" ]; then
  pass "6 unknown context fields refuse before Docker and clean context"
else
  flunk "6 malformed context reached Docker or leaked context"
fi

SYMLINK_CTX="$TMP/pmx-resolved-config.symlink-field"; mkdir -m 700 "$SYMLINK_CTX"
for pair in \
  'schema-version|1' 'provision|containerized' 'image|local/nonmem:captured' \
  'image-source|harness.yaml' "license|$FIX/nonmem.lic" \
  'license-source|harness.yaml' 'env-file|'; do
  key="${pair%%|*}"; value="${pair#*|}"; printf '%s\n' "$value" > "$SYMLINK_CTX/$key"
done
rm "$SYMLINK_CTX/image"; ln -s "$SYMLINK_CTX/provision" "$SYMLINK_CTX/image"
PATH="$DOCKER_BIN:$PATH" DOCKER_ARGS="$TMP/docker-args" REPO_ROOT="$FIX" TMPDIR="$TMP" \
  bash "$SCRIPT" --resolved-context "$SYMLINK_CTX" "$WORK" true >/dev/null 2>&1
status=$?
if [ "$status" != 0 ] && [ ! -e "$TMP/docker-args" ] && [ ! -e "$SYMLINK_CTX" ]; then
  pass "6b symlink context fields refuse before Docker and clean context"
else
  flunk "6b symlink context field reached Docker or leaked context"
fi

INSECURE="$TMP/pmx-resolved-config.insecure"; mkdir -m 755 "$INSECURE"
for pair in \
  'schema-version|1' 'provision|containerized' 'image|local/nonmem:captured' \
  'image-source|harness.yaml' "license|$FIX/nonmem.lic" \
  'license-source|harness.yaml' 'env-file|'; do
  key="${pair%%|*}"; value="${pair#*|}"; printf '%s\n' "$value" > "$INSECURE/$key"
done
rm -f "$TMP/docker-args"
PATH="$DOCKER_BIN:$PATH" DOCKER_ARGS="$TMP/docker-args" REPO_ROOT="$FIX" TMPDIR="$TMP" \
  bash "$SCRIPT" --resolved-context "$INSECURE" "$WORK" true >/dev/null 2>&1
status=$?
if [ "$status" != 0 ] && [ ! -e "$TMP/docker-args" ] && [ ! -e "$INSECURE" ]; then
  pass "6c group/world-accessible context refuses before Docker and cleans context"
else
  flunk "6c insecure context reached Docker or leaked context"
fi

MULTILINE="$TMP/pmx-resolved-config.multiline"; mkdir -m 700 "$MULTILINE"
for pair in \
  'schema-version|1' 'provision|containerized' 'image|local/nonmem:captured' \
  'image-source|harness.yaml' "license|$FIX/nonmem.lic" \
  'license-source|harness.yaml' 'env-file|'; do
  key="${pair%%|*}"; value="${pair#*|}"; printf '%s\n' "$value" > "$MULTILINE/$key"
done
printf 'first\nsecond\n' > "$MULTILINE/image"
rm -f "$TMP/docker-args"
PATH="$DOCKER_BIN:$PATH" DOCKER_ARGS="$TMP/docker-args" REPO_ROOT="$FIX" TMPDIR="$TMP" \
  bash "$SCRIPT" --resolved-context "$MULTILINE" "$WORK" true >/dev/null 2>&1
status=$?
if [ "$status" != 0 ] && [ ! -e "$TMP/docker-args" ] && [ ! -e "$MULTILINE" ]; then
  pass "6d multiline context field refuses before Docker and cleans context"
else
  flunk "6d multiline context reached Docker or leaked context"
fi

# --- 7. direct execution retains workdir/argv -------------------------------
printf 'environment_confirmed: true\nnonmem_image: local/nonmem:direct\nnonmem_license: %s\n' \
  "$FIX/nonmem.lic" > "$FIX/harness.yaml"
rm -f "$TMP/docker-args"
PATH="$DOCKER_BIN:$PATH" DOCKER_ARGS="$TMP/docker-args" REPO_ROOT="$FIX" TMPDIR="$TMP" \
  bash "$SCRIPT" "$WORK" true 'argument with space' >/dev/null 2>&1
status=$?
if [ "$status" = 0 ] && grep -qx "$WORK" "$TMP/docker-args" && \
   grep -qx 'argument with space' "$TMP/docker-args"; then
  pass "7 direct container preserves workdir and exact argv"
else
  flunk "7 direct container lost workdir or argv"
fi

make_context() {
  local dir="$1"; mkdir -m 700 "$dir"
  local pair key value
  for pair in \
    'schema-version|1' 'provision|containerized' 'image|local/nonmem:captured' \
    'image-source|harness.yaml' "license|$FIX/nonmem.lic" \
    'license-source|harness.yaml' 'env-file|'; do
    key="${pair%%|*}"; value="${pair#*|}"; printf '%s\n' "$value" > "$dir/$key"
  done
}

HIDDEN="$TMP/pmx-resolved-config.hidden"; make_context "$HIDDEN"
printf 'unexpected\n' > "$HIDDEN/.unknown"
rm -f "$TMP/docker-args"
PATH="$DOCKER_BIN:$PATH" DOCKER_ARGS="$TMP/docker-args" REPO_ROOT="$FIX" TMPDIR="$TMP" \
  bash "$SCRIPT" --resolved-context "$HIDDEN" "$WORK" true >/dev/null 2>&1
status=$?
if [ "$status" != 0 ] && [ ! -e "$TMP/docker-args" ] && [ ! -e "$HIDDEN" ]; then
  pass "7b hidden unknown context field refuses and cleans context"
else
  flunk "7b hidden context field bypassed whitelist"
fi

ATTACK_PARENT="$TMP/pmx-resolved-config.attack"; mkdir "$ATTACK_PARENT"
VICTIM="$TMP/victim"; make_context "$VICTIM"; touch "$VICTIM/survival-marker"
rm -f "$TMP/docker-args"
PATH="$DOCKER_BIN:$PATH" DOCKER_ARGS="$TMP/docker-args" REPO_ROOT="$FIX" TMPDIR="$TMP" \
  bash "$SCRIPT" --resolved-context "$ATTACK_PARENT/../victim" "$WORK" true >/dev/null 2>&1
status=$?
if [ "$status" != 0 ] && [ ! -e "$TMP/docker-args" ] && \
   [ -e "$VICTIM/survival-marker" ]; then
  pass "7c traversal context refuses without touching disposable victim"
else
  flunk "7c traversal context reached Docker or touched disposable victim"
fi

# Compare the fixture repository with an independent path/mode/content witness.
# Only a stub named docker may consume the production wrapper argv.
repo="$TMP/public-project"
mkdir -p "$repo/models"
printf '$PROBLEM generated wrapper fixture\n' > "$repo/models/run.mod"
printf 'synthetic untracked input\n' > "$repo/local.txt"
if git -C "$repo" init -q &&
   git -C "$repo" -c user.name='Alex Rowan' -c user.email='fixture@example.invalid' \
     add -- models/run.mod; then
  census() {
    git -C "$repo" ls-files --stage
    git -C "$repo" ls-files --others --exclude-standard
    stat -c '%a %n' "$repo/models/run.mod" "$repo/local.txt"
    sha256sum "$repo/models/run.mod" "$repo/local.txt"
  }
  before="$(census)"
  run_wrapper() {
    PATH="$DOCKER_BIN:$PATH" DOCKER_ARGS="$TMP/docker-args" \
      REPO_ROOT="$repo" TMPDIR="$TMP" NONMEM_IMAGE=local/nonmem:direct \
      NONMEM_LICENSE="${NONMEM_LICENSE-$FIX/nonmem.lic}" NONMEM_EXTRA_MOUNTS= \
      bash "$SCRIPT" "$@"
  }
  manifest_args() {
    printf '%s\n' run --rm -v "$repo:$repo" \
      -v "$FIX/nonmem.lic:/nonmem/nm751/license/nonmem.lic:ro" \
      -w "$repo/models" --user "$(id -u):$(id -g)" "$1" inert 'argument with space' \
      > "$TMP/expected-docker-args"
    cmp -s "$TMP/expected-docker-args" "$TMP/docker-args"
  }
  context="$TMP/pmx-resolved-config.clean"
  make_context "$context"; chmod 600 "$context"/*
  if run_wrapper "$repo/models" inert 'argument with space' >/dev/null 2>&1 &&
     manifest_args local/nonmem:direct &&
     run_wrapper --resolved-context "$context" "$repo/models" inert 'argument with space' >/dev/null 2>&1 &&
     manifest_args local/nonmem:captured && [ ! -e "$context" ] && [ "$(census)" = "$before" ] &&
     [ -z "$(find "$TMP" -maxdepth 1 -type d -name 'pmx-resolved-config.*' ! -name 'pmx-resolved-config.attack' -print)" ]; then
    pass "NM-CLEAN: direct and delegated mount the scratch project and preserve its census"
  else
    flunk "NM-CLEAN: wrapper mount, argument, cleanup or source preservation failed"
  fi
  rm -f "$TMP/docker-args"
  if run_wrapper "$repo/models/absent" inert >/dev/null 2>&1 ||
     [ -e "$TMP/docker-args" ] || [ "$(census)" != "$before" ]; then
    flunk "NM-CLEAN: invalid workdir reached execution or changed source files"
  else
    pass "NM-CLEAN: missing workdir refuses before invoking the stub"
  fi
  if NONMEM_LICENSE="$TMP/absent.lic" run_wrapper "$repo/models" inert >/dev/null 2>&1 ||
     [ -e "$TMP/docker-args" ] || [ "$(census)" != "$before" ]; then
    flunk "NM-PUBLIC: missing configured license reached execution or changed source files"
  else
    pass "NM-PUBLIC: missing configured license fails before invocation"
  fi
else
  flunk "NM-CLEAN: could not initialize the disposable project"
fi

if [ $fail = 0 ]; then echo "nonmem-image tests: ALL PASS"; else echo "nonmem-image tests: FAILURES"; fi
exit $fail
