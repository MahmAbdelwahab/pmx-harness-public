#!/usr/bin/env bash
set -euo pipefail
stage=wrapper-path
report_failure() {
  printf 'FAIL baseline fixture stage=%s status=%s\n' "$1" "$2" >&2
}
trap 'exit_status=$?; if (( exit_status != 0 )); then report_failure "$stage" "$exit_status"; fi' EXIT
self=$(realpath -e -- "${BASH_SOURCE[0]}" 2>/dev/null)
original=$(realpath -e -- "$(dirname -- "$self")/../.." 2>/dev/null)
case "$original" in
  /tmp/pmx-harness-baseline-*)
    trap - EXIT
    echo 'REFUSE recursive baseline wrapper' >&2
    exit 1
    ;;
esac

if [[ ${1-} == --setup ]]; then
  stage=copy-setup
  receipt=$2
  copy=$(mktemp -d /tmp/pmx-harness-baseline-XXXXXXXX)
  copy=$(realpath -e -- "$copy")
  [[ $copy != "$original" && $copy != "$original/"* && $original != "$copy/"* ]]
  stage=copy-storage
  fs_type=$(findmnt -n -o FSTYPE -T "$copy")
  [[ -n $fs_type && $fs_type != tmpfs ]]
  stage=disk-reserve
  # Reserve 128 MiB on the fixture filesystem for the 50 MiB source cap
  # plus synthetic inputs, logs, and scratch writes.
  LC_ALL=C df -k --output=avail "$copy" | awk '
    NR == 2 && NF == 1 && $1 ~ /^[0-9]+$/ && $1 + 0 >= 131072 { enough = 1 }
    END { exit !(NR == 2 && enough) }
  '
  stage=copy-setup
  printf '%s\n' "$copy" > "$receipt"
  printf 'SOURCE_ROOT=%s\nCOPY_ROOT=%s\n' "$original" "$copy"
  files=(pipeline/compose.R pipeline/tasks/baseline_input.R pipeline/tasks/baseline_fit.R
    tools/nonmem_exec.sh tools/nonmem_container.sh tools/tests/test_baseline_fit.sh
    tools/tests/fixtures/baseline_fit_worker.R skills/baseline-fit/SKILL.md
    skills/baseline-fit/reference/baseline_fit.R
    skills/nonmem-estimation/reference/bbr_estimation.R skills/nonmem-estimation/MANIFEST.yaml
    .github/workflows/ci.yaml config/r-packages.lock)
  for path in "$original"/tools/*.R; do files+=("${path#"$original"/}"); done
  if [[ -e $original/skills/baseline-fit/MANIFEST.yaml || -L $original/skills/baseline-fit/MANIFEST.yaml ]]; then
    files+=(skills/baseline-fit/MANIFEST.yaml)
    printf 'BOOTSTRAP_MANIFEST=present-required\n'
  else
    printf 'BOOTSTRAP_MANIFEST=absent-new-manifest-only\n'
  fi
  [[ ${#files[@]} -le 200 ]]
  : > "$copy/copy-files.txt"
  : > "$copy/copy-sha256.txt"
  total=0
  for relative in "${files[@]}"; do
    [[ $relative != *$'\n'* && $relative != *$'\r'* ]]
    LC_ALL=C grep -q '[^ -~]' <<< "$relative" && exit 1
    source_path=$original/$relative
    [[ -f $source_path && ! -L $source_path ]]
    [[ $(realpath -e -- "$source_path") == "$source_path" ]]
    size=$(stat -c %s -- "$source_path")
    [[ $size -le 10485760 ]]
    total=$((total + size))
    [[ $total -le 52428800 ]]
    printf '%s\n' "$relative" >> "$copy/copy-files.txt"
    digest=$(sha256sum -- "$source_path")
    printf '%s  %s\n' "${digest%% *}" "$relative" >> "$copy/copy-sha256.txt"
  done
  rsync -a --files-from="$copy/copy-files.txt" --exclude=.git --exclude=__pycache__ \
    --exclude='*.pyc' --exclude=harness.yaml --exclude=.mcp.json --exclude=.token_seed \
    --exclude=.env --exclude=.harness-unlock --exclude='/_targets*' --exclude=software \
    --exclude=.agent-scratch --exclude=.claude --exclude=.codex --exclude=.agents \
    --exclude=.aws --exclude=.ssh --exclude=.gnupg --exclude=pilot --exclude=gates \
    --exclude=stores --exclude=oracles --exclude=hands-on-examples "$original/" "$copy/"
  cd "$copy"
  sha256sum --check --status copy-sha256.txt
  expected=$(wc -l < copy-files.txt)
  actual=$(find . -type f | wc -l)
  [[ $actual -eq $((expected + 2)) ]]
  nonce=$(basename "$copy")
  printf 'original_root=%s\ncopy_root=%s\nnonce=%s\n' "$original" "$copy" "$nonce" > .baseline-fixture-copy
  printf 'CENSUS_FILES=%s\nCENSUS_BYTES=%s\n' "$expected" "$total"
  exit 0
fi

stage=rscript-resolution
rscript=$(type -P Rscript)
rscript=$(realpath -e -- "$rscript" 2>/dev/null)
[[ $rscript == /* && -f $rscript && -x $rscript ]]

stage=library-admission
[[ -n ${R_LIBS_USER-} ]]
libraries=()
for key in R_LIBS_USER R_LIBS_SITE R_LIBS; do
  value=${!key-}
  if [[ -n $value ]]; then
    [[ $value != *$'\n'* && $value != *$'\r'* ]]
    IFS=: read -r -a components <<< "$value"
    for component in "${components[@]}"; do
      [[ -n $component && -d $component ]]
      canonical=$(realpath -e -- "$component" 2>/dev/null)
      [[ $canonical == "$component" && $canonical != "$original" && $canonical != "$original/"* ]]
      case "$canonical" in /tmp/pmx-harness-baseline-*) exit 1;; esac
    done
    libraries+=("$key=$value")
  fi
done
stage=setup-receipt
receipt=$(mktemp /tmp/pmx-baseline-setup-receipt-XXXXXXXX 2>/dev/null)
setup_log=$receipt.log
start=$SECONDS
stage=copy-setup
if timeout --signal=TERM --kill-after=5s 60s env -i PATH=/usr/bin:/bin LANG=C.UTF-8 LC_ALL=C.UTF-8 \
  PYTHONDONTWRITEBYTECODE=1 bash "$self" --setup "$receipt" > "$setup_log" 2>&1; then
  setup_seconds=$((SECONDS - start))
else
  setup_status=$?
  for setup_stage in copy-storage disk-reserve; do
    if grep -Fxq "FAIL baseline fixture stage=$setup_stage status=$setup_status" "$setup_log" 2>/dev/null; then
      stage=$setup_stage
      break
    fi
  done
  exit "$setup_status"
fi
cat "$setup_log" 2>/dev/null
copy=$(cat "$receipt" 2>/dev/null)
[[ -d $copy && $copy == /tmp/pmx-harness-baseline-* ]]
mkdir "$copy/fixture-home" "$copy/fixture-tmp" 2>/dev/null
stage=original-cwd-guard
set +e
(
  cd "$original"
  timeout --signal=TERM --kill-after=5s 30s env -i PATH=/usr/bin:/bin LANG=C.UTF-8 LC_ALL=C.UTF-8 \
    PYTHONDONTWRITEBYTECODE=1 HOME="$copy/fixture-home" TMPDIR="$copy/fixture-tmp" \
    "${libraries[@]}" "$rscript" --vanilla "$copy/tools/tests/fixtures/baseline_fit_worker.R"
) > "$copy/guard-negative.log" 2>&1
guard_status=$?
timeout --signal=TERM --kill-after=5s 30s env -i PATH=/usr/bin:/bin LANG=C.UTF-8 LC_ALL=C.UTF-8 \
  PYTHONDONTWRITEBYTECODE=1 bash "$copy/tools/tests/test_baseline_fit.sh" > "$copy/recursive-negative.log" 2>&1
recursive_status=$?
set -e
[[ $guard_status -ne 0 && $guard_status -ne 124 ]]
grep -Fq 'baseline fixture: external wrapper copy required' "$copy/guard-negative.log" 2>/dev/null
stage=recursive-wrapper-guard
[[ $recursive_status -ne 0 && $recursive_status -ne 124 ]]
grep -Fq 'REFUSE recursive baseline wrapper' "$copy/recursive-negative.log" 2>/dev/null
printf 'PASS original-cwd copied-worker guard refusal\nPASS recursive-wrapper refusal\n'
stage=worker
cd "$copy" 2>/dev/null
start=$SECONDS
set +e
timeout --signal=TERM --kill-after=5s 300s env -i PATH=/usr/bin:/bin LANG=C.UTF-8 LC_ALL=C.UTF-8 \
  PYTHONDONTWRITEBYTECODE=1 HOME="$copy/fixture-home" TMPDIR="$copy/fixture-tmp" \
  "${libraries[@]}" "$rscript" --vanilla tools/tests/fixtures/baseline_fit_worker.R > worker.log 2>&1
status=$?
set -e
worker_seconds=$((SECONDS - start))
if (( status == 0 )); then
  stage=source-census
fi
(
  cd "$original"
  sha256sum --check --status "$copy/copy-sha256.txt"
) 2>/dev/null
if (( status == 0 )); then
  cat worker.log 2>/dev/null
fi
printf 'PASS original selected source census unchanged\n'
printf 'SETUP_SECONDS=%s\nWORKER_SECONDS=%s\nWORKER_EXIT=%s\nRETAINED_COPY=%s\nSETUP_RECEIPT=%s\n' \
  "$setup_seconds" "$worker_seconds" "$status" "$copy" "$receipt"
exit "$status"
