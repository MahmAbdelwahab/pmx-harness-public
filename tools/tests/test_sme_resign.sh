#!/usr/bin/env bash
# Test of the SME re-sign utility tools/sme_resign.sh (integrity batch 16,
# item 16.10; Codex round 15 finding R15-P3-2, SME decision D16-4. Extended in
# integrity batch 17 for Codex round 16 findings R16-P2-4 and R16-P2-5, SME
# decisions D17-1 and D17-2).
#
# CLAUDE.md describes a signing script that "refuses to run with the live
# repository as its working directory unless the SME passes an explicit flag".
# Until batch 16 that guard lived only in a per-batch scratchpad script, so a
# fresh checkout could neither reuse nor test it. The utility is now a tracked
# artifact and this is its acceptance set.
#
# Every case runs against a disposable fixture under a temp directory. The one
# case that concerns the live repository asserts a REFUSAL, so it never writes.
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# SME_RESIGN_UNDER_TEST lets the same battery run against another copy of the
# utility (the HEAD extract, for a red run) without editing the test.
UTIL="${SME_RESIGN_UNDER_TEST:-$REPO_ROOT/tools/sme_resign.sh}"

REVIEWER="Fixture SME Reviewer"

PASS=0
FAIL=0
ok() { printf 'PASS  %-64s\n' "$1"; PASS=$((PASS + 1)); }
no() { printf 'FAIL  %-64s\n' "$1"; FAIL=$((FAIL + 1)); }
check() { if [ "$1" = "yes" ]; then ok "$2"; else no "$2"; fi; }

FIX="$(mktemp -d)"
trap 'rm -rf "$FIX"' EXIT

# --- A disposable manifest fixture -----------------------------------------
# The shape the utility rewrites, and the shape write_method_manifest() emits: a
# root signature block whose reviewed_by and reviewed_at sit on lines 10 and 11,
# `units:` at 12, and unit keys written `  <name>:` with their fields indented
# four. The two unit blocks are the SAME SIZE, which is what makes the
# swapped-blocks case below possible at all.
#   13 alpha:   14 alpha's status   17 beta:   18 beta's status
mk_manifest() {
  local p="$1"
  mkdir -p "$(dirname "$p")"
  cat > "$p" <<'YAML'
schema: 2
skill: fixture
reference_models: false
notes: ""
generated_at: ""
generator: ""
manifest_version: 1
policy: ""
signature_block: ""
reviewed_by: ""
reviewed_at: ""
units:
  alpha:
    status: draft
    entrypoints: [reference_alpha]
    reason: "the alpha unit"
  beta:
    status: draft
    entrypoints: [reference_beta]
    reason: "the beta unit"
YAML
}

sha_of() { local s; s="$(sha256sum "$1")"; printf 'sha256:%s' "${s%% *}"; }

# The list in the batch 17 grammar (SME decision D17-1): a pre-sign digest per
# manifest, and every approval naming its unit as well as its line.
mk_list() {                       # mk_list <list-path> <manifest-path-on-disk>
  local l="$1" m="$2"
  cat > "$l" <<LIST
# fixture re-sign list
digest skills/fixture/MANIFEST.yaml $(sha_of "$m")
root skills/fixture/MANIFEST.yaml
appr skills/fixture/MANIFEST.yaml alpha:14 beta:18
LIST
}

# --- Case 1: the utility exists and is executable ---------------------------
if [ -x "$UTIL" ]; then ok "tools/sme_resign.sh exists and is executable"
else no "tools/sme_resign.sh exists and is executable"; fi

# --- Case 2: refuses at the root of a git checkout without --live -----------
# The condition the guard actually decides on is "the working directory is the
# root of a git checkout", which is what the live repository is. The case is run
# against a DISPOSABLE checkout created here rather than against $REPO_ROOT: the
# assertion then holds wherever this battery runs. A rehearsal copy made by
# rsync (for example under the session scratchpad in /tmp) is not a git
# checkout at all, so the guard allows it for that reason, and the old form of
# this case, which assumed $REPO_ROOT, failed there. Nothing in the live tree
# is read or written by it.
CKOUT="$FIX/checkout"
mkdir -p "$CKOUT"
(cd "$CKOUT" && git init -q .) >/dev/null 2>&1
out="$(cd "$CKOUT" && bash "$UTIL" --reviewer "$REVIEWER" /dev/null 2>&1)"; rc=$?
check "$([ "$rc" -ne 0 ] && echo yes || echo no)" "live root without --live exits non-zero"
check "$(printf '%s' "$out" | grep -qi 'REFUS' && echo yes || echo no)" \
      "live root without --live prints a refusal naming the flag"
check "$(printf '%s' "$out" | grep -q -- '--live' && echo yes || echo no)" \
      "the refusal names --live"

# --- Case 3: runs in a disposable copy --------------------------------------
COPY="$FIX/copy"
mkdir -p "$COPY"
M="$COPY/skills/fixture/MANIFEST.yaml"
mk_manifest "$M"
mk_list "$COPY/list.txt" "$M"
out="$(cd "$COPY" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc=$?
check "$([ "$rc" -eq 0 ] && echo yes || echo no)" "runs in a disposable copy (exit 0)"
check "$([ "$(sed -n 10p "$M")" = 'reviewed_by: "Fixture SME Reviewer"' ] && echo yes || echo no)" \
      "root reviewed_by rewritten"
check "$([ "$(sed -n 11p "$M")" = 'reviewed_at: "2026-09-06"' ] && echo yes || echo no)" \
      "root reviewed_at rewritten to the supplied date"
check "$([ "$(sed -n 14p "$M")" = '    status: approved' ] && echo yes || echo no)" \
      "alpha's status line (14) approved"
check "$([ "$(sed -n 18p "$M")" = '    status: approved' ] && echo yes || echo no)" \
      "beta's status line (18) approved"
check "$(grep -q '^digest-signed skills/fixture/MANIFEST.yaml sha256:' "$COPY/list.txt" && echo yes || echo no)" \
      "the post-sign digest is recorded in the list (D17-1)"

# Exercise the required operator flag before any manifest or receipt mutation.
ROLE_COPY="$FIX/role-copy"
ROLE_M="$ROLE_COPY/skills/fixture/MANIFEST.yaml"
mk_manifest "$ROLE_M"
mk_list "$ROLE_COPY/list.txt" "$ROLE_M"
role_manifest_before="$(sha_of "$ROLE_M")"
role_list_before="$(sha_of "$ROLE_COPY/list.txt")"
out="$(cd "$ROLE_COPY" && bash "$UTIL" list.txt 2026-09-06 2>&1)"; rc=$?
check "$([ "$rc" -ne 0 ] && [[ "$out" == *'--reviewer'* ]] && echo yes || echo no)" \
      "RESIGN-LOCAL: missing explicit reviewer is refused"
for role in '' ' sMe ' $'THE\t SME' 'Bad "Reviewer"'; do
  out="$(cd "$ROLE_COPY" && bash "$UTIL" --reviewer "$role" list.txt 2026-09-06 2>&1)"; rc=$?
  check "$([ "$rc" -ne 0 ] && [[ "$out" == *'reviewer'* ]] &&
             [ "$(sha_of "$ROLE_M")" = "$role_manifest_before" ] &&
             [ "$(sha_of "$ROLE_COPY/list.txt")" = "$role_list_before" ] && echo yes || echo no)" \
        "RESIGN-LOCAL: invalid reviewer leaves manifest and receipt untouched"
done
punctuation_reviewer="Fixture A&B | O'Neil"
out="$(cd "$ROLE_COPY" && bash "$UTIL" --reviewer "$punctuation_reviewer" list.txt 2026-09-06 2>&1)"; rc=$?
check "$([ "$rc" -eq 0 ] && [ "$(sed -n 10p "$ROLE_M")" = "reviewed_by: \"$punctuation_reviewer\"" ] &&
            grep -qxF "digest-signed skills/fixture/MANIFEST.yaml $(sha_of "$ROLE_M")" "$ROLE_COPY/list.txt" &&
            echo yes || echo no)" \
      "RESIGN-LOCAL: ordinary reviewer punctuation and exact receipt survive re-sign"

# --- Case 4: idempotent -----------------------------------------------------
before="$(cat "$M")"
out2="$(cd "$COPY" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc2=$?
after="$(cat "$M")"
check "$([ "$rc2" -eq 0 ] && echo yes || echo no)" "a second run exits 0 (idempotent)"
check "$([ "$before" = "$after" ] && echo yes || echo no)" "a second run changes no byte"
check "$([ "$(grep -c '^digest-signed ' "$COPY/list.txt")" -eq 1 ] && echo yes || echo no)" \
      "the second run records no second digest-signed line"
# A THIRD unchanged run is still a no-op (integrity batch 18, SME decision
# D18-2): the receipt is accepted only because nothing remains to be written.
out2c="$(cd "$COPY" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc2c=$?
check "$([ "$rc2c" -eq 0 ] && echo yes || echo no)" "a third unchanged run exits 0 (D18-2)"
check "$([ "$(cat "$M")" = "$after" ] && echo yes || echo no)" "a third run changes no byte"

# --- Case 5: asserts before rewriting ---------------------------------------
# A line that is neither the draft text the list expects nor the approved text
# it would write is a REFUSAL, not a silent overwrite.
COPY2="$FIX/copy2"
mkdir -p "$COPY2"
M2="$COPY2/skills/fixture/MANIFEST.yaml"
mk_manifest "$M2"
sed -i '14s/.*/    status: rejected/' "$M2"
mk_list "$COPY2/list.txt" "$M2"
snapshot="$(cat "$M2")"
out3="$(cd "$COPY2" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc3=$?
check "$([ "$rc3" -ne 0 ] && echo yes || echo no)" "an unexpected unit line is refused"
check "$(printf '%s' "$out3" | grep -qi 'REFUS' && echo yes || echo no)" \
      "the unexpected unit line refusal names the file and line"
check "$([ "$(cat "$M2")" = "$snapshot" ] && echo yes || echo no)" \
      "a refused run rewrites nothing"

# --- Case 6: an unexpected root signature line is refused -------------------
COPY3="$FIX/copy3"
mkdir -p "$COPY3"
M3="$COPY3/skills/fixture/MANIFEST.yaml"
mk_manifest "$M3"
sed -i '10s/.*/reviewed_by: "SOMEONE ELSE"/' "$M3"
mk_list "$COPY3/list.txt" "$M3"
snap3="$(cat "$M3")"
out4="$(cd "$COPY3" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc4=$?
check "$([ "$rc4" -ne 0 ] && echo yes || echo no)" "an unexpected root signature line is refused"
check "$([ "$(cat "$M3")" = "$snap3" ] && echo yes || echo no)" \
      "the refused root run rewrites nothing"

# --- Case 7: an unknown directive in the list file is refused ---------------
COPY4="$FIX/copy4"
mkdir -p "$COPY4"
mk_manifest "$COPY4/skills/fixture/MANIFEST.yaml"
printf 'revoke skills/fixture/MANIFEST.yaml alpha:14\n' > "$COPY4/list.txt"
snap4="$(cat "$COPY4/skills/fixture/MANIFEST.yaml")"
out5="$(cd "$COPY4" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc5=$?
check "$([ "$rc5" -ne 0 ] && echo yes || echo no)" "an unknown list directive is refused"
check "$([ "$(cat "$COPY4/skills/fixture/MANIFEST.yaml")" = "$snap4" ] && echo yes || echo no)" \
      "the refused directive run rewrites nothing"

# --- Case 8: a missing list file is refused ---------------------------------
out6="$(cd "$FIX" && bash "$UTIL" --reviewer "$REVIEWER" no-such-list.txt 2026-09-06 2>&1)"; rc6=$?
check "$([ "$rc6" -ne 0 ] && echo yes || echo no)" "a missing list file is refused"

# --- Case 8b: the live-tree guard fails CLOSED when git is unusable ---------
# Review observation O2, integrity batch 16 fix round 1.
STUB="$FIX/stubbin"
mkdir -p "$STUB"
printf '#!/bin/sh\nexit 128\n' > "$STUB/git"
chmod +x "$STUB/git"
out8b="$(cd "$CKOUT" && PATH="$STUB:$PATH" bash "$UTIL" --reviewer "$REVIEWER" /nonexistent-list.txt 2026-09-06 2>&1)"; rc8b=$?
check "$([ "$rc8b" -ne 0 ] && echo yes || echo no)" "an unusable git exits non-zero"
check "$(printf '%s' "$out8b" | grep -q 'git is not usable here' && echo yes || echo no)" \
      "an unusable git is refused before anything else (fails closed)"

# --- Case 8b2: git WORKS but repository discovery FAILS ---------------------
# Codex round 16 R16-P2-4, SME decision D17-2. `git --version` succeeding is not
# evidence that discovery succeeded: a broken worktree pointer, an ownership
# refusal or an inherited invalid GIT_DIR all let git report its version and
# still fail to name the repository. Round 16 discarded that failure with
# `|| true` and read the empty result as "not a checkout, proceed" -- in the
# live root. The stub reproduces exactly that split.
STUB2="$FIX/stubbin2"
mkdir -p "$STUB2"
cat > "$STUB2/git" <<'STUBSH'
#!/bin/sh
if [ "$1" = "--version" ]; then echo "git version 2.99.0-stub"; exit 0; fi
echo "fatal: detected dubious ownership in repository at '$(pwd)'" >&2
exit 128
STUBSH
chmod +x "$STUB2/git"
DISC="$FIX/disc"
mkdir -p "$DISC"
MD="$DISC/skills/fixture/MANIFEST.yaml"
mk_manifest "$MD"
mk_list "$DISC/list.txt" "$MD"
snapd="$(cat "$MD")"
out8d="$(cd "$DISC" && PATH="$STUB2:$PATH" bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc8d=$?
check "$([ "$rc8d" -ne 0 ] && echo yes || echo no)" \
      "a failed repository discovery exits non-zero (R16-P2-4)"
check "$(printf '%s' "$out8d" | grep -q 'could not identify the repository' && echo yes || echo no)" \
      "the discovery failure has its own refusal message, distinct from 'git is not usable'"
check "$([ "$(cat "$MD")" = "$snapd" ] && echo yes || echo no)" \
      "the discovery-failure refusal rewrites nothing"

# ... and the sanctioned rehearsal still works: a copy with no .git is not a
# checkout root, and git says so explicitly, so the guard allows it.
COPY6="$FIX/copy6"
mkdir -p "$COPY6"
M6="$COPY6/skills/fixture/MANIFEST.yaml"
mk_manifest "$M6"
mk_list "$COPY6/list.txt" "$M6"
out8c="$(cd "$COPY6" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc8c=$?
check "$([ "$rc8c" -eq 0 ] && echo yes || echo no)" \
      "a disposable copy that is not a git checkout is still allowed"

# --- Case 9: a missing manifest named by the list is refused ----------------
COPY5="$FIX/copy5"
mkdir -p "$COPY5"
M5tmp="$FIX/tmpman.yaml"; mk_manifest "$M5tmp"
mk_list "$COPY5/list.txt" "$M5tmp"          # the list names a path that is absent here
out7="$(cd "$COPY5" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc7=$?
check "$([ "$rc7" -ne 0 ] && echo yes || echo no)" "a list naming a missing manifest is refused"

# --- Case 10: swapped equal-size unit blocks are REFUSED --------------------
# Codex round 16 R16-P2-5. After the SME reviews the list, alpha's and beta's
# blocks exchange positions. Line 14 still reads `    status: draft`, so the
# round-16 utility approved it -- as beta, which the SME did not select. The
# unit key above the addressed line now decides.
COPY7="$FIX/copy7"
mkdir -p "$COPY7"
M7="$COPY7/skills/fixture/MANIFEST.yaml"
mk_manifest "$M7"
mk_list "$COPY7/list.txt" "$M7"
# swap the two four-line blocks (13-16 and 17-20)
{ sed -n '1,12p' "$M7"; sed -n '17,20p' "$M7"; sed -n '13,16p' "$M7"; } > "$M7.new"
mv "$M7.new" "$M7"
# re-record the digest so the DIGEST check is not what refuses: this case is
# about the unit binding alone.
sed -i "2s|.*|digest skills/fixture/MANIFEST.yaml $(sha_of "$M7")|" "$COPY7/list.txt"
snap7="$(cat "$M7")"
out10="$(cd "$COPY7" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc10=$?
check "$([ "$(sed -n 14p "$M7")" = '    status: draft' ] && echo yes || echo no)" \
      "case 10 setup: line 14 still reads 'status: draft' after the swap"
check "$([ "$(sed -n 13p "$M7")" = '  beta:' ] && echo yes || echo no)" \
      "case 10 setup: line 14 now belongs to beta, not alpha"
check "$([ "$rc10" -ne 0 ] && echo yes || echo no)" "swapped equal-size unit blocks are refused"
check "$(printf '%s' "$out10" | grep -q "belongs to unit 'beta'" && echo yes || echo no)" \
      "the swap refusal names the unit actually found (R16-P2-5)"
check "$([ "$(cat "$M7")" = "$snap7" ] && echo yes || echo no)" \
      "the swap refusal rewrites nothing"

# --- Case 11: a manifest that changed after the review is REFUSED ----------
COPY8="$FIX/copy8"
mkdir -p "$COPY8"
M8="$COPY8/skills/fixture/MANIFEST.yaml"
mk_manifest "$M8"
mk_list "$COPY8/list.txt" "$M8"
sed -i '4s/.*/notes: "edited after the SME reviewed the list"/' "$M8"
snap8="$(cat "$M8")"
out11="$(cd "$COPY8" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc11=$?
check "$([ "$rc11" -ne 0 ] && echo yes || echo no)" "a manifest digest mismatch is refused (D17-1)"
check "$(printf '%s' "$out11" | grep -q 'has changed since the SME reviewed this list' && echo yes || echo no)" \
      "the digest refusal says the manifest changed since the review"
check "$([ "$(cat "$M8")" = "$snap8" ] && echo yes || echo no)" \
      "the digest refusal rewrites nothing"

# --- Case 12: a list with no digest line for a signed manifest is REFUSED ---
COPY9="$FIX/copy9"
mkdir -p "$COPY9"
M9="$COPY9/skills/fixture/MANIFEST.yaml"
mk_manifest "$M9"
mk_list "$COPY9/list.txt" "$M9"
sed -i '2d' "$COPY9/list.txt"
snap9="$(cat "$M9")"
out12="$(cd "$COPY9" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc12=$?
check "$([ "$rc12" -ne 0 ] && echo yes || echo no)" "a list with no digest line is refused"
check "$([ "$(cat "$M9")" = "$snap9" ] && echo yes || echo no)" \
      "the missing-digest refusal rewrites nothing"

# --- Case 13: a malformed date is REFUSED before any rewrite ---------------
COPY10="$FIX/copy10"
mkdir -p "$COPY10"
M10="$COPY10/skills/fixture/MANIFEST.yaml"
mk_manifest "$M10"
mk_list "$COPY10/list.txt" "$M10"
snap10="$(cat "$M10")"
out13="$(cd "$COPY10" && bash "$UTIL" --reviewer "$REVIEWER" list.txt '2026-09-06"; rm -rf /' 2>&1)"; rc13=$?
check "$([ "$rc13" -ne 0 ] && echo yes || echo no)" "a malformed date is refused (R16-P2-5)"
check "$(printf '%s' "$out13" | grep -q 'is not YYYY-MM-DD' && echo yes || echo no)" \
      "the date refusal names the required format"
check "$([ "$(cat "$M10")" = "$snap10" ] && echo yes || echo no)" \
      "the date refusal rewrites nothing"

# --- Case 14: an appr entry without its unit name is REFUSED ---------------
COPY11="$FIX/copy11"
mkdir -p "$COPY11"
M11="$COPY11/skills/fixture/MANIFEST.yaml"
mk_manifest "$M11"
mk_list "$COPY11/list.txt" "$M11"
sed -i '4s|.*|appr skills/fixture/MANIFEST.yaml 14 18|' "$COPY11/list.txt"
snap11="$(cat "$M11")"
out14="$(cd "$COPY11" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc14=$?
check "$([ "$rc14" -ne 0 ] && echo yes || echo no)" "a bare line number without its unit is refused (D17-1)"
check "$([ "$(cat "$M11")" = "$snap11" ] && echo yes || echo no)" \
      "the bare-line-number refusal rewrites nothing"

# --- Case 15: an inline '#' is REFUSED, a full-line comment is not ---------
COPY12="$FIX/copy12"
mkdir -p "$COPY12"
M12="$COPY12/skills/fixture/MANIFEST.yaml"
mk_manifest "$M12"
mk_list "$COPY12/list.txt" "$M12"
printf 'root skills/fixture/MANIFEST.yaml # trailing\n' >> "$COPY12/list.txt"
snap12="$(cat "$M12")"
out15="$(cd "$COPY12" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc15=$?
check "$([ "$rc15" -ne 0 ] && echo yes || echo no)" "an inline '#' in the list is refused (R16-P2-5)"
check "$([ "$(cat "$M12")" = "$snap12" ] && echo yes || echo no)" \
      "the inline-comment refusal rewrites nothing"

# --- Case 16: an inherited repository-routing variable is REFUSED -----------
# Codex round 17 R17-P2-4, integrity batch 18. `GIT_DIR=/nonexistent git
# rev-parse --show-toplevel` fails with "fatal: not a git repository:
# '/nonexistent'". Batch 17 accepted ANY diagnostic containing "not a git
# repository" as proof that this directory is in no work tree, so an invalid
# inherited GIT_DIR was read as an ordinary rehearsal -- including in the live
# root, where the guard would then permit signing without --live. A discovery
# failure with GIT_DIR, GIT_WORK_TREE or GIT_COMMON_DIR set is now its own
# refusal, naming the variable.
COPY13="$FIX/copy13"
mkdir -p "$COPY13"
M13="$COPY13/skills/fixture/MANIFEST.yaml"
mk_manifest "$M13"
mk_list "$COPY13/list.txt" "$M13"
snap13="$(cat "$M13")"
out16="$(cd "$COPY13" && GIT_DIR=/nonexistent-pmx-b18-gitdir bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc16=$?
check "$([ "$rc16" -ne 0 ] && echo yes || echo no)" \
      "an invalid inherited GIT_DIR is refused (R17-P2-4)"
check "$(printf '%s' "$out16" | grep -q 'GIT_DIR' && echo yes || echo no)" \
      "the routing-variable refusal names the variable"
check "$([ "$(cat "$M13")" = "$snap13" ] && echo yes || echo no)" \
      "the routing-variable refusal rewrites nothing"

# --- Case 17: an impossible calendar date is REFUSED ------------------------
# Codex round 17 R17-P2-5. Batch 17 checked the SHAPE only, so 2026-02-30 was
# written into a manifest that tools/method_manifest.R then refuses to read.
d_case() {                        # d_case <date> <expected-rc: zero|nonzero>
  local d="$1" want="$2" dir out rc
  dir="$FIX/date_$(printf '%s' "$d" | tr -c 'A-Za-z0-9' '_')"
  mkdir -p "$dir"
  mk_manifest "$dir/skills/fixture/MANIFEST.yaml"
  mk_list "$dir/list.txt" "$dir/skills/fixture/MANIFEST.yaml"
  out="$(cd "$dir" && bash "$UTIL" --reviewer "$REVIEWER" list.txt "$d" 2>&1)"; rc=$?
  if [ "$want" = "nonzero" ]; then
    check "$([ "$rc" -ne 0 ] && echo yes || echo no)" "the date '$d' is refused (R17-P2-5)"
    check "$([ "$(sed -n 14p "$dir/skills/fixture/MANIFEST.yaml")" = '    status: draft' ] && echo yes || echo no)" \
          "the '$d' refusal rewrites nothing"
  else
    check "$([ "$rc" -eq 0 ] && echo yes || echo no)" "the real date '$d' is accepted"
    check "$([ "$(sed -n 11p "$dir/skills/fixture/MANIFEST.yaml")" = "reviewed_at: \"$d\"" ] && echo yes || echo no)" \
          "the real date '$d' is written to reviewed_at"
  fi
}
d_case 2026-02-30 nonzero
d_case 2026-13-01 nonzero
d_case 2025-02-29 nonzero
d_case 0026-09-06 nonzero        # real date, but R format() does not zero-pad the year (batch 18 review B1)
d_case 0000-01-01 nonzero
d_case 2024-02-29 zero
d_case 2026-09-06 zero

# --- Case 18: duplicate digest directives are REFUSED -----------------------
# Codex round 17 R17-P2-6. Two `digest` lines for one manifest silently took the
# last value, so a concatenated or amended list could carry an ambiguous
# reviewed input and still run.
COPY14="$FIX/copy14"
mkdir -p "$COPY14"
M14="$COPY14/skills/fixture/MANIFEST.yaml"
mk_manifest "$M14"
mk_list "$COPY14/list.txt" "$M14"
printf 'digest skills/fixture/MANIFEST.yaml %s\n' "$(sha_of "$M14")" >> "$COPY14/list.txt"
snap14="$(cat "$M14")"
out18="$(cd "$COPY14" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc18=$?
check "$([ "$rc18" -ne 0 ] && echo yes || echo no)" \
      "a second 'digest' line for one manifest is refused (R17-P2-6)"
check "$(printf '%s' "$out18" | grep -q 'line 2' && printf '%s' "$out18" | grep -q 'line 5' && echo yes || echo no)" \
      "the duplicate-digest refusal names both line numbers"
check "$([ "$(cat "$M14")" = "$snap14" ] && echo yes || echo no)" \
      "the duplicate-digest refusal rewrites nothing"

# --- Case 19: duplicate digest-signed directives are REFUSED ----------------
COPY15="$FIX/copy15"
mkdir -p "$COPY15"
M15="$COPY15/skills/fixture/MANIFEST.yaml"
mk_manifest "$M15"
mk_list "$COPY15/list.txt" "$M15"
printf 'digest-signed skills/fixture/MANIFEST.yaml %s\n' "$(sha_of "$M15")" >> "$COPY15/list.txt"
printf 'digest-signed skills/fixture/MANIFEST.yaml %s\n' "$(sha_of "$M15")" >> "$COPY15/list.txt"
snap15="$(cat "$M15")"
out19="$(cd "$COPY15" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc19=$?
check "$([ "$rc19" -ne 0 ] && echo yes || echo no)" \
      "a second 'digest-signed' line for one manifest is refused (R17-P2-6)"
check "$([ "$(cat "$M15")" = "$snap15" ] && echo yes || echo no)" \
      "the duplicate-receipt refusal rewrites nothing"

# --- Case 20: a list amended after signing is REFUSED (D18-2) ---------------
# The receipt is a NO-OP RECEIPT ONLY. Sign alpha; then append an approval for
# beta without a fresh reviewed pre-sign digest. The manifest on disk now
# matches only the post-sign digest, and batch 17 accepted that and approved
# beta -- a unit signed under a digest the SME never reviewed. A re-run is
# accepted through the receipt only when every target line already carries its
# target text, so this run is refused with nothing written.
COPY16="$FIX/copy16"
mkdir -p "$COPY16"
M16="$COPY16/skills/fixture/MANIFEST.yaml"
mk_manifest "$M16"
cat > "$COPY16/list.txt" <<LIST
digest skills/fixture/MANIFEST.yaml $(sha_of "$M16")
root skills/fixture/MANIFEST.yaml
appr skills/fixture/MANIFEST.yaml alpha:14
LIST
out20a="$(cd "$COPY16" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc20a=$?
check "$([ "$rc20a" -eq 0 ] && echo yes || echo no)" "case 20 setup: the first run signs alpha"
receipt_before="$(grep '^digest-signed ' "$COPY16/list.txt")"
printf 'appr skills/fixture/MANIFEST.yaml beta:18\n' >> "$COPY16/list.txt"
snap16="$(cat "$M16")"
out20b="$(cd "$COPY16" && bash "$UTIL" --reviewer "$REVIEWER" list.txt 2026-09-06 2>&1)"; rc20b=$?
check "$([ "$rc20b" -ne 0 ] && echo yes || echo no)" \
      "a list amended after signing is refused (D18-2, R17-P2-6)"
check "$(printf '%s' "$out20b" | grep -q 'amended after' && echo yes || echo no)" \
      "the amendment refusal says the list was amended after signing"
check "$([ "$(sed -n 18p "$M16")" = '    status: draft' ] && echo yes || echo no)" \
      "beta is NOT approved by the amended list"
check "$([ "$(cat "$M16")" = "$snap16" ] && echo yes || echo no)" \
      "the amendment refusal rewrites nothing"
check "$([ "$(grep '^digest-signed ' "$COPY16/list.txt")" = "$receipt_before" ] && echo yes || echo no)" \
      "the amendment refusal leaves the receipt untouched"

echo "-----------------------------------------------"
echo "sme_resign utility: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
