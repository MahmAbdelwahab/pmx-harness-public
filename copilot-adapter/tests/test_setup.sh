#!/usr/bin/env bash
# Test suite for copilot-adapter/setup.py.
#
# Cases (see the adapter README):
#   1. `setup.py check` passes on this repository (live files == rendered profile).
#   2. examples/minimal renders into a temp dir: five files, JSON parses, no
#      literal URL in mcp.json, scoped file carries applyTo frontmatter.
#   3. A profile with a literal `url` in [[mcp]] is refused with a message that
#      names the field.
#   4. `install` without --force onto a differing file exits non-zero and
#      leaves the file unmodified.
#   5. `export --no-profile` produces a tree without profile/; `init` there
#      creates one from examples/minimal.
#   6. `.github/hooks/<file>.json` parses, names both hook scripts, never
#      relies on $CLAUDE_PROJECT_DIR; session_mandate.py emits SessionStart JSON.
#   7. `.vscode/settings.json` carries the terminal autoApprove barrier and
#      the agent-discovery locations (and no APPLICATION-scoped edits key).
#   8. `.github/agents/*.agent.md` render from `.claude/agents` with the model
#      and tool mapping tables; an unmapped tool is refused.
#   9. `.github/skills/<name>/SKILL.md` shims exist for every competency and
#      drift between `skills/` and the profile is refused in both directions.
#  10. The generated instructions carry none of the v1 stale claims.
#  11. `export` carries the harness kit: profile + assets/ + ASSETS.json, the
#      payload present, the exclusions absent, re-export refused.
#  12. `install` in a bundle seeds the assets into the target tree, then renders;
#      `check` and `tools/tests/test_protect_paths.sh` pass there; re-install
#      is a no-op.
#  13. Seeding is a one-time bootstrap: the first install records
#      copilot-adapter/.seed-state.json; later installs skip seeding entirely;
#      --reseed re-enters the classification and refuses a differing source,
#      naming --overwrite-sources; --force never touches a seed asset;
#      --reseed --dry-run writes nothing; --overwrite-sources restores.
#  14. A literal MCP URL in a payload file refuses the export.
#  15. `export --no-profile` is the generic generator: no profile, no assets.
#  16. The home repository never carries copilot-adapter/assets (an exported
#      bundle carries them by design, and says so).
#  17. `install` probes the environment and drafts harness.yaml from a stub PATH
#      (Rscript, nmfe75 under <nm>/run/, execute, quarto), unconfirmed.
#  18. A second `install` never touches an existing harness.yaml.
#  19. `--refresh-env` rewrites the environment block while unconfirmed and
#      refuses once `environment_confirmed: true`.
#  20. `env` reports drift (exit 1) after a probed version string changes, and
#      says so plainly when harness.yaml is absent.
#  21. With an empty PATH every environment key is commented out and
#      `nonmem_provision_suggested` is commented too.
#  22. The draft pre-fills the OPERATIVE keys the harness reads: r_provision,
#      nonmem_home from the probe, nonmem_env_file and nonmem_license
#      commented out, all inside the operative marker pair.
#  23. With an empty PATH nonmem_home is commented out with its not-found note.
#  24. nonmem_license is pre-filled only when NONMEM_LICENSE is exported.
#  25. `--refresh-env` rewrites the operative section while unconfirmed and
#      refuses once `environment_confirmed: true`.
#  26. `env` reports drift on the operative nonmem_home key.
#  27. An existing marker-less harness.yaml keeps its hand-written keys
#      byte-for-byte; only the environment block is appended.
#  28. On a containerized machine (docker on PATH, image answered, no nmfe)
#      the draft pre-fills the operative `nonmem_image` key with provenance.
#  29. With docker present but the image not built, `nonmem_image` is
#      commented out with the not-found note.
#  30. `env` reports drift on the operative nonmem_image key.
#  31. A seed refusal is a preflight refusal: on a virgin bundle whose target
#      already differs, nothing at all is written.
#  32. Every ASSETS.json path is validated before any write: `..`, an absolute
#      prefix and a duplicate each refuse the whole install.
#  33. `export` refuses a DEST that resolves to, inside, or above the source
#      adapter or the source repository root, before any rmtree.
#  34. The exported `.github/workflows/ci.yaml` is the target-independent KIT
#      workflow, and every `run:` command in it exits 0 in an installed kit.
#  35. `install` writes copilot-adapter/.generated-inventory.json; its hashes
#      match the generated files and `check` stays clean.
#  36. An orphaned generated file fails `check`; `install` removes it when
#      unchanged and refuses it when hand-edited.
#  37. The inventory is validated before any read or delete: malformed, bad
#      sha grammar, `..`, absolute and out-of-root keys refuse; a missing
#      inventory on a managed target fails `check`; a fresh target installs.
#  38. A refused install leaves zero new files (listing diff on a virgin
#      bundle whose target owns a differing generated file).
#  39. An unmatched required [export].payload pattern fails export; an
#      `optional = true` one warns.
#  40. Generated reviewer/orchestrator wording: procedural isolation on
#      Copilot, bounded invocation claim, runtime-reported model recorded.
#  41. Profile do-not-edit entries and hook HARNESS_CORE remain identical.
#  42. Export refuses file and directory symlinks before reading payload bytes.
#  43. Generated destinations refuse symlink ancestors and traversal slugs
#      before any inside/outside write.
#  44. Confirmation parsing matches ordinary YAML booleans with comments and
#      refuses duplicate, malformed, quoted, or unsupported true forms before
#      any generated or harness.yaml write.
#
# Every fixture that exercises export/install/rmtree runs against a DISPOSABLE
# adapter under $TMP; safe_dest() aborts the suite if one ever resolves inside
# the repository.
#
# Runs with bash + python3 only. Exit 0 when every case passes.
# COPILOT_ADAPTER_TEST_DEPTH is set by case 34 when it runs this suite inside an
# exported kit; case 34 then skips its own nested run, so recursion is one deep.
set -u

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ADAPTER="$(dirname "$HERE")"
REPO="$(dirname "$ADAPTER")"
SETUP="$ADAPTER/setup.py"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/copilot-adapter-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

fail=0
pass() { echo "PASS  $1"; }
flunk() { echo "FAIL  $1"; fail=1; }

# --- fixture safety barrier --------------------------------------------------
# On 2026-09-03 a fixture passed the live repository as an export destination and
# `export --force` deleted copilot-adapter/ outright (the very defect case 33
# now covers). No fixture may name $REPO, $ADAPTER or anything inside them as a
# destination or a --target: every case that exercises export/install/rmtree
# runs against a DISPOSABLE copy under $TMP. safe_dest aborts the whole suite
# rather than let such a path through.
safe_dest() {
  local p; p="$(cd "$(dirname "$1")" 2>/dev/null && pwd)/$(basename "$1")"
  case "$p" in
    "$REPO"|"$REPO"/*)
      echo "ABORT: fixture destination '$1' resolves inside the repository ($p)." >&2
      echo "       Fixtures must use a disposable copy under \$TMP." >&2
      exit 99 ;;
  esac
  printf '%s\n' "$p"
}

# A throwaway adapter, so overlap/rmtree fixtures can never reach the live one.
disposable_adapter() {  # $1 = destination root to create
  mkdir -p "$1"
  cp -a "$ADAPTER" "$1/copilot-adapter"
  rm -rf "$1/copilot-adapter/assets"
  printf '%s\n' "$1/copilot-adapter/setup.py"
}

# --- 1. check passes on the repository --------------------------------------
if python3 "$SETUP" check --target "$REPO" --profile "$ADAPTER/profile" >"$TMP/check.log" 2>&1; then
  pass "1 check: live files match rendered profile"
else
  flunk "1 check: live files drift from profile"; cat "$TMP/check.log"
fi

# --- 2. examples/minimal renders and validates -------------------------------
MIN="$TMP/minimal"; mkdir -p "$MIN"
if python3 "$SETUP" install --target "$MIN" --profile "$ADAPTER/examples/minimal" >"$TMP/min.log" 2>&1; then
  ok=1
  for f in .github/copilot-instructions.md .vscode/mcp.json .vscode/settings.json .editorconfig; do
    [ -f "$MIN/$f" ] || { echo "  missing $f"; ok=0; }
  done
  scoped="$(ls "$MIN"/.github/instructions/*.instructions.md 2>/dev/null | head -n1)"
  [ -n "$scoped" ] || { echo "  missing scoped instruction file"; ok=0; }
  python3 -c 'import json,sys; json.load(open(sys.argv[1]))' "$MIN/.vscode/mcp.json" || { echo "  mcp.json does not parse"; ok=0; }
  grep -v '^[[:space:]]*//' "$MIN/.vscode/settings.json" | python3 -c 'import json,sys; json.load(sys.stdin)' || { echo "  settings.json does not parse"; ok=0; }
  if grep -qi 'http' "$MIN/.vscode/mcp.json" && grep -qiE 'https?://' "$MIN/.vscode/mcp.json"; then echo "  literal URL in mcp.json"; ok=0; fi
  grep -q '\${input:' "$MIN/.vscode/mcp.json" || { echo "  mcp.json lacks \${input:...}"; ok=0; }
  [ -n "$scoped" ] && { grep -q '^applyTo:' "$scoped" || { echo "  scoped file lacks applyTo frontmatter"; ok=0; }; }
  [ $ok = 1 ] && pass "2 minimal: renders, parses, no literal URL, applyTo present" || flunk "2 minimal: validation"
else
  flunk "2 minimal: install failed"; cat "$TMP/min.log"
fi

# --- 3. literal url in [[mcp]] is refused ------------------------------------
BAD="$TMP/badprofile"; cp -r "$ADAPTER/examples/minimal" "$BAD"
printf '\n[[mcp]]\nid = "leaky"\ntype = "http"\nurl = "https://example.invalid/mcp"\ninput_description = "x"\n' >> "$BAD/adapter.toml"
mkdir -p "$TMP/badout"
if python3 "$SETUP" install --target "$TMP/badout" --profile "$BAD" >"$TMP/bad.log" 2>&1; then
  flunk "3 refuse literal url: install succeeded"
elif grep -q 'url' "$TMP/bad.log" && grep -q 'leaky' "$TMP/bad.log"; then
  pass "3 refuse literal url: non-zero exit, message names the field and entry"
else
  flunk "3 refuse literal url: message does not name the field"; cat "$TMP/bad.log"
fi
[ -e "$TMP/badout/.vscode/mcp.json" ] && flunk "3 refuse literal url: mcp.json was written anyway"

# --- 4. install without --force refuses to overwrite a differing file --------
echo "# local edit" >> "$MIN/.editorconfig"
before="$(sha256sum "$MIN/.editorconfig")"
if python3 "$SETUP" install --target "$MIN" --profile "$ADAPTER/examples/minimal" >"$TMP/nf.log" 2>&1; then
  flunk "4 no --force: install succeeded over a differing file"
else
  after="$(sha256sum "$MIN/.editorconfig")"
  if [ "$before" = "$after" ]; then
    pass "4 no --force: non-zero exit, differing file left untouched"
  else
    flunk "4 no --force: file was modified"
  fi
fi
python3 "$SETUP" install --target "$MIN" --profile "$ADAPTER/examples/minimal" --force >/dev/null 2>&1 \
  && python3 "$SETUP" check --target "$MIN" --profile "$ADAPTER/examples/minimal" >/dev/null 2>&1 \
  && pass "4b --force then check: clean" || flunk "4b --force then check"

# --- 5. export --no-profile leaves no profile/, then init creates one ---------
EXP="$TMP/exported"
if python3 "$SETUP" export "$EXP" --no-profile >"$TMP/exp.log" 2>&1; then
  ok=1
  [ -d "$EXP/copilot-adapter/profile" ] && { echo "  export carried profile/"; ok=0; }
  for f in setup.py README.md templates examples/minimal tests/test_setup.sh; do
    [ -e "$EXP/copilot-adapter/$f" ] || { echo "  export missing $f"; ok=0; }
  done
  if python3 "$EXP/copilot-adapter/setup.py" init >"$TMP/init.log" 2>&1; then
    [ -f "$EXP/copilot-adapter/profile/adapter.toml" ] || { echo "  init created no adapter.toml"; ok=0; }
    python3 "$EXP/copilot-adapter/setup.py" init >/dev/null 2>&1 && { echo "  second init did not refuse"; ok=0; }
    python3 "$EXP/copilot-adapter/setup.py" install >/dev/null 2>&1 || { echo "  install in exported tree failed"; ok=0; }
  else
    echo "  init failed"; cat "$TMP/init.log"; ok=0
  fi
  [ $ok = 1 ] && pass "5 export/init: tree without profile/, init creates one, install works" || flunk "5 export/init"
else
  flunk "5 export failed"; cat "$TMP/exp.log"
fi

# --- 6. hooks: .github/hooks/<file>.json parses, repo-relative commands ------
HOOKS="$REPO/.github/hooks/pmx-harness.json"
if [ -f "$HOOKS" ] && python3 -c 'import json,sys; d=json.load(open(sys.argv[1])); assert "hooks" in d' "$HOOKS" 2>/dev/null; then
  ok=1
  grep -q 'CLAUDE_PROJECT_DIR' "$HOOKS" && { echo "  hooks JSON relies on \$CLAUDE_PROJECT_DIR"; ok=0; }
  grep -q 'protect_paths.py' "$HOOKS" || { echo "  hooks JSON lacks the PreToolUse guard"; ok=0; }
  grep -q 'session_mandate.py' "$HOOKS" || { echo "  hooks JSON lacks the SessionStart mandate"; ok=0; }
  [ $ok = 1 ] && pass "6 hooks: JSON parses, guard + mandate present, no CLAUDE_PROJECT_DIR" || flunk "6 hooks: content"
else
  flunk "6 hooks: $HOOKS missing or not JSON"
fi

# --- 6b. session_mandate.py emits SessionStart JSON with additionalContext ----
if out="$(cd "$REPO" && python3 .claude/hooks/session_mandate.py 2>/dev/null)" && \
   printf '%s' "$out" | python3 -c 'import json,sys; d=json.load(sys.stdin)["hookSpecificOutput"]; assert d["hookEventName"]=="SessionStart"; assert "Role: orchestrator" in d["additionalContext"]; assert "docs/PORTABILITY.md" in d["additionalContext"]' 2>/dev/null; then
  pass "6b session_mandate: JSON with hook event, orchestrator role and portable dispatch pointer"
else
  flunk "6b session_mandate: missing or wrong JSON"
fi

# --- 7. settings.json carries the editor second barrier ----------------------
# The terminal deny rules are WINDOW-scoped and take effect in a trusted
# workspace. The edits auto-approval map is APPLICATION-scoped (a workspace
# value is ignored by VS Code), so it must NOT be generated here; it lives in
# the user's settings if configured. `.claude/agents` is
# switched off so the pmx agents are not listed twice.
SETTINGS="$REPO/.vscode/settings.json"
if grep -v '^[[:space:]]*//' "$SETTINGS" | python3 -c '
import json,sys
d=json.load(sys.stdin)
assert "chat.tools.edits.autoApprove" not in d, "APPLICATION-scoped key must not be generated"
t=d["chat.tools.terminal.autoApprove"]
assert any("gates" in k for k in t) and all(v is False or v.get("approve") is False for v in t.values())
assert d["chat.agentFilesLocations"][".claude/agents"] is False
assert d["chat.useClaudeHooks"] is False
assert "[markdown]" in d and d["[markdown]"]["editor.formatOnSave"] is False
' 2>/dev/null; then
  pass "7 settings: terminal deny entries + agent locations present, no APPLICATION-scoped key, markdown block intact"
else
  flunk "7 settings: expected terminal deny entries and chat.agentFilesLocations, and no chat.tools.edits.autoApprove"
fi

# --- 8. agents: .github/agents/*.agent.md rendered from .claude/agents --------
AG="$REPO/.github/agents"
if [ -f "$AG/pmx-reviewer.agent.md" ] && [ -f "$AG/pmx-modeler.agent.md" ] && [ -f "$AG/pmx-orchestrator.agent.md" ]; then
  ok=1
  grep -q "^model: \['Claude Opus 4.8'\]" "$AG/pmx-modeler.agent.md" || { echo "  modeler model array wrong"; ok=0; }
  grep -qx "tools: \['read', 'edit', 'search', 'execute'\]" "$AG/pmx-modeler.agent.md" || { echo "  modeler tools must match the public local tool list"; ok=0; }
  grep -q "^disable-model-invocation: true" "$AG/pmx-reviewer.agent.md" || { echo "  reviewer lacks disable-model-invocation"; ok=0; }
  grep -q "^tools: \['read', 'search', 'execute'\]" "$AG/pmx-reviewer.agent.md" || { echo "  reviewer tools wrong"; ok=0; }
  grep -q 'generated from `.claude/agents/pmx-reviewer.md`' "$AG/pmx-reviewer.agent.md" || { echo "  reviewer lacks generated-from line"; ok=0; }
  grep -q "You are the reviewer sub-agent of the pmx-harness." "$AG/pmx-reviewer.agent.md" || { echo "  reviewer body not verbatim"; ok=0; }
  grep -q "^agents: \[" "$AG/pmx-orchestrator.agent.md" && grep -c 'pmx-' "$AG/pmx-orchestrator.agent.md" >/dev/null || { echo "  orchestrator lacks agents list"; ok=0; }
  grep -q "^user-invocable: true" "$AG/pmx-orchestrator.agent.md" || { echo "  orchestrator not user-invocable"; ok=0; }
  grep -q "^## Dispatch map" "$AG/pmx-orchestrator.agent.md" || { echo "  orchestrator lacks dispatch map"; ok=0; }
  grep -q "^## The four duties" "$AG/pmx-orchestrator.agent.md" || { echo "  orchestrator lacks role body"; ok=0; }
  grep -q "pmx-reviewer" "$AG/pmx-orchestrator.agent.md" || { echo "  orchestrator does not name the reviewer"; ok=0; }
  [ $ok = 1 ] && pass "8 agents: model array, tool mapping, reviewer invocation flag, orchestrator composed" || flunk "8 agents: content"
else
  flunk "8 agents: generated .agent.md files missing"
fi

# --- 8b. agents: an unmapped Claude tool is an error, not a silent drop -------
AGP="$TMP/agentprof"; cp -r "$ADAPTER/profile" "$AGP"
AGT="$TMP/agenttarget"; mkdir -p "$AGT/.claude/agents" "$AGT/roles" "$AGT/docs" "$AGT/skills"
cp "$REPO/.claude/agents/pmx-reviewer.agent.md" "$AGT/.claude/agents/" 2>/dev/null
cp "$REPO/.claude/agents/pmx-reviewer.md" "$AGT/.claude/agents/pmx-reviewer.md"
cp "$REPO/roles/orchestrator.md" "$AGT/roles/"; cp "$REPO/docs/PORTABILITY.md" "$AGT/docs/"
for d in "$REPO"/skills/*/; do [ -f "$d/SKILL.md" ] && { mkdir -p "$AGT/skills/$(basename "$d")"; : > "$AGT/skills/$(basename "$d")/SKILL.md"; }; done
sed -i 's/^tools: Read, Grep, Glob, Bash$/tools: Read, Grep, Glob, Bash, WebFetch/' "$AGT/.claude/agents/pmx-reviewer.md"
if python3 "$SETUP" install --target "$AGT" --profile "$AGP" >"$TMP/unmapped.log" 2>&1; then
  flunk "8b unmapped tool: install succeeded"
elif grep -q 'WebFetch' "$TMP/unmapped.log" && grep -q 'pmx-reviewer' "$TMP/unmapped.log"; then
  pass "8b unmapped tool: refused, message names the tool and the agent"
else
  flunk "8b unmapped tool: wrong message"; cat "$TMP/unmapped.log"
fi

# --- 9. skills: shims exist for every skills/<name>/SKILL.md; drift is an error
SK="$REPO/.github/skills"
ok=1
for d in "$REPO"/skills/*/; do
  n="$(basename "$d")"; [ -f "$d/SKILL.md" ] || continue
  [ -f "$SK/$n/SKILL.md" ] || { echo "  missing shim for $n"; ok=0; continue; }
  grep -q "^name: $n$" "$SK/$n/SKILL.md" || { echo "  shim $n name mismatch"; ok=0; }
  grep -q "^description: " "$SK/$n/SKILL.md" || { echo "  shim $n lacks description"; ok=0; }
  grep -q "skills/$n/SKILL.md" "$SK/$n/SKILL.md" || { echo "  shim $n does not point at the canonical file"; ok=0; }
done
[ -e "$SK/_parked" ] && { echo "  _parked rendered"; ok=0; }
[ $ok = 1 ] && pass "9 skills: one shim per competency, name = folder, points at canonical SKILL.md" || flunk "9 skills: shims"

# --- 9b. skills: folder without profile entry, and entry without folder, refuse
SKT="$TMP/skilltarget"; mkdir -p "$SKT"; cp -r "$AGT/." "$SKT/"
sed -i 's/WebFetch//; s/, $//' "$SKT/.claude/agents/pmx-reviewer.md"
mkdir -p "$SKT/skills/brand-new-skill"; : > "$SKT/skills/brand-new-skill/SKILL.md"
if python3 "$SETUP" install --target "$SKT" --profile "$AGP" >"$TMP/skdrift.log" 2>&1; then
  flunk "9b skill drift (folder without entry): install succeeded"
elif grep -q 'brand-new-skill' "$TMP/skdrift.log"; then
  pass "9b skill drift (folder without entry): refused, names the folder"
else
  flunk "9b skill drift: wrong message"; cat "$TMP/skdrift.log"
fi
rm -rf "$SKT/skills/brand-new-skill"
printf '\n[[skills]]\nname = "ghost-skill"\ndescription = "no folder"\n' >> "$AGP/adapter.toml"
if python3 "$SETUP" install --target "$SKT" --profile "$AGP" >"$TMP/skghost.log" 2>&1; then
  flunk "9c skill drift (entry without folder): install succeeded"
elif grep -q 'ghost-skill' "$TMP/skghost.log"; then
  pass "9c skill drift (entry without folder): refused, names the entry"
else
  flunk "9c skill drift: wrong message"; cat "$TMP/skghost.log"
fi

# --- 10. generated files carry no stale capability claims ---------------------
if grep -qiE 'no editor-time write guard|no session-start hook|no isolated subagents|Copilot has no PreToolUse' "$REPO/.github/copilot-instructions.md"; then
  flunk "10 stale claims remain in copilot-instructions.md"
else
  pass "10 copilot-instructions.md carries no stale capability claims"
fi

# --- 11. export carries the harness kit (profile + assets) --------------------
KIT="$TMP/kit"
if python3 "$SETUP" export "$KIT" >"$TMP/kit.log" 2>&1; then
  ok=1
  A="$KIT/copilot-adapter/assets"
  [ -f "$KIT/copilot-adapter/profile/adapter.toml" ] || { echo "  export did not carry the profile"; ok=0; }
  [ -f "$A/ASSETS.json" ] || { echo "  no assets/ASSETS.json"; ok=0; }
  if [ -f "$A/ASSETS.json" ]; then
    python3 - "$A" "$KIT/copilot-adapter/profile/adapter.toml" <<'PY' || ok=0
import hashlib, json, stat, sys, tomllib
from pathlib import Path

assets, profile = map(Path, sys.argv[1:])
m = json.loads((assets / "ASSETS.json").read_text())
with profile.open("rb") as stream:
    export = tomllib.load(stream)["export"]
assert isinstance(m["assets"], list), "assets is not a list"
paths = {record["path"] for record in m["assets"]}
assert len(paths) == len(m["assets"]) and paths == set(export["payload"]), "stock seed path parity"
assert {name for name in paths if name.startswith("analyses/")} == {"analyses/synthetic_example.R"}
assert {"LICENSE", "LICENSES/llm-r-skills-MIT.txt"} <= paths
assert m["source_commit"], "no source_commit"
assert m["exported"], "no export date"
for record in m["assets"]:
    source = assets / record["path"]
    assert source.resolve(strict=True) == source and source.is_file(), record["path"]
    raw = source.read_bytes()
    assert len(raw) == record["bytes"] and hashlib.sha256(raw).hexdigest() == record["sha256"], record["path"]
    expected_mode = 0o755 if record["path"] in export["executable"] else 0o644
    assert stat.S_IMODE(source.stat().st_mode) == expected_mode, record["path"]
PY
  fi
  for f in .claude/agents/pmx-modeler.md roles/orchestrator.md skills/diagnostics/MANIFEST.yaml tools/gate.R .claude/settings.json .claude/hooks/protect_paths.py CLAUDE.md pipeline/compose.R analyses/synthetic_example.R LICENSE LICENSES/llm-r-skills-MIT.txt; do
    [ -f "$A/$f" ] || { echo "  asset missing: $f"; ok=0; }
  done
  for f in gates pilot harness.yaml .harness-unlock data references software .vscode/mcp.json; do
    [ -e "$A/$f" ] && { echo "  excluded path present in assets: $f"; ok=0; }
  done
  python3 "$SETUP" export "$KIT" >/dev/null 2>&1 && { echo "  second export did not refuse"; ok=0; }
  [ $ok = 1 ] && pass "11 export kit: profile + assets manifest, payload present, exclusions honoured, re-export refused" \
    || flunk "11 export kit"
else
  flunk "11 export kit: export failed"; cat "$TMP/kit.log"
fi

# --- 12. install seeds the assets, then renders; check + guard tests pass -----
if [ -d "$KIT/copilot-adapter/assets" ]; then
  ok=1
  ( cd "$KIT" && git init -q . ) || ok=0
  if ( cd "$KIT" && python3 copilot-adapter/setup.py install ) >"$TMP/seed.log" 2>&1; then
    for f in .claude/agents/pmx-modeler.md roles/orchestrator.md skills/diagnostics/MANIFEST.yaml tools/gate.R .claude/settings.json .claude/hooks/protect_paths.py; do
      [ -f "$KIT/$f" ] || { echo "  not seeded: $f"; ok=0; }
    done
    ( cd "$KIT" && python3 copilot-adapter/setup.py check ) >"$TMP/kitcheck.log" 2>&1 \
      || { echo "  check failed in the exported tree"; tail -5 "$TMP/kitcheck.log"; ok=0; }
    ( cd "$KIT" && bash tools/tests/test_protect_paths.sh ) >"$TMP/kitguard.log" 2>&1 \
      || { echo "  protect_paths tests failed in the exported tree"; tail -5 "$TMP/kitguard.log"; ok=0; }
      # The guard suite's own summary and exit status are the oracle. A literal
    # count would break every time a fixture is added (it did: 35 -> 41).
    grep -qE '[0-9]+ passed, 0 failed' "$TMP/kitguard.log" \
      || { echo "  guard suite summary line absent or reported failures"; ok=0; }
    ( cd "$KIT" && python3 copilot-adapter/setup.py install ) >"$TMP/seed2.log" 2>&1 \
      || { echo "  second install failed"; ok=0; }
    grep -qE '^(create|overwrite|seed) ' "$TMP/seed2.log" && { echo "  second install was not a no-op"; ok=0; }
    [ $ok = 1 ] && pass "12 install seeds: sources placed, check clean, guard suite green, re-install no-op" \
      || flunk "12 install seeds"
  else
    flunk "12 install seeds: install failed"; cat "$TMP/seed.log"
  fi
fi

# --- 13. seeding is a one-time bootstrap recorded in .seed-state.json --------
if [ -f "$KIT/tools/gate.R" ]; then
  ok=1
  SEEDSTATE="$KIT/copilot-adapter/.seed-state.json"
  [ -f "$SEEDSTATE" ] || { echo "  no .seed-state.json after the first install"; ok=0; }
  if [ -f "$SEEDSTATE" ]; then
    python3 - "$SEEDSTATE" "$KIT/copilot-adapter/assets/ASSETS.json" <<'PY' || ok=0
import json, sys
st = json.load(open(sys.argv[1]))
mf = json.load(open(sys.argv[2]))
assert st["schema"] == 1, st
assert st["bundle_id"] == mf["source_commit"], (st["bundle_id"], mf["source_commit"])
assert st["seeded_at"].endswith("Z"), st["seeded_at"]
assert len(st["assets"]) == len(mf["assets"]), (len(st["assets"]), len(mf["assets"]))
for a in mf["assets"]:
    assert st["assets"][a["path"]] == a["sha256"], a["path"]
PY
  fi
  echo "# local edit" >> "$KIT/tools/gate.R"
  before="$(sha256sum "$KIT/tools/gate.R" | cut -d' ' -f1)"
  ( cd "$KIT" && python3 copilot-adapter/setup.py install ) >"$TMP/seedmod.log" 2>&1 \
    || { echo "  install refused although seeding was already recorded"; ok=0; }
  [ "$(sha256sum "$KIT/tools/gate.R" | cut -d' ' -f1)" = "$before" ] \
    || { echo "  a plain install rewrote a seeded source"; ok=0; }
  grep -q 'seeding skipped' "$TMP/seedmod.log" \
    || { echo "  install did not report that seeding was skipped"; ok=0; }
  [ $ok = 1 ] && pass "13 seed state: recorded on the first install, later installs skip seeding entirely" \
    || { flunk "13 seed state"; cat "$TMP/seedmod.log"; }
fi

# --- 13b. --reseed re-enters the classification and refuses a differing source
if [ -f "$KIT/tools/gate.R" ]; then
  before="$(sha256sum "$KIT/tools/gate.R" | cut -d' ' -f1)"
  if ( cd "$KIT" && python3 copilot-adapter/setup.py install --reseed ) >"$TMP/reseed.log" 2>&1; then
    flunk "13b --reseed: succeeded over a modified seeded source"
  elif grep -q 'tools/gate.R' "$TMP/reseed.log" \
       && grep -q -- '--overwrite-sources' "$TMP/reseed.log" \
       && [ "$(sha256sum "$KIT/tools/gate.R" | cut -d' ' -f1)" = "$before" ]; then
    pass "13b --reseed: refused, names the file and --overwrite-sources, leaves it untouched"
  else
    flunk "13b --reseed: wrong message or the file was modified"; cat "$TMP/reseed.log"
  fi
fi

# --- 13c. --force applies to generated outputs only, never to seed assets ----
if [ -f "$KIT/tools/gate.R" ]; then
  before="$(sha256sum "$KIT/tools/gate.R" | cut -d' ' -f1)"
  ( cd "$KIT" && python3 copilot-adapter/setup.py install --reseed --force ) >"$TMP/reseedf.log" 2>&1
  if [ "$(sha256sum "$KIT/tools/gate.R" | cut -d' ' -f1)" = "$before" ] \
     && grep -q -- '--overwrite-sources' "$TMP/reseedf.log"; then
    pass "13c --force: leaves a differing seed asset alone; the refusal names --overwrite-sources"
  else
    flunk "13c --force overwrote a seed asset"; cat "$TMP/reseedf.log"
  fi
fi

# --- 13d. --reseed --dry-run prints the plan and writes nothing --------------
if [ -f "$KIT/tools/gate.R" ]; then
  before="$(sha256sum "$KIT/tools/gate.R" | cut -d' ' -f1)"
  ( cd "$KIT" && python3 copilot-adapter/setup.py install --reseed --dry-run ) >"$TMP/seeddry.log" 2>&1
  if grep -q 'dry-run' "$TMP/seeddry.log" \
     && [ "$(sha256sum "$KIT/tools/gate.R" | cut -d' ' -f1)" = "$before" ]; then
    pass "13d --reseed --dry-run: prints the plan, writes nothing"
  else
    flunk "13d --reseed --dry-run"; cat "$TMP/seeddry.log"
  fi
fi

# --- 13e. --overwrite-sources is the explicit, separate flag -----------------
if [ -f "$KIT/tools/gate.R" ]; then
  before="$(sha256sum "$KIT/tools/gate.R" | cut -d' ' -f1)"
  if ( cd "$KIT" && python3 copilot-adapter/setup.py install --reseed --overwrite-sources ) >"$TMP/seedovr.log" 2>&1 \
     && [ "$(sha256sum "$KIT/tools/gate.R" | cut -d' ' -f1)" != "$before" ]; then
    pass "13e --overwrite-sources: restores the bundle's copy of a modified seed asset"
  else
    flunk "13e --overwrite-sources"; cat "$TMP/seedovr.log"
  fi
fi

# --- 14. a literal MCP URL in a payload file refuses the export ---------------
SRC="$TMP/urlsrc"; mkdir -p "$SRC/.vscode"
printf '{\n  "servers": {"x": {"type": "http", "url": "https://example.invalid/mcp"}}\n}\n' > "$SRC/.vscode/mcp.json"
URLPROF="$TMP/urlprofile"; cp -r "$ADAPTER/examples/minimal" "$URLPROF"
printf '\n[export]\npayload = [".vscode/**"]\nexclude = []\n' >> "$URLPROF/adapter.toml"
if python3 "$SETUP" export "$TMP/urlout" --target "$SRC" --profile "$URLPROF" >"$TMP/url.log" 2>&1; then
  flunk "14 literal URL: export succeeded"
elif grep -q '.vscode/mcp.json' "$TMP/url.log"; then
  pass "14 literal URL: export refused, message names the offending file"
else
  flunk "14 literal URL: wrong message"; cat "$TMP/url.log"
fi
[ -e "$TMP/urlout/copilot-adapter/assets" ] && flunk "14 literal URL: assets were written anyway"

# --- 15. export --no-profile is the generic generator, no payload -------------
NOP="$TMP/generic"
if python3 "$SETUP" export "$NOP" --no-profile >"$TMP/nop.log" 2>&1; then
  ok=1
  [ -e "$NOP/copilot-adapter/profile" ] && { echo "  --no-profile carried a profile"; ok=0; }
  [ -e "$NOP/copilot-adapter/assets" ] && { echo "  --no-profile carried assets"; ok=0; }
  [ -f "$NOP/copilot-adapter/setup.py" ] || { echo "  generator missing"; ok=0; }
  [ $ok = 1 ] && pass "15 export --no-profile: generator only, no profile, no assets" || flunk "15 export --no-profile"
else
  flunk "15 export --no-profile failed"; cat "$TMP/nop.log"
fi

# --- 16. the home repository never carries an assets/ directory --------------
# An EXPORTED bundle carries assets/ by design; the invariant under test is that
# the HOME repository does not, because there the live files are their own single
# source. ASSETS.json is what distinguishes the two.
if [ -f "$ADAPTER/assets/ASSETS.json" ]; then
  pass "16 bundle: assets/ present by design (this adapter is an exported kit)"
elif [ -e "$ADAPTER/assets" ]; then
  flunk "16 home repo: copilot-adapter/assets exists (the live files are the single source)"
else
  pass "16 home repo: no copilot-adapter/assets"
fi

# --- environment probe stubs -------------------------------------------------
# A fake PATH of tiny scripts that answer only the version queries the probe
# makes. Nothing here executes a real engine.
STUBS="$TMP/stubs"; NM="$TMP/nm75"; mkdir -p "$STUBS" "$NM/run"
mk_stub() { printf '#!/bin/sh\n%s\n' "$2" > "$1"; chmod +x "$1"; }
mk_stub "$STUBS/Rscript" 'echo "Rscript (R) version 4.4.1 (2024-06-14)"'
mk_stub "$STUBS/R" 'case "$1" in RHOME) echo /opt/R/4.4.1;; *) echo "R version 4.4.1 (2024-06-14)";; esac'
mk_stub "$NM/run/nmfe75" 'echo "nmfe75 stub"'
mk_stub "$STUBS/execute" 'echo "PsN version: 5.7.1"'
mk_stub "$STUBS/psn" 'case "$1" in -nm_versions) printf "nm751 is default\ndocker75\n";; *) echo "PsN version: 5.7.1";; esac'
mk_stub "$STUBS/quarto" 'echo "1.5.57"'
PY3="$(command -v python3)"
FAKEPATH="$STUBS:$NM/run"

# --- 17. install drafts harness.yaml from the probe --------------------------
ENVT="$TMP/envtarget"; mkdir -p "$ENVT"
cp "$REPO/harness.yaml.example" "$ENVT/harness.yaml.example"
if env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" install \
     --target "$ENVT" --profile "$ADAPTER/examples/minimal" >"$TMP/env1.log" 2>&1; then
  ok=1
  H="$ENVT/harness.yaml"
  [ -f "$H" ] || { echo "  harness.yaml not drafted"; ok=0; }
  if [ -f "$H" ]; then
    grep -q '^environment_confirmed: false' "$H" || { echo "  no environment_confirmed: false"; ok=0; }
    grep -q "r_rscript: $STUBS/Rscript" "$H" || { echo "  r_rscript not detected"; ok=0; }
    grep -q 'r_version: .*4\.4\.1' "$H" || { echo "  r_version not detected"; ok=0; }
    grep -q "nonmem_binary: $NM/run/nmfe75" "$H" || { echo "  nonmem_binary not detected"; ok=0; }
    grep -q "nonmem_home: $NM   #" "$H" || { echo "  nonmem_home not derived"; ok=0; }
    grep -q "psn_execute: $STUBS/execute" "$H" || { echo "  psn_execute not detected"; ok=0; }
    grep -q 'psn_version: .*5\.7\.1' "$H" || { echo "  psn_version not detected"; ok=0; }
    grep -q 'quarto_version: .*1\.5\.57' "$H" || { echo "  quarto_version not detected"; ok=0; }
    grep -q '^nonmem_provision_suggested: native' "$H" || { echo "  provision suggestion not native"; ok=0; }
    grep -q '# detected .* via ' "$H" || { echo "  no detected provenance comments"; ok=0; }
    grep -q '^r_provision:' "$H" || { echo "  example structure not carried over"; ok=0; }
  fi
  grep -qi 'review' "$TMP/env1.log" || { echo "  install did not print the review instruction"; ok=0; }
  grep -q 'Rscript' "$TMP/env1.log" || { echo "  install did not print the probe table"; ok=0; }
  [ $ok = 1 ] && pass "17 env draft: harness.yaml drafted from the probe, unconfirmed, provenance comments" \
    || flunk "17 env draft"
else
  flunk "17 env draft: install failed"; cat "$TMP/env1.log"
fi

# --- 18. a second install never touches harness.yaml -------------------------
if [ -f "$ENVT/harness.yaml" ]; then
  before="$(sha256sum "$ENVT/harness.yaml" | cut -d' ' -f1)"
  env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" install \
    --target "$ENVT" --profile "$ADAPTER/examples/minimal" --force >"$TMP/env2.log" 2>&1
  after="$(sha256sum "$ENVT/harness.yaml" | cut -d' ' -f1)"
  [ "$before" = "$after" ] && pass "18 env draft: second install leaves harness.yaml untouched" \
    || flunk "18 env draft: second install rewrote harness.yaml"
fi

# --- 20. env reports drift after a probed version changes --------------------
if env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" env --target "$ENVT" >"$TMP/env3.log" 2>&1; then
  mk_stub "$STUBS/Rscript" 'echo "Rscript (R) version 4.5.0 (2025-04-11)"'
  if env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" env --target "$ENVT" >"$TMP/env4.log" 2>&1; then
    flunk "20 env drift: exit 0 although the R version changed"
  elif grep -q 'r_version' "$TMP/env4.log" && grep -qi 'drift' "$TMP/env4.log"; then
    pass "20 env drift: exit 1, names the drifted key"
  else
    flunk "20 env drift: wrong message"; cat "$TMP/env4.log"
  fi
else
  flunk "20 env: no-drift run did not exit 0"; cat "$TMP/env3.log"
fi
NOENV="$TMP/noharness"; mkdir -p "$NOENV"
if env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" env --target "$NOENV" >"$TMP/env5.log" 2>&1; then
  grep -qi 'harness.yaml' "$TMP/env5.log" && pass "20b env: absent harness.yaml reported plainly, exit 0" \
    || { flunk "20b env: message does not name harness.yaml"; cat "$TMP/env5.log"; }
else
  flunk "20b env: absent harness.yaml should exit 0"; cat "$TMP/env5.log"
fi

# --- 19. --refresh-env rewrites while unconfirmed, refuses once confirmed ----
if env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" install --refresh-env \
     --target "$ENVT" --profile "$ADAPTER/examples/minimal" --force >"$TMP/env6.log" 2>&1; then
  if grep -q '4\.5\.0' "$ENVT/harness.yaml" \
     && grep -q '^environment_confirmed: false' "$ENVT/harness.yaml" \
     && [ "$(grep -c '^environment:' "$ENVT/harness.yaml")" = 1 ] \
     && [ "$(grep -c '^# >>> operative settings' "$ENVT/harness.yaml")" = 1 ]; then
    pass "19 --refresh-env: environment block rewritten while unconfirmed"
  else
    flunk "19 --refresh-env: block not rewritten"; cat "$TMP/env6.log"
  fi
else
  flunk "19 --refresh-env failed"; cat "$TMP/env6.log"
fi
sed -i 's/^environment_confirmed: false/environment_confirmed: true/' "$ENVT/harness.yaml"
before="$(sha256sum "$ENVT/harness.yaml" | cut -d' ' -f1)"
if env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" install --refresh-env \
     --target "$ENVT" --profile "$ADAPTER/examples/minimal" --force >"$TMP/env7.log" 2>&1; then
  flunk "19b --refresh-env: succeeded although environment_confirmed: true"
else
  after="$(sha256sum "$ENVT/harness.yaml" | cut -d' ' -f1)"
  if [ "$before" = "$after" ] && grep -qi 'confirmed' "$TMP/env7.log"; then
    pass "19b --refresh-env: refused once confirmed, file untouched"
  else
    flunk "19b --refresh-env: file changed or message unclear"; cat "$TMP/env7.log"
  fi
fi

# --- 21. empty PATH: every key commented, provision suggestion commented -----
EMPT="$TMP/emptypath"; mkdir -p "$EMPT"
if env -i PATH="$TMP/nowhere" HOME="$TMP/fakehome" "$PY3" "$SETUP" install \
     --target "$EMPT" --profile "$ADAPTER/examples/minimal" >"$TMP/env8.log" 2>&1; then
  H="$EMPT/harness.yaml"; ok=1
  [ -f "$H" ] || { echo "  no draft written"; ok=0; }
  if [ -f "$H" ]; then
    grep -qE '^  [a-z_]+:' "$H" && { echo "  an environment key was written uncommented"; ok=0; }
    grep -q '^  # r_rscript:' "$H" || { echo "  r_rscript not commented"; ok=0; }
    grep -q '# not found' "$H" || { echo "  no 'not found' provenance"; ok=0; }
    grep -q '^nonmem_provision_suggested:' "$H" && { echo "  provision suggestion not commented"; ok=0; }
    grep -q '^# nonmem_provision_suggested:' "$H" || { echo "  provision suggestion line absent"; ok=0; }
  fi
  [ $ok = 1 ] && pass "21 empty PATH: every key and the provision suggestion commented out" || flunk "21 empty PATH"
else
  flunk "21 empty PATH: install failed"; cat "$TMP/env8.log"
fi

# --- 22. the draft pre-fills the operative keys the harness reads ------------
OPT="$TMP/optarget"; mkdir -p "$OPT"
cp "$REPO/harness.yaml.example" "$OPT/harness.yaml.example"
if env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" install \
     --target "$OPT" --profile "$ADAPTER/examples/minimal" >"$TMP/op1.log" 2>&1; then
  H="$OPT/harness.yaml"; ok=1
  grep -q '^# >>> operative settings' "$H" || { echo "  no operative begin marker"; ok=0; }
  grep -q '^# <<< end operative settings' "$H" || { echo "  no operative end marker"; ok=0; }
  grep -qE "^nonmem_home: $NM   # detected " "$H" || { echo "  nonmem_home not pre-filled with provenance"; ok=0; }
  grep -q '^r_provision: managed' "$H" || { echo "  r_provision not pre-filled"; ok=0; }
  [ "$(grep -c '^r_provision:' "$H")" = 1 ] || { echo "  r_provision written more than once"; ok=0; }
  grep -q '^# nonmem_env_file:' "$H" || { echo "  nonmem_env_file not commented out"; ok=0; }
  grep -q '^# nonmem_license:' "$H" || { echo "  nonmem_license not commented out"; ok=0; }
  grep -qE '^nonmem_license:' "$H" && { echo "  nonmem_license written without NONMEM_LICENSE"; ok=0; }
  grep -q '^environment_confirmed: false' "$H" || { echo "  draft is not unconfirmed"; ok=0; }
  [ $ok = 1 ] && pass "22 operative: keys pre-filled from the probe inside their marker pair" \
    || flunk "22 operative pre-fill"
else
  flunk "22 operative: install failed"; cat "$TMP/op1.log"
fi

# --- 23. empty PATH comments nonmem_home out with the not-found note ---------
if [ -f "$EMPT/harness.yaml" ]; then
  ok=1
  grep -q '^# nonmem_home:.*not found' "$EMPT/harness.yaml" || { echo "  nonmem_home not commented with a reason"; ok=0; }
  grep -qE '^nonmem_home:' "$EMPT/harness.yaml" && { echo "  nonmem_home written although no nmfe was found"; ok=0; }
  [ $ok = 1 ] && pass "23 operative: nonmem_home commented out when no nmfe is on PATH" || flunk "23 operative not-found"
fi

# --- 24. nonmem_license pre-filled only when NONMEM_LICENSE is exported ------
LICF="$TMP/dummy.lic"; printf 'dummy license content, not a real license\n' > "$LICF"
LICT="$TMP/lictarget"; mkdir -p "$LICT"
cp "$REPO/harness.yaml.example" "$LICT/harness.yaml.example"
if env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" NONMEM_LICENSE="$LICF" "$PY3" "$SETUP" install \
     --target "$LICT" --profile "$ADAPTER/examples/minimal" >"$TMP/op2.log" 2>&1; then
  if grep -qE "^nonmem_license: $LICF   # detected " "$LICT/harness.yaml"; then
    pass "24 operative: nonmem_license pre-filled from NONMEM_LICENSE with provenance"
  else
    flunk "24 operative: nonmem_license not pre-filled"; grep -n 'nonmem_license' "$LICT/harness.yaml"
  fi
else
  flunk "24 operative: install failed"; cat "$TMP/op2.log"
fi

# --- 25. --refresh-env rewrites the operative section, refuses once confirmed -
NM2="$TMP/nm751b"; mkdir -p "$NM2/run"; mk_stub "$NM2/run/nmfe75" 'echo "nmfe75 stub"'
FAKEPATH2="$STUBS:$NM2/run"
if env -i PATH="$FAKEPATH2" HOME="$TMP/fakehome" "$PY3" "$SETUP" install --refresh-env \
     --target "$OPT" --profile "$ADAPTER/examples/minimal" --force >"$TMP/op3.log" 2>&1; then
  ok=1
  grep -qE "^nonmem_home: $NM2   #" "$OPT/harness.yaml" || { echo "  operative nonmem_home not re-probed"; ok=0; }
  grep -qE "^nonmem_home: $NM   #" "$OPT/harness.yaml" && { echo "  stale operative nonmem_home left behind"; ok=0; }
  [ "$(grep -c '^# >>> operative settings' "$OPT/harness.yaml")" = 1 ] || { echo "  operative section duplicated"; ok=0; }
  [ $ok = 1 ] && pass "25 --refresh-env: operative section rewritten while unconfirmed" || flunk "25 --refresh-env operative"
else
  flunk "25 --refresh-env failed"; cat "$TMP/op3.log"
fi
sed -i 's/^environment_confirmed: false/environment_confirmed: true/' "$OPT/harness.yaml"
before="$(sha256sum "$OPT/harness.yaml" | cut -d' ' -f1)"
if env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" install --refresh-env \
     --target "$OPT" --profile "$ADAPTER/examples/minimal" --force >"$TMP/op4.log" 2>&1; then
  flunk "25b --refresh-env: succeeded although environment_confirmed: true"
else
  [ "$before" = "$(sha256sum "$OPT/harness.yaml" | cut -d' ' -f1)" ] \
    && pass "25b --refresh-env: confirmed operative settings never overwritten" \
    || flunk "25b --refresh-env: confirmed file was modified"
fi

# --- 26. env reports drift on the operative nonmem_home ----------------------
if env -i PATH="$FAKEPATH2" HOME="$TMP/fakehome" "$PY3" "$SETUP" env --target "$OPT" >"$TMP/op5.log" 2>&1; then
  sed -i "s|^nonmem_home: .*|nonmem_home: /opt/NONMEM/elsewhere|" "$OPT/harness.yaml"
  if env -i PATH="$FAKEPATH2" HOME="$TMP/fakehome" "$PY3" "$SETUP" env --target "$OPT" >"$TMP/op6.log" 2>&1; then
    flunk "26 env: exit 0 although the operative nonmem_home no longer matches"; cat "$TMP/op6.log"
  elif grep -q 'operative nonmem_home' "$TMP/op6.log"; then
    pass "26 env: operative nonmem_home drift reported on its own line"
  else
    flunk "26 env: operative drift not reported"; cat "$TMP/op6.log"
  fi
else
  flunk "26 env: baseline run reported drift against a freshly refreshed file"; cat "$TMP/op5.log"
fi

# --- 27. a marker-less harness.yaml keeps its hand-written keys --------------
HAND="$TMP/handwritten"; mkdir -p "$HAND"
cat > "$HAND/harness.yaml" <<'YAML'
environment_confirmed: false
r_provision: site
nonmem_home: /opt/NONMEM/nm751
nonmem_license: /opt/nonmem/site.lic
YAML
cp "$HAND/harness.yaml" "$TMP/handwritten.orig"
if env -i PATH="$FAKEPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" install --refresh-env \
     --target "$HAND" --profile "$ADAPTER/examples/minimal" --force >"$TMP/op7.log" 2>&1; then
  ok=1
  head -c "$(wc -c < "$TMP/handwritten.orig")" "$HAND/harness.yaml" \
    | cmp -s - "$TMP/handwritten.orig" || { echo "  hand-written keys were modified"; ok=0; }
  grep -q '^# >>> operative settings' "$HAND/harness.yaml" && { echo "  operative section injected into a hand-written file"; ok=0; }
  grep -q '^# >>> environment block' "$HAND/harness.yaml" || { echo "  environment block not appended"; ok=0; }
  [ $ok = 1 ] && pass "27 hand-written harness.yaml: operative keys untouched, only the report appended" \
    || flunk "27 hand-written harness.yaml"
else
  flunk "27 hand-written: refresh failed"; cat "$TMP/op7.log"
fi

# --- containerized stubs (docker present, no nmfe on PATH) -------------------
# The image answer is a stub `docker` that prints an id for `images -q`.
# Nothing here contacts a daemon or pulls anything.
DSTUBS="$TMP/dstubs"; mkdir -p "$DSTUBS"
cp "$STUBS/Rscript" "$STUBS/R" "$STUBS/execute" "$STUBS/psn" "$STUBS/quarto" "$DSTUBS/"
mk_stub "$DSTUBS/docker" 'case "$1" in --version) echo "Docker version 27.1.1, build abc";; images) echo "0123456789ab";; *) exit 1;; esac'
DPATH="$DSTUBS"
IMG="pmx-harness/nonmem:7.5.1"

mk_engine_yaml() {
  mkdir -p "$1/config"
  printf 'engines:\n  nonmem:\n    provision: containerized\n    image: %s\n' "$IMG" \
    > "$1/config/engine.yaml"
}

# --- 28. containerized draft pre-fills the operative nonmem_image ------------
CIMG="$TMP/imgtarget"; mkdir -p "$CIMG"
cp "$REPO/harness.yaml.example" "$CIMG/harness.yaml.example"
mk_engine_yaml "$CIMG"
if env -i PATH="$DPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" install \
     --target "$CIMG" --profile "$ADAPTER/examples/minimal" >"$TMP/op8.log" 2>&1; then
  H="$CIMG/harness.yaml"; ok=1
  grep -qE "^nonmem_image: $IMG   # detected .* via docker images -q " "$H" \
    || { echo "  nonmem_image not pre-filled with provenance"; ok=0; }
  grep -q '^nonmem_provision_suggested: containerized' "$H" \
    || { echo "  provision suggestion not containerized"; ok=0; }
  grep -qE '^nonmem_home:' "$H" && { echo "  nonmem_home written although no nmfe was found"; ok=0; }
  [ $ok = 1 ] && pass "28 operative: nonmem_image pre-filled on a containerized machine" \
    || flunk "28 operative nonmem_image"
else
  flunk "28 operative nonmem_image: install failed"; cat "$TMP/op8.log"
fi

# --- 29. docker present, image not built: the key is commented out ----------
NOIMG="$TMP/noimgtarget"; mkdir -p "$NOIMG"
cp "$REPO/harness.yaml.example" "$NOIMG/harness.yaml.example"
mk_engine_yaml "$NOIMG"
NDSTUBS="$TMP/ndstubs"; mkdir -p "$NDSTUBS"
cp "$DSTUBS/Rscript" "$DSTUBS/R" "$DSTUBS/execute" "$DSTUBS/psn" "$DSTUBS/quarto" "$NDSTUBS/"
mk_stub "$NDSTUBS/docker" 'case "$1" in --version) echo "Docker version 27.1.1, build abc";; images) : ;; *) exit 1;; esac'
if env -i PATH="$NDSTUBS" HOME="$TMP/fakehome" "$PY3" "$SETUP" install \
     --target "$NOIMG" --profile "$ADAPTER/examples/minimal" >"$TMP/op9.log" 2>&1; then
  H="$NOIMG/harness.yaml"; ok=1
  grep -q '^# nonmem_image:.*not found' "$H" || { echo "  nonmem_image not commented with a reason"; ok=0; }
  grep -qE '^nonmem_image:' "$H" && { echo "  nonmem_image written although the image is not built"; ok=0; }
  [ $ok = 1 ] && pass "29 operative: nonmem_image commented out when the image is not built" \
    || flunk "29 operative nonmem_image not-found"
else
  flunk "29 operative nonmem_image not-found: install failed"; cat "$TMP/op9.log"
fi

# --- 30. env reports drift on the operative nonmem_image --------------------
if env -i PATH="$DPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" env --target "$CIMG" >"$TMP/op10.log" 2>&1; then
  sed -i "s|^nonmem_image: .*|nonmem_image: pmx-harness/nonmem:9.9.9|" "$CIMG/harness.yaml"
  if env -i PATH="$DPATH" HOME="$TMP/fakehome" "$PY3" "$SETUP" env --target "$CIMG" >"$TMP/op11.log" 2>&1; then
    flunk "30 env: exit 0 although the operative nonmem_image no longer matches"; cat "$TMP/op11.log"
  elif grep -q 'operative nonmem_image' "$TMP/op11.log"; then
    pass "30 env: operative nonmem_image drift reported on its own line"
  else
    flunk "30 env: operative nonmem_image drift not reported"; cat "$TMP/op11.log"
  fi
else
  flunk "30 env: baseline run reported drift against a freshly drafted file"; cat "$TMP/op10.log"
fi


# --- 31. a seed refusal is a preflight refusal: nothing is written -----------
KITV="$(safe_dest "$TMP/kitvirgin")"
if python3 "$SETUP" export "$KITV" >"$TMP/kitv.log" 2>&1; then
  # A virgin bundle whose target already owns a DIFFERENT copy of a seed asset.
  mkdir -p "$KITV/tools"
  printf '# the target owns this file already\n' > "$KITV/tools/gate.R"
  if ( cd "$KITV" && python3 copilot-adapter/setup.py install ) >"$TMP/kitvi.log" 2>&1; then
    flunk "31 preflight: install succeeded over a differing seed asset on a virgin bundle"
  else
    ok=1
    grep -q 'tools/gate.R' "$TMP/kitvi.log" || { echo "  refusal does not name the file"; ok=0; }
    [ -e "$KITV/.github/copilot-instructions.md" ] && { echo "  a generated file was written despite the refusal"; ok=0; }
    [ -e "$KITV/copilot-adapter/.seed-state.json" ] && { echo "  seed state written despite the refusal"; ok=0; }
    [ -e "$KITV/copilot-adapter/.generated-inventory.json" ] && { echo "  inventory written despite the refusal"; ok=0; }
    [ -e "$KITV/roles/orchestrator.md" ] && { echo "  an asset was seeded despite the refusal"; ok=0; }
    [ $ok = 1 ] && pass "31 preflight: seed refusal leaves no generated file, no seed state, no inventory, no seeded asset" \
      || { flunk "31 preflight"; cat "$TMP/kitvi.log"; }
  fi
else
  flunk "31 preflight: export failed"; cat "$TMP/kitv.log"
fi

# --- 32. ASSETS.json paths are validated before any write -------------------
tamper_manifest() {  # $1 = kit root, $2 = python snippet mutating `m`
  python3 - "$1/copilot-adapter/assets/ASSETS.json" "$2" <<'PY'
import json, sys
path, snippet = sys.argv[1], sys.argv[2]
m = json.load(open(path))
exec(snippet)
json.dump(m, open(path, "w"), indent=2)
PY
}
for label in escape absolute duplicate; do
  KITB="$(safe_dest "$TMP/kitbad-$label")"
  python3 "$SETUP" export "$KITB" >/dev/null 2>&1 || { flunk "32 $label: export failed"; continue; }
  case "$label" in
    escape)    tamper_manifest "$KITB" 'm["assets"].append({"path": "../escaped.R", "sha256": "0"*64, "bytes": 0})' ;;
    absolute)  tamper_manifest "$KITB" 'm["assets"].append({"path": "/etc/escaped.R", "sha256": "0"*64, "bytes": 0})' ;;
    duplicate) tamper_manifest "$KITB" 'm["assets"].append(dict(m["assets"][0]))' ;;
  esac
  if ( cd "$KITB" && python3 copilot-adapter/setup.py install ) >"$TMP/kitbad-$label.log" 2>&1; then
    flunk "32 $label: install succeeded on an invalid ASSETS.json path"
  elif grep -q 'invalid asset path' "$TMP/kitbad-$label.log" \
       && [ ! -e "$KITB/.github/copilot-instructions.md" ] \
       && [ ! -e "$KITB/roles/orchestrator.md" ]; then
    pass "32 $label ASSETS.json path: refused before any write, message names the defect"
  else
    flunk "32 $label ASSETS.json path"; cat "$TMP/kitbad-$label.log"
  fi
  rm -rf "$KITB"
done

# --- 33. export refuses a destination overlapping the source ----------------
# Run entirely against a DISPOSABLE adapter: this is the case whose defect
# deleted the live copilot-adapter/ on 2026-09-03, so it never names $REPO.
DROOT="$(safe_dest "$TMP/disposable")"
DSETUP="$(disposable_adapter "$DROOT")"
ok=1
DSUM="$(sha256sum "$DSETUP" | cut -d' ' -f1)"
for dest in "$DROOT" "$DROOT/copilot-adapter" "$DROOT/copilot-adapter/tests" "$(dirname "$DROOT")"; do
  if python3 "$DSETUP" export "$dest" --force >"$TMP/overlap.log" 2>&1; then
    echo "  export to '$dest' was not refused"; ok=0
  elif ! grep -q 'REFUSED' "$TMP/overlap.log"; then
    echo "  export to '$dest' failed without a clear refusal"; cat "$TMP/overlap.log"; ok=0
  fi
done
[ -f "$DSETUP" ] && [ "$(sha256sum "$DSETUP" | cut -d' ' -f1)" = "$DSUM" ] \
  || { echo "  the disposable source adapter was modified or deleted"; ok=0; }
[ -f "$DROOT/copilot-adapter/profile/adapter.toml" ] || { echo "  the disposable profile was deleted"; ok=0; }
# A non-overlapping destination still works, so the guard is not a blanket refusal.
# Generator-only: the disposable tree carries no payload sources, and a kit
# export from it is (correctly) refused by the required-pattern rule (case 39).
python3 "$DSETUP" export "$(safe_dest "$TMP/okdest")" --no-profile >"$TMP/okdest.log" 2>&1 \
  || { echo "  a non-overlapping export was refused"; cat "$TMP/okdest.log"; ok=0; }
[ $ok = 1 ] && pass "33 export overlap: DEST at/inside/above the adapter or the repo root refused, source intact, clean DEST still works" \
  || flunk "33 export overlap"

# --- 34. the installed kit workflow has its own effective commands ---------
KITC="$(safe_dest "$TMP/kitci")"
if python3 "$SETUP" export "$KITC" >"$TMP/kitc.log" 2>&1; then
  ok=1
  WF="$KITC/.github/workflows/ci.yaml"
  ( cd "$KITC" && git init -q -b main . &&
    python3 copilot-adapter/setup.py install ) >"$TMP/kitcinst.log" 2>&1 \
    || { echo "  install into the kit failed"; tail -5 "$TMP/kitcinst.log"; ok=0; }
  cmp -s "$WF" "$KITC/copilot-adapter/assets/.github/workflows/ci.yaml" \
    || { echo "  installed kit workflow differs from the exported asset"; ok=0; }

  # Parse the installed workflow, including the job key and complete effective
  # command list. The setup step provisions the host and is checked separately.
  python3 -c '
import sys
import yaml
with open(sys.argv[1], encoding="utf-8") as stream:
    jobs = yaml.safe_load(stream)["jobs"]
assert list(jobs) == ["kit-battery"], "unexpected kit job keys"
job = jobs["kit-battery"]
assert "if" not in job and not job.get("continue-on-error"), "kit job must be required"
steps = job["steps"]
assert steps[0]["with"] == {"fetch-depth": 0, "persist-credentials": False}
assert "r-cran-jsonlite" in steps[1]["run"] and "python3-yaml" in steps[1]["run"]
runs = []
for step in steps[2:]:
    assert not ({"if", "shell", "env", "working-directory", "continue-on-error"} & step.keys())
    runs.append(step["run"])
assert runs == ["Rscript tools/ci_check.R --ledger-only",
                "Rscript tools/ci_check.R --light",
                "bash tools/tests/test_protect_paths.sh",
                "bash copilot-adapter/tests/test_setup.sh",
                "python3 copilot-adapter/setup.py check"], "incorrect effective kit commands"
print("\n".join(runs))
' "$WF" >"$TMP/kitcommands.log" 2>"$TMP/kitparse.log" \
    || { echo "  could not verify the installed workflow"; cat "$TMP/kitparse.log"; ok=0; }
  mapfile -t RUNS <"$TMP/kitcommands.log"

  if [ -n "${COPILOT_ADAPTER_TEST_DEPTH:-}" ]; then
    echo "  (nested run: workflow parsed without running the commands again)"
  elif [ "$ok" = 1 ]; then
    if ! ( cd "$KITC" && git add -- .github/workflows/ci.yaml &&
      git -c user.name='Local Fixture Reviewer' -c user.email='fixture@example.invalid' \
        -c commit.gpgsign=false commit -qm 'Initial kit fixture' ); then
      echo "  could not create a parentless main for the kit"
      ok=0
    fi
    for cmd in "${RUNS[@]}"; do
      if [ "$cmd" = 'Rscript tools/ci_check.R --ledger-only' ]; then
        cmd="$cmd --initial-main"
      fi
      if ! ( cd "$KITC" && env -u GITHUB_ACTIONS -u GITHUB_EVENT_NAME \
        COPILOT_ADAPTER_TEST_DEPTH=1 bash -c "$cmd" ) >"$TMP/kitrun.log" 2>&1; then
        echo "  kit workflow command failed: $cmd"
        tail -25 "$TMP/kitrun.log" | sed 's/^/    | /'
        ok=0
      elif [[ "$cmd" == *'--initial-main' ]] &&
           ! grep -q 'no parent to compare; tracked/live approval ledgers 0/0, errata ledgers 0/0, signed records 0/0' "$TMP/kitrun.log"; then
        echo "  kit preflight did not attest an empty parentless baseline"
        ok=0
      fi
    done

    kit_sha="$(cd "$KITC" && git rev-parse --verify HEAD)"
    zero=0000000000000000000000000000000000000000
    kit_event="$TMP/kit-event.json"
    run_kit_event() {
      local before="$1" after="$2" created="$3" ref="$4"
      printf '{"ref":"%s","before":"%s","after":"%s","created":%s,"deleted":false,"forced":false,"repository":{"default_branch":"main"}}\n' \
        "$ref" "$before" "$after" "$created" >"$kit_event"
      ( cd "$KITC" && GITHUB_ACTIONS=true GITHUB_EVENT_NAME=push GITHUB_EVENT_PATH="$kit_event" \
        GITHUB_SHA="$after" GITHUB_REF="$ref" Rscript tools/ci_check.R --ledger-only ) >"$TMP/kitevent.log" 2>&1
    }
    if ! run_kit_event "$zero" "$kit_sha" true refs/heads/main ||
       ! grep -q 'PASS initial main ledger baseline' "$TMP/kitevent.log"; then
      echo "  modeled initial main push failed"; tail -12 "$TMP/kitevent.log"; ok=0
    fi
    if run_kit_event "$zero" "$kit_sha" true refs/heads/other ||
       ! grep -q 'Invalid main push identity' "$TMP/kitevent.log"; then
      echo "  non-main initial push did not fail with the expected reason"; ok=0
    fi
    mkdir -p "$KITC/gates/KIT-FIXTURE"
    : >"$KITC/gates/KIT-FIXTURE/errata.jsonl"
    if run_kit_event "$zero" "$kit_sha" true refs/heads/main ||
       ! grep -q 'Initial main has production history' "$TMP/kitevent.log"; then
      echo "  initial push with a live empty ledger did not fail"; ok=0
    fi
    rm -- "$KITC/gates/KIT-FIXTURE/errata.jsonl"

    if ( cd "$KITC" && git -c user.name='Local Fixture Reviewer' -c user.email='fixture@example.invalid' \
         -c commit.gpgsign=false commit --allow-empty -qm 'Later kit fixture' ); then
      later_sha="$(cd "$KITC" && git rev-parse --verify HEAD)"
      if ! run_kit_event "$kit_sha" "$later_sha" false refs/heads/main ||
         ! grep -q 'PASS approval and errata ledger append-only' "$TMP/kitevent.log"; then
        echo "  modeled later main push failed"; tail -12 "$TMP/kitevent.log"; ok=0
      fi
      if run_kit_event "$zero" "$later_sha" true refs/heads/main ||
         ! grep -q 'Initial main requires a parentless commit' "$TMP/kitevent.log"; then
        echo "  zero-base push with existing history did not fail"; ok=0
      fi
      if run_kit_event "" "$later_sha" false refs/heads/main ||
         ! grep -q 'Expected an exact nonzero commit SHA' "$TMP/kitevent.log"; then
        echo "  missing later base did not fail with the expected reason"; ok=0
      fi
    else
      echo "  could not create the later kit fixture commit"; ok=0
    fi
  fi
  [ "$ok" = 1 ] && pass "34 kit workflow: independent installed profile and ledger event checks" \
    || flunk "34 kit workflow"
else
  flunk "34 kit workflow: export failed"; cat "$TMP/kitc.log"
fi

# --- 35. install writes the generated inventory -----------------------------
INV="$KITC/copilot-adapter/.generated-inventory.json"
if [ -f "$INV" ]; then
  ok=1
  python3 - "$INV" "$KITC" <<'PY' || ok=0
import hashlib, json, pathlib, sys
inv = json.load(open(sys.argv[1]))
root = pathlib.Path(sys.argv[2])
assert inv["schema"] == 1, inv
assert inv["generated_at"].endswith("Z"), inv["generated_at"]
assert len(inv["files"]) >= 30, len(inv["files"])
for rel, sha in inv["files"].items():
    p = root / rel
    assert p.is_file(), f"inventory names a missing file: {rel}"
    got = hashlib.sha256(p.read_bytes()).hexdigest()
    assert got == sha, f"{rel}: {got} != {sha}"
PY
  ( cd "$KITC" && python3 copilot-adapter/setup.py check ) >"$TMP/invcheck.log" 2>&1 \
    || { echo "  check failed with an inventory present"; tail -5 "$TMP/invcheck.log"; ok=0; }
  [ $ok = 1 ] && pass "35 inventory: written, hashes match the generated files, check stays clean" \
    || flunk "35 inventory"
else
  flunk "35 inventory: copilot-adapter/.generated-inventory.json not written"
fi

# --- 36. an orphaned generated file fails check and is removed by install ----
ORPHAN="$KITC/.github/skills/forest-plots/SKILL.md"
if [ -f "$ORPHAN" ]; then
  ok=1
  cp "$ORPHAN" "$TMP/orphan.bak"
  python3 - "$KITC/copilot-adapter/profile/adapter.toml" <<'PY'
import re, sys
path = sys.argv[1]
text = open(path).read()
stripped = re.sub(r'\n\[\[skills\]\]\nname = "forest-plots"\ndescription = "[^"]*"\n', "\n", text)
assert stripped != text, "the forest-plots [[skills]] entry was not found"
open(path, "w").write(stripped)
PY
  rm -rf "$KITC/skills/forest-plots"
  if ( cd "$KITC" && python3 copilot-adapter/setup.py check ) >"$TMP/orphchk.log" 2>&1; then
    echo "  check reported green with an orphaned generated file"; ok=0
  elif ! grep -q 'ORPHAN.*\.github/skills/forest-plots/SKILL.md' "$TMP/orphchk.log"; then
    echo "  check did not name the orphan"; cat "$TMP/orphchk.log"; ok=0
  fi
  # A hand-edited orphan is refused, never deleted.
  echo "# hand edit" >> "$ORPHAN"
  if ( cd "$KITC" && python3 copilot-adapter/setup.py install ) >"$TMP/orphedit.log" 2>&1; then
    echo "  install removed or ignored a hand-edited orphan"; ok=0
  else
    grep -q '.github/skills/forest-plots/SKILL.md' "$TMP/orphedit.log" \
      || { echo "  the refusal does not name the hand-edited orphan"; ok=0; }
    [ -f "$ORPHAN" ] || { echo "  a hand-edited orphan was deleted"; ok=0; }
  fi
  # Restored to the bytes the generator recorded, it is removed.
  cp "$TMP/orphan.bak" "$ORPHAN"
  ( cd "$KITC" && python3 copilot-adapter/setup.py install ) >"$TMP/orphrm.log" 2>&1 \
    || { echo "  install failed on an unchanged orphan"; tail -5 "$TMP/orphrm.log"; ok=0; }
  [ -e "$ORPHAN" ] && { echo "  the unchanged orphan was not removed"; ok=0; }
  ( cd "$KITC" && python3 copilot-adapter/setup.py check ) >"$TMP/orphclean.log" 2>&1 \
    || { echo "  check still fails after the orphan was removed"; tail -5 "$TMP/orphclean.log"; ok=0; }
  [ $ok = 1 ] && pass "36 orphan: check fails and names it, a hand-edited one is refused, an unchanged one is removed" \
    || flunk "36 orphan"
else
  flunk "36 orphan: the fixture generated file is missing"
fi

# --- 37. the generated inventory is validated before any read or delete -----
# Every fixture here works on the installed kit KITC (disposable, under $TMP)
# and on files created under $TMP; nothing points at the repository.
INV="$KITC/copilot-adapter/.generated-inventory.json"
if [ -f "$INV" ] && ( cd "$KITC" && python3 copilot-adapter/setup.py check ) >/dev/null 2>&1; then
  cp "$INV" "$TMP/inventory.good"
  # 37a. malformed inventory: check fails and names the file.
  printf '{ not json\n' > "$INV"
  if ( cd "$KITC" && python3 copilot-adapter/setup.py check ) >"$TMP/inv-bad.log" 2>&1; then
    flunk "37a malformed inventory: check exited 0"
  elif grep -q 'generated-inventory.json' "$TMP/inv-bad.log"; then
    pass "37a malformed inventory: check fails and names the inventory"
  else
    flunk "37a malformed inventory: wrong message"; cat "$TMP/inv-bad.log"
  fi
  # 37b. a schema-valid document with an invalid files map (bad sha grammar).
  python3 - "$TMP/inventory.good" "$INV" <<'PY'
import json, sys
inv = json.load(open(sys.argv[1]))
inv["files"][".github/agents/pmx-modeler.agent.md"] = "not-a-sha"
json.dump(inv, open(sys.argv[2], "w"), indent=2)
PY
  if ( cd "$KITC" && python3 copilot-adapter/setup.py check ) >"$TMP/inv-sha.log" 2>&1; then
    flunk "37b bad sha grammar: check exited 0"
  elif grep -q 'not-a-sha' "$TMP/inv-sha.log"; then
    pass "37b bad sha grammar: check fails and names the offending value"
  else
    flunk "37b bad sha grammar: wrong message"; cat "$TMP/inv-sha.log"
  fi
  # 37c. a `..` key whose hash matches a real file OUTSIDE the target is
  # refused before any delete: the file must survive.
  ESC="$(safe_dest "$TMP/escaped-by-inventory.md")"
  printf 'must survive\n' > "$ESC"
  ESCSHA="$(sha256sum "$ESC" | cut -d' ' -f1)"
  python3 - "$TMP/inventory.good" "$INV" "$ESCSHA" <<'PY'
import json, sys
inv = json.load(open(sys.argv[1]))
inv["files"]["../escaped-by-inventory.md"] = sys.argv[3]
json.dump(inv, open(sys.argv[2], "w"), indent=2)
PY
  if ( cd "$KITC" && python3 copilot-adapter/setup.py install ) >"$TMP/inv-dotdot.log" 2>&1; then
    flunk "37c '..' inventory key: install succeeded"
  elif [ -f "$ESC" ] && grep -q 'escaped-by-inventory.md' "$TMP/inv-dotdot.log"; then
    pass "37c '..' inventory key: refused before any delete, file outside the target survives"
  else
    flunk "37c '..' inventory key: file deleted or message unclear"; cat "$TMP/inv-dotdot.log"
  fi
  # 37d. an absolute key with a matching hash is refused the same way.
  ABS="$(safe_dest "$TMP/absolute-by-inventory.md")"
  printf 'must survive too\n' > "$ABS"
  ABSSHA="$(sha256sum "$ABS" | cut -d' ' -f1)"
  python3 - "$TMP/inventory.good" "$INV" "$ABSSHA" "$ABS" <<'PY'
import json, sys
inv = json.load(open(sys.argv[1]))
inv["files"][sys.argv[4]] = sys.argv[3]
json.dump(inv, open(sys.argv[2], "w"), indent=2)
PY
  if ( cd "$KITC" && python3 copilot-adapter/setup.py install ) >"$TMP/inv-abs.log" 2>&1; then
    flunk "37d absolute inventory key: install succeeded"
  elif [ -f "$ABS" ] && grep -q 'absolute-by-inventory.md' "$TMP/inv-abs.log"; then
    pass "37d absolute inventory key: refused before any delete, file survives"
  else
    flunk "37d absolute inventory key: file deleted or message unclear"; cat "$TMP/inv-abs.log"
  fi
  # 37e. a relative key INSIDE the target but outside every generated root
  # (the kit's own CI workflow) is refused even with a matching hash.
  WFK="$KITC/.github/workflows/ci.yaml"
  if [ -f "$WFK" ]; then
    WFSHA="$(sha256sum "$WFK" | cut -d' ' -f1)"
    python3 - "$TMP/inventory.good" "$INV" "$WFSHA" <<'PY'
import json, sys
inv = json.load(open(sys.argv[1]))
inv["files"][".github/workflows/ci.yaml"] = sys.argv[3]
json.dump(inv, open(sys.argv[2], "w"), indent=2)
PY
    if ( cd "$KITC" && python3 copilot-adapter/setup.py install ) >"$TMP/inv-root.log" 2>&1; then
      flunk "37e key outside the generated roots: install succeeded"
    elif [ -f "$WFK" ] && grep -q 'workflows/ci.yaml' "$TMP/inv-root.log"; then
      pass "37e key outside the generated roots: refused, the workflow survives"
    else
      flunk "37e key outside the generated roots"; cat "$TMP/inv-root.log"
    fi
  fi
  # 37f. a MISSING inventory on a managed installation fails check.
  rm -f "$INV"
  if ( cd "$KITC" && python3 copilot-adapter/setup.py check ) >"$TMP/inv-missing.log" 2>&1; then
    flunk "37f missing inventory on a managed target: check exited 0"
  elif grep -q 'generated-inventory.json' "$TMP/inv-missing.log"; then
    pass "37f missing inventory on a managed target: check fails and names the inventory"
  else
    flunk "37f missing inventory: wrong message"; cat "$TMP/inv-missing.log"
  fi
  # Recovery: install rewrites the inventory and check is clean again.
  ( cd "$KITC" && python3 copilot-adapter/setup.py install ) >"$TMP/inv-recover.log" 2>&1 \
    && ( cd "$KITC" && python3 copilot-adapter/setup.py check ) >"$TMP/inv-recover2.log" 2>&1 \
    && pass "37g inventory recovery: install rewrites it, check clean" \
    || { flunk "37g inventory recovery"; cat "$TMP/inv-recover.log" "$TMP/inv-recover2.log"; }
  # 37h. a fresh target (nothing generated yet, no inventory) still installs.
  FRESH="$(safe_dest "$TMP/fresh-target")"; mkdir -p "$FRESH"
  if python3 "$SETUP" install --target "$FRESH" --profile "$ADAPTER/examples/minimal" >"$TMP/fresh.log" 2>&1 \
     && [ -f "$FRESH/copilot-adapter/.generated-inventory.json" ]; then
    pass "37h fresh target: installs without an inventory and writes one"
  else
    flunk "37h fresh target"; cat "$TMP/fresh.log"
  fi
else
  flunk "37 inventory validation: the installed kit fixture is not clean"
fi

# --- 38. a refused install leaves ZERO new files (listing diff) --------------
# Virgin bundle: the seeds are pending AND the target already owns a differing
# generated file. Before the global preflight the seeds were copied before the
# generated-file refusal was known.
KITR="$(safe_dest "$TMP/kitrefused")"
if python3 "$SETUP" export "$KITR" >"$TMP/kitr.log" 2>&1; then
  mkdir -p "$KITR/.github"
  printf '# the target owns this generated path already\n' > "$KITR/.github/copilot-instructions.md"
  ( cd "$KITR" && find . -type f | sort ) > "$TMP/kitr.before"
  if ( cd "$KITR" && python3 copilot-adapter/setup.py install ) >"$TMP/kitri.log" 2>&1; then
    flunk "38 refused install: install succeeded over a differing generated file"
  else
    ( cd "$KITR" && find . -type f | sort ) > "$TMP/kitr.after"
    if cmp -s "$TMP/kitr.before" "$TMP/kitr.after" && grep -q 'copilot-instructions.md' "$TMP/kitri.log"; then
      pass "38 refused install: zero new files (listing identical), refusal names the file"
    else
      flunk "38 refused install: files were written despite the refusal"
      diff "$TMP/kitr.before" "$TMP/kitr.after" | head -20
    fi
  fi
  # With --force the same virgin bundle installs completely and check is clean.
  ( cd "$KITR" && git init -q . ) >/dev/null 2>&1
  ( cd "$KITR" && python3 copilot-adapter/setup.py install --force ) >"$TMP/kitrf.log" 2>&1 \
    && ( cd "$KITR" && python3 copilot-adapter/setup.py check ) >"$TMP/kitrfc.log" 2>&1 \
    && pass "38b refused install then --force: seeds, renders, check clean" \
    || { flunk "38b --force on the virgin bundle"; tail -5 "$TMP/kitrf.log" "$TMP/kitrfc.log"; }
else
  flunk "38 refused install: export failed"; cat "$TMP/kitr.log"
fi

# --- 39. export payload patterns are required unless marked optional --------
PSRC="$(safe_dest "$TMP/paysrc")"; mkdir -p "$PSRC/tools"
printf '# tool\n' > "$PSRC/tools/a.R"
PPROF="$TMP/payprofile"; cp -r "$ADAPTER/examples/minimal" "$PPROF"
printf '\n[export]\npayload = ["tools/**", "docs/**"]\nexclude = []\n' >> "$PPROF/adapter.toml"
if python3 "$SETUP" export "$(safe_dest "$TMP/payout")" --target "$PSRC" --profile "$PPROF" >"$TMP/pay.log" 2>&1; then
  flunk "39 unmatched required pattern: export succeeded"
elif grep -q 'docs/\*\*' "$TMP/pay.log" && [ ! -e "$TMP/payout/copilot-adapter" ]; then
  pass "39 unmatched required pattern: export refused, names the pattern, writes nothing"
else
  flunk "39 unmatched required pattern: wrong message or files written"; cat "$TMP/pay.log"
fi
PPROF2="$TMP/payprofile2"; cp -r "$ADAPTER/examples/minimal" "$PPROF2"
printf '\n[export]\npayload = ["tools/**", { path = "docs/**", optional = true }]\nexclude = []\n' >> "$PPROF2/adapter.toml"
if python3 "$SETUP" export "$(safe_dest "$TMP/payout2")" --target "$PSRC" --profile "$PPROF2" >"$TMP/pay2.log" 2>&1; then
  if grep -q 'docs/\*\*' "$TMP/pay2.log" && [ -f "$TMP/payout2/copilot-adapter/assets/tools/a.R" ]; then
    pass "39b unmatched optional pattern: export succeeds with a warning naming the pattern"
  else
    flunk "39b unmatched optional pattern: no warning or assets missing"; cat "$TMP/pay2.log"
  fi
else
  flunk "39b unmatched optional pattern: export refused"; cat "$TMP/pay2.log"
fi

# --- 40. reviewer isolation wording and model attestation -------------------
if [ -f "$AG/pmx-reviewer.agent.md" ] && [ -f "$AG/pmx-orchestrator.agent.md" ]; then
  ok=1
  grep -q 'from anywhere else' "$AG/pmx-orchestrator.agent.md" && { echo "  orchestrator still claims the reviewer is not invocable from anywhere else"; ok=0; }
  grep -q 'not automatically invocable by generated specialists' "$AG/pmx-orchestrator.agent.md" || { echo "  orchestrator lacks the bounded invocation claim"; ok=0; }
  grep -qi 'procedural' "$AG/pmx-reviewer.agent.md" || { echo "  reviewer does not state that isolation is procedural on Copilot"; ok=0; }
  grep -qi 'runtime-reported model' "$AG/pmx-reviewer.agent.md" || { echo "  reviewer does not record the runtime-reported model"; ok=0; }
  [ $ok = 1 ] && pass "40 reviewer wording: procedural isolation, bounded invocation claim, model attestation" \
    || flunk "40 reviewer wording"
else
  flunk "40 reviewer wording: generated agents missing"
fi

# --- 41. the profile's explicit do-not-edit files equal the guard's HARNESS_CORE ---
# Integrity batch 13 (Codex round 12, P1-4): the profile's enforcement list
# omitted tools/approve_gate.R while the live hook protected it, and where hooks
# are disabled by policy the instruction IS the barrier. Both lists are parsed
# (TOML and the Python set literal) and compared as sets.
if python3 - "$ADAPTER/profile/adapter.toml" "$REPO/.claude/hooks/protect_paths.py" <<'PY' >"$TMP/core.log" 2>&1
import ast, re, sys, tomllib
prof = tomllib.load(open(sys.argv[1], "rb"))
paths = set()
for item in prof.get("do_not_edit", []):
    p = item["path"]
    for x in (p if isinstance(p, list) else [p]):
        if "*" not in x:
            paths.add(x)
src = open(sys.argv[2]).read()
m = re.search(r"HARNESS_CORE = (\{.*?\n\})", src, re.S)
core = set(ast.literal_eval(m.group(1)))
if paths != core:
    print("profile plain paths:", sorted(paths)); print("HARNESS_CORE:", sorted(core))
    print("only in profile:", sorted(paths - core)); print("only in HARNESS_CORE:", sorted(core - paths))
    sys.exit(1)
print("both lists:", sorted(core))
PY
then
  pass "41 profile do_not_edit plain paths equal HARNESS_CORE ($(tail -1 "$TMP/core.log"))"
else
  flunk "41 profile do_not_edit plain paths differ from HARNESS_CORE"; cat "$TMP/core.log"
fi

# --- 42. export refuses symlink payload sources before writing the kit -------
SYMSRC="$(safe_dest "$TMP/symlink-source")"; mkdir -p "$SYMSRC/roles" "$SYMSRC/private"
printf 'outside payload bytes\n' > "$TMP/external-canary.md"
printf 'excluded payload bytes\n' > "$SYMSRC/private/excluded.md"
SYMPROF="$TMP/symlink-profile"; cp -r "$ADAPTER/examples/minimal" "$SYMPROF"
printf '\n[export]\npayload = ["roles/**"]\nexclude = ["private/**"]\n' >> "$SYMPROF/adapter.toml"
ln -s "$TMP/external-canary.md" "$SYMSRC/roles/external.md"
SYMOUT="$(safe_dest "$TMP/symlink-export")"
if python3 "$SETUP" export "$SYMOUT" --target "$SYMSRC" --profile "$SYMPROF" >"$TMP/symlink-external.log" 2>&1; then
  flunk "42a external symlink payload: export succeeded"
elif grep -qi 'symlink' "$TMP/symlink-external.log" && [ ! -e "$SYMOUT/copilot-adapter" ]; then
  pass "42a external symlink payload: refused before kit bytes were written"
else
  flunk "42a external symlink payload: wrong message or output exists"; cat "$TMP/symlink-external.log"
fi
ln -s ../private/excluded.md "$SYMSRC/roles/excluded-alias.md"
SYMOUT2="$(safe_dest "$TMP/symlink-export-excluded")"
if python3 "$SETUP" export "$SYMOUT2" --target "$SYMSRC" --profile "$SYMPROF" >"$TMP/symlink-excluded.log" 2>&1; then
  flunk "42b excluded-referent symlink payload: export succeeded"
elif grep -qi 'symlink' "$TMP/symlink-excluded.log" && [ ! -e "$SYMOUT2/copilot-adapter" ]; then
  pass "42b excluded-referent symlink payload: refused before reading/copying referent"
else
  flunk "42b excluded-referent symlink payload: wrong message or output exists"; cat "$TMP/symlink-excluded.log"
fi
ANCSRC="$(safe_dest "$TMP/symlink-ancestor-source")"; mkdir -p "$ANCSRC/roles" "$TMP/ancestor-referent"
printf 'ordinary role\n' > "$ANCSRC/roles/ordinary.md"
printf 'ancestor canary\n' > "$TMP/ancestor-referent/private.md"
ln -s "$TMP/ancestor-referent" "$ANCSRC/roles/linked"
SYMOUT3="$(safe_dest "$TMP/symlink-export-ancestor")"
if python3 "$SETUP" export "$SYMOUT3" --target "$ANCSRC" --profile "$SYMPROF" >"$TMP/symlink-ancestor.log" 2>&1; then
  flunk "42c symlink directory in payload: export succeeded"
elif grep -qi 'symlink' "$TMP/symlink-ancestor.log" && [ ! -e "$SYMOUT3/copilot-adapter" ]; then
  pass "42c symlink directory in payload: refused before traversing any referent"
else
  flunk "42c symlink directory in payload: wrong message or output exists"; cat "$TMP/symlink-ancestor.log"
fi

# --- 43. generated destinations are contained before any install write ------
GENT="$(safe_dest "$TMP/generated-symlink-target")"; GENOUT="$(safe_dest "$TMP/generated-outside")"
mkdir -p "$GENT" "$GENOUT"
ln -s "$GENOUT" "$GENT/.vscode"
if python3 "$SETUP" install --target "$GENT" --profile "$ADAPTER/examples/minimal" >"$TMP/generated-symlink.log" 2>&1; then
  flunk "43a generated .vscode symlink: install succeeded"
elif grep -qiE 'outside|resolve|generated' "$TMP/generated-symlink.log" \
     && [ -z "$(find "$GENOUT" -mindepth 1 -print -quit)" ] \
     && [ ! -e "$GENT/.github/copilot-instructions.md" ] \
     && [ ! -e "$GENT/copilot-adapter/.generated-inventory.json" ]; then
  pass "43a generated .vscode symlink: refused before any inside/outside write"
else
  flunk "43a generated .vscode symlink: outside or target bytes changed"; cat "$TMP/generated-symlink.log"
fi

TRAVT="$(safe_dest "$TMP/generated-traversal-target")"; mkdir -p "$TRAVT"
TRAVPROF="$TMP/generated-traversal-profile"; cp -r "$ADAPTER/examples/minimal" "$TRAVPROF"
sed -i 's/slug = "signed-records"/slug = "..\/..\/..\/escaped-generated"/' "$TRAVPROF/adapter.toml"
ESCAPED="$TMP/escaped-generated.instructions.md"
if python3 "$SETUP" install --target "$TRAVT" --profile "$TRAVPROF" >"$TMP/generated-traversal.log" 2>&1; then
  flunk "43b parent-segment instruction slug: install succeeded"
elif grep -qi 'slug' "$TMP/generated-traversal.log" \
     && [ ! -e "$ESCAPED" ] \
     && [ -z "$(find "$TRAVT" -mindepth 1 -print -quit)" ]; then
  pass "43b parent-segment instruction slug: refused before any write"
else
  flunk "43b parent-segment instruction slug: escaped or target bytes changed"; cat "$TMP/generated-traversal.log"
fi

# --- 44. confirmation uses strict YAML boolean semantics before install ------
CONFT="$(safe_dest "$TMP/confirmed-inline-target")"; mkdir -p "$CONFT"
if python3 "$SETUP" install --target "$CONFT" --profile "$ADAPTER/examples/minimal" >"$TMP/confirmed-seed.log" 2>&1; then
  sed -i 's/^environment_confirmed: false/environment_confirmed: true # SME reviewed/' "$CONFT/harness.yaml"
  printf '\n# local generated edit must survive refusal\n' >> "$CONFT/.github/copilot-instructions.md"
  cp "$CONFT/harness.yaml" "$TMP/confirmed-inline.before"
  cp "$CONFT/.github/copilot-instructions.md" "$TMP/confirmed-generated.before"
  if python3 "$SETUP" install --refresh-env --force --target "$CONFT" \
       --profile "$ADAPTER/examples/minimal" >"$TMP/confirmed-inline.log" 2>&1; then
    flunk "44a inline-comment true confirmation: refresh succeeded"
  elif cmp -s "$CONFT/harness.yaml" "$TMP/confirmed-inline.before" \
       && cmp -s "$CONFT/.github/copilot-instructions.md" "$TMP/confirmed-generated.before" \
       && grep -qi 'confirmed' "$TMP/confirmed-inline.log"; then
    pass "44a inline-comment true confirmation: whole install refused, bytes preserved"
  else
    flunk "44a inline-comment true confirmation: bytes changed or message unclear"; cat "$TMP/confirmed-inline.log"
  fi
else
  flunk "44a inline-comment true confirmation: fixture install failed"; cat "$TMP/confirmed-seed.log"
fi

for variant in quoted duplicate malformed flow tagged anchored quoted_key encoded_key malformed_root; do
  CT="$(safe_dest "$TMP/confirmation-$variant")"; mkdir -p "$CT"
  python3 "$SETUP" install --target "$CT" --profile "$ADAPTER/examples/minimal" >"$TMP/confirmation-$variant-seed.log" 2>&1 \
    || { flunk "44 $variant confirmation: fixture install failed"; continue; }
  case "$variant" in
    quoted) sed -i 's/^environment_confirmed: false/environment_confirmed: "true"/' "$CT/harness.yaml" ;;
    duplicate) printf '\nenvironment_confirmed: true\n' >> "$CT/harness.yaml" ;;
    malformed) sed -i 's/^environment_confirmed: false/environment_confirmed: [true/' "$CT/harness.yaml" ;;
    flow) sed -i 's/^environment_confirmed: false/{environment_confirmed: true}/' "$CT/harness.yaml" ;;
    tagged) sed -i 's/^environment_confirmed: false/environment_confirmed: !!bool true/' "$CT/harness.yaml" ;;
    anchored) sed -i 's/^environment_confirmed: false/environment_confirmed: \&confirmed true/' "$CT/harness.yaml" ;;
    quoted_key) sed -i 's/^environment_confirmed: false/"environment_confirmed": true/' "$CT/harness.yaml" ;;
    encoded_key) sed -i 's/^environment_confirmed: false/"environment_\\u0063onfirmed": true/' "$CT/harness.yaml" ;;
    malformed_root) sed -i 's/^environment_confirmed: false/{broken: [}/' "$CT/harness.yaml" ;;
  esac
  printf '\n# local generated edit must survive malformed confirmation\n' >> "$CT/.github/copilot-instructions.md"
  cp "$CT/harness.yaml" "$TMP/confirmation-$variant.before"
  cp "$CT/.github/copilot-instructions.md" "$TMP/confirmation-$variant-generated.before"
  if python3 "$SETUP" install --refresh-env --force --target "$CT" \
       --profile "$ADAPTER/examples/minimal" >"$TMP/confirmation-$variant.log" 2>&1; then
    flunk "44 $variant confirmation: refresh succeeded"
  elif cmp -s "$CT/harness.yaml" "$TMP/confirmation-$variant.before" \
       && cmp -s "$CT/.github/copilot-instructions.md" "$TMP/confirmation-$variant-generated.before" \
       && grep -qiE 'environment_confirmed|duplicate|boolean|malformed' "$TMP/confirmation-$variant.log"; then
    pass "44 $variant confirmation: refused before generated or YAML writes"
  else
    flunk "44 $variant confirmation: bytes changed or message unclear"; cat "$TMP/confirmation-$variant.log"
  fi
done

AMBIGT="$(safe_dest "$TMP/confirmation-normal-install")"; mkdir -p "$AMBIGT"
if python3 "$SETUP" install --target "$AMBIGT" --profile "$ADAPTER/examples/minimal" >"$TMP/ambiguous-normal-seed.log" 2>&1; then
  sed -i 's/^environment_confirmed: false/environment_confirmed: "false"/' "$AMBIGT/harness.yaml"
  printf '\n# local generated edit must survive ambiguous confirmation\n' >> "$AMBIGT/.github/copilot-instructions.md"
  cp "$AMBIGT/.github/copilot-instructions.md" "$TMP/ambiguous-normal-generated.before"
  if python3 "$SETUP" install --force --target "$AMBIGT" \
       --profile "$ADAPTER/examples/minimal" >"$TMP/ambiguous-normal.log" 2>&1; then
    flunk "44d ambiguous confirmation on normal install: install succeeded"
  elif cmp -s "$AMBIGT/.github/copilot-instructions.md" "$TMP/ambiguous-normal-generated.before" \
       && grep -qiE 'environment_confirmed|boolean' "$TMP/ambiguous-normal.log"; then
    pass "44d ambiguous confirmation on normal install: refused before generated writes"
  else
    flunk "44d ambiguous confirmation on normal install: bytes changed or message unclear"; cat "$TMP/ambiguous-normal.log"
  fi
else
  flunk "44d ambiguous confirmation on normal install: fixture install failed"; cat "$TMP/ambiguous-normal-seed.log"
fi

FALSECT="$(safe_dest "$TMP/confirmation-false-comment")"; mkdir -p "$FALSECT"
if python3 "$SETUP" install --target "$FALSECT" --profile "$ADAPTER/examples/minimal" >"$TMP/false-comment-seed.log" 2>&1; then
  sed -i 's/^environment_confirmed: false/environment_confirmed: false # not reviewed/' "$FALSECT/harness.yaml"
  if python3 "$SETUP" install --refresh-env --target "$FALSECT" \
       --profile "$ADAPTER/examples/minimal" >"$TMP/false-comment.log" 2>&1 \
       && grep -q '^environment_confirmed: false # not reviewed' "$FALSECT/harness.yaml"; then
    pass "44e inline-comment false confirmation: ordinary refresh remains allowed"
  else
    flunk "44e inline-comment false confirmation: refresh refused or changed flag"; cat "$TMP/false-comment.log"
  fi
else
  flunk "44e inline-comment false confirmation: fixture install failed"; cat "$TMP/false-comment-seed.log"
fi

if [ $fail = 0 ]; then echo "copilot-adapter tests: ALL PASS"; else echo "copilot-adapter tests: FAILURES"; fi
exit $fail
