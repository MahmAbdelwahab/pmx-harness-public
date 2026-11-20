#!/usr/bin/env bash
# Direct test of the PreToolUse guard .claude/hooks/protect_paths.py.
#
# Live tool interception cannot be triggered from inside an agent session, so
# we exercise the script the way Claude Code does: pipe a tool-call JSON on
# stdin and assert the exit code (0 = allow, 2 = block).
#
# A dedicated fixture repo tree is built under a temp dir and pointed at via
# TEST_REPO_ROOT (which the guard honours only when set). This keeps the test
# hermetic — it never depends on the real repo's unlock state or skill
# signatures.
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HOOK="$REPO_ROOT/.claude/hooks/protect_paths.py"

FIX="$(mktemp -d)"
trap 'rm -rf "$FIX"' EXIT

# --- Build the fixture tree -------------------------------------------------
mkdir -p "$FIX/gates/SYNTHETIC-GUARD"
mkdir -p "$FIX/pipeline" "$FIX/tools"
mkdir -p "$FIX/skills/signed-skill/reference" "$FIX/skills/open-skill/reference"

echo "signed record" > "$FIX/gates/SYNTHETIC-GUARD/base-model.md"
echo "compose" > "$FIX/pipeline/compose.R"
echo "gate" > "$FIX/tools/gate.R"
echo "approve" > "$FIX/tools/approve_gate.R"

# A signed skill: SKILL.md carries a real 'Reviewed by:' signature.
printf 'title\nReviewed by: MM\n' > "$FIX/skills/signed-skill/SKILL.md"
echo "method <- function() {}" > "$FIX/skills/signed-skill/reference/method.R"

# An unsigned skill: 'Reviewed by:' present but empty (template field).
printf 'title\nReviewed by:\n' > "$FIX/skills/open-skill/SKILL.md"
echo "draft <- function() {}" > "$FIX/skills/open-skill/reference/draft.R"

# Method manifests (schema v2): a CONFIRMED one (non-empty root reviewed_by) is
# protected; an empty-reviewed_by one is still in the authoring state and stays
# editable.
mkdir -p "$FIX/skills/approved-mani" "$FIX/skills/draft-mani"
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\n' \
  > "$FIX/skills/approved-mani/MANIFEST.yaml"
# Every shipped manifest declares a `units:` mapping, and since fix round 2 of
# batch 11 one that does not is treated as unparseable (fail closed), so the
# draft fixture carries a unit too. What makes it a DRAFT here is the empty
# root reviewed_by, which is what the cases below test.
printf 'skill: draft-mani\nreviewed_by: ""\nreviewed_at: ""\nunits:\n  alpha:\n    status: draft\n    reference_path: skills/draft-mani/reference/loose.R\n    entrypoints: [loose]\n    sha256: sha256:00\n    evidence: SKILL.md\n    reason: "draft"\n' \
  > "$FIX/skills/draft-mani/MANIFEST.yaml"

# Reference code under a skill whose SKILL.md carries NO signature but whose
# MANIFEST.yaml is confirmed (round 7, finding 18): the manifest's digests are
# the active self-gate, so the reference file it binds is protected exactly as
# under a legacy signature. The draft-manifest skill's reference stays editable.
mkdir -p "$FIX/skills/approved-mani/reference" "$FIX/skills/draft-mani/reference"
printf 'title\nReviewed by:\n' > "$FIX/skills/approved-mani/SKILL.md"
echo "bound <- function() {}" > "$FIX/skills/approved-mani/reference/bound.R"
printf 'title\nReviewed by:\n' > "$FIX/skills/draft-mani/SKILL.md"
echo "loose <- function() {}" > "$FIX/skills/draft-mani/reference/loose.R"

# A tools/*.R helper named under a manifest's depends: (integrity batch 11,
# R10-P1-5). Such a helper is sourced INTO a reference's sealed validator
# object, so it is on that object's lookup chain and its digest is bound by the
# manifest -- but the guard's fixed core set did not cover it. An unreferenced
# tools/*.R helper stays editable.
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    status: approved\n    reference_path: skills/approved-mani/reference/bound.R\n    entrypoints: [bound]\n    sha256: sha256:00\n    evidence: SKILL.md\n    reason: "signed"\n    depends:\n      - path: tools/record_selection.R\n        sha256: sha256:00\n' \
  > "$FIX/skills/approved-mani/MANIFEST.yaml"
echo "harness_kept_record <- function(x) TRUE" > "$FIX/tools/record_selection.R"
echo "unbound <- function() {}" > "$FIX/tools/scratch_helper.R"

PASS=0
FAIL=0

# run_case <name> <expected_exit> <json>
run_case() {
  local name="$1" expected="$2" json="$3"
  local out actual
  out="$(printf '%s' "$json" | TEST_REPO_ROOT="$FIX" python3 "$HOOK" 2>&1)"
  actual=$?
  if [ "$actual" -eq "$expected" ]; then
    printf 'PASS  %-55s exit=%s\n' "$name" "$actual"
    PASS=$((PASS + 1))
  else
    printf 'FAIL  %-55s expected=%s got=%s\n' "$name" "$expected" "$actual"
    [ -n "$out" ] && printf '        stderr: %s\n' "$out"
    FAIL=$((FAIL + 1))
  fi
}

json_for() { printf '{"tool_name":"Edit","tool_input":{"file_path":"%s"}}' "$1"; }

rm -f "$FIX/.harness-unlock"

# 1. gates/ edit -> blocked (2)
run_case "gates edit blocked" 2 "$(json_for 'gates/SYNTHETIC-GUARD/base-model.md')"

# 2. compose.R without unlock -> blocked (2)
run_case "harness-core without unlock blocked" 2 "$(json_for 'pipeline/compose.R')"
# 2b. approve_gate.R is gate enforcement (ledger, release lock) and is core too
#     (Copilot kit v4 sync referral, 2026-09-05).
run_case "approve_gate.R (harness core) without unlock blocked" 2 "$(json_for 'tools/approve_gate.R')"

# 3. compose.R WITH fresh unlock -> allowed (0)
touch "$FIX/.harness-unlock"
run_case "harness-core with fresh unlock allowed" 0 "$(json_for 'pipeline/compose.R')"

# 3b. gates/ still blocked even with unlock (category 1 is absolute)
run_case "gates blocked even with unlock" 2 "$(json_for 'gates/SYNTHETIC-GUARD/base-model.md')"
rm -f "$FIX/.harness-unlock"

# 3c. stale unlock (older than 8h) -> harness-core blocked again
touch -d '9 hours ago' "$FIX/.harness-unlock" 2>/dev/null && \
  run_case "harness-core with stale unlock blocked" 2 "$(json_for 'pipeline/compose.R')"
rm -f "$FIX/.harness-unlock"

# 4. unsigned skill reference edit -> allowed (0)
run_case "unsigned skill reference allowed" 0 "$(json_for 'skills/open-skill/reference/draft.R')"

# 5. signed skill reference edit -> blocked (2)
run_case "signed skill reference blocked" 2 "$(json_for 'skills/signed-skill/reference/method.R')"

# 5b. signed skill reference WITH unlock -> allowed (0)
touch "$FIX/.harness-unlock"
run_case "signed skill reference with unlock allowed" 0 "$(json_for 'skills/signed-skill/reference/method.R')"
rm -f "$FIX/.harness-unlock"

# 5c. approved method manifest -> blocked (2)
run_case "approved manifest blocked" 2 "$(json_for 'skills/approved-mani/MANIFEST.yaml')"

# 5d. approved manifest WITH unlock -> allowed (0)
touch "$FIX/.harness-unlock"
run_case "approved manifest with unlock allowed" 0 "$(json_for 'skills/approved-mani/MANIFEST.yaml')"
rm -f "$FIX/.harness-unlock"

# 5e. empty-approver (draft) manifest -> allowed (0)
run_case "draft manifest allowed" 0 "$(json_for 'skills/draft-mani/MANIFEST.yaml')"

# 5f. reference code bound by a CONFIRMED manifest (no SKILL.md signature) -> blocked (2)
run_case "manifest-bound reference blocked" 2 "$(json_for 'skills/approved-mani/reference/bound.R')"

# 5g. same, WITH unlock -> allowed (0)
touch "$FIX/.harness-unlock"
run_case "manifest-bound reference with unlock allowed" 0 "$(json_for 'skills/approved-mani/reference/bound.R')"
rm -f "$FIX/.harness-unlock"

# 5h. reference code under a draft manifest and an unsigned SKILL.md -> allowed (0)
run_case "draft-manifest reference allowed" 0 "$(json_for 'skills/draft-mani/reference/loose.R')"

# 6. malformed JSON -> allowed (0), fail-open
run_case "malformed JSON fails open" 0 "not-json-at-all"

# 7. path traversal into a protected file -> blocked (2)
run_case "path traversal gates/../tools/gate.R blocked" 2 "$(json_for 'gates/../tools/gate.R')"

# 8. ordinary file -> allowed (0)
run_case "ordinary file allowed" 0 "$(json_for 'pipeline/tasks/eda.R')"

# --- Runtime parity: payload shapes other than Claude Code's -----------------
# GitHub Copilot in VS Code sends the same PreToolUse event with camelCase
# tool_input keys (filePath, dirPath, replacements[].filePath, files[],
# apply_patch `input` text) and ignores hook matchers, so the guard sees every
# tool call. Copilot CLI may carry the input under `toolArgs`. These fixtures
# are the copilot-parity POC set (2026-09-02); the VS Code tool names and key
# shapes are verified against the vscode-copilot-chat package.json tool schemas
# and toolNames.ts (main, 5863f5a). A byte-exact live capture is still pending
# (docs/COPILOT-RUNBOOK.md).
mkdir -p "$FIX/docs"; echo "notes" > "$FIX/docs/notes.md"

# 9. Claude Write of an ordinary file -> allowed (0)
run_case "claude Write docs allowed" 0 \
  "$(printf '{"hook_event_name":"PreToolUse","tool_name":"Write","tool_input":{"file_path":"%s","content":"x"}}' "$FIX/docs/notes.md")"

# 10. VS Code replace_string_in_file on a gate record (absolute filePath) -> blocked (2)
run_case "vscode replace_string_in_file gates blocked" 2 \
  "$(printf '{"hook_event_name":"PreToolUse","tool_name":"replace_string_in_file","tool_input":{"filePath":"%s","oldString":"a","newString":"b"},"tool_use_id":"t1"}' "$FIX/gates/SYNTHETIC-GUARD/base-model.md")"

# 11. VS Code create_file under gates/ -> blocked (2)
run_case "vscode create_file gates blocked" 2 \
  "$(printf '{"hook_event_name":"PreToolUse","tool_name":"create_file","tool_input":{"filePath":"%s","content":"x"}}' "$FIX/gates/SYNTHETIC-GUARD/new.md")"

# 12. VS Code create_file under docs/ -> allowed (0)
run_case "vscode create_file docs allowed" 0 \
  "$(printf '{"hook_event_name":"PreToolUse","tool_name":"create_file","tool_input":{"filePath":"%s","content":"x"}}' "$FIX/docs/new.md")"

# 13. VS Code multi_replace_string_in_file where one replacement hits gates/ -> blocked (2)
run_case "vscode multi_replace incl. gates blocked" 2 \
  "$(printf '{"tool_name":"multi_replace_string_in_file","tool_input":{"replacements":[{"filePath":"%s","oldString":"a","newString":"b"},{"filePath":"%s","oldString":"a","newString":"b"}]}}' "$FIX/docs/notes.md" "$FIX/gates/SYNTHETIC-GUARD/base-model.md")"

# 14. VS Code apply_patch touching tools/gate.R -> blocked (2)
run_case "vscode apply_patch tools/gate.R blocked" 2 \
  '{"tool_name":"apply_patch","tool_input":{"input":"*** Begin Patch\n*** Update File: tools/gate.R\n@@\n-a\n+b\n*** End Patch"}}'

# 15. VS Code editFiles files[] naming a gate record -> blocked (2)
run_case "vscode editFiles files[] gates blocked" 2 \
  '{"tool_name":"editFiles","tool_input":{"files":["gates/SYNTHETIC-GUARD/base-model.md"]}}'

# 16. VS Code create_file with a repo-relative gates path -> blocked (2)
run_case "vscode create_file relative gates blocked" 2 \
  '{"tool_name":"create_file","tool_input":{"filePath":"gates/SYNTHETIC-GUARD/other.md","content":"x"}}'

# 17. VS Code run_in_terminal -> allowed (0); terminal writes are not path-shaped
run_case "vscode run_in_terminal allowed" 0 \
  '{"tool_name":"run_in_terminal","tool_input":{"command":"ls"}}'

# 18. VS Code create_file outside the repo -> allowed (0)
run_case "vscode create_file outside repo allowed" 0 \
  '{"tool_name":"create_file","tool_input":{"filePath":"/etc/hosts","content":"x"}}'

# 19. Copilot CLI shape: input under toolArgs -> blocked (2)
run_case "copilot-cli toolArgs gates blocked" 2 \
  '{"toolName":"create_file","toolArgs":{"filePath":"gates/SYNTHETIC-GUARD/x.md","content":"x"}}'

# 20. file:// URI shape -> blocked (2)
run_case "vscode uri file:// gates blocked" 2 \
  "$(printf '{"tool_name":"editFiles","tool_input":{"uri":"file://%s"}}' "$FIX/gates/SYNTHETIC-GUARD/base-model.md")"

# 21. Read-only tools carry a path too (VS Code ignores matchers): must stay allowed.
run_case "vscode read_file gates allowed" 0 \
  '{"tool_name":"read_file","tool_input":{"filePath":"gates/SYNTHETIC-GUARD/base-model.md","startLine":1,"endLine":5}}'
run_case "claude Read gates allowed" 0 \
  '{"tool_name":"Read","tool_input":{"file_path":"gates/SYNTHETIC-GUARD/base-model.md"}}'

# --- Cloud / CLI runtime shapes and read-tool names -------------------------
# GitHub's hooks reference (fetched 2026-09-02) documents two payload dialects
# for the Copilot cloud agent and the Copilot CLI:
#   camelCase event `preToolUse`  -> {toolName, toolArgs} with runtime tool
#     names (create, edit, view, glob, grep, rg) and toolArgs that may arrive
#     as a JSON-encoded STRING;
#   PascalCase event `PreToolUse` -> {tool_name, tool_input} with the Claude
#     tool names (Write, Edit, Read, ...).
# The *argument key names* of the cloud create/edit/view tools are not
# documented; `path` is inferred from the `str_replace_editor` alias (Anthropic
# text-editor schema) and stays labelled as an inference until the live capture
# in docs/COPILOT-RUNBOOK.md confirms it. The VS Code read-tool names below are
# verified against vscode-copilot-chat `toolNames.ts` (main, 5863f5a).

# 22. Cloud create with toolArgs as a JSON STRING, path into gates/ -> blocked (2)
#     (key name `path` inferred; confirm per docs/COPILOT-RUNBOOK.md)
run_case "cloud create string toolArgs gates blocked" 2 \
  '{"toolName":"create","toolArgs":"{\"path\":\"gates/SYNTHETIC-GUARD/x.md\",\"file_text\":\"x\"}"}'

# 23. Cloud view (read) of a gate record -> allowed (0)
run_case "cloud view gates allowed" 0 \
  '{"toolName":"view","toolArgs":{"path":"gates/SYNTHETIC-GUARD/base-model.md"}}'

# 24. PascalCase cloud payload with the Claude tool name Read -> allowed (0)
run_case "cloud PascalCase Read gates allowed" 0 \
  '{"hook_event_name":"PreToolUse","tool_name":"Read","tool_input":{"path":"gates/SYNTHETIC-GUARD/base-model.md"}}'

# 25. VS Code notebook summary (real ToolName is copilot_getNotebookSummary) -> allowed (0)
run_case "vscode copilot_getNotebookSummary gates allowed" 0 \
  '{"tool_name":"copilot_getNotebookSummary","tool_input":{"filePath":"gates/SYNTHETIC-GUARD/x.ipynb"}}'

# 26. VS Code view_image of a figure under gates/ -> allowed (0)
run_case "vscode view_image gates allowed" 0 \
  '{"tool_name":"view_image","tool_input":{"filePath":"gates/SYNTHETIC-GUARD/a.png"}}'

# 27. VS Code get_errors with filePaths[] under gates/ -> allowed (0)
run_case "vscode get_errors filePaths gates allowed" 0 \
  '{"tool_name":"get_errors","tool_input":{"filePaths":["gates/SYNTHETIC-GUARD/base-model.md"]}}'

# 28. Cloud edit (write) with an object toolArgs into gates/ -> blocked (2)
#     (key name `path` inferred; confirm per docs/COPILOT-RUNBOOK.md)
run_case "cloud edit object toolArgs gates blocked" 2 \
  '{"toolName":"edit","toolArgs":{"path":"gates/SYNTHETIC-GUARD/base-model.md","old_str":"a","new_str":"b"}}'

# --- Malformed arguments for a RECOGNISED writer (round 6, finding 15) ------
# The guard used to turn unparseable arguments into {} and allow the call, and
# a JSON-decoded raw patch STRING yielded no paths and was likewise allowed.
# For a tool whose name is in WRITE_TOOLS both are now DENIED: a recognised
# mutation tool whose target cannot be resolved is refused, not waved through.
# Unknown tool names keep the fail-open policy (this hook is defence in depth;
# the R/CI layer is authoritative).

# 29. Recognised writer, arguments are an unparseable string -> denied (2)
run_case "known writer unparseable toolArgs denied" 2 \
  '{"toolName":"create_file","toolArgs":"not-json-at-all"}'

# 30. Recognised writer, arguments decode to a RAW PATCH STRING naming a gate
#     record -> the patch headers are scanned directly -> denied (2)
run_case "raw patch string toolArgs gates denied" 2 \
  '{"toolName":"apply_patch","toolArgs":"\"*** Begin Patch\\n*** Update File: gates/SYNTHETIC-GUARD/base-model.md\\n@@\\n-a\\n+b\\n*** End Patch\""}'

# 31. Recognised writer, DOUBLE-ENCODED toolArgs (a JSON string whose content is
#     itself a JSON-encoded object) -> decoded to bounded depth -> denied (2)
run_case "double-encoded toolArgs gates blocked" 2 \
  '{"toolName":"create","toolArgs":"\"{\\\"path\\\":\\\"gates/SYNTHETIC-GUARD/x.md\\\",\\\"file_text\\\":\\\"x\\\"}\""}'

# 32. Recognised writer with well-formed arguments but NO resolvable target
#     -> denied (2); a writer that names nothing is not a safe no-op.
run_case "known writer path-less arguments denied" 2 \
  '{"tool_name":"create_file","tool_input":{"content":"x"}}'

# 33. Read-name collision, documented: the read skip list is keyed on the tool
#     NAME, so a hypothetical write-capable tool sharing a read name would be
#     skipped. This case pins that behaviour so the collision is visible rather
#     than latent. The skip list is a CONVENIENCE for read traffic (VS Code
#     ignores hook matchers), not a security boundary — the boundary is the R
#     and CI layer. Adding a name here is therefore an integrity decision.
run_case "read-name collision skips path inspection (documented)" 0 \
  '{"toolName":"view","toolArgs":{"path":"gates/SYNTHETIC-GUARD/base-model.md","file_text":"x"}}'

# 33b. editFiles is a documented VS Code writer (hooks docs; fixtures 15 and 20
#      already treat it as one) and must be in WRITE_TOOLS: path-less or
#      unparseable arguments under that name are denied, not waved through.
run_case "editFiles path-less arguments denied" 2 \
  '{"tool_name":"editFiles","tool_input":{"content":"x"}}'
run_case "editFiles unparseable toolArgs denied" 2 \
  '{"toolName":"editFiles","toolArgs":"not-json-at-all"}'

# 34. UNKNOWN tool with unparseable arguments -> still fail-open (0)
run_case "unknown tool unparseable args fails open" 0 \
  '{"toolName":"some_future_tool","toolArgs":"not-json-at-all"}'

# 35. A tools/*.R helper bound by a manifest's depends: is unlock-gated exactly
#     like the harness core (integrity batch 11, R10-P1-5). The set is derived
#     from the manifests at hook time, so it follows them rather than a list.
rm -f "$FIX/.harness-unlock"
run_case "manifest-declared tools helper without unlock blocked" 2 \
  "$(json_for 'tools/record_selection.R')"
touch "$FIX/.harness-unlock"
run_case "manifest-declared tools helper with fresh unlock allowed" 0 \
  "$(json_for 'tools/record_selection.R')"
rm -f "$FIX/.harness-unlock"
run_case "tools helper named by no manifest stays editable" 0 \
  "$(json_for 'tools/scratch_helper.R')"

# 36. Category 2b must FAIL CLOSED (fix round 1 of batch 11, review finding
#     11.6). Every failure mode of the derivation used to open the gate: an
#     unreadable manifest was skipped silently, a manifest whose contents no
#     longer parse yielded an empty set, and a depends: path written with `..`
#     failed the literal `tools/` prefix test. In each case the helper dropped
#     out of the protected set and became freely editable with no unlock. A
#     guard whose whole design principle is fail-closed does not fail open on an
#     unreadable input: an unreadable or unparseable manifest now blocks EVERY
#     tools/*.R write and names the manifest.
#
# The fixtures below mutate the FIXTURE tree only ($FIX, a mktemp -d), never the
# live repository, and each restores the manifest it touched.
MANI="$FIX/skills/approved-mani/MANIFEST.yaml"
cp "$MANI" "$FIX/approved-mani.bak"

chmod 000 "$MANI"
run_case "unreadable manifest blocks every tools/*.R write (fail closed)" 2 \
  "$(json_for 'tools/scratch_helper.R')"
run_case "unreadable manifest still blocks the declared helper" 2 \
  "$(json_for 'tools/record_selection.R')"
chmod 644 "$MANI"

printf 'skill: approved-mani\n\x00\x00 not: [ yaml at all\n' > "$MANI"
run_case "garbage manifest with no resolvable depends blocks tools/*.R writes" 2 \
  "$(json_for 'tools/scratch_helper.R')"
cp "$FIX/approved-mani.bak" "$MANI"

run_case "restored manifest lets an undeclared helper stay editable" 0 \
  "$(json_for 'tools/scratch_helper.R')"

# A depends: path written with a `..` segment normalises to tools/... and must
# be protected, not dropped for failing a literal prefix test.
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: skills/../tools/record_selection.R\n        sha256: sha256:00\n' \
  > "$MANI"
run_case "a depends: path with a .. segment is normalised and still protected" 2 \
  "$(json_for 'tools/record_selection.R')"
cp "$FIX/approved-mani.bak" "$MANI"

# 37. Fix round 2 of batch 11 (independent review, finding F3-a). normpath()
#     closed the `..` case above but not an ABSOLUTE depends: path: the
#     normalised string starts with `/`, fails the `tools/` prefix test, and the
#     helper dropped out of the protected set with no unlock and no message --
#     the same fail-open one character different. An absolute path under the
#     repository is now mapped to its repository-relative form; anything that
#     still does not normalise to a repository-relative path is the fail-closed
#     signal, exactly like an unreadable manifest.
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: %s/tools/record_selection.R\n        sha256: sha256:00\n' \
  "$FIX" > "$MANI"
run_case "an absolute depends: path under the repository is still protected" 2 \
  "$(json_for 'tools/record_selection.R')"
cp "$FIX/approved-mani.bak" "$MANI"

printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: /etc/elsewhere/tools/record_selection.R\n        sha256: sha256:00\n' \
  > "$MANI"
run_case "a depends: path outside the repository blocks every tools/*.R write" 2 \
  "$(json_for 'tools/scratch_helper.R')"
cp "$FIX/approved-mani.bak" "$MANI"

# 38. Fix round 2 of batch 11 (finding F3-b). A manifest that is valid YAML but
#     declares nothing -- an empty mapping, or a mapping with no `units:` key --
#     removed the protection silently. A manifest that yields no unit at all is
#     not a manifest; treat it as unparseable and fail closed.
printf 'skill: approved-mani\nunits: {}\n' > "$MANI"
run_case "a manifest with an empty units: mapping fails closed" 2 \
  "$(json_for 'tools/scratch_helper.R')"
cp "$FIX/approved-mani.bak" "$MANI"

printf 'skill: approved-mani\nreviewed_by: "MM"\n' > "$MANI"
run_case "a manifest with no units: key fails closed" 2 \
  "$(json_for 'tools/scratch_helper.R')"
cp "$FIX/approved-mani.bak" "$MANI"

run_case "the restored manifest lets an undeclared helper stay editable again" 0 \
  "$(json_for 'tools/scratch_helper.R')"

# 39a. apply_patch move headers name two mutation endpoints. Both endpoints
#      must be classified before a move is allowed.
run_case "apply_patch move into gates is blocked by destination" 2 \
  '{"tool_name":"apply_patch","tool_input":{"input":"*** Begin Patch\n*** Update File: docs/draft.md\n*** Move to: gates/SYNTHETIC-GUARD/draft.md\n@@\n-old\n+new\n*** End Patch"}}'
run_case "apply_patch move out of gates is blocked by source" 2 \
  '{"tool_name":"apply_patch","tool_input":{"input":"*** Begin Patch\n*** Update File: gates/SYNTHETIC-GUARD/base-model.md\n*** Move to: docs/base-model.md\n@@\n-old\n+new\n*** End Patch"}}'
run_case "apply_patch ordinary move remains allowed" 0 \
  '{"tool_name":"apply_patch","tool_input":{"input":"*** Begin Patch\n*** Update File: docs/draft.md\n*** Move to: docs/final.md\n@@\n-old\n+new\n*** End Patch"}}'

# 39. G6 (SME decision, fix round 2). main()'s catch-all used to return 0 for
#     every internal error, so a guard bug opened even the categories the guard
#     exists to hold. It now fails CLOSED when the write target is under a
#     protected category and stays fail-open everywhere else, so a hook bug can
#     never freeze ordinary work. PROTECT_PATHS_FORCE_FAULT raises inside
#     _decision() and is honoured only alongside TEST_REPO_ROOT.
export PROTECT_PATHS_FORCE_FAULT=1
rm -f "$FIX/.harness-unlock"
run_case "internal fault on a gate record fails closed" 2 \
  "$(json_for 'gates/SYNTHETIC-EDA/eda.md')"
run_case "internal fault on a harness-core file fails closed" 2 \
  "$(json_for 'tools/gate.R')"
run_case "internal fault on a skill reference file fails closed" 2 \
  "$(json_for 'skills/approved-mani/reference/thing.R')"
run_case "internal fault on a skill manifest fails closed" 2 \
  "$(json_for 'skills/approved-mani/MANIFEST.yaml')"
run_case "internal fault on a tools/*.R helper fails closed" 2 \
  "$(json_for 'tools/scratch_helper.R')"
run_case "internal fault on an ordinary document stays fail-open" 0 \
  "$(json_for 'docs/SME-GUIDE.md')"
run_case "internal fault with no determinable path fails closed" 2 \
  '{"tool_name":"Write","tool_input":{}}'
run_case "internal fault sees later protected target" 2 \
  '{"tool_name":"multi_replace_string_in_file","tool_input":{"replacements":[{"filePath":"docs/ordinary.md"},{"filePath":"gates/SYNTHETIC-GUARD/base-model.md"}]}}'
run_case "internal fault sees earlier protected target" 2 \
  '{"tool_name":"multi_replace_string_in_file","tool_input":{"replacements":[{"filePath":"gates/SYNTHETIC-GUARD/base-model.md"},{"filePath":"docs/ordinary.md"}]}}'
unset PROTECT_PATHS_FORCE_FAULT

# 40. Integrity batch 12 (Codex round 11, P1-2). Without PyYAML the guard used
#     to fall back to a regex over `- path:` lines and a structural "looks like
#     YAML" check; a manifest truncated to `skill: x` + a bare `units:` passed
#     both and every manifest-bound tools/*.R helper became editable with no
#     unlock. A partial recogniser must not authorise writes: without PyYAML
#     the dependency-set derivation FAILS CLOSED and every tools/*.R write is
#     blocked with a message naming the missing module. PROTECT_PATHS_NO_YAML
#     simulates the missing module and is honoured only alongside
#     TEST_REPO_ROOT, like the fault hook.
export PROTECT_PATHS_NO_YAML=1
run_case "without PyYAML an undeclared tools/*.R write fails closed" 2 \
  "$(json_for 'tools/scratch_helper.R')"
run_case "without PyYAML a declared tools/*.R write fails closed" 2 \
  "$(json_for 'tools/record_selection.R')"
out_noyaml="$(printf '%s' "$(json_for 'tools/scratch_helper.R')" | TEST_REPO_ROOT="$FIX" PROTECT_PATHS_NO_YAML=1 python3 "$HOOK" 2>&1)"
if printf '%s' "$out_noyaml" | grep -qi 'pyyaml\|yaml module'; then
  printf 'PASS  %-55s\n' "the no-PyYAML refusal names the missing module"; PASS=$((PASS + 1))
else
  printf 'FAIL  %-55s got=%s\n' "the no-PyYAML refusal names the missing module" "$out_noyaml"; FAIL=$((FAIL + 1))
fi
run_case "without PyYAML an ordinary document stays editable" 0 \
  "$(json_for 'docs/SME-GUIDE.md')"
run_case "without PyYAML a gate record is still blocked" 2 \
  "$(json_for 'gates/SYNTHETIC-GUARD/base-model.md')"
# The truncated manifest that opened the gate under the regex fallback.
printf 'skill: approved-mani\nunits:\n' > "$MANI"
run_case "without PyYAML a manifest truncated to a bare units: fails closed" 2 \
  "$(json_for 'tools/scratch_helper.R')"
cp "$FIX/approved-mani.bak" "$MANI"
unset PROTECT_PATHS_NO_YAML
# With PyYAML the same truncated manifest fails closed too (units: is null).
printf 'skill: approved-mani\nunits:\n' > "$MANI"
run_case "with PyYAML a manifest truncated to a bare units: fails closed" 2 \
  "$(json_for 'tools/scratch_helper.R')"
cp "$FIX/approved-mani.bak" "$MANI"

# 41. Integrity batch 12 (Codex round 11, P3-1). The regex excluded whitespace
#     from the captured path, so `path: "tools/site helper.R"` -- a valid
#     dependency to the manifest reader, which merely unquotes the value --
#     matched nothing and the helper was editable without an unlock.
#     Dependencies are now derived from the parsed YAML, quotes and spaces
#     included.
echo "site_helper <- function() {}" > "$FIX/tools/site helper.R"
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: "tools/site helper.R"\n        sha256: sha256:00\n' \
  > "$MANI"
run_case "a double-quoted depends: path with a space is protected" 2 \
  "$(json_for 'tools/site helper.R')"
printf "skill: approved-mani\nreviewed_by: \"MM\"\nreviewed_at: \"2026-09-01\"\nunits:\n  alpha:\n    depends:\n      - path: 'tools/site helper.R'\n        sha256: sha256:00\n" \
  > "$MANI"
run_case "a single-quoted depends: path with a space is protected" 2 \
  "$(json_for 'tools/site helper.R')"
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: tools/record_selection.R\n        sha256: sha256:00\n      - sha256: sha256:00\n        path: "tools/site helper.R"\n' \
  > "$MANI"
run_case "a depends: entry with path written after sha256 is protected" 2 \
  "$(json_for 'tools/site helper.R')"
run_case "... and the first entry of the same list is still protected" 2 \
  "$(json_for 'tools/record_selection.R')"
# A depends: entry whose path is not a string cannot be represented exactly:
# fail closed rather than guess.
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: [tools/record_selection.R]\n        sha256: sha256:00\n' \
  > "$MANI"
run_case "a depends: path that is not a scalar string fails closed" 2 \
  "$(json_for 'tools/scratch_helper.R')"
cp "$FIX/approved-mani.bak" "$MANI"
run_case "the restored manifest lets the undeclared helper stay editable (batch 12)" 0 \
  "$(json_for 'tools/scratch_helper.R')"

# 41. Integrity batch 13 (Codex round 12, P2-1). PyYAML's safe_load accepts a
#     duplicate mapping key with last-value-wins semantics, while the harness
#     reader refuses it; a unit with a first `depends:` naming a spaced helper
#     and a second `depends: []` made PyYAML keep the empty list, so the helper
#     dropped out of protection. The guard now parses with a duplicate-key-
#     refusing loader and checks the manifest's schema (root fields, units
#     mapping, depends entries of {path, sha256} scalars) as read_method_manifest
#     does; any violation fails closed for tools/*.R writes, naming the manifest
#     and the violation.
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: "tools/site helper.R"\n        sha256: sha256:00\n    depends: []\n' \
  > "$MANI"
run_case "a unit with a duplicate depends: key fails closed for an undeclared helper (batch 13)" 2 \
  "$(json_for 'tools/scratch_helper.R')"
run_case "... and the spaced helper named by the first depends: is protected" 2 \
  "$(json_for 'tools/site helper.R')"
out_dup="$(printf '%s' "$(json_for 'tools/scratch_helper.R')" | TEST_REPO_ROOT="$FIX" python3 "$HOOK" 2>&1 >/dev/null || true)"
if printf '%s' "$out_dup" | grep -q 'approved-mani/MANIFEST.yaml' && printf '%s' "$out_dup" | grep -qi 'duplicate'; then
  printf 'PASS  %-55s\n' "the duplicate-key refusal names the manifest and the violation"; PASS=$((PASS + 1))
else
  printf 'FAIL  %-55s got=%s\n' "the duplicate-key refusal names the manifest and the violation" "$out_dup"; FAIL=$((FAIL + 1))
fi
printf 'skill: approved-mani\nskill: other\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: tools/record_selection.R\n        sha256: sha256:00\n' \
  > "$MANI"
run_case "a duplicate root key fails closed (batch 13)" 2 "$(json_for 'tools/scratch_helper.R')"
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nextra: 1\nunits:\n  alpha:\n    depends:\n      - path: tools/record_selection.R\n        sha256: sha256:00\n' \
  > "$MANI"
run_case "an unknown root field fails closed (schema mirrors read_method_manifest)" 2 "$(json_for 'tools/scratch_helper.R')"
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: tools/record_selection.R\n        sha256: sha256:00\n        extra: 1\n' \
  > "$MANI"
run_case "a depends: entry with a field other than path/sha256 fails closed" 2 "$(json_for 'tools/scratch_helper.R')"
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: tools/record_selection.R\n        sha256: [a, b]\n' \
  > "$MANI"
run_case "a depends: sha256 that is not a scalar fails closed" 2 "$(json_for 'tools/scratch_helper.R')"
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    depends:\n      - path: [tools/record_selection.R]\n        sha256: sha256:00\n' \
  > "$MANI"
run_case "a depends: path that is not a scalar string fails closed (batch 12, re-pinned)" 2 \
  "$(json_for 'tools/scratch_helper.R')"
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits: []\n' > "$MANI"
run_case "a units: that is not a mapping fails closed (batch 12, re-pinned)" 2 "$(json_for 'tools/scratch_helper.R')"
cp "$FIX/approved-mani.bak" "$MANI"
run_case "the restored manifest lets the undeclared helper stay editable (batch 13)" 0 \
  "$(json_for 'tools/scratch_helper.R')"

# 42. tools/load_reference.R (the trusted loader, integrity batch 13) is harness core.
echo "loader" > "$FIX/tools/load_reference.R"
run_case "load_reference.R (harness core) without unlock blocked" 2 "$(json_for 'tools/load_reference.R')"
touch "$FIX/.harness-unlock"
run_case "load_reference.R with fresh unlock allowed" 0 "$(json_for 'tools/load_reference.R')"
rm -f "$FIX/.harness-unlock"

# 42b. tools/ci_check.R is harness core (integrity batch 16; Codex round 15
#      R15-P2-3, SME decision D16-3). It carries the per-expression allowlist of
#      unresolved source targets and the batch-battery registration inventory:
#      one added allowlist line authorises an arbitrary computed load, one
#      deleted registration removes a battery from CI. The exception policy is
#      now under the same SME-controlled protection as the tools it enforces on.
echo "ci" > "$FIX/tools/ci_check.R"
run_case "ci_check.R (harness core) without unlock blocked" 2 "$(json_for 'tools/ci_check.R')"
touch "$FIX/.harness-unlock"
run_case "ci_check.R with fresh unlock allowed" 0 "$(json_for 'tools/ci_check.R')"
rm -f "$FIX/.harness-unlock"

# 43. Integrity batch 13 fix round 1 (R13-P1-C). write_method_manifest() emits
#     `generated_at:` as an UNQUOTED ISO-8601 timestamp, which PyYAML parses to
#     a datetime, not a str. The batch-12 schema check accepted only
#     (str, int, float), so every LIVE manifest failed the schema, the guard
#     fell closed on it, and EVERY tools/*.R write was blocked with a
#     misleading reason. The schema now accepts datetime/date/bool as scalars.
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\ngenerated_at: 2026-09-05T08:42:35Z\nunits:\n  alpha:\n    status: approved\n    reference_path: skills/approved-mani/reference/bound.R\n    entrypoints: [bound]\n    sha256: sha256:00\n    evidence: SKILL.md\n    reason: "signed"\n    depends:\n      - path: tools/record_selection.R\n        sha256: sha256:00\n' \
  > "$MANI"
run_case "an unquoted datetime generated_at does not fail the schema: an undeclared helper stays editable" 0 \
  "$(json_for 'tools/scratch_helper.R')"
run_case "... and the declared helper under that manifest is still BLOCKED" 2 \
  "$(json_for 'tools/record_selection.R')"
cp "$FIX/approved-mani.bak" "$MANI"

# 43b. The guard's schema mirrors read_method_manifest for missing required unit
#      fields (R13-P3-F: MANIFEST_UNIT_REQUIRED was defined and never consulted).
printf 'skill: approved-mani\nreviewed_by: "MM"\nreviewed_at: "2026-09-01"\nunits:\n  alpha:\n    status: approved\n    depends:\n      - path: tools/record_selection.R\n        sha256: sha256:00\n' \
  > "$MANI"
run_case "a unit missing required fields (no reference_path/sha256/...) fails closed" 2 \
  "$(json_for 'tools/scratch_helper.R')"
cp "$FIX/approved-mani.bak" "$MANI"

# 43c. The guard run against the REPOSITORY'S OWN ten manifests, copied verbatim:
#      an undeclared tools/*.R write is ALLOWED and the declared
#      tools/record_selection.R write is BLOCKED. Red before this fix round
#      (every live manifest's unquoted generated_at failed the schema).
FIX_LIVE="$(mktemp -d)"
mkdir -p "$FIX_LIVE/skills" "$FIX_LIVE/tools"
for m in "$REPO_ROOT"/skills/*/MANIFEST.yaml; do
  sk="$(basename "$(dirname "$m")")"
  mkdir -p "$FIX_LIVE/skills/$sk"
  cp "$m" "$FIX_LIVE/skills/$sk/MANIFEST.yaml"
done
out_live_undecl="$(printf '%s' "$(json_for 'tools/some_new_helper.R')" | TEST_REPO_ROOT="$FIX_LIVE" python3 "$HOOK" 2>&1; echo "EXIT:$?")"
if printf '%s' "$out_live_undecl" | grep -q 'EXIT:0'; then
  printf 'PASS  %-55s\n' "the live manifests let an undeclared tools/*.R stay editable"; PASS=$((PASS + 1))
else
  printf 'FAIL  %-55s got=%s\n' "the live manifests let an undeclared tools/*.R stay editable" "$out_live_undecl"; FAIL=$((FAIL + 1))
fi
out_live_decl="$(printf '%s' "$(json_for 'tools/record_selection.R')" | TEST_REPO_ROOT="$FIX_LIVE" python3 "$HOOK" 2>&1; echo "EXIT:$?")"
if printf '%s' "$out_live_decl" | grep -q 'EXIT:2'; then
  printf 'PASS  %-55s\n' "the live manifests still BLOCK the declared tools/record_selection.R"; PASS=$((PASS + 1))
else
  printf 'FAIL  %-55s got=%s\n' "the live manifests still BLOCK the declared tools/record_selection.R" "$out_live_decl"; FAIL=$((FAIL + 1))
fi
# 43d. The hook's own manifest parser reports no problem on any live manifest.
if python3 - "$REPO_ROOT" "$HOOK" <<'PY'
import importlib.util, glob, os, sys
repo, hook = sys.argv[1], sys.argv[2]
spec = importlib.util.spec_from_file_location("pp", hook)
pp = importlib.util.module_from_spec(spec); spec.loader.exec_module(pp)
yaml = pp._yaml_module()
bad = []
for m in sorted(glob.glob(os.path.join(repo, "skills", "*", "MANIFEST.yaml"))):
    doc, problem = pp._manifest_document(open(m).read(), yaml)
    if problem is not None:
        bad.append("{}: {}".format(os.path.relpath(m, repo), problem))
sys.exit(0 if not bad else 1)
PY
then
  printf 'PASS  %-55s\n' "the hook manifest parser reports no problem on all live manifests"; PASS=$((PASS + 1))
else
  printf 'FAIL  %-55s\n' "the hook manifest parser reports no problem on all live manifests"; FAIL=$((FAIL + 1))
fi
rm -rf "$FIX_LIVE"

# 44. Hook scripts (simplification row 4a). The directory .claude/hooks/ and
#     everything under it is absolute like gates/: a hook script decides what
#     every later tool call may do, the write guard's own file included, and
#     the directory is first on sys.path of every hook script, so a planted
#     module runs inside the guards. No unlock opens it; .claude/settings.json
#     stays editable (the recovery path). The SME's unlock file .harness-unlock
#     is refused the same way (owner decision D11 (a)): the SME creates it with
#     `touch` from a terminal. Plan: the path protection contract,
#     sections 3.1 and 4. Every payload only asks the guard; nothing is
#     written. Both symlinks point inside $FIX, which the trap removes.
mkdir -p "$FIX/.claude/hooks"
for s in protect_paths.py example_project_hook.py session_mandate.py; do
  echo "hook" > "$FIX/.claude/hooks/$s"
done
echo '{}' > "$FIX/.claude/settings.json"
ln -s "$FIX/.claude/hooks/protect_paths.py" "$FIX/docs/pp_link.py"
ln -s "$FIX/.claude/hooks" "$FIX/hooklink"

hk_write() { printf '{"tool_name":"Write","tool_input":{"file_path":"%s","content":"x"}}' "$1"; }
hk_nb() { printf '{"tool_name":"NotebookEdit","tool_input":{"notebook_path":"%s","new_source":"x"}}' "$1"; }

rm -f "$FIX/.harness-unlock"
# 44.1 to 44.9: the three hook scripts, absolute, through each Claude Code writer.
for s in protect_paths.py example_project_hook.py session_mandate.py; do
  run_case "hook scripts: Edit $s absolute" 2 "$(json_for "$FIX/.claude/hooks/$s")"
done
for s in protect_paths.py example_project_hook.py session_mandate.py; do
  run_case "hook scripts: Write $s absolute" 2 "$(hk_write "$FIX/.claude/hooks/$s")"
done
for s in protect_paths.py example_project_hook.py session_mandate.py; do
  run_case "hook scripts: NotebookEdit $s absolute" 2 "$(hk_nb "$FIX/.claude/hooks/$s")"
done
# 44.10 to 44.12: relative.
for s in protect_paths.py example_project_hook.py session_mandate.py; do
  run_case "hook scripts: Edit $s relative" 2 "$(json_for ".claude/hooks/$s")"
done
# 44.13 to 44.15: spellings that realpath resolves into the directory.
run_case "hook scripts: traversal" 2 "$(json_for 'docs/../.claude/hooks/protect_paths.py')"
run_case "hook scripts: symlinked file" 2 "$(json_for "$FIX/docs/pp_link.py")"
run_case "hook scripts: symlinked directory" 2 "$(json_for "$FIX/hooklink/example_project_hook.py")"
# 44.16 to 44.19: the directory, not a list of names.
run_case "hook scripts: new hook script planted" 2 "$(hk_write "$FIX/.claude/hooks/new_hook.py")"
run_case "hook scripts: non-.py file in the directory" 2 "$(hk_write "$FIX/.claude/hooks/new_hook.sh")"
run_case "hook scripts: planted package that would shadow an import" 2 \
  "$(hk_write "$FIX/.claude/hooks/json/__init__.py")"
run_case "hook scripts: the directory itself" 2 \
  '{"tool_name":"create_directory","tool_input":{"dirPath":".claude/hooks"}}'
# 44.20 to 44.24: the other dialects the guard parses.
run_case "hook scripts: vscode create_file relative" 2 \
  '{"tool_name":"create_file","tool_input":{"filePath":".claude/hooks/protect_paths.py","content":"x"}}'
run_case "hook scripts: apply_patch delete" 2 \
  '{"tool_name":"apply_patch","tool_input":{"input":"*** Begin Patch\n*** Delete File: .claude/hooks/example_project_hook.py\n*** End Patch"}}'
run_case "hook scripts: apply_patch move into" 2 \
  '{"tool_name":"apply_patch","tool_input":{"input":"*** Begin Patch\n*** Update File: docs/draft.md\n*** Move to: .claude/hooks/protect_paths.py\n@@\n-old\n+new\n*** End Patch"}}'
run_case "hook scripts: later target in a multi-target call" 2 \
  '{"tool_name":"multi_replace_string_in_file","tool_input":{"replacements":[{"filePath":"docs/ordinary.md","oldString":"a","newString":"b"},{"filePath":".claude/hooks/protect_paths.py","oldString":"a","newString":"b"}]}}'
run_case "hook scripts: copilot-cli string toolArgs" 2 \
  '{"toolName":"create","toolArgs":"{\"path\":\".claude/hooks/session_mandate.py\",\"file_text\":\"x\"}"}'
# 44.25: a fresh unlock does not open the directory (the test sits before the
# unlock read). The unlock is created and removed inside $FIX only, as the
# existing unlock cases do.
touch "$FIX/.harness-unlock"
run_case "hook scripts: fresh unlock does not open it" 2 "$(json_for '.claude/hooks/protect_paths.py')"
rm -f "$FIX/.harness-unlock"
# 44.26 to 44.30: what stays editable or outside.
run_case "hook scripts: settings.json Edit absolute allowed" 0 "$(json_for "$FIX/.claude/settings.json")"
run_case "hook scripts: settings.json Write relative allowed" 0 "$(hk_write '.claude/settings.json')"
run_case "hook scripts: sibling lookalike allowed" 0 "$(json_for '.claude/hooks_old/protect_paths.py')"
run_case "hook scripts: literal \$CLAUDE_PROJECT_DIR not expanded (documented)" 0 \
  '{"tool_name":"Edit","tool_input":{"file_path":"$CLAUDE_PROJECT_DIR/.claude/hooks/protect_paths.py"}}'
run_case "hook scripts: hook path outside the repository allowed" 0 \
  "$(json_for '/tmp/elsewhere/.claude/hooks/protect_paths.py')"
# 44.31 and 44.32: the fail-closed mirror in _looks_protected.
export PROTECT_PATHS_FORCE_FAULT=1
run_case "hook scripts: internal fault on a hook script fails closed" 2 \
  "$(json_for '.claude/hooks/protect_paths.py')"
run_case "hook scripts: internal fault on settings.json stays fail-open" 0 \
  "$(json_for '.claude/settings.json')"
unset PROTECT_PATHS_FORCE_FAULT
# 44.33: the category reads no manifest, so it holds without PyYAML.
export PROTECT_PATHS_NO_YAML=1
run_case "hook scripts: without PyYAML a hook script is still blocked" 2 \
  "$(json_for '.claude/hooks/example_project_hook.py')"
unset PROTECT_PATHS_NO_YAML
# 44.34 and 44.35: the guard located from its own path (no TEST_REPO_ROOT)
# refuses its own file and allows its root's settings.json. Both only decide.
out_self="$(printf '%s' "$(json_for "$HOOK")" | env -u TEST_REPO_ROOT python3 "$HOOK" 2>&1)"
rc_self=$?
if [ "$rc_self" -eq 2 ] && printf '%s' "$out_self" | grep -qF '.claude/hooks/protect_paths.py' \
    && printf '%s' "$out_self" | grep -qF 'no unlock opens it'; then
  printf 'PASS  %-55s exit=%s\n' "hook scripts: self: the guard refuses its own file" "$rc_self"; PASS=$((PASS + 1))
else
  printf 'FAIL  %-55s expected=2 got=%s\n' "hook scripts: self: the guard refuses its own file" "$rc_self"
  [ -n "$out_self" ] && printf '        stderr: %s\n' "$out_self"
  FAIL=$((FAIL + 1))
fi
out_self="$(printf '%s' "$(json_for "$REPO_ROOT/.claude/settings.json")" | env -u TEST_REPO_ROOT python3 "$HOOK" 2>&1)"
rc_self=$?
if [ "$rc_self" -eq 0 ]; then
  printf 'PASS  %-55s exit=%s\n' "hook scripts: self: the same guard allows its root's settings.json" "$rc_self"; PASS=$((PASS + 1))
else
  printf 'FAIL  %-55s expected=0 got=%s\n' "hook scripts: self: the same guard allows its root's settings.json" "$rc_self"
  [ -n "$out_self" ] && printf '        stderr: %s\n' "$out_self"
  FAIL=$((FAIL + 1))
fi
# 44.36 to 44.38: the SME's unlock file (D11 (a)). The payloads only ask the
# guard; no .harness-unlock is created by these cases.
run_case "unlock file: Write relative" 2 "$(hk_write '.harness-unlock')"
run_case "unlock file: Edit absolute" 2 "$(json_for "$FIX/.harness-unlock")"
export PROTECT_PATHS_FORCE_FAULT=1
run_case "unlock file: internal fault fails closed" 2 "$(hk_write '.harness-unlock')"
unset PROTECT_PATHS_FORCE_FAULT

# The public adapters register only the two shared local hooks.
if python3 - "$REPO_ROOT" <<'PY'
import json, re, sys
from pathlib import Path

root = Path(sys.argv[1])
expected = {"protect_paths.py", "session_mandate.py"}
for config in (root / ".claude/settings.json", root / ".github/hooks/pmx-harness.json"):
    assert set(re.findall(r"\.claude/hooks/([A-Za-z_]+\.py)", json.dumps(json.loads(config.read_text())))) == expected
assert {p.name for p in (root / ".claude/hooks").glob("*.py")} == expected
PY
then
  echo "PASS  public adapters register only the shared local hooks"
  PASS=$((PASS + 1))
else
  echo "FAIL  public adapter hook inventory"
  FAIL=$((FAIL + 1))
fi

echo "-----------------------------------------------"
echo "protect_paths guard: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
