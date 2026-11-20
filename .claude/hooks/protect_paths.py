#!/usr/bin/env python3
"""PreToolUse guard: convert integrity instructions into a mechanical barrier.

Reads a PreToolUse tool-call JSON on stdin and decides whether the call may
touch the target path(s). The payload shape differs per runtime and the guard
keys on SHAPE, never on the tool name:

  Claude Code        {"tool_name": "Edit", "tool_input": {"file_path": ...}}
                     (Write, Edit; NotebookEdit carries notebook_path)
  VS Code Copilot    {"tool_name": "create_file", "tool_input": {"filePath": ...}}
                     also dirPath (create_directory), replacements[].filePath
                     (multi_replace_string_in_file), files[] / path / uri
                     (editFiles examples in the hooks docs), and apply_patch
                     `input` text with "*** Add|Update|Delete File: <path>"
                     lines. VS Code ignores hook matchers, so read tools reach
                     this guard too; a small read-only name list keeps them
                     allowed (see READ_ONLY_TOOLS).

The read skip list is a CONVENIENCE for read traffic, not a boundary: it is
keyed on the tool name, so a future write-capable tool sharing a read name
would skip path inspection. The boundary is the R/CI layer. Conversely, for
names in WRITE_TOOLS the fail-open policy is withdrawn — unparseable or
path-less arguments are denied (round 6, finding 15). Unknown names stay
fail-open.
  Copilot CLI        the input may sit under "toolArgs" instead of "tool_input".

Relative paths resolve against the repo root (the process cwd is the
workspace root under VS Code; the script locates the root from its own path).

Payload forms. Under VS Code, `tool_name` is
the model-facing ToolName value (snake_case, e.g. `create_file`), not the
contributed extension id, and the hook runs after the tool's `resolveInput`, so
`filePath` is absolute by contract — a relative path is rejected before the
hook (vscode-copilot-chat main 5863f5a: toolNames.ts, toolCalling.tsx,
promptPathRepresentationService.ts, and the package.json tool schemas). The
Copilot cloud agent and the Copilot CLI send a camelCase `preToolUse` event
carrying runtime tool names (`create`, `edit`, `view`, `glob`, `grep`, `rg`)
with `toolArgs` that may arrive as a JSON-encoded string, while the PascalCase
`PreToolUse` form maps those to the Claude tool names with a `tool_input`
object. The argument key names of the
cloud create/edit/view tools are undocumented; `path` is an inference from the
`str_replace_editor` alias. A byte-exact live capture of both dialects is still
pending — see docs/COPILOT-RUNBOOK.md. The fixtures in
tools/tests/test_protect_paths.sh are the acceptance set for every runtime.

Exit codes (identical contract in Claude Code and VS Code Copilot; the Copilot
cloud agent treats ANY non-zero exit as a block):
    0  allow the tool call
    2  BLOCK the tool call; stderr is shown to the calling agent as feedback

Design rule (fail-open OUTSIDE the protected categories, fail-closed INSIDE
them). This hook is one of several integrity layers, not the only one, and a
bug here must never freeze ordinary work — so an unexpected condition on a path
under no protected category allows the call and prints a one-line warning. It
must not, however, weaken the integrity boundary: an internal error whose
target is under a protected category (gates/**, .claude/hooks/**,
.harness-unlock, HARNESS_CORE, skills/*/reference/*.R, skills/*/MANIFEST.yaml,
a manifest-declared tools/*.R), or whose target could not be determined at
all, BLOCKS with exit 2 and says the guard itself needs fixing. A payload that
is not JSON is an external condition rather than a guard bug and still fails
open. The gates/ and .claude/hooks/ categories and the unlock file are
absolute: they are blocked even when an override is active, because no
legitimate agent edit of a signature record exists, a hook script decides what
every later tool call may do, and the unlock file is created by the SME from a
terminal, never by a tool call.
"""

import datetime
import glob
import json
import os
import re
import sys
import time

# Repo root is two levels up from this file: .claude/hooks/protect_paths.py .
# Honour TEST_REPO_ROOT only when set, so the test harness can point the guard
# at a fixture tree without disturbing production behaviour.
_HERE = os.path.dirname(os.path.realpath(__file__))
REPO_ROOT = os.path.realpath(
    os.environ.get("TEST_REPO_ROOT", os.path.join(_HERE, "..", ".."))
)

UNLOCK_FILE = ".harness-unlock"
UNLOCK_MAX_AGE_SECONDS = 8 * 60 * 60  # 8 hours

# The directory boundary also covers hook scripts added later.
HOOKS_DIR = ".claude/hooks"

# These files implement gate enforcement and are protected as a class.
HARNESS_CORE = {
    "pipeline/compose.R",
    "tools/gate.R",
    "tools/gate_package.R",
    # The optional legacy allowlist can authorize otherwise refused records.
    "tools/legacy_gates.yaml",
    "tools/method_manifest.R",
    # Gate enforcement: the approvals ledger, its chain verifier and the release
    # lock live here.
    "tools/approve_gate.R",
    "tools/errata.R",
    "tools/sign_approve_gate.R",
    # The trusted reference loader: authenticates a reference's bytes and its
    # declared dependencies' bytes against the manifest BEFORE evaluating
    # exactly those bytes.
    "tools/load_reference.R",
    # CI enforces reference-loading rules and the test registration inventory.
    "tools/ci_check.R",
}

UNLOCK_HINT = (
    "ask the SME to `touch .harness-unlock` at the repo root to authorize "
    "harness-core/signed-skill edits (the unlock expires after 8 hours)"
)

# Matches skills/<name>/reference/<something>.R
SKILL_REF_RE = re.compile(r"^skills/([^/]+)/reference/[^/]+\.R$")
# Matches skills/<name>/MANIFEST.yaml (the method manifest).
SKILL_MANIFEST_RE = re.compile(r"^skills/([^/]+)/MANIFEST\.yaml$")
# A real signature: "Reviewed by:" followed by non-whitespace.
SIGNATURE_RE = re.compile(r"^Reviewed by:\s*\S")
# The manifest root sign-off line (schema v2): reviewed_by: "<value>". Captures
# the value so the guard can tell an empty reviewed_by (authoring state) from a
# filled one. (Batch 2 used `approver:`; batch 3a renamed it to `reviewed_by`
# with the per-unit schema.)
MANIFEST_APPROVER_RE = re.compile(r'^reviewed_by:\s*"?([^"]*)"?\s*$')
# A `depends:` entry's path in a MANIFEST.yaml: `      - path: tools/x.R`.
# Since integrity batch 12 this is a CROSS-CHECK only (see
# _manifest_declared_tools): the dependency set is derived from the parsed
# YAML, and a path this text scan sees that the parsed document does not is a
# fail-closed signal. It can never ADD a path to the protected set.
MANIFEST_DEP_PATH_RE = re.compile(r'^\s*-\s*path:\s*"?([^"\s]+)"?\s*$', re.M)


# Keys that carry a single target path in the known payload shapes.
SINGLE_PATH_KEYS = ("file_path", "notebook_path", "filePath", "dirPath", "path", "uri")
# apply_patch (VS Code / Codex patch format): one header line per mutation
# endpoint. An Update may be followed by ``*** Move to:``; both the source and
# destination are targets and must be classified.
PATCH_FILE_RE = re.compile(
    r"^\*\*\* (?:(?:Add|Update|Delete) File|Move to): (.+)$", re.M)
# Read-only tools that carry a path. Claude Code never routes these here (its
# matcher is Edit|Write|NotebookEdit); VS Code ignores matchers, so without
# this list a read of a gate record would be denied. The VS Code names are the
# ToolName enum values (vscode-copilot-chat main 5863f5a, toolNames.ts) —
# note that the notebook-summary tool is the one entry that is NOT snake_case.
# The lowercase `view`/`glob`/`grep`/`rg` entries are the Copilot cloud/CLI
# runtime read names; their PascalCase equivalents (Read/Glob/Grep) cover the
# same tools in the Claude-name dialect. Unknown names are judged by shape —
# the guard never allows a write because of its name.
READ_ONLY_TOOLS = {
    # Claude Code / PascalCase cloud dialect.
    "Read", "NotebookRead", "Glob", "Grep",
    # VS Code Copilot (model-facing ToolName values).
    "read_file", "list_dir", "file_search", "grep_search", "semantic_search",
    "get_errors", "get_changed_files", "copilot_getNotebookSummary",
    "view_image", "read_notebook_cell_output", "read_project_structure",
    "test_search",
    # Copilot cloud agent / Copilot CLI runtime names.
    "view", "glob", "grep", "rg",
}

# Tools KNOWN to mutate the filesystem, across the dialects this guard handles.
# For these the fail-open policy is withdrawn: unparseable arguments, or
# arguments carrying no resolvable target, are DENIED rather than treated as an
# empty payload (Codex round 6, finding 15). A recognised writer whose target
# cannot be determined is exactly the case where allowing is least defensible.
# Unknown tool names are NOT in this set and remain fail-open by design — this
# hook is defence in depth, and the R/CI layer is the authoritative boundary.
WRITE_TOOLS = {
    # Claude Code.
    "Write", "Edit", "MultiEdit", "NotebookEdit",
    # VS Code Copilot (model-facing ToolName values).
    "create_file", "replace_string_in_file", "multi_replace_string_in_file",
    "insert_edit_into_file", "create_directory", "edit_notebook_file",
    "apply_patch",
    # VS Code Copilot tool-set alias used in the hooks documentation's
    # editFiles examples (files[] / path / uri shapes). It was already treated
    # as a writer by the fixtures but was missing here, so a path-less or
    # unparseable call under that name failed open (Codex round 7, finding 18).
    "editFiles",
    # Copilot cloud agent / Copilot CLI runtime names.
    "create", "edit", "str_replace_editor",
}

# How many times a JSON-encoded argument payload may be unwrapped. Copilot CLI
# sends toolArgs as a JSON string; double encoding has been observed in the
# wild. The bound stops a hostile payload from costing unbounded work.
MAX_ARG_DECODE_DEPTH = 4


def _strip_uri(p):
    """file:// URI -> filesystem path; anything else unchanged."""
    if isinstance(p, str) and p.startswith("file://"):
        from urllib.parse import unquote, urlparse
        return unquote(urlparse(p).path)
    return p


def _extract_paths(tool_input):
    """Return every candidate target path found in the payload, any shape."""
    paths = []
    if not isinstance(tool_input, dict):
        return paths
    for key in SINGLE_PATH_KEYS:
        value = tool_input.get(key)
        if isinstance(value, str) and value:
            paths.append(_strip_uri(value))
    files = tool_input.get("files")
    if isinstance(files, list):
        for item in files:
            if isinstance(item, str) and item:
                paths.append(_strip_uri(item))
            elif isinstance(item, dict):
                paths.extend(_extract_paths(item))
    replacements = tool_input.get("replacements")
    if isinstance(replacements, list):
        for item in replacements:
            paths.extend(_extract_paths(item))
    patch = tool_input.get("input")
    if isinstance(patch, str) and "*** " in patch:
        paths.extend(p.strip() for p in PATCH_FILE_RE.findall(patch))
    return paths


def _decode_args(value):
    """Unwrap a JSON-encoded arguments payload to a bounded depth.

    The Copilot CLI sends `toolArgs` as a JSON string, and double encoding has
    been seen. Returns the innermost decoded value: a dict when the payload is
    an object, otherwise whatever it decoded to (often a bare string, e.g. a
    raw patch body) so the caller can scan it. A string that is not JSON is
    returned unchanged rather than replaced with an empty dict — the caller
    decides what an unresolvable payload means for that tool.
    """
    for _ in range(MAX_ARG_DECODE_DEPTH):
        if not isinstance(value, str):
            break
        try:
            value = json.loads(value)
        except ValueError:
            break
    return value


def _rel_to_repo(file_path):
    """Return the repo-relative, realpath-normalised POSIX path, or None if the
    target resolves outside the repo root."""
    if not os.path.isabs(file_path):
        file_path = os.path.join(REPO_ROOT, file_path)
    real = os.path.realpath(file_path)
    if real == REPO_ROOT:
        return ""
    prefix = REPO_ROOT + os.sep
    if not real.startswith(prefix):
        return None
    return real[len(prefix):].replace(os.sep, "/")


def _unlock_active():
    """True iff .harness-unlock exists at repo root and is younger than 8h."""
    path = os.path.join(REPO_ROOT, UNLOCK_FILE)
    try:
        age = time.time() - os.path.getmtime(path)
    except OSError:
        return False
    return 0 <= age < UNLOCK_MAX_AGE_SECONDS


def _skill_is_signed(skill_name):
    """True iff the skill's SKILL.md or any SKILL_*.md carries a real
    'Reviewed by:' signature line."""
    skill_dir = os.path.join(REPO_ROOT, "skills", skill_name)
    try:
        names = os.listdir(skill_dir)
    except OSError:
        return False
    doc_names = [
        n for n in names
        if n == "SKILL.md" or (n.startswith("SKILL_") and n.endswith(".md"))
    ]
    for name in doc_names:
        try:
            with open(os.path.join(skill_dir, name), encoding="utf-8") as fh:
                for line in fh:
                    if SIGNATURE_RE.match(line):
                        return True
        except OSError:
            continue
    return False


def _manifest_approved(skill_name):
    """True iff skills/<skill_name>/MANIFEST.yaml has a non-empty root
    reviewed_by (schema v2). A confirmed manifest is authoritative (its per-unit
    statuses + digests are load-bearing), so it is protected as signed content;
    an empty-reviewed_by manifest is still in the authoring state and stays
    editable."""
    path = os.path.join(REPO_ROOT, "skills", skill_name, "MANIFEST.yaml")
    try:
        with open(path, encoding="utf-8") as fh:
            for line in fh:
                m = MANIFEST_APPROVER_RE.match(line)
                if m:
                    return bool(m.group(1).strip())
    except OSError:
        return False
    return False


def _yaml_module():
    """The PyYAML module, or None when it cannot be imported.

    PROTECT_PATHS_NO_YAML simulates the missing module for the test battery
    and is honoured only alongside TEST_REPO_ROOT, like the fault hook, so it
    is inert in every real invocation.
    """
    if os.environ.get("TEST_REPO_ROOT") and os.environ.get("PROTECT_PATHS_NO_YAML"):
        return None
    try:
        import yaml
    except Exception:
        return None
    return yaml


def _manifest_declared_tools():
    """tools/*.R helpers named as a dependency by ANY method manifest.

    Integrity batch 11 (round-10 finding P1-5). A helper a reference sources
    INTO its sealed validator object is on that object's lookup chain, so its
    contents decide what an approved gate runs; validate_method_manifest() now
    refuses a unit whose trusted loads are not declared under `depends:` with
    their digests. This guard, though, protected a fixed core set plus
    skills/*/reference/*.R -- so such a helper was the one file on that chain
    an agent could still edit freely.

    The set is DERIVED from the manifests at hook time rather than hard-coded,
    so it follows the manifests instead of drifting from them. A tools/*.R file
    no manifest names stays editable.

    Fix round 1 of batch 11 (independent review, finding 11.6). Every failure
    mode of this derivation used to OPEN the gate: an unreadable manifest was
    skipped by `except OSError: continue`, a manifest whose bytes no longer
    parse yielded an empty set, and a `depends:` path written with a `..`
    segment failed the literal `tools/` prefix test. In each case the helper
    silently dropped out of the protected set and became freely editable with no
    unlock. A guard whose whole design principle is fail-closed must not fail
    open on an unreadable input.

    Integrity batch 12 (Codex round 11, P1-2 and P3-1). Without PyYAML the
    derivation used to fall back to a regex over `- path:` lines plus a
    structural "looks like YAML" check, and a manifest truncated to `skill: x`
    and a bare `units:` passed both -- so on an interpreter without PyYAML
    every manifest-bound helper was editable with no unlock. The regex also
    excluded whitespace from the captured path, so `path: "tools/site
    helper.R"` (valid to the manifest reader, which merely unquotes it)
    protected nothing. A partial recogniser must not authorise writes. The set
    is now derived from the PARSED document only; without PyYAML the
    derivation fails closed and names the missing module; a `depends:` entry
    whose path is not a scalar string cannot be represented exactly and fails
    closed; and the regex is kept solely as a cross-check that can narrow (a
    path the text scan sees but the parsed document does not is a refusal)
    and never widen.

    Returns a pair: the derived set, and None on success or a sentence naming
    what could not be derived. When that second value is not None the caller
    blocks EVERY tools/*.R write and repeats the sentence, rather than
    proceeding on a set it knows to be incomplete.
    """
    out = set()
    yaml = _yaml_module()
    if yaml is None:
        return out, ("the yaml module (PyYAML) is not importable in this Python, "
                     "so no manifest's depends: can be parsed; a text scan is "
                     "not a substitute")
    for path in sorted(glob.glob(os.path.join(REPO_ROOT, "skills", "*",
                                              "MANIFEST.yaml"))):
        rel_path = os.path.relpath(path, REPO_ROOT)
        try:
            with open(path, encoding="utf-8", errors="replace") as fh:
                text = fh.read()
        except OSError:
            return out, "{} cannot be read".format(rel_path)
        doc, problem = _manifest_document(text, yaml)
        if doc is None:
            return out, "{}: {}".format(rel_path, problem)
        units = doc.get("units")
        derived_here = set()
        for unit_name, unit in units.items():
            deps = unit.get("depends")
            if deps is None:
                continue
            for dep in deps:
                norm = _repo_relative(dep["path"].strip())
                if norm is None:
                    # Fix round 2 of batch 11 (review finding F3-a): a depends:
                    # path that will not reduce to a repository-relative path is
                    # a fail-closed signal rather than a silent omission.
                    return out, ("{} (depends: path {!r} does not resolve to a "
                                 "repository-relative path)".format(rel_path, dep["path"]))
                if norm.startswith("tools/") and norm.endswith(".R"):
                    derived_here.add(norm)
        # Cross-check that cannot widen: every tools/*.R the text scan sees
        # must be in the parsed set, else the two readings disagree.
        for hit in MANIFEST_DEP_PATH_RE.findall(text):
            norm = _repo_relative(hit)
            if norm is not None and norm.startswith("tools/") and norm.endswith(".R") \
                    and norm not in derived_here:
                return out, ("{} names {!r} under depends: in its text but the "
                             "parsed document does not; the two readings disagree"
                             .format(rel_path, hit))
        out |= derived_here
    return out, None


def _repo_relative(hit):
    """`hit` as a repository-relative POSIX path, or None if it is not one.

    An absolute path under REPO_ROOT is mapped to its relative form; an
    absolute path elsewhere, and a relative path that climbs out of the
    repository, both return None -- the caller's fail-closed signal.
    """
    if os.path.isabs(hit):
        try:
            rel = os.path.relpath(os.path.normpath(hit), REPO_ROOT)
        except ValueError:
            return None
    else:
        rel = os.path.normpath(hit)
    rel = rel.replace(os.sep, "/")
    if rel == ".." or rel.startswith("../") or os.path.isabs(rel):
        return None
    return rel


# The manifest's schema, mirrored from tools/method_manifest.R's strict reader
# (.MM_ROOT_FIELDS, .MM_UNIT_FIELDS, .MM_UNIT_REQUIRED): the guard must not be
# more permissive than the harness parser, or a manifest the harness would call
# malformed could authorise a write (integrity batch 13, Codex round 12, P2-1).
#
# The scalar types a field may hold are the ones PyYAML actually produces for
# the values write_method_manifest() emits and read_method_manifest() accepts.
# `generated_at`/`reviewed_at` are unquoted ISO-8601 timestamps, which PyYAML
# parses to datetime.datetime (or datetime.date); the R reader reads them as
# text and never type-checks. Integrity batch 13 fix round 1 (R13-P1-C): the
# batch-12 tuple accepted only (str, int, float), so every live manifest --
# each with an unquoted `generated_at:` -- failed this check, the guard fell
# closed on it, and EVERY tools/*.R write (the SME's own included) was blocked
# with a misleading reason while the helper derivation could never run. bool is
# included because a YAML `false`/`true` would parse to bool, still a scalar to
# the R reader.
MANIFEST_SCALAR_TYPES = (str, int, float, bool, datetime.datetime, datetime.date)
MANIFEST_ROOT_FIELDS = {"skill", "generated_at", "reviewed_by", "reviewed_at", "units"}
MANIFEST_UNIT_FIELDS = {"status", "reference_path", "entrypoints", "sha256",
                        "evidence", "reason", "depends"}
MANIFEST_UNIT_REQUIRED = {"status", "reference_path", "entrypoints", "sha256",
                          "evidence", "reason"}


def _strict_loader(yaml):
    """A SafeLoader subclass that refuses a duplicate key in any mapping.

    PyYAML's safe_load keeps the LAST value of a duplicated key silently, while
    read_method_manifest() refuses the manifest; a unit with a first `depends:`
    naming a helper and a second `depends: []` therefore protected nothing
    here while the harness would have refused to read it (R12-P2-1).
    """
    class StrictLoader(yaml.SafeLoader):
        def construct_mapping(self, node, deep=False):
            seen = set()
            for key_node, _ in node.value:
                key = self.construct_object(key_node, deep=deep)
                if key in seen:
                    raise yaml.constructor.ConstructorError(
                        "while constructing a mapping", node.start_mark,
                        "duplicate key {!r}".format(key), key_node.start_mark)
                seen.add(key)
            return super().construct_mapping(node, deep=deep)
    return StrictLoader


def _manifest_schema_problem(doc):
    """A sentence naming the first schema violation, or None when the parsed
    document has the shape read_method_manifest() accepts."""
    if not isinstance(doc, dict):
        return "the document is not a mapping"
    unknown = set(doc) - MANIFEST_ROOT_FIELDS
    if unknown:
        return "unknown root field {!r}".format(sorted(unknown)[0])
    for k in doc:
        if k != "units" and doc[k] is not None and not isinstance(doc[k], MANIFEST_SCALAR_TYPES):
            return "root field {!r} is not a scalar".format(k)
    units = doc.get("units")
    if not isinstance(units, dict) or not units:
        return "declares no units: mapping"
    for unit_name, unit in units.items():
        if not isinstance(unit, dict):
            return "unit {!r} is not a mapping".format(unit_name)
        unknown = set(unit) - MANIFEST_UNIT_FIELDS
        if unknown:
            return "unit {!r} has an unknown field {!r}".format(unit_name, sorted(unknown)[0])
        # The reader refuses a unit missing any required field, so the guard
        # must too or it would be more permissive than the harness parser
        # (integrity batch 13 fix round 1, R13-P3-F: MANIFEST_UNIT_REQUIRED was
        # defined and never consulted).
        missing = MANIFEST_UNIT_REQUIRED - set(unit)
        if missing:
            return "unit {!r} is missing the required field {!r}".format(
                unit_name, sorted(missing)[0])
        deps = unit.get("depends")
        if deps is None:
            continue
        if not isinstance(deps, list):
            return "unit {!r} has a depends: that is not a list".format(unit_name)
        for dep in deps:
            if not isinstance(dep, dict) or set(dep) != {"path", "sha256"}:
                return ("unit {!r} has a depends: entry that is not a mapping of exactly "
                        "path and sha256".format(unit_name))
            for f in ("path", "sha256"):
                if not isinstance(dep[f], str) or not dep[f].strip():
                    return ("unit {!r} has a depends: entry whose {} is not a scalar string, "
                            "so it cannot be represented exactly".format(unit_name, f))
    return None


# ONE GRAMMAR, BOTH READERS (integrity batch 14, Codex round 13 R13-P2-2).
# General YAML is a larger language than tools/method_manifest.R's line lexer,
# and the difference was observable: PyYAML decodes escapes inside a
# double-quoted scalar while the R reader only strips the quotes, so
# `path: "tools/site\\helper.R"` named two different (both legal) Unix files
# to the two readers -- and the guard would then have protected a file the
# harness never loads. The manifest language is therefore restricted, HERE and
# in read_method_manifest(), to what both readers lex identically, and the
# check runs on the raw text BEFORE any normalization.
_MANIFEST_SCALAR_REFUSALS = (
    ("\\", "a backslash: PyYAML decodes escapes inside a double-quoted scalar "
             "and the R reader does not, so the two readers would name "
             "different files"),
    ("&", "a YAML anchor"),
    ("*", "a YAML alias"),
    ("!", "a YAML tag"),
    # A SINGLE-QUOTED SCALAR (integrity batch 16, Codex round 15 R15-P2-4).
    # PyYAML strips single quotes and tools/method_manifest.R does not -- its
    # .mm_unquote() removes double quotes only -- so `path: 'tools/x.R'` named
    # tools/x.R to this guard and "'tools/x.R'" to the R reader. One spelling
    # is enough: the shipped manifests are written with double quotes or bare.
    ("'", "a single quote: PyYAML strips single quotes and the R reader keeps "
          "them, so the two readers would name different files. Write the "
          "scalar bare or double-quoted"),
)
# A `#` anywhere in a scalar value (integrity batch 15, Codex round 14
# R14-P1-4). PyYAML lexes ` #` as a comment and tools/method_manifest.R keeps
# it in the value, so `- path: tools/site # helper.R` named two different files
# to the two readers and left the real `.R` dependency outside this guard's
# protection set. Refused outright rather than given a meaning.
_MANIFEST_SCALAR_HASH_WHY = (
    "a '#': PyYAML lexes it as the start of a comment and the R reader keeps "
    "it in the value, so the two readers would name different files")
# YAML punctuation inside the bracketed entrypoints: list. Both readers used to
# skip their scalar refusals for that line; an entry point is an R function
# name, so nothing else belongs in the list.
_MANIFEST_ENTRYPOINT_BAD = "&*!{}[]#\\"


def _manifest_grammar_problem(text):
    """A sentence naming the first construct outside the shared manifest
    grammar, or None. Mirrors tools/method_manifest.R's line lexer."""
    in_units = False
    for lineno, raw in enumerate(text.split("\n"), start=1):
        line = raw.rstrip("\r")
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        if line.lstrip().startswith("<<:"):
            return "line {}: a YAML merge key is not part of the manifest grammar".format(lineno)
        key, sep, value = line.partition(":")
        if not sep:
            return "line {}: not a `key: value` line".format(lineno)
        stripped = key.strip()
        val = value.strip()
        indent = len(key) - len(key.lstrip(" "))
        is_dep_item = stripped.startswith("- ")
        # EVERY INDENTATION LEVEL IS ENFORCED, not only the unit header
        # (integrity batch 16, Codex round 15 R15-P2-4). Batch 15 fixed the unit
        # header at two spaces and said the other columns were "already implied
        # by the key names". They were not: tools/method_manifest.R matches unit
        # FIELDS with the literal `^    ` (four spaces) and depends fields with
        # `^      - path:` / `^        sha256:` (six and eight), so a document
        # whose unit fields sat at five spaces was representable in PyYAML,
        # accepted here, and refused there -- one document, two verdicts, which
        # is not one grammar. The columns the R reader requires are:
        #     0  root fields and `units:`
        #     2  a unit key
        #     4  a unit field
        #     6  `- path:` (a depends entry opener)
        #     8  `sha256:` (a depends entry's second field)
        # A COLUMN-ZERO KEY ENDS THE UNITS BLOCK (integrity batch 17, Codex
        # round 16 R16-P2-3). tools/method_manifest.R does exactly this: a line
        # matching a root key after `units:` flushes the current unit and sets
        # `in_units <- FALSE`, so `reviewed_by:` written AFTER the units block
        # is a valid document there. This lexer set `in_units` and never reset
        # it, so the same document hit the unit-field indentation rule and was
        # refused -- one document, two verdicts again, and this time on the
        # fail-closed side that makes the guard's dependency derivation fail and
        # block every `tools/*.R` write. The unit and dependency indentation
        # rules below are unchanged; they now apply only while a unit is open.
        if in_units and indent == 0 and not is_dep_item:
            in_units = False
        if in_units and not val and not is_dep_item and stripped not in (
                "units", "depends") and indent != 2:
            return ("line {}: the unit key '{}' is indented {} spaces; a unit "
                    "key is indented exactly two".format(lineno, stripped, indent))
        if in_units and is_dep_item and indent != 6:
            return ("line {}: a depends entry opener is indented {} spaces; it is "
                    "indented exactly six".format(lineno, indent))
        if in_units and not is_dep_item and stripped == "sha256" and indent not in (4, 8):
            return ("line {}: 'sha256:' is indented {} spaces; it is indented four "
                    "as a unit field and eight inside a depends entry".format(
                        lineno, indent))
        if in_units and not is_dep_item and stripped == "path" and indent != 8:
            return ("line {}: 'path:' is indented {} spaces; a depends entry's "
                    "path is written on the `- path:` opener at six".format(
                        lineno, indent))
        if in_units and val and not is_dep_item and stripped not in (
                "units", "depends") and indent not in (4, 8):
            return ("line {}: the field '{}' is indented {} spaces; a unit field is "
                    "indented four and a depends field eight".format(
                        lineno, stripped, indent))
        if in_units and not val and stripped == "depends" and indent != 4:
            return ("line {}: 'depends:' is indented {} spaces; it is a unit field "
                    "and is indented four".format(lineno, indent))
        # COLUMN ZERO, BEFORE `units:` (integrity batch 16 fix round 1, review
        # observation O1). Every check above is guarded by `in_units`, and the
        # unit-field check used to accept indent 0 as well, so two documents the
        # R reader refuses were accepted here: a ROOT field indented two spaces
        # (R: "cannot parse line N") and a unit field at column zero (R: the
        # unit "is missing field 'status'"). Both were the permissive side of a
        # fail-closed pair, so no authority was granted, but the claim was that
        # every level is enforced. It is now.
        if not in_units and indent != 0:
            return ("line {}: the root field '{}' is indented {} spaces; a root "
                    "field is at column zero".format(lineno, stripped, indent))
        if is_dep_item:
            dep_key = stripped[2:].strip()
            # Every `- ` item is checked, including the first one in the
            # document: read_method_manifest() checks every entry, so a first
            # entry written `- sha256:` must be refused here too.
            if dep_key != "path":
                return ("line {}: a depends entry opens with '{}:'; every entry is "
                        "written `- path:` first and `sha256:` second".format(lineno, dep_key))
        if stripped == "depends":
            if val:
                return ("line {}: depends: must open a block; an inline sequence or "
                        "mapping is not part of the manifest grammar".format(lineno))
            continue
        if stripped == "units":
            if val:
                return "line {}: units: must open a block".format(lineno)
            in_units = True
            continue
        if stripped == "entrypoints":
            if not (val.startswith("[") and val.endswith("]")):
                return "line {}: entrypoints must be a bracketed list".format(lineno)
            for element in val[1:-1].split(","):
                element = element.strip()
                if any(ch in element for ch in _MANIFEST_ENTRYPOINT_BAD):
                    return ("line {}: entrypoints list element '{}' carries YAML "
                            "punctuation (&, *, !, {{, [, # or a backslash)".format(
                                lineno, element))
            continue
        if val.startswith("[") or val.startswith("{"):
            return ("line {}: an inline sequence or mapping is not part of the "
                    "manifest grammar (only entrypoints: is bracketed)".format(lineno))
        if "#" in val:
            return "line {}: the scalar contains {}".format(
                lineno, _MANIFEST_SCALAR_HASH_WHY)
        for token, why in _MANIFEST_SCALAR_REFUSALS:
            if token == "\\":
                if token in val:
                    return "line {}: the scalar contains {}".format(lineno, why)
            elif val.startswith(token):
                return "line {}: the scalar begins with {}".format(lineno, why)
    return None


def _manifest_document(text, yaml):
    """The manifest parsed as a YAML mapping and schema-checked: (doc, None) on
    success, (None, problem) otherwise.

    An empty file, a file containing NUL bytes or replacement characters, a
    file that does not parse under the duplicate-key-refusing loader, and a
    document that violates the manifest schema are all refusals -- a
    fail-closed signal, not a permission. There is no fallback recogniser
    (integrity batch 12): without the parser the caller has already refused.
    """
    if not text.strip():
        return None, "the manifest is empty"
    if "\x00" in text or "\ufffd" in text:
        return None, "the manifest carries NUL or replacement characters"
    try:
        doc = yaml.load(text, Loader=_strict_loader(yaml))
    except Exception as exc:  # noqa: BLE001 -- any parser failure is a refusal
        return None, "does not parse as a method manifest ({})".format(
            " ".join(str(exc).split())[:160])
    # The shared grammar is checked after the parse so that a document PyYAML
    # itself refuses (a duplicate key, say) is still reported as that.
    problem = _manifest_grammar_problem(text)
    if problem is not None:
        return None, "is outside the shared manifest grammar ({})".format(problem)
    problem = _manifest_schema_problem(doc)
    if problem is not None:
        return None, "does not match the method-manifest schema ({})".format(problem)
    return doc, None


def _decision(rel):
    """Return (block: bool, message: str). message only meaningful when block."""
    # Fault injection, test-only. G6's fail-closed branch cannot be exercised
    # without an internal error to fail on, and a test that reaches it by
    # corrupting the guard's own source would be testing a different file. This
    # raises only when the test harness has already redirected the repository
    # root, so it is inert in every real invocation.
    if os.environ.get("TEST_REPO_ROOT") and os.environ.get("PROTECT_PATHS_FORCE_FAULT"):
        raise RuntimeError("injected fault (PROTECT_PATHS_FORCE_FAULT)")

    # Category 1: gate records — always blocked, never overridable.
    if rel == "gates" or rel.startswith("gates/"):
        return True, (
            "BLOCKED: gates/ holds gate records and approval or errata journals. "
            "Use the gate tools through Rscript for drafts and ledger transactions; "
            "the SME supplies the review and signature. "
            "Do not edit a gate record directly — escalate to the SME."
        )

    # Category 1b: the hook scripts. A hook script decides what every later
    # tool call may do, the write guard's own file included, so it is
    # absolute like gates/: the unlock file can be created through Bash by any
    # caller and would open nothing worth opening. .claude/settings.json stays
    # editable: it is the recovery path when a
    # registered hook script goes missing.
    if rel == HOOKS_DIR or rel.startswith(HOOKS_DIR + "/"):
        return True, (
            "BLOCKED: {} is under .claude/hooks/, the harness's hook scripts; "
            "a hook script decides what every later tool call may do, so no "
            "editor tool call may write it and no unlock opens "
            "it. Prepare a reviewed change in a separate copy and obtain owner "
            "authorization before installing it from a terminal. .claude/settings.json "
            "stays editable.".format(rel)
        )
    # Category 1c (D11 (a)): the SME's unlock file. A tool call that could
    # create or refresh it would open every unlock-gated category for eight
    # hours; the SME creates it with `touch` from a terminal, which no hook
    # sees.
    if rel == UNLOCK_FILE:
        return True, (
            "BLOCKED: .harness-unlock is the SME's unlock; no tool call may "
            "create or refresh it. The SME creates it with `touch` from a "
            "terminal."
        )

    override = _unlock_active()

    # Category 2: the gate mechanism itself.
    if rel in HARNESS_CORE and not override:
        return True, (
            "BLOCKED: {} is part of the gate mechanism; editing it can relax "
            "every gate at once. If this change is authorized, {}.".format(rel, UNLOCK_HINT)
        )

    # Category 2b: a tools/*.R helper bound by a method manifest's depends:.
    # It is loaded into a sealed validator object, so editing it changes what
    # an approved gate runs -- the same consequence as editing the reference
    # file the manifest digests (integrity batch 11, R10-P1-5).
    if not override and rel.startswith("tools/") and rel.endswith(".R"):
        declared, problem = _manifest_declared_tools()
        if problem is not None:
            return True, (
                "BLOCKED: the set of tools/*.R helpers bound by a method "
                "manifest's depends: cannot be derived: {}. This guard fails "
                "CLOSED: every tools/*.R write is blocked until the derivation "
                "succeeds. If this change is authorized regardless, "
                "{}.".format(problem, UNLOCK_HINT)
            )
        if rel in declared:
            return True, (
                "BLOCKED: {} is sourced into a sealed validator object and bound "
                "by its digest under a method manifest's depends:; editing it "
                "changes what an approved gate runs. If this change is "
                "authorized, {}.".format(rel, UNLOCK_HINT)
            )

    # Category 3: validated reference code. Protected when EITHER the skill's
    # SKILL.md carries a legacy signature OR its MANIFEST.yaml is confirmed
    # (non-empty root reviewed_by): a confirmed manifest binds the reference
    # file's digest, so an edit to the file breaks the same self-gate that an
    # edit to the manifest would (Codex round 7, finding 18).
    m = SKILL_REF_RE.match(rel)
    if m and not override and (_skill_is_signed(m.group(1))
                               or _manifest_approved(m.group(1))):
        return True, (
            "BLOCKED: {} is validated reference code under a signed skill or a "
            "confirmed method manifest; editing it invalidates the review "
            "signature / breaks the manifest digest binding. If this change is "
            "authorized, {}.".format(rel, UNLOCK_HINT)
        )

    # Category 4: a CONFIRMED method manifest (schema v2). Its per-unit statuses
    # + digests are the active self-gate once the SME fills root reviewed_by, so
    # editing it is exactly as consequential as editing signed reference code.
    # An empty-reviewed_by manifest is still being authored and stays editable.
    mm = SKILL_MANIFEST_RE.match(rel)
    if mm and not override and _manifest_approved(mm.group(1)):
        return True, (
            "BLOCKED: {} is an SME-approved method manifest; its digests are "
            "the active self-gate and editing it would break the binding. If "
            "this change is authorized, {}.".format(rel, UNLOCK_HINT)
        )

    return False, ""


def main():
    targets = []
    try:
        raw = sys.stdin.read()
        try:
            payload = json.loads(raw)
        except ValueError as exc:
            # A payload that is not JSON at all is an EXTERNAL condition, not a
            # guard bug: some other process is speaking a dialect this hook
            # does not know, and there is no target to judge. That case has
            # always failed open and still does; the fail-closed branch below
            # is for errors raised while the guard was doing its own work.
            sys.stderr.write(
                "protect_paths.py warning: guard skipped, stdin is not a JSON "
                "tool-call payload: {}\n".format(exc)
            )
            return 0
        tool_name = payload.get("tool_name") or payload.get("toolName") or ""
        if tool_name in READ_ONLY_TOOLS:
            # Reading a protected file is always fine; only writes are guarded.
            return 0
        is_writer = tool_name in WRITE_TOOLS
        tool_input = payload.get("tool_input")
        if tool_input is None:
            tool_input = payload.get("toolArgs")
        if tool_input is None:
            tool_input = {}
        tool_input = _decode_args(tool_input)
        if isinstance(tool_input, dict):
            paths = _extract_paths(tool_input)
        elif isinstance(tool_input, str):
            # Schema drift: the arguments decoded to a bare string. If it is a
            # patch body, its file headers are the targets; scan them directly
            # rather than returning nothing (round 6, finding 15).
            paths = [p.strip() for p in PATCH_FILE_RE.findall(tool_input)]
        else:
            paths = []
        # Preserve the complete extracted set before classification. If one
        # decision raises, the catch-all must still see every later target.
        targets = list(paths)
        if not paths:
            if is_writer:
                sys.stderr.write(
                    "BLOCKED: {} is a file-writing tool but its arguments could "
                    "not be parsed into a target path, so this guard cannot tell "
                    "what it would write. A recognised writer with an "
                    "unresolvable target is refused. Reissue the call with an "
                    "explicit file path.\n".format(tool_name or "<unnamed tool>")
                )
                return 2
            # No path to judge (terminal, search, MCP, ...) — nothing to guard.
            return 0
        for path in paths:
            rel = _rel_to_repo(path)
            if rel is None:
                # Target resolves outside the repo; not our concern.
                continue
            block, message = _decision(rel)
            if block:
                sys.stderr.write(message + "\n")
                return 2
        return 0
    except Exception as exc:
        # Fail CLOSED inside the protected categories, fail OPEN outside them
        # (SME decision, fix round 2 of batch 11). The catch-all used to return
        # 0 for every internal error, which meant a bug in this guard opened
        # precisely the categories the guard exists to hold -- one function
        # below a message that tells the operator the guard fails closed. The
        # split keeps the original reason for fail-open intact (a hook bug must
        # never freeze ordinary work) while refusing to trade an integrity
        # boundary for that convenience. A call whose target could not be
        # determined at all is treated as protected: an unknown target cannot
        # be shown to be outside the boundary.
        protected = _looks_protected(targets)
        sys.stderr.write(
            "{}: internal error in protect_paths.py: {}. Target(s): {}. {}\n".format(
                "BLOCKED" if protected else "protect_paths.py warning",
                exc,
                ", ".join(targets) if targets else "<could not be determined>",
                "This guard fails CLOSED inside the protected categories, so "
                "the call is refused until the guard itself is fixed."
                if protected else
                "The target is outside every protected category, so the call is "
                "allowed; the guard still needs fixing.",
            )
        )
        return 2 if protected else 0


def _looks_protected(targets):
    """Would any of these targets fall under a protected category?

    The last-resort classifier used by main()'s catch-all, deliberately
    independent of _decision() -- the point is to survive _decision() itself
    failing. It is purely syntactic: the same category SHAPES, no manifest
    reads, no unlock check (an internal error is not an authorization). No
    determinable target counts as protected.
    """
    if not targets:
        return True
    for path in targets:
        try:
            rel = _rel_to_repo(path)
        except Exception:
            return True
        if rel is None:
            continue
        if rel == "gates" or rel.startswith("gates/"):
            return True
        if rel == HOOKS_DIR or rel.startswith(HOOKS_DIR + "/"):
            return True
        if rel == UNLOCK_FILE:
            return True
        if rel in HARNESS_CORE:
            return True
        if rel.startswith("tools/") and rel.endswith(".R"):
            return True
        parts = rel.split("/")
        if len(parts) >= 2 and parts[0] == "skills":
            if parts[-1] == "MANIFEST.yaml":
                return True
            if "reference" in parts[1:-1] and rel.endswith((".R", ".r")):
                return True
    return False


if __name__ == "__main__":
    sys.exit(main())
