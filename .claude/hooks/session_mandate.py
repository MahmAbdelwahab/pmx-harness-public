#!/usr/bin/env python3
"""SessionStart hook: inject the orchestrator mandate as JSON additionalContext.

The Claude Code and Copilot adapters call this local script. It emits the
orchestrator role as the SessionStart event's structured context:

    {"hookSpecificOutput": {"hookEventName": "SessionStart",
                            "additionalContext": "<roles/orchestrator.md> + pointer"}}

The root comes from the script's path. If the role file is absent the
context identifies it as unavailable.
"""

import json
import os
import sys

_HERE = os.path.dirname(os.path.realpath(__file__))
REPO_ROOT = os.path.realpath(os.environ.get("TEST_REPO_ROOT", os.path.join(_HERE, "..", "..")))

ROLE_FILE = "roles/orchestrator.md"
POINTER = "Public adapter runbook and specialist dispatch map: docs/PORTABILITY.md."


def build_context():
    path = os.path.join(REPO_ROOT, ROLE_FILE)
    try:
        with open(path, encoding="utf-8") as fh:
            body = fh.read()
    except OSError:
        body = "(orchestrator mandate unavailable: {})".format(ROLE_FILE)
    return "# Project orchestrator mandate ({})\n\n{}\n\n{}\n".format(ROLE_FILE, body.rstrip("\n"), POINTER)


def main():
    context = build_context()
    print(json.dumps({
        "hookSpecificOutput": {
            "hookEventName": "SessionStart",
            "additionalContext": context,
        }
    }, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    sys.exit(main())
