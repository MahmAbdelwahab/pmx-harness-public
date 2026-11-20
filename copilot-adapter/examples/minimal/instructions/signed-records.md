# Signed records — never edit these files

Files under `signed/**` are the signed audit trail. They are not agent-writable.

- **Never edit a signed record.** Signing is the principal's hand, never yours.
- **No editor-time guard here.** Copilot has no write guard; post-hoc
  verification and CI detect any tamper. Detection is the control — the
  keystroke is not blocked.
