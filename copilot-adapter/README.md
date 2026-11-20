# Copilot adapter

The adapter uses Python 3.11 or newer and the standard library. Its profile
generates project instructions, scope-specific rules, local hooks, specialist
agents, skill shims, and editor settings. Claude Code and Copilot share the
same roles and method source. Read [Portability](../docs/PORTABILITY.md) for
the two runtime entry points.

## Install and check

```sh
python3 copilot-adapter/setup.py install
python3 copilot-adapter/setup.py check
python3 copilot-adapter/setup.py env
```

Run `install --dry-run` to review planned writes. Use `--force` to replace
differing generated files. An unchanged managed orphan can be removed during
installation. Review a hand-edited orphan before removing it manually.
Commit the generated artifacts with their `.generated-inventory.json`.
The inventory records each generated file and its digest.

The installer drafts an unconfirmed local environment file when absent.
Edit and confirm the draft locally before using tasks that require it.
`install --refresh-env` refreshes the probe blocks in an unconfirmed draft.
Use site-provided paths and license configuration for licensed engines.

## Export a standalone kit

```sh
python3 copilot-adapter/setup.py export /absolute/path/to/new-project
cd /absolute/path/to/new-project
python3 copilot-adapter/setup.py install
python3 copilot-adapter/setup.py check
```

Choose a separate destination outside the source repository. Export refuses
an existing destination adapter unless `--force` is supplied.

The stock profile lists every seed file by exact repository-relative path
and grants executable mode only to the declared executables. It includes
the synthetic example, local hooks, public method source, GPL license and
the vendored skills' MIT notice. Export substitutes the kit workflow for
the home workflow. The kit installs its own one-job lightweight checks.
The asset manifest records the path, exact permissions, byte count and
SHA-256 digest of each exported file after substitution. Extra files
under source directories cannot enter the selected payload. Export
rejects missing, symlinked and nonregular required sources.

Study data, signed history, credentials, local site configuration and
licensed software are excluded from the stock seed. The exported profile
does not add endpoint configuration. Supply any optional site resources
through site-controlled configuration.

First installation validates the complete manifest before seeding the
bundle's own target tree, then renders the adapter. Missing files seed
normally. Identical files are reused. A different existing source requires
explicit `install --reseed --overwrite-sources`. Review that operation in a
disposable copy first because it can restore an older method or gate tool.
Use `--reseed` to recheck source ownership after initial installation.

## Adapt it for another project

```sh
python3 copilot-adapter/setup.py export /absolute/path/to/project --no-profile
cd /absolute/path/to/project
python3 copilot-adapter/setup.py init
python3 copilot-adapter/setup.py install
```

Edit `profile/adapter.toml` and its fragments. Keep the role and skill source
in the target project. Custom profiles may use literal paths, glob patterns,
and optional payload entries. An optional entry has the form
`{path = "docs/**", optional = true}`. Custom profiles can independently
declare endpoint prompts through `[[mcp]]` with `id`, `type`, and
`input_description`. The generator refuses a literal URL in that profile
table. Use `--target` and `--profile` to point the adapter at a separate
project or a selected profile.
