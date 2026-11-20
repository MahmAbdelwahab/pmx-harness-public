# PMx Harness

PMx Harness provides tools and templates for pharmacometric analysis and model-informed drug development. It separates the work an agent can propose from the execution and approval a project must record. The pipeline can track analytical artifacts, method inputs, and review decisions across a `targets` workflow.

## Start with the synthetic example

Use the public R dependency setup in the [workflow guide](docs/WORKFLOW.md). From the project root, generate the example inputs locally, then build the introductory assembly target.

```sh
Rscript analyses/synthetic_example.R --generate
```

```r
targets::tar_make(
  script = "analyses/synthetic_example.R",
  store = "_targets_synthetic_example",
  names = tidyselect::all_of("data_assembly_package")
)
```

The example creates synthetic inputs for a disposable local project. Use the [review guide](docs/SME-GUIDE.md) when a project requires local method review or stage approval. The public manifests ship unsigned. A project must review its own methods and satisfy the relevant gates before guarded stages can run.

## What to explore

| Path | Contents |
| --- | --- |
| `pipeline/`, `analyses/` | Composable `targets` stages and the synthetic example |
| `skills/`, `roles/` | Method source and agent role definitions |
| `tools/`, `templates/` | Execution helpers, checks, and project artifacts |
| `.claude/`, `.github/`, `copilot-adapter/` | Claude Code and GitHub Copilot integrations |

See the [architecture](docs/ARCHITECTURE.md) for the pipeline and the [portability guide](docs/PORTABILITY.md) for local configuration. Keep study data and access credentials in your site's approved storage. Licensed engine integrations require a separately configured installation and license. CI exercises the public harness with synthetic inputs.

## License and contact

Owner-controlled code is licensed under the [GNU General Public License version 3](LICENSE). The vendored R skills retain their [MIT notice](LICENSES/llm-r-skills-MIT.txt). For project questions, open a GitHub issue in this repository.
