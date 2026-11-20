# Public analysis workflow

## Prepare the environment

Use an R installation that meets the repository's dependency declarations. [`setup_environment.R`](../setup_environment.R) is for development environments where local package installation is allowed. A site-managed installation should use its own verified library and follow the [portability guide](PORTABILITY.md). For report rendering, make Quarto and its required rendering tools available before composing the report tasks.

Configure the project using [`harness.yaml.example`](../harness.yaml.example) as a template. Review the paths and engine choices for your use case and confirm the environment under the [review guide](SME-GUIDE.md#prepare-a-project). Keep study inputs, credentials, and licensed engine assets in the site's designated locations. The synthetic example needs an R environment and its data/specification packages.

## Run the synthetic example

From the project root, generate the example inputs.

```sh
Rscript analyses/synthetic_example.R --generate
```

Then build the introductory data-assembly package from R.

```r
targets::tar_make(
  script = "analyses/synthetic_example.R",
  store = "_targets_synthetic_example",
  names = tidyselect::all_of("data_assembly_package")
)
```

The declaration generates its data and specification locally. Review the assembled package before proceeding to EDA. The downstream stage will refuse to use an unsigned upstream gate. The public methods also require local review before guarded methods can compute.

## Declare a project workflow

1. Choose the stages your analysis requires, then declare the input paths and the applicable method options. The task catalog is in [`pipeline/`](../pipeline/), with per-stage input behavior in `pipeline/tasks/`.
2. Compose the ordered graph with `compose_pipeline()`. The composer attests the environment, tracks declared files, and connects a stage to the relevant upstream gate.
3. Build the next review package. Review the package and artifacts, including declared decisions and evidence references. Sign and record the approval locally before asking the next stage to consume it.
4. When an input changes, inspect the invalidated target and regenerate the affected package. Obtain a new decision for changed bound bytes.
5. If using reporting, copy the [report template](../templates/report/) to a declared project directory. Supply document metadata, the preceding packages, and any required project inputs there. Retain the report verification alongside the outputs.

NONMEM base modeling and qualification require a separately configured licensed engine for a real run. Generic engine tooling remains available for user-configured integration. Check each module's declared input contract before using a different engine or a separately assembled artifact.
