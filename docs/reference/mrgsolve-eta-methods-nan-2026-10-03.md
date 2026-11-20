# Individual random effects in mrgsolve

The notes below concern mrgsolve 1.6.1. Use the documentation for the installed version and check the selected simulation path on a small fixture. [mrgsolve source at 1.6.1](https://github.com/metrumresearchgroup/mrgsolve/tree/1.6.1), [mrgsolve user guide](https://mrgsolve.org/user-guide/)

## Select the source of individual random effects

- With the default `etasrc = "omega"`, mrgsolve simulates individual random effects from the assembled OMEGA blocks. Use the R session's seed and record the simulation inputs and settings.
- Use `data.all` to supply a complete set of ETA columns from each individual's first data record. Use `idata.all` to require a complete set from individual data. The `data` and `idata` choices can substitute zero for an omitted ETA column when others are supplied.
- When combining event data and individual data, verify ETA assignment per ID. With both data and idata supplied, mrgsolve 1.6.1 can use positional assignment for ETAs from idata even when individual covariates are matched by ID. For that version, explicitly align idata to the order of individuals in data, require unique IDs in idata and keep each individual's event data together. In a test simulation, capture the ETA used by the model and compare it with the intended value per ID.
- Check any model code that calls `simeta()`, since a new draw can replace a supplied ETA. Zeroing OMEGA disables variability from that matrix, while an explicit ETA source can still supply variation.

## Validate the inputs and outputs

Check that the ETA columns match the expected dimension, that all required values are finite, and that the inputs are aligned per individual. Check that the covariance is finite, symmetric, and positive semidefinite. Verify finite simulated values before computing summaries. Include out-of-order IDs, incomplete ETAs and non-finite values in checks of the calling code, and repeat those checks when upgrading the simulation engine.
