# NONMEM estimation

Configure a licensed native or containerized NONMEM provision. Supply the `bbi_path` argument or set the machine-local `bbi_path` key in `harness.yaml`. When neither provides a path, the estimation wrapper looks for `bbi` on `PATH`. Declare model paths, initial estimates, operational settings and the run's working directory. Review the per-run configuration before execution and retain the configuration snapshot.

After local approval of the `estimation` unit, `reference_nm_workspace()` prepares the workspace, `reference_nm_fit()` submits the model and `reference_nm_results()` reads the results. The results reader checks that bbr can join observation data to the run tables. Use a control-stream data path and row key that support the selected workflow, and record exclusions.

Use the runtime outputs for minimization status, covariance status, estimates and diagnostics. Review the outputs in the local gate package before progressing. [bbr documentation](https://metrumresearchgroup.github.io/bbr/)
