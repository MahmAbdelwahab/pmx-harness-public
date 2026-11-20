# PsN parameter uncertainty

Use the `bootstrap`, `sir` or `llp` unit according to the declared uncertainty question. Each unit requires local approval. Configure the licensed NONMEM and PsN provision and retain the invocation and result artifacts.

Declare the control stream, working directory, replication or resampling settings, seed and any parameter selections. For bootstrap, `reference_psn_bootstrap_precheck()` checks the control stream, `reference_psn_bootstrap()` runs the tool and `reference_psn_bootstrap_samples()` reads explicitly selected parameter columns and replicate success flags. For importance resampling use `reference_psn_sir()`. For a likelihood profile use `reference_psn_llp()` with explicit parameter selections.

Review successful and failed replicate counts, interval finiteness and the suitability of the results before using the uncertainty summaries. [PsN documentation](https://github.com/UUPharmacometrics/PsN/tree/master/doc)
