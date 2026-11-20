# PsN simulation evaluation

Choose the intended use before running the engine. `reference_psn_vpc()` generates visual predictive-check outputs, `reference_psn_npc()` generates numerical predictive-check outputs, and `reference_psn_sse()` executes an estimation and simulation study. Each requires separate local approval of its corresponding manifest unit.

Declare the control stream, licensed provision, working directory, simulation counts and seed. For VPC also declare the binning rule, prediction correction, stratification and censoring where applicable. Review the invocation manifest, tool version, output paths and result-file layout. A raw PsN summary can support qualification once the project specifies and reviews its mapping to the decision criteria.

[PsN documentation](https://github.com/UUPharmacometrics/PsN/tree/master/doc)
