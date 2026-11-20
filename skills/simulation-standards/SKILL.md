# Simulation standards

Declare the population, regimen, covariate values, observation schedule and random seed. Specify the endpoint, summaries, uncertainty tier and decision bounds before simulation. Use the fitted model and review its translation and the simulated profiles before accepting results.

| Unit | Entry points and purpose |
|---|---|
| `mrgsolve_sim` | `reference_sim_compile()`, `reference_sim_translate_check()`, `reference_sim_physical_checks()` and `reference_simulate()` compile, check and simulate the model. |
| `uncertainty` | `reference_sim_uncertainty_draws()` and `reference_sim_install_check()` prepare and check parameter draws. `reference_simulate_full()` and `reference_sim_full_summary()` propagate and summarize the draws. |
| `exposure_metrics` | `reference_exposure_by_subject()`, `reference_exposure_summary()` and `reference_profile_summary()` compute exposure and profile summaries. |
| `bound_comparison` | `reference_bound_verdict()` compares supplied percentiles with their declared inclusive bounds. |

Each unit requires local approval. Record the fitted model, code and package versions, scenario inputs and parameter draws along with the output summaries. [mrgsolve user guide](https://mrgsolve.org/user-guide/)
