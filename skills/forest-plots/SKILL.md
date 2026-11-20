# Covariate forest plots

Declare the covariate contrasts, reference subject, analysis population and uncertainty source. Use `reference_forest_covstats()` and `reference_forest_covgrid()` to construct the covariate inputs. Use the draw reader for NONMEM output or `reference_forest_samples_nlmixr()` for an nlmixr fit.

Implement the fitted model's covariate equations in the analysis-level parameter function. Check its predictions against independent expected values with `reference_forest_verify()` before producing a plot. Use `reference_forest_parametric()` or `reference_forest_empirical()` according to the declared contrast, and `reference_forest_plot()` for the resulting table. Include the contrast, interval and reference population in the figure record.

The `forest` unit requires local method approval. [PMXForest documentation](https://github.com/pharmetheus/PMXForest)
