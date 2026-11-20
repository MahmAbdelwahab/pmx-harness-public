# Configure NONMEM and PsN

The harness dispatches to a user-supplied NONMEM and PsN installation or a locally available container image. Obtain software and licenses through your institution or the vendor. Keep installation media, license files and licensed images outside the repository.

## Native execution

1. Install NONMEM and PsN using your site's procedures. Configure the site-owned `psn.conf` and make the tools available on the execution PATH. Use `psn -nm_versions` to list the labels and record the selected label as `psn.nm_version` in the run configuration.
2. Copy the [example settings](../../harness.yaml.example) to the ignored machine-local `harness.yaml`. If you use the bbr estimation path, set `nonmem_home` (or `NONMEM_HOME`) to the installation root containing `run/nmfe75`. Set `nonmem_env_file` to a site-provided environment script when a batch process requires it to find the tools.
3. Select `native` provision in [engine settings](../../config/engine.yaml) or in the run configuration. Use the [environment check](../../tools/check_environment.R) to verify the setup before running an analysis.

## Containerized execution

1. Supply a local image with the selected NONMEM/PsN tools and configure its PsN registry. The dispatcher runs commands from the supplied image's PATH, using the caller's user and group IDs, with the project mounted at its host path. Configure `NONMEM_IMAGE` or the machine-local `nonmem_image` setting with the image tag.
2. Set `NONMEM_LICENSE` or the machine-local `nonmem_license` setting to the license file path. The wrapper requires a file, which it mounts read-only at `/nonmem/nm751/license/nonmem.lic`. Ensure your image uses that license location. Any additional mounts supplied through `NONMEM_EXTRA_MOUNTS` are read-only.
3. Select `containerized` provision in the engine settings or run configuration. Use the environment check to verify the local image, license path and tools. When needed, use the bbi container shim specified by the engine settings.

## R environment

For `r_provision: managed`, [setup_environment.R](../../setup_environment.R) installs the packages from the pinned MPN snapshot and the configured CRAN repository. For `r_provision: site`, the site R library is authoritative. The setup script refuses to install or shadow packages in that mode. The environment check compares installed versions against [the package lock](../../config/r-packages.lock); refer mismatches to the responsible reviewer.
