# FDPCrawleR

Crawl a FAIR Data Point index and view saved results in an interactive dashboard.
Run the commands below from the project directory.

## Setup
Use R 4.5.1
Restore the R packages:

```sh
Rscript scripts/setup.R
```

## Crawl

Choose one profile:

```sh
Rscript scripts/run_pipeline.R --profile generic
# Or:
Rscript scripts/run_pipeline.R --profile erdera
```

`generic` uses `home.fairdatapoint.org`<br>
`erdera` uses `index.vp.ejprarediseases.org` and runs FAIR Champion tests for Minimal VP Metadata for Level 1 and Level 2 Compliance

Results are saved in `results/<profile>/<run-id>/`. <br>
`report.rds` contains the dashboard data.

## Open saved results

Replace `RUN_ID` with the saved run's directory name:

```sh
Rscript scripts/run_dashboard.R --run results/generic/RUN_ID
```

Use the corresponding `results/erdera/` path for ERDERA. 

**Load saved results** button in the dashboard switches to another saved
run or lets you browse for a `report.rds` file. Excel/CSV exports are available
through dashboard downloads. 


## Custom profile
Create your custom profile:

Copy `config/generic.yml` to `config/my-profile.yml` and edit it.

Run your custom profile:

```sh
Rscript scripts/run_pipeline.R --config config/my-profile.yml
```
