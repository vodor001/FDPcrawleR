# FDPCrawleR

Crawl a FAIR Data Point index and view saved results in an interactive dashboard.
Run the commands below from the project directory.

## Setup

Use R 4.5.1 and Pandoc (included with RStudio). Internet access is required for
installation and crawling.

On Debian/Ubuntu, install system dependencies first:

```sh
sudo apt-get update
sudo apt-get install -y librdf0-dev libraptor2-dev librasqal3-dev libcurl4-openssl-dev libssl-dev libxml2-dev pandoc
```

On macOS, building packages from source may require command-line build tools and
Redland/Raptor/Rasqal libraries.

Restore the R packages from `renv.lock`:

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

`generic` uses `home.fairdatapoint.org` and all index states, without external
assessments. `erdera` uses `index.vp.ejprarediseases.org`, selects ACTIVE/INACTIVE
and runs L1/L2 assessments.

Results are saved in `results/<profile>/<run-id>/`. The final `Saved run:` message
prints the path. `report.rds` contains the dashboard data; `checkpoints/` and the
other RDS files allow an interrupted run to resume.

## Open saved results

Replace `RUN_ID` with the saved run's directory name:

```sh
Rscript scripts/run_dashboard.R --run results/generic/RUN_ID
```

Use the corresponding `results/erdera/` path for ERDERA. Reopen the same report
any time without another crawl. **Load saved results** switches to another saved
run or lets you browse for a `report.rds` file. Excel/CSV exports are available
through dashboard downloads. Stop the dashboard with Ctrl+C.

## Resume

Keep the complete run directory and use the original profile/configuration and
any original overrides. Code and configuration must be unchanged.

```sh
Rscript scripts/run_pipeline.R --profile generic --resume results/generic/RUN_ID
```

Completed FDP checkpoints are reused. An FDP interrupted before its checkpoint
was saved is crawled again.

## Custom profile

Copy `config/generic.yml` or `config/erdera.yml` to `config/my-profile.yml`.
Edit `profile`, `index_url`, `states`, timeouts and `output_dir` as needed.
`traversal` controls resource types and links; `assessments: {}` disables external
tests. `max_documents_per_fdp: null` means no metadata-document limit.

```sh
Rscript scripts/run_pipeline.R --config config/my-profile.yml
```

Use `--config` instead of `--profile`, including when resuming. Optional command-line
overrides are `--index-url URL` and `--output-dir DIR`. Run either launcher with
`--help` to see its arguments.
