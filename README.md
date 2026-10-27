# FDPCrawleR

One configurable crawler and one dashboard. Crawl once, then reopen the saved
dashboard as often as needed without repeating network requests. Two built-in
profiles and custom YAML configurations are supported. This repository contains
the files needed to install and run the crawler and dashboard:

| Profile | Default index | Selected index states | ERDERA L1/L2 |
|---|---|---|---|
| `generic` | `https://home.fairdatapoint.org` | All returned states | No tests configured by default |
| `erdera` | `https://index.vp.ejprarediseases.org` | ACTIVE, INACTIVE | Both requested independently |

Index membership and fetch status are separate. All entries are retained in the
saved index, including unselected states and invalid/missing URLs. Each distinct
selected root is crawled once; shared index membership is preserved.

## Setup

Use **R 4.5.1** to reproduce the recorded environment. From a repository checkout:

```sh
Rscript scripts/setup.R
```

The first run bootstraps the pinned version of `renv` and restores the exact R
package versions in `renv.lock`, including transitive dependencies. Installation
requires access to the package repositories. Packages are installed into the
project library under `renv/library/` (with renv's package cache); global R
libraries are not modified. Installed packages and caches are not included in
the repository; restore them on each computer with the setup command.

`.Rprofile` activates this environment automatically when opening the project in
RStudio or starting R in the project directory. The pipeline/dashboard launchers
also activate it when invoked from another directory. A `.Rproj` file is not
required: open R in the project directory or use the command-line launchers.

`DESCRIPTION` declares the direct dependencies; `renv.lock` records their exact
versions and those of their dependencies. The lockfile uses the `explicit`
snapshot mode, so dependency discovery follows `DESCRIPTION`. Commit `.Rprofile`, `renv.lock`, `renv/activate.R` and
`renv/settings.json`. Libraries, caches and local renv state are ignored via
`renv/.gitignore`; do not copy them between operating systems.

R itself, native libraries and Pandoc are not installed by renv. On Debian/Ubuntu,
install the source-build prerequisites and Pandoc before restoring:

```sh
sudo apt-get update
sudo apt-get install -y librdf0-dev libraptor2-dev librasqal3-dev libcurl4-openssl-dev libssl-dev libxml2-dev pandoc
```

On macOS, source installation may require command-line build tools and the native
Redland/Raptor/Rasqal libraries. RStudio supplies Pandoc. `renv.lock` does not pin
these system tools; use the same R version and compatible system libraries when
reproducing a run.

To inspect the environment or deliberately update dependencies, run in R from
the project directory:

```r
renv::status()
# After intentionally installing/updating a dependency and updating DESCRIPTION:
# renv::snapshot(prompt = FALSE)
```

Running setup restores the lockfile; it does not silently update to the newest
package releases. Run provenance includes the lockfile checksum, and changing it
requires starting a new crawl rather than resuming checkpoints created with the
previous environment definition.

## Run an analysis

```sh
Rscript scripts/run_pipeline.R --profile generic
Rscript scripts/run_pipeline.R --profile erdera
```

These commands make live metadata requests. The ERDERA profile additionally submits
root URLs to the two ERDERA assessment endpoints declared in `config/erdera.yml`.
Configurations with `assessments: {}` have no assessment columns, logs or Table 5.
Any configuration can explicitly enable FAIR Champion tests; the profile name
does not select or suppress tests.

RStudio alternative (from the project directory):

```r
source("R/bootstrap.R")
run <- run_pipeline("generic")
# Or: run <- run_pipeline("erdera")
```

Use the launchers in `scripts/`. The crawler and dashboard do not require an
existing report or historical data. After a crawl, open its generated `report.rds`
as described below.

Configuration lives in `config/generic.yml` and `config/erdera.yml`. Use `states: ALL`
or a YAML list of the exact states to select. Index labels are not rewritten as
ACTIVE/INACTIVE. A different index can be selected independently of the profile:

```sh
Rscript scripts/run_pipeline.R --profile generic --index-url https://example.org
```

## Saved data and resuming a crawl

By default, each run creates its own directory under the project root. The run ID
contains a UTC timestamp and process ID:

```text
results/<profile>/<timestamp-pid>/
  config.rds           # Validated configuration used for this run
  index_raw.rds        # Original index response
  index.rds            # Normalized index, states and selection decisions
  source_versions.rds  # Checksums of tracked source/configuration/environment files
  checkpoints/
    00001.rds          # Saved crawl and optional assessments for the first FDP
    00002.rds          # The next distinct selected FDP
    ...
  report.rds           # Complete version-2 snapshot for the dashboard
```

The final `Saved run: ...` message prints the directory to pass to the dashboard.
`report.rds` contains configuration, index, per-FDP crawl evidence, assessment
results, timestamps and provenance. It is written after all selected FDPs have
been processed. A failed/incomplete metadata fetch can still be a recorded result;
completion of the pipeline does not imply every resource was reachable.

**The pipeline does not automatically write `summary.xlsx` or `table4.csv`.**
Excel and CSV downloads remain available in the dashboard and are generated only
when requested. Existing files from earlier runs are left untouched.

Use `output_dir` in YAML or `--output-dir /absolute/path/to/results` to change the
output location. Relative output/resume paths are resolved from the project root.
Saved data are ignored by Git and are not required to install the project.
Keep the entire run directory if you want to resume it; opening a completed report
requires only its `report.rds` plus the application code and dependencies.

Resume an interrupted run with its original profile or custom configuration and
any original index override:

```sh
Rscript scripts/run_pipeline.R --profile generic --resume results/generic/<run-id>
Rscript scripts/run_pipeline.R --config config/my-profile.yml --resume results/my-profile/<run-id>
```

A checkpoint is saved after crawling a whole FDP, before remote assessments, and
updated after the configured assessments finish. Resume reuses the saved index
and completed checkpoints, including recorded failures. An FDP interrupted before
its crawl checkpoint is saved is crawled again from its root; the in-memory URL
queue is not saved halfway through an FDP. If crawling finished but assessments
did not, resume retries assessments without repeating that FDP's crawl.

Resume requires matching configuration and source checksums. After changing code,
configuration or the dependency lockfile, start a new crawl. This check does not
prevent opening previously completed version-2 reports in the dashboard.

Each metadata request has a timeout. Both built-in profiles use
`max_documents_per_fdp: null` (no metadata-document limit). A positive integer
sets a per-FDP request budget; reaching it records incomplete coverage. Endpoint
reachability checks are separate from this metadata budget. There is no depth limit.

## Open the dashboard

```sh
Rscript scripts/run_dashboard.R --run results/generic/<run-id>
```

Or in RStudio:

```r
source("scripts/run_dashboard.R")
run_shared_dashboard("results/generic/<run-id>")
# Or, immediately after run <- run_pipeline("generic"):
# run_shared_dashboard(run$run_dir)
```

The report uses saved results only. Its global Index State multi-select defaults to all
available states and filters cards, tables, charts, evidence, test logs and downloads.
Clearing the selection shows empty results. It includes tables 1–4,
a metadata chart, the Sankey, full evidence/metadata fetch logs, CSV downloads,
and the summary workbook. Configured tests add outcome columns,
assessment logs and Table 5 failure reasons using their configured labels. No new network assessments occur
when opening or filtering the dashboard.

You can close the dashboard and run the same command later. It reads the saved
snapshot; opening and filtering do not contact the index, FDPs or assessment
services. **Load saved results** opens a picker with completed `report.rds` files
from the project's `results/`, the initial run's configured output directory and
the initial report's folder. Select a saved run and click **Open selected report**,
or use **Browse...** to upload a `report.rds` from another folder on your computer.
The browser chooses the native file dialog's starting folder; the saved-run list
provides direct access to project results.

Loading a report selects all its Index States and updates tables, charts,
assessment sections, evidence and downloads. An invalid file shows an error and
leaves the current report open. No new crawl or assessments run. Uploads are held
in the Shiny session; the original files and run directories are not overwritten.
The default upload limit is 512 MB (set `options(shiny.maxRequestSize = ...)` before
launching to override it). To collect newer metadata, run a new crawl, then load
its saved report. The launcher also accepts
the path to a `report.rds` file directly. Stop the local Shiny process with Ctrl+C
in the terminal (or Stop in RStudio).

## Custom configuration

Copy a built-in YAML file, choose a `profile` name for its output directory, and run:

```sh
Rscript scripts/run_pipeline.R --config /absolute/path/my-config.yml
Rscript scripts/run_pipeline.R --config /absolute/path/my-config.yml --resume results/my-profile/<run-id>
```

For example, save the following as `config/my-profile.yml` (replace the example
index URL with a real FDP index):

```yaml
profile: my-profile
index_url: https://index.example.org
states: [ACTIVE, INACTIVE]   # Or ALL to include every index state
assessments: {}             # No external tests
generalized: true
timeout_seconds: 15
max_documents_per_fdp: null # Or a positive integer for a bounded trial
output_dir: results
traversal:
  extends: standard-fdp-dcat
  infer_types_from_url: false
  resource_types:
    cohort:
      rdf_types: ["https://example.org/vocab/Cohort"]
      label: Cohort
      behaves_like: dataset
  relationships:
    - from: catalog
      predicate: "https://example.org/vocab/hasCohort"
      to: cohort
```

This adds Catalog → Cohort discovery and lets Cohort use the outgoing rules of a
Dataset, including Distribution and DataService links. These example IRIs define
an illustrative custom model; replace them with the RDF classes and predicates
actually used by your metadata. Give custom profiles their own `profile` name;
the built-in `generic` presentation is limited to standard DCAT layers.

Changing YAML affects future crawls. A saved report continues to use the
configuration recorded in that run. Profile configuration is edited in YAML;
there is no configuration editor in the dashboard.

`--profile` and `--config` are mutually exclusive. Relative `--config` paths resolve
from the calling shell's working directory. Output and resume paths retain their
existing project-directory semantics. `--index-url` can override the YAML index.
In R, use `run_pipeline(config_file = "/absolute/path/my-config.yml")` or pass a
configuration list as `config`. Validation runs before fetching the index.

Required top-level fields are `profile`, `index_url`, `states`, `timeout_seconds`
and `output_dir`. `assessments` defaults to no tests; an omitted or null
`max_documents_per_fdp` means unlimited. Unknown fields and malformed rules fail
with an error rather than silently changing coverage.

The Index API remains `/index/entries/all`, returning `clientUrl` and `state`.
The configurable traversal does not change this API contract.

### Structural traversal rules

`traversal.extends: standard-fdp-dcat` loads `config/traversal-standard.yml`.
The base used by `generic` supports FDP → Catalog → Dataset → Distribution
→ DataService, FDP → Catalog → Dataset → DataService, and FDP → Catalog → DataService,
including incomplete prefixes of these routes. Dataset → DataService uses `dcat:accessService`.
Dataset discovery accepts `dcat:dataset`, or `dct:hasPart`/LDP containment followed
by a `dcat:Dataset` type. Generic never infers a class from a URL path and does
not reason over custom subclasses. A custom class also explicitly declared as
`dcat:Dataset` is processed as a Dataset, without extension-specific rules.
Unknown resources remain in traversal evidence as `unsupported_type`.

Patient registry, biobank, guideline and legacy service URL aliases are configured
only in `config/erdera.yml`. The direct Catalog → DataService route is shared with generic.
Custom configuration adds only the differences, using:

| Field | Meaning |
|---|---|
| `traversal.infer_types_from_url` | Optional legacy URL role inference; false by default, enabled explicitly for ERDERA. Only configured roles are eligible. |
| `traversal.resource_types.<id>.rdf_types` | List of absolute RDF class IRIs identifying this resource type. |
| `traversal.resource_types.<id>.label` | Human-readable type name in traversal evidence. |
| `traversal.resource_types.<id>.behaves_like` | Optional existing type ID whose outgoing rules are inherited. This does not assert RDF class equivalence. |
| `traversal.resource_types.<id>.terminal` | Explicitly allow this type to end a metadata route; defaults to false, or inherits from `behaves_like`. |
| `traversal.relationships` | List of mappings with `from`, `predicate`, `to`: source type ID, absolute child predicate IRI, destination type ID. |

Type IDs use lowercase letters, digits and underscores, starting with a letter.
Rules are additive to the built-in rules. A terminal type cannot have outgoing
rules; set `terminal: false` if extending a terminal type with children. A
relationship's destination supplies a discovery role even when a child omits
`rdf:type`. Child RDF types can refine that role within the permitted destination types. No remote ontology
loading, subclass reasoning or automatic SHACL interpretation occurs.

This configuration describes **which links to traverse**, not mandatory metadata
fields. Properties such as title, publisher or license need not be enumerated.
An unknown custom predicate without LDP membership is not inferred to be a child
link; declare it explicitly if it belongs to the metadata structure.

### FAIR Champion assessments

`assessments` is a mapping keyed by a stable test ID. Each test has a unique `label`,
an `endpoint` and optional `timeout_seconds` (default 120). ERDERA's actual L1
configuration is:

```yaml
assessments:
  l1:
    label: L1
    endpoint: https://tests.ostrails.eu/community-tests/assess/test/erdera_core_vp_metadata
    timeout_seconds: 120
```

`config/erdera.yml` contains both L1 and L2. Add other tests as sibling entries.
Use the **Execution Endpoint** from the [FAIR Champion registry](https://tools.ostrails.eu/champion/tests/),
not the test description or API-definition URL. The adapter POSTs JSON with
`resource_identifier` set to the FDP root URL and expects a FAIR Champion JSON-LD
`@graph` containing one `ftr:TestResult` (`pass`, `fail`, or `indeterminate`).
Choose tests suitable for FDP root identifiers; dataset-only tests are not
redirected to individual datasets. Tests run independently. HTTP, timeout and
response-format failures are recorded as `error`, not as compliance failures.
Reports display the configured test labels and preserve response bodies and logs.

## Traversal scope and interpretation

All pipeline runs use the same configurable queue and Turtle parser. The queue
follows declared direct links and scoped LDP container membership, retaining links
found on only one side. Publisher, license and arbitrary vocabulary links are not
followed unless explicitly configured as structural relationships.

Each document URL is fetched at most once per FDP crawl, including failed fetches.
Expansion is tracked separately by owner, URL and role: a document downloaded for
classification can still be expanded later when another link supplies its role.
Cycles terminate through this bookkeeping; shared resources retain all incoming
edges. There is no depth limit. The standard layer-specific tables are built from
the same cache after the queue has finished.

The evidence explorer includes `traversal_documents` (type, RDF types, fetch URL,
expansion status), `traversal_edges` (all configured parent-child links) and
`additional_relationships` (unknown scoped LDP relationships, not traversed).
Unknown resource types without applicable rules remain `unsupported_type` and
make the FDP incomplete; a successful download alone does not establish coverage.
Fetch/parse failures, unknown scoped relationships and optional document-limit
exhaustion also prevent a complete traversal result. Explicit terminal types are
allowed to have no child rules.

The Sankey and terminal-path Table 2 retain the FDP → Catalog → Resource
→ Distribution → DataService and Resource → DataService projection. They are not a visualization of arbitrary
configured routes. When a projected branch stops but the queue finds children,
`Outside standard route` replaces a false absence claim. It does not mean the
queue failed. Use `traversal_documents` and `additional_relationships` in the
evidence explorer to inspect coverage across the full configured graph. `generalized` is kept
for compatibility with earlier callers;
it does not disable the queue or select assessment tests. Turtle remains the only
supported metadata serialization.

Table 4 warning counts include recorded structural cases in custom descendants;
failed roots have NA. Unknown types and unknown-relationship coverage notices do
not count as structural violations. A zero is not assurance of complete traversal.
Tables 1–4 use stable labels, layout and calculations,
with the global Index State checkbox filter applied throughout. Multiple selected
states appear side by side, without extra Total columns. Table 4 has one row per
FDP with the original warning occurrence total; ERDERA retains L1/L2 outcomes.
The metadata chart uses the successful-fetch counts from Table 1.

| Dashboard table | Meaning |
|---|---|
| Table 1 | Distinct metadata URLs per structural layer and state; successful retrieval and Turtle parsing versus failed/skipped attempts. Document-limit details remain in `fetch_log`. |
| Table 2 | Distinct complete terminal URL paths. This measures discovered routes, not successful HTTP requests to every terminal service. |
| Table 3 | Recorded structural-case occurrences. Categories can overlap; terminal services have `NA` for outgoing-structure checks. |
| Table 4 | One row per FDP, with configured assessment outcomes and distinct structural warning occurrences. Unassessed or failed roots have `NA` counts. |
| Table 5 | Explanations derived from saved assessment logs; present only when assessments are configured. |

## Minimal publication and project layout

The published files are:

```text
.gitignore
.Rprofile
DESCRIPTION
README.md
renv.lock
renv/
  .gitignore
  activate.R
  settings.json
R/                         # Crawler, traversal, assessments and dashboard helpers
config/                    # Built-in profiles and shared traversal rules
scripts/
  setup.R                  # Restore the dependency environment
  run_pipeline.R           # Crawl or resume an interrupted run
  run_dashboard.R          # Open the dashboard with a saved report
report/
  dashboard.Rmd            # Current Shiny dashboard
```

Keep the environment files in this distribution. `R/bootstrap.R` loads
`renv/activate.R`; setup uses `DESCRIPTION`, `renv.lock` and renv settings to
restore the project environment. The pipeline also records checksums of
`DESCRIPTION`, `renv.lock`, `.Rprofile` and renv bootstrap/settings files.
Installed packages under `renv/library/` and caches under `renv/local/` are created
locally and are never published.

The root `.gitignore` uses an allowlist. New source files under `R/` and profiles
under `config/` are included automatically, subject to the local-state exclusions.
Only the three launchers shown above and `report/dashboard.Rmd` are allowed in
`scripts/` and `report/`. New top-level files, launchers or report assets require
an explicit exception in `.gitignore`.

Tests and their runner (`tests/`, `scripts/test.R`), supplementary documentation
(`docs/`), GitHub Actions workflows (`.github/`) and the RStudio `.Rproj` file are
not included in this minimal publication. If present in a development workspace,
they stay local; ignoring them does not delete them. Results, checkpoints,
`Deprecated/`, research scripts, exported tables/figures, local `.env` files and
R session history also stay local.

There are no automatic tests or crawl jobs on GitHub in this distribution. Run
the commands in this README using R. The interactive dashboard requires a running
R/Shiny process; publishing the source repository does not start a dashboard.

Review the file list before committing. `.gitignore` prevents adding untracked
files; it does not remove files that were already committed to another repository.
