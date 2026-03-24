# Repository Structure

This repository is the clean working copy of the pilot-study pipeline. Legacy code is being migrated incrementally into a more reproducible structure. Raw input data stays outside the repo, while generated datasets, tables, and figures are organized by pipeline stage inside the repo.

## Top-Level Folders
- `code/`: active production pipeline
- `data/`: repo-managed generated datasets
- `output/`: exported figures and tables
- `legacy_code/`: archived legacy scripts kept for reference during migration
- `tests/`: durable automated tests
- `notes/`: migration tracking and project notes
- `draft/`: paper draft and related writeups

## Pipeline Stages
- `code/00_setup/`: package loading, path resolution, and shared constants
- `code/01_ingest/`: read external raw files from supported sources, standardize them, and build pooled intermediate datasets
- `code/02_clean/`: define the analytic sample and create derived datasets
- `code/03_analysis/`: build summary tables and analysis-ready outputs
- `code/04_plots/`: generate figures from derived data
- `code/99_validation/`: compare clean outputs against legacy outputs during migration
- `code/utils/`: reusable helper functions used across stages

## Data and Output Locations
- Raw input data is external and currently read from `/Users/xinyuc/Documents/pilots/data`
- Supported ingest sources currently include `raw/airmen_data` and `raw/aviationdb_data`
- Crosswalk files are external under `xwalks/`
- `data/intermediate/`: generated pooled or harmonized datasets
- `data/derived/`: generated analysis-ready datasets
- `output/figures/`: presentation figures
- `output/tables/`: exported summary tables

## Where New Work Goes
- New production scripts go in the appropriate `code/` stage folder
- Reusable functions go in `code/utils/`
- Temporary migration comparison scripts go in `code/99_validation/`
- New legacy files that are being retired go in the relevant subfolder under `legacy_code/`
- Legacy-compatible cleaning reads the FAA-rich intermediate dataset, not the minimal `pilot_ingest_panel.csv`

## Placement Rules
- Do not add new production logic to `legacy_code/`
- Do not mix plotting into cleaning scripts
- Do not duplicate reusable helper logic across stage scripts
- Keep scripts thin and pass information between stages through files, not workspace objects
