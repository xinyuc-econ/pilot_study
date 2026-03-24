# pilot_study

This project builds a reproducible research pipeline to study how state personal income taxes affect the residential location and interstate mobility of U.S. pilots. It combines FAA Airmen Certification records with state tax measures and state working-population data to construct pilot location, migration, and summary datasets for analysis and figures.

This repository is the clean working copy for the project. Legacy scripts are kept under `legacy_code/` and migrated into `code/` one functional chunk at a time.

## Project layout
- `code/00_setup/`: package loading, project paths, shared constants
- `code/01_ingest/`: source-aware raw input ingestion and harmonization
- `code/02_clean/`: sample restrictions and analysis-ready datasets
- `code/03_analysis/`: summary tables and analysis datasets
- `code/04_plots/`: figures generated from derived data
- `code/99_validation/`: temporary legacy-vs-clean validation scripts
- `code/utils/`: reusable helper functions
- `data/intermediate/`: generated pooled or harmonized datasets
- `data/derived/`: generated analysis-ready datasets
- `output/figures/` and `output/tables/`: exported outputs
- `tests/testthat/`: durable automated tests

## External raw data
Raw data lives outside this repository and is currently referenced directly in the setup script at `/Users/xinyuc/Documents/pilots/data`.

Expected subfolders under that external directory:
- `raw/airmen_data/`
- `raw/aviationdb_data/`
- `raw/tot_working_pop_weights/`
- `xwalks/`

If this local path changes again, update `code/00_setup/00_packages_paths.R` and keep the repo docs aligned with that change.

## Working pattern
- Keep legacy code in `legacy_code/`.
- Migrate one functional chunk at a time.
- Validate clean outputs against legacy behavior before moving to the next chunk.
- Keep plotting separate from data construction.
- The ingest layer is designed to normalize both `raw/airmen_data` and `raw/aviationdb_data` to a common pilot-year schema.
- FAA-specific cleaning currently uses a richer intermediate dataset built from `raw/airmen_data` so legacy summary statistics can be replicated before a later AviationDB redesign.
