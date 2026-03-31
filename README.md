# pilot_study

This project builds a reproducible research pipeline to study how state personal income taxes affect the residential location and interstate mobility of U.S. pilots. It combines AviationDB pilot records with state tax measures and state working-population data to construct pilot location, migration, and summary datasets for analysis and figures.

This repository is the clean working copy for the project. Legacy scripts are kept under `legacy_code/` and migrated into `code/` one functional chunk at a time.

## Project layout
- `code/00_setup/`: package loading, project paths, shared constants
- `code/01_ingest/`: source-aware raw input ingestion and harmonization
- `code/02_clean/`: sample restrictions, TAXSIM pipelines, and analysis-ready datasets
- `code/03_analysis/`: summary tables, tax/pilot analysis datasets, and figures generated from derived data
- `code/99_validation/`: temporary legacy-vs-clean validation scripts
- `code/utils/`: reusable helper functions
- `data/intermediate/`: generated pooled or harmonized datasets
- `data/derived/aviationdb/`: generated production datasets, PIT measure datasets, pilot-tax merged analysis datasets, TAXSIM inputs, and TAXSIM outputs
- `data/derived/faa/`: FAA-only reference and validation datasets
- `output/aviationdb/figures/` and `output/aviationdb/tables/`: production AviationDB outputs
- `output/faa/figures/` and `output/faa/tables/`: FAA-reference and overlap-validation outputs
- `tests/testthat/`: durable automated tests

## External raw data
Raw data lives outside this repository and is currently referenced directly in the setup script at `/Users/xinyuc/Documents/pilots/data`.

Expected subfolders under that external directory:
- `raw/airmen_data/`
- `raw/aviationdb_data/`
- `raw/bls/`
- `raw/soi/`
- `raw/tot_working_pop_weights/`
- `xwalks/`

If this local path changes again, update `code/00_setup/00_packages_paths.R` and keep the repo docs aligned with that change.

## Working pattern
- Keep legacy code in `legacy_code/`.
- Migrate one functional chunk at a time.
- Validate clean outputs against legacy behavior before moving to the next chunk.
- Keep plotting logic with analysis outputs, not in cleaning scripts.
- Keep post-TAXSIM PIT measure construction in `code/02_clean/`, separate from the TAXSIM runner itself.
- Keep merged pilot-tax analysis datasets in `code/02_clean/`, then generate binscatter figures from those datasets in `code/03_analysis/`.
- The ingest layer normalizes both `raw/airmen_data` and `raw/aviationdb_data` to a common pilot-year schema.
- AviationDB is now the production pilot source for the annual 2001-2022 analysis panel.
- FAA flat files are retained for overlap-year validation only.
- SOI-based tax-measure analyses currently use 2001-2022, while BLS-based tax-measure analyses use 2007-2022 because earlier BLS wage data were removed due to reporting issues.
