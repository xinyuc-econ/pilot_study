# Repository Guide

## Purpose
- This repository builds a clean, reproducible pipeline for a pilot study on whether state personal income taxes affect where U.S. pilots live and whether they move across states.
- The pipeline uses FAA Airmen Certification data, tax data, and state working-population data to produce cleaned datasets, summary tables, and figures while legacy code is migrated in small validated chunks.
- Keep the clean pipeline understandable and reproducible while legacy code is migrated in small functional chunks.
- See `structure.md` for the canonical guide to folder roles and where new work should go.

## Folder Rules
- `legacy_code/` is archival reference code. Do not add new production logic there.
- `code/` contains the active pipeline, split by purpose.
- `code/utils/` contains reusable functions only.
- `code/99_validation/` contains temporary migration checks that compare clean outputs with legacy outputs.
- `tests/` contains durable automated tests.
- `data/intermediate/` and `data/derived/` store generated datasets for this repo.
- `output/figures/` and `output/tables/` store presentation outputs only.

## Pipeline Conventions
- Keep `ingest`, `clean`, and `analysis` as the active production stages; analysis also owns plotting outputs.
- Scripts communicate through files, not objects left in the workspace.
- Keep production scripts thin: load setup, call helper functions, write outputs.
- Put shared constants and path resolution in `code/00_setup/00_packages_paths.R`.
- Every production script should state its inputs and outputs at the top.
- In `code/03_analysis/`, group generated outputs under numbered `Tables` and `Figures` comment headers using `#`, then label each table or figure block with numbered `##` subsections.
- The zero-tax striped overlay used in analysis plots is validated on this machine with `ggplot2 4.0.2` and `ggpattern 1.3.1`; keep that pair aligned if the exact legacy look is required.
- The clean ingest contract is a canonical pilot-year dataset with shared fields across FAA flat files and AviationDB yearly TSVs.

## External Data Policy
- Large raw input data stays outside the repo.
- The current setup script hard-codes the raw data root as `/Users/xinyuc/Documents/pilots/data`.
- If the raw data path changes, update the setup script and the repo docs together.
- Crosswalk files are also external and currently resolved from `/Users/xinyuc/Documents/pilots/data/xwalks`.
- Derived outputs may live inside this repo unless they become too large.

## Migration Workflow
- Migrate one functional chunk at a time.
- Validate each migrated chunk against legacy outputs before replacing downstream steps.
- Keep naming descriptive. Avoid temporary names like `m1` in clean code.
- Keep plotting work out of cleaning and validation scripts; place it with analysis outputs.
- For now, legacy-compatible cleaning depends on an FAA-rich intermediate dataset rather than the minimal source-agnostic ingest panel.
