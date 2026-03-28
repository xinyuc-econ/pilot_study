# Migration Tracker

Use this file to track which legacy chunks have clean replacements.

Suggested columns:

| Legacy script | Functional chunk | Clean replacement | Status | Validation notes |
| --- | --- | --- | --- | --- |
| `legacy_code/ingest/pool_all_raw_pilot_data.R` | Pool raw pilot data | TBD | Not started | |
| `legacy_code/cleaning/clean_pilot_data.R` | Clean pilot data and build outputs | TBD | Not started | |
| `legacy_code/analysis/mover_analysis.R` | Build ATR mover panel | `code/02_clean/06_build_pilot_mover_panel.R` | In progress | Validate against legacy-equivalent mover panel logic |
| `legacy_code/analysis/mover_analysis.R` | Mover summary and migration flow tables | `code/03_analysis/06_mover_tables.R` | In progress | Validate with `code/99_validation/validate_mover_outputs.R` |
| `legacy_code/cleaning/mb_mover_ols_bls.R` | Build pilot-level BLS tax-merged ATR datasets | `code/02_clean/07_build_pilots_atr_tax_merged_bls.R` | In progress | Validate airline mean dataset against a legacy-equivalent reconstruction |
| `legacy_code/cleaning/mb_mover_ols_bls.R` | Main BLS airline mean LPM table | `code/03_analysis/07_lpm_bls_airline_mean.R` | In progress | Validate sample row count and coefficients against legacy-equivalent logic |
| `legacy_code/cleaning/mb_mover_ols.R` | Build pilot-level SOI tax-merged ATR datasets | `code/02_clean/07_build_pilots_atr_tax_merged_bls.R` | In progress | Validate the SOI p95 dataset against a legacy-equivalent reconstruction |
| `legacy_code/cleaning/mb_mover_ols.R` | Main SOI mover LPM tables | `code/03_analysis/07_lpm_bls_airline_mean.R` | In progress | Validate SOI p95 sample and regression specification against the legacy script |
| `legacy_code/analysis/mw_reg_more_tax_bls.R` | Build MW regression datasets from merged pilot-tax inputs | `code/02_clean/08_build_mw_regression_datasets.R` | In progress | Validate the BLS airline mean case against a legacy-equivalent reconstruction |
| `legacy_code/analysis/mw_reg_more_tax_bls.R` | Combined MW regression table and case-specific binscatters | `code/03_analysis/08_mw_regressions.R` | In progress | Smoke-test five tax-measure cases and confirm one combined table plus five figures |
| `legacy_code/analysis/stock_reg_rewrite.R` | Build AFM stock datasets from merged pilot-tax inputs | `code/02_clean/09_build_afm_stock_datasets.R` | In progress | Validate balanced and unbalanced BLS airline mean datasets against legacy-equivalent reconstructions |
| `legacy_code/analysis/stock_reg_rewrite.R` | Combined AFM stock tables | `code/03_analysis/09_afm_stock_tables.R` | In progress | Smoke-test balanced and unbalanced five-case combined tables |
