# Migration Tracker

Use this file to track which legacy chunks have clean replacements.

Suggested columns:

| Legacy script | Functional chunk | Clean replacement | Status | Validation notes |
| --- | --- | --- | --- | --- |
| `legacy_code/ingest/pool_all_raw_pilot_data.R` | Pool raw pilot data | TBD | Not started | |
| `legacy_code/cleaning/clean_pilot_data.R` | Clean pilot data and build outputs | TBD | Not started | |
| `legacy_code/analysis/mover_analysis.R` | Build ATR mover panel | `code/02_clean/06_build_pilot_mover_panel.R` | In progress | Validate against legacy-equivalent mover panel logic |
| `legacy_code/analysis/mover_analysis.R` | Mover summary and migration flow tables | `code/03_analysis/06_mover_tables.R` | In progress | Validate with `code/99_validation/validate_mover_outputs.R` |
