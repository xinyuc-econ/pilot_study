# Purpose: shared cleaning helpers used by more than one production script.

load_state_fips_crosswalk <- function(paths) {
  read_excel(file.path(paths$xwalks, "StateFIPSicsprAB.xls")) |>
    clean_names() |>
    select("fips", "name", "ab") |>
    rename(
      statefull = "name",
      state = "ab"
    )
}
