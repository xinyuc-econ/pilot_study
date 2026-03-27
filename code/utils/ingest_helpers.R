# Purpose: helper functions for source-aware pilot ingest.

# Shared Ingest Utilities ----

ingest_sources <- c("faa_flat", "aviationdb")

trim_missing <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

extract_year_from_path <- function(path) {
  year_text <- str_extract(basename(path), "\\d{4}")

  if (is.na(year_text)) {
    stop(sprintf("Could not extract a 4-digit year from '%s'.", path), call. = FALSE)
  }

  as.integer(year_text)
}

parse_year_subset <- function(years = NULL) {
  if (is.null(years) || length(years) == 0) {
    return(NULL)
  }

  if (length(years) == 1 && is.character(years)) {
    years <- unlist(strsplit(years, ","))
  }

  years <- trimws(as.character(years))
  years <- years[years != ""]

  if (length(years) == 0) {
    return(NULL)
  }

  sort(unique(as.integer(years)))
}

# Source Validation and File Discovery ----

validate_ingest_source <- function(source) {
  if (!source %in% ingest_sources) {
    stop(
      sprintf(
        "Unsupported ingest source '%s'. Expected one of: %s",
        source,
        paste(ingest_sources, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

filter_years <- function(discovered_years, years) {
  years <- parse_year_subset(years)

  if (is.null(years)) {
    return(discovered_years)
  }

  missing_years <- setdiff(years, discovered_years)

  if (length(missing_years) > 0) {
    stop(
      sprintf(
        "Requested years not available for this source: %s",
        paste(missing_years, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  intersect(discovered_years, years)
}

# FAA flat files arrive as paired basic and cert files for the same year.
discover_faa_flat_inputs <- function(paths, years = NULL) {
  basic_files <- list.files(
    paths$raw_airmen_data,
    pattern = "^(RELDOMCB_\\d{4}|PILOT_BASIC_\\d{4})\\.csv$",
    full.names = TRUE
  )

  cert_files <- list.files(
    paths$raw_airmen_data,
    pattern = "^(RELDOMCC_\\d{4}|PILOT_CERT_\\d{4})\\.csv$",
    full.names = TRUE
  )

  basic_index <- tibble(
    year = vapply(basic_files, extract_year_from_path, integer(1)),
    basic_path = basic_files
  )

  cert_index <- tibble(
    year = vapply(cert_files, extract_year_from_path, integer(1)),
    cert_path = cert_files
  )

  inputs <- inner_join(basic_index, cert_index, by = "year") |>
    arrange(year)

  available_years <- inputs$year
  selected_years <- filter_years(available_years, years)

  inputs <- filter(inputs, year %in% selected_years) |>
    mutate(source = "faa_flat")

  if (nrow(inputs) == 0) {
    stop("No FAA flat-file inputs were discovered.", call. = FALSE)
  }

  inputs
}

# AviationDB already ships one TSV per year, so discovery is simpler.
discover_aviationdb_inputs <- function(paths, years = NULL) {
  files <- list.files(
    paths$raw_aviationdb_data,
    pattern = "^\\d{4}\\.tsv$",
    full.names = TRUE
  )

  inputs <- tibble(
    year = vapply(files, extract_year_from_path, integer(1)),
    aviationdb_path = files
  ) |>
    arrange(year)

  available_years <- inputs$year
  selected_years <- filter_years(available_years, years)

  inputs <- filter(inputs, year %in% selected_years) |>
    mutate(source = "aviationdb")

  if (nrow(inputs) == 0) {
    stop("No AviationDB inputs were discovered.", call. = FALSE)
  }

  inputs
}

discover_ingest_inputs <- function(paths, source = "faa_flat", years = NULL) {
  validate_ingest_source(source)

  switch(
    source,
    faa_flat = discover_faa_flat_inputs(paths, years),
    aviationdb = discover_aviationdb_inputs(paths, years)
  )
}

# Shared Validation ----

# Both ingest paths are expected to produce one pilot row per year.
assert_unique_pilot_year <- function(data, source) {
  duplicates <- data |>
    count(unique_id, name = "n") |>
    filter(n > 1)

  if (nrow(duplicates) > 0) {
    stop(
      sprintf(
        "Source '%s' produced duplicate pilot rows within a year for %d unique_id values.",
        source,
        nrow(duplicates)
      ),
      call. = FALSE
    )
  }
}

# Source Readers ----

# This reader keeps only the minimal shared schema used by both ingest sources.
read_faa_flat_year <- function(year, basic_path, cert_path) {
  basic <- read_csv(
    basic_path,
    na = c("", "NA"),
    show_col_types = FALSE,
    name_repair = "unique_quiet",
    col_types = cols(.default = col_character())
  ) |>
    clean_names() |>
    transmute(
      unique_id = trim_missing(unique_id),
      state = trim_missing(state),
      zip_code = trim_missing(zip_code)
    )

  cert <- read_csv(
    cert_path,
    na = c("", "NA"),
    show_col_types = FALSE,
    name_repair = "unique_quiet",
    col_types = cols(.default = col_character())
  ) |>
    clean_names() |>
    transmute(
      unique_id = trim_missing(unique_id),
      certificate_type = trim_missing(type),
      certificate_level = trim_missing(level)
    ) |>
    filter(certificate_type == "P") |>
    select(-"certificate_type")

  assert_unique_pilot_year(cert, "faa_flat")

  cert |>
    left_join(basic, by = "unique_id") |>
    transmute(
      year = as.integer(year),
      unique_id = unique_id,
      state = state,
      zip_code = zip_code,
      certificate_level = certificate_level,
      source = "faa_flat"
    )
}

# This reader preserves the richer FAA columns needed to replicate legacy cleaning.
read_faa_flat_year_rich <- function(year, basic_path, cert_path) {
  basic <- read_csv(
    basic_path,
    na = c("", "NA"),
    show_col_types = FALSE,
    name_repair = "unique_quiet",
    col_types = cols(.default = col_character())
  ) |>
    clean_names() |>
    transmute(
      unique_id = trim_missing(unique_id),
      first_name = trim_missing(first_name),
      last_name = trim_missing(last_name),
      street_1 = trim_missing(street_1),
      street_2 = trim_missing(street_2),
      city = trim_missing(city),
      state = trim_missing(state),
      zip_code = trim_missing(zip_code),
      country = trim_missing(country),
      region = trim_missing(region),
      med_class = trim_missing(med_class),
      med_date = trim_missing(med_date),
      med_exp_date = trim_missing(med_exp_date)
    )

  cert <- read_csv(
    cert_path,
    na = c("", "NA"),
    show_col_types = FALSE,
    name_repair = "unique_quiet",
    col_types = cols(.default = col_character())
  ) |>
    clean_names() |>
    transmute(
      unique_id = trim_missing(unique_id),
      first_name = trim_missing(first_name),
      last_name = trim_missing(last_name),
      type = trim_missing(type),
      level = trim_missing(level),
      expire_date = trim_missing(expire_date)
    ) |>
    filter(type == "P")

  assert_unique_pilot_year(cert, "faa_flat")

  cert |>
    left_join(basic, by = "unique_id", suffix = c("_cert", "")) |>
    mutate(
      first_name = coalesce(first_name, first_name_cert),
      last_name = coalesce(last_name, last_name_cert)
    ) |>
    transmute(
      year = as.integer(year),
      unique_id = unique_id,
      first_name = first_name,
      last_name = last_name,
      street_1 = street_1,
      street_2 = street_2,
      city = city,
      state = state,
      zip_code = zip_code,
      country = country,
      region = region,
      med_class = med_class,
      med_date = med_date,
      med_exp_date = med_exp_date,
      type = type,
      level = level,
      expire_date = expire_date,
      source = "faa_flat"
    )
}

# AviationDB already matches the future minimal contract closely.
read_aviationdb_year <- function(year, aviationdb_path) {
  data <- read_tsv(
    aviationdb_path,
    na = c("", "NA"),
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) |>
    transmute(
      unique_id = trim_missing(unique_id),
      state = trim_missing(state),
      zip_code = trim_missing(zip_code),
      certificate_level = trim_missing(certificate_level)
    )

  assert_unique_pilot_year(data, "aviationdb")

  data |>
    transmute(
      year = as.integer(year),
      unique_id = unique_id,
      state = state,
      zip_code = zip_code,
      certificate_level = certificate_level,
      source = "aviationdb"
    )
}

# Dataset Builders ----

# This is the source-agnostic canonical ingest output used for future migration work.
build_pilot_ingest_dataset <- function(paths, source = "faa_flat", years = NULL) {
  inputs <- discover_ingest_inputs(paths, source, years)

  rows <- lapply(seq_len(nrow(inputs)), function(i) {
    row <- inputs[i, , drop = FALSE]

    if (source == "faa_flat") {
      return(read_faa_flat_year(row$year[[1]], row$basic_path[[1]], row$cert_path[[1]]))
    }

    read_aviationdb_year(row$year[[1]], row$aviationdb_path[[1]])
  })

  bind_rows(rows) |>
    arrange(year, unique_id)
}

# This FAA-only builder exists so the cleaning stage can still replicate legacy outputs.
build_faa_rich_ingest_dataset <- function(paths, years = NULL) {
  inputs <- discover_ingest_inputs(paths, source = "faa_flat", years = years)

  rows <- lapply(seq_len(nrow(inputs)), function(i) {
    row <- inputs[i, , drop = FALSE]

    read_faa_flat_year_rich(
      year = row$year[[1]],
      basic_path = row$basic_path[[1]],
      cert_path = row$cert_path[[1]]
    )
  })

  bind_rows(rows) |>
    arrange(year, unique_id)
}
