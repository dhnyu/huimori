daily_buffer_radii <- function() {
  c(100, 500, 2000, 5000)
}

daily_key_cols <- function() {
  c("TMSID", "TMSID2", "date")
}

daily_grid_key_cols <- function() {
  c("gid", "date")
}

daily_aod_terms <- function(radii = daily_buffer_radii()) {
  paste0("aod_", radii, "m")
}

daily_era5_land_terms <- function(radii = daily_buffer_radii()) {
  unlist(
    lapply(c("t2m", "u10", "v10", "sp", "ssr", "tp"), function(variable) {
      paste0(variable, "_", radii, "m")
    }),
    use.names = FALSE
  )
}

daily_blh_terms <- function(radii = daily_buffer_radii()) {
  paste0("blh_", radii, "m")
}

daily_static_terms <- function(radii = daily_buffer_radii()) {
  c("d_road", "dem", "dsm", "mtpi", "mtpi_1km", landuse_fixed_terms(radii))
}

daily_predictor_terms <- function(radii = daily_buffer_radii()) {
  c(
    daily_static_terms(radii),
    daily_aod_terms(radii),
    daily_era5_land_terms(radii),
    daily_blh_terms(radii)
  )
}

summarize_daily_correct <- function(
  data,
  timeflag = "datehour",
  dateflag = "date",
  min_valid_hours = 18L
) {
  required_cols <- c("TMSID", timeflag, dateflag, "PM10", "PM25")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop("summarize_daily_correct() is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  min_valid_hours <- as.integer(min_valid_hours)
  if (length(min_valid_hours) != 1L || is.na(min_valid_hours) || min_valid_hours < 1L) {
    stop("min_valid_hours must be one positive integer.")
  }
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, TMSID := as.character(TMSID)]
  dt[, `.daily_timestamp` := get(timeflag)]
  dt[, date_s := as.Date(get(dateflag))]
  if (anyNA(dt$.daily_timestamp) || anyNA(dt$date_s)) {
    stop("Daily correct timestamps and KST dates must not be missing.")
  }
  dt[, PM10 := as.numeric(PM10)]
  dt[, PM25 := as.numeric(PM25)]
  dt[PM10 < 0, PM10 := NA_real_]
  dt[PM25 < 0, PM25 := NA_real_]
  mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  hourly <- dt[, .(
    PM10 = mean_or_na(PM10),
    PM25 = mean_or_na(PM25),
    n_source_rows = .N
  ), by = .(TMSID, date_s, .daily_timestamp)]
  daily <- hourly[, .(
    n_hours = .N,
    n_valid_hours_PM10 = sum(!is.na(PM10)),
    n_valid_hours_PM25 = sum(!is.na(PM25)),
    n_duplicate_rows = sum(n_source_rows - 1L),
    PM10 = mean_or_na(PM10),
    PM25 = mean_or_na(PM25)
  ), by = .(TMSID, date_s)]
  daily[, PM10flag := as.integer(n_valid_hours_PM10 < min_valid_hours)]
  daily[, PM25flag := as.integer(n_valid_hours_PM25 < min_valid_hours)]
  daily[PM10flag == 1L, PM10 := NA_real_]
  daily[PM25flag == 1L, PM25 := NA_real_]
  data.table::setcolorder(
    daily,
    c(
      "TMSID", "date_s", "PM10flag", "PM25flag", "PM10", "PM25",
      "n_hours", "n_valid_hours_PM10", "n_valid_hours_PM25", "n_duplicate_rows"
    )
  )
  tibble::as_tibble(daily)
}

daily_month_dates <- function(month) {
  if (length(month) != 1L || is.na(month) || !grepl("^[0-9]{4}-[0-9]{2}$", month)) {
    stop("Expected one month formatted as YYYY-MM.")
  }
  start <- as.Date(paste0(month, "-01"))
  seq(start, seq(start, by = "month", length.out = 2L)[[2L]] - 1L, by = "day")
}

as_kst_date <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  as.Date(format(x, tz = "Asia/Seoul", format = "%Y-%m-%d"))
}

daily_key_frame <- function(x, id_cols = c("TMSID", "TMSID2")) {
  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }
  key_cols <- c(id_cols, "date")
  missing_cols <- setdiff(key_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop("Daily key frame is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  out <- x[, key_cols, drop = FALSE]
  out$date <- as.Date(out$date)
  out
}

assert_daily_branch <- function(
  x,
  month,
  label,
  required_cols = character(),
  id_cols = c("TMSID", "TMSID2")
) {
  key_cols <- c(id_cols, "date")
  required <- unique(c(key_cols, required_cols))
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0L) {
    stop(label, " is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  keys <- daily_key_frame(x, id_cols)
  duplicate_idx <- anyDuplicated(keys)
  if (duplicate_idx > 0L) {
    stop(label, " has duplicated ", paste(key_cols, collapse = "/"), " keys at row ", duplicate_idx, ".")
  }
  bad_month <- format(keys$date, "%Y-%m") != month
  if (anyNA(keys$date) || any(bad_month)) {
    stop(label, " contains missing dates or dates outside branch month ", month, ".")
  }
  invisible(x)
}

assert_same_daily_keys <- function(
  x,
  reference,
  label,
  id_cols = c("TMSID", "TMSID2")
) {
  x_key <- daily_key_frame(x, id_cols)
  reference_key <- daily_key_frame(reference, id_cols)
  signature <- function(key) {
    values <- c(key[id_cols], list(format(key$date, "%Y-%m-%d")))
    sort(do.call(paste, c(values, sep = "\r")))
  }
  if (!identical(signature(x_key), signature(reference_key))) {
    stop(label, " key set does not match the base ", paste(c(id_cols, "date"), collapse = "/"), " key set.")
  }
  invisible(x)
}

prepare_daily_location_history <- function(site_history, invalid = c("drop", "error")) {
  invalid <- match.arg(invalid)
  required <- c(
    "TMSID", "TMSID2", "date_start", "date_end", "lon", "lat",
    "site_type", "coords_google"
  )
  missing_cols <- setdiff(required, names(site_history))
  if (length(missing_cols) > 0L) {
    stop("Daily location history is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  history <- site_history |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      TMSID = as.character(TMSID),
      TMSID2 = as.character(TMSID2),
      date_start_kst = as_kst_date(date_start),
      date_end_kst = as_kst_date(date_end)
    ) |>
    dplyr::arrange(TMSID, date_start_kst, date_end_kst, TMSID2)

  if (anyNA(history[c("TMSID", "TMSID2", "date_start_kst", "date_end_kst", "lon", "lat")])) {
    stop("Daily location history contains missing key, interval, or coordinate values.")
  }
  if (anyDuplicated(history$TMSID2)) {
    stop("Daily location history TMSID2 values must be globally unique.")
  }
  invalid_rows <- which(history$date_start_kst > history$date_end_kst)
  if (length(invalid_rows) > 0L) {
    invalid_ids <- paste(history$TMSID2[invalid_rows], collapse = ", ")
    if (invalid == "error") {
      stop("Daily location history has reversed intervals: ", invalid_ids)
    }
    warning("Dropping reversed daily location intervals with no active dates: ", invalid_ids)
    history <- history[-invalid_rows, , drop = FALSE]
  }

  overlap <- history |>
    dplyr::group_by(TMSID) |>
    dplyr::mutate(previous_end = dplyr::lag(date_end_kst)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(previous_end), date_start_kst <= previous_end)
  if (nrow(overlap) > 0L) {
    stop(
      "Daily location history has overlapping intervals for TMSID(s): ",
      paste(unique(overlap$TMSID), collapse = ", ")
    )
  }
  attr(history, "dropped_invalid_intervals") <- length(invalid_rows)
  history
}

build_sf_monitors_correct_daily <- function(
  measurements,
  site_history,
  date_range = NULL,
  min_valid_hours = 18L
) {
  history <- prepare_daily_location_history(site_history, invalid = "drop")
  dropped_invalid_intervals <- attr(history, "dropped_invalid_intervals")
  required_measurement_cols <- c("TMSID", "date", "datehour", "PM10", "PM25")
  missing_cols <- setdiff(required_measurement_cols, names(measurements))
  if (length(missing_cols) > 0L) {
    stop("Daily measurements are missing columns: ", paste(missing_cols, collapse = ", "))
  }
  measurement_date <- as.Date(measurements[["date"]])
  if (is.null(date_range)) {
    date_range <- range(measurement_date, na.rm = TRUE)
  }
  date_range <- as.Date(date_range)
  if (length(date_range) != 2L || anyNA(date_range) || date_range[[1L]] > date_range[[2L]]) {
    stop("date_range must contain two ordered, non-missing dates.")
  }
  keep_measurements <- measurement_date >= date_range[[1L]] & measurement_date <= date_range[[2L]]
  measurement_dt <- data.table::as.data.table(
    data.table::copy(measurements[keep_measurements, ])
  )
  measurement_dt[, TMSID := as.character(TMSID)]
  measurement_dt[, `.date_kst` := as.Date(date)]

  daily_pm <- summarize_daily_correct(
    data = measurement_dt,
    timeflag = "datehour",
    dateflag = ".date_kst",
    min_valid_hours = min_valid_hours
  ) |>
    dplyr::rename(date = date_s)

  history <- history |>
    dplyr::mutate(
      clipped_start = pmax(date_start_kst, date_range[[1L]]),
      clipped_end = pmin(date_end_kst, date_range[[2L]])
    ) |>
    dplyr::filter(clipped_start <= clipped_end) |>
    dplyr::arrange(TMSID, clipped_start, TMSID2) |>
    dplyr::group_by(TMSID) |>
    dplyr::mutate(lon2 = dplyr::lag(lon), lat2 = dplyr::lag(lat)) |>
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dist_m = if (is.na(lon2) || is.na(lat2)) {
        NA_real_
      } else {
        geosphere::distGeo(c(lon, lat), c(lon2, lat2))
      }
    ) |>
    dplyr::ungroup()

  daily_sites <- history |>
    dplyr::rowwise() |>
    dplyr::mutate(date = list(seq(clipped_start, clipped_end, by = "day"))) |>
    dplyr::ungroup() |>
    tidyr::unnest(date) |>
    dplyr::mutate(date = as.Date(date))
  assert_unique_key(daily_sites, daily_key_cols(), "daily location skeleton")
  if (any(daily_sites$date < date_range[[1L]] | daily_sites$date > date_range[[2L]])) {
    stop("Daily location skeleton contains dates outside date_range.")
  }

  unmatched <- dplyr::anti_join(
    daily_pm |> dplyr::select(TMSID, date),
    daily_sites |> dplyr::select(TMSID, date),
    by = c("TMSID", "date")
  )
  if (nrow(unmatched) > 0L) {
    stop(
      "Daily measurements could not be assigned to an active KST location interval: ",
      nrow(unmatched), " TMSID/date rows."
    )
  }

  base_n <- nrow(daily_sites)
  out <- daily_sites |>
    dplyr::left_join(daily_pm, by = c("TMSID", "date"))
  if (nrow(out) != base_n) {
    stop("Daily monitor join changed row count: base=", base_n, ", joined=", nrow(out), ".")
  }
  out <- out |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
    sf::st_transform(5179) |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::relocate(dplyr::any_of(c("date", "PM10", "PM25")), .after = TMSID2)
  assert_unique_key(out, daily_key_cols(), "sf_monitors_correct_daily")
  attr(out, "dropped_invalid_intervals") <- dropped_invalid_intervals
  attr(out, "min_valid_hours") <- as.integer(min_valid_hours)
  out
}

subset_daily_monitor_month <- function(x, month) {
  out <- x |>
    dplyr::filter(format(date, "%Y-%m") == month)
  assert_daily_branch(out, month, "sf_monitors_correct_month")
  out
}

build_grid_daily_month <- function(grid, month) {
  if (!inherits(grid, "sf")) {
    stop("Grid daily input must be an sf object.")
  }
  if (is.na(sf::st_crs(grid))) {
    stop("Grid daily input must have a valid CRS.")
  }
  if (nrow(grid) < 1L) {
    stop("Grid daily input must contain at least one point.")
  }
  if (any(sf::st_is_empty(grid))) {
    stop("Grid daily input contains empty geometries.")
  }
  assert_unique_key(grid, "gid", "grid daily chunk")
  dates <- daily_month_dates(month)
  out <- grid[rep(seq_len(nrow(grid)), each = length(dates)), , drop = FALSE]
  out$date <- rep(dates, times = nrow(grid))
  out <- out |>
    dplyr::relocate(date, .after = gid)
  assert_daily_branch(
    out,
    month,
    "sf_grid_daily_month",
    id_cols = "gid"
  )
  attr(out, "branch_month") <- month
  attr(out, "buffer_radii_m") <- daily_buffer_radii()
  out
}

daily_branch_month <- function(x) {
  month <- attr(x, "branch_month", exact = TRUE)
  if (is.null(month)) {
    dates <- unique(format(as.Date(x$date), "%Y-%m"))
    if (length(dates) != 1L || is.na(dates)) {
      stop("Daily branch does not contain exactly one month.")
    }
    month <- dates
  }
  daily_month_dates(month)
  month
}

daily_entity_buffers <- function(
  base,
  radii,
  context,
  id_cols = c("TMSID", "TMSID2")
) {
  key_cols <- c(id_cols, "date")
  assert_unique_key(base, key_cols, paste0(context, " base"))
  locations <- base |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(id_cols)), .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(id_cols))
  make_feature_buffer_set(
    points_sf = locations,
    radii = radii,
    id_cols = id_cols,
    fallback_crs = 5179,
    row_col = ".daily_buffer_row",
    context = context
  )
}

daily_location_buffers <- function(base, radii, context) {
  daily_entity_buffers(
    base = base,
    radii = radii,
    context = context,
    id_cols = c("TMSID", "TMSID2")
  )
}

extract_raster_stack_by_daily_buffer <- function(
  raster,
  dates,
  buffer_set,
  value_prefix,
  radii,
  id_cols = c("TMSID", "TMSID2")
) {
  dates <- as.Date(dates)
  if (terra::nlyr(raster) != length(dates)) {
    stop(value_prefix, " raster layer/date count mismatch.")
  }
  location_meta <- buffer_set_meta(
    buffer_set,
    id_cols,
    context = value_prefix
  )
  row_col <- buffer_set$row_col
  out <- tidyr::expand_grid(
    .daily_buffer_row = location_meta[[row_col]],
    date = dates
  ) |>
    dplyr::left_join(location_meta, by = stats::setNames(row_col, ".daily_buffer_row")) |>
    dplyr::select(dplyr::all_of(id_cols), date, dplyr::all_of(row_col))

  for (radius in radii) {
    buffers <- get_feature_buffer(
      buffer_set,
      radius,
      id_cols,
      context = value_prefix
    ) |>
      sf::st_transform(terra::crs(raster))
    extracted <- exactextractr::exact_extract(
      raster,
      buffers,
      fun = "mean",
      force_df = TRUE,
      progress = FALSE
    )
    value_cols <- grep("^mean", names(extracted), value = TRUE)
    if (length(value_cols) != length(dates) || nrow(extracted) != nrow(location_meta)) {
      stop(value_prefix, " extraction dimensions do not match locations and dates.")
    }
    values <- as.numeric(as.vector(t(as.matrix(extracted[, value_cols, drop = FALSE]))))
    values[is.nan(values)] <- NA_real_
    out[[paste0(value_prefix, "_", radius, "m")]] <- values
  }
  out |>
    dplyr::select(-dplyr::all_of(row_col))
}

extract_aod_daily_month <- function(
  base,
  month,
  aod_dir,
  radii = daily_buffer_radii(),
  id_cols = c("TMSID", "TMSID2"),
  output_label = "df_feat_correct_aod_daily"
) {
  radii <- normalize_buffer_radii(radii, "daily AOD")
  key_cols <- c(id_cols, "date")
  assert_daily_branch(base, month, "AOD daily base", id_cols = id_cols)
  keys <- daily_key_frame(base, id_cols)
  terms <- daily_aod_terms(radii)
  out <- keys
  for (term in terms) {
    out[[term]] <- NA_real_
  }
  buffers <- daily_entity_buffers(base, radii, "daily AOD buffer", id_cols)
  dates <- daily_month_dates(month)
  files <- file.path(
    aod_dir,
    paste0("MCD19A2_Daily_Composite_", strftime(dates, "%Y%j"), ".tif")
  )
  available <- file.exists(files)
  for (i in which(available)) {
    raster <- terra::rast(files[[i]])
    extracted <- extract_raster_stack_by_daily_buffer(
      raster = raster,
      dates = dates[[i]],
      buffer_set = buffers,
      value_prefix = "aod",
      radii = radii,
      id_cols = id_cols
    ) |>
      dplyr::select(-dplyr::any_of(".daily_buffer_row"))
    out <- out |>
      dplyr::rows_update(extracted, by = key_cols, unmatched = "ignore")
  }
  assert_daily_branch(out, month, output_label, terms, id_cols)
  assert_same_daily_keys(out, base, output_label, id_cols)
  attr(out, "buffer_radii_m") <- radii
  attr(out, "missing_dates") <- dates[!available]
  out
}

neighbor_months <- function(month) {
  current <- as.Date(paste0(month, "-01"))
  format(
    c(
      seq(current, by = "-1 month", length.out = 2L)[[2L]],
      current,
      seq(current, by = "month", length.out = 2L)[[2L]]
    ),
    "%Y-%m"
  )
}

era5_valid_time <- function(raster, label) {
  stamp <- suppressWarnings(as.numeric(sub(".*valid_time=", "", names(raster))))
  if (length(stamp) != terra::nlyr(raster) || anyNA(stamp)) {
    stop(label, " does not expose a parseable valid_time for every layer.")
  }
  as.POSIXct(stamp, origin = "1970-01-01", tz = "UTC")
}

aggregate_era5_kst_daily <- function(rasters, variable, month, fun) {
  parts <- lapply(seq_along(rasters), function(i) {
    idx <- grep(paste0("^", variable, "_"), names(rasters[[i]]))
    if (length(idx) == 0L) {
      stop("ERA5 input ", i, " is missing variable ", variable, ".")
    }
    raster <- rasters[[i]][[idx]]
    list(raster = raster, time = era5_valid_time(raster, paste0("ERA5 ", variable)))
  })
  raster <- do.call(c, lapply(parts, `[[`, "raster"))
  time_utc <- do.call(c, lapply(parts, `[[`, "time"))
  order_idx <- order(time_utc)
  raster <- raster[[order_idx]]
  time_utc <- time_utc[order_idx]
  if (anyDuplicated(time_utc)) {
    stop("ERA5 ", variable, " contains duplicated UTC timestamps.")
  }
  date_kst <- as.Date(time_utc, tz = "Asia/Seoul")
  target_dates <- daily_month_dates(month)
  keep <- date_kst %in% target_dates
  raster <- raster[[which(keep)]]
  date_kst <- date_kst[keep]
  time_utc <- time_utc[keep]
  coverage <- data.frame(date = target_dates) |>
    dplyr::left_join(
      tibble::tibble(date = date_kst, time_utc = time_utc) |>
        dplyr::group_by(date) |>
        dplyr::summarise(n_hours = dplyr::n_distinct(time_utc), .groups = "drop"),
      by = "date"
    )
  if (anyNA(coverage$n_hours) || any(coverage$n_hours != 24L)) {
    bad <- coverage$date[is.na(coverage$n_hours) | coverage$n_hours != 24L]
    stop(
      "ERA5 ", variable, " does not contain 24 unique UTC hours for KST date(s): ",
      paste(bad, collapse = ", ")
    )
  }
  daily <- terra::tapp(
    raster,
    index = match(date_kst, target_dates),
    fun = fun,
    na.rm = TRUE
  )
  names(daily) <- format(target_dates, "%Y-%m-%d")
  attr(daily, "hourly_coverage") <- coverage
  daily
}

era5_land_archives <- function(era5_dir, month) {
  months <- neighbor_months(month)
  paths <- file.path(
    era5_dir,
    paste0("ERA5_Land_", sub("-", "_", months), ".nc")
  )
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop("Missing adjacent ERA5-Land archive(s): ", paste(missing, collapse = ", "))
  }
  paths
}

era5_blh_files <- function(era5_dir, month) {
  months <- neighbor_months(month)
  paths <- file.path(
    era5_dir,
    paste0("ERA5_BLH_", sub("-", "_", months), ".nc")
  )
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop("Missing adjacent ERA5-BLH file(s): ", paste(missing, collapse = ", "))
  }
  paths
}

extract_era5_land_daily_month <- function(
  base,
  month,
  era5_dir,
  radii = daily_buffer_radii(),
  id_cols = c("TMSID", "TMSID2"),
  output_label = "df_feat_correct_era5_land_daily"
) {
  radii <- normalize_buffer_radii(radii, "daily ERA5-Land")
  key_cols <- c(id_cols, "date")
  assert_daily_branch(base, month, "ERA5-Land daily base", id_cols = id_cols)
  archives <- era5_land_archives(era5_dir, month)
  temp_dir <- tempfile(pattern = paste0("era5_land_", gsub("-", "", month), "_"))
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  nc_files <- vapply(seq_along(archives), function(i) {
    extract_dir <- file.path(temp_dir, as.character(i))
    dir.create(extract_dir)
    status <- utils::unzip(archives[[i]], files = "data_0.nc", exdir = extract_dir)
    path <- file.path(extract_dir, "data_0.nc")
    if (length(status) < 1L || !file.exists(path)) {
      stop("Could not extract data_0.nc from ", archives[[i]], ".")
    }
    path
  }, character(1))
  rasters <- lapply(nc_files, terra::rast)
  buffers <- daily_entity_buffers(base, radii, "daily ERA5-Land buffer", id_cols)
  specs <- list(t2m = "mean", u10 = "mean", v10 = "mean", sp = "mean", ssr = "sum", tp = "sum")
  feature_parts <- lapply(names(specs), function(variable) {
    daily_raster <- aggregate_era5_kst_daily(rasters, variable, month, specs[[variable]])
    extracted <- extract_raster_stack_by_daily_buffer(
      raster = daily_raster,
      dates = daily_month_dates(month),
      buffer_set = buffers,
      value_prefix = variable,
      radii = radii,
      id_cols = id_cols
    )
    attr(extracted, "hourly_coverage") <- attr(daily_raster, "hourly_coverage")
    extracted
  })
  out <- Reduce(
    function(x, y) dplyr::left_join(x, y, by = key_cols),
    c(list(daily_key_frame(base, id_cols)), feature_parts)
  )
  t2m_cols <- paste0("t2m_", radii, "m")
  out <- out |>
    dplyr::mutate(dplyr::across(dplyr::all_of(t2m_cols), function(x) x - 273.15))
  terms <- daily_era5_land_terms(radii)
  assert_daily_branch(out, month, output_label, terms, id_cols)
  assert_same_daily_keys(out, base, output_label, id_cols)
  attr(out, "buffer_radii_m") <- radii
  attr(out, "hourly_coverage") <- lapply(feature_parts, attr, "hourly_coverage")
  attr(out, "source_files") <- archives
  out
}

extract_era5_blh_daily_month <- function(
  base,
  month,
  era5_dir,
  radii = daily_buffer_radii(),
  id_cols = c("TMSID", "TMSID2"),
  output_label = "df_feat_correct_era5_blh_daily"
) {
  radii <- normalize_buffer_radii(radii, "daily ERA5-BLH")
  key_cols <- c(id_cols, "date")
  assert_daily_branch(base, month, "ERA5-BLH daily base", id_cols = id_cols)
  files <- era5_blh_files(era5_dir, month)
  rasters <- lapply(files, terra::rast)
  daily_raster <- aggregate_era5_kst_daily(rasters, "blh", month, "mean")
  buffers <- daily_entity_buffers(base, radii, "daily ERA5-BLH buffer", id_cols)
  extracted <- extract_raster_stack_by_daily_buffer(
    raster = daily_raster,
    dates = daily_month_dates(month),
    buffer_set = buffers,
    value_prefix = "blh",
    radii = radii,
    id_cols = id_cols
  )
  out <- daily_key_frame(base, id_cols) |>
    dplyr::left_join(extracted, by = key_cols)
  terms <- daily_blh_terms(radii)
  assert_daily_branch(out, month, output_label, terms, id_cols)
  assert_same_daily_keys(out, base, output_label, id_cols)
  attr(out, "buffer_radii_m") <- radii
  attr(out, "hourly_coverage") <- attr(daily_raster, "hourly_coverage")
  attr(out, "source_files") <- files
  out
}

collect_daily_annual_frames <- function(x) {
  if (is.data.frame(x)) {
    return(list(x))
  }
  if (!is.list(x)) {
    return(list())
  }
  unlist(lapply(x, collect_daily_annual_frames), recursive = FALSE)
}

merge_correct_daily_month <- function(
  base,
  aod,
  era5_land,
  era5_blh,
  annual_features,
  month,
  radii = daily_buffer_radii()
) {
  radii <- normalize_buffer_radii(radii, "daily merged")
  assert_daily_branch(base, month, "daily merged base")
  feature_specs <- list(
    aod = list(data = aod, terms = daily_aod_terms(radii)),
    era5_land = list(data = era5_land, terms = daily_era5_land_terms(radii)),
    era5_blh = list(data = era5_blh, terms = daily_blh_terms(radii))
  )
  for (name in names(feature_specs)) {
    spec <- feature_specs[[name]]
    assert_daily_branch(spec$data, month, paste0("daily feature ", name), spec$terms)
    assert_same_daily_keys(spec$data, base, paste0("daily feature ", name))
  }

  year <- as.integer(substr(month, 1L, 4L))
  annual_frames <- collect_daily_annual_frames(annual_features)
  if (length(annual_frames) == 0L) {
    stop("No annual feature frames were supplied for daily static merge.")
  }
  static_terms <- daily_static_terms(radii)
  static <- dplyr::bind_rows(annual_frames) |>
    dplyr::filter(year == .env$year) |>
    dplyr::select(TMSID, TMSID2, year, dplyr::all_of(static_terms)) |>
    dplyr::mutate(TMSID = as.character(TMSID), TMSID2 = as.character(TMSID2))
  assert_unique_key(static, c("TMSID", "TMSID2", "year"), "daily annual-static features")

  out <- sf::st_drop_geometry(base) |>
    dplyr::mutate(
      TMSID = as.character(TMSID),
      TMSID2 = as.character(TMSID2),
      date = as.Date(date),
      year = as.integer(year)
    )
  base_n <- nrow(out)
  for (name in names(feature_specs)) {
    spec <- feature_specs[[name]]
    out <- out |>
      dplyr::left_join(
        spec$data |> dplyr::select(dplyr::all_of(daily_key_cols()), dplyr::all_of(spec$terms)),
        by = daily_key_cols()
      )
    if (nrow(out) != base_n) {
      stop("Daily feature join changed row count for ", name, ".")
    }
  }
  out <- out |>
    dplyr::left_join(static, by = c("TMSID", "TMSID2", "year"))
  if (nrow(out) != base_n) {
    stop("Daily annual-static join changed row count.")
  }
  predictors <- daily_predictor_terms(radii)
  missing_predictors <- setdiff(predictors, names(out))
  if (length(missing_predictors) > 0L) {
    stop("Daily merged output is missing predictors: ", paste(missing_predictors, collapse = ", "))
  }
  yearly_dynamic <- grep("^(aod|blh)_yearly", names(out), value = TRUE)
  if (length(yearly_dynamic) > 0L) {
    stop("Daily merged output contains excluded yearly dynamic features: ", paste(yearly_dynamic, collapse = ", "))
  }
  metadata_cols <- setdiff(names(out), predictors)
  out <- out |>
    dplyr::select(dplyr::all_of(metadata_cols), dplyr::all_of(predictors))
  actual_predictors <- names(out)[names(out) %in% predictors]
  if (!identical(actual_predictors, predictors)) {
    stop("Correct daily predictor names or order do not match daily_predictor_terms().")
  }
  assert_daily_branch(out, month, "df_feat_correct_merged_daily", predictors)
  assert_same_daily_keys(out, base, "df_feat_correct_merged_daily")
  attr(out, "buffer_radii_m") <- radii
  attr(out, "predictor_terms") <- predictors
  out
}

merge_grid_daily_month <- function(
  base,
  aod,
  era5_land,
  era5_blh,
  annual_features,
  month = daily_branch_month(base),
  radii = daily_buffer_radii()
) {
  radii <- normalize_buffer_radii(radii, "grid daily merged")
  id_cols <- "gid"
  key_cols <- daily_grid_key_cols()
  assert_daily_branch(
    base,
    month,
    "grid daily merged base",
    required_cols = c("x", "y"),
    id_cols = id_cols
  )
  feature_specs <- list(
    aod = list(data = aod, terms = daily_aod_terms(radii)),
    era5_land = list(data = era5_land, terms = daily_era5_land_terms(radii)),
    era5_blh = list(data = era5_blh, terms = daily_blh_terms(radii))
  )
  for (name in names(feature_specs)) {
    spec <- feature_specs[[name]]
    assert_daily_branch(
      spec$data,
      month,
      paste0("grid daily feature ", name),
      spec$terms,
      id_cols
    )
    assert_same_daily_keys(
      spec$data,
      base,
      paste0("grid daily feature ", name),
      id_cols
    )
  }

  year <- as.integer(substr(month, 1L, 4L))
  annual_frames <- collect_daily_annual_frames(annual_features)
  if (length(annual_frames) == 0L) {
    stop("No annual grid feature frames were supplied for daily static merge.")
  }
  static_terms <- daily_static_terms(radii)
  annual_frames <- lapply(annual_frames, function(frame) {
    if (inherits(frame, "sf")) {
      frame <- sf::st_drop_geometry(frame)
    }
    required <- c("gid", "x", "y", "year", static_terms)
    missing_cols <- setdiff(required, names(frame))
    if (length(missing_cols) > 0L) {
      stop(
        "Annual grid feature frame is missing columns: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    frame |>
      dplyr::filter(as.integer(year) == .env$year) |>
      dplyr::select(gid, x, y, year, dplyr::all_of(static_terms))
  })
  static <- dplyr::bind_rows(annual_frames)
  locations <- base |>
    sf::st_drop_geometry() |>
    dplyr::distinct(gid, x, y)
  static <- static |>
    dplyr::semi_join(locations, by = c("gid", "x", "y")) |>
    dplyr::mutate(year = as.integer(year))
  assert_unique_key(static, c("gid", "x", "y", "year"), "grid daily annual-static features")
  missing_static <- locations |>
    dplyr::anti_join(static, by = c("gid", "x", "y"))
  extra_static <- static |>
    dplyr::anti_join(locations, by = c("gid", "x", "y"))
  if (nrow(static) != nrow(locations) || nrow(missing_static) > 0L || nrow(extra_static) > 0L) {
    stop("Grid daily annual-static key set does not match the base grid chunk.")
  }

  out <- base |>
    sf::st_drop_geometry() |>
    dplyr::mutate(date = as.Date(date), year = year)
  base_n <- nrow(out)
  for (name in names(feature_specs)) {
    spec <- feature_specs[[name]]
    out <- out |>
      dplyr::left_join(
        spec$data |>
          dplyr::select(dplyr::all_of(key_cols), dplyr::all_of(spec$terms)),
        by = key_cols
      )
    if (nrow(out) != base_n) {
      stop("Grid daily feature join changed row count for ", name, ".")
    }
  }
  out <- out |>
    dplyr::left_join(static, by = c("gid", "x", "y", "year"))
  if (nrow(out) != base_n) {
    stop("Grid daily annual-static join changed row count.")
  }

  predictors <- daily_predictor_terms(radii)
  missing_predictors <- setdiff(predictors, names(out))
  if (length(missing_predictors) > 0L) {
    stop(
      "Grid daily merged output is missing predictors: ",
      paste(missing_predictors, collapse = ", ")
    )
  }
  yearly_dynamic <- grep("^(aod|blh)_yearly", names(out), value = TRUE)
  if (length(yearly_dynamic) > 0L) {
    stop(
      "Grid daily merged output contains excluded yearly dynamic features: ",
      paste(yearly_dynamic, collapse = ", ")
    )
  }
  metadata_cols <- setdiff(names(out), predictors)
  out <- out |>
    dplyr::select(dplyr::all_of(metadata_cols), dplyr::all_of(predictors))
  actual_predictors <- names(out)[names(out) %in% predictors]
  if (!identical(actual_predictors, predictors)) {
    stop("Grid daily predictor names or order do not match daily_predictor_terms().")
  }
  assert_daily_branch(
    out,
    month,
    "df_feat_grid_merged_daily",
    predictors,
    id_cols
  )
  assert_same_daily_keys(out, base, "df_feat_grid_merged_daily", id_cols)
  attr(out, "buffer_radii_m") <- radii
  attr(out, "predictor_terms") <- predictors
  out
}
