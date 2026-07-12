prediction_pollutant_label <- function(pollutant) {
  ifelse(pollutant == "PM25", "PM2.5", pollutant)
}

prediction_concentration_unit <- function() {
  "\u00b5g/m\u00b3"
}

prediction_map_palette <- function(n = 256L) {
  grDevices::hcl.colors(n, palette = "YlOrRd", rev = FALSE)
}

prediction_map_index_values <- function(map_index) {
  if (is.data.frame(map_index)) {
    pollutant <- unique(as.character(map_index$pollutant))
    year <- unique(as.integer(map_index$year))
  } else if (is.list(map_index)) {
    pollutant <- as.character(map_index$pollutant)
    year <- as.integer(map_index$year)
  } else {
    stop("map_index must be a data frame or list.")
  }
  if (length(pollutant) != 1L || !pollutant %in% c("PM10", "PM25")) {
    stop("map_index must identify exactly one pollutant: PM10 or PM25.")
  }
  if (length(year) != 1L || is.na(year)) {
    stop("map_index must identify exactly one year.")
  }
  list(pollutant = pollutant, year = year)
}

prediction_chunk_id <- function(prediction_df) {
  required_cols <- c("gid", "x", "y", "layer")
  missing_cols <- setdiff(required_cols, names(prediction_df))
  if (length(missing_cols) > 0L) {
    stop("Prediction chunk is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  layer_values <- paste(sort(unique(as.character(prediction_df$layer))), collapse = "-")
  key <- sprintf(
    "x%s_%s_y%s_%s_gid%s_%s_n%s_l%s",
    round(min(prediction_df$x, na.rm = TRUE)),
    round(max(prediction_df$x, na.rm = TRUE)),
    round(min(prediction_df$y, na.rm = TRUE)),
    round(max(prediction_df$y, na.rm = TRUE)),
    min(prediction_df$gid, na.rm = TRUE),
    max(prediction_df$gid, na.rm = TRUE),
    nrow(prediction_df),
    layer_values
  )
  gsub("[^A-Za-z0-9_-]+", "_", key)
}

prediction_chunk_raster <- function(xyz, value_col, crs = "EPSG:5179", fallback_resolution = 30) {
  unique_x <- sort(unique(xyz$x))
  unique_y <- sort(unique(xyz$y))
  if (length(unique_x) > 1L && length(unique_y) > 1L) {
    return(terra::rast(xyz, crs = crs, type = "xyz"))
  }

  resolution <- fallback_resolution
  if (length(unique_x) > 1L) {
    resolution <- min(diff(unique_x))
  } else if (length(unique_y) > 1L) {
    resolution <- min(diff(unique_y))
  }
  if (!is.finite(resolution) || resolution <= 0) {
    resolution <- fallback_resolution
  }

  template <- terra::rast(
    terra::ext(
      min(xyz$x, na.rm = TRUE) - resolution / 2,
      max(xyz$x, na.rm = TRUE) + resolution / 2,
      min(xyz$y, na.rm = TRUE) - resolution / 2,
      max(xyz$y, na.rm = TRUE) + resolution / 2
    ),
    resolution = resolution,
    crs = crs
  )
  points <- terra::vect(xyz, geom = c("x", "y"), crs = crs)
  terra::rasterize(points, template, field = value_col, fun = "mean")
}

write_prediction_chunk_raster <- function(
  prediction_df,
  output_root = file.path("outputs", "prediction_maps"),
  crs = "EPSG:5179",
  overwrite = TRUE,
  gdal_options = c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL=6")
) {
  required_cols <- c("gid", "x", "y", "layer", "year")
  missing_cols <- setdiff(required_cols, names(prediction_df))
  if (length(missing_cols) > 0L) {
    stop("Prediction chunk is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  pollutant_cols <- intersect(c("PM10", "PM25"), names(prediction_df))
  if (length(pollutant_cols) != 1L) {
    stop("Prediction chunk must contain exactly one pollutant column: PM10 or PM25.")
  }
  pollutant <- pollutant_cols[[1L]]
  year <- unique(as.integer(prediction_df$year))
  if (length(year) != 1L || is.na(year)) {
    stop("Prediction chunk must contain exactly one year.")
  }

  chunk_id <- prediction_chunk_id(prediction_df)
  chunk_dir <- file.path(output_root, "chunks", pollutant, paste0("year=", year))
  dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
  raster_path <- file.path(
    chunk_dir,
    sprintf("%d_%s_chunk_%s.tif", year, pollutant, chunk_id)
  )
  metadata_path <- sub("\\.tif$", ".csv", raster_path)

  xyz <- data.frame(
    x = as.numeric(prediction_df$x),
    y = as.numeric(prediction_df$y),
    value = as.numeric(prediction_df[[pollutant]])
  )
  names(xyz)[3L] <- pollutant

  raster_chunk <- prediction_chunk_raster(xyz, value_col = pollutant, crs = crs)
  names(raster_chunk) <- pollutant
  terra::writeRaster(
    x = raster_chunk,
    filename = raster_path,
    overwrite = overwrite,
    datatype = "FLT4S",
    gdal = gdal_options
  )

  gid_int <- as.integer(prediction_df$gid)
  metadata <- data.frame(
    raster_path = raster_path,
    metadata_path = metadata_path,
    pollutant = pollutant,
    year = year,
    chunk_id = chunk_id,
    layer_values = paste(sort(unique(as.character(prediction_df$layer))), collapse = "|"),
    n_rows = nrow(prediction_df),
    n_gid = length(unique(gid_int)),
    gid_min = min(gid_int, na.rm = TRUE),
    gid_max = max(gid_int, na.rm = TRUE),
    gid_has_na = anyNA(gid_int),
    gid_has_duplicates = anyDuplicated(gid_int) > 0L,
    gid_is_sequence = identical(sort(unique(gid_int)), seq_len(nrow(prediction_df))),
    x_min = min(prediction_df$x, na.rm = TRUE),
    x_max = max(prediction_df$x, na.rm = TRUE),
    y_min = min(prediction_df$y, na.rm = TRUE),
    y_max = max(prediction_df$y, na.rm = TRUE),
    n_xy = nrow(unique(data.frame(x = prediction_df$x, y = prediction_df$y))),
    xy_has_duplicates = anyDuplicated(prediction_df[c("x", "y")]) > 0L,
    value_min = min(prediction_df[[pollutant]], na.rm = TRUE),
    value_max = max(prediction_df[[pollutant]], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  utils::write.csv(metadata, metadata_path, row.names = FALSE, na = "")

  c(raster = raster_path, metadata = metadata_path)
}

build_prediction_chunk_manifest <- function(chunk_files) {
  files <- unlist(chunk_files, recursive = TRUE, use.names = FALSE)
  metadata_files <- files[grepl("\\.csv$", files)]
  if (length(metadata_files) < 1L) {
    stop("No chunk metadata files were found.")
  }
  manifest <- do.call(
    rbind,
    lapply(metadata_files, utils::read.csv, stringsAsFactors = FALSE)
  )
  manifest$year <- as.integer(manifest$year)
  manifest$n_rows <- as.integer(manifest$n_rows)
  manifest$n_gid <- as.integer(manifest$n_gid)
  manifest$n_xy <- as.integer(manifest$n_xy)
  manifest$gid_has_na <- as.logical(manifest$gid_has_na)
  manifest$gid_has_duplicates <- as.logical(manifest$gid_has_duplicates)
  manifest$gid_is_sequence <- as.logical(manifest$gid_is_sequence)
  manifest$xy_has_duplicates <- as.logical(manifest$xy_has_duplicates)

  missing_rasters <- manifest$raster_path[!file.exists(manifest$raster_path)]
  if (length(missing_rasters) > 0L) {
    stop("Manifest references missing chunk rasters: ", paste(head(missing_rasters), collapse = ", "))
  }
  duplicated_chunks <- manifest[duplicated(manifest[c("pollutant", "year", "chunk_id")]), ]
  if (nrow(duplicated_chunks) > 0L) {
    stop("Duplicate pollutant/year/chunk_id rows found in chunk manifest.")
  }
  manifest[order(manifest$pollutant, manifest$year, manifest$chunk_id), ]
}

prediction_map_index <- function(years, pollutants = c("PM10", "PM25")) {
  out <- expand.grid(
    pollutant = pollutants,
    year = as.integer(years),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$pollutant, out$year), ]
  rownames(out) <- NULL
  dplyr::group_by(out, pollutant, year) |>
    targets::tar_group()
}

write_annual_prediction_raster <- function(
  manifest,
  map_index,
  output_root = file.path("outputs", "prediction_maps"),
  expected_chunks = 593L,
  boundary_sf = NULL,
  overwrite = TRUE,
  gdal_options = c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL=6")
) {
  idx <- prediction_map_index_values(map_index)
  rows <- manifest[manifest$pollutant == idx$pollutant & manifest$year == idx$year, ]
  if (!is.null(expected_chunks) && nrow(rows) != expected_chunks) {
    stop(
      "Expected ", expected_chunks, " chunks for ",
      idx$year, " ", idx$pollutant, ", got ", nrow(rows), "."
    )
  }
  if (nrow(rows) < 1L) {
    stop("No chunk rasters found for ", idx$year, " ", idx$pollutant, ".")
  }
  if (any(rows$gid_has_na) || any(rows$gid_has_duplicates) || any(rows$xy_has_duplicates)) {
    stop("Chunk-level gid/xy validation failed for ", idx$year, " ", idx$pollutant, ".")
  }

  out_dir <- file.path(output_root, "rasters", idx$pollutant)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, sprintf("%d_%s.tif", idx$year, idx$pollutant))

  rasters <- lapply(rows$raster_path, terra::rast)
  annual <- if (length(rasters) == 1L) {
    rasters[[1L]]
  } else {
    do.call(terra::mosaic, c(rasters, list(fun = "mean")))
  }
  names(annual) <- idx$pollutant

  if (!is.null(boundary_sf)) {
    annual <- terra::mask(annual, terra::vect(sf::st_transform(boundary_sf, terra::crs(annual))))
  }

  terra::writeRaster(
    x = annual,
    filename = out_path,
    overwrite = overwrite,
    datatype = "FLT4S",
    gdal = gdal_options
  )
  out_path
}

parse_annual_prediction_raster_path <- function(paths) {
  data.frame(
    raster_path = paths,
    pollutant = basename(dirname(paths)),
    year = as.integer(sub("^([0-9]{4})_.*$", "\\1", basename(paths))),
    stringsAsFactors = FALSE
  )
}

calculate_prediction_color_ranges <- function(
  annual_raster_paths,
  probs = c(0.01, 0.99),
  sample_size_per_raster = 1000000L
) {
  if (length(probs) != 2L || anyNA(probs) || probs[1L] < 0 || probs[2L] > 1 || probs[1L] >= probs[2L]) {
    stop("probs must be two increasing probabilities between 0 and 1.")
  }
  annual_raster_paths <- unlist(annual_raster_paths, recursive = TRUE, use.names = FALSE)
  meta <- parse_annual_prediction_raster_path(annual_raster_paths)
  out <- lapply(split(meta, meta$pollutant), function(df) {
    vals <- unlist(lapply(df$raster_path, function(path) {
      r <- terra::rast(path)
      if (!is.null(sample_size_per_raster) &&
          is.finite(sample_size_per_raster) &&
          terra::ncell(r) > sample_size_per_raster) {
        sampled <- terra::spatSample(
          r,
          size = sample_size_per_raster,
          method = "regular",
          na.rm = TRUE,
          values = TRUE
        )
        as.numeric(sampled[[1L]])
      } else {
        as.numeric(terra::values(r, mat = FALSE, na.rm = TRUE))
      }
    }), use.names = FALSE)
    vals <- vals[is.finite(vals)]
    if (length(vals) < 1L) {
      stop("No finite raster values found for ", unique(df$pollutant), ".")
    }
    qs <- stats::quantile(vals, probs = probs, na.rm = TRUE, names = FALSE)
    data.frame(
      pollutant = unique(df$pollutant),
      lower = as.numeric(qs[[1L]]),
      upper = as.numeric(qs[[2L]]),
      prob_lower = probs[[1L]],
      prob_upper = probs[[2L]],
      sample_size_per_raster = if (is.null(sample_size_per_raster)) NA_real_ else sample_size_per_raster,
      n_rasters = nrow(df),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

prediction_color_range_for <- function(color_ranges, pollutant) {
  row <- color_ranges[color_ranges$pollutant == pollutant, ]
  if (nrow(row) != 1L) {
    stop("Expected exactly one color range for ", pollutant, ".")
  }
  c(lower = row$lower[[1L]], upper = row$upper[[1L]])
}

render_prediction_png <- function(
  annual_raster_path,
  map_index,
  color_ranges,
  output_root = file.path("outputs", "prediction_maps"),
  width = 2400L,
  height = 3000L,
  res = 300L,
  palette_n = 20L,
  overwrite = TRUE
) {
  idx <- prediction_map_index_values(map_index)
  out_dir <- file.path(output_root, "png", idx$pollutant)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, sprintf("%d_%s.png", idx$year, idx$pollutant))
  if (file.exists(out_path) && !overwrite) {
    return(out_path)
  }

  r <- terra::rast(annual_raster_path)
  range <- prediction_color_range_for(color_ranges, idx$pollutant)
  display_r <- terra::clamp(r, lower = range[["lower"]], upper = range[["upper"]], values = TRUE)
  names(display_r) <- prediction_pollutant_label(idx$pollutant)
  pal <- prediction_map_palette(palette_n)
  breaks <- seq(range[["lower"]], range[["upper"]], length.out = palette_n + 1L)
  title <- sprintf("%d %s predicted concentration", idx$year, prediction_pollutant_label(idx$pollutant))

  grDevices::png(
    filename = out_path,
    width = width,
    height = height,
    res = res,
    units = "px",
    bg = "white",
    type = "cairo"
  )
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::par(mar = c(1.5, 1.5, 3.5, 5.5))
  terra::plot(
    display_r,
    col = pal,
    breaks = breaks,
    main = title,
    axes = FALSE,
    plg = list(title = prediction_concentration_unit())
  )
  out_path
}

render_prediction_html <- function(
  annual_raster_path,
  map_index,
  color_ranges,
  output_root = file.path("outputs", "prediction_maps"),
  aggregate_factor = 50L,
  opacity = 0.75,
  max_bytes = 25 * 1024 * 1024,
  selfcontained = TRUE,
  overwrite = TRUE
) {
  idx <- prediction_map_index_values(map_index)
  out_dir <- file.path(output_root, "html", idx$pollutant)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, sprintf("%d_%s.html", idx$year, idx$pollutant))
  if (file.exists(out_path) && !overwrite) {
    return(out_path)
  }

  r <- terra::rast(annual_raster_path)
  if (aggregate_factor > 1L) {
    r <- terra::aggregate(r, fact = aggregate_factor, fun = mean, na.rm = TRUE)
  }
  range <- prediction_color_range_for(color_ranges, idx$pollutant)
  display_r <- terra::clamp(r, lower = range[["lower"]], upper = range[["upper"]], values = TRUE)
  display_r <- terra::project(display_r, "EPSG:4326", method = "bilinear")
  names(display_r) <- prediction_pollutant_label(idx$pollutant)

  pal_values <- prediction_map_palette(256L)
  pal <- leaflet::colorNumeric(
    palette = pal_values,
    domain = c(range[["lower"]], range[["upper"]]),
    na.color = "transparent"
  )
  title <- sprintf("%d %s predicted concentration", idx$year, prediction_pollutant_label(idx$pollutant))
  legend_title <- sprintf("%s (%s)", prediction_pollutant_label(idx$pollutant), prediction_concentration_unit())
  title_html <- sprintf(
    "<div style='padding:6px 10px;background:rgba(255,255,255,0.9);font:16px sans-serif;font-weight:600;'>%s</div>",
    title
  )

  widget <-
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addRasterImage(
      raster::raster(display_r),
      colors = pal,
      opacity = opacity,
      project = FALSE,
      maxBytes = max_bytes
    ) |>
    leaflet::addLegend(
      pal = pal,
      values = c(range[["lower"]], range[["upper"]]),
      title = legend_title,
      opacity = opacity
    ) |>
    leaflet::addControl(html = title_html, position = "topright")

  htmlwidgets::saveWidget(widget, file = out_path, selfcontained = selfcontained)
  out_path
}

validate_prediction_map_outputs <- function(
  manifest,
  annual_raster_paths,
  png_paths,
  html_paths,
  expected_years = 2015:2023,
  pollutants = c("PM10", "PM25"),
  expected_chunks = 593L
) {
  annual_raster_paths <- unlist(annual_raster_paths, recursive = TRUE, use.names = FALSE)
  png_paths <- unlist(png_paths, recursive = TRUE, use.names = FALSE)
  html_paths <- unlist(html_paths, recursive = TRUE, use.names = FALSE)
  expected_n <- length(expected_years) * length(pollutants)

  if (length(annual_raster_paths) != expected_n) {
    stop("Expected ", expected_n, " annual rasters, got ", length(annual_raster_paths), ".")
  }
  if (length(png_paths) != expected_n) {
    stop("Expected ", expected_n, " PNG files, got ", length(png_paths), ".")
  }
  if (length(html_paths) != expected_n) {
    stop("Expected ", expected_n, " HTML files, got ", length(html_paths), ".")
  }

  chunk_counts <- stats::aggregate(
    chunk_id ~ pollutant + year,
    data = manifest,
    FUN = length
  )
  bad_counts <- chunk_counts[chunk_counts$chunk_id != expected_chunks, ]
  if (nrow(bad_counts) > 0L) {
    stop("Unexpected chunk counts in manifest.")
  }
  if (any(manifest$gid_has_na) || any(manifest$gid_has_duplicates) || any(manifest$xy_has_duplicates)) {
    stop("Chunk-level gid/xy validation failed.")
  }

  raster_meta <- parse_annual_prediction_raster_path(annual_raster_paths)
  raster_checks <- lapply(seq_len(nrow(raster_meta)), function(i) {
    path <- raster_meta$raster_path[[i]]
    pollutant <- raster_meta$pollutant[[i]]
    year <- raster_meta$year[[i]]
    rows <- manifest[manifest$pollutant == pollutant & manifest$year == year, ]
    r <- terra::rast(path)
    non_na_cells <- as.numeric(terra::global(!is.na(r), "sum", na.rm = TRUE)[1, 1])
    data.frame(
      pollutant = pollutant,
      year = year,
      expected_chunk_rows = sum(rows$n_rows),
      annual_non_na_cells = non_na_cells,
      annual_matches_manifest_rows = isTRUE(all.equal(non_na_cells, sum(rows$n_rows))),
      stringsAsFactors = FALSE
    )
  })
  raster_checks <- do.call(rbind, raster_checks)
  if (!all(raster_checks$annual_matches_manifest_rows)) {
    stop("Annual raster non-NA cell counts do not match manifest row counts.")
  }

  html_sizes <- data.frame(
    html_path = html_paths,
    size_bytes = file.info(html_paths)$size,
    stringsAsFactors = FALSE
  )
  html_ok <- vapply(html_paths, function(path) {
    first_lines <- paste(readLines(path, n = 20L, warn = FALSE), collapse = "\n")
    grepl("<html|<!DOCTYPE html", first_lines, ignore.case = TRUE)
  }, logical(1))
  if (!all(html_ok)) {
    stop("At least one HTML file does not look like an HTML document.")
  }
  png_ok <- file.exists(png_paths) & file.info(png_paths)$size > 0
  if (!all(png_ok)) {
    stop("At least one PNG file is missing or empty.")
  }

  list(
    annual_rasters = annual_raster_paths,
    png = png_paths,
    html = html_paths,
    chunk_counts = chunk_counts,
    raster_checks = raster_checks,
    html_sizes = html_sizes
  )
}
