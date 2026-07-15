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

prediction_raster_gdal_options <- function() {
  c(
    "COMPRESS=DEFLATE",
    "PREDICTOR=3",
    "ZLEVEL=9",
    "TILED=YES",
    "BIGTIFF=IF_SAFER",
    "NUM_THREADS=ALL_CPUS"
  )
}

gdal_cog_creation_option_text <- function() {
  gdalinfo <- Sys.which("gdalinfo")
  if (!nzchar(gdalinfo)) {
    return(character())
  }
  out <- tryCatch(
    system2(gdalinfo, args = c("--format", "COG"), stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )
  out
}

gdal_cog_supports_compression <- function(compression, option_text = gdal_cog_creation_option_text()) {
  compression <- toupper(compression)
  any(grepl(paste0("<Value>", compression, "</Value>"), option_text, fixed = TRUE))
}

gdal_cog_level_option_name <- function(compression, option_text = gdal_cog_creation_option_text()) {
  compression <- toupper(compression)
  if (any(grepl('Option name="LEVEL"', option_text, fixed = TRUE))) {
    return("LEVEL")
  }
  if (identical(compression, "DEFLATE") &&
      any(grepl('Option name="ZLEVEL"', option_text, fixed = TRUE))) {
    return("ZLEVEL")
  }
  if (identical(compression, "ZSTD") &&
      any(grepl('Option name="ZSTD_LEVEL"', option_text, fixed = TRUE))) {
    return("ZSTD_LEVEL")
  }
  "LEVEL"
}

prediction_cog_creation_options <- function(
  compression = c("DEFLATE", "ZSTD"),
  level = NULL,
  predictor = 3L,
  blocksize = 512L,
  overviews = "AUTO",
  bigtiff = "IF_SAFER",
  num_threads = "ALL_CPUS",
  option_text = gdal_cog_creation_option_text()
) {
  compression <- toupper(match.arg(compression))
  if (identical(compression, "ZSTD") && !gdal_cog_supports_compression("ZSTD", option_text)) {
    stop("The installed GDAL COG driver does not advertise ZSTD compression support.")
  }
  if (identical(compression, "DEFLATE") && !gdal_cog_supports_compression("DEFLATE", option_text)) {
    stop("The installed GDAL COG driver does not advertise DEFLATE compression support.")
  }
  if (is.null(level)) {
    level <- if (identical(compression, "ZSTD")) 19L else 9L
  }
  level_option <- gdal_cog_level_option_name(compression, option_text)
  c(
    paste0("COMPRESS=", compression),
    paste0("PREDICTOR=", predictor),
    paste0(level_option, "=", as.integer(level)),
    paste0("BLOCKSIZE=", as.integer(blocksize)),
    paste0("OVERVIEWS=", overviews),
    paste0("BIGTIFF=", bigtiff),
    paste0("NUM_THREADS=", num_threads)
  )
}

prediction_cog_option_value <- function(options, key) {
  prefix <- paste0(key, "=")
  hit <- options[startsWith(options, prefix)]
  if (length(hit) < 1L) {
    return(NA_character_)
  }
  sub("^[^=]+=", "", hit[[1L]])
}

prediction_cog_option_level <- function(options) {
  for (key in c("LEVEL", "ZLEVEL", "ZSTD_LEVEL")) {
    value <- prediction_cog_option_value(options, key)
    if (!is.na(value)) {
      return(value)
    }
  }
  NA_character_
}

prediction_gdalinfo_summary <- function(raster_path) {
  gdalinfo <- Sys.which("gdalinfo")
  empty <- list(
    gdalinfo_available = nzchar(gdalinfo),
    layout = NA_character_,
    compression = NA_character_,
    predictor = NA_character_,
    block = NA_character_,
    overviews = NA_character_
  )
  if (!nzchar(gdalinfo) || !file.exists(raster_path)) {
    return(empty)
  }
  info <- tryCatch(
    system2(gdalinfo, args = raster_path, stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )
  value_after_equals <- function(pattern) {
    line <- info[grepl(pattern, info, ignore.case = TRUE)]
    if (length(line) < 1L) {
      return(NA_character_)
    }
    sub("^.*=", "", line[[1L]])
  }
  block_line <- info[grepl("Block=", info, fixed = TRUE)]
  overview_line <- info[grepl("Overviews:", info, fixed = TRUE)]
  list(
    gdalinfo_available = TRUE,
    layout = value_after_equals("LAYOUT="),
    compression = value_after_equals("COMPRESSION="),
    predictor = value_after_equals("PREDICTOR="),
    block = if (length(block_line) > 0L) sub("^.*Block=([^ ]+).*$", "\\1", block_line[[1L]]) else NA_character_,
    overviews = if (length(overview_line) > 0L) sub("^.*Overviews: *", "", overview_line[[1L]]) else NA_character_
  )
}

translate_prediction_raster_to_cog <- function(
  raster_path,
  overwrite = TRUE,
  creation_options = prediction_cog_creation_options(compression = "DEFLATE")
) {
  gdal_translate <- Sys.which("gdal_translate")
  if (!nzchar(gdal_translate)) {
    warning("gdal_translate not found; keeping GeoTIFF without COG conversion: ", raster_path)
    return(FALSE)
  }
  if (!file.exists(raster_path)) {
    stop("Raster does not exist for COG conversion: ", raster_path)
  }

  temp_cog <- tempfile(fileext = ".tif")
  args <- c(
    "-of", "COG",
    raster_path,
    temp_cog,
    unlist(Map(function(x) c("-co", x), creation_options), use.names = FALSE)
  )
  status <- system2(gdal_translate, args = args)
  if (!identical(status, 0L) || !file.exists(temp_cog)) {
    warning("COG conversion failed for ", raster_path, ". Keeping terra::writeRaster output.")
    unlink(temp_cog, force = TRUE)
    return(FALSE)
  }
  if (!overwrite && file.exists(raster_path)) {
    unlink(temp_cog, force = TRUE)
    stop("Cannot replace existing raster when overwrite = FALSE: ", raster_path)
  }
  ok <- file.copy(temp_cog, raster_path, overwrite = TRUE)
  unlink(temp_cog, force = TRUE)
  if (!ok) {
    warning("COG conversion succeeded but could not replace raster: ", raster_path)
    return(FALSE)
  }
  TRUE
}

write_prediction_chunk_raster <- function(
  prediction_df,
  output_root = file.path("outputs", "prediction_maps"),
  crs = "EPSG:5179",
  overwrite = TRUE,
  gdal_options = prediction_raster_gdal_options(),
  cog = FALSE
) {
  required_cols <- c("gid", "x", "y", "layer", "year")
  missing_cols <- setdiff(required_cols, names(prediction_df))
  if (length(missing_cols) > 0L) {
    stop("Prediction chunk is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  pollutant_cols <- intersect(c("PM10", "PM25"), names(prediction_df))
  if (length(pollutant_cols) < 1L) {
    stop("Prediction chunk must contain at least one pollutant column: PM10 or PM25.")
  }
  year <- unique(as.integer(prediction_df$year))
  if (length(year) != 1L || is.na(year)) {
    stop("Prediction chunk must contain exactly one year.")
  }

  chunk_id <- prediction_chunk_id(prediction_df)
  gid_int <- as.integer(prediction_df$gid)
  out_files <- character()
  for (pollutant in pollutant_cols) {
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
    cog_created <- if (isTRUE(cog)) {
      translate_prediction_raster_to_cog(raster_path, overwrite = overwrite)
    } else {
      FALSE
    }

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
      gdal_options = paste(gdal_options, collapse = "|"),
      cog_requested = isTRUE(cog),
      cog_created = isTRUE(cog_created),
      stringsAsFactors = FALSE
    )
    utils::write.csv(metadata, metadata_path, row.names = FALSE, na = "")
    out_files <- c(out_files, raster = raster_path, metadata = metadata_path)
  }

  out_files
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
  gdal_options = prediction_raster_gdal_options(),
  cog = TRUE
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
  cog_created <- if (isTRUE(cog)) {
    translate_prediction_raster_to_cog(out_path, overwrite = overwrite)
  } else {
    FALSE
  }
  metadata_path <- sub("\\.tif$", ".csv", out_path)
  utils::write.csv(
    data.frame(
      raster_path = out_path,
      metadata_path = metadata_path,
      pollutant = idx$pollutant,
      year = idx$year,
      n_chunks = nrow(rows),
      gdal_options = paste(gdal_options, collapse = "|"),
      cog_requested = isTRUE(cog),
      cog_created = isTRUE(cog_created),
      stringsAsFactors = FALSE
    ),
    metadata_path,
    row.names = FALSE,
    na = ""
  )
  out_path
}

flatten_prediction_data_frames <- function(x) {
  if (is.data.frame(x)) {
    return(list(x))
  }
  if (!is.list(x)) {
    stop("Expected prediction data frames or a list of prediction data frames.")
  }
  unlist(lapply(x, flatten_prediction_data_frames), recursive = FALSE)
}

prediction_chunks_for_map_index <- function(prediction_dfs, map_index, expected_chunks = 593L) {
  idx <- prediction_map_index_values(map_index)
  if (is.data.frame(prediction_dfs)) {
    required_cols <- c("gid", "x", "y", "layer", "year", idx$pollutant)
    missing_cols <- setdiff(required_cols, names(prediction_dfs))
    if (length(missing_cols) > 0L) {
      stop(
        "Aggregated prediction data is missing columns for ",
        idx$year, " ", idx$pollutant, ": ",
        paste(missing_cols, collapse = ", ")
      )
    }
    year_values <- as.integer(prediction_dfs$year)
    out <- prediction_dfs[year_values == idx$year, , drop = FALSE]
    if (nrow(out) < 1L) {
      stop("No prediction rows found for ", idx$year, " ", idx$pollutant, ".")
    }
    chunks <- list(out)
    attr(chunks, "n_chunks") <- expected_chunks
    attr(chunks, "aggregated_data_frame") <- TRUE
    return(chunks)
  }

  chunks <- flatten_prediction_data_frames(prediction_dfs)
  keep <- vapply(chunks, function(df) {
    if (!is.data.frame(df) || !"year" %in% names(df)) {
      return(FALSE)
    }
    years <- unique(as.integer(df$year))
    length(years) == 1L && !is.na(years) && years == idx$year
  }, logical(1))
  chunks <- chunks[keep]
  if (!is.null(expected_chunks) && length(chunks) != expected_chunks) {
    stop(
      "Expected ", expected_chunks, " prediction chunks for ",
      idx$year, " ", idx$pollutant, ", got ", length(chunks), "."
    )
  }
  if (length(chunks) < 1L) {
    stop("No prediction chunks found for ", idx$year, " ", idx$pollutant, ".")
  }
  required_cols <- c("gid", "x", "y", "layer", "year", idx$pollutant)
  for (i in seq_along(chunks)) {
    missing_cols <- setdiff(required_cols, names(chunks[[i]]))
    if (length(missing_cols) > 0L) {
      stop(
        "Prediction chunk ", i, " is missing columns for ",
        idx$year, " ", idx$pollutant, ": ",
        paste(missing_cols, collapse = ", ")
      )
    }
  }
  chunks
}

prediction_chunks_count <- function(chunks) {
  n_chunks <- attr(chunks, "n_chunks", exact = TRUE)
  if (length(n_chunks) == 1L && !is.na(n_chunks)) {
    return(as.integer(n_chunks))
  }
  length(chunks)
}

prediction_chunk_to_raster <- function(prediction_df, pollutant, crs = "EPSG:5179") {
  xyz <- data.frame(
    x = as.numeric(prediction_df$x),
    y = as.numeric(prediction_df$y),
    value = as.numeric(prediction_df[[pollutant]])
  )
  names(xyz)[3L] <- pollutant
  raster_chunk <- prediction_chunk_raster(xyz, value_col = pollutant, crs = crs)
  names(raster_chunk) <- pollutant
  raster_chunk
}

prediction_raster_display_range <- function(raster, probs = c(0.01, 0.99), sample_size = 1000000L) {
  if (length(probs) != 2L || anyNA(probs) || probs[1L] < 0 || probs[2L] > 1 || probs[1L] >= probs[2L]) {
    stop("probs must be two increasing probabilities between 0 and 1.")
  }
  vals <- if (!is.null(sample_size) &&
              is.finite(sample_size) &&
              terra::ncell(raster) > sample_size) {
    sampled <- terra::spatSample(
      raster,
      size = sample_size,
      method = "regular",
      na.rm = TRUE,
      values = TRUE
    )
    as.numeric(sampled[[1L]])
  } else {
    as.numeric(terra::values(raster, mat = FALSE, na.rm = TRUE))
  }
  vals <- vals[is.finite(vals)]
  if (length(vals) < 1L) {
    stop("No finite raster values found for display range.")
  }
  stats::quantile(vals, probs = probs, na.rm = TRUE, names = FALSE)
}

write_annual_prediction_outputs <- function(
  prediction_dfs,
  map_index,
  output_root = file.path("outputs", "prediction_maps"),
  expected_chunks = 593L,
  boundary_sf = NULL,
  crs = "EPSG:5179",
  overwrite = TRUE,
  raster_gdal_options = prediction_raster_gdal_options(),
  cog = TRUE,
  cog_compression = c("DEFLATE", "ZSTD"),
  cog_level = NULL,
  cog_overviews = "NONE",
  png_width = 2400L,
  png_height = 3000L,
  png_res = 300L,
  png_range_probs = c(0.01, 0.99),
  png_sample_size = 1000000L
) {
  idx <- prediction_map_index_values(map_index)
  chunks <- prediction_chunks_for_map_index(
    prediction_dfs = prediction_dfs,
    map_index = map_index,
    expected_chunks = expected_chunks
  )

  chunk_rows <- vapply(chunks, nrow, integer(1))
  gid_na <- vapply(chunks, function(df) anyNA(as.integer(df$gid)), logical(1))
  if (any(gid_na)) {
    stop("Prediction chunk gid validation failed for ", idx$year, " ", idx$pollutant, ".")
  }
  if (!isTRUE(attr(chunks, "aggregated_data_frame", exact = TRUE))) {
    xy_duplicates <- vapply(chunks, function(df) anyDuplicated(df[c("x", "y")]) > 0L, logical(1))
    if (any(xy_duplicates)) {
      stop("Prediction chunk xy validation failed for ", idx$year, " ", idx$pollutant, ".")
    }
  }
  n_chunks <- prediction_chunks_count(chunks)

  rasters <- lapply(chunks, prediction_chunk_to_raster, pollutant = idx$pollutant, crs = crs)
  annual <- if (length(rasters) == 1L) {
    rasters[[1L]]
  } else {
    do.call(terra::mosaic, c(rasters, list(fun = "mean")))
  }
  names(annual) <- idx$pollutant

  if (!is.null(boundary_sf)) {
    annual <- terra::mask(annual, terra::vect(sf::st_transform(boundary_sf, terra::crs(annual))))
  }

  out_dir <- file.path(output_root, "rasters", idx$pollutant)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  raster_path <- file.path(out_dir, sprintf("%d_%s.tif", idx$year, idx$pollutant))
  metadata_path <- sub("\\.tif$", ".csv", raster_path)

  terra::writeRaster(
    x = annual,
    filename = raster_path,
    overwrite = overwrite,
    datatype = "FLT4S",
    gdal = raster_gdal_options
  )

  cog_compression <- toupper(match.arg(cog_compression))
  cog_options <- prediction_cog_creation_options(
    compression = cog_compression,
    level = cog_level,
    overviews = cog_overviews
  )
  cog_created <- if (isTRUE(cog)) {
    translate_prediction_raster_to_cog(
      raster_path = raster_path,
      overwrite = overwrite,
      creation_options = cog_options
    )
  } else {
    FALSE
  }

  display_range <- prediction_raster_display_range(
    raster = annual,
    probs = png_range_probs,
    sample_size = png_sample_size
  )
  color_ranges <- data.frame(
    pollutant = idx$pollutant,
    lower = as.numeric(display_range[[1L]]),
    upper = as.numeric(display_range[[2L]]),
    stringsAsFactors = FALSE
  )
  png_path <- render_prediction_png(
    annual_raster_path = raster_path,
    map_index = map_index,
    color_ranges = color_ranges,
    output_root = output_root,
    width = png_width,
    height = png_height,
    res = png_res,
    overwrite = overwrite
  )

  gdal_summary <- prediction_gdalinfo_summary(raster_path)
  compression_level <- prediction_cog_option_level(cog_options)
  utils::write.csv(
    data.frame(
      raster_path = raster_path,
      png_path = png_path,
      metadata_path = metadata_path,
      pollutant = idx$pollutant,
      year = idx$year,
      n_chunks = n_chunks,
      n_prediction_rows = sum(chunk_rows),
      value_min = min(vapply(chunks, function(df) min(df[[idx$pollutant]], na.rm = TRUE), numeric(1)), na.rm = TRUE),
      value_max = max(vapply(chunks, function(df) max(df[[idx$pollutant]], na.rm = TRUE), numeric(1)), na.rm = TRUE),
      raster_gdal_options = paste(raster_gdal_options, collapse = "|"),
      cog_requested = isTRUE(cog),
      cog_created = isTRUE(cog_created),
      cog_options = paste(cog_options, collapse = "|"),
      cog_compression = prediction_cog_option_value(cog_options, "COMPRESS"),
      cog_predictor = prediction_cog_option_value(cog_options, "PREDICTOR"),
      cog_compression_level = compression_level,
      cog_blocksize = prediction_cog_option_value(cog_options, "BLOCKSIZE"),
      cog_overviews = prediction_cog_option_value(cog_options, "OVERVIEWS"),
      cog_bigtiff = prediction_cog_option_value(cog_options, "BIGTIFF"),
      cog_num_threads = prediction_cog_option_value(cog_options, "NUM_THREADS"),
      gdalinfo_layout = gdal_summary$layout,
      gdalinfo_compression = gdal_summary$compression,
      gdalinfo_predictor = gdal_summary$predictor,
      gdalinfo_block = gdal_summary$block,
      gdalinfo_overviews = gdal_summary$overviews,
      png_range_lower = color_ranges$lower,
      png_range_upper = color_ranges$upper,
      png_range_prob_lower = png_range_probs[[1L]],
      png_range_prob_upper = png_range_probs[[2L]],
      png_sample_size = png_sample_size,
      stringsAsFactors = FALSE
    ),
    metadata_path,
    row.names = FALSE,
    na = ""
  )

  c(raster = raster_path, png = png_path, metadata = metadata_path)
}

annual_prediction_output_paths <- function(outputs, type = c("raster", "png", "metadata")) {
  type <- match.arg(type)
  paths <- unlist(outputs, recursive = TRUE, use.names = FALSE)
  if (length(paths) < 1L) {
    return(character())
  }
  switch(
    type,
    raster = paths[grepl("\\.tif$", paths, ignore.case = TRUE)],
    png = paths[grepl("\\.png$", paths, ignore.case = TRUE)],
    metadata = paths[grepl("\\.csv$", paths, ignore.case = TRUE)]
  )
}

parse_annual_prediction_raster_path <- function(paths) {
  paths <- annual_prediction_output_paths(paths, type = "raster")
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
  annual_outputs,
  expected_years = 2015:2023,
  pollutants = c("PM10", "PM25"),
  expected_chunks = 593L
) {
  annual_raster_paths <- annual_prediction_output_paths(annual_outputs, type = "raster")
  png_paths <- annual_prediction_output_paths(annual_outputs, type = "png")
  metadata_paths <- annual_prediction_output_paths(annual_outputs, type = "metadata")
  expected_n <- length(expected_years) * length(pollutants)

  if (length(annual_raster_paths) != expected_n) {
    stop("Expected ", expected_n, " annual rasters, got ", length(annual_raster_paths), ".")
  }
  if (length(png_paths) != expected_n) {
    stop("Expected ", expected_n, " PNG files, got ", length(png_paths), ".")
  }
  if (length(metadata_paths) != expected_n) {
    stop("Expected ", expected_n, " annual metadata files, got ", length(metadata_paths), ".")
  }

  missing_files <- c(annual_raster_paths, png_paths, metadata_paths)[
    !file.exists(c(annual_raster_paths, png_paths, metadata_paths))
  ]
  if (length(missing_files) > 0L) {
    stop("Prediction map outputs are missing: ", paste(head(missing_files), collapse = ", "))
  }

  metadata <- do.call(
    rbind,
    lapply(metadata_paths, utils::read.csv, stringsAsFactors = FALSE)
  )
  metadata$year <- as.integer(metadata$year)
  metadata$n_chunks <- as.integer(metadata$n_chunks)
  bad_counts <- metadata[metadata$n_chunks != expected_chunks, ]
  if (nrow(bad_counts) > 0L) {
    stop("Unexpected prediction chunk counts in annual metadata.")
  }
  expected_grid <- expand.grid(
    pollutant = pollutants,
    year = as.integer(expected_years),
    stringsAsFactors = FALSE
  )
  missing_grid <- dplyr::anti_join(
    expected_grid,
    metadata[c("pollutant", "year")],
    by = c("pollutant", "year")
  )
  if (nrow(missing_grid) > 0L) {
    stop("Annual metadata is missing pollutant/year rows.")
  }
  if (!all(as.logical(metadata$cog_created))) {
    stop("At least one annual raster metadata row does not report cog_created = TRUE.")
  }
  if (!all(metadata$cog_compression %in% c("DEFLATE", "ZSTD"))) {
    stop("Unexpected COG compression in annual metadata.")
  }

  raster_meta <- parse_annual_prediction_raster_path(annual_raster_paths)
  raster_checks <- lapply(seq_len(nrow(raster_meta)), function(i) {
    path <- raster_meta$raster_path[[i]]
    pollutant <- raster_meta$pollutant[[i]]
    year <- raster_meta$year[[i]]
    row <- metadata[metadata$pollutant == pollutant & metadata$year == year, ]
    r <- terra::rast(path)
    non_na_cells <- as.numeric(terra::global(!is.na(r), "sum", na.rm = TRUE)[1, 1])
    data.frame(
      pollutant = pollutant,
      year = year,
      expected_prediction_rows = row$n_prediction_rows[[1L]],
      annual_non_na_cells = non_na_cells,
      annual_matches_prediction_rows = isTRUE(all.equal(non_na_cells, row$n_prediction_rows[[1L]])),
      stringsAsFactors = FALSE
    )
  })
  raster_checks <- do.call(rbind, raster_checks)
  if (!all(raster_checks$annual_matches_prediction_rows)) {
    stop("Annual raster non-NA cell counts do not match annual metadata prediction row counts.")
  }

  png_ok <- file.exists(png_paths) & file.info(png_paths)$size > 0
  if (!all(png_ok)) {
    stop("At least one PNG file is missing or empty.")
  }

  list(
    annual_rasters = annual_raster_paths,
    png = png_paths,
    metadata = metadata_paths,
    annual_metadata = metadata,
    raster_checks = raster_checks
  )
}
