prepare_xgb_correct_training_data <- function(data, formula, target_year) {
  yvar <- as.character(formula)[2]
  target_year <- as.integer(target_year)
  if (length(target_year) != 1L || is.na(target_year)) {
    stop("target_year must be a single non-missing integer year.")
  }
  if (!"year" %in% names(data)) {
    stop("Training data must contain a year column.")
  }
  if (!yvar %in% names(data)) {
    stop("Training data is missing outcome column: ", yvar)
  }

  out <-
    data |>
    dplyr::filter(year <= target_year) |>
    dplyr::filter(!is.na(.data[[yvar]])) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      site_type = droplevels(site_type),
      latitude = as.double(stringi::stri_extract_first_regex(
        coords_google,
        pattern = "[3-4][0-9]\\.[0-9]{2,8}"
      )),
      longitude = as.double(stringi::stri_extract_last_regex(
        coords_google,
        pattern = "1[2-4][0-9]\\.[0-9]{2,8}"
      ))
    )

  if (nrow(out) < 1L) {
    stop("No training rows remain for ", yvar, " target_year=", target_year, ".")
  }
  year_max <- max(as.integer(out$year), na.rm = TRUE)
  if (is.finite(year_max) && year_max > target_year) {
    stop("Future-year leakage detected: max training year ", year_max, " > target_year ", target_year, ".")
  }
  if (anyNA(out$latitude) || anyNA(out$longitude)) {
    stop("Could not parse coords_google for all training rows in ", yvar, " target_year=", target_year, ".")
  }

  out <-
    out |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(crs = "EPSG:5179")

  attr(out, "target_year") <- target_year
  attr(out, "outcome") <- yvar
  attr(out, "formula") <- formula
  out
}

make_xgb_spatial_resamples <- function(data, v = 5L, method = "snake") {
  if (!inherits(data, "sf")) {
    stop("Spatial resampling data must be an sf object.")
  }
  spatialsample::spatial_block_cv(data = data, method = method, v = v)
}

plot_xgb_spatial_folds <- function(
  data,
  resamples,
  output_dir = file.path("logs", "cv_blocks"),
  width = 8,
  height = 8,
  dpi = 180
) {
  if (!".row_id" %in% names(data)) {
    stop("CV plot data must contain .row_id.")
  }
  yvar <- attr(data, "outcome")
  target_year <- attr(data, "target_year")
  if (length(yvar) != 1L || is.na(yvar) || length(target_year) != 1L || is.na(target_year)) {
    stop("CV plot data must carry outcome and target_year attributes.")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, sprintf("%s_%d_cv_blocks.png", yvar, as.integer(target_year)))

  fold_membership <- do.call(
    rbind,
    Map(
      f = function(split, fold_id) {
        rsample::assessment(split) |>
          sf::st_drop_geometry() |>
          dplyr::select(.row_id) |>
          dplyr::mutate(fold_id = fold_id)
      },
      split = resamples$splits,
      fold_id = resamples$id
    )
  )
  plot_data <- data |>
    dplyr::left_join(fold_membership, by = ".row_id")
  if (anyNA(plot_data$fold_id)) {
    stop("Some rows were not assigned to a spatial CV assessment fold.")
  }

  plot_obj <-
    ggplot2::ggplot(plot_data) +
    ggplot2::geom_sf(ggplot2::aes(color = fold_id), size = 1.8, alpha = 0.9) +
    ggplot2::coord_sf() +
    ggplot2::labs(
      title = sprintf("%s %d spatial block CV", yvar, as.integer(target_year)),
      color = "Fold"
    ) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(
    filename = out_path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
  out_path
}

flatten_workflow_list <- function(x) {
  if (inherits(x, "workflow")) {
    return(list(x))
  }
  if (!is.list(x)) {
    stop("Expected a workflow or list of workflows.")
  }
  unlist(lapply(x, flatten_workflow_list), recursive = FALSE)
}

predict_grid_with_matching_year_models <- function(grid_data, fitted_models, chr_terms_x) {
  df_combined <- sf::st_drop_geometry(grid_data)
  required_key_cols <- c("gid", "x", "y", "layer", "year")
  missing_key_cols <- setdiff(required_key_cols, names(df_combined))
  if (length(missing_key_cols) > 0L) {
    stop("df_feat_grid_merged is missing prediction key columns: ", paste(missing_key_cols, collapse = ", "))
  }
  missing_terms <- setdiff(chr_terms_x, names(df_combined))
  if (length(missing_terms) > 0L) {
    stop("df_feat_grid_merged is missing training predictors: ", paste(missing_terms, collapse = ", "))
  }

  grid_year <- unique(as.integer(df_combined$year))
  if (length(grid_year) != 1L || is.na(grid_year)) {
    stop("df_feat_grid_merged branch must contain exactly one prediction year.")
  }

  models <- flatten_workflow_list(fitted_models)
  model_year <- vapply(models, function(x) as.integer(attr(x, "target_year")), integer(1))
  selected <- models[model_year == grid_year]
  if (length(selected) < 1L) {
    stop("No fitted XGBoost model found for target_year=", grid_year, ".")
  }

  outcomes <- vapply(selected, function(x) as.character(attr(x, "outcome")), character(1))
  if (anyNA(outcomes) || any(outcomes == "")) {
    stop("All fitted XGBoost workflows must carry an outcome attribute.")
  }
  duplicated_outcomes <- unique(outcomes[duplicated(outcomes)])
  if (length(duplicated_outcomes) > 0L) {
    stop(
      "Multiple fitted XGBoost workflows found for target_year=",
      grid_year,
      " and outcome(s): ",
      paste(duplicated_outcomes, collapse = ", ")
    )
  }

  metadata_cols <-
    intersect(
      c("gid", "x", "y", "layer", "year", "X", "Y", "longitude", "latitude", "lon", "lat"),
      names(df_combined)
    )
  fitted <- df_combined[, metadata_cols, drop = FALSE]
  for (i in seq_along(selected)) {
    yvar <- outcomes[[i]]
    pred <- predict(selected[[i]], df_combined)
    if (nrow(pred) != nrow(df_combined)) {
      stop("Prediction row count changed for ", yvar, " target_year=", grid_year, ".")
    }
    fitted[[yvar]] <- pred$.pred
  }

  missing_pollutants <- setdiff(c("PM10", "PM25"), names(fitted))
  if (length(missing_pollutants) > 0L) {
    stop(
      "Missing pollutant predictions for target_year=",
      grid_year,
      ": ",
      paste(missing_pollutants, collapse = ", ")
    )
  }
  fitted
}
