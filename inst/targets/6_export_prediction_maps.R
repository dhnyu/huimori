list_export_prediction_maps <-
  list(
    targets::tar_target(
      name = chr_dir_prediction_maps,
      command = file.path("outputs", "prediction_maps")
    ),
    targets::tar_target(
      name = df_prediction_map_index,
      command = {
        prediction_map_index(
          years = int_years_spatial,
          pollutants = c("PM10", "PM25")
        )
      },
      iteration = "group"
    ),
    targets::tar_target(
      name = workflow_fit_xgb_correct_annual_raster,
      command = {
        write_annual_prediction_outputs(
          prediction_dfs = workflow_fit_xgb_correct,
          map_index = df_prediction_map_index,
          output_root = chr_dir_prediction_maps,
          expected_chunks = 593L,
          boundary_sf = sf_korea_all,
          cog_compression = "ZSTD",
          cog_overviews = "NONE"
        )
      },
      pattern = map(df_prediction_map_index),
      format = "file",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_04")
      )
    ),
    targets::tar_target(
      name = workflow_fit_xgb_correct_map_validation,
      command = {
        validate_prediction_map_outputs(
          annual_outputs = workflow_fit_xgb_correct_annual_raster,
          expected_years = int_years_spatial,
          pollutants = c("PM10", "PM25"),
          expected_chunks = 593L
        )
      },
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_04")
      )
    )
  )
