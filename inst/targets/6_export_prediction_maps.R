list_export_prediction_maps <-
  list(
    targets::tar_target(
      name = chr_dir_prediction_maps,
      command = file.path("outputs", "prediction_maps")
    ),
    targets::tar_target(
      name = workflow_fit_xgb_correct_chunk_files,
      command = {
        write_prediction_chunk_raster(
          prediction_df = workflow_fit_xgb_correct,
          output_root = chr_dir_prediction_maps
        )
      },
      pattern = map(workflow_fit_xgb_correct),
      iteration = "list",
      format = "file",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_20")
      )
    ),
    targets::tar_target(
      name = df_workflow_fit_xgb_correct_chunk_manifest,
      command = {
        build_prediction_chunk_manifest(workflow_fit_xgb_correct_chunk_files)
      }
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
        write_annual_prediction_raster(
          manifest = df_workflow_fit_xgb_correct_chunk_manifest,
          map_index = df_prediction_map_index,
          output_root = chr_dir_prediction_maps,
          expected_chunks = 593L,
          boundary_sf = sf_korea_all
        )
      },
      pattern = map(df_prediction_map_index),
      format = "file",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_04")
      )
    ),
    targets::tar_target(
      name = df_workflow_fit_xgb_correct_color_ranges,
      command = {
        calculate_prediction_color_ranges(
          annual_raster_paths = workflow_fit_xgb_correct_annual_raster,
          probs = c(0.01, 0.99),
          sample_size_per_raster = 1000000L
        )
      }
    ),
    targets::tar_target(
      name = workflow_fit_xgb_correct_png,
      command = {
        render_prediction_png(
          annual_raster_path = workflow_fit_xgb_correct_annual_raster,
          map_index = df_prediction_map_index,
          color_ranges = df_workflow_fit_xgb_correct_color_ranges,
          output_root = chr_dir_prediction_maps,
          width = 2400L,
          height = 3000L,
          res = 300L
        )
      },
      pattern = map(workflow_fit_xgb_correct_annual_raster, df_prediction_map_index),
      format = "file",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_04")
      )
    ),
    targets::tar_target(
      name = workflow_fit_xgb_correct_html,
      command = {
        render_prediction_html(
          annual_raster_path = workflow_fit_xgb_correct_annual_raster,
          map_index = df_prediction_map_index,
          color_ranges = df_workflow_fit_xgb_correct_color_ranges,
          output_root = chr_dir_prediction_maps,
          aggregate_factor = 50L,
          selfcontained = TRUE
        )
      },
      pattern = map(workflow_fit_xgb_correct_annual_raster, df_prediction_map_index),
      format = "file",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_04")
      )
    ),
    targets::tar_target(
      name = workflow_fit_xgb_correct_map_validation,
      command = {
        validate_prediction_map_outputs(
          manifest = df_workflow_fit_xgb_correct_chunk_manifest,
          annual_raster_paths = workflow_fit_xgb_correct_annual_raster,
          png_paths = workflow_fit_xgb_correct_png,
          html_paths = workflow_fit_xgb_correct_html,
          expected_years = int_years_spatial,
          pollutants = c("PM10", "PM25"),
          expected_chunks = 593L
        )
      }
    )
  )
