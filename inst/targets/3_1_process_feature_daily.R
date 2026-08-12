list_process_site_daily <-
  list(
    targets::tar_target(
      name = sf_monitors_correct_daily,
      command = {
        build_sf_monitors_correct_daily(
          measurements = dt_measurements,
          site_history = sf_monitors_base,
          min_valid_hours = 18L
        )
      }
    ),
    targets::tar_target(
      name = sf_monitors_correct_month,
      command = {
        subset_daily_monitor_month(
          sf_monitors_correct_daily,
          chr_months_spatial
        )
      },
      pattern = map(chr_months_spatial),
      iteration = "list"
    )
  )


list_process_feature_daily <-
  list(
    targets::tar_target(
      name = df_feat_correct_aod_daily,
      command = {
        extract_aod_daily_month(
          base = sf_monitors_correct_month,
          month = chr_months_spatial,
          aod_dir = chr_dir_aod,
          radii = daily_buffer_radii()
        )
      },
      pattern = map(chr_months_spatial, sf_monitors_correct_month),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_20")
      )
    ),
    targets::tar_target(
      name = df_feat_correct_era5_land_daily,
      command = {
        extract_era5_land_daily_month(
          base = sf_monitors_correct_month,
          month = chr_months_spatial,
          era5_dir = chr_dir_era5_land,
          radii = daily_buffer_radii()
        )
      },
      pattern = map(chr_months_spatial, sf_monitors_correct_month),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_20")
      )
    ),
    targets::tar_target(
      name = df_feat_correct_era5_blh_daily,
      command = {
        extract_era5_blh_daily_month(
          base = sf_monitors_correct_month,
          month = chr_months_spatial,
          era5_dir = chr_dir_era5_blh,
          radii = daily_buffer_radii()
        )
      },
      pattern = map(chr_months_spatial, sf_monitors_correct_month),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_20")
      )
    ),
    targets::tar_target(
      name = df_feat_correct_merged_daily,
      command = {
        merge_correct_daily_month(
          base = sf_monitors_correct_month,
          aod = df_feat_correct_aod_daily,
          era5_land = df_feat_correct_era5_land_daily,
          era5_blh = df_feat_correct_era5_blh_daily,
          annual_features = df_feat_correct_merged,
          month = chr_months_spatial,
          radii = daily_buffer_radii()
        )
      },
      pattern = map(
        chr_months_spatial,
        sf_monitors_correct_month,
        df_feat_correct_aod_daily,
        df_feat_correct_era5_land_daily,
        df_feat_correct_era5_blh_daily
      ),
      iteration = "list"
    ),
    targets::tar_target(
      name = sf_grid_daily_month,
      command = {
        build_grid_daily_month(
          grid = list_pred_calc_grid,
          month = chr_months_spatial
        )
      },
      pattern = cross(
        map(list_pred_calc_grid),
        map(chr_months_spatial)
      ),
      iteration = "list"
    ),
    targets::tar_target(
      name = df_feat_grid_aod_daily,
      command = {
        extract_aod_daily_month(
          base = sf_grid_daily_month,
          month = daily_branch_month(sf_grid_daily_month),
          aod_dir = chr_dir_aod,
          radii = daily_buffer_radii(),
          id_cols = "gid",
          output_label = "df_feat_grid_aod_daily"
        )
      },
      pattern = map(sf_grid_daily_month),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_20")
      )
    ),
    targets::tar_target(
      name = df_feat_grid_era5_land_daily,
      command = {
        extract_era5_land_daily_month(
          base = sf_grid_daily_month,
          month = daily_branch_month(sf_grid_daily_month),
          era5_dir = chr_dir_era5_land,
          radii = daily_buffer_radii(),
          id_cols = "gid",
          output_label = "df_feat_grid_era5_land_daily"
        )
      },
      pattern = map(sf_grid_daily_month),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_20")
      )
    ),
    targets::tar_target(
      name = df_feat_grid_era5_blh_daily,
      command = {
        extract_era5_blh_daily_month(
          base = sf_grid_daily_month,
          month = daily_branch_month(sf_grid_daily_month),
          era5_dir = chr_dir_era5_blh,
          radii = daily_buffer_radii(),
          id_cols = "gid",
          output_label = "df_feat_grid_era5_blh_daily"
        )
      },
      pattern = map(sf_grid_daily_month),
      iteration = "list",
      resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "controller_20")
      )
    ),
    targets::tar_target(
      name = df_feat_grid_merged_daily,
      command = {
        merge_grid_daily_month(
          base = sf_grid_daily_month,
          aod = df_feat_grid_aod_daily,
          era5_land = df_feat_grid_era5_land_daily,
          era5_blh = df_feat_grid_era5_blh_daily,
          annual_features = df_feat_grid_merged,
          month = daily_branch_month(sf_grid_daily_month),
          radii = daily_buffer_radii()
        )
      },
      pattern = map(
        sf_grid_daily_month,
        df_feat_grid_aod_daily,
        df_feat_grid_era5_land_daily,
        df_feat_grid_era5_blh_daily
      ),
      iteration = "list"
    )
  )
