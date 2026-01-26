
list_configs <-
  list(
    targets::tar_target(
      name = chr_dir_data,
      command = {
        if (Sys.getenv("USER") == "isong") {
          file.path("/mnt/hdd001", "Korea")
        } else {
          file.path("/mnt/hdd001", "Korea")
        }
      }
    ),
    targets::tar_target(
      name = chr_dir_git,
      command = file.path(Sys.getenv("HOME"), "histmap-ko")
    )
  )