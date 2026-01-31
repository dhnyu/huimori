
list_configs <-
  list(
    targets::tar_target(
      name = chr_dir_data,
      command = {
        if (Sys.getenv("USER") == "isong") {
          file.path("/mnt/hdd001", "Korea")
        } else if (Sys.getenv("USER") == "dhnyu") {
          file.path("/mnt/hdd001", "Korea")
        } else {
          file.path(Sys.getenv("HOME"), "Documents")
        }
      }
    ),
    targets::tar_target(
      name = chr_dir_git,
      command = {
        if (Sys.getenv("USER") == "isong") {
          file.path(Sys.getenv("HOME"), "GitHub", "histmap-ko")
        } else if (Sys.getenv("USER") == "dhnyu") {
          file.path(Sys.getenv("HOME"), "histmap-ko")
        } else {
          file.path(Sys.getenv("HOME"), "Documents")
        }
      }
    ),
    targets::tar_target(
      name = chr_dir_climate,
      command = {
        if (Sys.getenv("USER") == "isong") {
          file.path("/mnt/hdd001", "huimori")
        } else if (Sys.getenv("USER") == "dhnyu") {
          file.path("/mnt/hdd001", "huimori")
        } else {
          file.path(Sys.getenv("HOME"), "Documents")
        }
      }
    )
  )