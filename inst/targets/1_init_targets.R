
list_configs <-
  list(
    targets::tar_target(
      name = chr_dir_data,
      command = "/mnt/hdd001/Korea"
    ),
    
    # 2. [추가] 쓰기 전용 출력 데이터 경로
    targets::tar_target(
      name = chr_dir_out,
      command = {
        user_name <- Sys.getenv("USER")
        if (user_name == "dhnyu") {
          # 본인 계정의 홈 디렉토리 하위 경로
          out_path <- file.path(Sys.getenv("HOME"), "huimori/data")
        } else {
          # 기본 공유 경로 (isong 등 권한이 있는 사용자용)
          out_path <- "/mnt/hdd001/Korea"
        }
        
        # 디렉토리가 없으면 자동 생성
        if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)
        out_path
      }
    ),
    
    targets::tar_target(
      name = chr_dir_git,
      command = file.path(Sys.getenv("HOME"), "histmap-ko")
    )
  )