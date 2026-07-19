# huimori Project Context

작성일: 2026-06-30  
작업 디렉터리: `/members/dhnyu/huimori`

## 프로젝트 개요

`huimori`는 한국 전역의 대기오염, 특히 PM10/PM2.5 미세먼지 농도를 재현 가능한 방식으로 모델링하고 예측하기 위한 R 패키지형 프로젝트다. README는 프로젝트명을 `Hybrid Urban-rural Integrated Modeling with Open Reproducible Infrastructure in Korea`로 설명하며, 최종 목표를 2010-2023년 및 이후 기간의 한국 전역 시간별 10m 해상도 대기오염 예측 GeoTIFF 생산으로 둔다.

현재 저장소의 실제 구현 중심은 R `targets` 파이프라인이다. R 패키지 함수(`R/`), target 정의(`inst/targets/`), 실행 캐시(`_targets/`), 분석/디버그 산출물(`daehoon/`), Snakemake 전환 실험(`dev/workflow/`), GeoTIFF focal mean Rust 도구(`rust_tools/geotiff_focal_mean/`)가 함께 있다.

## 핵심 목적

- AirKorea 측정자료와 측정소 위치 이력을 정리한다.
- 측정소 위치 보정 전후(`correct`/`incorrect`)의 feature와 예측 차이를 비교할 수 있게 한다.
- 도로, 토지피복, DEM/DSM, MTPI, 배출원, AOD, ERA5/BLH, CHELSA 등 다양한 공간/기상 covariate를 측정소와 예측 격자에 결합한다.
- PM10/PM25 예측 모델을 튜닝하고 한국 전역 격자에 적용한다.
- 계산을 `targets` DAG와 `crew` 병렬 worker로 관리해 재현성, 캐싱, branch 단위 재실행을 확보한다.

## Repository 구조

| 경로 | 역할 |
|---|---|
| `README.md` | 프로젝트 이름, 배경, 최종 목표 설명. |
| `DESCRIPTION`, `NAMESPACE`, `man/` | R 패키지 메타데이터, export/import 선언, roxygen 문서. |
| `_targets.R` | 메인 `targets` 진입점. 패키지 로드, `crew` controller, target option, target list source를 담당. |
| `R/helpers.R` | 날짜 보정, 일/연 단위 집계, 격자 생성, GEE helper, 시공간 grid 확장 함수. |
| `R/processing.R` | raster frequency/focal 처리, mask/filter, 배출원 공간가중 계산 등 geospatial 처리 함수. |
| `R/models.R` | `sdmTMB`/TMB/INLA helper, XGBoost/LightGBM tidymodels wrapper. |
| `inst/targets/` | 현재 핵심 파이프라인 구현. 설정, input pin, feature engineering, daily feature, 모델 튜닝 정의. |
| `inst/data/` | 작은 패키지 포함 데이터. 현재 측정소 이력 Excel 파일 포함. |
| `_targets/` | `targets` 실행 캐시와 메타데이터. 재생성 가능하지만 큰 산출물이므로 직접 수정 금지. |
| `daehoon/` | Quarto 분석 노트, 다운로드 스크립트, DAG/상태 보고서, 중간 산출물. `.gitignore`와 `.Rbuildignore` 대상이다. |
| `daehoon/outputs/` | 최근 작업 산출물, target 실행 로그, parquet 샘플, 디버그 보고서. |
| `tools/` | 수동/보조 스크립트. AOD 처리, GEE export, DB/raster export, plot, 모델 실험 포함. |
| `tools/run_models/` | RNN/Mamba/Transformer 계열 실험 코드. 현재 활성 `targets` 모델 경로와는 분리되어 있다. |
| `dev/workflow/` | R `targets` 파이프라인을 Python/Snakemake로 전환하려는 개발 초안. |
| `rust_tools/geotiff_focal_mean/` | landcover class fraction 계산을 가속하는 Rust CLI. |
| `container/` | Apptainer/Singularity 기반 geospatial/R 실행 환경 정의. |

## 주요 스크립트/모듈

### 메인 targets 파이프라인

`_targets.R`는 `targets`, `tarchetypes`, `terra`, `chopin`, `crew`, `sf`를 로드하고 `sf::sf_use_s2(FALSE)`를 설정한다. local `crew` controller는 worker 1/4/8/10/15/20개 그룹으로 정의되어 있다. 기본 target format은 `qs`이고, error 정책은 `continue`다.

현재 `_targets.R`가 source하는 파일:

- `R/`
- `inst/targets/1_init_targets.R`
- `inst/targets/2_pin_files.R`
- `inst/targets/3_process_feature.R`
- `inst/targets/3_1_process_feature_daily.R`
- `inst/targets/4_tune_models.R`

현재 `_targets.R`가 반환하는 active target list:

- `list_configs`
- `list_basefiles`
- `list_process_site`
- `list_process_split`
- `list_process_feature`
- `list_process_site_daily`
- `list_process_feature_daily`
- `list_fit_models`
- `list_tune_models`
- `list_tune_eval`

`inst/targets/5_fit_tmb.R`는 파일이 존재하지만 현재 `_targets.R`에서 source하지 않는다. `inst/targets/4_tune_models.R`의 `list_pred_process`도 정의되어 있으나 현재 반환 list에는 포함되지 않는다.

### target 정의 파일

- `1_init_targets.R`: 외부 데이터 루트, 외부 `histmap-ko` 경로, 날짜 범위, 연도/month branch 정의. 현재 코드상 `chr_date_range`는 2015-01-01부터 2023-12-31이고, `int_years_spatial`은 2015:2023이다.
- `2_pin_files.R`: 측정소 Excel, AirKorea parquet, landuse raster, DEM/DSM, 도로, ASOS, GADM Korea boundary, watershed, MTPI, 배출원, AOD, ERA5, CHELSA 경로를 target으로 등록한다.
- `3_process_feature.R`: 측정값/측정소 이력 처리, corrected/incorrect annual monitor table, annual feature, prediction grid, grid feature, annual grid merged feature를 만든다.
- `3_1_process_feature_daily.R`: corrected daily monitor table, monthly branch, daily AOD/ERA5-Land/ERA5-BLH feature, daily merged monitor feature를 만든다.
- `4_tune_models.R`: predictor set, PM10/PM25 formula, XGBoost tuning, placeholder Mamba target, grid prediction target, metrics/VIP target을 정의한다.

### 보조 도구

- `rust_tools/geotiff_focal_mean`: 범주형 GeoTIFF에서 class별 focal fraction multi-band GeoTIFF를 만드는 Rust/GDAL CLI다. `focal-mean --input ... --output ... --radius ...` 형태로 사용한다.
- `dev/workflow/Snakefile`: Snakemake 전환 초안이다. monitor preparation, feature calculation, tuning, prediction, comparison rule이 있으나 Python script에는 단순화/TODO가 남아 있다.
- `tools/process_aod.R`: MODIS MCD19A2 AOD HDF를 QA mask와 inverse-variance weighting으로 daily composite raster로 만드는 보조 스크립트다.
- `tools/export_to_raster.R`: `targets` prediction 결과를 GeoTIFF로 export하는 수동 스크립트다.
- `tools/export_to_db.R`: DEM/DSM, emission, ASOS 등을 PostGIS에 올리는 수동 스크립트다.
- `container/container_covariates.def`: `rocker/geospatial` 기반 Apptainer 이미지에서 R 패키지와 huimori를 설치하는 정의 파일이다.

## 입력 데이터와 출력 데이터

### 주요 입력

현재 코드에서 기대하는 주요 외부 입력 경로는 사용자명에 따라 달라진다. `dhnyu` 기준으로 `chr_dir_data`는 `/mnt/hdd001/Korea`, `chr_dir_git`는 `~/histmap-ko`로 계산된다.

- 측정소 이력: `data/sites/sites_history_cleaning_20250311.xlsx`
- AirKorea 측정 parquet: `airquality/outdoor/sites_airkorea_2010_2023_spt_yd.parquet`
- 토지피복: `landuse/glc_fcs30d` 하위 연도별 GeoTIFF
- DEM: `elevation/kngii_2022_merged_res30d.tif`
- DSM: `elevation/copernicus_korea_30m.tif`
- MTPI: `elevation/kngii_90m_mtpi.tif`, 파생 `kngii_1km_mtpi.tif`
- 도로: `transportation/nodelink/data/**/MOCT_LINK.shp`
- 유역: `watersheds/data/watershed-korea.gpkg`
- 배출원: `emission/data/emission_location.gpkg`
- AOD raster: `airquality/aod/MCD19A2_Daily_Composite_*.tif`
- ERA5-Land/BLH: `climate/ERA5_Land`, `climate/ERA5_BLH`
- CHELSA: `climate/Chelsa`

### 주요 target 산출물

- `dt_measurements`: 측정 parquet를 읽고 KST 9시간 보정, 음수 sentinel 결측 처리.
- `sf_monitors_base`: 측정소 위치 이력 정리.
- `sf_monitors_correct`, `sf_monitors_incorrect`: 위치 보정/비보정 annual monitor table.
- `sf_monitors_correct_daily`, `sf_monitors_correct_month`: daily/monthly corrected monitor branch.
- `df_feat_correct_merged`: corrected annual training feature table.
- `df_feat_incorrect_merged`: incorrect-coordinate comparison training feature table.
- `df_feat_correct_merged_daily`: daily dynamic covariate가 결합된 corrected monitor feature table.
- `list_pred_calc_grid`: 한국 영역 prediction grid point chunk.
- `df_feat_grid_merged_static`: grid별 static feature.
- `df_feat_grid_merged`: grid별 annual prediction feature.
- `workflow_tune_xgb_correct_spatial`: corrected annual feature 기반 PM10/PM25 XGBoost tuning.
- `workflow_fit_xgb_correct`: tuned XGBoost를 grid feature에 적용한 prediction target.

### 파일 산출물

- `tools/export_to_raster.R`는 prediction target을 `/mnt/s/Korea/predictions/*_pred_*_annual.tif` 형태로 쓴다.
- `dev/workflow` Snakemake 초안은 `/mnt/u/interim_results` 아래 parquet/GPKG/CSV를 산출하도록 설정되어 있다.
- `daehoon/outputs/`에는 이전 조사/실행에서 만든 parquet, DAG HTML/PNG, 로그, 보고서가 있다. 이번 문서 작성에서는 새 대용량 산출물을 만들지 않았다.

## 실행 방법 또는 주요 workflow

### R targets 기준

패키지 의존성과 외부 데이터 경로가 준비된 환경에서 저장소 루트에서 실행한다.

```r
library(targets)
targets::tar_make()
```

특정 target만 확인/실행할 때는 다음 계열을 사용한다.

```r
targets::tar_manifest(callr_function = NULL)
targets::tar_outdated(callr_function = NULL)
targets::tar_meta(fields = c("name", "seconds", "warnings", "error"), complete_only = FALSE)
targets::tar_make(names = target_name)
targets::tar_read(target_name)
```

현재 파이프라인은 외부 대용량 raster/vector/parquet에 강하게 의존하므로, 전체 `tar_make()`는 무겁다. 문서 조사만 할 때는 `tar_make()`를 실행하지 않는 것이 안전하다.

### Snakemake 초안

`dev/workflow/README.md` 기준 실행 예:

```bash
cd dev/workflow
snakemake --cores 12
snakemake --cores 12 -j 1 /mnt/u/interim_results/features_monitors_correct.parquet
```

다만 이 경로는 R targets 구현의 완전한 대체본으로 보이지 않는다. `dev/workflow/scripts/calc_features.py`에는 multiband landuse extraction TODO와 grid/year 처리 단순화가 남아 있다.

### Rust landcover 도구

```bash
cd rust_tools/geotiff_focal_mean
cargo build --release
target/release/focal-mean --input input.tif --output output.tif --radius 2000 --threads 8 --compression DEFLATE --cog
```

현재 환경에서 cargo 설치 여부는 이번 조사에서 실행 확인하지 않았다. 확인 필요.

## 현재 구현 상태

- R 패키지 구조와 roxygen 문서가 갖춰져 있다.
- 메인 파이프라인은 `targets` + `crew` 기반으로 구성되어 있다.
- 현재 활성 기간은 코드 기준 2015-2023이며, README의 최종 목표인 2010-2023/시간별/10m와는 차이가 있다.
- annual corrected/incorrect feature, daily corrected feature, grid annual feature, XGBoost tuning/fit target이 존재한다.
- daily feature는 AOD, ERA5-Land, ERA5-BLH를 monthly branch로 추출해 `df_feat_correct_merged_daily`로 합치는 흐름이다.
- `workflow_tune_mamba_*`는 이름과 달리 현재 실제 Mamba 학습을 수행하지 않고 filtered data를 반환하는 placeholder 성격이다.
- TMB/sdmTMB/INLA 관련 함수와 `inst/targets/5_fit_tmb.R`가 있지만 현재 활성 DAG의 중심 경로는 XGBoost다.
- `df_feat_grid_emittors`는 `COMPUTE_GRID_EMISSIONS=true`가 없으면 grid emission 계산을 건너뛰고 `gw_emission = NA_real_`를 반환한다.
- 기존 `daehoon/outputs/compare_correct_grid_merged_for_xgb_spatial.md`에 따르면 2026-06-30 read-only 확인 시 correct/grid feature schema는 XGBoost predictor 기준으로 호환되지만, `form_fit`, `workflow_tune_xgb_correct_spatial`, `workflow_fit_xgb_correct`는 outdated로 표시되었고 `workflow_fit_xgb_correct` 메타데이터는 없었다.

## 주의사항 및 확인 필요 사항

- `daehoon/`과 `_targets/`는 ignore 대상이며, 분석 산출물과 대용량 캐시가 섞여 있다. 필요한 문서 외에는 새 파일을 만들지 않는다.
- 외부 데이터 경로는 사용자명과 마운트 상태에 의존한다. `dhnyu` 환경에서 `/mnt/hdd001/Korea`와 `~/histmap-ko`가 실제로 존재하고 최신인지 확인 필요.
- README의 최종 목표는 2010-2023 시간별 10m이지만 현재 활성 설정은 2015-2023, grid size 30m, annual/daily feature 중심이다. 최종 목표와 현재 scope의 차이를 명확히 관리해야 한다.
- daily ERA5/BLH 추출은 월 branch에서 측정소별 첫 geometry를 사용하는 구간이 있어 월중 측정소 이전을 완전히 반영하는지 확인 필요.
- `chr_terms_x`의 predictor 패턴과 실제 feature column 이름이 항상 일치하는지 확인 필요. 기존 문서에서는 `gw_emission`이 모델 predictor에 포함되지 않을 수 있음이 지적되어 있다.
- CHELSA daily target은 placeholder/comment에 가깝고, annual CHELSA는 현재 merged feature에 직접 포함되는지 확인 필요.
- Python/Snakemake 구현은 전환 초안으로 판단된다. 운영 경로로 쓰려면 R targets와 산출 schema 동등성을 별도로 검증해야 한다.
- 전체 `tar_make()`는 매우 무겁고 worker crash/OOM 가능성이 있었던 이력이 있다. branch별 subset 검증과 read-only 상태 확인을 우선한다.

## Operational context

- 저장소 루트에서 작업한다: `/members/dhnyu/huimori`.
- R target 변경 전에는 `targets::tar_manifest(callr_function = NULL)`와 `targets::tar_outdated(callr_function = NULL)`로 DAG 변화를 확인한다.
- target 실행 로그와 상태 분석은 `daehoon/docs/`와 `daehoon/outputs/*.md`에 축적되어 있다. 특히 grid landuse, daily feature, XGBoost schema 관련 문서를 먼저 읽는다.
- `_targets/`를 수동 삭제하거나 `git reset --hard`류 명령으로 작업 상태를 되돌리지 않는다.
- 대용량 raster/parquet 산출물은 외부 데이터 디렉터리 또는 `daehoon/outputs/`에 이미 존재할 수 있다. 문서 작업 중 재생성하지 않는다.
- grid emission 계산은 운영 정책 결정이 필요하다. 실제 계산을 원하면 `COMPUTE_GRID_EMISSIONS=true`를 명시해야 하며, 비용이 클 수 있다.
- 예측 GeoTIFF export는 메인 DAG 산출 뒤 `tools/export_to_raster.R` 같은 별도 스크립트로 수행하는 구조로 보인다. 최종 production export 경로와 파일명 규칙은 확인 필요.
