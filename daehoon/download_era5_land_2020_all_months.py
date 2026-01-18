import cdsapi
import calendar
import os

client = cdsapi.Client()

dataset = "reanalysis-era5-land"

variables = [
    "2m_temperature",
    "surface_net_solar_radiation",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    "surface_pressure",
    "total_precipitation"
]

year = 2020
times = [f"{h:02d}:00" for h in range(24)]

# spatial extent: [North, West, South, East]
area = [39, 124, 33, 132]

for month in range(1, 13):
    _, n_days = calendar.monthrange(year, month)
    days = [f"{d:02d}" for d in range(1, n_days + 1)]

    out_file = f"ERA5_Land_{year}_{month:02d}.zip"

    if os.path.exists(out_file):
        print(f"{out_file} already exists, skipping.")
        continue

    request = {
        "variable": variables,
        "year": str(year),
        "month": f"{month:02d}",
        "day": days,
        "time": times,
        "area": area,
        "format": "netcdf"
    }

    print(f"Downloading {out_file} ...")
    client.retrieve(dataset, request, out_file)
