import xarray as xr
import glob

temp_path = '/data/users/kgruber/Data/era5/TX/'
file_path = '/data/users/kgruber/other-data/TX/'
res_path = '/data/users/kgruber/results/TX/'

years = range(1950,2021)

# load population
pop = xr.open_dataarray(file_path + 'pop_era5_rel.nc')

for year in years:
    out = res_path + 'temppop_'+str(year)+'C.nc'
    if out not in glob.glob(res_path + '*C.nc'):
        files = temp_path + 'era5_temp_TX_'+str(year)+'??.nc'
        if len(glob.glob(files))==12:
            temp = xr.open_mfdataset(files).t2m-273.15
            (temp*pop).sum(['longitude','latitude']).to_netcdf(out)

# 2021
out = res_path + 'temppop_2021C.nc'
files = temp_path + 'era5_temp_TX_2021??.nc'
temp = xr.open_mfdataset(files).t2m - 273.15
(temp*pop).sum(['longitude','latitude']).to_netcdf(out)