# Script for preparing ERA5 reanalysis data:
# 1. Calculate effective wind speeds from u and v wind speeds in two heights (10 and 100m)
# 2. Calculate alpha friction coefficient

from paths_usa import era_path
import glob
import numpy as np
import xarray as xr



out_files = glob.glob(era_path + '/eff_ws/*')

for year in range(0,35,2):
    wfile = era_path+'/eff_ws/era5_wind_USA_' + str(1951+year) + '-' + str(1951+year+1) + '.nc'
    afile = era_path+'/eff_ws/era5_alpha_USA_' + str(1951+year) + '-' + str(1951+year+1) + '.nc'
    if wfile not in out_files:
        files1 = glob.glob(era_path + '/era5_wind_USA_' + str(1951+year) + '??.nc')
        files2 = glob.glob(era_path + '/era5_wind_USA_' + str(1951+year+1) + '??.nc')
        files = files1 + files2
        files.sort()
        if len(files)==24:
            print('calculating wind ' + str(1951+year) + '-' + str(1951+year+1))
            data = xr.open_mfdataset(files, chunks = {'time': 38})
            wh10 = ((data.u10**2+data.v10**2)**0.5).compute()
            wh100 = ((data.u100**2+data.v100**2)**0.5).compute()
            print('saving wind ' + str(1951+year) + '-' + str(1951+year+1))
            eff_ws = xr.Dataset({'wh10': wh10,
                                 'wh100': wh100})
                             
            eff_ws.to_netcdf(wfile)
            eff_ws.close()
            del(eff_ws)
    if afile not in out_files:
        if wfile in glob.glob(era_path + '/eff_ws/era5_wind_USA_*.nc'):
            print('calculating alpha ' + str(1951+year) + '-' + str(1951+year+1))
            eff_ws = xr.open_dataset(wfile)
            alpha = (xr.ufuncs.log(eff_ws.wh100/eff_ws.wh10)/np.log(100/10)).compute()
            print('saving alpha ' + str(1951+year) + '-' + str(1951+year+1))
            xr.Dataset({'alpha': alpha}).to_netcdf(afile)
            del(alpha)


# 1950
wfile = era_path+'/eff_ws/era5_wind_USA_1950.nc'
afile = era_path+'/eff_ws/era5_alpha_USA_1950.nc'
if wfile not in out_files:
    files = glob.glob(era_path + '/era5_wind_USA_1950??.nc')
    if len(files)==12:
        print('calculating wind 1950')
        data = xr.open_mfdataset(files, chunks = {'time': 38})
        wh10 = ((data.u10**2+data.v10**2)**0.5).compute()
        wh100 = ((data.u100**2+data.v100**2)**0.5).compute()
        print('saving wind 1950')
        eff_ws = xr.Dataset({'wh10': wh10,
                             'wh100': wh100})
                         
        eff_ws.to_netcdf(wfile)
        eff_ws.close()
        del(eff_ws)
if afile not in out_files:
    if wfile in glob.glob(era_path + '/eff_ws/era5_wind_USA_*.nc'):
        print('calculating alpha 1950')
        eff_ws = xr.open_dataset(wfile)
        alpha = (xr.ufuncs.log(eff_ws.wh100/eff_ws.wh10)/np.log(100/10)).compute()
        print('saving alpha 1950')
        xr.Dataset({'alpha': alpha}).to_netcdf(afile)
        del(alpha)
        
# 2020
wfile = era_path+'/eff_ws/era5_wind_USA_2020.nc'
afile = era_path+'/eff_ws/era5_alpha_USA_2020.nc'
if wfile not in out_files:
    files = glob.glob(era_path + '/era5_wind_USA_2020??.nc')
    if len(files)==12:
        print('calculating wind 2020')
        data = xr.open_mfdataset(files, chunks = {'time': 38})
        wh10 = ((data.u10**2+data.v10**2)**0.5).compute()
        wh100 = ((data.u100**2+data.v100**2)**0.5).compute()
        print('saving wind 2020')
        eff_ws = xr.Dataset({'wh10': wh10,
                             'wh100': wh100})
                         
        eff_ws.to_netcdf(wfile)
        eff_ws.close()
        del(eff_ws)
if afile not in out_files:
    if wfile in glob.glob(era_path + '/eff_ws/era5_wind_USA_*.nc'):
        print('calculating alpha 2020')
        eff_ws = xr.open_dataset(wfile)
        alpha = (xr.ufuncs.log(eff_ws.wh100/eff_ws.wh10)/np.log(100/10)).compute()
        print('saving alpha 2020')
        xr.Dataset({'alpha': alpha}).to_netcdf(afile)
        del(alpha)

# 2021
wfile = era_path+'/eff_ws/era5_wind_USA_2021.nc'
afile = era_path+'/eff_ws/era5_alpha_USA_2021.nc'
if wfile not in out_files:
    files = glob.glob(era_path + '/era5_wind_USA_2021??.nc')
    print('calculating wind 2021')
    data = xr.open_mfdataset(files, chunks = {'time': 38})
    wh10 = ((data.u10**2+data.v10**2)**0.5).compute()
    wh100 = ((data.u100**2+data.v100**2)**0.5).compute()
    print('saving wind 2021')
    eff_ws = xr.Dataset({'wh10': wh10,
                         'wh100': wh100})
                         
    eff_ws.to_netcdf(wfile)
    eff_ws.close()
    del(eff_ws)
if afile not in out_files:
    if wfile in glob.glob(era_path + '/eff_ws/era5_wind_USA_*.nc'):
        print('calculating alpha 2021')
        eff_ws = xr.open_dataset(wfile)
        alpha = (xr.ufuncs.log(eff_ws.wh100/eff_ws.wh10)/np.log(100/10)).compute()
        print('saving alpha 2021')
        xr.Dataset({'alpha': alpha}).to_netcdf(afile)
        del(alpha)