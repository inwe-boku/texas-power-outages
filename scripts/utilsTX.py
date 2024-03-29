import xarray as xr
import numpy as np
from scipy.interpolate import interp1d
import pandas as pd

import sys
sys.path.append('./..')
from paths import ryberg_path

def windpower_simulation_era5(windh100,alpha,hubheight,capacity,specific_pow,lons,lats,GWAcf=[]):
    '''
    function for simulating wind power generation
    
    input parameters:
    
    windh100	wind speeds dataset with effective wind speeds in one height (100m)
    alpha		alpha friction coefficient calculate from wind speeds in two heights
    hubheight	vector of hub height of different turbines
    capacity	installed capacity of turbines
    lons, lats	locations of wind power plants
    commissioning	commissioning date of wind power plants, can be year only or date in datetime format
    GWA			empty list by default, if wind speed correction with GWA desired provide GWA
    '''
    
    # interpolate wind to locations of turbines
    # for the simulation
    wind = windh100.interp(coords={"longitude":xr.DataArray(lons,dims='location'),
                                                                    "latitude":xr.DataArray(lats,dims='location')},
                                                                    method="nearest").compute()
    
    # interpolate alpha to locations of turbines
    alphai = alpha.interp(coords={"longitude":xr.DataArray(lons,dims='location'),
                                                                   "latitude":xr.DataArray(lats,dims='location')},
                                                                   method="nearest").compute()
    # calculate wind at hubheight using alpha
    windhh = (wind * (hubheight/100)**alphai).compute()
    
    # apply GWA bias correction
    if(len(GWAcf)>0):
        # interpolate to turbine locations
        GWAcf_locations = GWAcf.interp(coords={"longitude":xr.DataArray(lons,dims='location'),
                                           "latitude":xr.DataArray(lats,dims='location')},
                                   method="nearest").compute()
                # apply correction factor
        windhhg = (windhh * GWAcf_locations).compute()
        # replace wind speeds higher than 25 m/s with 0, because cutout windspeed
        windhhg = windhhg.where(windhhg<=25,0)
    else:
        windhhg = windhh.where(windhh<=25,0)
    
    # Ryberg power curve model
    RybCoeff = pd.read_csv(ryberg_path+"/ryberg_coeff.csv")
    A = xr.DataArray(RybCoeff.A, dims = 'CF')
    B = xr.DataArray(RybCoeff.B, dims = 'CF')
    wp1 = power_curve(windhhg,A,B,specific_pow,len(windhhg.location))
    
    # multiply with installed capacity
    wp2 = capacity*wp1/100
    
    
    return(wp2)




    

    
    
def stats(sim,obs,rd=True):
    '''
    function for calculating statistics for analysis of simulated wind power generation time series
        
    input parameters:
    
    sim		simulated windpower time series, pandas Series
    obs		observed windpower time series, pandas Series
    rd		round results to two digits
    
    sim and obs need to have the same length and index
    
    '''
    cor = np.corrcoef(sim,obs)[0,1]
    rmse = (((sim-obs)**2).mean())**0.5
    mbe = (sim-obs).mean()
    mean = sim.mean()
    
    if rd:
        return([round(i,2) for i in [cor,rmse,mbe,mean]])
    else:
        return([cor,rmse,mbe,mean])
        
        
        
    
    
def powerfunc(wind, CF, v):
    return interp1d(v, CF)(wind)
    
    
    
    
# functions for handling larger states (with more locations)
def windpower_simulation_era5_large(windh100,alpha,hubheight,capacity,specific_pow,lons,lats,GWAcf=[]):
    # interpolate wind to locations of turbines
    # for the simulation
    wind = windh100.interp(coords={"longitude":xr.DataArray(lons,dims='location'),
                                                                    "latitude":xr.DataArray(lats,dims='location')},
                                                                    method="nearest")
    # interpolate alpha to locations of turbines
    alphai = alpha.interp(coords={"longitude":xr.DataArray(lons,dims='location'),
                                                                   "latitude":xr.DataArray(lats,dims='location')},
                                                                   method="nearest")
    # calculate wind at hubheight using alpha
    windhh = (wind * (hubheight/100)**alphai)

    # apply GWA bias correction
    if(len(GWA)>0):
        # interpolate to turbine locations
        GWAcf_locations = GWAcf.interp(coords={"longitude":xr.DataArray(lons,dims='location'),
                                           "latitude":xr.DataArray(lats,dims='location')},
                                   method="nearest")
        # apply correction factor
        windhhg = (windhh * GWAcf_locations)
        # replace wind speeds higher than 25 m/s with 0, because cutout windspeed
        windhhg = windhhg.where(windhhg<=25,0)
    else:
        windhhg = windhh.where(windhh<=25,0)

    # Ryberg power curve model
    RybCoeff = pd.read_csv(ryberg_path+"/ryberg_coeff.csv")
    A = xr.DataArray(RybCoeff.A, dims = 'CF')
    B = xr.DataArray(RybCoeff.B, dims = 'CF')
    wp1 = power_curve(windhhg,A,B,specific_pow,len(windhhg.location))

    # multiply with installed capacity
    wp2 = capacity*wp1/100

    return(wp2)
    

	
def power_curve(wind,A,B,spec_pow,num_locations):

    spec_pow = xr.DataArray(spec_pow, dims='location')
    v = xr.concat((xr.DataArray(0 * np.ones((1,num_locations)), dims=('CF', 'location'), coords={'CF': [0]}),
                   np.exp(A+B*np.log(spec_pow)),
                   xr.DataArray(25 * np.ones((1,num_locations)), dims=('CF', 'location'), coords={'CF': [100]}),),
                  dim='CF')

    CF = np.concatenate((np.array([0]),np.linspace(0, 100, num=A.sizes['CF']),np.array([100])))
    CF = xr.DataArray(CF, dims='CF', coords={'CF': v.CF})

    v_gridded = xr.DataArray(np.linspace(0, 25, num=300), dims='v', coords={'v': np.linspace(0, 25, num=300)})

    power_curves_grid = xr.apply_ufunc(powerfunc,v_gridded,CF,v,
                                       input_core_dims=[[],['CF'],['CF']],
                                       vectorize = True)
    
    try:
        wind_new = wind.assign_coords(location=np.arange(len(wind.location))).drop(['longitude','latitude'])
    except:
        wind_new = wind.assign_coords(location=np.arange(len(wind.location))).drop(['lon','lat'])
    if 'x' in list(wind_new.dims):
        wind_new = wind_new.drop(['x','y'])
    if(num_locations == 1):
        wp1 = power_curves_grid.sel(location=0).interp(v = wind_new,
                                                       method = 'linear')
    else:
        wp1 = power_curves_grid.interp(v = wind_new,
                                       location = wind_new.location,
                                       method = 'linear')
    wp1 = wp1.drop('v').assign_coords(time=wind.time)
    return(wp1)
	
