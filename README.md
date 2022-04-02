[![MIT
License](https://badgen.net/github/license/inwe-boku/texas-power-outages)](https://choosealicense.com/licenses/mit/)


Profitability and investment risk of Texan power system winterization
=====================================================================

This repository contains code complementing an analysis of the 2021 cold spell in Texas. Data resulting from this analysis can be found at [Zenodo](https://doi.org/10.5281/zenodo.5902745). Input data needs to be [downloaded from their original sources](#data-download) to reproduce the results. The paper can be found [here](https://www.nature.com/articles/s41560-022-00994-y).


Abstract
--------

A lack of winterization of power system infrastructure resulted in significant rolling blackouts in Texas in 2021 though debate about the cost of winterization continues. Here, we assess if incentives for winterization on the energy only market are sufficient. We combine power demand estimates with estimates of power plant outages to derive power deficits and scarcity prices. Expected profits from winterization of a large share of existing capacity are positive. However, investment risk is high due to the low frequency of freeze events, potentially explaining under-investment, as do high discount rates and uncertainty about power generation failure under cold temperatures. As the social cost of power deficits is one to two orders of magnitude higher than winterization cost, regulatory enforcement of winterization is welfare enhancing. Current legislation can be improved by emphasizing winterization of gas power plants and infrastructure.

How to
======

Data download
-------------

Data that can be downloaded automatically:
- temperatures: **scripts/download_era5_TX_temp.py** (downloads temperatures for the area of Texas 1950-2021)
- wind speeds: **scripts/download_era5_USA.py** (downloads wind speeds for the area of the USA 1950-2021)
uses *scripts/logging_config.py*, *scripts/config.py*

Data that needs to be downloaded manually is described in section *input* the [data repository on Zenodo](https://doi.org/10.5281/zenodo.5902745).


Data preparation
----------------
- creating timeseries of outages from raw ERCOT data: **scripts/R/create-ercot-outage-timeseries.R** reads raw outage data provided by ERCOT and creates outage timeseries per power plant per minute and joins it with a different data source (i.e. outage data by Edgar Virguez [here](https://bit.ly/EGOVADatabase))
- preparation of wind speeds: **scripts/prepare_USA_ERA5.py** calculates effective wind speeds and Hellman's exponents from wind speeds in u and v direction
- preparation of turbine data: **scripts/prepare_TX_turbines.py** prepares USWTDB turbine data for Texas
- outages: **notebooks/prepare_outages_NSsplit.ipynb** prepares outage data, aggregates per technology and splits into North and South
- population: **notebooks/population.ipynb** prepares population data


Temperature weighting
---------------------

- population weighted temperature: **scripts/calc_temppopC.py** weights ERA5 temperatures by population density 
- wind park weighted temperatures: **notebooks/wp_temp_NSsplit.ipynb** weights ERA5 temperatures by wind parks and splits into Northern and Southern Texas
- power plant and gas field weighted temperatures: **notebooks/temperatures_NSsplit.ipynb** weights ERA5 temperatures by wind and coal gas power plant and gasfield capacity


Wind power simulation
---------------------

- wind power simulation for Texas: **notebooks/windpower_ERA5_GWA2_const_cap.ipynb** simulates wind power generation from prepared ERA5 wind speeds with GWA2
uses *scripts/utilsTX.py*


Outage model
------------

- simulating temperature dependent outages: **notebooks/outage-model_pr.ipynb** estimates parameters for modelling outages of gas, coal in Texas and wind in Northern and Southern Texas
- simulating available capacity and modelling loss of load: **notebooks/outages_thresholds_gasPP_vs_gasfield_LR24temptrend_Hook-8.ipynb** generates outage time series for gas, coal and wind power and resulting available capacity, and loss of load


Load model
----------

- load model: **notebooks/load_estimation_LR24_temptrend_Hook-8.ipynb** models population weighted temperature dependent load


Frequency analysis and determining revenues from winterization
--------------------------------------------------------------
- temperature trend: **notebooks/temperature_trend.ipynb** plots population weighted temperatures and trendline
- summary of results: **scripts/R/events.R** summarises temperature and loss of load time series for extreme value analysis
- trend in extreme events: **scripts/R/figure-trend-extreme-events.R** plots trend in loss of load events
- loss of load events: **scripts/R/figure-deficit-events.R** plots each of the 9 loss of load events (load and temperature) within the seven decades
uses *scripts/R/functions_tg.R*
- return periods and probability of extreme events: **scripts/R/figure-quantile-plots.R** plots quantile plots of temperature and load extreme events
uses *scripts/R/functions_tg.R*
- estimation of marginal revenues: **notebooks/outages_reduced_bootstrap_LR24temptrend_Hook-8.ipynb** bootstraps from loss of load and varies the outage of each technology to estimate marginal revenues from winterization
- sensitivity of marginal revenues to choice of modelling parameters: **notebooks/outages_thresholds_sensitivity_LR24temptrend_Hook-8.ipynb** varies outage and recovery threshold temperatures
- estimation of marginal revenues without 2021 event: **notebooks/outages_reduced_bootstrap_2020_LR24temptrend_Hook-8.ipynb**
- estimation of marginal revenues with 10% discount rate: **notebooks/outages_reduced_bootstrap_10_LR24temptrend_Hook-8.ipynb**
