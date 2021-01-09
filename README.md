# EFCLUM

This project uses Ecological Footprint data alongside sets of international data series downloaded and aggregated into the
7 final consumption categories linked to major areas of economic services.
The first script downloads SDG Indicator data from the API, then selects specific series, and downloads specific selected data
series from the World Bank data API. The data is organised by datasource, consumption categroy and year. 2 normalized
'economic service qulity' indicators are created from the various data series for each consumption categroy/year - a 
MinMax normalization (minimum values become 0, max become 1, and values in between are spaced accordingly, and a
zscore normalization (each data value is assigned a value relative to the mean and standard deviation of that series.
Data outputs to csv files.
The 2nd script reads the output csv, country name correspondence and population data and reaggregates to match the GTAP country
grouping.
3.a and 4 are Shiny app versions of the results with different data sources.
3.b builds plots of all the results.
5. Is the (abandoned) start of an attempt to write a paper using Rmarkdown.

 
For reasons of herstorical logistics the actual working directory is 2 levels down in GFN_Data_Visualization\\ScatterVisuals
