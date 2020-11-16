# temperature_cleaning
Scripts to interactively prepare temperature time series for further analysis. Reads in data directly from Onset logger *.hobo files that have been saved as *.csv.
Use these scripts at your own risk. They were not developed to be fully generalized, and may contain watershed-specific idiosyncracies.

cleaning_functions: functions used by main scripts to read in, plot, clip to date endpoints, remove erroneous readings, collate across years, etc.
clean_Watershed_water: main script for cleaning water temperature; this script calls 'cleaning_functions'.
clean_Watershed_air: main script for cleaning air temperature; this script calls 'cleaning_functions'.
