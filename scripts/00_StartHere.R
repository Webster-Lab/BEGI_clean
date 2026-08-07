# READ ME

# The purpose of this script is to provide an overview and prepare users first cloning this repo to run through the workflow. 

# ABOUT:
# This repo provides a reproducible analysis of Eve Tipps et al's 2023-2024 floodplain carbon cycling project, which was the first study associated with the Webster Lab's Bosque Ecosystem Groundwater Interactions (BEGI) project.
# Note that the related repo "BEGI" (https://github.com/Webster-Lab/BEGI) contains original scripts and data for handling data from the 2023-2024 field campaign and for Eve Tipp's MS thesis. The "BEGI_clean" repo here contains an updated analysis and cleaned-up scripts associated with peer-reviewed publication of Eve's thesis.

# FIRST: 
# This repo has the renv package associated with it to maintain the repo’s own package library. This provides users with the exact version of every package used when the repo was created so that the code doesn’t break when packages are updated or stop being maintained over time. When you first clone the repo, run renv::restore() in the console. If it asks "Would you like to try installing the latest available versions of these packages?", say "n". 

# NEXT: 
# Users should use the following run order: 
# 01_compileEXO.R -> 02_depthtogw.R -> 03_datacompilation.R -> 04_eventdelineation.R -> 05_eventcalcs.R -> 06_clusteranalysis.R -> 07_eventstats.R

# Scripts overview:
# 01_compileEXO.R: Compiles 15-min EXO1 sonde data
# 02_depthtogw.R: Compiles 15-min groundwater depth data
# 03_datacompilation.R: Combines all data and makes pub-ready plots of full time series
# 04_eventdelination.R: Delineates individual dissolved oxygen events and makes pub-ready plots of each event
# 05_eventcalcs.R: Calculates total event size, ecosystem respiration, and accrual metrics for every DO event
# 06_clusteranalysis.R: Performs cluster analyses on a) groundwater depth changes 2 days before DO events and b) fDOM changes in the 2 days following the start of a DO event. B) is used to flag DO events as possible chemical oxidation.
# 07_eventstats.R: calculates mean and variance of groundwater depth and temperature in 2 days before DO events, for use as predictors in exploratory models