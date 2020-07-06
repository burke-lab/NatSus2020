# Dust pollution from the Sahara and African infant mortality


Replication materials for Heft-Neal, Burney, Bendavid, Voss & Burke 2020 [`Dust pollution from the Sahara and African infant mortality'](https://www.nature.com/articles/s41893-020-0562-1).

The materials in this repository allow users to reproduce the figures and calculations appearing in the main text and extended data of the paper.

If you find meaningful errors in the code or have questions or suggestions, please contact Sam Heft-Neal at sheftneal@stanford.edu.

## Organization of repository

* **scripts**: scripts for downloading data and for replication of figures and calculations.
* **figures/raw**: scripts will generate pdf figures in this directory.
* **figures/published**: published versions of the figures. Cosmetic editing done in Adobe Illustrator to go from raw -> final versions.
* **data/inputs**: data inputs for analysis.
* **data/figure_data**: data used for any figures that don't use the main analysis data. Directory includes pre-processed files with replication materials. Anytime a *dataPrep.R script runs it generates new figure data in files of the same name but ending with "_new" and all subsequent runs of plotting scripts bring in the new data instead of the pre-processed data included with replication materials.
* **NatSus2020_replication.Rproj**: organizes the replication materials into an RStudio Project.

## Instructions
The repository is ~360Mb (and >2Gb if you download all the raw data using script 00)

Users can manage replication through the R project "NatSus2020_replication.Rproj". Alternatively users can set working directory to NatSus2020 and run scripts independently.

* A note about replication: Ideally we would like to provide replication materials for the entire pipeline beginning with the raw publicly available data and ending with the results presented in the paper. However, due to the [DHS data use agreement](https://dhsprogram.com/data/Terms-of-Use.cfm), we cannot post the individual level DHS data with location identifying information. Furthermore, our analysis combines data from 82 different surveys so it's not as simple as downloading a single file from the DHS website. We therefore have included the following materials to facilitate replication:

* **data/inputs/analysis_data.rds** This file includes the final processed data used in the analysis with all location identifying information removed. This file can be used to replicate the results in the paper but because identifying information has been removed, it cannot be used to replicate pre-processing since those steps require linking exposures to individual DHS observations based on location and timing of births.

* **data/inputs/dhs/dhs_locations.csv** This file includes the locations of all DHS clusters included in the analysis. All information that could be used to link the locations to the birth data has been removed. However, this file is used in the replication materials for some figures that do calculations at the locations of DHS clusters but do not involve the birth data (e.g., figure 5). Moreover, this file will also eventually be used to replicate all but the final step of pre-processing the exposure data using Script 19. When we were doing data processing for the paper we only processed exposures for the years and months we observed births in each location. However, if we provide birth year and birth months by location then it would be possible to identify locations for some of the observations. We are therefore working on re-writing our code to process exposures for all DHS locations for all location-birth year-birth month combinations. This would then allow full replication from raw data through results with the exception of a single line of code joining exposures with birth data on cluster_id, child_birth_year, and child_birth_month. However, the revised process required to maintain anonymity would necessitate processing nearly 6 million rows of data instead of the approximately 750K rows that we actually processed (since we ignored year-month-location combinations without observations) so still working on this.

* **data/inputs/dhs/dhs_surveys.csv** This is a list of surveys included in the analysis. In order to replicate our sample from the raw DHS data you would need to first [register](https://dhsprogram.com/data/new-user-registration.cfm) for access to the data, then you could use this csv file to identify the relevant surveys to [download](https://dhsprogram.com/data/available-datasets.cfm). Reshaping the individual recode (IR) files to be at the birth level, appending all surveys, subsetting to births that occurred between 2001 and 2015, and dropping children that were alive for less than 12 months at the time of survey should reproduce our sample. For questions on any of these steps contact Sam at sheftneal@stanford.edu.

* **data/inputs/** Includes a combination of empty directories that are populated when running script 00 and directories with pre-processed data that are not used in the main analysis but are used for generating some components of figures.

* **Scripts**

Script 00 downloads the PM2.5, DAOD, and CHIRPs rainfall data used in the analysis. THIS STEP IS OPTIONAL. R scripts have been divided into processing scripts and scripts that generate figures. Pre-processed versions of these data are inclueed in analysis_data.rds, or in files in the directory data/figure_data, which are called by the scripts that generate the figures. Therefore if you only want to generate the results and figures from pre-processed data then you can skip this step. If you want to replicate data processing then this step is required.

Script 01 processes the data for figure 1 (script 00 must be run first)

Script 02 generates figure 1

Script 03 generates figure 2 panel a

Script 04 processes data for figure 2 panels b and c (slow, estimated runtime >30 min if you run all 1,000 iterations)

Script 05 generates figure 2 panels b and c 

Script 06 generates figure 3 panel a

Script 07 generates figure 3 panel b

Script 08 processes data for figure 4 (script 00 must be run first. slow, estimated runtime >15 min if you run 1,000 iterations)

Script 09 generates figure 4

Script 10 processes data for figure 5 (script 00 must be run first. Estimated runtime >5 min if you run all 1,000 iterations)

Script 11 plots figure 5 (> 1 min if you run 1,000 iterations)

Script 12 plots figure ED1

Script 13 processes the data for figure ED2 (script 00 must be run first. Estimated runtime >10 min)

Script 14 generates figure ED2

Script 15 processes the data for figure ED3 (script 00 must be run first)

Script 16 generates figure ED3

Script 17 generates figure ED4

Script 18 generates figure ED5

Script 19 will process the raw exposure data for all DHS locations. Note the processed output cannot be merged with the birth data. This script is a work in progress (see above for details).



## R packages required
* **classInt**
* **data.table**
* **fields**
* **latex2exp**
* **lfe**
* **plotrix**
* **R.utils**
* **RANN**
* **raster**
* **sp**
* **tidyverse**

Scripts were originally written with R 3.6.3 then ultimately run with R 4.0.0.

Users can run the following command to install the most recent versions of these packages:

```R
install.packages(c('classInt', 'data.table', 'fields', 'latex2exp', 'lfe', 'plotrix', 'R.utils', 'RANN', 'raster', 'sp', 'tidyverse'), dependencies = T)
```
