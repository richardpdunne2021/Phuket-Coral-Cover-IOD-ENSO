# Phuket-Coral-Cover-IOD-ENSO
Analysis of coral cover on the reef flat at Phuket and the effects of IOD and ENSO on sea level and sea temperature.

This folder contains R code and data used for a submission to the journal 'Global Change Biology' for a paper titled:

     The Indian Ocean Dipole and El Niño Southern Oscillation as major drivers of coral cover on shallow reefs in the Andaman Sea
     Authors: R.P. Dunne, B.E. Brown, N. Phongsuwan, L. Putchim

All R code was written and run under R version 4.0.3 (2020-10-10) using R Studio.

Contents:
1. R code - 'Analysis of DMI from NOAA NetCDF data - Fig 1A.R'. This code requires data from the NetCDF file 'dmi to 13 Jan 2021.nc'. It is used to produce Fig 1A in the paper and to output monthly values of DMI to a CSV file for use in the later GAMM models.
2. Data File - 'dmi to 13 Jan 2021.nc'.
3. R code - 'Analysis pf Nino 3.4 from NOAA NetCDF data - Figure 1B.R'. This code requires data from the NetCDF file 'Nino 3.4 to 13 Jan 2020.nc'. It is used to produce Figure 1B in the paper and to output monthly values of Niono 3.4 to a CSV file for use later in the GAMM models.
4. Data File - 'Nino 3.4 to 13 Jan 2021.nc'. 
5. R code - 'Phuket CMEMS 008-057 NetCDF data - GAMM models.R'. This code requires data from the NetCDF file 'dataset-duacs-rep-global-merged-twosat-phy-l4_1608108067774.nc' and 'DMI Nino 3.4 monthly.csv'. It is used to run the GAMM models for the absolute sea level data and to produce Figures 2 and 3 in the paper.
6. Data File - 'DMI Nino 3.4 monthly to 14 mar 2020.csv'. 
7. R code - 'Phuket CRW data - GAMM Models.R'. This code requires data from 'CRW data for Phuket square - 1 May 1985 to 13 Jan 2021.csv' and 'DMI Nino 3.4 monthly May 1995 to Jul 2020.csv'.
8. Data file - CRW data for Phuket square - 1 May 1985 to 13 Jan 2021.csv
9. Data file - DMI Nino 3.4 monthly May 1995 to Jul 2020.csv
10. R code - 'Plotting of Phototransect Results Figures 5 & 6.R'. This code is used to produce Figures 5 and 6 and requires data from the file 'Coral Cover Phototransects.xlsx'
11. Data File - 'Coral Cover Phototransects.xlsx'.
12. R code - 'Phuket CMEMS 008-057 NetCDF data - Grid - Figure S2.R'. This code requires data from the NetCDF file 'dataset-duacs-rep-global-merged-twosat-phy-l4_1608108067774.nc'. It is used to produce Figure 2S in the Supplementary Information to the paper. 
13. Data File - 'dataset-duacs-rep-global-merged-twosat-phy-l4_1608108067774.nc'.

