# Phuket-Coral-Cover-IOD-ENSO
Analysis of coral cover on the reef flat at Phuket and the effects of IOD and ENSO on sea level and sea temperature.

This folder contains R code and data used for a submission to the journal 'Global Change Biology' for a paper titled:

     The Indian Ocean Dipole and El Niño Southern Oscillation as major drivers of coral cover on shallow reefs in the Andaman Sea
     Authors: R.P. Dunne, B.E. Brown, N. Phongsuwan, L. Putchim

All R code was written and run under R version 4.0.3 (2020-10-10) using R Studio.

TO RUN THE R CODE YOU SHOULD DOWNLOAD BOTH THE CODE FILES AND THE DATA AND PLACE THEM IN A SINGLE FILE FOLDER. YOU WILL THEN SEE THAT EACH R CODE FILE STARTS BY ASSIGNING THE FOLDER IN USE. YOU WILL NEED TO CHANGE THIS TO YOUR FOLDER ADDRESS. THE CODE SHOULD THEN RUN WITHOUT ANY PROBLEMS SO LONG AS YOU HAVE INSTALLED THE NECESSARY PACKAGES ON YOUR VERSION OF R. 

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
12. R code - 'Phototransect Amalysis - GAMM models.R'. This code requires data from 'Coral Cover and Covariates.csv'. After computing GAMMS it produces the plots in Figure 7 in the paper.
13. Data File - 'Coral Cover and Covariates.csv'. 
14. R code - 'Phuket CMEMS 008-057 NetCDF data - Grid - Figure S2.R'. This code requires data from the NetCDF file 'dataset-duacs-rep-global-merged-twosat-phy-l4_1608108067774.nc'. It is used to produce Figure 2S in the Supplementary Information to the paper. 
15. Data File - 'dataset-duacs-rep-global-merged-twosat-phy-l4_1608108067774.nc'.
16. A PDF File "Additional Information on the Datasets and Modelling". This contains additional information about (1) the Indian Ocean Dipole, (2) El Nino Southern Oscillation, (3) the satellite altimetry and the data used for both. It then contains a record of the detailed GAMM modelling which the R code above runs, including details of the data exploration and model validation. This file will assist anyone who wishes to run the R code for themselves. 
