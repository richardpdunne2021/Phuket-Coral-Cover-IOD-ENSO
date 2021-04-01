# Created by : Richard Dunne
# Last updated: 1 April 2021

# This R script uses CMEMS Satellite Altimetry Data in NETCDF file format to analyse and plot long term absolute sea-level rise in the Phuket sea area


# ____________ SET DIRECTORY ____________________

# First set the directory in which we are working in the properties section of R
# change the file address below to where you have stored the NetCDF data file on your computer

setwd ("E:/Our Papers/2020 IOD and Coral Cover/Resubmission/Data & R Code")
# then double check that you are in this directory before running any script
getwd()

# Load package
library(ncdf4)
# useful source of code is: http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm

# the Phuket are data - daily values of gridded SSH ("sla[longitude,latitude,time]") in metres from 1993 to 7 March 2020 Grid Area is 96-100E, 6-10N
fn <- "dataset-duacs-rep-global-merged-twosat-phy-l4_1608108067774.nc" 

nc <- nc_open(fn)

# look at the variables etc within the NETCDF file
print(nc)
attributes(nc$var)

# Get the longitudes and latitudes
lon <- ncvar_get(nc, "longitude")
nlon <- dim(lon)
# head(lon)

lat <- ncvar_get (nc, "latitude")
nlat <- dim(lat)
# head(lat)

print(c(nlon,nlat)) # confirms the dimensions of the data # 17 x 17 grid for Phuket

# extract the time in days - which is called "time" in the NETCDF file
tm_days <- ncvar_get(nc,"time")
# head (tm_days) # display this data if we want to
nt <- dim(tm_days) # length of time series 
# dim(tm_hours)

# The time above (tm_days) is in the format days since 1950-01-01 00:00:00 so need to convert this to a meaningful date. NOTE multiply by 86400 to get seconds
tm <- as.POSIXct(86400 * (tm_days), origin = '1950-01-01', tz = "GMT") 
head(tm) # lists first few data
tail(tm) # lists last few data
# check class of time field
class(tm)

# alternative format of time as a class of "Date"
tmdate <- as.Date(tm_days, origin='1950-01-01')
class(tmdate)

# Get the variable for Sea Level Anomaly (sla) and its attributes, and verify the size of the array which is 17 long x 17 lat x 9928 days data. Grid is 0.25deg.
tmp.array <- ncvar_get(nc, "sla")
dlname <- ncatt_get(nc, "sla", "long_name")
dunits <- ncatt_get(nc, "sla", "units")
fillvalue <- ncatt_get(nc, "sla", "_FillValue") # is -2147483647
dim(tmp.array)

# close the NetCDF file - good practice generally although here we can't write to it accidentally and so corrupt it.
nc_close(nc)

# Replace NetCDF missing values with NAs for any R analysis
tmp.array[tmp.array == fillvalue$value] <- NA
# The total number of non-missing grid cells can be got by determining the length of a vector of values representing one slice from the brick, omitting the NA values - the slice is a grid 17*17 = 289 
length(na.omit(as.vector(tmp.array[, , 1]))) # this returns 245 for the Phuket data so there are 289-245 = 44 grids with NAs where the land is.

# NetCDF variables are read and written as one-dimensional vectors (e.g. longitudes), two-dimensional arrays or matrices (raster "slices"), or multi-dimensional arrays (raster "bricks"). In such data structures, the coordinate values for each grid point are implicit, inferred from the marginal values of, for example, longitude, latitude and time. In contrast, in R, the principal data structure for a variable is the data frame. In the kinds of data sets usually stored as NetCDF files, each row in the data frame will contain the data for an individual grid point, with each column representing a particular variable, including explicit values longitude and latitude (and perhaps time). In the data set considered here, the variables would consist of longitude, latitude and XX columns of monthly mean SSH, with the full data set thus consisting 'r nlon' by 'r nlat' rows and 'r nt+2' columns.


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   CONVERT THE WHOLE ARRAY TO A DATA FRAME  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 
# Convert the nlon by nlat by nt array into a nlon by nlat by nt (days) matrix. (This will work if the NetCDF data set was written as a CF-compliant data set, with arrays dimensioned as in Fortran, as nlon x nlat x nt arrays) First, create a long vector tmp.vec.long using the as.vector() reshaping function, and verify its length.

tmp.vec.long <- as.vector(tmp.array)
length(tmp.vec.long) # check that this is the correct length = 2,782,781 ie 17 lat x 17 long x 9629 daily values

# Then reshape that vector into a 9928 by 289 matrix using the matrix() function, and verify its dimensions, which should be 17x17=289 (lat/lon grid) by 9928 (time length - months between 1993- 7 Mar 2020).A matrix can only contain one type of data - here NUM

# create the matrix of rows 17x17=289 and 9928 columns. 
tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt) 
dim(tmp.mat) # check the dimensions are 289 x 9928
# head (tmp.mat)
# head (na.omit(tmp.mat))

# we need to add a row in this data frame for our values of X to run a regression later - this will be the date which was put in the variable "tm_days" above
# This means that the computed regression slope will be in days rather than years.
time_df <- data.frame(tm_days) # this is in a vertical column
# transpose this into rows rather than columns
time_DF <- as.data.frame(t(time_df)) # 9629 rows
# now rbind the new data frame to the matrix
tmp.SSHxtime <- data.frame(rbind(tmp.mat, time_DF)) # this way round puts the Sea Level Anomaly rows first and adds the tm_hours to the bottom

# also add the date/time in POSIXct format. We can't do this as we did above by adding a POSIXct row because we then have mixed types in each column. So we must add the POSIXct time as a column
# make a dataframe with the POSIXct date/time as a vertical column
timePOSIXct_df <- data.frame(tm) 
# take the dataframe "tmp.SSHxtime" and transpose it to columns
tmp.SSHxtime_columns <- as.data.frame(t(tmp.SSHxtime))
nc <- ncol(tmp.SSHxtime_columns)

# now cbind the new data frame to the matrix
tmp.SSHxtime_POSIXct_columns <- data.frame(cbind(tmp.SSHxtime_columns, timePOSIXct_df)) # this way round puts the POSIXct column last

# multiply columns 1-289 (ie all data excluding the time columns 290 and 291) by 1000 to convert from metres to mm
tmp.SSHxtime_POSIXct_columns[,1:289] <- tmp.SSHxtime_POSIXct_columns[,1:289]*1000

# TIME BOUNDS FOR DATA
min(timePOSIXct_df$tm)
max(timePOSIXct_df$tm)

# __________________________________________________________________________________________________________________

# __________________________TO COMPUTE SEA LEVEL RISE FOR ALL THE LAT/LONG GRIDS   __________________________________

# MULTIPLE COLUMNS TOGETHER by using lapply such that each result will be stored in a list which we can access later for extracting the coefficients

# regression doesn't seem to like NAs -whether using na.exclude or na.omit - see where these columns are
colnames(tmp.SSHxtime_POSIXct_columns)[colSums(is.na(tmp.SSHxtime_POSIXct_columns)) > 0]

# If there are NAs then we need to change NA to a numerical value, say 0 to preserve the size of the matrix whilst still allowing the regression function to work 
tmp.SSHxtime_POSIXct_columns[is.na(tmp.SSHxtime_POSIXct_columns)] <- 0

# options(max.print=999999) # so that all the column names will be printed rather than the abbreviated selection default
# names(tmp.SSDxtime_columns) # tells us the names of the columns which are simply numbered 1 to 357 of the variables we want to regress against tm_hours

varlist <- names(tmp.SSHxtime_POSIXct_columns)[1:289] # here we store the 289 column names for regressing

models1 <- lapply(varlist, function(x) lm(substitute(i~ tm_days, list(i = as.name(x))), data = tmp.SSHxtime_POSIXct_columns))

summary (models1[[1]]) # to check just print summary for Column 1
# lapply(models1, summary) # send all summaries to console - very long list

~ --------------------------------------------------------------------------------------------------
# NOW EXTRACT THE SLOPE COEFFICIENTS AND SEND TO DATAFRAME
  
models1[[10]]$coefficients["tm_hours"] # extracts slope for one model at time - here it is column 10 and print to console to view

coefficients.lst <- lapply(models1,coefficients) # outputs both the intercept and slope - we only need the slope

coefficients.df <- data.frame(matrix(unlist(coefficients.lst), nrow=length(coefficients.lst), byrow=T)) # make this into a dataframe containing two columns for intercept and slope

#rename the columns "Intercept" and "Slope"
names(coefficients.df)[1] <- "Intercept"
names(coefficients.df)[2] <- "Slope"

# NEXT WE NEED TO APPEND THESE COLUMNS ON TO A DATAFRAME WITH THE LAT & LONG DATA IN COLUMNS so that we can plot these results
# create a dataframe with columns of lat and long
lonlat3 <- expand.grid(lon, lat)
names(lonlat3)[1] <- "lon"
names(lonlat3)[2] <- "lat" 
# now bind the coefficients dataframe to the long/lat datafarame
regression_coefficients <- data.frame(cbind(lonlat3, coefficients.df))

# now get rid of intercept column
regression_coefficients[c("Intercept")] <- list(NULL)

## if we want to get the original land areas back as NA
# regression_coefficients[regression_coefficients == 0] <- NA

# ------------------------------------------------------------------------------------

# NOW WE CAN PLOT THE RESULTS

# first convert the slopes into mm per year. They are in mm per day at the moment. Use the mean no of annual days every 4 yrs = 365.25.
# If you want to replace the original values, use
# regression_coefficients$Slope <- regression_coefficients$Slope * 365.25

# we want to save the new values in a new column
regression_coefficients$Slope_mm_yr <- regression_coefficients$Slope * 365.25

# now get rid of original slope column
regression_coefficients[c("Slope")] <- list(NULL)

# ********************************************************

# PLOT THE GRID Using GGPLOT2 so that we can output a vector image for better quality and any labelling etc in Adobe Illustrator, etc if required
library(ggplot2)
#citation("ggplot2")
library(metR) # for some of the plotting functions - eg geom_contour_fill
#citation("metR")
# ************************************************************************************************************************

# THIS IS THE BEST PLOT TO DISPLAY THE SEA LEVEL RISE DATA
# Note we select the midpoint below as 3.3 since this is the Global SLR from the CMEMS altimetry. Values in the plot below this will be blue and those above red

# ********************************************************************************************************************

plotCC <- plotA + geom_contour_fill() +
  geom_contour(color = "black", size = 0.1, breaks=c(3.3,3.4,3.5,3.6,3.7,3.8,3.9,4,4.1,4.2,4.3,4.4,4.5)) +
  scale_fill_divergent (low = scales::muted("blue"), mid = "white", high = scales::muted("red"), midpoint = 3.3, na.value = "darkolivegreen", guide = "legend", limits=c(3.0,4.5)) + 
  geom_label_contour(size = 4.5, breaks=c(3.3,3.4,3.5,3.6,3.7,3.8,3.9,4,4.1,4.2,4.3,4.4,4.5), skip = 0, label.padding = grid::unit(0.25,"lines"), label.size = 0.25) + 
  scale_y_latitude(name="", limits = c(6,10.25), ticks = 1, breaks = seq(6,10.25)) +
  scale_x_longitude(name = "", limits = c(96,100.25), ticks = 1, breaks = seq(96,100.35)) +
  theme_bw() +
  theme(aspect.ratio = 4/4, axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0)) +
  theme(panel.grid = element_line(color = "black"), panel.border = element_blank(), axis.line = element_line(colour = "white")) +
  guides(fill = guide_colorbar(title = "mm y-1"))

plotCC

# add the grid square and also some labelling text
plotDD <- plotCC + geom_rect(aes(xmin = 98, ymin= 7, xmax= 98.25, ymax = 7.25), colour="black", fill="black") +
  annotate("text", x = 98.5, y = 8.6, label = "Phuket", color= "white", size=6) +
  annotate("text", x = 99.5, y = 8.2, label = "Thai/Malay Peninsula", color= "white", size=8, angle=315)

plotDD

# if want to add text to grid square then geom_text(x=98, y=6.9, label="7.125N 98.125E", size = 4)

#####################################################################################################################




















