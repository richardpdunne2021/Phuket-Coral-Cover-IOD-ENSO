
# Using DMI NETCDF file from NOAA to analyse the IOD DMI
# Creator: Richard Dunne
# Last Revision: 1 April 2021

# source of data: - https://stateoftheocean.osmc.noaa.gov/sur/ind/dmi.php

# ____________ SET DIRECTORY ____________________

# First set the directory in which we are working in the properties section of R
# change the file address below to where you have stored the data file on your computer
setwd ("E:/Our Papers/2020 IOD and Coral Cover/Resubmission/Data & R Code")
# then double check that you are in this directory before running any script
getwd()

# Load package
library(ncdf4)

# choose the NetCDF file
fn <- "dmi to 13 Jan 2021.nc"   # this data is the weekly values of DMI from 1981 to the present

# open the NetCDF file
nc <- nc_open(fn)

# look at the variables etc within the NETCDF file
print(nc)
attributes(nc$var)

# extract the time in days - which is called WEDCEN2 in the NETCDF file
tm_day <- ncvar_get(nc,"WEDCEN2")

# this is in the format days since 1900-01-01 00:00:00 so need to convert this to a meaningful date
tm = as.Date(tm_day, origin = "1900-01-01", tz = "UTC")

# check maximum and minimum dates
max(tm) # display the last date in the current time series
min(tm) # start date of time series

# now get the DMI from the NETCDF file
DMI <- ncvar_get(nc, "DMI")

# combine the time and DMI into a dataframe which can later be exported to a CSV file for use elsewhere if required
df <- data.frame(DMI,tm, tm_day)
str(df)

# add a POSIXct format date column
df$date <- as.POSIXct(paste(df$tm), format = "%Y-%m-%d", tz = "UTC") 
class(df$date) # POSIXct date

# add a decimal date column
library(lubridate)
df$decimal_date <- decimal_date(tm)

# compute a column for the moving average of DMI
library(tidyverse)
library(zoo)

# we can compute 3 differently 'positioned' moving averages if we require 
# cma = centered moving average
# tma = trailing moving average
# ama = advancing moving average
# k= integer width of rolling window
# align = specify whether index of result should be left or right aligned or centered compared to the rolling window of observations
df = df %>%
  mutate(cma = rollmean(DMI, k = 12, fill = NA, align = "center")) %>%
  mutate(ama = rollmean(DMI, k = 12, fill = NA, align = "left")) %>%
  mutate(tma = rollmean(DMI, k = 12, fill = NA, align = "right"))

# for DMI data save this dataframe to a CSV file if we require
# write.csv(df,"Weekly DMI to 13 Jan 2021.csv")

# plot the raw data for DMI against a time axis to visualise this:
plot(tm,DMI,pch=20,type="p", xlab="Year",ylab="DMI (degC)",cex.lab=1.3,cex.axis=1.3)  #type=o plots points and lines

# we can add LOWESS PLOTS FOR THE DATA to visualise any trends at different sampling intervals
lines(lowess(tm,DMI), col="red", lwd=2) # uses default f= 0.667
lines(lowess(tm,DMI, f=0.001), col="blue", lwd=2) # using less points in the smoothing

# Compute a weekly Standard deviation of DMI for placing a line at this value on the plot we will create later
std <- function(df) sd(DMI)
std()
# for adding to the plot below
stddevline <- std()

# if we needed the standard error then
#se <- function(df) sqrt(var(DMI)/length(DMI))
#se()

####################################################################
##### PLOTTING USING THE DECIMAL DATE FOR X-AXIS   ####################
###################################################################

# USING GGPLOT2 GRAPHICS
library(ggplot2)

# First plot the bare bones plot and check it as we want it:

Plot_1 <- ggplot(data=df, aes(decimal_date)) +   
    scale_x_continuous(name = "", limits = c(1987,2022), breaks = seq(1987,2022,1), labels=c("1987","","1989","","1991","","1993","","1995","","1997","","1999","","2001","","2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017","","2019","","2021",""))   +
  scale_y_continuous(limits=c(-1.5,3.5))+
  labs(y=expression(DMI~" "^{o}~C)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 0, hjust= 0, vjust = 0)) +  # adjust the position of the X axis tick labels
  theme(plot.margin=unit(c(4,2,4,0), "cm")) +                           # plot margins are in the order top, right, bottom, left
  theme(panel.grid.minor = element_blank()) +                           # remove the minor grid lines vertically - not working
  theme(legend.position = "none")
  
Plot_1

# add rectangles to mark combined ENSO/IOD or just IOD
# these rectangles have start and end dates for when the DMI was above the standard deviation value computed above for several weeks. The decimal date information entered below was extracted from the weekly DMI file ("DMI weekly to 13 Jan 2021.xlsx") in the folder E:\Phuket Databases\IOD-NNI-SOI\DMI\State of the Ocean Climate. For Nino 3.4 the file is "El Nino 3.4 weekly 1981 to 15 Jul 2020.xlsx" and the level used was >0.5 deg C.
# fill=colour of rectangle, size=border width, colour=border col
# Combined IOD/ENSO is grey
# IOD only is lemonchiffon
# ENSO only is lightcyan
Plot_1a <- Plot_1 +
  geom_rect(data=df, aes(NULL,NULL,xmin=1994.27945205479,xmax=1994.87397260274,ymin=-1.5,ymax=3.5),colour="white",alpha=0.2,fill="gray70",size=0.5) +
  geom_rect(data=df, aes(NULL,NULL,xmin=1997.47945205479,xmax=1998.05479452055,ymin=-1.5,ymax=3.5),colour="white",alpha=0.2,fill="gray70",size=0.5) +
  geom_rect(data=df, aes(NULL,NULL,xmin=2006.62191780822,xmax=2006.94794520548,ymin=-1.5,ymax=3.5),colour="white",alpha=0.2,fill="gray70",size=0.5) +
 
  # DISCOUNT THIS SHORT IOD (12 WEEKS) geom_rect(data=df, aes(NULL,NULL,xmin=2012.56284153005,xmax=2012.77322404372,ymin=-1.5,ymax=3.5),colour="white",alpha=0.2,fill="lemonchiffon",size=0.5) +
  
  # DISCOUNT THIS SHORT IOD (14 WEEKS) geom_rect(data=df, aes(NULL,NULL,xmin=2015.59178082192,xmax=2015.84109589041,ymin=-1.5,ymax=3.5),colour="white",alpha=0.2,fill="lemonchiffon",size=0.5) +
  
  #DISCOUNT THIS SHORT IOD (18 WEEKS) geom_rect(data=df, aes(NULL,NULL,xmin=2017.27671232877,xmax=2017.60273972603,ymin=-1.5,ymax=3.5),colour="white",alpha=0.2,fill="lemonchiffon",size=0.5) +
  
  #DISCOUNT THIS SHORT IOD (17 WEEKS) geom_rect(data=df, aes(NULL,NULL,xmin=2018.6,xmax=2018.90684931507,ymin=-1.5,ymax=3.5),colour="white",alpha=0.2,fill="lemonchiffon",size=0.5) +

  geom_rect(data=df, aes(NULL,NULL,xmin=2019.2904109589,xmax=2020,ymin=-1.5,ymax=3.5),colour="white",alpha=0.2,fill="gray70",size=0.5)

Plot_1a

# add a line at Zero
Plot_1b <-Plot_1a + geom_abline(aes(intercept=0, slope=0), color="black", size=1)
Plot_1b

# add points for each weekly reading
Plot_2 <- Plot_1b + geom_point(aes(y=DMI), size=0.5)
Plot_2

# add moving average 12 week
Plot_3 <- Plot_2 + geom_line(aes(y=df$cma), size=1, colour="red")
Plot_3

# add the standard deviation lines for the data
Plot_4 <- Plot_3 + 
  geom_hline(yintercept=stddevline, linetype="dashed", size=1) + 
  geom_hline(yintercept=-stddevline, linetype="dashed", size=1) + 
  annotate("text", x= 2022, y=0.7, label="std dev") + 
  annotate("text", x= 2022, y=-0.65, label="std dev")
  
Plot_4


##########################################################################################################
##########################################################################################################

# ******************************************************************************************************************************************
# CREATE A MONTHLY TIME SERIES from the weekly data so that we can use this in our later GAMM modelling of sea level
# ******************************************************************************************************************************************
# NEED TO RECONFIGURE THE DATA INTO MONTHLY MEANS
# first create new columns containing Year and Month
df$year <- as.numeric(format(df$tm, "%Y")) # extracts Year from the date variable and adds in new column in the dataframe
df$month <- as.numeric(format(df$tm, "%m")) # ditto for month
# if we want all the original columns as well as the grouping columns 'month & year' then:
month_DMI <- aggregate(df, by=list(df$month, df$year), FUN=mean) # note that this averages each month time column (tm_days or date) according to the month length and centers the value at the mid month point.
##  if we wanted to restrict the function to selected columns only then:
##  month2_DMI <- aggregate(df[,1:4], by=list(df$month, df$year), FUN=mean) # columns 1 to 4 only

write.csv(month_DMI,"Monthly DMI to 13 Jan 2021.csv")

######################################################################################
# APPENDIX A - A CLOSER LOOK AT TRENDS - not used in the paper

# here we are looking at whether the DMI has any trend by month or over time

#*************************************************************************************
# Generalized Additive Models (GAMs) offer a flexible approach to calculating trends and in particular, the mgcv package contains many functions that are very useful for such modelling. Some of the details of this type of model are presented in WOOD (2006) and the mgcv package itself.
library(mgcv)

# makes new dataframe with variables from the original dataframe and adds a 'trend' column - the new trend column is a new time where each of the weekly values has the same time value eg Feb 1993 will be 1993.083 and will have up to 4 weekly data points for the same 'time'. The new dataframe retains all the weekly values though for plotting.
monthly <- transform(df, trend = year + (month - 1) / 12) 

# we could first run an 'lm' in GAM - a purely linear model
lm_mod_GAM <- gam(DMI ~ trend, data = monthly)
summary(lm_mod_GAM) # output from a GAM 
gam.check(lm_mod_GAM) # diagnostics of fitting and results - 4 residual plots and information about the convergence of the smoothness selection optimisation

# the parts of the output that are different from a normal 'lm' function are:
# R-sq.(adj) =     Deviance explained = 
# GCV =   Scale est. =   n = 

# Deviance serves as a generalisation of R squared, R sq adjusted is the R sq adjusted for small sample size and model complexity. 
# GCV = generalized cross validation - equivalent to an estimate of the 'mean square prediction error' based on a leave-one out cross validation estimation process. We can use it similarly to AIC as a comparative measure to choose among different models, with the lower being better.

# now run a smoothed model
M0 <- gam(DMI ~s(trend), data= monthly)
summary(M0)
# this model explains XX% of the variation as shown by the adj R squared value. No account has been taken of any seasonal variation.

coef(M0) # extracts the coefficients of each of the basis functions of the model - here we can see we have 9 coefficients

plot.gam(M0, select=1, shade=TRUE,residuals=TRUE, pch=1) # plot of the trend component with residuals

# plot the autocorrelation function (ACF) of the residuals
acf(residuals(M0))
# the residuals show no real evidence of a seasonal (repeating pattern) but clear evidence of autocorrelation which is not surprising

# COMPARE TO ORIGINAL WEEKLY DATA - gives same output as M0 above - we have to use time as 'tm_day'
M_weekly <- gam(DMI ~s(tm_day), data= df)
summary(M_weekly)
coef(M_weekly)
plot.gam(M_weekly, select=1, shade=TRUE, residuals=TRUE, pch=1) # plot of the trend component with the residuals
M_weekly$sp # extract the smoothing parameter of the model 

# ..................................................................

# refine the model to add a possible periodic (seasonal) variation in DMI.
# bs="cc" chooses a cyclic spline for the monthly component which joins the first and last points ie Jan and Dec
M1 <- gam(DMI ~ s(trend) + s(month, bs = "cc"), data = monthly)
summary(M1)
# this model explains XX% of the variation
plot.gam(M1, select = 2, shade = TRUE, residuals = TRUE, pch=1) # plot of how DMI varies by month. There is very little seasonal variation
plot.gam(M1, select=1, shade=TRUE, residuals=TRUE, pch=1) # plot of the trend component with the residuals
acf(residuals(M1)) # same as for the M0 model - autocorrelation present

# is it a better model than the smoothed model without seasonality (M0)?  
anova.gam(M0,M1, test="Chisq") # if p<0.001 then yes

# ...................................................................

# This is where modelling using a GAMM (Generalized Additive Mixed Model) comes in because it is possible to model the short-term autocorrelation using a linear mixed model. The gamm function uses the package nlme and the Generalized Linear Mixed Model (GLMM) fitting routine.

# In the M2 model below the correlation structure is added, and assumes that the trend and seasonal terms vary independently of one another
M2 <- gamm(DMI ~ s(trend) + s(month, bs = "cc"), data = monthly, correlation = corAR1(form = ~ month | year)) # need to do this using the monthly averaged dataframe - doesn't compute using weekly GIVES:  Error in Initialize.corAR1(X[[i]], ...) : covariate must have unique values within groups for "corAR1" objects 

# USE THE MONTHLY AVERAGED DATA FRAME
# FIRSTLY USE THIS DATAFRAME WITH A SEASONAL TERM BUT NO CORRELATION STRUCTURE TERM
M1_gamm <- gamm(DMI ~ s(year) + s(month, bs = "cc"), data = month_DMI)
summary(M1_gamm$gam)
plot(M1_gamm$gam, select = 2, shade = TRUE, residuals = TRUE, pch=1) # plot of the seasonal component
plot(M1_gamm$gam, select = 1, shade = TRUE, residuals = TRUE, pch=1) # plot of the trend component
acf(residuals(M1_gamm$lme, type = "normalized")) # autocorrelation present

# now add the correlation term
M2_gamm <- gamm(DMI ~ s(year) + s(month, bs = "cc"), data = month_DMI, correlation = corAR1(form = ~ month | year))
summary(M2_gamm$gam)

plot(M2_gamm$gam, select = 2, shade = TRUE, residuals = TRUE, pch=1) # plot of the seasonal component
plot(M2_gamm$gam, select = 1, shade = TRUE, residuals = TRUE, pch=1) # plot of the trend component

acf(residuals(M2_gamm$lme, type = "normalized"))

# ALTHOUGH THE DATA ARE NO LONGER AUTOCORRELATED THE TREND COMPONENT IS NO LONGER SIGNIFICANT AND THE SEASONAL COMPONENT IS SIGNIFICANT (P=0.002)

# it also indicates that the trend 'year' term should be linear (edf=1) so revise the model to change the smoothing term for year to a linear term:
M3_gamm <- gamm(DMI ~ year + s(month, bs = "cc"), data = month_DMI, correlation = corAR1(form = ~ month | year))
summary(M3_gamm$gam)

plot(M3_gamm$gam, select = 1, shade = TRUE, residuals = TRUE, pch=1) # plot of the seasonal component
acf(residuals(M3_gamm$lme, type = "normalized"))


# ...................................................................

# this M4 model assumes that the seasonal amplitude and or phase change over time - ie that there is an interaction between them.
# it uses a tensor product smooth (te) [rather than isotropic smooth (s)] and the reason for doing this is that the trend and seasonal components are essentially on different scales and we want to apply the same level of smoothness to both

M4_gamm <- gamm(DMI ~ s(month, bs = "cc") + te(year, month), data = month_DMI, correlation = corAR1(form = ~ month | year))
summary(M4_gamm$gam)

plot(M4_gamm$gam, select = 2, shade = TRUE, residuals = TRUE, pch=1) # plot of the trend component
plot(M4_gamm$gam, select = 1, shade = TRUE, residuals = TRUE, pch=1) # plot of the seasonal component

# It becomes a bit more difficult to plot the two-way interaction between the trend and the month, but it is possible with a surface as shown in this plot. This plot shows for example that during summertime the trends component varies less whilst later in the year the trend of SLA is greater.
plot(M4_gamm$gam, select = 2, pers = TRUE, theta = 225, phi = 10, ticktype = "detailed")

# different values of theta rotate the plot
plot(M4_gamm$gam, select = 2, pers = TRUE, theta = 180, phi = 10, ticktype = "detailed")
plot(M4_gamm$gam, select = 2, pers = TRUE, theta = 90, phi = 10, ticktype = "detailed")
plot(M4_gamm$gam, select = 2, pers = TRUE, theta = -90, phi = 10, ticktype = "detailed")

# phi rotates the diagram in the other axis
plot(M4_gamm$gam, select = 2, pers = TRUE, theta = 90, phi = 15, ticktype = "detailed")

acf(residuals(M4_gamm$lme, type = "normalized"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

