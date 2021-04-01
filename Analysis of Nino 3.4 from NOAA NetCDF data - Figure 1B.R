# Created by: Richard Dunne
# Last updated: 1 April 2021

# Using Nino 3.4 NETCDF file from NOAA to analyse the Nino 3.4 Index

# https://stateoftheocean.osmc.noaa.gov/sur/pac/nino34.php

# ____________ SET DIRECTORY ____________________

# First set the directory in which we are working in the properties section of R
# change the file address below to where you have stored the data file on your computer
setwd ("E:/Our Papers/2020 IOD and Coral Cover/Resubmission/Data & R Code")
# then double check that you are in this directory before running any script
getwd()

# Load package
library(ncdf4)

fn <- "Nino 3.4 to 13 Jan 2021.nc"   # El Nino 3.4 index from State of the Ocean website

nc <- nc_open(fn)

# look at the variables etc within the NETCDF file
print(nc)
attributes(nc$var)

# extract the time in days - which is called WEDCEN2 in the NETCDF file
tm_day <- ncvar_get(nc,"WEDCEN2")

# display this data
# tm_day
# this is in the format days since 1900-01-01 00:00:00 so need to convert this to a meaningful date
tm = as.Date(tm_day, origin = "1900-01-01", tz = "UTC")
# tm  # display this
max(tm) # display the last date in the current time series
min(tm) # start date of time series

# get the Nino 3.4 from the NETCDF file - call it NINO3.4 in this script file so we don't have to change all the code below - WEEKLY data
NINO3.4 <- ncvar_get(nc, "NINO34")

# combine the time and NINO3.4 into a dataframe which can later be exported to a CSV file for use elsewhere if required
df <- data.frame(NINO3.4,tm, tm_day)
str(df)

# add a POSIXct format date column
df$date <- as.POSIXct(paste(df$tm), format = "%Y-%m-%d", tz = "UTC") 
class(df$date) # POSIXct date

# add a decimal date column
library(lubridate)
df$decimal_date <- decimal_date(tm)

# compute a column for the moving average of NINO3.4
library(tidyverse)
library(zoo)

# we will compute 3 differently 'positioned' moving averages 
# cma = centered moving average
# tma = trailing moving average
# ama = advancing moving average
# k= integer width of rolling window
# align = specify whether index of result should be left or right aligned or centered compared to the rolling window of observations
df = df %>%
  mutate(cma = rollmean(NINO3.4, k = 12, fill = NA, align = "center")) %>%
  mutate(ama = rollmean(NINO3.4, k = 12, fill = NA, align = "left")) %>%
  mutate(tma = rollmean(NINO3.4, k = 12, fill = NA, align = "right"))

# for NINO3.4 data save this dataframe to a CSV file if we want
#write.csv(df,"Weekly NINO3.4 to 13 Jan 2021.csv")

# plot the raw data for NINO3.4 against a time axis
plot(tm,NINO3.4,pch=20,type="p", xlab="Year",ylab="NINO3.4 (degC)",cex.lab=1.3,cex.axis=1.3)  #type=o plots points and lines

# we can add LOWESS PLOTS FOR THE DATA to see any trends at different sampling intervals
lines(lowess(tm,NINO3.4), col="red", lwd=2) # uses default f= 0.667
lines(lowess(tm,NINO3.4, f=0.001), col="blue", lwd=2) # using less points in the smoothing

# Compute a weekly Standard deviation of NINO3.4 for placing a line at this value on the plot - not used on the final plot
std <- function(df) sd(NINO3.4)
std()
# for adding to the plot below
stddevline <- std()

# if we needed the standard error then
#se <- function(df) sqrt(var(NINO3.4)/length(NINO3.4))
#se()

####################################################################
##### PLOTTING USING THE DECIMAL DATE FOR X-AXIS   ####################
###################################################################

# USING GGPLOT2 GRAPHICS
library(ggplot2)

# First plot the bare bones plot and check it as we want it:

Plot_1 <- ggplot(data=df, aes(decimal_date)) +   
  scale_x_continuous(name = "", limits = c(1987,2022), breaks = seq(1987,2022,1), labels=c("1987","","1989","","1991","","1993","","1995","","1997","","1999","","2001","","2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017","","2019","","2021",""))   +
  scale_y_continuous(limits=c(-2.5,3.5))+
  labs(y=expression(Nino_3.4~" "^{o}~C)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 0, hjust= 0, vjust = 0)) +  # adjust the position of the X axis tick labels
  theme(plot.margin=unit(c(4,2,4,0), "cm")) +                           # plot margins are in the order top, right, bottom, left
  theme(panel.grid.minor = element_blank()) +                           # remove the minor grid lines vertically 
  theme(legend.position = "none")

Plot_1

# add rectangles to mark combined ENSO/IOD or just IOD
# these rectangles have start and end dates for when the NINO3.4 was above the standard deviation value computed above for several weeks. The decimal date information entered below was extracted from the weekly NINO3.4 file ("NINO3.4 weekly to 13 Jan 2021.xlsx") in the folder E:\Phuket Databases\IOD-NNI-SOI\NINO3.4\State of the Ocean Climate. For Nino 3.4 the file is "El Nino 3.4 weekly 1981 to 15 Jul 2020.xlsx" and the level used was >0.5 deg C.
# fill=colour of rectangle, size=border width, colour=border col
# Combined IOD/ENSO is grey
# IOD only is lemonchiffon
# ENSO only is lightcyan

Plot_1a <- Plot_1 +
  geom_rect(data=df, aes(NULL,NULL,xmin=1987,xmax=1988.07103825137,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5) + 
  # Note the above period actually started on 1986.7095890411 but if this date is used it is omitted because the scale starts at 1987
  geom_rect(data=df, aes(NULL,NULL,xmin=1991.38630136986,xmax=1992.45901639344,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5) +
  # discount this period only 13 weeks - geom_rect(data=df, aes(NULL,NULL,xmin=1993.22465753425,xmax=1993.45479452055,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5) +
  geom_rect(data=df, aes(NULL,NULL,xmin=1994.7397260274,xmax=1995.14246575342,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5) +
  geom_rect(data=df, aes(NULL,NULL,xmin=1997.32602739726,xmax=1998.32328767123,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5) +
  geom_rect(data=df, aes(NULL,NULL,xmin=2002.42465753425,xmax=2003.15342465753,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5) +
  geom_rect(data=df, aes(NULL,NULL,xmin=2004.59016393443,xmax=2004.99180327869,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5) +
  geom_rect(data=df, aes(NULL,NULL,xmin=2006.69863013699,xmax=2007.02465753425,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5) +
  geom_rect(data=df, aes(NULL,NULL,xmin=2009.47671232877,xmax=2010.26301369863,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5) +
  geom_rect(data=df, aes(NULL,NULL,xmin=2015.15068493151,xmax=2016.33879781421,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5) +
  geom_rect(data=df, aes(NULL,NULL,xmin=2018.73424657534,xmax=2019.48219178082,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="lightcyan",size=0.5)

Plot_1a

# add a line at Zero
Plot_1b <-Plot_1a + geom_abline(aes(intercept=0, slope=0), color="black", size=1)
Plot_1b

# add points for each weekly reading
Plot_2 <- Plot_1b + geom_point(aes(y=NINO3.4), size=0.5)
Plot_2

# add moving average 12 week
Plot_3 <- Plot_2 + geom_line(aes(y=df$cma), size=1, colour="red")
Plot_3

# add a line to denote the accepted value for an El Nino or La Nina (0.5 deg C)
Plot_4 <- Plot_3 + 
  geom_hline(yintercept=0.5, linetype="dashed", size=1) + 
  geom_hline(yintercept=-0.5, linetype="dashed", size=1) + 
  annotate("text", x= 2022, y=0.7, label="0.5") + 
  annotate("text", x= 2022, y=-0.65, label="-0.5")

Plot_4

# if we want to add the standard deviation lines for the data
#Plot_4a <- Plot_4 + 
#  geom_hline(yintercept=stddevline, linetype="dashed", size=1) + 
#  geom_hline(yintercept=-stddevline, linetype="dashed", size=1) + 
#  annotate("text", x= 2022, y=0.7, label="std dev") + 
#  annotate("text", x= 2022, y=-0.65, label="std dev")

#Plot_4a


##########################################################################################################
##########################################################################################################

# ******************************************************************************************************************************************
# OUTPUT TO A MONTHLY TIME SERIES from the weekly data
# ******************************************************************************************************************************************
# NEED TO RECONFIGURE THE DATA INTO MONTHLY MEANS
# first create new columns containing Year and Month
df$year <- as.numeric(format(df$tm, "%Y")) # extracts Year from the date variable and adds in new column in the dataframe
df$month <- as.numeric(format(df$tm, "%m")) # ditto for month
# if we want all the original columns as well as the grouping columns 'month & year' then:
month_NINO3.4 <- aggregate(df, by=list(df$month, df$year), FUN=mean) # note that this averages each month time column (tm_days or date) according to the month length and centers the value at the mid month point.
##  if we wanted to restrict the function to selected columns only then:
##  month2_NINO3.4 <- aggregate(df[,1:4], by=list(df$month, df$year), FUN=mean) # columns 1 to 4 only

write.csv(month_NINO3.4,"Monthly NINO3.4 to 13 Jan 2021.csv")



######################################################################################
# APPENDIX A - A CLOSER LOOK AT TRENDS - not used in the paper

# here we are looking at whether the NINO3.4 has any trend by month or over time

#*************************************************************************************
# Generalized Additive Models (GAMs) offer a flexible approach to calculating trends and in particular, the mgcv package contains many functions that are very useful for such modelling. Some of the details of this type of model are presented in WOOD (2006) and the mgcv package itself.
library(mgcv)

# makes new dataframe with variables from the original dataframe and adds a 'trend' column - the new trend column is a new time where each of the weekly values has the same time value eg Feb 1993 will be 1993.083 and will have up to 4 weekly data points for the same 'time'. The new dataframe retains all the weekly values though for plotting.
monthly <- transform(df, trend = year + (month - 1) / 12) 

# we could first run an 'lm' in GAM - a purely linear model
lm_mod_GAM <- gam(NINO3.4 ~ trend, data = monthly)
summary(lm_mod_GAM) # output from a GAM 
gam.check(lm_mod_GAM) # diagnostics of fitting and results - 4 residual plots and information about the convergence of the smoothness selection optimisation

# the parts of the output that are different from a normal 'lm' function are:
# R-sq.(adj) =     Deviance explained = 
# GCV =   Scale est. =   n = 

# Deviance serves as a generalisation of R squared, R sq adjusted is the R sq adjusted for small sample size and model complexity. 
# GCV = generalized cross validation - equivalent to an estimate of the 'mean square prediction error' based on a leave-one out cross validation estimation process. We can use it similarly to AIC as a comparative measure to choose among different models, with the lower being better.

# now run a smoothed model
M0 <- gam(NINO3.4 ~s(trend), data= monthly)
summary(M0)
# this model explains XX% of the variation as shown by the adj R squared value. No account has been taken of any seasonal variation.

coef(M0) # extracts the coefficients of each of the basis functions of the model - here we can see we have 9 coefficients

plot.gam(M0, select=1, shade=TRUE,residuals=TRUE, pch=1) # plot of the trend component with residuals

# plot the autocorrelation function (ACF) of the residuals
acf(residuals(M0))
# the residuals show no real evidence of a seasonal (repeating pattern) but clear evidence of autocorrelation which is not surprising

# COMPARE TO ORIGINAL WEEKLY DATA - gives same output as M0 above - we have to use time as 'tm_day'
M_weekly <- gam(NINO3.4 ~s(tm_day), data= df)
summary(M_weekly)
coef(M_weekly)
plot.gam(M_weekly, select=1, shade=TRUE, residuals=TRUE, pch=1) # plot of the trend component with the residuals
M_weekly$sp # extract the smoothing parameter of the model 

# ..................................................................

# refine the model to add a possible periodic (seasonal) variation in NINO3.4.
# bs="cc" chooses a cyclic spline for the monthly component which joins the first and last points ie Jan and Dec
M1 <- gam(NINO3.4 ~ s(trend) + s(month, bs = "cc"), data = monthly)
summary(M1)
# this model explains XX% of the variation
plot.gam(M1, select = 2, shade = TRUE, residuals = TRUE, pch=1) # plot of how NINO3.4 varies by month. There is very little seasonal variation
plot.gam(M1, select=1, shade=TRUE, residuals=TRUE, pch=1) # plot of the trend component with the residuals
acf(residuals(M1)) # same as for the M0 model - autocorrelation present

# is it a better model than the smoothed model without seasonality (M0)?  
anova.gam(M0,M1, test="Chisq") # if p<0.001 then yes

# ...................................................................

# This is where modelling using a GAMM (Generalized Additive Mixed Model) comes in because it is possible to model the short-term autocorrelation using a linear mixed model. The gamm function uses the package nlme and the Generalized Linear Mixed Model (GLMM) fitting routine.

# In the M2 model below the correlation structure is added, and assumes that the trend and seasonal terms vary independently of one another
M2 <- gamm(NINO3.4 ~ s(trend) + s(month, bs = "cc"), data = monthly, correlation = corAR1(form = ~ month | year)) # need to do this using the monthly averaged dataframe - doesn't compute using weekly GIVES:  Error in Initialize.corAR1(X[[i]], ...) : covariate must have unique values within groups for "corAR1" objects 

# USE THE MONTHLY AVERAGED DATA FRAME
# FIRSTLY USE THIS DATAFRAME WITH A SEASONAL TERM BUT NO CORRELATION STRUCTURE TERM
M1_gamm <- gamm(NINO3.4 ~ s(year) + s(month, bs = "cc"), data = month_NINO3.4)
summary(M1_gamm$gam)
plot(M1_gamm$gam, select = 2, shade = TRUE, residuals = TRUE, pch=1) # plot of the seasonal component
plot(M1_gamm$gam, select = 1, shade = TRUE, residuals = TRUE, pch=1) # plot of the trend component
acf(residuals(M1_gamm$lme, type = "normalized")) # autocorrelation present

# now add the correlation term
M2_gamm <- gamm(NINO3.4 ~ s(year) + s(month, bs = "cc"), data = month_NINO3.4, correlation = corAR1(form = ~ month | year))
summary(M2_gamm$gam)

plot(M2_gamm$gam, select = 2, shade = TRUE, residuals = TRUE, pch=1) # plot of the seasonal component
plot(M2_gamm$gam, select = 1, shade = TRUE, residuals = TRUE, pch=1) # plot of the trend component

acf(residuals(M2_gamm$lme, type = "normalized"))

# ALTHOUGH THE DATA ARE NO LONGER AUTOCORRELATED THE TREND COMPONENT IS NO LONGER SIGNIFICANT AND THE SEASONAL COMPONENT IS SIGNIFICANT (P=0.002)

# it also indicates that the trend 'year' term should be linear (edf=1) so revise the model to remove the smoothing for year:
M3_gamm <- gamm(NINO3.4 ~ year + s(month, bs = "cc"), data = month_NINO3.4, correlation = corAR1(form = ~ month | year))
summary(M3_gamm$gam)

plot(M3_gamm$gam, select = 1, shade = TRUE, residuals = TRUE, pch=1) # plot of the seasonal component
acf(residuals(M3_gamm$lme, type = "normalized"))


# ...................................................................

# this M4 model assumes that the seasonal amplitude and or phase change over time - ie that there is an interaction between them.
# it uses a tensor product smooth (te) [rather than isotropic smooth (s)] and the reason for doing this is that the trend and seasonal components are essentially on different scales and we want to apply the same level of smoothness to both

M4_gamm <- gamm(NINO3.4 ~ s(month, bs = "cc") + te(year, month), data = month_NINO3.4, correlation = corAR1(form = ~ month | year))
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







