# ANALYSIS OF PHOTOTRANSECT CORAL COVER
# Created by: Richard Dunne
 # Last updated: 1 April 2021

# NOTE ON P VALUES IN GAM
# Wood (2006) mentions that based on limited simulation experience, p-values close to 0.05 can be around half of their correct value when the null hypothesis is true. This means that smoothers with p-values smaller than 0.001 can be trusted, and we can also trust the p-value if it is 0.2, 0.5, or 0.8. It is the smoother with a p-value of 0.02, 0.03, 0.04, or 0.05 that gives trouble. 

# The same holds for F-tests comparing nested GAMs. If this is a serious issue for your data, then you could do bootstrapping to get better p-values. Chapter 8 in Keele (2008) gives a nice introduction, and a detailed algorithm is presented in Davison and Hinkley (1997).


# ____________ SET DIRECTORY ____________________

# First set the directory in which we are working in the properties section of R
# change the file address below to where you have stored the data file on your computer
setwd ("E:/Our Papers/2020 IOD and Coral Cover/Resubmission/Data & R Code")
# then double check that you are in this directory before running any script
getwd()

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  PRELIMINARY   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# load all the packages we will use:
library(ggplot2) # basic GGPLOT2
library(scales) # helps to label and scale axes
library(plyr)
library(ggstatsplot)
library(lattice) # for the function dotplot
library(ggpubr)
library(MuMIn) # for uGAMM and other functions
library(gratia)
library(mgcv)

# load all the functions that we will need:
source("HighstatLibV10.R")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@  READ IN THE COMBINED CORAL COVER, SLA AND DHW DATA  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# use the method of importing from a CSV file
Coral_cover <- read.csv("Coral Cover and Covariates.csv")

# This file is a data.frame of 232 observations (rows) of 53 variables (columns)
#names (Coral_cover) # displays the column headers
#head(Coral_cover)
str(Coral_cover) # display the type of each column

# convert the date column to a POSIXct date column
Coral_cover$date <- as.POSIXct(paste(Coral_cover$Date), format = "%d/%m/%Y", tz = "UTC") 
class(Coral_cover$date) # POSIXct date

# compute columns for year and month for later models
Coral_cover$year <- as.numeric(format(Coral_cover$date, "%Y")) # extracts Year from the date variable and adds in new column in the dataframe
Coral_cover$month <- as.numeric(format(Coral_cover$date, "%m")) # ditto for month

# makes new dataframe with variables and adds a trend column
monthly <- transform(Coral_cover, trend = year + (month - 1) / 12) # makes new dataframe with variables from the Coral_cover dataframe and adds a trend column

# also add a month column (fmonth) that is a factor with 12 levels for later use in GAMM models. This is an ordered factor in the order 1 to 12 (see ?factor for more info on ordered and unordered factors)
monthly$fmonth <- factor(monthly$month, ordered = TRUE)

# also make a column for transect as a factor if we are using the data listed separately by each Transect
monthly$fTransect <- factor(monthly$Transect, ordered = FALSE)

# if there is a need to centre the DHW data:
# Centered <- function(x) {(x-mean(x, na.rm = TRUE))}
# apply this to the DHW column and store this as a new column
# monthly$DHW_centered <- Centered(monthly$DHW_3month_avg)

# -------------------------------------------------------------------------------------
# We could also have standardized DHW:
# Mystd <- function(x) {(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}
# monthly$DHW_stand <- Mystd(monthly$DHW_3month_avg)

str(monthly)

# ----------------------------------------------------------------------------------------------------------------
# Remove any spacing columns which just contain NAs
# monthly <- subset(monthly, select=-c(6,22,38))

# *****************************************************************************************************************
# ********************  ARE THERE ANY MISSING DATA? - which might cause problems  *********************************
# *****************************************************************************************************************

colSums(is.na(monthly))
# 3 NAs in Transect_14 and 2 in Transect_15. These are in the rows entered below (check the monthly dataframe first before running)
# delete these rows in Transect separate dataframe
monthly <- monthly[-c(118,119,140,176,177),]

# or rows containing NAs can be removed with this command:
# monthly <- na.exclude(monthly)

# if we are using means we need to change the column name to "Cover" for the code below
# names(monthly)[names(monthly) == "Mean"] <- "Cover"  

# dataframe using coral cover only in measured months is now 227 rows by XX columns. Mean data is 57 rows

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@   1.2 SCATTERPLOT OF DATA TO SEE WHAT IT LOOKS LIKE  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# USING GGPLOT2 GRAPHICS
# because all our columns are the same length we can plot different combinations with GGPLOT2:

# quick plot of the mean coral cover by date:
#g1 <- ggplot(data=monthly, aes(x= Decimal_date)) + geom_line(aes(y = Mean), color = "darkred") 

g1 <- ggplot(data=monthly, aes(x= Decimal_date)) + geom_line(aes(y = Cover), color = "darkred") 
# + scale_x_continuous(name = "Year", limits = c(1985, 2020), breaks = c(1985,1990,1995,2000,2005,2010,2015,2020), minor_breaks = seq(1985,2020,1))
# + scale_y_continuous(name="Coral cover (%)")
g1 # display this graph

# OPTIONS
# could change the line type for any one of the lines by adding e.g. - , linetype="twodash") 
# could change minor grid to blue lines - both x and y
# + theme(panel.grid.minor = element_line(colour="blue", size=0.5))

# @@@@@@@@@@   Histograms & Density Plots of the Cover or Mean data  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# all Cover data combined
density <- ggplot(monthly, aes(x=Cover)) + geom_density()
density
# Add mean line
density + geom_vline(aes(xintercept=mean(Cover)), color="blue", linetype="dashed", size=1)

# by Transect
# first calculate mean of each Transect
mu <- ddply(monthly, "Transect", summarise, grp.mean=mean(Cover))
density1 <-ggplot(monthly, aes(x=Cover, color=Transect)) + geom_density() + geom_vline(data=mu, aes(xintercept=grp.mean, color=Transect), linetype="dashed", size=1)
density1

# Histogram with density plot
density3 <- ggplot(monthly, aes(x=Cover)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 2) + geom_density(alpha=.2, fill="#FF6666") + facet_wrap(Transect~ ., ncol=2) + ggtitle("Distribution of Coral cover values by Transect")  
density3
# add mean lines
density3 + geom_vline(data=mu, aes(xintercept=grp.mean, color="red"), linetype="dashed", size=1)

# Colour by groups
ggplot(monthly, aes(x=Cover, color=Transect, fill=Transect)) + geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + geom_density(alpha=.2)

# facet plots
density4 <- ggplot(monthly, aes(x=Cover)) + geom_density() + facet_grid(Transect ~ .)
density4
# Add mean lines
density4 + geom_vline(data=mu, aes(xintercept=grp.mean, color="red"), linetype="dashed", size=1)

# although we see from these plots that the distributions are not especially 'normal', when using a model in GAM or GAMM it is not the original response we are concerned about but the residuals. It is these that we examine for normality and choose different methods if they are non-normal or heteroscedastic.

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# look at spread of data for each transect. If this appears to be very different then we might have to consider a weighting for each transect in order to account for heterogeneity. If there were differences then we might choose an option in the model "weights=varIdent(form=~1|fTransect)". # the weights option implements a variance structure to deal with heterogeneity where each Transect group is allowed to have a different variance.
# see Zuur et al (2009) Mixed effects models and extensions in ecology with R. Springer - Chapt 4 for more detail of different methods of dealing with heterogeneity.

xyplot(Cover ~ Decimal_date | fTransect,col=1, data = monthly)

# there doesn't appear to be any large differences in the spread between transects so assume for now we don't need this option.


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@  DATA EXPLORATION @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Any analysis should be preceded by data exploration. Key factors are (1) outliers, (2) collinearity (correlation among covariates), (3) relationships between response and covariates, zero inflation, and sampling effort over time and space. 

# These aspects can be visualized using simple graphs. Expect to spend at least 30% of your time on data exploration.[Zuur 2014 Guide to GAM]


# *****************************************************************************************************************
# *************************************  CHECK FOR OUTLIERS  ******************************************************
# *****************************************************************************************************************

#################################  Use Cleveland dotplots  ########################################################

# the functions from Zuur will be used below. All the routines can be found in <<source("HighstatLibV10.R")>> but we will just load the functions we need below:

# To determine whether there are observations that are considerably smaller or larger than the majority, Cleveland dotplots are made for each variable.

# initially display dotplots for all covariates that we might consider using 
MyVar <- c("Decimal_date", "X0.6.1", "DHW_360", "Cover")
Mydotplot(monthly[,MyVar])

# Each panel corresponds to a variable. The x-axes represent the values of the variables and the y-axes show the order of the data as imported from the data file.

# coral cover by transect

my_cols <- c("blue", "black", "red", "turquoise")
grps <- as.factor(monthly$fTransect)
dotchart(monthly$Cover, groups = grps, gcolor = my_cols, color=my_cols[grps] ,xlab = "Percentage Cover", ylab = "Order of observations", main = "Cleveland dotplot", pch= 19, labels=monthly$Transect)

###################################   Boxplots    ##################################################################

# visualise how coral cover varies by month

library(ggplot2)
# boxplot of data by month with outliers highlighted in blue
# The lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles)
# The upper whisker extends from the hinge to the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers are called "outlying" points and are plotted individually.

library(dplyr)
library(tibble)

# create function to detect outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

# new dataframe to record outliers and prepare the label 'year' for these
dat <- monthly %>% tibble::rownames_to_column(var="outlier") %>% group_by(fmonth) %>% mutate(is_outlier=ifelse(is_outlier(Cover), Cover, as.numeric(NA)))
# new column for label
dat <- dat %>% mutate(outlier1 = year)

# for all years when not outlier substitute NA so label will not plot below
dat$outlier1[which(is.na(dat$is_outlier))] <- as.numeric(NA)

# boxplot with outliers labelled by year when they occur
ggplot(dat, aes(y=Cover, x=factor(fmonth))) + geom_boxplot(outlier.colour = "blue") + geom_text(aes(label=outlier1),na.rm=TRUE,nudge_y=-3)

# -----------------------------------------------------------------
# can do the same for Transects
# new dataframe to record outliers and prepare the label 'year' for these
datTr <- monthly %>% tibble::rownames_to_column(var="outlier") %>% group_by(fTransect) %>% mutate(is_outlier=ifelse(is_outlier(Cover), Cover, as.numeric(NA)))
# new column for label
datTr <- datTr %>% mutate(outlier1 = year)

# for all years when not outlier substitute NA so label will not plot below
datTr$outlier1[which(is.na(datTr$is_outlier))] <- as.numeric(NA)

# boxplot with outliers labelled by year when they occur
ggplot(datTr, aes(y=Cover, x=factor(fTransect))) + geom_boxplot(outlier.colour = "blue") + geom_text(aes(label=outlier1),na.rm=TRUE,nudge_y=-3)


####################################################################################################################

# ALTERNATIVE METHOD also gives stats and checks for differences between groups

# since the confidence intervals for the effect sizes are computed using bootstrapping, important to set a seed for reproducibility
library(ggstatsplot)
# citation("ggstatsplot")

set.seed(123)

ggbetweenstats(data = monthly, x = fmonth, y = Cover, outlier.tagging = TRUE, outlier.label = year, outlier.color = "blue", 
               package = "yarrr", # package from which color palette is to be taken
               palette = "info2", # choosing color palette
               xlab ="Month", ylab = "Mean Coral Cover %", title = "Coral Cover 1987 to 2020")
#, mean.ci=TRUE)  # this would add 95% CIs for the mean but makes plot a bit cluttered 

# by Transect
set.seed(123)
ggbetweenstats(data = monthly, x = fTransect, y = Cover, outlier.tagging = TRUE, outlier.label = year, outlier.color = "blue", 
               package = "yarrr", # package from which color palette is to be taken
               palette = "info2", # choosing color palette
               xlab ="Transect", ylab = "Mean Coral Cover %", title = "Coral Cover 1987 to 2020")
#, mean.ci=TRUE)  # this would add 95% CIs for the mean but makes plot a bit cluttered 

# ggstatsplot also reports the statistical test comparing the groups at the top of the plot
# it identifies and labels the group means by red dots
# other useful info is displayed e.g., n
# outlier.coef - Coefficient for outlier detection using Tukey’s method. With Tukey’s method, outliers are below (1st Quartile) or above (3rd Quartile) outlier.coef times the Inter-Quartile Range (IQR) (Default is: 1.5).

# OUTLIERS ARE LABELLED WITH THE YEAR IN WHICH THEY OCCURRED
# more info see: https://indrajeetpatil.github.io/ggstatsplot/


# ********************************************************************************************************************
# *****************************************  COLLINEARITY ************************************************************
# ********************************************************************************************************************



# Collinearity is a condition in which some of the independent variables are highly correlated.

# See e.g. : https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/

# Multicollinearity causes the following two basic types of problems:

#  (1)  The coefficient estimates can swing wildly based on which other independent variables are in the model. The coefficients become very sensitive to small changes in the model.
#     In this situation, the coefficient estimates of the multiple regression may change erratically in response to small changes in the model or the data. It does not reduce the predictive power or reliability of the model as a whole, at least within the sample data set; it only affects calculations regarding individual predictors. That is, a multivariate regression model with collinear predictors can indicate how well the entire bundle of predictors predicts the outcome variable, but it may not give valid results about any individual predictor, or about which predictors are redundant with respect to others.

#   (2)  Multicollinearity reduces the precision of the estimate coefficients, which weakens the statistical power of your regression model. You might not be able to trust the p-values to identify independent variables that are statistically significant. It increases the standard errors of estimated regression parameters and therefore inflates p-values compared to situations in which there is no collinearity (Montgomery and Peck 1992)

# ---------------------  IDENTIFYING COLLINEARITY  -------------------------------------

# ######################################################################################
# ############# (1) PAIR PLOTS WITH PEARSON CORRELATION COEFFICIENTS ###################
# ######################################################################################

# Examine the correlation coefficient for each pair of independent variables. A value of the correlation near ±1 indicates that the two variables are highly correlated. 

MyVar <- c("Decimal_date","X0.6.1","DHW_360","month")

# although we are only concerned with correlation between explanatory variables we will include the response variable (SLA) just to get an idea of how the explanatory variables each relate to it.
pairs(monthly[,MyVar],lower.panel = panel.cor)
# correlations which are >0.8 are considered critical. Where 0.5-0.7 special care is required
# here we have a correlation between Decimal_date and the Sea Level covariate

# --------------------------------------------------------------------------------------------------------------------------------
#  ----------------  If there are any possibly problematical correlations - have a quick look at these: --------------------------
# --------------------------------------------------------------------------------------------------------------------------------

Pearson_corr <- cor.test(monthly$Decimal_date, monthly$X0.6.1, method = "pearson")
Pearson_corr

library(ggpubr)
ggscatter(monthly, x = "Decimal_date", y = "X0.6.1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Date", ylab = "Sea level anomaly")

# check normality assumptions met:
shapiro.test(monthly$X0.6.1)
shapiro.test(monthly$Decimal_date)
# both are non-normal - S-W rejected

ggqqplot(monthly$X0.6.1, ylab = "Sea level Anomalies") 
ggqqplot(monthly$Decimal_date, ylab = "Date")

# because of the non-normality in Date use a Spearman rank correlation test:
Spearman_corr <- cor.test(monthly$Decimal_date, monthly$X0.6.1, method ="spearman")
Spearman_corr

ggscatter(monthly, x = "Decimal_date", y = "X0.6.1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Date", ylab = "Sea Level Anomaly")

# ###########################################################################################################
# ############################ (2) MULTIPANEL BOXPLOTS - categorical covariates  ############################
# ###########################################################################################################

#  we can use these to look for collinearity between continuous covariates and categorical covariates                    

# select variables and run:
MyVar2 <- c("Decimal_date", "X0.6.1", "DHW_360","month")
Mybwplot(monthly, MyVar2, "fTransect")

# #######################################################################################################
# ############### (3) VARIANCE INFLATION FACTORS (VIF) ##################################################
# #######################################################################################################

# The variance inflation factors are also very useful. VIF(j) is the factor by which the variance of βˆj is increased over what it would be if xj was uncorrelated with the other independent variables. If all values of VIF(j) are near 1, then collinearity is not a problem. VIF(j) > 10 indicates serious collinearity.

# A value of 1 indicates that there is no correlation between this independent variable and any others. VIFs between 1 and 5 suggest that there is a moderate correlation, but it is not severe enough to warrant corrective measures. VIFs greater than 5 represent critical levels of multicollinearity where the coefficients are poorly estimated, and the p-values are questionable.

# From Ieno & Zuur 2012 - page 99 - "various options for VIF cut-off level in the literature. Higher than 10 indicates very strong collinearity (Belsley et al 1980; Quinn & Keough 2002). Others argue that values larger than 5 or even 3 might be considered quite detrimental to regressions models (Montgomery & Peck 1992). Our preferred limit is 5 or even 3. The level also depends on the strength of the relationships. If you have weak signals in your dataset, then collinearity may result in no explanatory variable being significant."

# chooose the covariates

MyVar <- c("Decimal_date","X0.6.1", "DHW_360", "month")  # choose the variable we will compare

# run the function
corvif(monthly[,MyVar])

# at a cut-off level 3 - variables are deemed okay

# ==========================================================================================

# if some of our covariates are collinear then we need to choose which of these we are going to use in the model. Here we have a collinearity between time and sea level. Since we are more interested on how sea level influences the coral cover we may choose this and drop time.


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@  INITIAL LOOK AT THE RELATIONSHIPS BETWEEN INDIVIDUAL CONTINUOUS COVARIATES AND THE RESPONSE VARIABLE @@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# select the covariates
MyVar <- c("Decimal_date", "X0.6.1", "DHW_360","month") 

# look at all the DHW combinations to see when goes from upward to downward
#MyVar <- c("DHW", "DHW_30", "DHW_60", "DHW_90", "DHW_120", "DHW_150", "DHW_180", "DHW_210", "DHW_240", "DHW_270", "DHW_300", "DHW_330", "DHW_360")

# look at trends with all corrected datum sea levels
#MyVar <- c("RTN_final_datums_corrected","X0.1.1", "X0.2.1","X0.3.1","X0.4.1","X0.5.1","X0.6.1","X0.7.1","X0.8.1","X0.9.1","X0.10.1","X0.11.1","X0.12.1","X0.13.1","X0.14.1")

# look at trends with all uncorrected datum sea levels
# MyVar <- c("RTN_final_mm","X0.1", "X0.2","X0.3","X0.4","X0.5","X0.6","X0.7","X0.8","X0.9","X0.10","X0.11","X0.12","X0.13","X0.14")

# Use Multipanel scatterplots
Myxyplot(monthly, MyVar, "Cover")

# A LOESS smoother is added to aid visual interpretation.
# note how the actual reading for the month when the cover was recorded (RTN-final_datums_corrected) has a quite different shape of the curve to all the running means

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@   GAM and GAMM Models  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# ------------------------------------------------------------------------------------------------------------------------------------

# Although we have not done it here, it is wise to centre each covariate (subtract its mean), as this may avoid potential numerical problems due to collinearity.

# Generalized Additive Models (GAMs) offer a flexible approach to calculating trends and in particular, the mgcv package contains many functions that are very useful for such modelling. Some of the details of this type of model are presented in WOOD (2006) and the mgcv package itself.
library(mgcv)
library(MuMIn)
library(gratia) # for the 'appraise' function

# -------------------------------------------------------------------------------------------------------------------------------------------
# -------------  SPECIFY COVARIATES FOR MODEL & MAKE CUTDOWN DATAFRAME ----------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------

# to make it easy to change the covariates in the model for sea level or DHW make each covariate a string to SUBSET A NEW DATAFRAME WITH ONLY THOSE COVARIATES:
SLA_string <- 'X0.6.1'
DHW_string <- 'DHW_360'

# now make a subset which will be used - Transects separate
monthly_subset <- subset(monthly, select = c("Cover","Cover_prop", "Transect","Decimal_date","year","month","trend","fmonth","fTransect",SLA_string,DHW_string))

# now rename the covariate to SLA and DHW
names(monthly_subset)[10] <- "SLA"
names(monthly_subset)[11] <- "DHW"

# now we can just use SLA and DHW in the code below

# -------------------------------------------------------------------------------------------------------------------------------------------

##### MODEL FITTING ##########

# START BY FITTING A GAM (no autocorrelation structure in the model) 
# this model fits the same smoothers for each Transect but with a different intercept for each by the inclusion of the term "fTransect". The different intercept for each transect adds or subtracts a constant value to the common smoother.
# using covariates Time, SLA, DHW
Gam_model <- gam(Cover_prop ~ fTransect + s(Decimal_date, k=9) +s(SLA, k=9) +s(DHW, k=9),  data = monthly_subset, method="REML")
# leaving time out
Gam_model <- gam(Cover_prop ~ fTransect + s(SLA, k=9) +s(DHW, k=9),  data = monthly_subset, method="REML")
# include month as covariate
Gam_model <- gam(Cover_prop ~ fTransect + s(Decimal_date, k=9) +s(SLA, k=9) +s(DHW, k=9) + fmonth,  data = monthly_subset, method="REML")
Gam_model <- gam(Cover_prop ~ fTransect + s(Decimal_date, k=9) +s(SLA, k=9) +s(DHW, k=9) + s(month, k=9),  data = monthly_subset, method="REML")

# using mean data
#Gam_model <- gam(Cover ~ s(Decimal_date, k=12) +s(SLA, k=12) +s(DHW, k=12),  data = monthly_subset, method="REML")

# using covariates SLA and DHW only
#Gam_model <- gam(Cover_prop ~ fTransect + s(SLA, k=9) +s(DHW, k=9),  data = monthly_subset, method="REML")

print (summary(Gam_model), digits=2, signif.stars=FALSE)
# the variance in the residuals is the "Scale est" figure. The std deviation of the residuals would be the sq root of this.

# check that overall Transect term is significant:
anova(Gam_model)
# plot the model:
plot(Gam_model, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(Gam_model)[1], shade=TRUE, pages=1, all.terms = TRUE)

# GRAPHICAL VALIDATION OF THE MODEL
appraise(Gam_model)

# For graphical model validation, we can use two types of residuals: (i) residuals calculated as observed minus fitted values (also called ordinary residuals) and (ii) normalised residuals

# alternative method to extract residuals and fitted values and to plot these:
E1 <- Gam_model[["residuals"]]                     # ordinary residuals
# You should use standardised residuals instead of the ordinary residuals for the model validation. These are obtained by calculating the observed minus the fitted values and then dividing by the square root of the variance
Gam_model[["deviance"]]                            # the value for deviance
E2 <- E1/Gam_model[["deviance"]]                  # standardised residuals

F1 <- Gam_model[["fitted.values"]]
plot(x = F1, y = E1, xlab = "Fitted values", ylab = "Ordinary Residuals")
abline(h = 0)
plot(x = F1, y = E2, xlab = "Fitted values", ylab = "Normalised Residuals")
abline(h = 0)

# residuals plotted in a coplot or residuals vs Time (or SLA or DHW) showing each Transect separately:
coplot(E1 ~ Decimal_date| fTransect, data=monthly_subset, ylab="Ordinary residuals", rows=1, col="red")
coplot(E2 ~ Decimal_date| fTransect, data=monthly_subset, ylab="Normalised residuals", rows=1)
coplot(E2 ~ SLA| fTransect, data=monthly_subset, ylab="Normalised residuals", rows=1)
coplot(E2 ~ DHW| fTransect, data=monthly_subset, ylab="Normalised residuals", rows=1)

# the power of this tool potentially becomes more apparent when you inspect two or more conditioning variables. For example:
coplot(E2 ~ Decimal_date| fTransect*fmonth, data=monthly_subset, ylab="Normalised residuals", rows=1)
coplot(E2 ~ Decimal_date| fTransect*year, data=monthly_subset, ylab="Normalised residuals", number=7)
# Plotting these residuals should not show any heterogeneity. If there is any heterogeneity, then further model improvement is required.

acf(E2)
acf(residuals(Gam_model))  # a little autocorrelation in the residuals
gam.check(Gam_model)

# Plot Residuals versus model terms:
monthly_subset$E2 <- E2
MyVar   <- c("SLA", "DHW", "Decimal_date", "month")
MyxyplotPolygon(monthly_subset, MyVar, "E2")

acf(residuals(Gam_model, type = "deviance")) # some autocorrelation present
# ---------------------------------------------------------------------------------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ADD AUTOCORRELATION TERM IN GAMM

# ZUUR et al (2009) Mixed Effects.... see page 147: Schabenberger and Pierce (2002): ‘In our experience it is more important to model the correlation structure in a reasonable and meaningful way rather than to model the correlation structure perfectly’. Similar statements can be found in Diggle et al. (2002), and Verbeke and Molenberghs (2000). We agree with this statement as differences in p-values for the F- and t-statistics obtained by using similar correlation structures tend to differ only marginally.

# we will use the uGamm from MuMIn so that we have a call component for 'dredge' later

Full_model <- uGamm(Cover ~ 
                      s(Decimal_date, k=25) +        # Time 
                      s(SLA, k=25) +                 # sea level
                      fTransect +                    # Transect number as a categorical term
                      s(month) +                     # month 
                      s(DHW, k=15),                  # Degree Heating Weeks
                      data = monthly_subset, 
                      correlation = corAR1(form = ~ 1 | year), method="REML")   

# drop month term for comparison

Full_model_1 <- uGamm(Cover ~ 
                      s(Decimal_date, k=25) +        # Time 
                      s(SLA, k=25) +                 # sea level
                      fTransect +                    # Transect number as a categorical term
                      #s(month) +                     # month 
                      s(DHW, k=15),                  # Degree Heating Weeks
                      data = monthly_subset, 
                      correlation = corAR1(form = ~ 1 | year), method="REML") 

# which is the better model
model.sel(Full_model, Full_model_1)
anova(Full_model_1$lme, Full_model$lme)

# inspect the partial plots
plot(Full_model$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(Full_model$gam)[1], shade=TRUE, pages=1, all.terms = TRUE)
plot(Full_model_1$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(Full_model$gam)[1], shade=TRUE, pages=1, all.terms = TRUE)

# different correlation structures:
# (1) the simple correlation structure would be 'correlation = corAr1()'
# (2) the "form = ~1|year" means that the temporal order of the data is specified by the variable "year". This means that the same residual correlation structure is assumed each year - shown by only one value for phi in the $lme component of the model. This speeds up fitting no end, but is potentially risky as we do not consider residual variation from year to year.
# (3) when the term is specified as "form = ~ year | month" (ie order by year and nested by month), the model does not compute -  "Error in Initialize.corAR1(X[[i]], ...) : covariate must have unique values within groups for "corAR1" objects". NOTE: does compute where mean values are used.
# (4) same as 3 when "form = ~ year | fTransect"

# if we exclude 'fTransect' as a categorical term (with 4 levels) we then find (below) in our plots of the residuals against each transect that there are significant differences between each transect. If we then include fTransect as a categorical term above then we see a marked improvement in AICc (from 1633.8 to 1614.1) and that no Transect effect remains in the residuals. 

# including the fTransect categorical covariate means that the model uses one intercept and one smooth curve for each smoothed covariate and 4 regression parameters for Transect. Each of these regression parameters is used as a correction of the intercept for a particular plot. As a result we will find that the predicted values consist of 4 parallel smooth lines. The question is now what to do with the covariate Transect. We cannot ignore it as the model will then contain residual patterns. But if we include the covariate in the model then we use an additional 4 regression parameters and the model allows only for statements and predictions applying to these 4 Transects not for Transects in general. In the next section we show that the linear mixed effects plot allows for a Transect effect while overcoming these obstacles.

print (summary(Full_model$gam), digits=4, signif.stars=FALSE)
summary(Full_model$lme)


# check that the fTransect term is significant
anova(Full_model$gam)
plot(Full_model$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(Full_model$gam)[1], shade=TRUE, pages=1, all.terms = TRUE)

# examine whether removing terms improves the model 
dredge(Full_model)

appraise(Full_model$gam)
acf(residuals(Full_model$lme, type = "normalized")) # some autocorrelation present
gam.check(Full_model$gam)

# Plot Residuals versus model terms:
E4 <- resid(Full_model$lme, type = "normalized") 
monthly_subset$E4 <- E4
MyVar   <- c("SLA", "DHW", "Decimal_date","month")
MyxyplotPolygon(monthly_subset, MyVar, "E4")

###################################################################################################
# the Fullmodel indicates that the terms for month and DHW are linear so we could use linear terms for these:
Full_model_linear <- uGamm(Cover ~ 
                      s(Decimal_date, k=25) +        # Time 
                      s(SLA, k=25) +                 # sea level
                      fTransect +                    # Transect number as a categorical term
                      month +                        # linear term for month 
                      DHW,                           # linear term for Degree Heating Weeks
                      data = monthly_subset, 
                      correlation = corAR1(form = ~ 1 | year), method="REML")   

print (summary(Full_model_linear$gam), digits=4, signif.stars=FALSE)  
# is this a better model?
model.sel(Full_model, Full_model_linear)
anova(Full_model$lme, Full_model_linear$lme)
# difference is barely noticeable and not significant
plot(Full_model_linear$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(Full_model_linear$gam)[1], shade=TRUE, pages=1, all.terms = TRUE)

# ----------------     TRANSECTS   -------------------------------------------------------------------------------
# are the residuals from the same transect more similar than from different transects? Make a conditional boxplot:
boxplot(E4 ~ fTransect, data = monthly_subset, cex.lab = 1.5, xlab = "Transect", ylab = "Standardized residuals")
abline(0, 0, lty = 2)

# if we suspect that they may be then apply a linear regression to the residuals as a function of the Transect:
T1 <- lm(E4 ~ fTransect, data = monthly_subset)
summary(T1)
anova(T1)
drop1 (T1, test="F")
# A significant population effect would mean that we should abandon the current model and possibly include a random term for Transect

# ---------------------   MONTHS   -------------------------------------------------------------------------------
# are the residuals from the same Month more similar than from different Months? Make a conditional boxplot:
boxplot(E4 ~ month, data = monthly_subset, cex.lab = 1.5, xlab = "Month", ylab = "Standardized residuals")
abline(0, 0, lty = 2)

# if we suspect that they may be then apply a linear regression to the residuals as a function of the Month:
T1 <- lm(E4 ~ month, data = monthly_subset)
summary(T1)
anova(T1)
drop1 (T1, test="F")
# A significant population effect would mean that we should abandon the current model and possibly include a random term for Month

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# change corAR term from year to Decimal_date
Full_model_a <- uGamm(Cover ~ 
                        s(Decimal_date, k=25) +        # Time 
                        s(SLA, k=25) +                 # sea level
                        fTransect +                    # Transect number as a categorical term
                        s(month) +                    # month 
                        s(DHW, k=15),                  # Degree Heating Weeks
                      data = monthly_subset, 
                      correlation = corAR1(form = ~ 1| Decimal_date), method="REML") 
# The "form = ~1|Decimal_date" means that the ARMA is nested within the date expressed as a decimal month. 

# IS THIS CORRECT? - This means that the same residual correlation structure is assumed from year to year - shown by only one value for phi in the $lme component of the model. This speeds up fitting no end, but is potentially risky as we do not consider residual variation from year to year.

# the parameter Phi = XX - this means that residuals separated by the date have a correlation of XX. This is rather high, but seems to be in line with the pattern for the first few years in the auto-correlation function. The AIC indicates that the AR-1 correlation structure is a considerable model improvement compared to the non AR1 regression model. In general, you would expect ρ to be positive as values at any particular point in time are positively related to preceding time points. Occasionally, you find a negative ρ. Plausible explanations are either the model is missing an important explanatory variable or the abundances go from high values in one year to low values in the next year.

print (summary(Full_model_a$gam), digits=4, signif.stars=FALSE)
  
summary(Full_model_a$lme)
# check that the fTransect term is significant
anova(Full_model_a$gam)
plot(Full_model_a$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(Full_model_a$gam)[1], shade=TRUE, pages=1, all.terms = TRUE)
# compare this with the Full_model
anova(Full_model$lme, Full_model_a$lme)
model.sel(Full_model,Full_model_a)

# examine whether removing terms improves the model 
dredge(Full_model_a)

appraise(Full_model_a$gam)
# Looking now at the normalized residuals (which take into account the covariance matrix of the residuals):
acf(residuals(Full_model_a$lme, type = "normalized")) # some autocorrelation present
gam.check(Full_model_a$gam)                              

# Plot Residuals versus model terms:
E4a <- resid(Full_model_a$lme, type = "normalized") 
monthly_subset$E4a <- E4a
MyVar   <- c("SLA", "DHW", "Decimal_date","month")
MyxyplotPolygon(monthly_subset, MyVar, "E4a")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# the model with a random effect for Transect

Full_model_random <- uGamm(Cover ~ 
                      s(Decimal_date, k=25) +        # Time - smoothed term is: s(Decimal_date, k=25)
                      s(SLA, k=25) +                 # sea level
                      s(month) +                     # month 
                      s(DHW, k=15) +                 # Degree Heating Weeks
                      s(fTransect, bs="re"),         # the random term for Transect
                      data = monthly_subset, 
                      correlation = corAR1(form = ~ 1 | Decimal_date), method="REML")    


# see Zuur et al 2012 Chapt 4 - the inclusion of a random intercept automatically imposes a correlation structure on all Coral Cover values from the same Transect. What this means is that the correct dependency structure in the Coral Cover values is specified and consequently a correct covariance matrix is used to calculate the standard errors. *** So the primary reason for applying a linear mixed effects model is to incorporate a dependency structure into the model and, consequently, obtain better standard errors for the regression parameters than those given by a linear regression model.**** Essentially we have nested data (multiple observations made on the same transect).

# alternative way of incorporating the random effect as specified in the mgcv::gamm package:
Full_model_random1 <- uGamm(Cover ~ 
                             s(Decimal_date, k=25) +        # Time - smoothed term is: s(Decimal_date, k=25)
                             s(SLA, k=25) +                 # sea level
                             s(month) +                     # month 
                             s(DHW, k=15),                  # Degree Heating Weeks
                             random=list(fTransect=~1),     # the random term for Transect - =~1 means we want an estimated random intercept
                             data = monthly_subset, 
                             correlation = corAR1(form = ~ 1 | Decimal_date), method="REML")  

# there is nothing statistically wrong in specifying either of these models with the random effect. The second (random=list(fTransect=~1)) is less conservative. It uses a "standard" maximum likelihood function on the estimates, whereas the s(fTransect,bs="re") uses a penalized ridge function which makes it a more conservative approach.

# in this second method the summary does not contain any information on the random effect - you have to go to ranef (see below) to see how the intercepts differ.

# COMPARING THE OUTPUT FROM BOTH MODELS WITH DIFFERENTLY CONFIGURED RANDOM EFFECTS FOR TRANSECT.

#First compare the models:
anova(Full_model_random$lme, Full_model_random1$lme)
# the first method has a slightly improved AIC
# us MuMIn for this comparison:
library(MuMIn)
model.sel(Full_model_random,Full_model_random1,Full_model)

print (summary(Full_model_random$gam), digits=2, signif.stars=FALSE)
# adj R2 69% 
print (summary(Full_model_random1$gam), digits=2, signif.stars=FALSE)
# adj R2 66.8% 

plot(Full_model_random$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(Full_model$gam)[1], shade=TRUE, pages=1, all.terms = TRUE) 
plot(Full_model_random1$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(Full_model$gam)[1], shade=TRUE, pages=1, all.terms = TRUE)

# examine whether removing terms improves the model 
dredge(Full_model_random)
# this indicates that the full Model has the smallest AICc 
dredge(Full_model_random1)
# this indicates that the full Model has the smallest AICc 

appraise(Full_model_random$gam)
appraise(Full_model_random1$gam)

acf(residuals(Full_model_random$lme, type = "normalized")) # some autocorrelation present
acf(residuals(Full_model_random1$lme, type = "normalized")) # less autocorrelation present

gam.check(Full_model_random$gam) 
gam.check(Full_model_random1$gam) 


# -----------------------------------------------------------------------------------------------------------------
# USING ZUUR (2012)'s plotting of all covariates vs residuals

## we could also plot the residuals vs each other covariate and each covariate not in the model (if there are any). We are checking that there are no significant patterns in the residuals.

## For continuous covariates we can use multi-panel scatterplots. To aid visual interpretation we add a smoother to each panel. The 95% point-wise confidence intervals for the smoothers (grey area) should contain 0 for all covariate values, indicating that there are no significant patterns in the residuals.

## R code to make the multi-panel scatterplot is based on the MyxyplotPolygon function that can be found in the HighstatLibV10.R function.

# =============================================================================================================


# Plot Residuals versus model terms:
Gammodel <- resid(Gam_model, type = "working") # can be one of  “deviance”, “pearson”, “scaled.pearson”, “working”, “response”
monthly_subset$Gammodel <- Gammodel
MyVar   <- c("SLA", "DHW", "Decimal_date","month")
MyxyplotPolygon(monthly_subset, MyVar, "Gammodel")


# Plot Residuals versus model terms:
E4r <- resid(Full_model_random$lme, type = "normalized") 
monthly_subset$E4r <- E4r
MyVar   <- c("SLA", "DHW", "Decimal_date","month")
MyxyplotPolygon(monthly_subset, MyVar, "E4r")

# Plot Residuals versus model terms:
E4r1 <- resid(Full_model_random1$lme, type = "normalized") 
monthly_subset$E4r1 <- E4r1
MyVar   <- c("SLA", "DHW", "Decimal_date","month")
MyxyplotPolygon(monthly_subset, MyVar, "E4r1")

# ------------------------------------------------------------------------------------------------------
# We have multiple observations from the same population, where ‘population’ refers to month or Transect
table (monthly_subset$month)
table (monthly_subset$Transect)

## One may argue that the measurements of Mean coral cover from the same month or Transect are likely to be more similar than those from different months/ Transects. Use Boxplots to check this:

# ----------------     TRANSECTS   -------------------------------------------------------------------------------
# are the residuals from the same transect more similar than from different transects? Make a conditional boxplot:
boxplot(E4r ~ fTransect, data = monthly_subset, cex.lab = 1.5, xlab = "Transect", ylab = "Standardized residuals")
abline(0, 0, lty = 2)
boxplot(E4r1 ~ fTransect, data = monthly_subset, cex.lab = 1.5, xlab = "Transect", ylab = "Standardized residuals")
abline(0, 0, lty = 2)

# if we suspect that they may be then apply a linear regression to the residuals as a function of the Transect:
T1r <- lm(E4r ~ fTransect, data = monthly_subset)
summary(T1r)
anova(T1r)
drop1 (T1r, test="F")
# A significant population effect would mean that we should abandon the current model and possibly include a random term for Transect

# ---------------------   MONTHS   -------------------------------------------------------------------------------
# are the residuals from the same Month more similar than from different Months? Make a conditional boxplot:
boxplot(E4r ~ month, data = monthly_subset, cex.lab = 1.5, xlab = "Month", ylab = "Standardized residuals")
abline(0, 0, lty = 2)

boxplot(E4r1 ~ month, data = monthly_subset, cex.lab = 1.5, xlab = "Month", ylab = "Standardized residuals")
abline(0, 0, lty = 2)

# if we suspect that they may be then apply a linear regression to the residuals as a function of the Month:
T1r <- lm(E4r1 ~ month, data = monthly_subset)
summary(T1r)
anova(T1r)
drop1 (T1r, test="F")
# A significant population effect would mean that we should abandon the current model and possibly include a random term for Month

## If we are faced with problems consider the following options: 
# 1. By design of the study, apply a linear mixed effects model in which month/transect is used as a random intercept. This will impose a correlation on the observations from the same month/transect. Under this option we would not test whether the random effects are needed. 
# 2. Apply a linear mixed effects model using month/transect as random effect and compare this to a model without the random effects. A likelihood ratio test can be used for formal comparison. See, for example, Pinheiro and Bates (2000) or Zuur et al. (2009a). 
# 3. Apply a linear regression model that does not contain month/transect, extract the residuals, and verify whether the residuals contain a month/transect effect. 
# 4. Take averages for each variable (response and covariates) per month/treansect and analyze these. Would this make any sense for this data????

# ----------------------------------------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# inspect the random effect coefficients:
ranef(Full_model_random$lme)
ranef(Full_model_random1$lme)

# _______________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@  WHAT IS THE CONTRIBUTION OF INDIVIDUAL TERMS TO THE VARIATION IN THE DATA EXPLAINED BY THE MODEL?   @@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# see the post on "variance explained by each term in a GAM" - https://stat.ethz.ch/pipermail/r-help/2007-October/142811.html
# answered by Simon Wood (mgcv package creator)

# "You should use the same smoothing parameters throughout. i.e. the reduced models should use the same smoothing parameters as the full model. Otherwise you get in trouble if terms are correlated, since the smoothing parameters will then tend to change a lot when terms are dropped as one smooth tries to `do the work' of the other". 

# BEST MODEL WAS: Full_model
# define the best model to save changing all code below:
Bestmodel <- Full_model

summary(Bestmodel$gam)

# display and check the smoothing parameters used by the above model for each smoothed term - they will be used below in the reduced models
Bestmodel$gam$sp[1] # Decimal_date term
Bestmodel$gam$sp[2] # SLA term
Bestmodel$gam$sp[3] # Month term
Bestmodel$gam$sp[4] # DHW term

# GAMM does not allow the use of "sp" to produce reduced models. Instead use GAMs to fix the smoothing parameters and use the resulting R2 to describe the contribution of each term. 

b1s <- gam(Cover ~ s(Decimal_date, sp=Bestmodel$gam$sp[1]), data = monthly_subset, method="REML", select=FALSE) # time only
summary(b1s)$r.sq
# summary(b1s)

b2s <- gam(Cover ~ s(SLA, sp=Bestmodel$gam$sp[2]), data = monthly_subset, method="REML", select=FALSE)  # SLA only
summary(b2s)$r.sq
# summary(b2s)

b3s <- gam(Cover ~ s(DHW, sp=Bestmodel$gam$sp[3]), data = monthly_subset, method="REML", select=FALSE)  # Month only
summary(b3s)$r.sq
# summary(b3s)

b4s <- gam(Cover ~ s(DHW, sp=Bestmodel$gam$sp[4]), data = monthly_subset, method="REML", select=FALSE)  # DHW only
summary(b4s)$r.sq
# summary(b3s)

# this way below does the same thing using the GAMM model but not fixing the smoothing parameter. Can compare the result if you wish:
# time only
M2atime <- gamm(Cover ~  s(Decimal_date) ,data = monthly_subset, correlation = corARMA(form = ~ 1 | year, p=1), method="REML", select=FALSE)
summary(M2atime$gam)
# SLA only
M2aSLA <- gamm(Cover ~  s(SLA), data= monthly_subset, correlation = corARMA(form = ~ 1 | year, p=1), method="REML", select=FALSE)
summary(M2aSLA$gam)
# DHW only
M2aDHW <- gamm(Cover ~  s(DHW), data= monthly_subset, correlation = corARMA(form = ~ 1 | year, p=1), method="REML", select=FALSE)
summary(M2aDHW$gam)


# what is the linear relationship of time,SLA and DHW make these terms linear
M2alinear <- gam(Cover ~  s(Decimal_date) + SLA + DHW, data = monthly_subset, method="REML", select=FALSE)
summary(M2alinear)

# ----------------------------------------------------------------------

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@  EXTRACTING INDIVIDUAL MODEL TERMS AND PLOTTING THEM @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# The fitted GAM model object contains a lot of information that can be used to interrogate the model. 

# GAVIN SIMPSON https://fromthebottomoftheheap.net/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/
# and Wood (2017) pages 339

library(Hmisc) # for specifying minor axes ticks on the plots we will use.

# the Best Model chosen was as follows:
Bestmodel

# SPECIFY A NEW RANGE AND FREQUENCY OF SAMPLING FROM THE ORIGINAL DATA OVER WHICH WE MAKE PREDICTIONS 
# The code below selects XXX evenly-spaced values over the range of the data.
want <- seq(1, nrow(monthly_subset), length.out = 227) # gets all data points For the original measured data this is 227 for all transects.  

# we need all the predictor variables used in the model because these are required by the model for computing the predictions.
pdat1 <- with(monthly_subset, data.frame(Decimal_date=Decimal_date[want], SLA=SLA[want], fTransect=fTransect[want], DHW=DHW[want], month=month[want]))
# these model terms will be placed in the above order in the dataframe pdat1

# LINEAR PREDICTORS
# type="link" is the default - returns predictions (and SE) on the linear predictor scale, ie these are partial plots for the selected term with other terms held at their median

# computes overall model fit
Model.fitFull_model <- predict(Bestmodel$gam, newdata = pdat1, type="link", se.fit=TRUE)
# Model.fitFull_model

# p2a  <- predict(Bestmodel$gam,  newdata = pdat1, type = "response", se.fit = TRUE) # returns overall model fit on the scale of the response - same as Model.fitM2a above but with SEs now as well 

## NOW PREDICT EACH TERM IN THE MODEL SEPARATELY
##   returns linear predictor scale predictions (and Standard Error) split up by term
#  The terms in our model were: s(Decimal_date), s(SLA=X0.12), s(fTransect), s(DHW=DHW_120) and have already been placed in the pdat1 dataframe above.

# predict each term separately with the standard error
p2x  <- predict(Bestmodel$gam,  newdata = pdat1, type = "terms", se.fit = TRUE) 
# here the output contains both "fit" and the standard error "se.fit" for each term, hence we can extract these and add them into out dataframe "pdat1"

# this will tell us what the order of predictions is:
Bestmodel[["gam"]][["pred.formula"]]

# DOUBLE CHECK the output of p2x IS AS EXPECTED by printing to the console so that we can see what order each fitted term is in:
p2x[["fit"]]

# add the output from p2x to the dataframe pdat1
# this code may need to be changed to agree with the console output of p2x above:
pdat1 <- transform (pdat1, fit.fTransect= p2x$fit[,1], se.fit.fTransect= p2x$se.fit[,1], fit.Decimal_date = p2x$fit[,2], se.fit.Decimal_date= p2x$se.fit[,2], fit.SLA = p2x$fit[,3], se.fit.SLA= p2x$se.fit[,3], fit.month = p2x$fit[,4], se.fit.month= p2x$se.fit[,4], fit.DHW = p2x$fit[,5], se.fit.DHW= p2x$se.fit[,5])

# also add the overall model fit predictions to the dataframe:
pdat1 <- transform(pdat1, Model.fit = Model.fitFull_model[["fit"]], se.Model.fit = Model.fitFull_model[["se.fit"]])

###################################################

# LET US CHECK THAT THE SUM OF EACH SEPARATE TERM IS THE SAME AS THE OVERALL MODEL PREDICTIONS (Model.fit) TOGETHER WITH THE INTERCEPT
# in the dataframe add each of the separate term fits together and make new column:
# models with no interaction term - all transects
pdat1$no.intercept.fit <- pdat1$fit.SLA + pdat1$fit.Decimal_date + pdat1$fit.DHW + pdat1$fit.fTransect +pdat1$fit.month

# in the dataframe subtract no.intercept.fit from p1.fit - this should equal our intercept value which was produced by the model summary:
pdat1$intercept <- pdat1$Model.fit - pdat1$no.intercept.fit
# it does!

# also add a fit column for the combined effect of DHW and SLA
pdat1$fit.DHW_SLA <- pdat1$fit.SLA + pdat1$fit.DHW

# to view the coefficients
coefficients(Bestmodel$gam)
# or use
Bestmodel$gam$coefficients

# better presentation
summary_Full_model <- summary(Bestmodel$gam)
summary_Full_model$p.table

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE PDAT1 TO A CSV
#write.csv(pdat1, "Coral Cover model & term predicted values.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now plot the original data so that we can add plots for the overall model and/or each of the terms computed below
plot(Cover ~ Decimal_date, data = monthly_subset, type = "p", ylab = "Mean Coral Cover (%)", xlab="", ylim=c(-20,60)) # plot the original data
minor.tick(nx=5, tick.ratio=0.8) # add minor ticks on x axis - nx=5 means 5 ticks between the major marks
#minor.tick(ny=1, tick.ratio=0.8) # add minor ticks on y axis
abline(h=c(0), col="dark grey") # horizontal reference line at 0
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# and plot these:
lines(Model.fit ~ Decimal_date, data = pdat1, col = "red")                  # the overall model

# sea level term without intercept - #lines(fit.SLA ~ Decimal_date, data = pdat1, col = "blue")         
# add the intercept & transect to the sea level term and replot:
pdat1$sea_level <- pdat1$fit.SLA + pdat1$intercept
lines(sea_level ~ Decimal_date, data = pdat1, col = "blue") 

# time term
#lines(fit.Decimal_date ~ Decimal_date, data = pdat1, col = "black", lwd=2)  # time
# add the intercept to the time term and replot:
pdat1$time_trend <- pdat1$fit.Decimal_date + pdat1$intercept
lines(time_trend ~ Decimal_date, data = pdat1, col = "black") 

# DHW term
#lines(fit.DHW ~ Decimal_date, data = pdat1, col = "green")            # DHW
# add the intercept to the DHW term and replot:
pdat1$DHW_trend <- pdat1$fit.DHW + pdat1$intercept
lines(DHW_trend ~ Decimal_date, data = pdat1, col = "green") 

# Month term
# add the intercept to the Month term and plot:
pdat1$Month_trend <- pdat1$fit.month + pdat1$intercept
lines(Month_trend ~ Decimal_date, data = pdat1, col = "red") 

# DHW and SLA combined:
pdat1$DHW_SLA <- pdat1$fit.DHW_SLA + pdat1$intercept
lines(DHW_SLA ~ Decimal_date, data = pdat1, col = "blue") 



# add approx 95% CI for time trend as dotted lines - FIX THIS
lines(pdat1$Model.fit,(pdat1$Model.fit+1.96*pdat1$se.Model.fit),col="black", lty=2, lwd=1)
lines(pdat1$Model.fit,(pdat1$Model.fit-1.96*pdat1$se.Model.fit),col="black", lty=2, lwd=1)

# --------------------------------------------------------------------------------------------
# using GGPLOT2 for more control

library(ggplot2)
library(scales) # helps to label and scale axes
###################################################################################

# set up basic plotting 
gX <- ggplot(data=monthly_subset, aes(x= Decimal_date)) + 
  scale_x_continuous(name = "", limits = c(1987,2022), breaks = seq(1987,2022,1), labels=c("1987","","1989","","1991","","1993","","1995","","1997","","1999","","2001","","2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017","","2019","","2021",""))  + 
  scale_y_continuous(name="Coral Cover %", limits=c(0,70))+
  theme_bw() +
  theme(plot.margin=unit(c(4,3,4,0), "cm")) +     # plot margins are in the order top, right, bottom, left                         
  theme(axis.text.x = element_text(angle = 0, hjust= 0, vjust = 0)) + # adjust the position of the X axis tick labels
  theme(panel.grid.minor = element_blank()) + # remove the minor grid lines vertically
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) # makes axis text black
gX

# add rectangles to mark combined ENSO/IOD or just IOD
# these rectangles have start and end dates for when the NINO3.4 was above the standard deviation value computed above for several weeks. The decimal date information entered below was extracted from the weekly NINO3.4 file ("NINO3.4 weekly to 13 Jan 2021.xlsx") in the folder E:\Phuket Databases\IOD-NNI-SOI\NINO3.4\State of the Ocean Climate. For Nino 3.4 the file is "El Nino 3.4 weekly 1981 to 15 Jul 2020.xlsx" and the level used was >0.5 deg C.
# fill=colour of rectangle, size=border width, colour=border col
# Combined IOD/ENSO is grey
# IOD only is lemonchiffon
# ENSO only is lightcyan

gX1 <- gX +
  geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=1987,xmax=1988.07103825137,ymin=0,ymax=70),colour="white",alpha=0.2,fill="cyan",size=0.5) + 
  # Note the above period actually started on 1986.7095890411 but if this date is used it is omitted because the scale starts at 1987
  geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=1991.38630136986,xmax=1992.45901639344,ymin=0,ymax=70),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  # discount this period only 13 weeks geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=1993.22465753425,xmax=1993.45479452055,ymin=0,ymax=70),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=1994.27945205479,xmax=1995.14246575342,ymin=0,ymax=70),colour="white",alpha=0.2,fill="gray70",size=0.5) +
  geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=1997.32602739726,xmax=1998.32328767123,ymin=0,ymax=70),colour="white",alpha=0.2,fill="gray70",size=0.5) +
  geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=2002.42465753425,xmax=2003.15342465753,ymin=0,ymax=70),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=2004.59016393443,xmax=2004.99180327869,ymin=0,ymax=70),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=2006.62191780822,xmax=2007.02465753425,ymin=0,ymax=70),colour="white",alpha=0.2,fill="gray70",size=0.5) +
  geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=2009.47671232877,xmax=2010.26301369863,ymin=0,ymax=70),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  #IOD but discount this short period geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=2012.56284153005,xmax=2012.77322404372,ymin=0,ymax=70),colour="white",alpha=0.2,fill="lemonchiffon",size=0.5) +
  geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=2015.15068493151,xmax=2016.33879781421,ymin=0,ymax=70),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  #IOD but discount this short period geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=2017.27671232877,xmax=2017.60273972603,ymin=0,ymax=70),colour="white",alpha=0.2,fill="lemonchiffon",size=0.5) +
  geom_rect(data=monthly_subset, aes(NULL,NULL,xmin=2018.73424657534,xmax=2020,ymin=0,ymax=70),colour="white",alpha=0.2,fill="gray70",size=0.5)

gX1

# add the transect cover data
gX2 <- gX1 + geom_point(aes(y=Cover, group=Transect, color=Transect))
gX2

# add the Model.fit
gX2 + geom_line(aes(y=pdat1$Model.fit)) + ggtitle("Overall Model Fit") #, group=pdat1$fTransect, color=pdat1$fTransect)) 

# add the DHW term only
gX2 + geom_line(aes(y=pdat1$DHW_trend)) + ggtitle("DHW Term")    #, group=pdat1$fTransect, color=pdat1$fTransect))

# add the time term only
gX2 + geom_line(aes(y=pdat1$time_trend)) +ggtitle("Time Term")   #, group=pdat1$fTransect, color=pdat1$fTransect))

# add the Sea level term only
gX2 + geom_line(aes(y=pdat1$sea_level))+ ggtitle("Sea Level Term")     #, group=pdat1$fTransect, color=pdat1$fTransect))

# add the seasonality term only
gX2 + geom_line(aes(y=pdat1$Month_trend))+ ggtitle("Month Term")     #, group=pdat1$fTransect, color=pdat1$fTransect))


# all terms 
gX2 + geom_line(aes(y=pdat1$Model.fit), colour="black", size=1) + 
  geom_line(aes(y=pdat1$DHW_trend), colour="red") + 
  geom_line(aes(y=pdat1$time_trend), colour="grey") + 
  geom_line(aes(y=pdat1$sea_level), colour="blue") +  
  annotate("text", x = 2005, y = 70, label = "Model: Bestmodel") +
  annotate("text", x = 2004.5, y = 0, label = Bestmodel[["call"]]) + 
  annotate("text", x = 1990.5, y = 67, label = "Full Model") + 
  annotate("text", x = 1993.7, y = 65, label = "Degree Heating Weeks (DHW_120)")+ 
  annotate("text", x = 1989.9, y = 63, label = "Time")+ 
  annotate("text", x = 1992.5, y = 61, label = "Sea Level Anomaly (X0.12)") + 
  annotate("text", x = 1990.4, y = 65, label = DHW_string)+ 
  annotate("text", x = 1989.9, y = 63, label = "Time")+ 
  annotate("text", x = 1990.2, y = 61, label = SLA_string) +
  annotate("segment", x = 1988, xend = 1989, y = 67, yend = 67, colour = "black", size=1) +
  annotate("segment", x = 1988, xend = 1989, y = 65, yend = 65, colour = "red") +
  annotate("segment", x = 1988, xend = 1989, y = 63, yend = 63, colour = "grey") +
  annotate("segment", x = 1988, xend = 1989, y = 61, yend = 61, colour = "blue") 

# DHW and SLA on same plot
gX2 + 
  #geom_line(aes(y=pdat1$Model.fit), colour="black", size=1) + 
  geom_line(aes(y=pdat1$DHW_trend), colour="red") + 
  #geom_line(aes(y=pdat1$time_trend), colour="grey") + 
  geom_line(aes(y=pdat1$sea_level), colour="blue") +  
  #annotate("text", x = 2005, y = 70, label = "Model: Bestmodel") +
  #annotate("text", x = 2004.5, y = 0, label = Bestmodel[["call"]]) + 
  #annotate("text", x = 1990.5, y = 67, label = "Full Model") + 
  annotate("text", x = 1992.9, y = 65, label = "Degree Heating Weeks")+ 
  #annotate("text", x = 1990.3, y = 63, label = "Time")+ 
  annotate("text", x = 1991, y = 61, label = "Sea Level") + 
  #annotate("segment", x = 1988, xend = 1989, y = 67, yend = 67, colour = "black", size=1) +
  annotate("segment", x = 1988, xend = 1989, y = 65, yend = 65, colour = "red") +
  #annotate("segment", x = 1988, xend = 1989, y = 63, yend = 63, colour = "grey") +
  annotate("segment", x = 1988, xend = 1989, y = 61, yend = 61, colour = "blue") 

# plot DHW and SLA combined
gX2 + 
  #geom_line(aes(y=pdat1$Model.fit), colour="black", size=1) + 
  #geom_line(aes(y=pdat1$DHW_trend), colour="red") + 
  #geom_line(aes(y=pdat1$time_trend), colour="grey") + 
  #geom_line(aes(y=pdat1$sea_level), colour="blue") +  
  #annotate("text", x = 2005, y = 70, label = "Model: Bestmodel") +
  #annotate("text", x = 2004.5, y = 0, label = Bestmodel[["call"]]) + 
  #annotate("text", x = 1990.5, y = 67, label = "Full Model") + 
  #annotate("text", x = 1992.9, y = 65, label = "Degree Heating Weeks")+ 
  #annotate("text", x = 1990.3, y = 63, label = "Time")+ 
  #annotate("text", x = 1991, y = 61, label = "Sea Level") + 
  #annotate("segment", x = 1988, xend = 1989, y = 67, yend = 67, colour = "black", size=1) +
  #annotate("segment", x = 1988, xend = 1989, y = 65, yend = 65, colour = "red") +
  #annotate("segment", x = 1988, xend = 1989, y = 63, yend = 63, colour = "grey") +
  #annotate("segment", x = 1988, xend = 1989, y = 61, yend = 61, colour = "blue") 
  geom_line(aes(y=pdat1$DHW_SLA), colour="black", size=1)



# *******************************************************************************************************  


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@  CHECK THE MODELS  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# MODEL CHECKING WITH gam.check(): Now that we have fitted a GAMM model to the data, we need some checks to make sure that we have well-fit models. There are several pitfalls we need to look out for when fitting GAMMs. Thankfully, mgcv provides helpful tools to check for these.

# 2.5.1 READING MODEL DIAGNOSTICS: gam.check() helps you understand whether you have enough basis functions to model the data.

# (1) Print diagnostics on model (mod) basis size and plots of model residuals

gam.check(Bestmodel$gam)

# ***********************************************************************************************************************

# CHECKING CONCURVITY: Now we'll learn about another area that's important to check in GAMs: concurvity. This function produces summary measures of concurvity between GAMM components.

# Concurvity occurs when some smooth term in a model could be approximated by one or more of the other smooth terms in the model. This is often the case when a  smooth of space is included in a model, along with smooths of other covariates that also vary more or less smoothly in space. Similarly it tends to be an issue in models including a smooth of time, along with smooths of other time varying covariates.

# Concurvity can be viewed as a generalization of co-linearity in linear models (i.e. when two variables are correlated), and causes similar problems of interpretation - namely giving poorly fitted models with large CIs. It can also make estimates somewhat unstable (so that they become sensitive to apparently innocuous modelling details, for example). Even if 2 variables in GAM are not colinear they may have concurvity - one may be a smooth curve of another.

# This routine computes three related indices of concurvity, all bounded between 0 and 1, with 0 indicating no problem, and 1 indicating total lack of identifiability. The three indices are all based on the idea that a smooth term, f, in the model can be decomposed into a part, g, that lies entirely in the space of one or more other terms in the model, and a remainder part that is completely within the term's own space. If g makes up a large part of f then there is a concurvity problem. The indices used are all based on the square of ||g||/||f||, that is the ratio of the squared Euclidean norms of the vectors of f and g evaluated at the observed covariate values.

# The three measures are as follows:

# worst - This is the largest value that the square of ||g||/||f|| could take for any coefficient vector. This is a fairly pessimistic measure, as it looks at the worst case irrespective of data. This is the only measure that is symmetric.

# observed - This just returns the value of the square of ||g||/||f|| according to the estimated coefficients. This could be a bit over-optimistic about the  potential for a problem in some cases.

# estimate - This is the squared F-norm of the basis for g divided by the F-norm of the basis for f. It is a measure of the extent to which the f basis can be explained by the g basis. It does not suffer from the pessimism or potential for over-optimism of the previous two measures, but is less easy to understand.

# First Check overall concurvity
concurvity(Bestmodel$gam, full = TRUE) # If TRUE then go on to check concurvity of each term with the whole of the rest of the model. Always look at the worst case and then if concurvity is say >0.8 check the model.

# pairwise concurvity between model variables. Use to find which 2 variables have a close relationship. Do these variable have problematic shapes or large CIs?
concurvity(Bestmodel$gam, full = FALSE)  # If FALSE then pairwise concurvity measures between each smooth term (as well as the parametric component) are considered.

# Even if concurvity is present, the mgcv estimation procedures have been developed with such issues in mind, and one can still feel fairly confident in the results. See Wood, Simon N. 2008. “Fast Stable Direct Fitting and Smoothness Selection for Generalized Additive Models.” Journal of the Royal Statistical Society: Series B (Statistical Methodology) 70 (3). Wiley Online Library: 495–518.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# *************************************************************************************************************************
# ************************** GAVIN SIMPSON DIAGNOSTICS ********************************************************************
# *************************************************************************************************************************

# Zuur 2012 Beginners Guide to GAM page 23

# MODEL VALIDATION
# When a model has been fitted, a model validation must be applied. This process requires the following steps: 1. To verify homogeneity of variance, plot the residuals versus the fitted values. 2. To verify model misfit (or independence), plot the residuals versus each covariate in the model and versus each covariate not in the model. 3. To verify independence if repeated measurements were taken over time or at multiple spatial locations, create auto-correlation functions or variograms using the residuals. 4. To verify the normality assumption, create a histogram of the residuals. 5. Check the model for influential observations. 6. If repeated measurements were taken from the same object (e.g. site, transect), check whether the residuals contain resulting patterns.

# There are several types of residuals that we can use for this, ordinary residuals, standardized residuals, or studentized residuals, see Subsection 1.5.1 
# (1) Standardized residuals plotted versus fitted values.
#     This graph should not show patterns, either in spread or in groups of residuals being consistently above or below zero at certain points on the horizontal axis.
# (2) Plot of residuals vs the 'time' covariate
# (3) Fitted vs Observed
# (4) Histogram of residuals for normality check
# (5) Quantile-Quantile (QQ) plot also for normality check. 
# (6) ACF of residuals to check for autocorrelation

# https://fromthebottomoftheheap.net/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/

## For GAMM models from mgcv:::gamm

# =============================================================================================
## Model Checking function
tsDiagGamm <- function(x, timevar, observed, f = 0.3, type = "normalized") {
  resi <- resid(x$lme, type = type)
  fits <- fitted(x$lme)
  on.exit(layout(1))
  layout(matrix(1:6, ncol = 3, byrow = TRUE))
  plot(resi ~ fits, ylab = "Normalized Residuals",
       xlab = "Fitted Values", main = "Fitted vs. Residuals")
  lines(lowess(x = fits, y = resi, f = f), col = "blue",
        lwd = 2)
  plot(resi ~ timevar, ylab = "Normalized Residuals",
       xlab = "Time", main = "Time series of residuals")
  lines(lowess(x = timevar, y = resi, f = f), col = "blue", lwd = 2)
  plot(observed ~ fits, ylab = "Observed",
       xlab = "Fitted Values", main = "Fitted vs. Observed",
       type = "n")
  abline(a = 0, b = 1, col = "red")
  points(observed ~ fits)
  lines(lowess(x = fits, y = observed, f = f), col = "blue",
        lwd = 2)
  hist(resi, freq = FALSE, xlab = "Normalized Residuals")
  qqnorm(resi)
  qqline(resi)
  acf(resi, main = "ACF of Residuals")
}

# ==============================================================================================

# this is where we tell the function which model output etc., to use - here Bestmodel
with(monthly_subset, tsDiagGamm(Bestmodel, timevar = Decimal_date, observed = Cover)) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# END





