## File: Phuket CRW data - GAMM models.R
# CREATED BY: Richard Dunne
# Last updated: 1 April 2021

# Using CRW data to analyse Hotspot at Phuket

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@   SET DIRECTORY  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# First set the directory in which we are working in the properties section of R
# change the file address below to where you have stored the data file on your computer

setwd ("E:/Our Papers/2020 IOD and Coral Cover/Resubmission/Data & R Code")
# then double check that you are in this directory before running any script
getwd()

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@   LOAD PHUKET HOTSPOT DATA  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# use the method of inporting from a CSV file

Hotspot_df <- read.csv("CRW data for Phuket 5km square - 1 May 1985 to 13 Jan 2021.csv")

class(Hotspot_df$date) # "date") is a 'character' in this dataframe
class(Hotspot_df$short_date) # "date") is also a 'character' in this dataframe

# Make a POSIXct date vector from the "date" column to POSIXct date
tm <- as.POSIXct(Hotspot_df$date, tz="GMT") 
head(tm) # lists first few data
tail(tm) # lists last few data
class(tm) # this is of class "POSIXct" "POSIXt"

# we can also make an alternative of this as a class of "Date"
tmdate <- as.Date(Hotspot_df$date)
class(tmdate)

# number of NA values in a particular column - here it is our Hotspot column "HOT"
sum(is.na(Hotspot_df$HOT))

# add the POSIXct time as a column
# make a dataframe with the POSIXct date/time as a vertical column
timePOSIXct_df <- data.frame(tm) 
# CHECK THE TIME BOUNDS FOR THE DATA
min(timePOSIXct_df$tm)
max(timePOSIXct_df$tm)

# now cbind the time data frame to the Hotspot_df - this adds this as the last column
Hotspot_df <- data.frame(cbind(Hotspot_df, timePOSIXct_df)) # this way round puts the POSIXct column last

##     we could write and store this dataframe to a CSV
##     write.csv(Hotspot_df, "Phuket Hotspot.csv")

##############################################################################################################################
#####################################   HAVE A QUICK LOOK AT THE DATA  #######################################################
#######################################    PLOTS OF THE DATA    #############################################################

# TIME VERSUS SLA FOR THIS GRID - this is daily data

library(ggplot2)
# here we select the full time frame 1 May 1985 to 31 Dec 2020  - tm column is in POSIXct format
# breaks for each year and year labels as specified

PLOT1 <- ggplot(Hotspot_df, aes(tm, HOT))+
    geom_line(data=Hotspot_df, color="blue")+
    scale_x_datetime(name="", date_breaks = "5 year", date_minor_breaks = "1 year")+
    scale_y_continuous(name="Hotspot (deg C)", limits=c(-3,2.5))+
    geom_hline(yintercept=0)

PLOT1
# ------------------------------------------------------------------------------------------------------------------------

# If we want this plot with a black and white background - change the GGPLOT2 display theme to black and white 
theme_set(theme_bw())

# here we select the time frame 1 Jan 1993 to 7 Mar 2020 - using the "tm_days" column
# breaks for each year and year labels as specified

PLOT2 <- ggplot(Hotspot_df, aes(tm_days, HOT))+
  geom_line(data=Hotspot_df, color="blue")+
  scale_x_continuous(name = "", limits = c(12904, 25932), breaks = seq(12904, 26297, 365.25), labels=c("1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999"  ,"2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2103","2014","2015","2016","2017","2018","2019","2020","2021"),minor_breaks = NULL )  + 
  scale_y_continuous(name="Hotspot (deg C)", limits=c(-3,2.5))+
  geom_hline(yintercept=0)+
  geom_hline(yintercept = 1, colour="red")+
  geom_smooth(method='lm', colour="black")+ # regression line added
  theme(axis.text.x = element_text(angle=45, hjust=0.8))
PLOT2

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@  An exploratory OLS Regression of this daily data to see if there is a linear trend @@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

OLS_regress <- lm(HOT ~ tm_days, data=Hotspot_df, na.action = na.exclude)
summary(OLS_regress)
coefficients(OLS_regress) # prints intercept and slope
slope <- OLS_regress$coefficients["tm_days"]
slope_degC_yr <- slope*365.25 # converts slope in days to years and deg C by using the mean no of days every 4 yrs = 365.25
slope_degC_yr
# standard error of slope
se_slope<-summary(OLS_regress)$coef[[4]] 
se_slope
se_slope_degC <- se_slope*365.25
se_slope_degC
# CONFIDENCE INTERVALS 95% OF THE COEFFICIENTS
confintervals <- confint(OLS_regress)
confintervals2 <- confintervals*365.25
confintervals2

# we can inspect the residuals to see if these are autocorrelated (see other file for a more extensive discussion of autocorrelation): 
library(astsa)
# 'acf2' produces a simultaneous plot of both ACF and PACF one above each other - the zero lag value is also removed

# original SLA data
acf2(Hotspot_df$HOT, max.lag = 100, na.action= na.pass)

# residuals of the OLS to check that these have not been removed and therefore the assumptions of OLS are violated:
acf2(resid(OLS_regress), max.lag=100, na.action= na.pass)

# we can see that the residuals are serially correlated which is what we would expect for a time series such as this


# *************************************************************************************************************************
# *******************  NEED TO RECONFIGURE THE DATA INTO MONTHLY MEANS  *************************
# *************************************************************************************************************************
# for our model analysis with GAMs and GAMMs we will need to reconfigure the data.

# first create new columns containing Year and Month
Hotspot_df$year <- as.numeric(format(Hotspot_df$tm, "%Y")) # extracts Year from the date variable and adds in new column in the dataframe
Hotspot_df$month <- as.numeric(format(Hotspot_df$tm, "%m")) # ditto for month

Hotspot_df$week <- as.numeric(strftime(Hotspot_df$tm, format="%W")) # adds new column with week number

# Now create new dataframe with just monthly averaged values
month_Hotspot <- aggregate(HOT ~ month + year, Hotspot_df, mean) # this creates new dataframe with just 'month, year, HOT (mean)'

# if we want all the original columns as well as the grouping columns 'month & year' then:
month2_Hotspot <- aggregate(Hotspot_df, by=list(Hotspot_df$month, Hotspot_df$year), FUN=mean) # note that this averages each month time column (tm_days or date) according to the month length and centers the value at the mid month point.

# to create a weekly dataframe
# week_Hotspot <- aggregate(Hotspot_df, by=list(Hotspot_df$week, Hotspot_df$month, Hotspot_df$year), FUN=mean)

##  if we wanted to restrict the function to selected columns only then:
##  month3_SLA <- aggregate(subset_SingleGrid[,1:4], by=list(subset_SingleGrid$month, subset_SingleGrid$year), FUN=mean) # columns 1 to 4 only

# for a yearly averaged dataframe - we do not use this
# year_SLA <- aggregate(subset_SingleGrid, by=list(subset_SingleGrid$year), FUN=mean)


# ***************************************************************************************************************************
# ***********     NOW CREATE A DATFRAME CONTAINING OUR COVARIATES   ********************************************************* 
# *** this is monthly, DMI, Nino 3.4. From Excel File"DMI SOI Nino monthly to 14 Mar 2020.xlsx" (in my IOD-NNI-SOI folder) **
# ***************************************************************************************************************************

# use the method of inputing from a CSV file
DMI_df <- read.csv("DMI Nino 3.4 monthly May 1995 to Jul 2020.csv")

# DMI and Nino3.4 are already centered since they are anomalies.  

class(DMI_df$tm) # date ("tm") is a 'character' in this dataframe

# DMI and Nino3.4 are already centered since they are anomalies.   

# first we need to make sure that this DMI_df is the same length as the month_Hotspot_df. month_Hotspot_df has 5 extra rows at the end from Aug-Dec
# delete these
month2_Hotspot_del <- month2_Hotspot[-c(424,425,426,427,428),]

# bind DMI_df to our Hotspot data to create a new dataframe:
monthly_Hotspot <- data.frame(cbind(month2_Hotspot_del, DMI_df)) 
# drop some columns we don't need
monthly_Hotspot_drop <- subset(monthly_Hotspot, select = -c(1,2,3,4))

# now make a new dataframe with variables from the monthly_Hotspot_drop dataframe and add a numeric time column computed from the year and month columns which is centred in the middle of the month hence the -0.5 value in the formula
monthly_Hotspot_DMI <- transform(monthly_Hotspot_drop, time = year + (month - 0.5) / 12) 

# also add a month column (fmonth) that is a factor with 12 levels for later use in GAMM models. This is an ordered factor in the order 1 to 12 (see ?factor for more info on ordered and unordered factors)
monthly_Hotspot_DMI$fmonth <- factor(monthly_Hotspot_DMI$month, ordered = TRUE)

attributes(monthly_Hotspot_DMI)
str(monthly_Hotspot_DMI)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@  DATA EXPLORATION @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Any analysis should be preceded by data exploration. Key factors are outliers, collinearity (correlation among covariates), relationships between response and covariates, zero inflation, and sampling effort over time and space. These aspects can be visualized using simple graphs. Expect to spend at least 30% of your time on data exploration.[Zuur 2014 Guide to GAM]

# *****************************************************************************************************************
# ********************  ARE THERE ANY MISSING DATA? - which might cause problems  *********************************
# *****************************************************************************************************************

colSums(is.na(monthly_Hotspot_DMI)) 


# *****************************************************************************************************************
# *************************************  CHECK FOR OUTLIERS  ******************************************************
# *****************************************************************************************************************

#################################  Use Cleveland dotplots  ########################################################

# the functions from Zuur will be used below. All the routines can be found in <<source("HighstatLibV10.R")>> but we will just load the functions we need below:

# To determine whether there are observations that are considerably smaller or larger than the majority, Cleveland dotplots are made for each variable.

# ======================================================================================= 

# Function for multi-panel Cleveland dotplot (from Zuur 2012 Beginners guide to GAM).
# Note, the input file must contain no categorical variables

Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)),
               groups=FALSE,
               strip = strip.custom(bg = 'white',
                                    par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE),
                             y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data in the file", cex = 1.5))
  
  print(P)  
}
# =======================================================================================

library(lattice) # for the function dotplot
# initially display dotplots for all covariates that we might consider using - DMI, Nino 3.4
MyVar <- c("DMI", "month", "time","Nino_3.4")
Mydotplot(monthly_Hotspot_DMI[,MyVar])

# Each panel corresponds to a variable. The x-axes represent the values of the variables and the y-axes show the order of the data as imported from the data file.

###################################   Boxplots    ##################################################################

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
dat <- monthly_Hotspot_DMI %>% tibble::rownames_to_column(var="outlier") %>% group_by(fmonth) %>% mutate(is_outlier=ifelse(is_outlier(HOT), HOT, as.numeric(NA)))
# new column for label
dat <- dat %>% mutate(outlier1 = year)

# for all years when not outlier substitute NA so label will not plot below
dat$outlier1[which(is.na(dat$is_outlier))] <- as.numeric(NA)

# boxplot with outliers labelled by year when they occur
ggplot(dat, aes(y=HOT, x=factor(fmonth))) + geom_boxplot(outlier.colour = "blue") + geom_text(aes(label=outlier1),na.rm=TRUE,nudge_y=3)

######################################################################################
# ALTERNATIVE PLOTTING 
######################################################################################
library(ggstatsplot)
# citation("ggstatsplot")

# since the confidence intervals for the effect sizes are computed using bootstrapping, important to set a seed for reproducibility
set.seed(123)

ggbetweenstats(data = monthly_Hotspot_DMI, x = fmonth, y = HOT, outlier.tagging = TRUE, outlier.label = year, outlier.color = "blue", 
               package = "yarrr", # package from which color palette is to be taken
               palette = "info2", # choosing color palette
               pairwise.comparisons = FALSE,    # TRUE if you want to display the results of all pairwise comparisons above the plot
               pairwise.display = "significant", # only display the pairwise comparisons which are significantly different
               p.adjust.method = "holm",
               xlab ="Month", ylab = "Sea-Level Anomaly (mm)", title = "Hotspot by month")
               #, mean.ci=TRUE)  # this would add 95% CIs for the mean but makes plot a bit cluttered 

# ggstatsplot also reports the statistical test comparing the groups at the top of the plot
# it identifies and labels the group means by red dots
# other useful info is displayed e.g., n
# outlier.coef - Coefficient for outlier detection using Tukey’s method. With Tukey’s method, outliers are below (1st Quartile) or above (3rd Quartile) outlier.coef times the Inter-Quartile Range (IQR) (Default is: 1.5).

# OUTLIERS ARE LABELLED WITH THE YEAR IN WHICH THEY OCCURRED
# more info see: https://indrajeetpatil.github.io/ggstatsplot/


# if we need to remove some years, e.g. 2020 to obtain full years only
#df_1993_2019 <-  dplyr::filter(.data = monthly_SLA_DMI, year < 2020)
#ggbetweenstats(data = df_1993_2019, x = fmonth, y = SLA)


# ********************************************************************************************************************
# *****************************************  COLLINEARITY ************************************************************
# ********************************************************************************************************************

# =============================================================
# load function from Zuur et al  which will be used in the plot below
## source("HighstatLibV10.R")

# we will use the panel.cor function:
panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}
# ============================================================

# Collinearity is a condition in which some of the independent variables are highly correlated.

# Why is this a problem?  --- In this situation, the coefficient estimates of the multiple regression may change erratically in response to small changes in the model or the data. It does not reduce the predictive power or reliability of the model as a whole, at least within the sample data set; it only affects calculations regarding individual predictors. That is, a multivariate regression model with collinear predictors can indicate how well the entire bundle of predictors predicts the outcome variable, but it may not give valid results about any individual predictor, or about which predictors are redundant with respect to others.

# correlation increases the standard errors of estimated regression parameters and therefore inflates p-values compared to situations in which there is no collinearity (Montgomery and Peck 1992)

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  IDENTIFYING COLLINEARITY  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

######### (1) PAIR PLOTS WITH PEARSON CORRELATION COEFFICIENTS ##########

# Examine the correlation coefficient for each pair of independent variables. A value of the correlation near ±1 indicates that the two variables are highly correlated. 
  
MyVar <- c("HOT","DMI","fmonth","time", "Nino_3.4") # choose the variable we will compare
# although we are only concerned with correlation between explanatory variables we will include the response variable (SLA) just to get an idea of how the explanatory variables each relate to it.
pairs(monthly_Hotspot_DMI[,MyVar],lower.panel = panel.cor)
# correlations >0.8 are critical. 0.5-0.7 special care required

# ---------------------------------------------------------------------------------------------------------------

################ (2) VARIANCE INFLATION FACTORS (VIF) ################################################### 

# The variance inflation factors are also very useful. VIF(j) is the factor by which the variance of βˆj is increased over what it would be if xj was uncorrelated with the other independent variables. If all values of VIF(j) are near 1, then collinearity is not a problem. VIF(j) > 10 indicates serious collinearity.

MyVar <- c("DMI","month","time","Nino_3.4") # choose the variable we will compare

# From Ieno & Zuur 2012 - page 99 - "various options for VIF cut-off level in the literature. Higher than 10 indicates very strong collinearity (Belsley et al 1980; Quinn & Keough 2002). Others argue that values larger than 5 or even 3 might be considered quite detrimental to regressions models (Montgomery & Peck 1992). Our preferred limit is 5 or even 3. The level also depends on the strength of the relationships. If you have weak signals in your dataset, then collinearity may result in no explanatory variable being significant."

# ==========================================================================================

# use Highland Statistics function for computing VIF:
### source("HighstatLibV10.R")
#Support function for corvif. Will not be called by the user
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}

corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1 + rnorm(nrow(dataz)) ,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}

# ==========================================================================================

corvif(monthly_Hotspot_DMI[,MyVar])

# at cut-off level 3 - all variables are okay

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@  INITIAL LOOK AT THE RELATIONSHIPS BETWEEN INDIVIDUAL CONTINUOUS COVARIATES AND THE RESPONSE VARIABLE @@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# =========================================================================================================================

# Load Highland Statistics code "Myxyplot" to use below - this is a wrapper around the function xyplot - note I have slightly modified the original (eg grey boxes):

Myxyplot <- function(Z, MyV, NameY1, MyXlab = "Values of covariates", MyYlab="Sea-level anomaly (mm)") {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  
  library(mgcv)
  library(lattice)
  
  P <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(MyXlab, cex = 1),
              #ylab = list("Response variable", cex = 1.5),
              #ylab = list("Pearson residuals", cex = 1.5),
              ylab = list(MyYlab, cex = 1),
              #layout = c(2,2),   #Modify
              strip = function(bg='grey', ...)
                strip.default(bg='grey', ...),
              scales = list(alternating = TRUE,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              panel=function(x, y){
                panel.grid(h=-1, v= 2)
                panel.points(x, y, col = 1)
                panel.loess(x, y, span = 0.8,col = 1, lwd = 2)
              })
  
  print(P)
}
# ==================================================================================================================

# select the covariates
MyVar <- c("time", "DMI", "month","Nino_3.4")

# Use Multipanel scatterplots
Myxyplot(monthly_Hotspot_DMI, MyVar, "HOT")

# A LOESS smoother is added to aid visual interpretation.

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@   MODELLING THE DATA - using GAMs and GAMMs for non-linear trends @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# To call help on any aspect:
? smooth.terms
? missing.data
? gam.selection
? gam.models
? family.mgcv

# From the bottom of the heap - BLOG on GAMs - Gavin Simpson
# https//noamross.github/mgcv-esa-workshop/
# DataCamp course GAM in R. Noam Ross

# Generalized Additive Models (GAMs) offer a flexible approach to calculating trends and in particular, the mgcv package contains many functions that are very useful for such modelling. Some of the details of this type of model are presented in Wood (2017) and the mgcv package itself.

library(mgcv)
citation("mgcv")

# The starting point of the analysis is application of the model. Based on the underlying questions and the quality of the data as revealed by the data exploration, a model must be selected and applied. 

# When a model has been fitted, we need to know which parameters are significantly different from zero, and decide how to deal with any that are not.

# What if some of the estimated parameters are not significantly different from 0? There are several options, e.g.,

# 1. Leave the model as it is and state which term is/is not significantly different from 0 at the 5% level.
# 2. Apply a classical model selection using backward (or forward) selection using the Akaike Information Criteria (AIC) or related criteria. 
# 3. Apply a model selection based on hypothesis testing, i.e. drop the least significant terms. 
# 4. Apply a model selection on the interactions but not on the main terms. 
# 5. Adopt the Information Theoretic (IT) approach following Burnham and Anderson (2002). If this is done it should be decided upon before starting the              analysis!

# Once we have selected the optimal model we need to verify whether it complies with the underlying assumptions of the technique. If it does, it is time to unravel what it all means in terms of biology, and graphing tools will accomplish this. If assumptions are violated, we need to use that information to improve the model, or apply a different statistical model or technique.

## [see Zuur 2014 Guide to GAM]

# ------------------------------------------------------------------------------------------------------------------------------

# SMOOTHING METHOD
? gam.selection
# various methods can be chosen for the smoothing method. The default setting is GCV (Generalized Cross Validation Score) but it is widely recommended to use REML (restricted maximum likelihood) since this is most likely to give stable, reliable results. See Pedersen et al 2019 PeerJ "We strongly recommend using either REML or marginal likelihood (ML) rather than the default generalized cross-validation criteria when fitting GAMs, for the reasons outlined in Wood (2011)".

# ------------  SMOOTHING IN GAMs ------------------------------------------------

# Smooth functions are also called splines. Smoothing splines are real functions that are piecewise-defined by polynomial functions (basis functions). The places, where the polynomial pieces connect are called knots. In GAMs, penalized regression splines are used in order to regularize the smoothness of a spline.

# When run without a SMOOTHING PARAMETER (lambda) or BASIS FUNCTION inserted in the model - mgcv will work by automatic smoothing and select the appropriate values. If we want to control this then add these functions. 

# A SMOOTHING PARAMETER can be added for the entire model or for selected terms:
#    Examples:
#      M_time <- gam(SLA ~s(time), sp=0.1, data= monthly_SLA_DMI, method="REML") - WHOLE MODEL.
#      M_time <- gam(SLA ~s(time, sp=0.1), data= monthly_SLA_DMI, method="REML") - SELECTED TERM ONLY - other terms can have different values

# To extract the smoothing parameter from the model use -  M_time$sp

# ---------   PARAMETERS OF SMOOTH FUNCTIONS  -------------------:
? choose.k
#  k is the number of knots

# This parameter determines the upper bound of the number of underlying base functions being used to build up the curve. Thus, this parameter constraints the wigglyness of a smooth, or - as a metaphor - the number of bowpoints of a curve.

# Note that the model will base the number of base functions (reflected in the edf of the summary) on the data with the setting for k as upper bound. By default, the value of k for s() is around 9, and for te() and ti() 5 per dimension. Importantly, the value of k should be at most one less than the number of unique data points, otherwise it will fit the density of that predictor.

#         M_time <- gam(SLA ~s(time, sp=0.1, k=20), data= monthly_SLA_DMI, method="REML")

# gam.check can be used to check that the model terms have appropriate k values. 

# ---------------  TYPE OF BASIS FUNCTION  -------------------------------------------------------------------------
? smooth.terms

# bs: specifies the type of underlying base functions. For s() this defaults to "tp" (thin plate regression spline) and for te() and ti() this defaults to "cr" (cubic regression spline). For random intercepts and linear random slopes use bs="re", but for random smooths use bs="fs".

#   bs="tp" - the default - thin plate regression spline
#   bs="so" - soap films
#   bs="gp" - gaussian process - good way to represent autocorrelated data
#   bs="cc" - cyclic cubic regression spline - a cyclic smooth for seasonal data which joins the beginning and end of each cycle, e.g., s(month, bs="cc")
#   bs="ad" - adaptive smooths - use where data is flat in places and wiggly elsewhere

# LINEAR TERMS IN GAM
# In practice it is unnecessary to use linear terms in GAM because the automatic smoothing will correctly identify linear terms where there is enough data. To force linear terms use a strong smoothing, e.g., sp=1000 . However linear terms are very useful for categorical terms - eg., male/female etc.NOTE: these must be stored as 'factors' in R - mgcv does not use categorical variables

# --------------------------  NOTE ON CATEGORICAL TERMS ------------------------------------------------------------

# We can use mgcv to examine models for each categorical term separately using the "by" function, e.g.:

#          model_4 <- gam(SLA ~ s(time, by = fmonth), data = monthly_SLA_DMI, method = "REML")
#          summary(model_4)

# in models such as above we normally include separate terms for a varying intercept in case the different factors are different in overall means in addition to the shape of the smooth - adding this will improve estimates of the smooth: 

#          model_4b <- gam(SLA ~ s(time, by = fmonth) + fmonth, data = monthly_SLA_DMI, method = "REML")

# -------------------------------------------------------------------------------------------------------------------

# ----------------  RANDOM EFFECTS  ------------------------------------------------------------------------------

# Three different types of random effects are distinguished when using GAMMs:

#   (1) random intercepts adjust the height of other model terms with a constant value: s(Subject, bs="re").
#   (2) random slopes adjust the slope of the trend of a numeric predictor: s(Subject, Time, bs="re").
#   (3) random smooths adjust the trend of a numeric predictor in a nonlinear way: s(Time, Subject, bs="fs", m=1).
#Notes:
#       Random intercepts and random slopes could be combined, but the random smooths already include random intercepts and random slope effects.
#       The argument m=1 sets a heavier penalty for the smooth moving away from 0, causing shrinkage to the mean.

# ----------------------------------------------------------------------------------------------------------------

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@  START BY INCLUDING AN INTERACTION TERM @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# the interaction which we might expect in this data is between time and month, ie that any seasonality in the data (identified by the month term) changes between years.

# READ: 
#       http://jacolienvanrij.com/Tutorials/GAMM.html 
#       https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/

# ----------------  INTERACTIONS:  -------------------------------------------------------------------------

# interactions are a very important part of the regression model for double seasonal time series. With GAMs there are four main possibilities, how to include them to the model. 
#   (1) the most basic, like in MLR, the multiplication of two independent variables: x1 x x2. 
#   (2) is possibility to use smoothed function to one variable: f1(x1) x x2
#   (3) use same smoothed function for both variables: f1(x1) x f1(x2) - often denoted f(x1,x2)
#   (4) the most complex, with GAM it is possible to use tensor product interactions. So it is possible to use different smoothing bases for variables and penalize it in two (when we do interactions of two independent variables) different ways:  f1(x1) x f2(x2) - This allows for an overall anisotropic (different in each direction) penalty, so the overall shape of a tensor product smooth is invariant to a rescaling of its independent variables. This is a huge advantage in the comparison to usage of one smoothing function. Simply said, we have theoretically supported that it’s allowed to use different metrics of variables in the interactions term.

# Thus, if we want a model which allows for the seasonal amplitude and/or phase to change over time, i.e. that there is an interaction between them, we need to include an appropriate term.

# ---------------   CHOICE OF te OR ti FUNCTION SMOOTHS  -------------------------------------------------

# To model a potentially nonlinear smooth or surface, three different smooth functions are available:
#   (1)     s() : for modeling a 1-dimensional smooth, or for modeling isotropic interactions (variables are measured in same units and on same scale).

#   (2)     te(): for modeling 2- or n-dimensional interaction surfaces of variables that are not isotropic [ie have different scales e.g. time in years and months] (but see info about d parameter below). Includes ‘main’ effects.Thus te produces a 'full tensor product smooth'.

# FROM HELP FILE -  (? smooth.terms):   te smooths have one penalty per marginal basis, each of which is interpretable in a similar way to the marginal penalty from which it is derived. See Wood (2006b).

#   (3)     ti(): for modeling 2- or n-dimensional interaction surfaces that do not include the ‘main effects’. If the main effects are also required in the model then these are specified by separate terms.

# FROM HELP FILE -  (? smooth.terms):   ti smooths exclude the basis functions associated with the ‘main effects’ of the marginal smooths, plus interactions other than the highest order specified. These provide a stable and interpretable way of specifying models with main effects and interactions. For example if we are interested in linear predictors  f1(x) + f2(z) + f3(x,z), we might use model formula y~s(x)+s(z)+ti(x,z) or y~ti(x)+ti(z)+ti(x,z). A similar construction involving te terms instead will be much less statistically stable.

# ----------------------------------------------
# The functions do not evaluate the smooth - they exists purely to help set up a model using tensor product based smooths. Designed to construct tensor products from any marginal smooths with a basis-penalty representation (with the restriction that each marginal smooth must have only one penalty).


# -------------------------  PARAMETER 'd' in te or ti smooth  -------------------------------------------------
# d : for specifying that predictors in the interaction are on the same scale or dimension - only used in te() and ti(). For example, in te(Time, width, height, d=c(1,2)), with width and height reflecting the picture size measured in pixels, we specify that Time is on a different dimension than the next two variables. By default, the value would be d=c(1,1,1) in this case.
# --------------------------------------------------------------------------------------------------------------

# It is sometimes useful to investigate smooth models with a main-effects + interactions structure, for example:
#               f_1(x) + f_2(z) + f_3(x,z)

# This functional ANOVA decomposition is supported by ti terms, which produce tensor product interactions from which the main effects have been excluded, under the assumption that they will be included separately. For example the ~ ti(x) + ti(z) + ti(x,z) would produce the above main effects + interaction structure. This is much better than attempting the same thing with tensor te terms representing the interactions (although mgcv does not forbid it).

# Technically ti terms are very simple: they simply construct tensor product bases from marginal smooths to which identifiability constraints (usually sum-to-zero) have already been applied: correct nesting is then automatic (as with all interactions in a GLM framework). See Wood (2017, section 5.6.3).

###########  CONSTRUCT OUR MODEL WITH INTERACTION TERM  ###################################

# we want to decompose the effects so:
#   also make this AR(1) model because the simple time series is serially autocorrelated
#   finally specify in interaction term that different smooth terms apply to time (thin plate regression splines [the default]) and month (cyclic cubic regression splines [because we want to join the end and beginning of each cycle])

# we will use a tensor product smooth (te) or (ti) [rather than isotropic smooth (s)] and the reason for doing this is that the time (e.g. units 1983.25) and seasonal components (e.g. units 1 to 12) are on different scales and we want to apply the same level of smoothness to both.

# this model produces an error:
# Error in solve.default(as.matrix(a)) : system is computationally singular: reciprocal condition number = XXXXX 
# M3a <- gamm(SLA ~  ti(time, month, bs=c("tp","cc")) + ti(DMI) + ti(time) + ti(Nino3.4) + ti(month, bs = "cc"), data = monthly_SLA_DMI, correlation = corARMA(form = ~ month | year, p=1), method="REML", knots=list(month=c(0,12)))
# specifying bs=("tp","cc") in the interaction term makes the matrix non-invertable. This results from linearly dependent columns, i.e. strongly correlated variables which time and month are. 

# Remove the bs for the interaction
M3a <- gamm(HOT ~  ti(time, month) + ti(DMI) + ti(time) + ti(Nino_3.4) + ti(month, bs = "cc"), data = monthly_Hotspot_DMI, correlation = corARMA(form = ~ month | year, p=1), method="REML")

# , knots=list(month=c(0,12)) 

summary(M3a$gam)
coef(M3a$gam)
acf(residuals(M3a$lme, type = "normalized")) 
pacf(residuals(M3a$lme, type = "normalized"))

plot(M3a$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(M3a$gam)[1], shade=TRUE, pages=1)

# if interaction is not significant then continue with fitting models without interaction term.

# ----------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------

# USING GAMM TO ACCOUNT FOR THE AUTOCORRELATION

# GAMM combines "mgcv" with "lme" to model complex random effects in the model

# using a GAMM (Generalized Additive Mixed Model) it is possible to model the short-term autocorrelation using a linear mixed model. The gamm function uses the package nlme and the Generalized Linear Mixed Model (GLMM) fitting routine. 

# It is a mixed model because we have our 'fixed' effects (time, month, DMI, Nino3.4) which we expect to have an effect on all the data, and a 'random' effect where we model the autocorrelation

# the model M2 below assumes that the time and seasonal terms vary independently of one another and that the correlation term indicates that the variable determining the ordering of residuals is 'month' and that the correlation applies within measurements made on one year. In other words month is nested within year. As a result this does not consider residual variation from year to year.

# other ways could be "correlation = corARMA(form = ~ 1|year, p = 1)". The "form = ~1|year" means that the ARMA is nested within each year. This means that the same residual correlation structure is assumed each year - this shown by only one value for phi in the $lme component of the model. This speeds up fitting no end, but is potentially risky as we do not consider residual variation from year to year. If you want longer-term dependencies then you need to fit the entire correlation matrix, by not nesting it within year, but this can slow down fitting a lot - the non-nested model term would be "(form = ~ time)" but this gives an error message "Error in chol.default(V$V) : the leading minor of order 2 is not positive definite".

# Make the AR term with month nested within year

# this model has FIXED EFFECTS - smoothed 'time', 'DMI', 'month', and Nino3.4.
M2 <- gamm(HOT ~  s(time) + s(DMI) + s(month, bs = "cc") + s(Nino_3.4), data = monthly_Hotspot_DMI, correlation = corAR1(form = ~ month | year), method="REML", select=FALSE, knots=list(month=c(0,12)), control = lmeControl(opt = 'optim', msVerbose = TRUE))

# the knots define the start and end for the cyclic smoother for month (bs="cc"). We use 0 and 12 because the first (Jan) and last (Dec) month should not have exactly the same expected value. 

summary(M2$gam)
summary(M2$lme)
M2
# the adj r squared is 73.4% 

acf(residuals(M2$lme, type = "normalized"))
pacf (residuals(M2$lme, type = "normalized"))
# no autocorrelation remains

plot(M2$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(M2$gam)[1], shade=TRUE, pages=1)

# Use gam.check() to make sure we have a well fitting model

# (1) Convergence - we want full convergence. Failure to converge can happen when there are too many terms for insufficient data.
# (2) K' value = number of basis functions. 
# (3) small p values for a term indicate that the residuals are not randomly distributed and ofetn means that there are not enough basis functions in the        model term. If this is the case then refit the model with a higher k. However fixing one problem might reveal another.
# (4) PLOTS. QQ should follow a straight line. Histogram should be a bell shaped normal distribution. Residuals vs linear predictor should be evenly            distributed around 0. Response vs fitted - a perfect model is a straight line relationship. Expect the points to cluster around the 1:1 line.
gam.check(M2$gam)

# Estimated phi (ϕ) coefficient of AR(1) process can be seen here:
M2[["lme"]][["modelStruct"]][["corStruct"]]

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# DMI is not significant so remove this term:
M2_no_DMI <- gamm(HOT ~  s(time) + s(month, bs = "cc") + s(Nino_3.4), data = monthly_Hotspot_DMI, correlation = corAR1(form = ~ month | year), method="REML", select=FALSE, knots=list(month=c(0,12)), control = lmeControl(opt = 'optim', msVerbose = TRUE))

# the knots define the start and end for the cyclic smoother for month (bs="cc"). We use 0 and 12 because the first (Jan) and last (Dec) month should not have exactly the same expected value. 

summary(M2_no_DMI$gam)
summary(M2_no_DMI$lme)
M2_no_DMI

acf(residuals(M2_no_DMI$lme, type = "normalized"))
pacf (residuals(M2_no_DMI$lme, type = "normalized"))
# no autocorrelation remains

plot(M2_no_DMI$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(M2_no_DMI$gam)[1], shade=TRUE, pages=1)

# Use gam.check() to make sure we have a well fitting model

# (1) Convergence - we want full convergence. Failure to converge can happen when there are too many terms for insufficient data.
# (2) K' value = number of basis functions. 
# (3) small p values for a term indicate that the residuals are not randomly distributed and ofetn means that there are not enough basis functions in the        model term. If this is the case then refit the model with a higher k. However fixing one problem might reveal another.
# (4) PLOTS. QQ should follow a straight line. Histogram should be a bell shaped normal distribution. Residuals vs linear predictor should be evenly            distributed around 0. Response vs fitted - a perfect model is a straight line relationship. Expect the points to cluster around the 1:1 line.
gam.check(M2_no_DMI$gam)

# Estimated phi (ϕ) coefficient of AR(1) process can be seen here:
M2_no_DMI[["lme"]][["modelStruct"]][["corStruct"]]

# ----------------------------------------------------------------------

#####  A MODEL WITH MONTH AS A FACTOR     #######################

# this model has FIXED EFFECTS - smoothed 'time', 'DMI', Nino3.4, and 'fmonth' as a factor.
# using month as a factor means that the model produces separate intercepts for each of the 12 months

M2aa <- gamm(HOT ~  s(time) + s(DMI) + s(Nino_3.4) + fmonth, data = monthly_Hotspot_DMI, correlation = corAR1(form = ~ month | year), method="REML", select=FALSE, control = lmeControl(opt = 'optim', msVerbose = TRUE))
summary(M2aa$gam)
summary(M2aa$lme)
anova(M2aa$gam)       # to check whether the overall month term is significant
M2aa
# the adj r squared is % 

acf(residuals(M2aa$lme, type = "normalized"))
pacf (residuals(M2aa$lme, type = "normalized"))
# autocorrelation removed

# plot the smoothed terms
plot(M2aa$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(M2aa$gam)[1], shade=TRUE, pages=1)
# include the fmonth factor term in the plot
plot(M2aa$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(M2aa$gam)[1], shade=TRUE, pages=1, all.terms=TRUE)

gam.check(M2aa$gam)

# this version for the AR() term provides a small improvement in the AICc value - see MUMin comparison - WHY?
M2aa_1 <- gamm(HOT ~  s(time) + s(DMI) + s(Nino_3.4) + fmonth, data = monthly_Hotspot_DMI, correlation = corAR1(), method="REML", select=FALSE)
summary(M2aa_1$gam)
# and R2 adj is % 

#######################################################################
#### MONTH AS FACTOR and using 12 variances, one per month #########

# we will see later in the model fit diagnostics that M2 had left a pattern in the residuals when plotted against the covariate month using this revised model removes the pattern.

# we have to add a lmeControl of 'optim' rather than the dafault of 'nlminb' here to get output

M2x <- gamm(HOT ~  s(time) + s(DMI) + s(Nino_3.4) + fmonth, data = monthly_Hotspot_DMI,  weights = varIdent(form=~1|fmonth) ,correlation = corARMA(form = ~ month | year, p=1), method="REML", select=FALSE, control = lmeControl(opt = 'optim', msVerbose = TRUE))

summary(M2x$gam)
summary(M2x$lme)
anova(M2x$gam) # this is how we extract the month effect which is split up between the months in the Summary 
M2x
# the adj r squared is now % ie more of the variation is explained by the model compared to the M2aa model above
acf(residuals(M2x$lme, type = "normalized"))
pacf (residuals(M2x$lme, type = "normalized"))
# all autocorrelation removed

plot(M2x$gam, residuals=TRUE, pch=1, cex=1, rug=TRUE, se=TRUE, seWithMean = TRUE, shift = coef(M2x$gam)[1], shade=TRUE, pages=1, all.terms=TRUE)
gam.check(M2x$gam)

# compare our models to see which is the better fit:
anova(M2$lme,M2aa$lme,M2x$lme)
anova(M2aa$lme,M2x$lme)
anova(M2$lme,M2x$lme)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@ Use MUMIn package to select best model based on lowest AIC  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

require(MuMIn)

# use the mod.sel function to conduct model selection
# and put output into object out.put

out.put<-model.sel(M2$lme,M2aa$lme,M2x$lme,M2_no_DMI$lme)
out.put

# coerce the object out.put into a data frame
# elements 1-7 in out.put have what we want
sel.table<-as.data.frame(out.put)[1:7]
sel.table

# This is a bit messy and not ready for any report. Let’s clean this up a bit -- first by rounding.
# a little clean-up, lets round things a bit
sel.table[,4:5]<- round(sel.table[,4:5],2)
sel.table[,6:7]<- round(sel.table[,6:7],3)
sel.table

# AIC and R2 can be extracted from models using e.g.
summary(M2aa$lme)$AIC            # AIC
summary(M2aa$gam)$r.sq           # adjusted r squared

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@  WHAT IS THE CONTRIBUTION OF INDIVIDUAL TERMS TO THE VARIATION IN THE DATA EXPLAINED BY THE MODEL?   @@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# see the post on "variance explained by each term in a GAM" - https://stat.ethz.ch/pipermail/r-help/2007-October/142811.html
# answered by Simon Wood (mgcv package creator)

# "You should use the same smoothing parameters throughout. i.e. the reduced models should use the same smoothing parameters as the full model. Otherwise you get in trouble if terms are correlated, since the smoothing parameters will then tend to change a lot when terms are dropped as one smooth tries to `do the work' of the other". 

# BEST MODEL WAS: M2_no_DMI

# display and check the smoothing parameters used by the above model for each smoothed term - they will be used below in the reduced models
M2_no_DMI$gam$sp[1]

# reduced models note GAMM cannot use sp so it is just ignored in this code so disabled
b1 <- gamm(HOT ~ s(time), sp=M2_no_DMI$gam$sp[1], data = monthly_Hotspot_DMI, correlation = corAR1(form = ~ month | year), method="REML", select=FALSE)

# note that because DMI and Nino 3.4 are linear terms the functions below for GAMM won't run when the smoothing and smoothing parameter are removed
#b2 <- gamm(SLA ~ s(DMI), sp=M2aa$gam$sp[2], data = monthly_SLA_DMI, correlation = corAR1(form = ~ month | year), method="REML", select=FALSE)
#b3 <- gamm(SLA ~ s(Nino3.4), sp=M2aa$gam$sp[3], data = monthly_SLA_DMI, correlation = corAR1(form = ~ month | year), method="REML", select=FALSE)
#b4 <- gamm(SLA ~ s(time) + fmonth, sp=M2aa$gam$sp[1], data = monthly_SLA_DMI, correlation = corAR1(form = ~ month | year), method="REML", select=FALSE)

# extract contribution to r squared from these models
summary(M2_no_DMI$gam)$r.sq # full model
summary(b1$gam)$r.sq   # time

#summary(b2$gam)$r.sq   # DMI
#summary(b3$gam)$r.sq   # Nino3.4
#summary(b4$gam)$r.sq   # time and fmonth

# Instead use GAMs to fix the smoothing parameters and use the resulting R2 to describe the contribution of each term 
b1s <- gam(HOT ~ s(time, sp=M2_no_DMI$gam$sp[1]), data = monthly_Hotspot_DMI, method="REML", select=FALSE) # time only
summary(b1s)$r.sq
# b2s <- gam(HOT ~ DMI, data = monthly_Hotspot_DMI, method="REML", select=FALSE)  # DMI only
# summary(b2s)$r.sq
b3s <- gam(HOT ~ Nino_3.4, data = monthly_Hotspot_DMI, method="REML", select=FALSE) # Nino3.4 only
summary(b3s)$r.sq
b4s <- gam(HOT ~ s(time, sp=M2_no_DMI$gam$sp[1]) + fmonth, data = monthly_Hotspot_DMI, method="REML", select=FALSE) # time and fmonth
summary(b4s)$r.sq
b5s <- gam(HOT ~ s(month), data = monthly_Hotspot_DMI, method="REML", select=FALSE) # month only
summary(b5s)$r.sq


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@  EXTRACTING INDIVIDUAL MODEL TERMS AND PLOTTING THEM @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# The fitted GAM model object contains a lot of information that can be used to interrogate the model. 

# GAVIN SIMPSON https://fromthebottomoftheheap.net/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/
# and Wood (2017) pages 339

library(Hmisc) # for specifying minor axes ticks on the plots we will use.

# SPECIFY A NEW RANGE AND FREQUENCY OF SAMPLING FROM THE ORIGINAL DATA OVER WHICH WE MAKE PREDICTIONS 
# The code below selects XXX evenly-spaced values over the range of the data.
want <- seq(1, nrow(monthly_Hotspot_DMI), length.out = 423)                # gets all monthly data points

# ------------------------------------------------------------------------------------------------
# ------------------------- FOR M2_no_DMI MODEL -------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# model was :
M2_no_DMI <- gamm(HOT ~  s(time) + s(month, bs = "cc") + s(Nino_3.4), data = monthly_Hotspot_DMI, correlation = corAR1(form = ~ month | year), method="REML", select=FALSE, knots=list(month=c(0,12)), control = lmeControl(opt = 'optim', msVerbose = TRUE))

# we need all the predictor variables used in the model because these are required by the model for computing the predictions.
pdat <- with(monthly_Hotspot_DMI, data.frame(time = time[want], month = month[want], Nino_3.4=Nino_3.4[want])) 

# LINEAR PREDICTORS
# type="link" is the default - returns predictions (and SE) on the linear predictor scale, ie these are partial plots for the selected term with other terms held at their median

# this computes the overall model fit predictions for the M2_no_DMI model, ie all the terms added together + the parametric intercept
Model.fitM2 <- predict(M2_no_DMI$gam, newdata = pdat, type="link") 

## NOW PREDICT EACH TERM IN THE MODEL SEPARATELY
##   returns linear predictor scale predictions (and SE) split up by term

#    The terms in our model were: s(DMI), s(time), s(month) and s(Nino_3.4) in that order.
p2  <- predict(M2_no_DMI$gam,  newdata = pdat, type = "terms", se.fit = TRUE) 

# this will tell us what the order of predictions is:
M2_no_DMI[["gam"]][["pred.formula"]]

# DOUBLE CHECK the output of p2 IS AS EXPECTED by printing to the console so that we can see what order each fitted term is in:
p2[["fit"]]

# here the output is a dataframe which contains both "fit" and the standard error "se.fit" for each term, hence we can extract these and add them into out dataframe "pdat"

# for M2_no_DMI model 
pdat <- transform(pdat, fit.time = p2$fit[,1], se.fit.time= p2$se.fit[,1], fit.month = p2$fit[,2], se.fit.month= p2$se.fit[,2], fit.Nino_3.4 = p2$fit[,3], se.fit.Nino_3.4= p2$se.fit[,3])

# also add the overall model predictions to the dataframe:
pdat <- transform(pdat, Model.fit = Model.fitM2)

# create new column with (Nino_3.4 + time) predictions added for later plot
pdat <- transform(pdat, Nino_time = fit.Nino_3.4 + fit.time)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now plot the original data so that we can add plots for the overall model and/or each of the terms computed below
plot(HOT ~ time, data = monthly_Hotspot_DMI, type = "p", ylab = "Hotspot (deg C)", xlab="") # plot the original data
minor.tick(nx=5, tick.ratio=0.8) # add minor ticks on x axis - nx=5 means 5 ticks between the major marks
#minor.tick(ny=1, tick.ratio=0.8) # add minor ticks on y axis
abline(h=c(0), col="dark grey") # horizontal reference line at 0
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# and plot these:
# lines(fit.DMI ~ time, data = pdat, col = "blue")          # DMI
lines(fit.month ~ time, data = pdat, col = "purple")     # month
lines(fit.Nino_3.4 ~ time, data = pdat, col="green")       # Nino 3.4
lines(Model.fit ~ time, data = pdat, col = "red")         # the overall model
lines(Nino_time ~ time, data = pdat, col="red")         # the combined values of time + Nino3.4 predictions


lines(fit.time ~ time, data = pdat, col = "black", lwd=2) # time
# add approx 95% CI for time linear trend as dotted lines
lines(pdat$time,(pdat$fit.time+1.96*pdat$se.fit.time),col="black", lty=2, lwd=1)
lines(pdat$time,(pdat$fit.time-1.96*pdat$se.fit.time),col="black", lty=2, lwd=1)

#########  SAVE PDAT TO A CSV   #######################################
#write.csv(pdat, "Phuket Grid 77 M2 model & term predicted values.csv")


###################################################################################################
####################     using GGPLOT2 for more control     #######################################
###################################################################################################

library(ggplot2)
library(scales) # helps to label and scale axes

# set up basic plotting 
gX <- ggplot(data=monthly_Hotspot_DMI, aes(x= time)) + 
  scale_x_continuous(name = "", limits = c(1985,2021), breaks = seq(1985,2021,1), labels=c("1985","","1987","","1989","","1991","","1993","","1995","","1997","","1999","","2001","","2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017","","2019","","2021"))  + 
  scale_y_continuous(limits=c(-3,2.5))+
  labs(y=expression(Hotspot~" "^{o}~C)) +
  theme_bw() +
  theme(plot.margin=unit(c(4,3,4,0), "cm")) +     # plot margins are in the order top, right, bottom, left                         
  theme(axis.text.x = element_text(angle = 0, hjust= 0, vjust = 0)) + # adjust the position of the X axis tick labels
  theme(panel.grid.minor = element_blank()) + # remove the minor grid lines vertically
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) # makes axis text black
gX

# add rectangles to mark ENSO
# these rectangles have start and end dates for when the NINO3.4 was above the standard deviation value computed above for several weeks. The decimal date information entered below was extracted from the weekly NINO3.4 file ("NINO3.4 weekly to 13 Jan 2021.xlsx") in the folder E:\Phuket Databases\IOD-NNI-SOI\NINO3.4\State of the Ocean Climate. For Nino 3.4 the file is "El Nino 3.4 weekly 1981 to 15 Jul 2020.xlsx" and the level used was >0.5 deg C.
# fill=colour of rectangle, size=border width, colour=border col
# ENSO only is lightcyan

gX1 <- gX +
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=1986.7095890411,xmax=1988.07103825137,ymin=-3,ymax=2.5),colour="white",alpha=0.2,fill="cyan",size=0.5) + 
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=1991.38630136986,xmax=1992.45901639344,ymin=-3,ymax=2.5),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=1993.22465753425,xmax=1993.45479452055,ymin=-2.5,ymax=3.5),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=1994.7397260274,xmax=1995.14246575342,ymin=-3,ymax=2.5),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=1997.32602739726,xmax=1998.32328767123,ymin=-3,ymax=2.5),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=2002.42465753425,xmax=2003.15342465753,ymin=-3,ymax=2.5),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=2004.59016393443,xmax=2004.99180327869,ymin=-3,ymax=2.5),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=2006.69863013699,xmax=2007.02465753425,ymin=-3,ymax=2.5),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=2009.47671232877,xmax=2010.26301369863,ymin=-3,ymax=2.5),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=2015.15068493151,xmax=2016.33879781421,ymin=-3,ymax=2.5),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=monthly_Hotspot_DMI, aes(NULL,NULL,xmin=2018.73424657534,xmax=2019.48219178082,ymin=-3,ymax=2.5),colour="white",alpha=0.2,fill="cyan",size=0.5)

gX1

# add the monthly HOT data points
gX2 <- gX1 + geom_point(aes(y=HOT))
gX2

# LET US CHECK THAT THE SUM OF EACH SEPARATE TERM IS THE SAME AS THE OVERALL MODEL PREDICTIONS (Model.fit) TOGETHER WITH THE INTERCEPT
# in the dataframe add each of the separate term fits together and make new column:
# models with no interaction term
pdat$no.intercept.fit <- pdat$fit.time + pdat$fit.month + pdat$fit.Nino_3.4

# in the dataframe subtract no.intercept.fit from p.fit - this should equal our intercept value which was produced by the model summary:
pdat$intercept <- pdat$Model.fit - pdat$no.intercept.fit
# it does!

###### ADD INTERCEPTS TO EACH MODEL FIT TERM FOR PLOTTING HERE AT THE CORRECT POSITION   ########
# DMI term:
# pdat$DMI_term <- pdat$fit.DMI + pdat$intercept

# time term
pdat$time_term <- pdat$fit.time + pdat$intercept

# Nino 3.4 term
pdat$Nino_3.4_term <- pdat$fit.Nino_3.4 + pdat$intercept

# Month term
pdat$Month_term <- pdat$fit.month + pdat$intercept

# Time and Nino 3.4 terms combined
pdat$Time_Nino_3.4 <- pdat$fit.time + pdat$fit.Nino_3.4 + pdat$intercept

##################################################################################################

# add the Model.fit
gX2 + geom_line(aes(y=pdat$Model.fit)) + ggtitle("Overall Model Fit")  

# add the DMI term only
#gX2 + geom_line(aes(y=pdat$DMI_term)) + ggtitle("DMI Term")   

# add the time term only
gX2 + geom_line(aes(y=pdat$time_term)) +ggtitle("Time Term")   

# add the Nino 3.4 term only
gX2 + geom_line(aes(y=pdat$Nino_3.4_term), colour="red", size=1)+ ggtitle("Nino 3.4 Term") 

# add the Month term only
gX2 + geom_line(aes(y=pdat$Month_term),colour="purple", size=1)+ ggtitle("Month (seasonality) Term") 

# add the Time and Nino 3.4 terms combined 
gX2 + geom_line(aes(y=pdat$Time_Nino_3.4),colour="black", size=1)+ ggtitle("Nino 3.4 and Time Combined") 

# add the Time and Nino 3.4 terms combined - no title
gX2 + geom_line(aes(y=pdat$Time_Nino_3.4),colour="black", size=1)+ ggtitle("") 


# all terms 
gX2 + geom_line(aes(y=pdat$Model.fit), colour="black", size=1) + 
  #geom_line(aes(y=pdat$DMI_term), colour="red") + 
  geom_line(aes(y=pdat$time_term), colour="grey") + 
  geom_line(aes(y=pdat$Nino_3.4_term), colour="blue") +  
  annotate("text", x = 1996.5, y = 240, label = "Full Model") + 
  #annotate("text", x = 1995.8, y = 220, label = "DMI")+ 
  annotate("text", x = 1995.9, y = 200, label = "Time")+ 
  annotate("text", x = 1996.3, y = 180, label = "Nino 3.4") + 
  annotate("segment", x = 1994, xend = 1995, y = 240, yend = 240, colour = "black", size=1) +
  annotate("segment", x = 1994, xend = 1995, y = 220, yend = 220, colour = "red") +
  annotate("segment", x = 1994, xend = 1995, y = 200, yend = 200, colour = "grey") +
  annotate("segment", x = 1994, xend = 1995, y = 180, yend = 180, colour = "blue") 

# just Nino 3.4 term
gX2 + 
  #geom_line(aes(y=pdat$Model.fit), colour="black", size=1) + 
  #geom_line(aes(y=pdat$DMI_term), colour="red", size=1) + 
  #geom_line(aes(y=pdat$time_term), colour="grey") + 
  geom_line(aes(y=pdat$Nino_3.4_term), colour="blue", size=1) +  
  #annotate("text", x = 1996.5, y = 240, label = "Full Model") + 
  annotate("text", x = 1995.8, y = 240, label = "DMI")+ 
  #annotate("text", x = 1995.9, y = 200, label = "Time")+ 
  annotate("text", x = 1996.3, y = 220, label = "Nino 3.4") + 
  #annotate("segment", x = 1994, xend = 1995, y = 240, yend = 240, colour = "black", size=1) +
  annotate("segment", x = 1994, xend = 1995, y = 240, yend = 240, colour = "red", size=1) +
  #annotate("segment", x = 1994, xend = 1995, y = 200, yend = 200, colour = "grey") +
  annotate("segment", x = 1994, xend = 1995, y = 220, yend = 220, colour = "blue", size=1) 

# plot DMI and Nino3.4 combined
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
geom_line(aes(y=pdat$DMI_Nino_3.4), colour="black", size=1)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@  CHECK THE MODELS  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# MODEL CHECKING WITH gam.check(): Now that we can fit and plot GAMs, we need some checks to make sure that we have well-fit models. There are several pitfalls we need to look out for when fitting GAMs. Thankfully, mgcv provides helpful tools to check for these.

# 2.5.1 READING MODEL DIAGNOSTICS: gam.check() helps you understand whether you have enough basis functions to model the data.

# (1) Print diagnostics on model (mod) basis size and plots of model residuals

gam.check(M2_no_DMI$gam)

# CHECKING CONCURVITY: Now we'll learn about another area that's important to check in GAMs: concurvity. This function produces summary measures of concurvity between GAMM components.

# Concurvity occurs when some smooth term in a model could be approximated by one or more of the other smooth terms in the model. This is often the case when a  smooth of space is included in a model, along with smooths of other covariates that also vary more or less smoothly in space. Similarly it tends to be an issue in models including a smooth of time, along with smooths of other time varying covariates.

# Concurvity can be viewed as a generalization of co-linearity in linear models (ie when two variables are correlated), and causes similar problems of interpretation - namely giving poorly fitted models with lareg CIs. It can also make estimates somewhat unstable (so that they become sensitive to apparently innocuous modelling details, for example). Even if 2 variables in GAM are not colinear they may have concurvity - one may be a smooth curve of another.

# This routine computes three related indices of concurvity, all bounded between 0 and 1, with 0 indicating no problem, and 1 indicating total lack of identifiability. The three indices are all based on the idea that a smooth term, f, in the model can be decomposed into a part, g, that lies entirely in the space of one or more other terms in the model, and a remainder part that is completely within the term's own space. If g makes up a large part of f then there is a concurvity problem. The indices used are all based on the square of ||g||/||f||, that is the ratio of the squared Euclidean norms of the vectors of f and g evaluated at the observed covariate values.

# The three measures are as follows:

# worst - This is the largest value that the square of ||g||/||f|| could take for any coefficient vector. This is a fairly pessimistic measure, as it looks at the worst case irrespective of data. This is the only measure that is symmetric.

# observed - This just returns the value of the square of ||g||/||f|| according to the estimated coefficients. This could be a bit over-optimistic about the  potential for a problem in some cases.

# estimate - This is the squared F-norm of the basis for g divided by the F-norm of the basis for f. It is a measure of the extent to which the f basis can be explained by the g basis. It does not suffer from the pessimism or potential for over-optimism of the previous two measures, but is less easy to understand.

# First Check overall concurvity
concurvity(M2_no_DMI$gam, full = TRUE) # If TRUE then go on to check concurvity of each term with the whole of the rest of the model. Always look at the worst case and then if concurvity is say >0.8 check the model.

# pairwise concurvity between model variables. Use to find which 2 variables have a close relationship. Do these variable have problematic shapes or large CIs?
concurvity(M2_no_DMI$gam, full = FALSE)  # If FALSE then pairwise concurvity measures between each smooth term (as well as the parametric component) are considered.

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

# this is where we tell the function which model output etc., to use 
with(monthly_Hotspot_DMI, tsDiagGamm(M2_no_DMI, timevar = time, observed = HOT)) 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USING ZUUR (2012)'s plotting of all covariates vs residuals

## we could also plot the residuals vs each other covariate and each covariate not in the model (if there are any). We are checking that there are no significant patterns in the residuals.

## For continuous covariates we can use multi-panel scatterplots. To aid visual interpretation we add a smoother to each panel. The 95% point-wise confidence intervals for the smoothers (grey area) should contain 0 for all covariate values, indicating that there are no significant patterns in the residuals.

## R code to make the multi-panel scatterplot is based on the MyxyplotPolygon function that can be found in the HighstatLibV10.R function.

# =============================================================================================================

# load the MyxyplotPolygon function
# source("HighstatLibV10.R")

MyxyplotPolygon <- function(Z, MyV, NameY1) {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))

  library(mgcv)
  library(lattice)
  Z <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(label = "Explanatory variables", cex = 1.5),
              ylab = "",
              strip = function(bg='white',cex.lab = 1.5,...)
                strip.default(bg='white', ...),
              scales = list(alternating = TRUE,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              panel=function(x, y){
                t1 <- gam(y~s(x))
                MD1 <- data.frame(x=seq(from = min(x, na.rm = TRUE),
                                        to = max(x, na.rm = TRUE),
                                        length = 100))
                P1 <- predict(t1,   se.fit = TRUE)
                I1 <- order(x)
                xs <- sort(x)
                panel.lines(xs, P1$fit[I1], col = 1)
                panel.polygon(c(xs, rev(xs)),
                              c(P1$fit[I1]-2*P1$se.fit[I1],
                                rev(P1$fit[I1]+2*P1$se.fit[I1])),
                              col = gray(0.7),
                              density = 10 )
                panel.grid(h=-1, v= 2)
                panel.abline(0,0)
                panel.points(x, y, col = 1)
                
              })
  #Because the xyplot is inside a function you need to print 
  #construction below
  print(Z)
}

# =============================================================================================================


# for model M2_no_DMI
E2 <- resid(M2_no_DMI$lme, type= "normalized") # normalized = standardized
monthly_Hotspot_DMI$E2 <- E2
MyVar   <- c("time", "DMI", "month", "Nino_3.4")
MyxyplotPolygon(monthly_Hotspot_DMI, MyVar, "E2")


# We finally investigate whether residuals from the same population (month) are more similar than residuals from different populations (month), and for this we use a conditional boxplot. The boxplot for model M2 seems to suggest that there is a population (month) effect in these residuals. To verify this, we apply a linear regression in which we model the residuals as a function of population (month). A significant population (month) effect would mean that we should abandon the current model and possibly apply a linear mixed effects model


# model M2_no_DMI
boxplot(E2 ~ month, data = monthly_Hotspot_DMI, cex.lab = 1.5, xlab = "Month", ylab = "Standardized residuals")
abline(0, 0, lty = 2)


# ----------------------------------------------------------------------------------------------------

# END



