# PLOTTING OF PHOTOTRANSECT RESULTS

# ____________ SET DIRECTORY ____________________

# First set the directory in which we are working in the properties section of R
# change the file address below to where you have stored the data file on your computer
setwd ("E:/Our Papers/2020 IOD and Coral Cover/Resubmission/Data & R Code")
# then double check that you are in this directory before running any script
getwd()

library(ggplot2) # basic GGPLOT2
library(scales) # helps to label and scale axes

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@  1.1 READ IN THE COMBINED CORAL COVER, SLA AND DHW DATA  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# capture from Excel file - "Coral Cover Phototransects.xlsx"

library(xlsx)
Coral_cover <- read.xlsx("Coral Cover PhotoTransects.xlsx", sheetIndex = 1, header = TRUE, as.data.frame = TRUE )
str(Coral_cover) # check that the type of all columns have been read in correctly
# here Transect.14 and Transect.15 have incorrectly been taken in as 'characters' - need to convert to numeric
Coral_cover$Transect.14 <- as.numeric(as.character(Coral_cover$Transect.14))
Coral_cover$Transect.15 <- as.numeric(as.character(Coral_cover$Transect.15))
# recheck
str(Coral_cover)

# use GGPLOT2 for more control

# FULL TIME SERIES 1987 TO 2020

# set up basic plotting 
gX <- ggplot(data=Coral_cover, aes(x= Decimal_date)) + 
  #geom_point(aes(y=Transect.9, color="TR 9")) +
  scale_x_continuous(name = "", limits = c(1987,2022), breaks = seq(1987,2022,1), labels=c("1987","","1989","","1991","","1993","","1995","","1997","","1999","","2001","","2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017","","2019","","2021",""))  + 
  scale_y_continuous(name="Coral Cover %", limits=c(0,85))+
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
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=1987,xmax=1988.07103825137,ymin=0,ymax=85),colour="white",alpha=0.2,fill="cyan",size=0.5) + 
  # Note the above period actually started on 1986.7095890411 but if this date is used it is omitted because the scale starts at 1987
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=1991.38630136986,xmax=1992.45901639344,ymin=0,ymax=85),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  # discount this period only 13 weeks geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=1993.22465753425,xmax=1993.45479452055,ymin=0,ymax=75),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=1994.27945205479,xmax=1995.14246575342,ymin=0,ymax=85),colour="white",alpha=0.2,fill="gray70",size=0.5) +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=1997.32602739726,xmax=1998.32328767123,ymin=0,ymax=85),colour="white",alpha=0.2,fill="gray70",size=0.5) +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2002.42465753425,xmax=2003.15342465753,ymin=0,ymax=85),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2004.59016393443,xmax=2004.99180327869,ymin=0,ymax=85),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2006.62191780822,xmax=2007.02465753425,ymin=0,ymax=85),colour="white",alpha=0.2,fill="gray70",size=0.5) +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2009.47671232877,xmax=2010.26301369863,ymin=0,ymax=85),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  #IOD but discount this short period geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2012.56284153005,xmax=2012.77322404372,ymin=0,ymax=75),colour="white",alpha=0.2,fill="lemonchiffon",size=0.5) +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2015.15068493151,xmax=2016.33879781421,ymin=0,ymax=85),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  #IOD but discount this short period geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2017.27671232877,xmax=2017.60273972603,ymin=0,ymax=75),colour="white",alpha=0.2,fill="lemonchiffon",size=0.5) +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2018.73424657534,xmax=2020,ymin=0,ymax=85),colour="white",alpha=0.2,fill="gray70",size=0.5)

gX1

# add Transect 9 data
gX2 <- gX1 + geom_point(aes(y=Transect.9, color="TR 9"))

# add Transect 11 data
gX3 <- gX2 + geom_point(aes(y=Transect.11, color="TR 11"))

# add Transect 14 data
gX4 <- gX3 + geom_point(aes(y=Transect.14, color="TR 14"))

# add Transect 15 data
gX5 <- gX4 + geom_point(aes(y=Transect.15, color="TR 15"))
gX5

# in order to plot a line joining the data points for Mean cover we need to compute an interpolated set of data, thus:
# Create a spline interpolation of the mean coral cover for plotting - compute the spline points and add them to a new dataframe
# method="fmm" or "natural" or "periodic" or "monoH.FC" or "hyman"
spline.df <- as.data.frame(spline(Coral_cover$Decimal_date, Coral_cover$Mean, method="fmm", n=500))
# similarly for the Upper & Lower 95% CI 
spline.df1 <- as.data.frame(spline(Coral_cover$Decimal_date, Coral_cover$Upper_CI, method="fmm", n=500))
spline.df2 <- as.data.frame(spline(Coral_cover$Decimal_date, Coral_cover$Lower_CI, method="fmm", n=500))

# add the splines that we computed above and stored in 'spline.d' dataframe this is through mean cover points
gX6 <- gX5 + geom_line(data=spline.df, aes(x=x,y=y), colour="black", size=1) +
  geom_line(data=spline.df1, aes(x=x,y=y), colour="black", size=0.7, linetype="dashed") +
  geom_line(data=spline.df2, aes(x=x,y=y), colour="black", size=0.7, linetype="dashed")
gX6

# now add DHW 
# if needed we could scale the DHW values so that they plot of the Coral Cover axis at a suitable scale. No need here.
#Coral_cover$Scaled_DHW <- Coral_cover$DHW * X # choose X as appropriate

gX7 <- gX6 + geom_line(aes(y=Coral_cover$DHW), colour="red", size=0.5) +
 geom_area(aes(y=Coral_cover$DHW), colour="red", fill= "red") 
gX7


#################################################################################################
# ###############################################################################################

# 1997 EVENT

# set up basic plotting 
gX1997 <- ggplot(data=Coral_cover, aes(x= Decimal_date)) + 
  scale_x_continuous(name = "", limits = c(1996,1999), breaks = seq(1996,1999,1), labels=c("1996","1997","1998","1999"))  + 
  scale_y_continuous(name="Coral Cover %", limits=c(0,85))+
  theme_bw() +
  theme(plot.margin=unit(c(4,3,4,0), "cm")) +     # plot margins are in the order top, right, bottom, left                         
  theme(axis.text.x = element_text(angle = 0, hjust= 0, vjust = 0)) + # adjust the position of the X axis tick labels
  theme(panel.grid.minor = element_blank())+ # remove the minor grid lines vertically
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) # makes axis text black
gX1997

# add rectangles to mark combined ENSO/IOD or just IOD
# these rectangles have start and end dates for when the NINO3.4 was above the standard deviation value computed above for several weeks. The decimal date information entered below was extracted from the weekly NINO3.4 file ("NINO3.4 weekly to 13 Jan 2021.xlsx") in the folder E:\Phuket Databases\IOD-NNI-SOI\NINO3.4\State of the Ocean Climate. For Nino 3.4 the file is "El Nino 3.4 weekly 1981 to 15 Jul 2020.xlsx" and the level used was >0.5 deg C.
# fill=colour of rectangle, size=border width, colour=border col
# Combined IOD/ENSO is grey
# IOD only is lemonchiffon
# ENSO only is lightcyan

gX1_1997 <- gX1997 +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=1997.32602739726,xmax=1998.32328767123,ymin=0,ymax=85),colour="white",alpha=0.2,fill="cyan",size=0.5) +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=1997.47945,xmax=1998.05479452055,ymin=0,ymax=85),colour="white",alpha=0.2,fill="gray70",size=0.5) 
  
gX1_1997

# add Transect 9 data
gX2_1997 <- gX1_1997 + geom_point(aes(y=Transect.9, color="TR 9"))
# add Transect 11 data
gX3_1997 <- gX2_1997 + geom_point(aes(y=Transect.11, color="TR 11"))
# add Transect 14 data
gX4_1997 <- gX3_1997 + geom_point(aes(y=Transect.14, color="TR 14"))
# add Transect 15 data
gX5_1997 <- gX4_1997 + geom_point(aes(y=Transect.15, color="TR 15"))
gX5_1997

# add line for sea level and a second y axis to visualise this
# convert the sea level anomaly back to the original values and make this an extra column to avoid using negative values on this combined graph
# the mean we had used in "Monthly sea level at Tapao Noi 1977-2020.xlsx" was 2247
Coral_cover$RTN_corrected_SLA <- Coral_cover$RTN_final_datums_corrected + 2247
# normalize the SLA data (this way the spread and mean can be varied, and made to align with the primary y-scale) and then compute the reverse normalization for the secondary y-axis.
ylim.prim <- c(0, 85)     # for Coral Cover
ylim.sec <- c(1500, 2500) # for SLA
a <- ylim.prim[1] + 30    #  adjust these parameters to obtain a nice visual fit - the higher the value the more the SLA plot moves up the graph relative to Cover
scalingfactor <- 10      # to vary the spread
sd(Coral_cover$RTN_corrected_SLA)
mean(Coral_cover$RTN_corrected_SLA)

gX6_1997 <- gX5_1997 + geom_line(aes(y = (a + ((Coral_cover$RTN_corrected_SLA - mean(Coral_cover$RTN_corrected_SLA))/sd(Coral_cover$RTN_corrected_SLA)) *scalingfactor)), colour="steelblue4", size=1) +
  ylim(ylim.prim) +
  scale_y_continuous(name="Coral Cover %", sec.axis = sec_axis(~ (. - a) / scalingfactor * sd(Coral_cover$RTN_corrected_SLA) + 
                                                                 mean(Coral_cover$RTN_corrected_SLA), name = "Sea Level (mm)"),) +
  theme(axis.text.y.right=element_text(color="black"), axis.title.y.right=element_text(color="black"))

gX6_1997

# add the splines through mean cover and 95% CI points
gX7_1997 <- gX6_1997 + geom_line(data=spline.df, aes(x=x,y=y), colour="black", size=1) +
  geom_line(data=spline.df1, aes(x=x,y=y), colour="black", size=0.7, linetype="dashed") +
  geom_line(data=spline.df2, aes(x=x,y=y), colour="black", size=0.7, linetype="dashed")

gX7_1997

# now add DHW 
# if needed we could scale the DHW values so that they plot of the Coral Cover axis at a suitable scale. No need here.
#Coral_cover$Scaled_DHW <- Coral_cover$DHW * X # choose X as appropriate

gX8_1997 <- gX7_1997 + geom_line(aes(y=Coral_cover$DHW), colour="red", size=0.5) 
gX8_1997

gX9_1997 <- gX8_1997 + geom_area(aes(y=Coral_cover$DHW), colour="red", fill= "red") 
gX9_1997


#################################################################################################

# 2010 EVENT

# set up basic plotting 
gX2010 <- ggplot(data=Coral_cover, aes(x= Decimal_date)) + 
  scale_x_continuous(name = "", limits = c(2007,2012), breaks = seq(2007,2012,1), labels=c("2007","2008","2009","2010","2011","2012"))  + 
  scale_y_continuous(name="Coral Cover %", limits=c(0,85))+
  theme_bw() +
  theme(plot.margin=unit(c(4,3,4,0), "cm")) +     # plot margins are in the order top, right, bottom, left                         
  theme(axis.text.x = element_text(angle = 0, hjust= 0, vjust = 0)) + # adjust the position of the X axis tick labels
  theme(panel.grid.minor = element_blank())+ # remove the minor grid lines vertically
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) # makes axis text black
gX2010

# add rectangles to mark combined ENSO/IOD or just IOD
# these rectangles have start and end dates for when the NINO3.4 was above the standard deviation value computed above for several weeks. The decimal date information entered below was extracted from the weekly NINO3.4 file ("NINO3.4 weekly to 13 Jan 2021.xlsx") in the folder E:\Phuket Databases\IOD-NNI-SOI\NINO3.4\State of the Ocean Climate. For Nino 3.4 the file is "El Nino 3.4 weekly 1981 to 15 Jul 2020.xlsx" and the level used was >0.5 deg C.
# fill=colour of rectangle, size=border width, colour=border col
# Combined IOD/ENSO is grey
# IOD only is lemonchiffon
# ENSO only is lightcyan

gX1_2010 <- gX2010 +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2009.47671232877,xmax=2010.26301369863,ymin=0,ymax=85),colour="white",alpha=0.2,fill="cyan",size=0.5) 

gX1_2010

# add Transect 9 data
gX2_2010 <- gX1_2010 + geom_point(aes(y=Transect.9, color="TR 9"))
# add Transect 11 data
gX3_2010 <- gX2_2010 + geom_point(aes(y=Transect.11, color="TR 11"))
# add Transect 14 data
gX4_2010 <- gX3_2010 + geom_point(aes(y=Transect.14, color="TR 14"))
# add Transect 15 data
gX5_2010 <- gX4_2010 + geom_point(aes(y=Transect.15, color="TR 15"))
gX5_2010

gX6_2010 <- gX5_2010 + geom_line(aes(y = (a + ((Coral_cover$RTN_corrected_SLA - mean(Coral_cover$RTN_corrected_SLA))/sd(Coral_cover$RTN_corrected_SLA)) *scalingfactor)), colour="steelblue4", size=1) +
  ylim(ylim.prim) +
  scale_y_continuous(name="Coral Cover %", sec.axis = sec_axis(~ (. - a) / scalingfactor * sd(Coral_cover$RTN_corrected_SLA) + 
                                                                 mean(Coral_cover$RTN_corrected_SLA), name = "Sea Level (mm)"),) +
  theme(axis.text.y.right=element_text(color="black"), axis.title.y.right=element_text(color="black"))

gX6_2010

# add the spline through mean cover points
gX7_2010 <- gX6_2010 + geom_line(data=spline.df, aes(x=x,y=y), colour="black", size=1)+
  geom_line(data=spline.df1, aes(x=x,y=y), colour="black", size=0.7, linetype="dashed") +
  geom_line(data=spline.df2, aes(x=x,y=y), colour="black", size=0.7, linetype="dashed")
gX7_2010

# now add DHW 
# if needed we could scale the DHW values so that they plot of the Coral Cover axis at a suitable scale. No need here.
#Coral_cover$Scaled_DHW <- Coral_cover$DHW * X # choose X as appropriate

gX8_2010 <- gX7_2010 + geom_line(aes(y=Coral_cover$DHW), colour="red", size=0.5) +
  geom_area(aes(y=Coral_cover$DHW), colour="red", fill= "red")  

gX8_2010

####################################################################################################################################

# 2019 EVENT

# set up basic plotting 
gX2019 <- ggplot(data=Coral_cover, aes(x= Decimal_date)) + 
  scale_x_continuous(name = "", limits = c(2017,2021), breaks = seq(2017,2021,1), labels=c("2017","2018","2019","2020","2021"))  + 
  scale_y_continuous(name="Coral Cover %", limits=c(0,85))+
  theme_bw() +
  theme(plot.margin=unit(c(4,3,4,0), "cm")) +     # plot margins are in the order top, right, bottom, left                         
  theme(axis.text.x = element_text(angle = 0, hjust= 0, vjust = 0)) + # adjust the position of the X axis tick labels
  theme(panel.grid.minor = element_blank())+ # remove the minor grid lines vertically
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) # makes axis text black
gX2019

# add rectangles to mark combined ENSO/IOD or just IOD
# these rectangles have start and end dates for when the NINO3.4 was above the standard deviation value computed above for several weeks. The decimal date information entered below was extracted from the weekly NINO3.4 file ("NINO3.4 weekly to 13 Jan 2021.xlsx") in the folder E:\Phuket Databases\IOD-NNI-SOI\NINO3.4\State of the Ocean Climate. For Nino 3.4 the file is "El Nino 3.4 weekly 1981 to 15 Jul 2020.xlsx" and the level used was >0.5 deg C.
# fill=colour of rectangle, size=border width, colour=border col
# Combined IOD/ENSO is grey
# IOD only is lemonchiffon
# ENSO only is lightcyan

gX1_2019 <- gX2019 +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2018.6,xmax=2018.90684931507,ymin=0,ymax=85),colour="white",alpha=0.2,fill="gray70",size=0.5) +
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2019.2904109589,xmax=2020,ymin=0,ymax=85),colour="white",alpha=0.2,fill="gray70",size=0.5) + 
  geom_rect(data=Coral_cover, aes(NULL,NULL,xmin=2018.73424657534,xmax=2019.48219178082,ymin=0,ymax=85),colour="white",alpha=0.2,fill="cyan",size=0.5) 

gX1_2019

# add Transect 9 data
gX2_2019 <- gX1_2019 + geom_point(aes(y=Transect.9, color="TR 9"))
# add Transect 11 data
gX3_2019 <- gX2_2019 + geom_point(aes(y=Transect.11, color="TR 11"))
# add Transect 14 data
gX4_2019 <- gX3_2019 + geom_point(aes(y=Transect.14, color="TR 14"))
# add Transect 15 data
gX5_2019 <- gX4_2019 + geom_point(aes(y=Transect.15, color="TR 15"))
gX5_2019

gX6_2019 <- gX5_2019 + geom_line(aes(y = (a + ((Coral_cover$RTN_corrected_SLA - mean(Coral_cover$RTN_corrected_SLA))/sd(Coral_cover$RTN_corrected_SLA)) *scalingfactor)), colour="steelblue4", size=1) +
  ylim(ylim.prim) +
  scale_y_continuous(name="Coral Cover %", sec.axis = sec_axis(~ (. - a) / scalingfactor * sd(Coral_cover$RTN_corrected_SLA) + 
                                                                 mean(Coral_cover$RTN_corrected_SLA), name = "Sea Level (mm)"),) +
  theme(axis.text.y.right=element_text(color="black"), axis.title.y.right=element_text(color="black"))

gX6_2019

# add the spline through mean cover points
gX7_2019 <- gX6_2019 + geom_line(data=spline.df, aes(x=x,y=y), colour="black", size=1)+
  geom_line(data=spline.df1, aes(x=x,y=y), colour="black", size=0.7, linetype="dashed") +
  geom_line(data=spline.df2, aes(x=x,y=y), colour="black", size=0.7, linetype="dashed")
gX7_2019

# now add DHW 
# if needed we could scale the DHW values so that they plot of the Coral Cover axis at a suitable scale. No need here.
#Coral_cover$Scaled_DHW <- Coral_cover$DHW * X # choose X as appropriate

gX8_2019 <- gX7_2019 + geom_line(aes(y=Coral_cover$DHW), colour="red", size=0.5) +
  geom_area(aes(y=Coral_cover$DHW), colour="red", fill= "red")  

gX8_2019

# END
#################################################################################################################











