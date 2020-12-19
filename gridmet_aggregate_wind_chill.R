######################################################
### WIND CHILL CALCULATION AND SPATIAL AGGREGATION ###
######################################################

# this script sources wind velocity and temperature data from previously downloaded data to calculate heat index
# wind chill is first calculated at the grid level and then indexed at the grid level based on wind chill frostbite threshold levels
# then the number of wind chill days at each index level is aggregated by the set of input polygons
# the output is a set of csv files which indicate the number of days at each wind chill index level by month by polygon

# it is recommended that the user ONLY edit the user-set parameters section

# TIME TO COMPLETE: running one year of data aggregating at the census tract takes approxmatly 2.5 to 3 hours on the test machine

# recommended: close all other applications and turn off internet connection to the machine while running
# recommended: run this script in R GUI rather than RStudio - this script can be run in RStudio but does cause RStudio to occasionally crash

# SUPPORT: Kate Vavra-Musser vavramusser@gmail.com



#########################
### environment setup ###
#########################

# update R
# uncomment and run this line to update R
#install.packages("installr"); library(installr); updateR()

# installs relevant libaraies
# uncomment and run this line if these packages need to be installed on your local system
# the ncdf4 package needs to be installed in the workspace but does not need to be loaded
# the ncdf4 namespace will automatically self-load when needed
#install.packages(c("exactextractr", "foreign", "ncdf4", "raster", "sf", "stringr))

# load relevant libraries
library(exactextractr)
library(foreign)
library(raster)
library(sf)
library(stringr)

# clears the workspace and runs garbage collection
rm(list = ls()); gc()

# turns off warnings
options(warn = -1)



###################
### user inputs ###
###################

# specify the desired geography for aggrgeation
# please use the following codes to indicate geometry
# co      county (2010)
# tr      Census tract (2010)
# bg      Census block group (2010)
geography <- "tr"

# indicate the desired year(s) or range of year for which the calculations should be processed
# to select a list of individual years, use c()
# to select a range of years, use seq()
# as the script reads in local data, please only select year(s) for which the reference data has been downloaded
years <- seq(2015, 2019)

# set the working directory to the location of the data_download.R script
# which should be located in the "/Working" folder
setwd("C:/Users/Kate/Dropbox/Weather/CDR Data/Working")


#################
### functions ###
#################

# function to convert Kelvin to Fahrenheit
ktof <- function(r) {return((r - 273.15) * (9/5) + 32)}

# function to convert meters per second to mmiles per hour
mstomph <- function(r) {return(r * 2.23694)}

# wind chill calculation function
# to be used with temperature in degrees F and wind speed at 10m/33ft in mph
windchill <- function(t, vs) {
  wc = 35.75 +
    (0.6215 * t) -
    (35.75 * (vs^0.16)) +
    (0.4275 * t * (vs^0.16))
}

# function to convert Kelvin to Fahrenheit
ktoc <- function(r) {return(r - 273.15)}

# wind chill calculation function
# to be used with temperature in degrees C and wind speed at 10m/33ft in m/s
windchill_celsius <- function(t, vs) {
  wc = 13.712 +
    (0.6215 * t) -
    (11.37 * (vs^0.16)) +
    (0.3965 * t * (vs^0.16))
}



########################
### additional setup ###
########################

# imports geogography file based on user specification and truncates the polygon file to only include the identification column
if(geography == "bg") {polygons <- read_sf("../Working/us_blck_grp_2010/US_blck_grp_2010.shp")}
if(geography == "tr") {polygons <- read_sf("../Working/us_tract_2010/US_tract_2010.shp")}
if(geography == "co") {polygons <- read_sf("../Working/us_county_2010/US_county_2010.shp")}
polygons <- polygons["GEOID10"]

# specifes the sequence of days which correspond to each month
# for non-leap years
month_num <- list(seq(1, 31),
                  seq(32, 59),
                  seq(60, 90),
                  seq(91, 120),
                  seq(121, 151),
                  seq(152, 181),
                  seq(182, 212),
                  seq(213, 243),
                  seq(244, 273),
                  seq(274, 304),
                  seq(305, 334),
                  seq(335, 365))

# specifes the sequence of days which correspond to each month
# for leap years
month_num_leap <- list(seq(1, 31),
                       seq(32, 60),
                       seq(61, 91),
                       seq(92, 121),
                       seq(122, 152),
                       seq(153, 182),
                       seq(183, 213),
                       seq(214, 244),
                       seq(245, 274),
                       seq(275, 305),
                       seq(306, 335),
                       seq(336, 366))



#####################################
### wind chill index calculations ###
#####################################

# captures start time for total time calculation
start <- proc.time()

# initiates a year counter
# this counter is used to calculate time estimates as the code runs
yn <- 1

# loops over each specified year
for (y in years) {
  
  # prints the current being processed as a reference for the user
  print(paste("PROCESSING YEAR", y), quote = F)
  
  # calculation to determine if the current year is a leap year or not
  # based on this calculation, either the month_num or month_num_leap list will be saved to "months"
  ifelse(y %% 4 == 0,
         ifelse(y %% 100 == 0,
                ifelse(y %% 400 == 0,
                       months <- month_num_leap,
                       months <- month_num),
                months <- month_num_leap),
         months <- month_num)

  # initializes an empty dataframe "dat" which will store the final variables
  # using the first (and currently only) data column in the polygon file - which is the census tract refnerece column
  dat <- as.data.frame(polygons)[1]
  # renames the census tract reference column
  names(dat) <- c("LINKCEN2010")

  # imports the minimum temperature and wind velocity raster bricks for the current year
  tmin <- brick(paste("../Original/", "tmmn_", y, ".nc", sep = ""))
  vspd <- brick(paste("../Original/", "/", "vs_", y, ".nc", sep = ""))
  
  # uses the temperature conversion function to convert the temperature raster brick from K to C
  tmin_c <- calc(tmin, fun = ktoc)
  
  # calculates the wind chill using C and m/s
  wc_c <- overlay(tmin_c, vspd, fun = windchill)
  
  # creates raster bricks of binary inclusion in each wind chill index level at the grid level
  # using wind chill calculated with C and m/s and hazard levels from Environment Canada
  wc_c_lvl0 <- calc(wc_c, fun = function(r){ifelse(r > 0, 1, 0)})
  wc_c_lvl1 <- calc(wc_c, fun = function(r){ifelse(r > 0, 0, ifelse(r > -10, 1, 0))})
  wc_c_lvl2 <- calc(wc_c, fun = function(r){ifelse(r > -10, 0, ifelse(r > -28, 1, 0))})
  wc_c_lvl3 <- calc(wc_c, fun = function(r){ifelse(r > -28, 0, ifelse(r > -40, 1, 0))})
  wc_c_lvl4 <- calc(wc_c, fun = function(r){ifelse(r > -40, 0, ifelse(r > -48, 1, 0))})
  wc_c_lvl5 <- calc(wc_c, fun = function(r){ifelse(r > -48, 0, ifelse(r > -55, 1, 0))})
  wc_c_lvl6 <- calc(wc_c, fun = function(r){ifelse(r > -55, 0, 1)})
  
  # uses the temperature conversion function to convert the temperature raster brick from K to F
  tmin_f <- calc(tmin, fun = ktof)
  
  # uses the wind speed conversion function to convert the wind speed raster brick from m/s to mph
  vspd_mph <- calc(vspd, fun = mstomph)
  
  # calculates the wind chill using F and mph
  wc_f <- overlay(tmin_f, vspd_mph, fun = windchill)
  
  # creates raster bricks of binary inclusion in each wind chill index level at the grid level
  # using wind chill calculated with F and mph and hazard levels from Roshan et al. 2010
  wc_f_lvl0 <- calc(wc_f, fun = function(r){ifelse(r > 0, 1, 0)})
  wc_f_lvl1 <- calc(wc_f, fun = function(r){ifelse(r > 0, 0, ifelse(r > -10, 1, 0))})
  wc_f_lvl2 <- calc(wc_f, fun = function(r){ifelse(r > -10, 0, ifelse(r > -15, 1, 0))})
  wc_f_lvl3 <- calc(wc_f, fun = function(r){ifelse(r > -15, 0, ifelse(r > -25, 1, 0))})
  wc_f_lvl4 <- calc(wc_f, fun = function(r){ifelse(r > -25, 0, ifelse(r > -45, 1, 0))})
  wc_f_lvl5 <- calc(wc_f, fun = function(r){ifelse(r > -45, 0, ifelse(r > -60, 1, 0))})
  wc_f_lvl6 <- calc(wc_f, fun = function(r){ifelse(r > -60, 0, 1)})
  
  # prints a note to the user that preprossing is complete for the current year 
  print(paste("year", y, "preprocessing complete"), quote = F)
  
  # initiates a month counter
  mn <- 1
  
  # loops over the list of sequences of numbers which correspond to sets of days for each month
  for (m in months) {
    
    # creates single rasters for total number of days at given wind chill index level for the current momth
    # using wind chill calculated with C and m/s and hazard levels from Environment Canada
    wc_lvl0_m <- calc(subset(wc_c_lvl0, m), sum)
    wc_lvl1_m <- calc(subset(wc_c_lvl1, m), sum)
    wc_lvl2_m <- calc(subset(wc_c_lvl2, m), sum)
    wc_lvl3_m <- calc(subset(wc_c_lvl3, m), sum)
    wc_lvl4_m <- calc(subset(wc_c_lvl4, m), sum)
    wc_lvl5_m <- calc(subset(wc_c_lvl5, m), sum)
    wc_lvl6_m <- calc(subset(wc_c_lvl5, m), sum)
    
    # creates a list of the previously-created rasters
    # this will be used to loop through to process each raster individually
    index <- list(wc_lvl0_m, wc_lvl1_m, wc_lvl2_m, wc_lvl3_m, wc_lvl4_m, wc_lvl5_m, wc_lvl6_m)
    
    # initiates variable identification number for output variable
    # the 200 sequence is used for wind chill
    varname_num <- 200
    
    # loops over the set of rasters
    for (i in index) {
      
      # calculates the mean of all grids within each polygon from the polygon file
      # this will produce an average number of days at the given heat index level for each polygon for the current month
      extract <- exact_extract(i, polygons, "mean", progress = F)
      # calculates the percent of days in the month for the given heat index level
      percent <- extract / length(m)
      # binds the outcome to the dataframe containing the census trat reference column
      dat <- cbind(dat, extract, percent)
      # renames the extract column to correspond to the variable naming schema for this project
      # using the "m" prefix and varname_num
      names(dat)[names(dat) == "extract"] <- paste("m", varname_num, geography, y, str_pad(mn, 2, pad = 0), sep = "")
      names(dat)[names(dat) == "percent"] <- paste("m", (varname_num + 10), geography, y, str_pad(mn, 2, pad = 0), sep = "")
      
      # increments the variable name
      varname_num <- varname_num + 1
    }
    
    # creates single rasters for total number of days at given wind chill index level for the current momth
    # using wind chill calculated with F and mph and hazard levels from Roshan et al. 2010
    wc_lvl0_m <- calc(subset(wc_f_lvl0, m), sum)
    wc_lvl1_m <- calc(subset(wc_f_lvl1, m), sum)
    wc_lvl2_m <- calc(subset(wc_f_lvl2, m), sum)
    wc_lvl3_m <- calc(subset(wc_f_lvl3, m), sum)
    wc_lvl4_m <- calc(subset(wc_f_lvl4, m), sum)
    wc_lvl5_m <- calc(subset(wc_f_lvl5, m), sum)
    wc_lvl6_m <- calc(subset(wc_f_lvl5, m), sum)
    
    # creates a list of the previously-created rasters
    # this will be used to loop through to process each raster individually
    index <- list(wc_lvl0_m, wc_lvl1_m, wc_lvl2_m, wc_lvl3_m, wc_lvl4_m, wc_lvl5_m, wc_lvl6_m)
    
    # initiates variable identification number for output variable
    # the 200 sequence is used for wind chill
    varname_num <- 220
    
    # loops over the set of rasters
    for (i in index) {
      
      # calculates the mean of all grids within each polygon from the polygon file
      # this will produce an average number of days at the given heat index level for each polygon for the current month
      extract <- exact_extract(i, polygons, "mean", progress = F)
      # calculates the percent of days in the month for the given heat index level
      percent <- extract / length(m)
      # binds the outcome to the dataframe containing the census trat reference column
      dat <- cbind(dat, extract, percent)
      # renames the extract column to correspond to the variable naming schema for this project
      # using the "m" prefix and varname_num
      names(dat)[names(dat) == "extract"] <- paste("m", varname_num, geography, y, str_pad(mn, 2, pad = 0), sep = "")
      names(dat)[names(dat) == "percent"] <- paste("m", (varname_num + 10), geography, y, str_pad(mn, 2, pad = 0), sep = "")
      
      # increments the variable name
      varname_num <- varname_num + 1
    }
    
    # prints a note to the user when the month is complete
    print(paste(y, ".", mn, " complete", sep = ""), quote = F)
    
    # increments the month counter
    mn <- mn + 1
  }
  # when all 12 months of the year are complete
  # saves the dat dataframe as both a csv and dta file in the output directory
  write.csv(dat, paste("../Final/", paste("wc", y, "tr", sep = ""), ".csv", sep = ""), row.names = F)
  write.dta(dat, paste("../Final/", paste("wc", y, "tr", sep = ""), ".dta", sep = ""))
  
  # prints a message to the user that the year is complete
  # also provides total elapsed time ad estimated remaining time based on the number of years yet to be processed
  print(paste(y, "complete"), quote = F)
  print(paste("total time elapsed:", round((proc.time() - start)[[3]]/60, 1), "minutes"), quote = F)
  print(paste("estimated time remaining:", round((proc.time() - start)[[3]]/60/yn * (length(years) - yn), 1), "minutes"), quote = F)
  print("", quote = F)

  # increments the year counter
  yn <- yn + 1
  gc()
}

# prints a message to indicate the end of the script
print("SCRIPT COMPLETE", quote = F)