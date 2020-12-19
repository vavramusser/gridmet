######################################################
### HEAT INDEX CALCULATION AND SPATIAL AGGREGATION ###
######################################################

# this script sources temperature and relative humidity data from previously downloaded data to calculate heat index
# heat index is first calculated at the grid level and then indexed at the grid level based on heat index threshold levels
# from this, the number of heat days at each index level is aggregated by the specified geometry
# the output is a set of csv and dta files which indicate the number of days at each heat index level by month by polygon

# it is recommended that the user ONLY edit the user-set parameters section

# TIME TO COMPLETE: running one year of data aggregating at the census tract level takes approxmatly 1.5 to 2.5 hours on the test machine

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
#install.packages(c("exactextractr", "foreign", "ncdf4", "raster", "sf", "stringr", "tibble"))

# load relevant libraries
library(exactextractr)
library(foreign)
library(ncdf4)
library(raster)
library(sf)
library(stringr)

# clears the workspace and runs garbage collection
rm(list = ls()); gc()

# turns off warnings
options(warn = -1)



###########################
### user-set paraemters ###
###########################

# specify the desired geography for aggrgeation
# please use the following codes to indicate geometry
# co      county (2010)
# tr      Census tract (2010)
# bg      Census block group (2010)
geography <- "tr"

# indicate the desired year(s) or range of year for which the calculations should be processed
# to run a single year or list of years, use c()
# to run a range of years, use seq()
years <- c(2000, 2005, 2010, 2015)

# set the working directory to the location of the heat_index script
# which should be located in the "/Working" folder
setwd("C:/Users/vavra/Dropbox/Weather/CDR Data/Working")




#################
### functions ###
#################

# function to convert Kelvin to Fahrenheit
ktof <- function(r) {return((r - 273.15) * (9/5) + 32)}

# simplified heat index function without adjustments
# NOTE: this function has been replaced by hi_withadj (defined below) which directly incorporates the adjustments
# this function is NOT CURRENTLY IN USE
hi_base <- function(t, rh) {
  hi = -42.379 +
    (2.04901523 * t) +
    (10.14333127 * rh) -
    (0.22475541 * t * rh) -
    (0.00683783 * t * t) -
    (0.05481717 * rh * rh) +
    (0.00122874 * t * t * rh) +
    (0.00085282 * t * rh * rh) -
    (0.00000199 * t * t * rh * rh)
}

# heat index function with adjustments
hi_withadj <- function(t, rh, adj1_condt, adj1_condrh, adj2_condt, adj2_condrh, adj3_condt1, adj3_condt0) {
  hi = (adj3_condt0 *
          (-42.379 +
             (2.04901523 * t) +
             (10.14333127 * rh) -
             (0.22475541 * t * rh) -
             (0.00683783 * t * t) -
             (0.05481717 * rh * rh) +
             (0.00122874 * t * t * rh) +
             (0.00085282 * t * rh * rh) -
             (0.00000199 * t * t * rh * rh) +
             (adj1_condt * adj1_condrh * (-(((13 - rh) / 4) * ifelse(adj1_condt, sqrt((17 - abs(t - 95)) / 17), 0)))) +
             (adj2_condt * adj2_condrh * ((rh - 85) / 10) * ((87 - t) / 5)))) +
    (adj3_condt1 * (0.5 * (61 + t + (1.2 * (t - 68)) + (0.094 * rh))))
}

# the following set of functions creates suppementaty adjustment binary raster bricks
# the output of these functions are used in the complete version of the heat index function
# to activate different adjustments based on temperature and relative humidity ranges
adj1_condt <- function(t) {ifelse(t >= 80, ifelse(t <= 112, 1, 0), 0)}
adj1_condrh <- function(rh) {ifelse(rh < 13, 1, 0)}
adj2_condt <- function(t) {ifelse(t >= 80, ifelse(t <= 87, 1, 0), 0)}
adj2_condrh <- function(rh) {ifelse(rh > 85, 1, 0)}
adj3_condt <- function(t) {ifelse(t < 80, 1, 0)}

# the following functions use the binary raster bricks created by the above set of functions
# to calculate the exact adjustment values to be added to the base heat index
# NOTE: adjustment code is directly incorporated the hi_withadj function - these two functions are not currently in use
adj1 <- function(t, rh, adj1_t, adj1_rh) {adj1_t * adj1_rh * (-(((13 - rh) / 4) * ifelse(adj1_t, sqrt((17 - abs(t - 95)) / 17), 0)))}
adj2 <- function(t, rh, adj2_t, adj2_rh) {adj2_t * adj2_rh * ((rh - 85) / 10) * ((87 - t) / 5)}



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
month_num <- list(seq(1, 31),    seq(32, 59),   seq(60, 90),   seq(91, 120),
                  seq(121, 151), seq(152, 181), seq(182, 212), seq(213, 243),
                  seq(244, 273), seq(274, 304), seq(305, 334), seq(335, 365))

# specifes the sequence of days which correspond to each month
# for leap years
month_num_leap <- list(seq(1, 31),    seq(32, 60),   seq(61, 91),   seq(92, 121),
                       seq(122, 152), seq(153, 182), seq(183, 213), seq(214, 244),
                       seq(245, 274), seq(275, 305), seq(306, 335), seq(336, 366))



###############################
### heat index calculations ###
###############################

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
  
  # imports the maximum temperature and minimum relative humidity raster bricks for the current year
  tmax <- brick(paste("../Original/", "tmmx_", y, ".nc", sep = ""))
  rmin <- brick(paste("../Original/", "/", "rmin_", y, ".nc", sep = ""))
  
  # uses the temperature conversion function to convert the temperature raster brick from K to F
  tmax_f <- calc(tmax, fun = ktof)
  
  # calcualtes the binary rasters for adjustment 1
  adj1_t <- calc(tmax_f, fun = adj1_condt)
  adj1_rh <- calc(rmin, fun = adj1_condrh)
  
  # calculates the binary rasters for adjustment 2
  adj2_t <- calc(tmax_f, fun = adj2_condt)
  adj2_rh <- calc(rmin, fun = adj2_condrh)
  
  # calculates the binary raster for adjustment 3
  adj3_t1 <- calc(tmax_f, fun = adj3_condt)
  adj3_t0 <- abs(adj3_t1 - 1)
  
  # calculates the heat index using the complete formula with adjustments
  hi <- overlay(tmax_f, rmin, adj1_t, adj1_rh, adj2_t, adj2_rh, adj3_t1, adj3_t0, fun = hi_withadj)

  # creates raster bricks of binary inclusion in each heat index level at the grid level
  hi_lvl0 <- calc(hi, fun = function(r){ifelse(r < 80, 1, 0)})
  hi_lvl1 <- calc(hi, fun = function(r){ifelse(r < 80, 0, ifelse(r < 90, 1, 0))})
  hi_lvl2 <- calc(hi, fun = function(r){ifelse(r < 90, 0, ifelse(r < 103, 1, 0))})
  hi_lvl3 <- calc(hi, fun = function(r){ifelse(r < 103, 0, ifelse(r < 125, 1, 0))})
  hi_lvl4 <- calc(hi, fun = function(r){ifelse(r < 125, 0, 1)})
  
  # prints a note to the user that preprossing is complete for the current year 
  print(paste("year", y, "preprocessing complete"), quote = F)
  
  # initiates a month counter
  mn <- 1
  
  # loops over the list of sequences of numbers which correspond to sets of days for each month
  for (m in months) {
    
    # creates single rasters for total number of days at given heat index level for the current momth
    hi_lvl0_m <- calc(subset(hi_lvl0, m), sum)
    hi_lvl1_m <- calc(subset(hi_lvl1, m), sum)
    hi_lvl2_m <- calc(subset(hi_lvl2, m), sum)
    hi_lvl3_m <- calc(subset(hi_lvl3, m), sum)
    hi_lvl4_m <- calc(subset(hi_lvl4, m), sum)
    
    # creates a list of the previously-created rasters
    # this will be used to loop through to process each raster individually
    index <- list(hi_lvl0_m, hi_lvl1_m, hi_lvl2_m, hi_lvl3_m, hi_lvl4_m)
    
    # initiates variable identification number for output variable
    # the 100 sequence is used for heat index
    varname_num <- 100
    
    # loops over the set of rasters
    for (i in index) {
      
      # calculates the mean of all grids within each polygon from the polygon file
      # this will produce an average number of days at the given heat index level for each polygon for the current month
      extract <- exact_extract(i, polygons, "mean", progress = F)
      # calculates the percent of days in the month for the given heat index level
      percent <- extract / length(m)
      # binds the outcome to the dataframe containing the census tract reference column
      dat <- cbind(dat, extract, percent)
      # renames the extract column to correspond to the variable naming schema for this project
      # using the "m" prefix and varname_num
      names(dat)[names(dat) == "extract"] <- paste("m", varname_num, geography, y, str_pad(mn, 2, pad = 0), sep = "")
      names(dat)[names(dat) == "percent"] <- paste("m", (varname_num + 10), geography, y, str_pad(mn, 2, pad = 0), sep = "")
      
      # increments the variable name
      varname_num <- varname_num + 1
    }
    
    # prints a note to the user when the month is complete
    print(paste("month", mn, "complete"), quote = F)
    
    # increments the month counter
    mn <- mn + 1
  }

  # when all 12 months of the year are complete
  # saves the dat dataframe as both a csv and dta file in the output directory
  write.csv(dat, paste("../Final/", paste("hi", y, "tr", sep = ""), ".csv", sep = ""), row.names = F)
  write.dta(dat, paste("../Final/", paste("hi", y, "tr", sep = ""), ".dta", sep = ""))
  
  # prints a message to the user that the year is complete
  # also provides total elapsed time and estimated remaining time based on the number of years yet to be processed
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