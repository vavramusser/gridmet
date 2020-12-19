#########################################
### PRECIPITATION SPATIAL AGGREGATION ###
#########################################

# this script sources preciptation data from previously downloaded data
# the number of precipiation days and the total amount of monhtly precipitation aggregated by the set of input polygons
# the output is a set of csv files which indicate the number of days and total precipitation by month by polygon

# the user should specify the following in the user-set parameters section below
### source folder (the location of the raw precipitation data)
### save folder (the folder where the output data will be saved)
### filepath for the aggregation polygons
# it is recommended that the user ONLY edit the user-set parameters section

# TIME TO COMPLETE: running one year of data aggregating at the census tract takes took approximalty 40 minutes

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
#install.packages(c("exactextractr", ncdf4", "raster", "sf"))

# load relevant libraries
library(exactextractr, raster, sf)

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
years <- c(2015, 2003, 2004, 2006, 2007, 2008, 2009, 2011, 2012, 2013, 2014, 2016, 2017, 2019)

# set the working directory to the location of the heat_index script
# which should be located in the "/Working" folder
setwd("C:/Users/vavra/Dropbox/Weather/CDR Data/Working")



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

# truncates the polygon file to ONLY include the previously-specified reference column
# removes unneeded data from the polygon file
polys <- polys[polyref]



##################################
### precipitation calculations ###
##################################

# captures start time for total time calculation
start <- proc.time()

# initiates a year counter
# this counter is used to calculate time estimates as the code runs
yn <- 1


# loops over each specified year
for (y in years) {
  
  # prints the current being processed as a reference for the user
  print(paste("PROCESSING YEAR", y))
  
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
  prec <- brick(paste("../Original/", "pr_", y, ".nc", sep = ""))

  # prints a note to the user that preprossing is complete for the current year 
  print(paste("year", y, "preprocessing complete"), quote = F)
  
  # initiates a month counter
  mn <- 1
  
  # loops over the list of sequences of numbers which correspond to sets of days for each month
  for (m in months) {
    
    # calculates the total number of days with any (> 0 precipitation) per grid cell for the month
    prec_days <- calc(calc(subset(prec, m), fun = function(r){ifelse(r > 0, 1, 0)}), sum)
    
    # calculates the mean of all grids within each polygon from the polygon file
    # this will produce an average number of days with any precipitation for each polygon for the current month
    extract_days <- exact_extract(prec_days, polygons, "mean", progress = F)
    # calculates the percent of days in the month with any precipitation
    percent_days <- extract_days / length(m)
    
    # calculates the total amount of precipitation occurring per grid cell for the month
    prec_total <- calc(subset(prec, m), sum)
    
    # calculates the mean of all grids within each polygon from the polygon file
    # this will produce an average total amount of precipitation for each polygon for the current month
    extract_total <- exact_extract(prec_total, polygons, "mean", progress = F)
    # calculates the average precipitation per number of days in the month
    percent_total1 <- extract_total / length(m)
    # calculates the average precipitation per number of days with any precipitation in the month
    percent_total2 <- extract_total / extract_days
    
    # binds the outcome to the dataframe containing the census trat reference column
    dat <- cbind(dat, extract_days, percent_days, extract_total, percent_total1, percent_total2)
    # renames the extract column to correspond to the variable naming schema for this project
    # using the "m" prefix and varname_num
    names(dat)[names(dat) == "extract_days"] <- paste("m", 300, geography, y, str_pad(mn, 2, pad = 0), sep = "")
    names(dat)[names(dat) == "percent_days"] <- paste("m", 301, geography, y, str_pad(mn, 2, pad = 0), sep = "")
    names(dat)[names(dat) == "extract_total"] <- paste("m", 302, geography, y, str_pad(mn, 2, pad = 0), sep = "")
    names(dat)[names(dat) == "percent_total1"] <- paste("m", 303, geography, y, str_pad(mn, 2, pad = 0), sep = "")
    names(dat)[names(dat) == "percent_total2"] <- paste("m", 304, geography, y, str_pad(mn, 2, pad = 0), sep = "")
    
    # prints a note to the user when the month is complete
    print(paste("month", mn, "complete"))
    
    # increments the month counter
    mn <- mn + 1
  }
  
  # when all 12 months of the year are complete
  # saves the dat dataframe as both a csv and dta file in the output directory
  write.csv(dat, paste("../Final/", paste("pr", y, "tr", sep = ""), ".csv", sep = ""), row.names = F)
  write_dta(dat, paste("../Final/", paste("pr", y, "tr", sep = ""), ".dta", sep = ""))
  
  # prints a message to the user that the year is complete
  # also provides total elapsed time ad estimated remaining time based on the number of years yet to be processed
  print(paste(y, "complete"))
  print(paste("total time elapsed:", round((proc.time() - start)[[3]]/60, 1), "minutes"))
  print(paste("estimated time remaining:", round((proc.time() - start)[[3]]/60/yn * (length(years) - yn), 1), "minutes"))
  print("")
  
  # increments the year counter
  yn <- yn + 1
  gc()
}

# prints a message to indicate the end of the script
print("SCRIPT COMPLETE", quote = F)
