###############################################################
### DATA DOWNLOAD FOR DATA FROM THE GRIDMET DATA REPOSITORY ###
###############################################################

# this script is designed to download datasets from gridMET data repository
# from the Climatology Laboratory Group website
# http://www.climatologylab.org/gridmet.html

# this script requires the machine to have an active internet connection

# the user should specify the ddesired parameters and desired years
# these are set in the user-set parameters section below
# downloaded data is save in the "/Original" folder
# it is recommended that the user ONLY edit the user-set parameters section

# SUPPORT: Kate Vavra-Musser vavramusser@gmail.com



#########################
### environment setup ###
#########################

# clears the workspace and runs garbage collection
rm(list = ls()); gc()

# turns off warnings
options(warn = -1)

# expands working memory allocation to the maximum
memory.size(max = T)



###########################
### user-set parameters ###
###########################

# set the desired parameters to download
# please use the following codes to indicate desired parameters
# bi      model-G
# etr     reference alfalfa evapotranspiration
# erc     model-G
# fm100   100-hour dead fuel moisture
# fm1000  1000-hour dead fuel moisture
# pdsi    Palmer drought severity index
# pet     reference grass evapotranspiration
# pr      precipitation
# rmax    maximum near-surface relative humidity
# rmin    minimum near-surface relative humidity
# sph     near-surface specific humidity
# srad    surface downwelling solar radiation
# th      wind direction at 10m
# tmmn    minimum near-surface air temperature
# tmmx    maximum near-surface air temperature
# vpd     mean vapor pressure deficit
# vs      wind speed at 10m
parameters <- c( "bi", "etr", "erc", "fm100", "fm1000",
                 "pdsi", "pet", "pr", "rmax", "rmin",
                 "sph", "srad", "th", "tmmn", "tmmx",
                 "vpd", "vs")

# indicate the desired year(s) or range of years
# to select a list of individual years, use c()
# to select a range of years, use seq()
# available years: 1979 through current year
years <- seq(1979, 2019)

# set the working directory to the location of the data_download.R script
# which should be located in the "/Working" folder
setwd("C:/Users/vavra/Dropbox/HRS Documentation/gridMet/CDR Data/Working")



#####################
### data download ###
#####################

# saves the previously-specified working directory filepath to a variable
# this is used later to append file extensions
setwd("../Original")
dir <- getwd()

# base url reference to the gridMet data repository
baseurl <- "http://www.northwestknowledge.net/metdata/data/"

# captures start time for total time calculation
start <- proc.time()

# loops over each parameter-year combination in the specified lists
# downloads each file and saves in the destiation folder
for (p in parameters) {
  for (y in years) {
    
    # concatenates the file extension from the parameter specifications
    extension <- paste(p, "_", y, ".nc", sep = "")
    # concatenates the file url from the base url and parameter specifications
    url <- paste(baseurl, extension, sep = "")
    # concatenates the local destination file name from the destination folder address
    # and the file extension
    # result is that the downloaded files will have the same name as the files online
    destfile <- paste(dir, extension, sep = "/")
    
    # downloads the file to the destination folder
    download.file(url, destfile, mode = "wb")
  }
}

# prints a message to indicate the end of the script and provide total elapsed time
print(paste("total time elapsed:", round((proc.time() - start)[[3]]/60, 1), "minutes"), quote = F)
print("SCRIPT COMPLETE", quote = F)