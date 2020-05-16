library(tools)
library(readr)
library(stringr)
library(fs)
library(dplyr)

#### next step: 
#~~ add specific warning message for if the file path is wrong

#' extract_huc_data()
#' 
#' This function takes a list of watershed Huc 8s and extracts daily mean forcing dayment data as well as relevant climate, geology, hyrdology, area, soil, topography and vegetation data
#'
#' @param daymet_dir directory path to daymet data should look something like: "~/basin_dataset_public_v1p2/basin_mean_forcing/daymet" this directory pathway MUST end with the daymet/ folder 
#' -- within this folder there should more directories (labeled '01', '02', '03' etc) representing different huc2 watersheds
#' @param attr_dir directory path for attributes data (e.g: "~/camels_attributes_v2.0")
#' @param huc8_names vector of huc8 watershed IDs
#'
#' @return named list for each huc8 with 8 nested named lists, one for each attribut of interest
#' @export
#'
#' @examples
#' 
#' 
#' daymet_dir <- "~/CAMELS/basin_dataset_public_v1p2/basin_mean_forcing/daymet"
#' attr_dir <- "~/CAMELS/camels_attributes_v2.0"
#' huc8_names <- c("01013500", "08269000")
#' 
#' data <- extract_data(daymet_dir = daymet_dir, attr_dir = attr_dir, huc8_names = huc8_names)
#' 
#' ## list of hucs queried
#' > names(data)
#' [1] "01013500" "08269000"
#' 
#' ## names of dataframes within first list item
#' > names(data[[1]])
#' [1] "daymet"       "camels_clim"  "camels_geol"  "camels_hydro"
#' [5] "camels_name"  "camels_soil"  "camels_topo"  "camels_vege" 
#' 
#' ## same results using the huc ID for first ID in entered vector
#' > names(data[["01013500"]])
#' [1] "daymet"       "camels_clim"  "camels_geol"  "camels_hydro"
#' [5] "camels_name"  "camels_soil"  "camels_topo"  "camels_vege" 
#' 
#' ## getting data for specific attribute from huc 01013500
#' > data[["01013500"]]$camels_name
#   A tibble: 1 x 3
#' gauge_id huc_02 gauge_name                      
#'    <chr>    <chr>  <chr>                           
#'  1 01013500 01     Fish River near Fort Kent, Maine
#'  
#' ## similarly using the $ syntax:
#' > data$`08269000`$camels_name
#' A tibble: 1 x 3
#' gauge_id huc_02 gauge_name                      
#'    <chr>    <chr>  <chr>                           
#'  1 08269000 13     RIO PUEBLO DE TAOS NEAR TAOS, NM

extract_huc_data <- function(daymet_dir, attr_dir, huc8_names) {
  ### check that filepaths exist
  if(!dir_exists(daymet_dir)) stop("daymet directory does not exist, please check file path")
  if(str_sub(daymet_dir, start = -6) != "daymet") stop("daymet directory is incorrect, please check file path")
  if(!dir_exists(attr_dir)) stop("attribute directory does not exist, please check file path")

  
  ## create empty huc list
  huc_list <- list()
  
  
  ## loop through each of the huc8 that were entered
  ## there are 2 steps in this loop
  ##~ step 1: get daymet data and add as item 1 w/in attribute list
  ##~ step 2: get attribute data and add as items 2:8 in list
  ## this whole list is added as item i in the larger huc list
  for(i in 1:length(huc8_names)) {
    
    #create empty attribute/data list
    attr_list <- list()
    
    #### STEP 1 ####
    ## get file path for the Huc8's daymet data
    ##~ we do this because the huc2 directories is not reliable
    # this should return a df with 1 row and column, the contents of wihch is the filepath to he daymet data for this huc
    daymet_file <- data.frame(files = list.files(daymet_dir, 
                                                 recursive = T)) %>% 
      filter(str_detect(files, huc8_names[i]))
    
    # if this dataframe is empty, then that huc does not exist in the daymet dataset ==> skip to next loop
    if(nrow(daymet_file) == 0) {
      warning(sprintf("ID: '%s' not found \n check that this ID is accurate and present within the CAMELS dataset \n if all IDs are correct, check that daymet directory is correct", huc8_names[i])) 
      huc_list[[i]] <- NULL
      next
    }

    
    ### read the met file in
    daymet_data <- read_table2(
      file.path(daymet_dir, as.character(daymet_file[1,1])), 
      skip = 3,
      col_types = cols())
    
    # add this as first item within this huc's list
    attr_list[[1]] <- daymet_data
    
    
    #### STEP 2 ####
    ### get file paths for each file in attribute directory
    att_files <- list.files(file.path(attr_dir),
                            pattern = "^camels_[^a]", # remove the xls too; will be better with the extension instead
                            full.names = TRUE)
    
    ### loop through each attribute, adding each as a new nested list item
    for (j in 1:length(att_files)) {
      
      attr_list[[j+1]] <- read_delim(att_files[j], 
                                     delim = ";",
                                     col_types = cols()) %>% 
        filter(gauge_id == huc8_names[i])
      
    }
    
    ## name these nested lists
    #~ first is daymet, the following are named based on the name of the text file
    list_names <- c("mean_forcing_daymet", file_path_sans_ext(basename(att_files)))
    names(attr_list) <- list_names
    
    ## add this whole list of data for this huc to this hucs list
    huc_list[[i]] <- attr_list
    
    
  }
  ## name each parent list based on the names of the hucs
  names(huc_list) <- huc8_names
  
  return(huc_list)
  
}



##### test function #######


# Constant
daymet_dir <- "~/CAMELS/basin_dataset_public_v1p2/basin_mean_forcing/daymet"
attr_dir <- "~/CAMELS/camels_attributes_v2.0"

# To become a variable
huc8_names <- c("01013500", "08269000", "test", "10259200")



data <- extract_huc_data(daymet_dir = daymet_dir, attr_dir = attr_dir, huc8_names = huc8_names)

