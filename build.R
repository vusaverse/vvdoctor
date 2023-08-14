############################################### #############################
### build_vvmover.R
############################################### #############################
### R code for Student Analytics VU University Amsterdam
### Copyright 2018 VU
### Web Page: http://www.vu.nl
### Contact: Theo Bakker (t.c.bakker@vu.nl)
###
### Filename: build_vvMover.R
### Purpose: This script is used to build and release the vvmover pacakge
###
### Dependencies: None
###
### Datasets used: Datasets
###
### Comments:
### 1) None
###
############################################### #############################
### TODO:
### 1) None
###
############################################### #############################
### History:
### 28-09-2018: JvZ: Create file
############################################### #############################
##
## =============================================== ==============================
## Install and load the packages
build_packages <- c("devtools",
                    "usethis",
                    "renv")

## install the packages that are not installed yet
lapply(build_packages[which(!build_packages %in% installed.packages())],
       install.packages)

renv::load("../sa-scripts/")

## Set project name
package_name <- basename(rstudioapi::getActiveProject())

## load the packages
invisible(lapply(build_packages,
                 library,
                 character.only = TRUE))

## =============================================== ==============================
## Build the new vusa package


## Check if the package is correct and can be built.
devtools::document()
devtools::check(manual = T)
devtools::build_manual(path = paste0("G:/DSZ/SA2016/Datasets/Packages/package_man/", package_name, "/"))

## Commit your changes in Smartgit (or manually in the terminal, next to console)
# If you don't do this, you will get the question with the (use_version) command below:
#There are uncommitted changes and you're about to bump version
#Do you want to proceed anyway?
# Before answering this question you can also commit, but if you don't
# nothing is changed


## Increment the version number and check in that change in Git
usethis::use_version()

## Build the package, and release it to the correct folder
devtools::build(path = paste0("G:/DSZ/SA2016/Datasets/Packages/", package_name, "/"))

## Make use of the development version again
usethis::use_dev_version()
