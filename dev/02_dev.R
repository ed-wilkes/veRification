# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.package('attachment') # if needed.
attachment::att_amend_desc()
usethis::use_package( "bayestestR" )
usethis::use_package( "brms" )
usethis::use_package( "caret" )
usethis::use_package( "DescTools" )
usethis::use_package( "dplyr" )
usethis::use_package( "DT" )
usethis::use_package( "epiR" )
usethis::use_package( "ggplot2" )
usethis::use_package( "magrittr" )
usethis::use_package( "modelr" )
usethis::use_package( "plotly" )
usethis::use_package( "readxl" )
usethis::use_package( "rlist" )
usethis::use_package( "rstan" )
# usethis::use_package( "rstanarm" )
usethis::use_package( "shinycssloaders" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinydashboardPlus" )
usethis::use_package( "shinyFeedback" )
usethis::use_package( "StanHeaders" )
usethis::use_package( "stringr" )
usethis::use_package( "tidybayes" )
usethis::use_package( "tidyr" )
usethis::use_package( "yardstick" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "imprecision_data", open = FALSE)
usethis::use_data_raw(name = "eqa_data", open = FALSE)
usethis::use_data_raw(name = "reference_data", open = FALSE)
usethis::use_data_raw(name = "comparison_data", open = FALSE)
usethis::use_data_raw(name = "roc_data", open = FALSE)

# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
