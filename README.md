# veRification
Shiny app for the analysis of laboratory assay verification data.

To use this app, first install R, which can be downloaded for free [here](https://cran.r-project.org/).  It is recommended to then use RStudio on top of the base R installation. This can be downloaded for free [here](https://posit.co/downloads/).

The app can then be installed as an R package using the following command (requiring the `devtools` R package):

`devtools::install_github("ed-wilkes/veRification", dependencies = TRUE)`

The app can then be run through the use of the `veRification::run_app()` function.
