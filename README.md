# veRification
Shiny app for the analysis of laboratory assay verification data.

To use this app, first install R, which can be downloaded for free [here](https://cran.r-project.org/).  It is recommended to then use RStudio on top of the base R installation. This can be downloaded for free [here](https://posit.co/downloads/).

The app can then be installed as an R package using the following command (requiring the `devtools` R package):

`devtools::install_github("ed-wilkes/veRification", dependencies = TRUE)`

The app can then be run through the use of the `veRification::run_app()` function.

Example data for each module can be found within the `veRification/inst/extdata` folder of the package.

## Citations
If you find this package useful for your own work, please consider citing the associated publication:

```
@article{wilkes2023cclm,
    title={veRification: an R Shiny application for laboratory method verification and validation },
    author={Wilkes, Edmund H},
    journal={Clin Chem Lab Med},
    year={2023}
}
```
