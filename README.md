# vvdoctor <a href='https://github.com/vusaverse/vvdoctor'><img src="man/figures/hex-vvdoctor.png" style="float:right; height:200px;" height="200" align="right"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/vusaverse/vvdoctor/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/vusaverse/vvdoctor/actions/workflows/check-standard.yaml)

[![CodeFactor](https://www.codefactor.io/repository/github/vusaverse/vvdoctor/badge)](https://www.codefactor.io/repository/github/vusaverse/vvdoctor)
<!-- badges: end -->

`vvdoctor` is an R package/Shiny app that provides a user-friendly interface for data analysis. It allows users to upload data files, visualize the data, perform statistical tests, and interpret the results.

## Usage

``` r
## Install the app
devtools::install_github("vusaverse/vvdoctor")
library(vvdoctor)

## run the app
vvdoctor::run_vvdoctor()
```

-   Uploading Data Files: Click on the "Browse" button to select and upload your data file in CSV or Excel format.

-   Displaying the Dataframe: Once the data file is uploaded, the app will display the data as a dataframe. You can explore the data by scrolling through the table or using the search and filter options.

-   Generating a Histogram: To generate a histogram of a specific variable, select a numeric dependent variable from the dropdown menu. The histogram will be displayed, allowing you to visualize the distribution of the data.

-   Choosing Dependent and Independent Variables: To perform statistical tests, select the dependent and independent variables from the respective dropdown menus. The available variables will be automatically populated based on the uploaded data.

-   Statistical Test Options: Once the variables are selected, the app will provide a list of statistical test options, such as t-tests, ANOVA, or correlation analysis. Choose the desired test and click on the "Run Test" button. The output of the test will be displayed, including the test statistic, p-value, and any additional relevant information.

## Example

Below is a screenshot of an example in `vvdoctor`.

![example](man/figures/vvdoctor_example.png)
