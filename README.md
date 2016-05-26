[![Build Status](https://travis-ci.org/talgalili/heatmaply.png?branch=master)](https://travis-ci.org/talgalili/heatmaply)
[![codecov.io](http://codecov.io/github/talgalili/heatmaply/coverage.svg?branch=master)](http://codecov.io/github/talgalili/heatmaply?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/heatmaply)](http://cran.r-project.org/package=heatmaply)
![](http://cranlogs.r-pkg.org/badges/heatmaply?color=yellow)
![](http://cranlogs.r-pkg.org/badges/grand-total/heatmaply?color=yellowgreen)

# heatmaply

**Table of contents:**

* [Introduction](#introduction)
* [Installation](#installation)
* [Usage](#usage)
* [Credit](#credit)
* [Contact](#contact)


## Please submit features requests

This package is still under active development. If you have features you would like to have added, please submit your suggestions (and bug-reports) at: <https://github.com/talgalili/heatmaply/issues>


## Introduction

A heatmap is a popular graphical method for visualizing high-dimensional data, in which a table of numbers are encoded as a grid of colored cells. The rows and columns of the matrix are ordered to highlight patterns and are often accompanied by dendrograms. Heatmaps are used in many fields for visualizing observations, correlations, missing values patterns, and more.

Interactive heatmaps allow the inspection of specific value by hovering the mouse over a cell, as well as zooming into a region of the heatmap by draging a rectangle around the relevant area.

This work is based on the ggplot2 and plotly.js engine. It produces similar heatmaps as d3heatmap, with the advatange of speed (plotly.js is able to handle larger size matrix), and the ability to zoom from the dendrogram.


## Installation

To install the stable version on CRAN:

```r
# install.packages('heatmaply') # not on CRAN yet, please install using devtools. See the next code chunk for the code
```

To install the latest ("cutting-edge") GitHub version run:

```R
# You'll need devtools
install.packages.2 <- function (pkg) if (!require(pkg)) install.packages(pkg);
install.packages.2('devtools')
# make sure you have Rtools installed first! if not, then run:
#install.packages('installr'); install.Rtools()

devtools::install_github("ropensci/plotly") # you will probably benefit from the latest version of plotly
devtools::install_github('talgalili/heatmaply')
```

And then you may load the package using:

```R
library("heatmaply")
```

## Usage

```r

# mtcars
# x <- heatmapr(mtcars)
library(heatmaply)
heatmaply(iris[,-5], k_row = 3, k_col = 2)
heatmaply(mtcars, k_row = 3, k_col = 2)

```


## Credit

This package is thanks to the amazing work done by MANY people in the open source community. Beyond the many people working on the pipeline of R, thanks should go to the plotly team, and especially to Carson Sievert and others working on the R package of plotly. Also, many of the design elements were inspired by the work done on heatmap, heatmap.2 and d3heatmap, so special thanks goes to the R core team, Gregory R. Warnes, and Joe Cheng from RStudio. The dendrogram side of the package is based on the work in dendextend, in which special thanks should go to Andrie de Vries for his original work on bringing dendrograms to ggplot2 (which evolved into the richer ggdend objects, as implemented in dendextend).


## Contact

You are welcome to:

* submit suggestions and bug-reports at: <https://github.com/talgalili/heatmaply/issues>
* send a pull request on: <https://github.com/talgalili/heatmaply/>
* compose a friendly e-mail to: <tal.galili@gmail.com>


## Latest news

You can see the most recent changes to the package in the [NEWS.md file](https://github.com/talgalili/heatmaply/blob/master/NEWS.md)





## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

