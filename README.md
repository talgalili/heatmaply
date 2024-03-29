<a href="https://github.com/talgalili/heatmaply/actions"
class="pkgdown-devel"><img
src="https://github.com/talgalili/heatmaply/workflows/R-CMD-check/badge.svg"
alt="R-CMD-check" /></a> 
[![codecov](https://codecov.io/gh/talgalili/heatmaply/branch/master/graph/badge.svg?token=cl7mOAKWTI)](https://app.codecov.io/gh/talgalili/heatmaply)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/heatmaply)](https://cran.r-project.org/package=heatmaply)
![](https://cranlogs.r-pkg.org/badges/heatmaply?color=yellow)
![](https://cranlogs.r-pkg.org/badges/grand-total/heatmaply?color=yellowgreen)

# heatmaply

**Table of contents:**

- [heatmaply](#heatmaply)
  - [Screenshot demo](#screenshot-demo)
  - [Introduction](#introduction)
    - [Please submit features requests](#please-submit-features-requests)
    - [Latest news](#latest-news)
    - [Code of conduct](#code-of-conduct)
  - [Installation](#installation)
  - [Usage](#usage)
    - [Saving your heatmaply into a file](#saving-your-heatmaply-into-a-file)
    - [Replacing d3heatmap with heatmaply](#replacing-d3heatmap-with-heatmaply)
  - [Acknowledgements](#acknowledgements)
  - [How to cite the heatmaply package](#how-to-cite-the-heatmaply-package)
  - [Contact](#contact)


## Screenshot demo

![](https://i.imgur.com/qdUCKlg.gif)



## Introduction

A heatmap is a popular graphical method for visualizing high-dimensional data, in which a table of numbers are encoded as a grid of colored cells. The rows and columns of the matrix are ordered to highlight patterns and are often accompanied by dendrograms. Heatmaps are used in many fields for visualizing observations, correlations, missing values patterns, and more.

Interactive heatmaps allow the inspection of specific value by hovering the mouse over a cell, as well as zooming into a region of the heatmap by dragging a rectangle around the relevant area.

This work is based on the ggplot2 and plotly.js engine. It produces similar heatmaps as d3heatmap (or the static heatmap.2 from gplots), with the advantage of more features such as speed (plotly.js is able to handle larger size matrix), sidebar annotation, and the ability to zoom from the dendrogram.


### Please submit features requests

This package is still under active development. If you have features you would like to have added, please submit your suggestions (and bug-reports) at: <https://github.com/talgalili/heatmaply/issues>


### Latest news

You can see the most recent changes to the package in the [NEWS.md file](https://talgalili.github.io/heatmaply/news/index.html)



### Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/talgalili/heatmaply/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.



## Installation

To install the stable version on CRAN:

```r
install.packages('heatmaply')
```

To install the latest ("cutting-edge") GitHub version run:

```R

# good packages to install for this to work smoothly:

install.packages(c("Rcpp","ggplot2","munsell","htmltools","DBI","assertthat",
"gridExtra","digest","fpc","TSP","registry","gclus","gplots","RColorBrewer",
"stringr","labeling","yaml"))

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

Quick example:

```r
library(heatmaply)
heatmaply(mtcars, k_row = 3, k_col = 2)
```

**For more (interactive) examples see the [online vignette on CRAN](https://CRAN.R-project.org/package=heatmaply/vignettes/heatmaply.html)**

There are also more complex biological examples of using heatmaply in the R package [heatmaplyExamples](https://github.com/talgalili/heatmaplyExamples) (hosted on github). Here are direct links for available examples:


* [Introduction to heatmaply](https://CRAN.R-project.org/package=heatmaply/vignettes/heatmaply.html)
* General biological examples
  - [Using heatmaply with the measles data set](https://cdn.jsdelivr.net/gh/talgalili/heatmaplyExamples@master/inst/doc/measles.html)
  - [Using heatmaply with famous data sets](https://cdn.jsdelivr.net/gh/talgalili/heatmaplyExamples@master/inst/doc/heatmaply_examples.html)
* Reproducing heatmaps from papers published in Nature
  - [Using heatmaply to reproduce Nature (2015) Kotsyfakis et al.](https://cdn.jsdelivr.net/gh/talgalili/heatmaplyExamples@master/inst/doc/reproducing_Nature_2015_Kotsyfakis.html)
  - [Using heatmaply to reproduce Nature (2015) Alfano et al.](https://cdn.jsdelivr.net/gh/talgalili/heatmaplyExamples@master/inst/doc/reproducing_Nature_2015_Alfano.html)
* Using heatmaply with gene expression data  
  - Visualization of raw and voom-transformed data (all genes) (url offline, you could combile this on your own)
  - Visualization of raw data (median-centered data, PAM50 genes only) (url offline, you could combile this on your own)
  - Visualization of voom-transformed data (median-centered data, PAM50 genes only) (url offline, you could combile this on your own)
  - [Using heatmaply with non-centred RNAseq heatmaps (PAM50 genes) ](https://cdn.jsdelivr.net/gh/talgalili/heatmaplyExamples@master/inst/doc/non_centred_heatmaps.html)
* General examples
  - [Using heatmaply for visualizing glmnet coefficient path](https://cdn.jsdelivr.net/gh/talgalili/heatmaplyExamples@master/inst/doc/glmnet.html)



### Saving your heatmaply into a file

You can save an interactive version of your heatmaply into an HTML file using the following code:

```r
dir.create("folder")
library(heatmaply)
heatmaply(mtcars, file = "folder/heatmaply_plot.html")
browseURL("folder/heatmaply_plot.html")
```

Similar code can be used for saving a static file (png/jpeg/pdf)

```r
dir.create("folder")
library(heatmaply)
# Before the first time using this code you may need to first run:
# webshot::install_phantomjs()
heatmaply(mtcars, file = "folder/heatmaply_plot.png")
browseURL("folder/heatmaply_plot.png")
```

### Replacing d3heatmap with heatmaply

The package d3heatmap (https://github.com/talgalili/d3heatmap) is no not actively maintained.

If users are interested in replacing their `d3heatmap` functions, this is easily done with `heatmaply`:

* Function calls `d3heatmap::d3heatmap()` can be replaced with `heatmaply::heatmaply()`
* Users of `d3heatmap` for `shiny` applications can use the following functions:
    * `d3heatmap::d3heatmapOutput()` can be replaced with `plotly::plotlyOutput()`
    * `d3heatmap::renderD3heatmap()` can be replaced with `plotly::renderPlotly()`


## Acknowledgements


This package is thanks to the amazing work done by MANY people in the open source community. Beyond the many people working on the pipeline of R, thanks should go to the people working on ggplot2 (Hadley Wickham, etc.) and plotly (Carson Sievert, etc.). Also, many of the design elements were inspired by the work done on heatmap, heatmap.2 and d3heatmap, so special thanks goes to the R core team, Gregory R. Warnes, and Joe Cheng from RStudio. The dendrogram side of the package is based on the work in dendextend, in which special thanks should go to Andrie de Vries for his original work on bringing dendrograms to ggplot2 (which evolved into the richer ggdend objects, as implemented in dendextend). 

The work on heatmaply was done by Tal Galili, Alan O'Callaghan, and Jonathan Sidi (mostly on shinyHeatmaply).


**Funding**: This work was supported in part by the European Union Seventh Framework Programme (FP7/2007-2013) under grant agreement no. 604102 (Human Brain Project).  



## How to cite the heatmaply package

The methods within the package can be cited as:

    Tal Galili, Alan O'Callaghan, Jonathan Sidi, Carson Sievert; heatmaply: an R package for creating
    interactive cluster heatmaps for online publishing, Bioinformatics, , btx657,
    https://doi.org/10.1093/bioinformatics/btx657

A BibTeX entry for LaTeX users is

    @Article{,
      author = {{Galili} and {Tal} and {O'Callaghan} and {Alan} and {Sidi} and {Jonathan} and {Sievert} and {Carson}},
      title = {heatmaply: an R package for creating interactive cluster heatmaps for online publishing},
      journal = {Bioinformatics},
      year = {2017},
      doi = {10.1093/bioinformatics/btx657},
      url = {https://dx.doi.org/10.1093/bioinformatics/btx657},
      eprint = {https://academic.oup.com/bioinformatics/article-pdf/doi/10.1093/bioinformatics/btx657/21358327/btx657.pdf},
    }

This free open-source software implements academic research by the authors and co-workers. If you use
it, please support the project by citing the appropriate journal articles.




## Contact

You are welcome to:

* Ask questions on: <https://stackoverflow.com/questions/tagged/heatmaply>
* Submit suggestions and bug-reports at: <https://github.com/talgalili/heatmaply/issues>
* Send a pull request on: <https://github.com/talgalili/heatmaply/>
* Compose a friendly e-mail to: <tal.galili@gmail.com>

