---
title: "Introduction to heatmaply"
date: "2017-02-22"
author: "Tal Galili"
output:
  html_document:
    self_contained: yes
    toc: true    
---
<!--
%\VignetteEngine{knitr::rmarkdown}
%\VignetteIndexEntry{Introduction to heatmaply}
-->





**Author**: [Tal Galili](http://www.r-statistics.com/) ( Tal.Galili@gmail.com )



Introduction
============

A heatmap is a popular graphical method for visualizing high-dimensional data, in which a table of numbers are encoded as a grid of colored cells. The rows and columns of the matrix are ordered to highlight patterns and are often accompanied by dendrograms. Heatmaps are used in many fields for visualizing observations, correlations, missing values patterns, and more.

Interactive heatmaps allow the inspection of specific value by hovering the mouse over a cell, as well as zooming into a region of the heatmap by draging a rectangle around the relevant area.

This work is based on the ggplot2 and plotly.js engine. It produces similar heatmaps as d3heatmap, with the advantage of speed (plotly.js is able to handle larger size matrix), and the ability to zoom from the dendrogram.


Installation
============

To install the stable version on CRAN:


```r
install.packages('heatmaply')
```

To install the GitHub version:


```r
# You'll need devtools
install.packages.2 <- function (pkg) if (!require(pkg)) install.packages(pkg);
install.packages.2('devtools')
# make sure you have Rtools installed first! if not, then run:
#install.packages('installr'); install.Rtools()

devtools::install_github("ropensci/plotly") 
devtools::install_github('talgalili/heatmaply')
```


And then you may load the package using:


```r
library("heatmaply")
```

Usage
======


Default
-------------




































