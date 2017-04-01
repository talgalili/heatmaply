## ---- echo = FALSE, message = FALSE--------------------------------------
library(heatmaply)
library(knitr)
knitr::opts_chunk$set(
   # cache = TRUE,
   dpi = 60,
  comment = "#>",
  tidy = FALSE)

# http://stackoverflow.com/questions/24091735/why-pandoc-does-not-retrieve-the-image-file
# < ! -- rmarkdown v1 -->


## ---- eval = FALSE-------------------------------------------------------
#  install.packages('heatmaply')

## ---- eval = FALSE-------------------------------------------------------
#  # You'll need devtools
#  install.packages.2 <- function (pkg) if (!require(pkg)) install.packages(pkg);
#  install.packages.2('devtools')
#  # make sure you have Rtools installed first! if not, then run:
#  #install.packages('installr'); install.Rtools()
#  
#  devtools::install_github("ropensci/plotly")
#  devtools::install_github('talgalili/heatmaply')
#  

## ------------------------------------------------------------------------
library("heatmaply")

