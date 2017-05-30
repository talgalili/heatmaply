# Copyright (C) Tal Galili
#
# This file is part of heatmaply.
#
# heatmaply is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# heatmaply is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#


# library(RColorBrewer)
#
# The sequential palettes names are
# Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
#
# All the sequential palettes are available in variations from 3 different values up to 9 different values.
#
# The diverging palettes are
# BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
#
# All the diverging palettes are available in variations from 3 different values up to 11 different values.
#




# importFrom("grDevices", "rgb")
# importFrom("methods", "as")


#' @importFrom grDevices rgb
#' @importFrom methods as






#' @title RColorBrewer color Ramp Palette
#' @name RColorBrewer_colors
#'
#' @description
#' Functions for getting the colors of RColorBrewer (i.e.: \link{brewer.pal}) without the limitation of only 9/11
#' color values, based on \link{colorRampPalette}.
#'
#' For sequential palettes this is not essential since we have \link{viridis}. But for diverging palettes
#' this is quit essential.
#'
#' The sequential palettes names are
#' Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
#'
#' The diverging palettes are
#' BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
#' And also cool_warm. The cool_warm palette is based on Kenneth Moreland's proposal (see ref).
#' It goes from blue (cool) to ref (warm), based on well thought-out design elements.
#'
#' @references
#' * Moreland, Kenneth. "Diverging color maps for scientific visualization." Advances in Visual Computing (2009): 92-103.
#' url: http://www.kennethmoreland.com/color-maps/
#' The code was provided here: http://stackoverflow.com/a/44073011/256662
#' Thanks to the user YAK, who relied on the code from the Rgnuplot package
#' (which is duplicated here, in order to save the need to import the entire package)
#'
#'
#'
#' @param n the number of colors (>= 1) to be in the palette.
#'
#' @return
#' A character vector of color names.
#'
#' @examples
#' \dontrun{
#'
#' library(RColorBrewer)
#' display.brewer.all(n=11,type="div"); title(main = "Divergent color palette")
#' display.brewer.all(n=9,type=c("seq")); title(main = "Sequential color palette")
#'
#'
#'
#' img <- function(obj, nam) {
#'   image(1:length(obj), 1, as.matrix(1:length(obj)), col=obj,
#'         main = nam, ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
#' }
#'
#' par(mfrow = c(10,1))
#' img(rev(cool_warm(500)), "cool_warm, (Moreland 2009)")
#' img(RdBu(500), "RdBu")
#' img(BrBG(500), "BrBG")
#' img(PiYG(500), "PiYG")
#' img(PRGn(500), "PRGn")
#' img(PuOr(500), "PuOr")
#' img(RdGy(500), "RdGy")
#' img(RdYlBu(500), "RdYlBu")
#' img(RdYlGn(500), "RdYlGn")
#' img(Spectral(500), "Spectral")
#'
#'
#' library(heatmaply)
#' heatmaply(cor(mtcars), colors = PiYG, limits = c(-1,1))
#' heatmaply(cor(mtcars), colors = RdBu, limits = c(-1,1))
#'
#' }
NULL



#' @export
#' @rdname RColorBrewer_colors
BrBG <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "BrBG"))

#' @export
#' @rdname RColorBrewer_colors
PiYG <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))

#' @export
#' @rdname RColorBrewer_colors
PRGn <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "PRGn"))

#' @export
#' @rdname RColorBrewer_colors
PuOr <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "PuOr"))

#' @export
#' @rdname RColorBrewer_colors
RdBu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))

#' @export
#' @rdname RColorBrewer_colors
RdGy <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdGy"))

#' @export
#' @rdname RColorBrewer_colors
RdYlBu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlBu"))

#' @export
#' @rdname RColorBrewer_colors
RdYlGn <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))

#' @export
#' @rdname RColorBrewer_colors
Spectral <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))



#' @export
#' @rdname RColorBrewer_colors
Blues <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))


#' @export
#' @rdname RColorBrewer_colors
BuGn <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "BuGn"))


#' @export
#' @rdname RColorBrewer_colors
BuPu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "BuPu"))


#' @export
#' @rdname RColorBrewer_colors
GnBu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "GnBu"))


#' @export
#' @rdname RColorBrewer_colors
Greens <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Greens"))


#' @export
#' @rdname RColorBrewer_colors
Greys <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Greys"))


#' @export
#' @rdname RColorBrewer_colors
Oranges <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Oranges"))




#' @export
#' @rdname RColorBrewer_colors
OrRd <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "OrRd"))


#' @export
#' @rdname RColorBrewer_colors
PuBu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "PuBu"))


#' @export
#' @rdname RColorBrewer_colors
PuBuGn <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))


#' @export
#' @rdname RColorBrewer_colors
PuRd <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "PuRd"))


#' @export
#' @rdname RColorBrewer_colors
Purples <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Purples"))


#' @export
#' @rdname RColorBrewer_colors
RdPu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "RdPu"))


#' @export
#' @rdname RColorBrewer_colors
Reds <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Reds"))




#' @export
#' @rdname RColorBrewer_colors
YlGn <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlGn"))


#' @export
#' @rdname RColorBrewer_colors
YlGnBu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))


#' @export
#' @rdname RColorBrewer_colors
YlOrBr <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrBr"))


#' @export
#' @rdname RColorBrewer_colors
YlOrRd <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))









# install.packages("Rgnuplot")
# Rgnuplot:::GpdivergingColormap

GpdivergingColormap <- function (s, rgb1, rgb2, outColorspace = "sRGB")
{
  LabToMsh <- function(Lab) {
    L <- Lab@coords[1]
    a <- Lab@coords[2]
    b <- Lab@coords[3]
    M <- sqrt(L * L + a * a + b * b)
    s <- (M > 0.001) * acos(L/M)
    h <- (s > 0.001) * atan2(b, a)
    c(M, s, h)
  }
  MshToLab <- function(Msh) {
    M <- Msh[1]
    s <- Msh[2]
    h <- Msh[3]
    L <- M * cos(s)
    a <- M * sin(s) * cos(h)
    b <- M * sin(s) * sin(h)
    colorspace::LAB(L, a, b)
  }
  AngleDiff <- function(a1, a2) {
    v1 <- matrix(c(cos(a1), sin(a1)), 1, 2, byrow = TRUE)
    v2 <- matrix(c(cos(a2), sin(a2)), 1, 2, byrow = TRUE)
    x <- acos(sum(v1 * v2))
    x
  }
  AdjustHue <- function(msh, unsatM) {
    if (msh[1] >= unsatM - 0.1)
      h <- msh[3]
    else {
      hueSpin <- (msh[2] * sqrt(unsatM^2 - msh[1]^2)/(msh[1] *
                                                        sin(msh[2])))
      if (msh[3] > -0.3 * pi)
        h <- msh[3] + hueSpin
      else h <- msh[3] - hueSpin
    }
    h
  }
  divergingMap1val <- function(s, rgb1, rgb2, outColorspace = "sRGB") {
    msh1 <- LabToMsh(as(rgb1, "LAB"))
    msh2 <- LabToMsh(as(rgb2, "LAB"))
    if (msh1[2] > 0.05 & msh2[2] > 0.05 & AngleDiff(msh1[3],
                                                    msh2[3]) > pi/3) {
      Mmid <- max(88, msh1[1], msh2[1])
      if (s < 0.5) {
        msh2[1] <- Mmid
        msh2[2] <- 0
        msh2[3] <- 0
        s <- 2 * s
      }
      else {
        msh1[1] <- Mmid
        msh1[2] <- 0
        msh1[3] <- 0
        s <- 2 * s - 1
      }
    }
    if ((msh1[2] < 0.05) & (msh2[2] > 0.05))
      msh1[3] <- AdjustHue(msh2, msh1[1])
    else if ((msh2[2] < 0.05) & (msh1[2] > 0.05))
      msh2[3] <- AdjustHue(msh1, msh2[1])
    mshTmp <- msh1
    mshTmp[1] <- (1 - s) * msh1[1] + s * msh2[1]
    mshTmp[2] <- (1 - s) * msh1[2] + s * msh2[2]
    mshTmp[3] <- (1 - s) * msh1[3] + s * msh2[3]
    as(MshToLab(mshTmp), outColorspace)
  }
  dvmap <- matrix(0, length(s), 3)
  for (n in 1:length(s)) dvmap[n, ] <- divergingMap1val(s[n],
                                                        rgb1, rgb2, outColorspace)@coords
  dvmap
}



#' @export
#' @rdname RColorBrewer_colors
cool_warm  <- function(n) {
  colormap <- GpdivergingColormap(seq(0,1,length.out=n),
                                  rgb1 = colorspace::sRGB( 0.230, 0.299, 0.754),
                                  rgb2 = colorspace::sRGB( 0.706, 0.016, 0.150),
                                  outColorspace = "sRGB")
  colormap[colormap>1] <- 1 # sometimes values are slightly larger than 1
  colormap <- grDevices::rgb(colormap[,1], colormap[,2], colormap[,3])
  colormap
}

# img <- function(obj, nam) {
#   image(1:length(obj), 1, as.matrix(1:length(obj)), col=obj,
#         main = nam, ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
# }
#
# par(mfrow = c(2,1))
# img(red_blue_diverging_colormap(500), "Cool-warm, (Moreland 2009)")
#
# library(heatmaply)
# img(rev(RdBu(500)), "RdBu")
#

