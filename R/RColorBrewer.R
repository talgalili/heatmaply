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
#'
#' @param n the number of colors (>= 1) to be in the palette.
#'
#' @return
#' A character vector of color names.
#'
#' @examples
#' \dontrun{
#'
#' display.brewer.all(n=11,type="div"); title(main = "Divergent color palette")
#' display.brewer.all(n=9,type=c("seq")); title(main = "Sequential color palette")
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

