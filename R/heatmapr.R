# prepare the heatmapr object.

`%||%` <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

#' Creates a heatmapr object
#'
#' An object of class heatmapr includes all the needed information
#' for producing a heatmap. The goal is to seperate the pre-processing of the
#' heatmap elements from the graphical rendaring of the object, which could be done
#' using plotly (but potentially also with other graphical devices).
#'
#' @param x A numeric matrix
#'   Defaults to \code{TRUE} unless \code{x} contains any \code{NA}s.
#' @param theme A custom CSS theme to use. Currently the only valid values are
#'   \code{""} and \code{"dark"}. \code{"dark"} is primarily intended for
#'   standalone visualizations, not R Markdown or Shiny.
#' @param colors Either a colorbrewer2.org palette name (e.g. \code{"YlOrRd"} or
#'   \code{"Blues"}), or a vector of colors to interpolate in hexadecimal
#'   \code{"#RRGGBB"} format, or a color interpolation function like
#'   \code{\link[grDevices]{colorRamp}}.
#' @param width Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#'
#' @param xaxis_height Size of axes, in pixels.
#' @param yaxis_width Size of axes, in pixels.
#' @param xaxis_font_size Font size of axis labels, as a CSS size (e.g. "14px" or "12pt").
#' @param yaxis_font_size Font size of axis labels, as a CSS size (e.g. "14px" or "12pt").
#'
#' @param brush_color The base color to be used for the brush. The brush will be
#'   filled with a low-opacity version of this color. \code{"#RRGGBB"} format
#'   expected.
#' @param show_grid \code{TRUE} to show gridlines, \code{FALSE} to hide them, or
#'   a numeric value to specify the gridline thickness in pixels (can be a
#'   non-integer).
#' @param anim_duration Number of milliseconds to animate zooming in and out.
#'   For large \code{x} it may help performance to set this value to \code{0}.
#'
#' @param Rowv determines if and how the row dendrogram should be reordered.	By default, it is TRUE, which implies dendrogram is computed and reordered based on row means. If NULL or FALSE, then no dendrogram is computed and no reordering is done. If a dendrogram, then it is used "as-is", ie without any reordering. If a vector of integers, then dendrogram is computed and reordered based on the order of the vector.
#' @param Colv determines if and how the column dendrogram should be reordered.	Has the options as the Rowv argument above and additionally when x is a square matrix, Colv = "Rowv" means that columns should be treated identically to the rows.
#' @param distfun function used to compute the distance (dissimilarity) between both rows and columns. Defaults to dist.
#' @param hclustfun function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms. Defaults to hclust.
#' @param dendrogram character string indicating whether to draw 'none', 'row', 'column' or 'both' dendrograms. Defaults to 'both'. However, if Rowv (or Colv) is FALSE or NULL and dendrogram is 'both', then a warning is issued and Rowv (or Colv) arguments are honoured.
#' @param reorderfun function(d, w) of dendrogram and weights for reordering the row and column dendrograms. The default uses stats{reorder.dendrogram}
#'
#' @param k_row an integer scalar with the desired number of groups by which to color the dendrogram's branches in the rows (uses \link[dendextend]{color_branches})
#' @param k_col an integer scalar with the desired number of groups by which to color the dendrogram's branches in the columns (uses \link[dendextend]{color_branches})
#'
#' @param symm logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.
#' @param revC logical indicating if the column order should be reversed for plotting.
#' Default (when missing) - is FALSE, unless symm is TRUE.
#' This is useful for cor matrix.
#'
#' @param scale character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "none".
#' @param na.rm logical indicating whether NA's should be removed.
#'
#' @param digits integer indicating the number of decimal places to be used by \link{round} for 'label'.
#' @param cellnote (optional) matrix of the same dimensions as \code{x} that has the human-readable version of each value, for displaying to the user on hover. If \code{NULL}, then \code{x} will be coerced using \code{\link{as.character}}.
#' If missing, it will use \code{x}, after rounding it based on the \code{digits} parameter.
#' @param cellnote_scale logical (default is TRUE). IF cellnote is missing and x is used,
#' should cellnote be scaled if x is also scaled?
#'
#' @param cexRow positive numbers. If not missing, it will override \code{xaxis_font_size}
#' and will give it a value cexRow*14
#' @param cexCol positive numbers. If not missing, it will override \code{yaxis_font_size}
#' and will give it a value cexCol*14
#'
#' @param labRow character vectors with row labels to use (from top to bottom); default to rownames(x).
#' @param labCol character vectors with column labels to use (from left to right); default to colnames(x).
#'
#' @param row_side_colors,col_side_colors data.frame of factors to produce
#'    row/column side colors in the style of heatmap.2/heatmap.3.
#'    col_side_colors should be "wide", ie be the same dimensions
#'    as the column side colors it will produce.
#'
#' @param seriate character indicating the method of matrix sorting (default: "OLO").
#' Implemented options include:
#' "OLO" (Optimal leaf ordering, optimzes the Hamiltonian path length that is restricted by the dendrogram structure - works in O(n^4) )
#' "mean" (sorts the matrix based on the reorderfun using marginal means of the matrix. This is the default used by \link[gplots]{heatmap.2}),
#' "none" (the default order produced by the dendrogram),
#' "GW" (Gruvaeus and Wainer heuristic to optimze the Hamiltonian path length that is restricted by the dendrogram structure)
#'
#' @param ... currently ignored
#'
#' @export
#' @source
#' The interface was designed based on \link{heatmap}, \link[gplots]{heatmap.2}, and \link[d3heatmap]{d3heatmap}.
#'
#' @seealso
#' \link{heatmap}, \link[gplots]{heatmap.2},  \link[d3heatmap]{d3heatmap}
#'
#' @examples
#' library(heatmaply)
#' hm <- heatmapr(mtcars, scale = "column", colors = "Blues")
#' heatmaply(hm)
#'
heatmapr <- function(x,

                      ## dendrogram control
                      Rowv = TRUE,
                      Colv = if (symm) "Rowv" else TRUE,
                      distfun = dist,
                      hclustfun = hclust,
                      dendrogram = c("both", "row", "column", "none"),
                      reorderfun = function(d, w) reorder(d, w),

                      k_row,
                      k_col,

                      symm = FALSE,
                      revC,

                      ## data scaling
                      scale = c("none", "row", "column"),
                      na.rm = TRUE,

                      labRow = rownames(x),
                      labCol = colnames(x),

                      cexRow,
                      cexCol,

                      ## value formatting
                      digits = 3L,
                      cellnote,
                      cellnote_scale = TRUE,

                      ##TODO: decide later which names/conventions to keep
                      theme = NULL,
                      colors = "RdYlBu",
                      width = NULL, height = NULL,
                      xaxis_height = 80,
                      yaxis_width = 120,
                      xaxis_font_size = NULL,
                      yaxis_font_size = NULL,
                      brush_color = "#0000FF",
                      show_grid = TRUE,
                      anim_duration = 500,

                      row_side_colors = NULL,
                      col_side_colors = NULL,
                      seriate = c("OLO", "mean", "none", "GW"),

                      ...
) {

  ## x is a matrix!
  ##====================
  if(!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if(!is.matrix(x)) stop("x must be a matrix")


  seriate <- match.arg(seriate)

  nr <- nrow(x)
  nc <- ncol(x)
  ### TODO: debating if to include this or not:
  #   if(nr <= 1 || nc <= 1)
  #     stop("`x' must have at least 2 rows and 2 columns")


  ## Labels for Row/Column
  ##======================
  rownames(x) <- labRow %||% as.character(1:nrow(x))
  colnames(x) <- labCol %||% as.character(1:ncol(x))

  if(!missing(cexRow)) {
    if(is.numeric(cexRow)) {
      xaxis_font_size <- cexRow * 14
    } else {
      warning("cexRow is not numeric. It is ignored")
    }
  }
  if(!missing(cexCol)) {
    if(is.numeric(cexCol)) {
      yaxis_font_size <- cexCol * 14
    } else {
      warning("cexCol is not numeric. It is ignored")
    }
  }


  ## Dendrograms for Row/Column
  ##=======================
  dendrogram <- match.arg(dendrogram)

  # Use dendrogram argument to set defaults for Rowv/Colv
  if (missing(Rowv)) {
    Rowv <- dendrogram %in% c("both", "row")
  }
  if (missing(Colv)) {
    Colv <- dendrogram %in% c("both", "column")
  }


  # switch("c",
  #        "a" = 4,
  #        "b" = 5,
  #        "c" = {
  #          5+3
  #          2+1
  #        })

  if (isTRUE(Rowv)) {
    Rowv <- switch(seriate,
                     "mean" = rowMeans(x, na.rm = na.rm),
                     "none" = 1:nrow(x),
                     "OLO" = {
                                dist_x <- distfun(x) # dist is on the rows by default
                                hc_x <- hclustfun(dist_x)
                                dend_x <- as.dendrogram(hc_x)
                                dend_x2 <- dendextend::seriate_dendrogram(dend_x, dist_x, method = "OLO")
                                dend_x2
                             },
                      "GW" = {
                        dist_x <- distfun(x) # dist is on the rows by default
                        hc_x <- hclustfun(dist_x)
                        dend_x <- as.dendrogram(hc_x)
                        dend_x2 <- dendextend::seriate_dendrogram(dend_x, dist_x, method = "GW")
                        dend_x2
                      }

                   )



  }
  if (is.numeric(Rowv)) {
    Rowv <- reorderfun(as.dendrogram(hclustfun(distfun(x))), Rowv)
    Rowv <- rev(Rowv) # I would rather the matrix will be with the first row at the top
  }
  if (is.dendrogram(Rowv)) {
    # Rowv <- rev(Rowv)
    rowInd <- order.dendrogram(Rowv)
    if(nr != length(rowInd))
      stop("Row dendrogram is the wrong size")
  } else {
    if (!is.null(Rowv) && !is.na(Rowv) && !identical(Rowv, FALSE))
      warning("Invalid value for Rowv, ignoring")
    Rowv <- NULL
    rowInd <- 1:nr
  }

  # making the order of the matrix rows comparable with heatmap.2
  Rowv <- rev(Rowv)
  rowInd <- rev(rowInd)



  if (identical(Colv, "Rowv")) {
    # i.e.: if symm=TRUE
    Colv <- Rowv
  }
  if (isTRUE(Colv)) {
    Colv <-  switch(seriate,
                    "mean" = colMeans(x, na.rm = na.rm),
                    "none" = 1:ncol(x),
                    "OLO" = {
                      dist_x <- distfun(t(x)) # dist is on the rows by default
                      hc_x <- hclustfun(dist_x)
                      o <- seriate(dist_x, method = "OLO", control = list(hclust = hc_x) )
                      dend_x <- as.dendrogram(hc_x)
                      dend_x2 <- dendextend::rotate(dend_x, order = rev(labels(dist_x)[get_order(o)]))
                      dend_x2
                    },
                    "GW" = {
                      dist_x <- distfun(t(x)) # dist is on the rows by default
                      hc_x <- hclustfun(dist_x)
                      o <- seriate(dist_x, method = "GW", control = list(hclust = hc_x) )
                      dend_x <- as.dendrogram(hc_x)
                      dend_x2 <- dendextend::rotate(dend_x, order = rev(labels(dist_x)[get_order(o)]))
                      dend_x2
                    }

    )
  }
  if (is.numeric(Colv)) {
    Colv <- reorderfun(as.dendrogram(hclustfun(distfun(t(x)))), rev(Colv))
  }
  if (is.dendrogram(Colv)) {
    Colv <- rev(Colv)
    colInd <- order.dendrogram(Colv)
    if (nc != length(colInd))
      stop("Col dendrogram is the wrong size")
  } else {
    if (!is.null(Colv) && !is.na(Colv) && !identical(Colv, FALSE))
      warning("Invalid value for Colv, ignoring")
    Colv <- NULL
    colInd <- 1:nc
  }


  # TODO:  We may wish to change the defaults a bit in the future
  ## revC
  ##=======================
  if(missing(revC)) {
    if (symm) {
      revC <- TRUE
    } else if(is.dendrogram(Colv) & is.dendrogram(Rowv) & identical(Rowv, rev(Colv))) {
      revC <- TRUE
    } else {
      revC <- FALSE
    }
  }
  if(revC) {
    Colv <- rev(Colv)
    colInd <- rev(colInd)
  }

  ## reorder x (and others)
  ##=======================
  x <- x[rowInd, colInd, drop = FALSE]
  if (!missing(cellnote))
    cellnote <- cellnote[rowInd, colInd, drop = FALSE]

  if (!is.null(row_side_colors)) {
    if(is.vector(row_side_colors)) {
      row_side_colors <- data.frame("row_side_colors" = row_side_colors)
    }
    if (dim(row_side_colors)[1] != dim(x)[1])
      stop("row_side_colors and x have different numbers of rows")
    row_side_colors <- row_side_colors[rowInd, , drop = FALSE]
  }
  if (!is.null(col_side_colors)) {
    if(is.vector(col_side_colors)) {
      col_side_colors <- matrix(col_side_colors, nrow = 1)
      rownames(col_side_colors) <- "col_side_colors"
    }
    if (dim(col_side_colors)[2] != dim(x)[2])
      stop("col_side_colors and x have different numbers of columns")
    col_side_colors <- col_side_colors[, colInd, drop = FALSE]
  }

  ## Dendrograms - Update the labels and change to dendToTree
  ##=======================

  # color branches?
  #----------------
  # Due to the internal working of dendextend, in order to use it we first need
  # to populate the dendextend::dendextend_options() space:
  if(!missing(k_row) | !missing(k_col)) dendextend::assign_dendextend_options()

  if(is.dendrogram(Rowv) & !missing(k_row)) {
    Rowv <- dendextend::color_branches(Rowv, k = k_row)
  }
  if(is.dendrogram(Colv) & !missing(k_col)) {
    Colv <- dendextend::color_branches(Colv, k = k_col)
  }

  rowDend <- if(is.dendrogram(Rowv)) Rowv else NULL
  colDend <- if(is.dendrogram(Colv)) Colv else NULL


  ## Scale the data?
  ##====================
  scale <- match.arg(scale)

  if(!cellnote_scale) x_unscaled <- x #keeps a backup for cellnote

  if(scale == "row") {
    x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
    x <- sweep(x, 1, apply(x, 1, sd, na.rm = na.rm), "/")
  }
  else if(scale == "column") {
    x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
    x <- sweep(x, 2, apply(x, 2, sd, na.rm = na.rm), "/")
  }


  ## cellnote
  ##====================
  if(missing(cellnote)) {
    if(cellnote_scale) {
      cellnote <- round(x, digits = digits)
    } else { # default
      cellnote <- round(x_unscaled, digits = digits)
    }
  }

  # Check that cellnote is o.k.:
  if (is.null(dim(cellnote))) {
    if (length(cellnote) != nr*nc) {
      stop("Incorrect number of cellnote values")
    }
    dim(cellnote) <- dim(x)
  }
  if (!identical(dim(x), dim(cellnote))) {
    stop("cellnote matrix must have same dimensions as x")
  }


  ## Final touches before exporting the object
  ##=======================

  mtx <- list(data = as.matrix(cellnote),
              dim = dim(x),
              rows = rownames(x),
              cols = colnames(x)
  )


  if (is.factor(x)) {
    colors <- col_factor(colors, x, na.color = "transparent")
  } else {
    rng <- range(x, na.rm = TRUE)
    if (scale %in% c("row", "column")) {
      rng <- c(max(abs(rng)), -max(abs(rng)))
    }

    colors <- col_numeric(colors, rng, na.color = "transparent")
  }

  # imgUri <- base64enc::encodeAsPNG(t(x), colors)

  options <- NULL

  options <- c(options, list(
    xaxis_height = xaxis_height,
    yaxis_width = yaxis_width,
    xaxis_font_size = xaxis_font_size,
    yaxis_font_size = yaxis_font_size,
    brush_color = brush_color,
    show_grid = show_grid,
    anim_duration = anim_duration
  ))

  if (is.null(rowDend)) {
    c(options, list(yclust_width = 0))
  }
  if (is.null(colDend)) {
    c(options, list(xclust_height = 0))
  }

  heatmapr <- list(rows = rowDend, cols = colDend, matrix = mtx, # image = imgUri,
                  theme = theme, options = options,
                  row_side_colors = row_side_colors,
                  col_side_colors = col_side_colors)

  class(heatmapr) <- "heatmapr"

  heatmapr
}




#' @title Is the object of class heatmapr
#' @description Is the object of class heatmapr.
#' @export
#' @param x an object.
#' @return logical - is the object of class heatmapr.
is.heatmapr <- function(x) {
  inherits(x,"heatmapr")
}

