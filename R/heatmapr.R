# prepare the heatmapr object.

`%||%` <- function(a, b) {
  if (!is.null(a)) {
    a
  } else {
    b
  }
}



# Check if all values in vector x are unique
all_unique <- function(x, ...) {
  length(x) == length(unique(x))
}

fix_not_all_unique <- function(x, ...) {
  if (all_unique(x)) {
    return(x)
  } else {
    warning("Not all the values are unique - manually added prefix numbers")
    return(paste0(seq_along(x), "_", x))
  }
}





#' Creates a heatmapr object
#'
#' An object of class heatmapr includes all the needed information
#' for producing a heatmap. The goal is to separate the pre-processing of the
#' heatmap elements from the graphical rendering of the object, which could be done
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
#' @param Rowv determines if and how the row dendrogram should be reordered.	By default, it is TRUE, which implies dendrogram is computed and reordered based on row means. If NULL or FALSE, then no dendrogram is computed and no reordering is done. If a \link{dendrogram} (or \link{hclust}), then it is used "as-is", ie without any reordering. If a vector of integers, then dendrogram is computed and reordered based on the order of the vector.
#' @param Colv determines if and how the column dendrogram should be reordered.	Has the options as the Rowv argument above and additionally when x is a square matrix, Colv = "Rowv" means that columns should be treated identically to the rows.
#' @param distfun function used to compute the distance (dissimilarity) between both rows and columns. Defaults to dist.
#' @param hclustfun function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms. Defaults to hclust.
#'
#' @param dist_method default is NULL (which results in "euclidean" to be used). Can accept alternative character strings indicating the
#' method to be passed to distfun. By default distfun. is \link{dist} hence
#' this can be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#' @param hclust_method default is NULL (which results in "complete" to be used). Can accept alternative character strings indicating the
#' method to be passed to hclustfun By default hclustfun is \link{hclust} hence
#' this can be one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
#'
#' @param distfun_row distfun for row dendrogram only.
#' @param hclustfun_row hclustfun for col dendrogram only.
#' @param distfun_col distfun for row dendrogram only.
#' @param hclustfun_col hclustfun for col dendrogram only.
#'
#' @param dendrogram character string indicating whether to compute 'none',
#' 'row', 'column' or 'both' dendrograms. Defaults to 'both'. However, if Rowv
#' (or Colv) is FALSE or NULL and dendrogram is 'both', then a warning is issued
#' and Rowv (or Colv) arguments are honoured.
#' @param show_dendrogram Logical vector of length controlling whether the row
#' and column dendrograms are displayed. If a logical scalar is
#' provided, it is repeated to become a logical vector of length two.
#' @param reorderfun function(d, w) of dendrogram and weights for reordering the row and column dendrograms. The default uses stats{reorder.dendrogram}
#'
#' @param k_row an integer scalar with the desired number of groups by which to color the dendrogram's branches in the rows (uses \link[dendextend]{color_branches})
#' If NA then \link[dendextend]{find_k} is used to deduce the optimal number of clusters.
#' @param k_col an integer scalar with the desired number of groups by which to color the dendrogram's branches in the columns (uses \link[dendextend]{color_branches})
#' If NA then \link[dendextend]{find_k} is used to deduce the optimal number of clusters.
#'
#' @param symm logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.
#' @param revC logical indicating if the column order should be reversed for plotting.
#' Default (when NULL) - is FALSE, unless symm is TRUE.
#' This is useful for cor matrix.
#'
#' @param scale character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "none".
#' @param na.rm logical indicating whether NA's should be removed.
#'
#' @param digits integer indicating the number of decimal places to be used by \link{round} for 'label'.
#' @param cellnote (optional) matrix of the same dimensions as \code{x}
#' that has the human-readable version of each value,
#' for displaying on top of the heatmap cells.
#'
#' @param cexRow positive numbers. If not NULL, it will override \code{xaxis_font_size}
#' and will give it a value cexRow*14
#' @param cexCol positive numbers. If not NULL, it will override \code{yaxis_font_size}
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
#' "OLO" (Optimal leaf ordering, optimizes the Hamiltonian path length that is restricted by the dendrogram structure - works in O(n^4) )
#' "mean" (sorts the matrix based on the reorderfun using marginal means of the matrix. This is the default used by \link[gplots]{heatmap.2}),
#' "none" (the default order produced by the dendrogram),
#' "GW" (Gruvaeus and Wainer heuristic to optimize the Hamiltonian path length that is restricted by the dendrogram structure)
#' @param point_size_mat A matrix of values which can be mapped to point size
#' @param custom_hovertext Custom hovertext matrix (the same dimensions as the input).
#' @param ... currently ignored
#'
#' @export
#' @source
#' The interface was designed based on \link{heatmap}, \link[gplots]{heatmap.2}, and (the also d3heatmap).
#'
#' @seealso
#' \link{heatmap}, \link[gplots]{heatmap.2}
#'
#' @examples
#' \dontrun{
#' library(heatmaply)
#' hm <- heatmapr(mtcars, scale = "column", colors = "Blues")
#' heatmaply(hm)
#' }
#'
heatmapr <- function(x,
                     ## dendrogram control
                     Rowv = NULL,
                     Colv = NULL,
                     distfun = dist,
                     hclustfun = hclust,
                     dist_method = NULL,
                     hclust_method = NULL,
                     distfun_row = distfun,
                     hclustfun_row = hclustfun,
                     distfun_col = distfun,
                     hclustfun_col = hclustfun,
                     dendrogram = c("both", "row", "column", "none"),
                     show_dendrogram = c(TRUE, TRUE),
                     reorderfun = function(d, w) reorder(d, w),
                     k_row = 1,
                     k_col = 1,
                     symm = FALSE,
                     revC = symm || (is.dendrogram(Colv) & is.dendrogram(Rowv) & identical(Rowv, rev(Colv))),
                     ## data scaling
                     scale = c("none", "row", "column"),
                     na.rm = TRUE,
                     labRow = rownames(x),
                     labCol = colnames(x),
                     cexRow = NULL,
                     cexCol = NULL,
                     ## value formatting
                     digits = 3L,
                     cellnote = NULL,
                     ## TODO: decide later which names/conventions to keep
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
                     point_size_mat = NULL,
                     custom_hovertext = NULL,
                     ...) {

  ## update hclust/dist functions?
  ## ====================
  distfun <- match.fun(distfun)
  if (!is.null(dist_method)) {
    distfun_old <- distfun
    distfun <- function(x) {
      distfun_old(x, method = dist_method)
    }
  }
  if (!is.null(hclust_method)) {
    if (is.na(hclust_method)) {
      hclustfun <- find_dend
    } else {
      hclustfun_old <- hclustfun
      hclustfun <- function(x) {
        hclustfun_old(x, method = hclust_method)
      }
    }
  }

  if (!(is.logical(show_dendrogram) & length(show_dendrogram) %in% c(1, 2))) {
    stop("show_dendrogram must be a logical vector of length 1 or 2")
  }
  if (length(show_dendrogram) == 1) {
    show_dendrogram <- rep(show_dendrogram, 2)
  }

  distfun_row <- match.fun(distfun_row)
  distfun_col <- match.fun(distfun_col)

  ## x is a matrix!
  ## ====================
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.matrix(x)) stop("x must be a matrix")


  seriate <- match.arg(seriate)

  nr <- nrow(x)
  nc <- ncol(x)
  ### TODO: debating if to include this or not:
  #   if(nr <= 1 || nc <= 1)
  #     stop("`x' must have at least 2 rows and 2 columns")


  ## Labels for Row/Column
  ## ======================
  rownames(x) <- labRow %||% as.character(1:nrow(x))
  colnames(x) <- labCol %||% as.character(1:ncol(x))


  # fix a case where the row/col names are non unique (this assume the users does NOT supply a dendrogram)
  rownames(x) <- fix_not_all_unique(rownames(x))
  colnames(x) <- fix_not_all_unique(colnames(x))

  if (!is.null(cexRow)) {
    if (is.numeric(cexRow)) {
      xaxis_font_size <- cexRow * 14
    } else {
      warning("cexRow is not numeric. It is ignored")
    }
  }
  if (!is.null(cexCol)) {
    if (is.numeric(cexCol)) {
      yaxis_font_size <- cexCol * 14
    } else {
      warning("cexCol is not numeric. It is ignored")
    }
  }

  ## Scale the data?
  ## ====================
  scale <- match.arg(scale)

  if (scale == "row") {
    x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
    x <- sweep(x, 1, apply(x, 1, sd, na.rm = na.rm), "/")
  } else if (scale == "column") {
    x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
    x <- sweep(x, 2, apply(x, 2, sd, na.rm = na.rm), "/")
  }

  ## Dendrograms for Row/Column
  ## =======================
  dendrogram <- match.arg(dendrogram)

  # Use dendrogram argument to set defaults for Rowv/Colv
  if (is.null(Rowv)) {
    Rowv <- dendrogram %in% c("both", "row")
  }
  if (is.null(Colv)) {
    if (dendrogram %in% c("both", "column")) {
      Colv <- if (symm) "Rowv" else TRUE
    } else {
      Colv <- FALSE
    }
  }

  if (isTRUE(Rowv)) {
    Rowv <- create_dend(x, seriate, distfun_row, hclustfun_row, na.rm)
  }
  if (is.numeric(Rowv)) {
    Rowv <- reorderfun(as.dendrogram(hclustfun_row(distfun_row(x))), Rowv)
    Rowv <- rev(Rowv) # I would rather the matrix will be with the first row at the top
  }

  if (is.hclust(Rowv)) Rowv <- as.dendrogram(Rowv)

  if (is.dendrogram(Rowv)) {
    # Rowv <- rev(Rowv)
    rowInd <- order.dendrogram(Rowv)
    if (nr != length(rowInd)) {
      stop("Row dendrogram is the wrong size")
    }
  } else {
    if (!is.null(Rowv) && !is.na(Rowv) && !identical(Rowv, FALSE)) {
      warning("Invalid value for Rowv, ignoring")
    }
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
    Colv <- create_dend(t(x), seriate, distfun_col, hclustfun_col, na.rm)
  }
  if (is.numeric(Colv)) {
    Colv <- reorderfun(as.dendrogram(hclustfun_col(distfun_col(t(x)))), rev(Colv))
  }

  if (is.hclust(Colv)) Colv <- as.dendrogram(Colv)

  if (is.dendrogram(Colv)) {
    Colv <- rev(Colv)
    colInd <- order.dendrogram(Colv)
    if (nc != length(colInd)) {
      stop("Col dendrogram is the wrong size")
    }
  } else {
    if (!is.null(Colv) && !is.na(Colv) && !identical(Colv, FALSE)) {
      warning("Invalid value for Colv, ignoring")
    }
    Colv <- NULL
    colInd <- 1:nc
  }


  # TODO:  We may wish to change the defaults a bit in the future
  ## revC
  ## =======================
  if (revC) {
    Colv <- rev(Colv)
    colInd <- rev(colInd)
  }

  ## reorder x (and others)
  ## =======================
  if (is.null(cellnote)) cellnote <- x
  if (is.numeric(cellnote)) {
    cellnote <- round(cellnote, digits = digits)
  }
  x <- x[rowInd, colInd, drop = FALSE]
  cellnote <- cellnote[rowInd, colInd, drop = FALSE]
  if (!is.null(point_size_mat)) {
    point_size_mat <- point_size_mat[rowInd, colInd, drop = FALSE]
  }
  if (!is.null(custom_hovertext)) {
    custom_hovertext <- custom_hovertext[rowInd, colInd, drop = FALSE]
  }
  if (!is.null(row_side_colors)) {
    if (!(is.data.frame(row_side_colors) | is.matrix(row_side_colors))) {
      row_side_colors <- data.frame("row_side_colors" = row_side_colors)
    }
    assert_that(nrow(row_side_colors) == nrow(x))
    row_side_colors <- row_side_colors[rowInd, , drop = FALSE]
  }
  if (!is.null(col_side_colors)) {
    if (!(is.data.frame(col_side_colors) | is.matrix(col_side_colors))) {
      col_side_colors <- data.frame(col_side_colors)
      colnames(col_side_colors) <- "col_side_colors"
    }
    assert_that(nrow(col_side_colors) == ncol(x))
    col_side_colors <- col_side_colors[colInd, , drop = FALSE]
  }

  ## Dendrograms - Update the labels and change to dendToTree
  ## =======================

  # color branches?
  # ----------------
  # Due to the internal working of dendextend, in order to use it we first need
  # to populate the dendextend::dendextend_options() space:
  # if(!missing(k_row) | !missing(k_col))  # Setting k_row and k_col to 1 by default
  dendextend::assign_dendextend_options()

  if (is.dendrogram(Rowv)) {
    if (is.na(k_row)) k_row <- find_k(Rowv)$k

    if (k_row > 1) {
      Rowv <- color_branches(
        Rowv,
        k = k_row,
        col = k_colors(k_row)
      )
    }
  }
  if (is.dendrogram(Colv)) {
    if (is.na(k_col)) k_col <- find_k(Colv)$k

    if (k_col > 1) {
      Colv <- color_branches(
        Colv,
        k = k_col,
        col = k_colors(k_col)
      )
    }
  }

  rowDend <- if (is.dendrogram(Rowv) & show_dendrogram[[1]]) Rowv else NULL
  colDend <- if (is.dendrogram(Colv) & show_dendrogram[[2]]) Colv else NULL


  ## cellnote
  ## ====================
  # Check that cellnote is o.k.:
  if (!is.null(cellnote)) {
    if (is.null(dim(cellnote))) {
      if (length(cellnote) != nr * nc) {
        stop("Incorrect number of cellnote values")
      }
      dim(cellnote) <- dim(x)
    }
    if (!identical(dim(x), dim(cellnote))) {
      stop("cellnote matrix must have same dimensions as x")
    }
  }

  ## Final touches before exporting the object
  ## =======================

  # if(!is.null(custom_hovertext) && !is.matrix(custom_hovertext)) {
  if (is.data.frame(custom_hovertext)) {
    custom_hovertext <- as.matrix(custom_hovertext)
  }

  mtx <- list(
    data = as.matrix(x),
    cellnote = cellnote,
    dim = dim(x),
    rows = rownames(x),
    cols = colnames(x),
    point_size_mat = point_size_mat,
    custom_hovertext = custom_hovertext
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

  heatmapr <- list(
    rows = rowDend, cols = colDend, matrix = mtx,
    theme = theme, options = options
  )

  if (!is.null(row_side_colors)) heatmapr[["row_side_colors"]] <- row_side_colors
  if (!is.null(col_side_colors)) heatmapr[["col_side_colors"]] <- col_side_colors

  class(heatmapr) <- "heatmapr"

  heatmapr
}




#' @title Is the object of class heatmapr
#' @description Is the object of class heatmapr.
#' @export
#' @param x an object.
#' @return logical - is the object of class heatmapr.
is.heatmapr <- function(x) {
  inherits(x, "heatmapr")
}


create_dend <- function(x, seriate, distfun, hclustfun, na.rm) {
  switch(seriate,
    "mean" = rowMeans(x, na.rm = na.rm),
    "none" = 1:nrow(x),
    "OLO" = {
      dist <- distfun(x)
      hc <- hclustfun(dist)
      dend <- as.dendrogram(hc)
      dend <- seriate_dendrogram(dend, dist, method = "OLO")
      dend
    },
    "GW" = {
      dist <- distfun(x)
      hc <- hclustfun(dist)
      dend <- as.dendrogram(hc)
      dend <- seriate_dendrogram(dend, dist, method = "GW")
      dend
    }
  )
}
