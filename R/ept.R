
# kCDF_fun, ecdf
ept_predict <- function(x, ecdf_fun = ecdf, ...) {
  # http://stackoverflow.com/questions/25130531/how-to-select-only-numeric-columns-from-a-data-table
  # x must be a data.frame
  ss_numeric <- sapply(x, is.numeric)


  # ecdf_fun <- kCDF_fun # ecdf

  ecdf_list <- list()
  for(i in 1:ncol(x)) {
    ecdf_list[[i]] <- if(ss_numeric[i]) {
      ecdf_fun(na.omit(x[,i]))
    } else {
      NA
    }
  }

  fun <- function(new_x) {
    if( any(colnames(new_x) != colnames(x)) ) stop("The column names (or order) of the new x are different than that of the old x. Please fix and try again.")

    for(i in 1:ncol(x)) {
      ecdf_fun <- ecdf_list[[i]]
      if(! (is.vector(ecdf_fun) && is.na(ecdf_fun))  ) {
        ss_no_NA <- !is.na(new_x[,i])
        new_x[ss_no_NA,i] <- ecdf_fun(new_x[ss_no_NA,i])
      }
    }

    return(new_x)
  }

  class(fun) <- c("function", "ept_predict")

  return(fun)
}






#' @title  Empirical Percentile Transformation
#' @export
#'
#' @description
#' An Empirical Percentile Transformation (ept) is similar to taking the rank
#' of a variable. The difference is that it is simpler to compare and interpret
#' the transformed variables.
#'
#' This is helpful for comparing several variables in a heatmap (e.g.: \link{heatmaply}).
#'
#' @param x a vector or a data.frame.
#'
#' @param ... Currently ignored.
#'
#' @return
#' A vector (or data.frame) after \link{ecdf} was used on that vector.
#' If x is a \link{data.frame} then only the numeric variables are transformed.
#'
#' @aliases
#' ept.default
#' ept.data.frame
#' ept.matrix
#' @examples
#' \dontrun{
#' x <- mtcars
#' x <- data.frame(x)
#' x$am <- factor(x$am)
#' x$vs <- factor(x$vs)
#' heatmaply(ept(x))
#'
#'
#' x <- data.frame(a = 1:10, b = 11:20)
#' x[4:6, 1:2] <- NA
#' ept(x)
#' ept(x[,1])
#'
#' }
ept <- function(x, ...) {
  UseMethod("ept")
}


#' @export
ept.default <- function(x, ...) {
  ss_no_NA <- !is.na(x)
  x[ss_no_NA] <- ecdf(x[ss_no_NA])(x[ss_no_NA])
  x
}


#' @export
ept.data.frame <- function(x, ...) {
  # x <- na.omit(x)
  ept_predict(x)(x)
}


#' @export
ept.matrix <- function(x, ...) {
  x <- as.data.frame(x)
  ept(x)
}

