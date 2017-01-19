



#' @title Indicates which elements are missing (either 1 and 0)
#' @export
#'
#' @param x a vector, matrix or data.frame.
#' @param ... not used.
#'
#' @seealso \link{is.na}
#' @return
#' Returns a numeric (instead of a logical) variable/matrix of 1 (missing) or 0 (not missing) values (hence the name is.na10)
#' while still presering the attributes resulted from running \link{is.na}.
#'
#' These are useful for funnelling into a heatmap (see the examples).
#'
#' @examples
#' \dontrun{
#' x <- mtcars
#' x <- data.frame(x)
#' x$am <- factor(x$am)
#' x$vs <- factor(x$vs)
#' set.seed(2017-01-19)
#' x[sample(nrow(x))[1:6],sample(ncol(x))[1:6]] <- NA
#'
#'
#' # nice grey colors from here: https://github.com/njtierney/visdat/blob/master/R/vis_miss_ly.R
#' x %>% is.na10 %>% heatmaply( colors = c("grey80", "grey20"), dendrogram = "none")
#' x %>% is.na10 %>% heatmaply( colors = c("grey80", "grey20"), k_col = 2, k_row = 2)
#'
#' }
is.na10 <- function(x, ...) {
  # x %>% is.na %>% str
  # x %>% is.na %>% as.numeric

  # get NA
  x_na <- is.na(x)

  # turn numeric (whlie keeping the attributes!)
  mat_attr <- attributes(x_na)
  x_na <- as.numeric(x_na)
  attributes(x_na) <- mat_attr

  # return
  x_na
}



