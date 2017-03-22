#
# # source: http://www.peterhaschke.com/r/2013/04/24/MultiPlot.html
# # http://rstudio-pubs-static.s3.amazonaws.com/2852_379274d7c5734f979e106dcf019ec46c.html
# multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
#   # library(grid)
#
#   plots <- c(list(...), plotlist)
#
#   numPlots = length(plots)
#
#   if (is.null(layout)) {
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#
#   if (numPlots == 1) {
#     print(plots[[1]])
#
#   } else {
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#
#     for (i in 1:numPlots) {
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }

# grid,
# gridExtra

#' @title  Creates a ggplot2 heatmap
#'
#' @description
#' An object of class heatmapr includes all the needed information
#' for producing a heatmap. The goal is to seperate the pre-processing of the
#' heatmap elements from the graphical rendaring of the object, which could be done
#'
#' @param x can either be a heatmapr object, or a numeric matrix
#'   Defaults to \code{TRUE} unless \code{x} contains any \code{NA}s.
#'
#' @param ... other parameters passed to \link{heatmapr} (currently, various parameters may be ignored.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' library(heatmaply)
#' x <- heatmapr(iris[,-5], scale = "column", colors = "Blues")
#' ggheatmap(x)
#'
#'
#' }
# ggheatmap <- function(x, ...) {
#   if(!is.heatmapr(x)) {
#     x <- heatmapr(x, ...)
#   }
#   ppxpy <- heatmaply(x, return_ppxpy = TRUE)
#   ggempty <- ggplot() + geom_blank() + theme_bw()
#   gg_list <- list(ppxpy$py, ggempty, ppxpy$p, ppxpy$px)
#   # gg_list <- list(ppxpy$py, ppxpy$p, ggempty, ppxpy$px)
#   # sapply(ppxpy, class)
#   # lapply(gg_list, class)
#   GGally::ggmatrix(gg_list, nrow = 2, ncol = 2) # in this we can't control the relative widths
#   # multiplot(ppxpy$py, ggempty, ppxpy$p, ppxpy$px,

#   # pp <- ppxpy$p + guides(fill=FALSE) +
#   #   theme(axis.title.x=element_blank(),
#   #         axis.text.x=element_blank(),
#   #         axis.ticks.x=element_blank(),
#   #         axis.title.y=element_blank(),
#   #         axis.text.y=element_blank(),
#   #         axis.ticks.y=element_blank())
#   # multiplot(ppxpy$py, pp , ggempty, ppxpy$px,
#   #           layout = matrix(c(1,2,2,1,2,2,3,4,4), nrow = 3) ,
#   #                     cols = 2, main = "Main title")

#   # ppxpy$p + guides(fill=FALSE)

# }





# xx is a data matrix
ggplot_heatmap <- function(xx,
                           row_text_angle = 0,
                           column_text_angle = 45,
                           scale_fill_gradient_fun =
                             scale_fill_gradientn(colors = viridis(n=256, alpha = 1, begin = 0,
                                                                   end = 1, option = "viridis"),
                                                  na.value = "grey50", limits = NULL),
                           grid_color = NA,
                           grid_size = 0.1,
                           key.title = NULL,
                           layers,
                           row_dend_left = FALSE,
                           ...) {
  theme_clear_grid_heatmap <- theme(axis.line = element_line(color = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
                                    panel.background = element_blank())
  # heatmap
  if(!is.data.frame(df)) df <- as.data.frame(xx)

  if(!is.null(rownames(xx))) df$row <- rownames(xx) else df$row <- 1:nrow(xx)

  df$row <- with(df, factor(row, levels=row, ordered=TRUE))
  mdf <- reshape2::melt(df, id.vars="row")
  colnames(mdf)[2] <- "column" # rename "variable"
  # TODO:
  # http://stackoverflow.com/questions/15921799/draw-lines-around-specific-areas-in-geom-tile
  # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  p <- ggplot(mdf, aes_string(x = "column", y = "row")) +
    geom_tile(aes_string(fill = "value"), color = grid_color, size = grid_size) +
    # scale_linetype_identity() +
    # scale_fill_viridis() +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient_fun +
    theme_bw()+ theme_clear_grid_heatmap +
    theme(axis.text.x = element_text(angle = column_text_angle, hjust = 1),
          axis.text.y = element_text(angle = row_text_angle, hjust = 1)
          )

  if(!missing(layers)) p <- p + layers
    ## Passed in to allow users to alter (courtesy of GenVisR)

  # p <- p + scale_x_discrete(limits = unique(mdf))
  # http://stats.stackexchange.com/questions/5007/how-can-i-change-the-title-of-a-legend-in-ggplot2
  p <- p + labs(fill=key.title)

  # until this bug is fixed: https://github.com/ropensci/plotly/issues/699
  # we are forced to use geom_hline and geom_vline
  if(!is.na(grid_color)) {
    p <- p + geom_hline(yintercept =c(0:nrow(xx))+.5, color = grid_color) # , size = grid_size # not implemented since it doesn't work with plotly
    p <- p + geom_vline(xintercept =c(0:ncol(xx))+.5, color = grid_color) # , size = grid_size # not implemented since it doesn't work with plotly

  }

  if(row_dend_left) p <- p + scale_y_discrete(position = "right") # possible as of ggplot 2.1.0 !

  p
}


plotly_heatmap <- function(x, limits = range(x), colors,
    row_text_angle=0, column_text_angle=45, grid.color, grid.size, key.title, 
    row_dend_left) {

  plot_ly(z = x, x = 1:ncol(x), y = 1:nrow(x), 
    type = "heatmap", showlegend = FALSE, colors=colors, 
    zmin = limits[1], zmax = limits[2]) %>%
      layout(
        xaxis = list(
          tickvals = 1:ncol(x), ticktext = colnames(x),
          showticklabels = TRUE
        ),
        yaxis = list(
          tickvals = 1:nrow(x), ticktext = rownames(x),
          showticklabels = TRUE
        )
      ) %>% colorbar(lenmode = "fraction", y = 0, yanchor="bottom", len=0.3)
}    



make_colorscale <- function(colors) {
    seq <- seq(0, 1, by = 1/length(colors))
    scale <- lapply(seq_along(colors), 
        function(i) {
            # eg
            # list(c(0, "rgb(255, 0, 0)"), c(1, "rgb(0, 255, 0)")),
            if (i == 1) {
                list(0, col2plotlyrgb(colors[i]))
            } else if (i == length(colors)) {
                list(1, col2plotlyrgb(colors[i]))
            } else {
                list(seq[i], col2plotlyrgb(colors[i]))
            }
        }
    )
    scale
}

col2plotlyrgb <- function(col) {
    rgb <- col2rgb(col)
    paste0(
      "rgb(", 
      rgb["red", ], ",", 
      rgb["green", ], ",", 
      rgb["blue", ], ")"
    )
}

#' @importFrom ggdendro dendro_data
plotly_dend_row <- function(dend, flip = FALSE) {
  dend_data <- dendro_data(dend)
  segs <- dend_data$segment
  p <- plot_ly(segs) %>% 
    add_segments(x = ~y, xend = ~yend, y = ~x, yend = ~xend,
      line=list(color = '#000000'), showlegend = FALSE, hoverinfo = "none") %>%
    layout(
      hovermode = "closest",
      xaxis = list(
        title = "", 
        linecolor = "#ffffff", 
        showgrid = FALSE
      ),
      yaxis = list(
        title = "",
        range = c(0, max(segs$x) + 1), 
        linecolor = "#ffffff",
        showgrid = FALSE
      )
    )

  if (flip) {
    p <- layout(p, xaxis = list(autorange = "reversed"))
  }
  p
}

plotly_dend_col <- function(dend, flip = FALSE) {
  dend_data <- dendro_data(dend)
  segs <- dend_data$segment

  plot_ly(segs) %>% 
    add_segments(x = ~x, xend = ~xend, y = ~y, yend = ~yend,
      line = list(color='#000000'), showlegend = FALSE, hoverinfo = "none") %>%
    layout(
      hovermode = "closest",
      xaxis = list(
        title = "", 
        range = c(0, max(segs$x) + 1), 
        linecolor = "#ffffff",
        showgrid = FALSE
      ),
      yaxis = list(
        title = "", 
        linecolor = "#ffffff", 
        showgrid = FALSE
      )
    )
}



#'
#' geom_tile for side color plots
#'
#' @param df A "molten" data.frame as produced by (eg) reshape2::melt
#' @param palette A function which can return colors to be used in the sidebar
#' plot
#' @param scale_title Title of the color scale. Not currently used.
#' @param type Horizontal or vertical plot? Valid values are "column" and "row"
#' @param row_text_angle,column_text_angle the angle of the text of the rows/columns.
#' @param is_colors Use if the values in df are valid colours and should not be mapped
#'  to a color scheme, and instead should be plotted directly.
#'
#' @return A ggplot geom_tile object
#'
#' @export
side_color_plot <- function(df, palette,
  scale_title = paste(type, "side colors"), type = c("column", "row"),
  row_text_angle, column_text_angle, is_colors) {

  if (is.matrix(df)) df <- as.data.frame(df)
  stopifnot(is.data.frame(df))

  ## TODO: Find out why names are dropped when dim(df)[2] == 1
  original_dim <- dim(df)

  if (missing(column_text_angle)) column_text_angle <- 0
  if (missing(row_text_angle)) row_text_angle <- 45
  if (missing(palette)) palette <- colorspace::rainbow_hcl

  type <- match.arg(type)
  if (type %in% colnames(df))
    stop("Having", type, "in the colnames of the side_color df will drop data!")

  df[[type]] <- if(!is.null(rownames(df))) rownames(df) else 1:nrow(df)

  df[[type]] <- factor(df[[type]], levels = df[[type]], ordered = TRUE)
  df <- reshape2::melt(df, id.vars = type)
  df[["value"]] <- factor(df[["value"]])

  id_var <- colnames(df)[1]
  if (type == "column") {
    mapping <- aes_string(x = id_var, y = 'variable', fill = 'value')
    if(original_dim[2] > 1) {
      text_element <- element_text(angle = column_text_angle)
    } else text_element <- element_blank()

    theme <- theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = text_element,
        axis.ticks = element_blank())
  } else {
    if(original_dim[2] > 1) {
      text_element <- element_text(angle = row_text_angle)
    } else text_element <- element_blank()

    mapping <- aes_string(x = 'variable', y = id_var, fill = 'value')
    theme <- theme(
        panel.background = element_blank(),
        axis.text.x = text_element,
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
  }

  color_vals <- if (is_colors) levels(df[["value"]])
  else palette(length(unique(df[["value"]])))

  g <- ggplot(df, mapping = mapping) +
    geom_raster() +
    xlab("") +
    ylab("") +
    scale_fill_manual(
      name = NULL,
      breaks = levels(df[["value"]]),
      values = color_vals) +
    theme
  return(g)
}

