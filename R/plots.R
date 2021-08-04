

# heatmaply:::ggplot_heatmap(as.matrix(mtcars))
# heatmaply:::plotly_heatmap(as.matrix(mtcars))
# style(plotly_heatmap(as.matrix(mtcars)), xgap = 5, ygap = 5)




#
# library(ggplot2)
# library(plotly)
# # library(heatmaply)
# ggplot_heatmap <- heatmaply:::ggplot_heatmap
# class_to <- function(x, new_class) {
#   class(x) <- new_class
#   x
# }
# na_mat <- function(x) {
#   x %>% is.na %>% class_to("numeric")
# }
#
# p <- heatmaply:::ggplot_heatmap(na_mat(airquality),
#                     scale_fill_gradient_fun = scale_fill_gradientn(colors= c("white","black")) ,
#                     grid_color = "grey", grid_size = 1)
# plot(p)
# ggplotly(p)
# p <- ggplot_heatmap(mtcars,
#                     grid_color = "white")
# p
#


# heatmaply:::ggplot_heatmap(as.matrix(mtcars))


# xx is a data matrix
ggplot_heatmap <- function(xx,
                           row_text_angle = 0,
                           column_text_angle = 45,
                           scale_fill_gradient_fun =
                           scale_fill_gradientn(
                             colors = viridis(
                               n = 256, alpha = 1, begin = 0,
                               end = 1, option = "viridis"
                             ),
                             na.value = "grey50", limits = NULL
                           ),
                           grid_color = NA,
                           grid_size = 0.1,
                           key.title = NULL,
                           layers,
                           row_dend_left = FALSE,
                           label_names = NULL,
                           fontsize_row = 10,
                           fontsize_col = 10,
                           type = c("heatmap", "scatter"),
                           pointsize = 5,
                           point_size_mat = NULL,
                           point_size_name = "Point size",
                           label_format_fun = function(...) format(..., digits = 4),
                           custom_hovertext = NULL,
                           showticklabels = c(TRUE, TRUE),
                           ...) {
  theme_clear_grid_heatmap <- theme(
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )

  type <- match.arg(type)
  # heatmap
  # xx <- x$matrix$data

  df <- xx
  if (!is.data.frame(df)) df <- as.data.frame(df, check.rows = FALSE)

  if (is.null(label_names)) {
    if (is.null(dim_names <- names(dimnames(df)))) {
      label_names <- c("row", "column", "value")
    } else {
      label_names <- dim_names
    }
  } else {
    assert_that(length(label_names) == 3)
  }

  mdf <- melt_df(xx, label_names)
  if (!is.null(point_size_mat)) {
    ps_label_names <- label_names
    ps_label_names[[3]] <- point_size_name
    point_size_mat <- melt_df(point_size_mat, ps_label_names)
    mdf <- cbind(mdf, point_size_mat[point_size_name])
  }
  row <- label_names[[1]]
  col <- label_names[[2]]
  val <- label_names[[3]]

  mdf[["text"]] <- paste0(
    row, ": ", mdf[[1]], "<br>",
    col, ": ", mdf[[2]], "<br>",
    val, ": ", label_format_fun(mdf[[3]])
  )

  if (type == "heatmap") {
    geom <- "geom_tile"
    aes_args <- list(fill = paste_aes(val), text = "text")
    geom_args <- list(
      # mapping = aes_string(fill = paste_aes(val)),
      color = grid_color,
      size = grid_size
    )
  } else if (type == "scatter") {
    geom <- "geom_point"
    geom_args <- list()
    if (!is.null(point_size_mat)) {
      mdf[["text"]] <- paste(
        mdf[["text"]], "<br>",
        point_size_name, ": ", label_format_fun(mdf[[4]])
      )
      aes_args <- list(color = paste_aes(val),
        text = "text",
        size = paste_aes(point_size_name))

      # geom_args[["mapping"]] <- aes_string(
      #   color = paste_aes(val),
      #   text = "text",
      #   size = paste_aes(point_size_name)
      # )
    } else {
      geom_args[["size"]] <- grid_size
      aes_args <- list(color = paste_aes(val),
        text = "text")
      # geom_args[["mapping"]] <- aes_string(color = paste_aes(val), text = "text")
    }
  }
  if (!is.null(custom_hovertext)) {
    mdf[["text"]] <- paste0(mdf[["text"]], "<br>", custom_hovertext)
  }
  geom_args[["mapping"]] <- do.call(aes_string, aes_args)

  # TODO:
  # http://stackoverflow.com/questions/15921799/draw-lines-around-specific-areas-in-geom-tile
  # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  p <- ggplot(mdf, aes_string(x = paste_aes(col), y = paste_aes(row))) +
    ## Using the text aes produces a warning... Not ideal!
    suppressWarnings(do.call(geom, geom_args)) +
    scale_fill_gradient_fun +
    theme_bw() +
    theme_clear_grid_heatmap +
    theme(
      axis.title = element_blank(),
      axis.text.x = if (showticklabels[[1]]) {
        element_text(angle = column_text_angle, size = fontsize_col, hjust = 1)
      } else element_blank(),
      axis.ticks.x = if (showticklabels[[1]]) element_line() else element_blank(),
      axis.text.y = if (showticklabels[[2]]) {
        element_text(angle = row_text_angle, size = fontsize_row, hjust = 1)
      } else element_blank(),
      axis.ticks.y = if (showticklabels[[2]]) element_line() else element_blank(),
    )

  if (type == "scatter") {
    p <- p +
      coord_cartesian(xlim = c(1, ncol(xx)), ylim = c(1, nrow(xx)))
  } else {
    p <- p + coord_cartesian(expand = FALSE)
  }
  ## Passed in to allow users to alter (courtesy of GenVisR)
  if (!missing(layers)) {
    p <- p + layers
  }

  # p <- p + scale_x_discrete(limits = unique(mdf))
  # http://stats.stackexchange.com/questions/5007/how-can-i-change-the-title-of-a-legend-in-ggplot2
  p <- p + labs(fill = key.title)

  # until this bug is fixed: https://github.com/ropensci/plotly/issues/699
  # we are forced to use geom_hline and geom_vline
  if (!is.na(grid_color)) {
    p <- p + geom_hline(yintercept = c(0:nrow(xx)) + 0.5, color = grid_color) # , size = grid_size # not implemented since it doesn't work with plotly
    p <- p + geom_vline(xintercept = c(0:ncol(xx)) + 0.5, color = grid_color) # , size = grid_size # not implemented since it doesn't work with plotly
  }

  if (row_dend_left) {
    p <- p + scale_y_discrete(position = "right") # possible as of ggplot 2.1.0 !
  }

  p
}

melt_df <- function(x, label_names) {
  # heatmap
  # xx <- x$matrix$data
  df <- as.data.frame(x)

  row <- label_names[[1]]
  col <- label_names[[2]]
  val <- label_names[[3]]

  # colnames(df) <- x$matrix$cols
  if (!is.null(rownames(x))) {
    df[[row]] <- rownames(x)
  } else {
    df[[row]] <- 1:nrow(x)
  }

  df[[row]] <- factor(
    df[[row]],
    levels = df[[row]],
    ordered = TRUE
  )

  mdf <- reshape2::melt(df, id.vars = row)
  colnames(mdf)[2:3] <- c(col, val) # rename "variable" and "value"
  mdf
}

paste_aes <- function(x) {
  paste0("`", x, "`")
}

plotly_heatmap <- function(x,
                           limits = range(x),
                           colors = viridis(n = 256, alpha = 1, begin = 0, end = 1, option = "viridis"),
                           row_text_angle = 0,
                           column_text_angle = 45,
                           grid.color,
                           grid.size,
                           row_dend_left = FALSE,
                           fontsize_row = 10,
                           fontsize_col = 10,
                           key_title = "",
                           colorbar_xanchor = "left",
                           colorbar_yanchor = "bottom",
                           label_names = NULL,
                           colorbar_xpos = 1.1,
                           colorbar_ypos = 1,
                           colorbar_len = 0.3,
                           colorbar_thickness = 30,
                           custom_hovertext = NULL,
                           point_size_mat = NULL,
                           point_size_name = "Point size",
                           showticklabels = c(TRUE, TRUE)) {

  if (is.function(colors)) colors <- colors(256)

  if (is.null(label_names)) {
    if (is.null(dim_names <- names(dimnames(x)))) {
      label_names <- c("row", "column", "value")
    } else {
      label_names <- dim_names
    }
  } else {
    assert_that(length(label_names) == 3)
  }

  if (!is.null(custom_hovertext)) {
    text_mat <- custom_hovertext
  } else {
    text_mat <- as.data.frame(x)
    text_mat[] <- lapply(
      seq_along(text_mat),
      function(i) {
        paste0(
          label_names[3], ": ", x[, i], "<br>",
          label_names[2], ": ", colnames(x)[i], "<br>",
          label_names[1], ": ", rownames(x)
        )
      }
    )
    text_mat <- as.matrix(text_mat)
    if (!is.null(point_size_mat)) {
      point_size_name
    }
  }

  if (is.null(point_size_mat)) {
    p <- plot_ly(
      z = x, x = 1:ncol(x), y = 1:nrow(x), text = text_mat,
      type = "heatmap",
      showlegend = FALSE,
      colors = colors,
      hoverinfo = "text",
      zmin = limits[1], zmax = limits[2]
    )    
  } else {

    melt <- function(x, cn=colnames(x), rn=rownames(x)) {
      xdf <- reshape2::melt(x)
      xdf$Var1 <- factor(xdf$Var1, levels = rn)
      xdf$Var2 <- factor(xdf$Var2, levels = cn)
      xdf
    }
    xdf <- melt(x)
    tdf <- melt(text_mat)
    pdf <- melt(point_size_mat)

    p <- plot_ly(
      x = as.numeric(xdf$Var1),
      y = as.numeric(xdf$Var2),
      text = tdf$value,
      color = xdf$value,
      size = pdf$value,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE,
      colors = colors,
      hoverinfo = "text"
    )
  }
  p <- p  %>%
    layout(
      xaxis = list(
        tickfont = list(size = fontsize_col),
        tickangle = column_text_angle,
        tickvals = 1:ncol(x), ticktext = colnames(x),
        linecolor = "#ffffff",
        range = c(0.5, ncol(x) + 0.5),
        showticklabels = showticklabels[[1]]
      ),
      yaxis = list(
        tickfont = list(size = fontsize_row),
        tickangle = row_text_angle,
        tickvals = 1:nrow(x), ticktext = rownames(x),
        linecolor = "#ffffff",
        range = c(0.5, nrow(x) + 0.5),
        showticklabels = showticklabels[[2]]
      )
    )
  p <- plotly::colorbar(
    p,
    lenmode = "fraction",
    title = key_title,
    xanchor = colorbar_xanchor,
    x = colorbar_xpos,
    y = colorbar_ypos,
    yanchor = colorbar_yanchor,
    len = colorbar_len,
    thickness = colorbar_thickness
  )
  p
}


plotly_dend <- function(dend,
                        side = c("row", "col"),
                        flip = FALSE,
                        dend_hoverinfo = TRUE) {

  if (is.hclust(dend)) {
    dend <- as.dendrogram(dend)
  }

  side <- match.arg(side)
  dend_data <- as.ggdend(dend)
  segs <- dend_data$segments

  ## Have to get colors back from dendrogram otherwise plotly will make some up
  if (is.null(segs$col) || all(is.na(segs$col))) {
    segs$col <- rep(1, length(segs$col))
  }
  segs$col[is.na(segs$col)] <- "black" # default value for NA is "black"

  if (is.numeric(segs$col)) {
    segs$col <- factor(segs$col)
  }

  ## Need to somehow convert to colors that plotly will understand
  colors <- sort(unique(segs$col))
  if (is.numeric(colors)) {
    colors <- col2hex(grDevices::palette()[seq_along(colors)])
  }

  lab_max <- nrow(dend_data$labels)
  if (side == "row") {
    lab_max <- lab_max + 0.5
  }

  axis1 <- list(
    title = "",
    range = c(0, max(segs$y)),
    linecolor = "#ffffff",
    showgrid = FALSE
  )
  axis2 <- list(
    title = "",
    range = c(0, lab_max),
    linecolor = "#ffffff",
    showgrid = FALSE
  )

  ## Have to change x and y depending on which orientation
  if (side == "row") {
    add_plot_lines <- function(p) {
      p %>%
        add_segments(
          x = ~y, xend = ~yend, y = ~x, yend = ~xend, color = ~col,
          showlegend = FALSE,
          colors = colors,
          hoverinfo = if (dend_hoverinfo) "x" else "none"
        ) %>%
        layout(
          hovermode = "closest",
          xaxis = axis1,
          yaxis = axis2
        )
    }
  }
  else {
    add_plot_lines <- function(p) {
      p %>%
        add_segments(
          x = ~x, xend = ~xend, y = ~y, yend = ~yend, color = ~col,
          showlegend = FALSE,
          colors = colors,
          hoverinfo = if (dend_hoverinfo) "y" else "none"
        ) %>%
        layout(
          hovermode = "closest",
          xaxis = axis2,
          yaxis = axis1
        )
    }
  }

  p <- plot_ly(segs) %>% add_plot_lines()

  if (flip) {
    p <- layout(p, xaxis = list(autorange = "reversed"))
  }
  p
}

#' @title Side color plots for heatmaps
#' @description
#' Important for creating annotation.
#'
#' @param df A "molten" data.frame as produced by (eg) reshape2::melt
#' @param palette A function which can return colors to be used in the sidebar
#' plot
#' @param scale_title Title of the color scale. Not currently used.
#' @param type Horizontal or vertical plot? Valid values are "column" and "row"
#' @param text_angle the angle of the text of the rows/columns.
#' @param is_colors Use if the values in df are valid colours and should not be mapped
#'  to a color scheme, and instead should be plotted directly.
#' @param fontsize Font size (currently unused)
#' @param label_name Name for the mouseover label, usually "row" or "column"
#'
#' @return A ggplot geom_tile object
#'
ggplot_side_color_plot <- function(df,
                                   palette = NULL,
                                   scale_title = paste(type, "side colors"),
                                   type = c("column", "row"),
                                   text_angle = if (type == "column") 0 else 90,
                                   is_colors = FALSE,
                                   fontsize = 10,
                                   label_name = NULL) {

  type <- match.arg(type)
  if (is.matrix(df)) df <- as.data.frame(df)
  assert_that(is.data.frame(df))

  if (is.null(label_name)) label_name <- type

  ## Cooerce to character
  df[] <- lapply(df, as.character)

  ## TODO: Find out why names are dropped when ncol(df) == 1 (Not any more?)
  original_dim <- dim(df)

  if (is.null(palette)) palette <- default_side_colors

  ## Custom label
  if (type %in% colnames(df)) {
    stop("Having", type, "in the colnames of the side_color df will drop data!")
  }

  df[[type]] <- if (!is.null(rownames(df))) rownames(df) else 1:nrow(df)

  df[[type]] <- factor(df[[type]], levels = df[[type]], ordered = TRUE)
  df <- reshape2::melt(df, id.vars = type)
  df[["value"]] <- factor(df[["value"]])

  id_var <- colnames(df)[1]

  common_theme <- theme(
    panel.background = element_blank(),
    axis.ticks = element_blank()
  )

  ## Don't need this hack any more?
  # if(original_dim[2] > 1) {
  text_element <- element_text(angle = text_angle, hjust = 1, size = fontsize)
  # } else text_element <- element_blank()

  if (type == "column") {
    mapping <- aes_string(x = paste_aes(id_var), y = "variable", fill = "value")
    specific_theme <- theme(
      axis.text.x = element_blank(),
      axis.text.y = text_element
    )
  } else {
    mapping <- aes_string(x = "variable", y = paste_aes(id_var), fill = "value")
    specific_theme <- theme(
      axis.text.x = text_element,
      axis.text.y = element_blank(),
      legend.position = "top"
    )
  }
  theme <- list(common_theme, specific_theme)

  if (is_colors) {
    palette <- setNames(unique(df[["value"]]), unique(df[["value"]]))
  }

  if (is.function(palette)) {
    palette <- setNames(palette(nlevels(df[["value"]])), levels(df[["value"]]))
  } else if (!all(levels(factor(df[["value"]])) %in% names(palette))) {
    stop(paste0(
      "Not all levels of the ", type,
      "_side_colors are mapped in the ", type, "_side_palette"
    ))
  }

  g <- ggplot(df, mapping = mapping) +
    geom_tile() +
    xlab("") +
    ylab("") +
    scale_fill_manual(
      name = scale_title,
      breaks = levels(df[["value"]]),
      values = as.character(palette[levels(df[["value"]])])
    ) +
    theme
  return(g)
}



default_side_colors <- function(n) {
  ## Warning if n < 3 in any case
  suppressWarnings(
    if (n <= 12) {
      RColorBrewer::brewer.pal(n, "Paired")[seq_len(n)]
    } else if (n <= 20) {
      c(
        RColorBrewer::brewer.pal(12, "Paired"),
        RColorBrewer::brewer.pal(n - 12, "Set2")
      )[seq_len(n)]
    } else if (n <= 32) {
      c(
        RColorBrewer::brewer.pal(12, "Paired"),
        RColorBrewer::brewer.pal(8, "Set2"),
        RColorBrewer::brewer.pal(n - 20, "Set3")
      )[seq_len(n)]
    } else {
      colorspace::rainbow_hcl(n)
    }
  )
}

## Predict luminosity of cells and change text based on that
## http://stackoverflow.com/questions/12043187/how-to-check-if-hex-color-is-too-black
predict_colors <- function(p,
                           colorscale_df = p$x$data[[1]]$colorscale %||% p$x$data[[2]]$marker$colorscale,
                           cell_values = p$x$data[[1]]$z,
                           plot_method = c("ggplot", "plotly")) {

  plot_method <- match.arg(plot_method)

  if (is.null(cell_values)) {
    cell_values <- as.data.frame(
      matrix(
        p$x$data[[1]]$marker$size,
        ncol = length(unique(p$x$data[[1]]$x)),
        nrow = length(unique(p$x$data[[1]]$y))
      )
    )
  }
  if (is.null(colorscale_df)) {
    stop("Internal error in predicting colours!")
  }
  cell_values <- as.data.frame(cell_values)
  cell_values$row <- seq_len(nrow(cell_values))
  cell_values_m <- reshape2::melt(cell_values, id.vars = "row")
  cell_values_vector <- cell_values_m$value

  ## Need to normalise to (0, 1) scale as this is what plotly
  ## uses internally
  if (plot_method == "plotly") {
    ## Need to convert plotly colors to hex colors
    colorscale_df[, 2] <- parse_plotly_color(colorscale_df[, 2])

    cell_values_vector <- normalize(as.numeric(cell_values_vector))
    ## interpolate to 256 colors because that's probably enough
    colorscale_df <- data.frame(
      stats::approx(as.numeric(colorscale_df[, 1]), n = 256)$y,
      grDevices::colorRampPalette(colorscale_df[, 2])(256)
    )

    ## Then need to sort, find nearest neighbour, and map across
    cell_values_vector_sort <- sort(cell_values_vector)
    nearest_neighbours <- sapply(
      cell_values_vector_sort,
      function(val) {
        max(colorscale_df[as.numeric(colorscale_df[, 1]) <= val, 1])
      }
    )
    names(nearest_neighbours) <- cell_values_vector_sort
    cell_values_vector <- nearest_neighbours[as.character(cell_values_vector)]
  }

  cell_values_vector <- as.character(cell_values_vector)
  ind <- match(cell_values_vector, colorscale_df[, 1])
  cell_colors <- unlist(colorscale_df[ind, 2])

  # apply colors only to non-NA cells
  # In the future, it might be worth using na.color
  cell_colors <- as.character(cell_colors)
  cell_colors[is.na(cell_colors)] <- "#ffffff" # make the default white for NA values

  cell_colors_rgb <- colorspace::hex2RGB(cell_colors)
  cell_font_colors <- sapply(
    seq_len(nrow(cell_colors_rgb@coords)),
    function(i) {
      col <- cell_colors_rgb@coords[i, ]
      luma <- (0.2126 * col[1]) +
        (0.7152 * col[2]) +
        (0.0722 * col[3])
      ifelse(luma < 0.4, "white", "black")
    }
  )
  cell_font_colors
}


parse_plotly_color <- function(color) {
  r <- gsub(
    "rgb[a]?\\((\\d+),(\\d+),(\\d+)(,\\d+)?)",
    "\\1", color
  )
  g <- gsub(
    "rgb[a]?\\((\\d+),(\\d+),(\\d+)(,\\d+)?)",
    "\\2", color
  )
  b <- gsub(
    "rgb[a]?\\((\\d+),(\\d+),(\\d+)(,\\d+)?)",
    "\\3", color
  )
  rgb(r, g, b, maxColorValue = 255)
}


## Helper function to generate "normal" colors for dendrograms
## ie black if one k or rainbow_hcl otherwise
k_colors <- function(k) {
  if (k > 1) {
    colorspace::rainbow_hcl(k)
  } else {
    "black"
  }
}


# Create a plotly colorscale from a list of colors in any format.
discrete_colorscale <- function(colors) {
  colors <- rep(colors, each = 100)
  seq <- seq(0, 1, length.out = length(colors))
  setNames(data.frame(seq, colors), NULL)
}

#' @importFrom stats setNames
plotly_side_color_plot <- function(df,
                                   palette = NULL,
                                   scale_title = paste(type, "side colors"),
                                   type = c("column", "row"),
                                   text_angle = if (type == "column") 0 else 90,
                                   is_colors = FALSE,
                                   label_name = NULL,
                                   colorbar_len = 0.3,
                                   fontsize = 10) {

  type <- match.arg(type)

  if (is.null(label_name)) label_name <- type

  data <- df
  data[] <- lapply(df, factor)
  # if (type == "column") {
  #   data <- t(data)
  # }
  data <- as.data.frame(data, stringsAsFactors = TRUE)
  data[] <- lapply(data, factor)
  data_vals <- unlist(data)
  levels <- levels(data_vals)
  levels <- levels[!is.na(levels)]

  if (is.null(palette)) palette <- default_side_colors

  if (is.function(palette)) {
    palette <- setNames(palette(length(levels)), levels)
  } else {
    palette <- setNames(col2hex(palette), names(palette))
    if (!all(levels %in% names(palette))) {
      stop(paste0(
        "Not all levels of the ", type,
        "_side_colors are mapped in the ", type, "_side_palette"
      ))
    }
  }

  levs2colors <- palette[as.character(levels)]
  levs2nums <- setNames(seq_along(levels), levels)

  df_nums <- data
  df_nums[] <- lapply(data, function(col) as.numeric(levs2nums[as.character(col)]))
  df_nums <- as.matrix(df_nums)
  if (type == "column") {
    df_nums <- t(df_nums)
  }
  if (ncol(df) == 1) {
    key_title <- colnames(df)
  } else {
    key_title <- paste(gsub("^(\\w)", "\\U\\1", type, perl = TRUE), "annotation")
  }

  text_mat <- data
  text_mat[] <- lapply(
    seq_along(text_mat),
    function(i) {
      if (type == "row") {
        paste0(
          "value: ", data[, i], "<br>",
          "variable: ", colnames(data)[i], "<br>",
          label_name, ": ", rownames(data)
        )
      } else {
        paste0(
          "value: ", data[, i], "<br>",
          label_name, ": ", colnames(data)[i], "<br>",
          "variable: ", rownames(data)
        )
      }
    }
  )

  ## Ensure tickvals are in right position when n = 2
  offset <- ((length(levels) - 1) / length(levels)) / 2

  ## https://stackoverflow.com/questions/42524450/using-discrete-custom-color-in-a-plotly-heatmap
  p <- plot_ly(
    z = df_nums, x = 1:ncol(df_nums), y = 1:nrow(df_nums),
    text = as.matrix(text_mat), hoverinfo = "text",
    type = "heatmap", showlegend = FALSE, colors = levs2colors,
    colorscale = discrete_colorscale(levs2colors),
    colorbar = list(
      # Capitalise first letter
      title = key_title,
      tickmode = "array",
      ## Issue #137
      tickvals = seq(
        1 + offset,
        length(levels) - offset,
        length.out = length(levels)
      ),
      ticktext = levels,
      len = colorbar_len
    )
  )
  if (type == "row") {
    p <- p %>% layout(
      xaxis = list(
        tickfont = list(size = fontsize),
        tickangle = text_angle,
        tickvals = 1:ncol(df_nums), ticktext = colnames(df_nums),
        linecolor = "#ffffff",
        range = c(0.5, ncol(df_nums) + 0.5),
        showticklabels = TRUE
      ),
      yaxis = list(showticklabels = FALSE)
    )
  } else {
    p <- p %>% layout(
      yaxis = list(
        tickfont = list(size = fontsize),
        tickangle = text_angle,
        tickvals = 1:nrow(df_nums), ticktext = rownames(df_nums),
        linecolor = "#ffffff",
        range = c(0.5, nrow(df_nums) + 0.5),
        showticklabels = TRUE
      ),
      xaxis = list(showticklabels = FALSE)
    )
  }
  p
}





#' @import webshot
NULL
# just so to have an excuse for why webshot is in import (the real reason is that plotly has it as suggests while it is used there by plotly::export)
# webshot <- webshot::webshot



# TODO: add roxydoc

# This function gets a heatmaply object and a file, and writes that to a file
# we should later add control over width...
hmly_to_file_1file <- function(hmly, file, width = NULL, height = NULL, ...) {

  # tools::file_ext("hithere.png")
  # tools::file_ext("hithere.html")
  # tools::file_ext("hithere.html.png")
  file_extension <- tolower(tools::file_ext(file))
  if (!(file_extension %in% c("html", "pdf", "png", "jpeg"))) {
    stop(paste(
      "file extension must be one of the following:",
      "\"html\", \"pdf\", \"png\", or \"jpeg\".",
      "Since it was not, your heatmaply was not saved to a file."
    ))
  } else {
    if (file_extension == "html") {
      # solution to dealing with the folder:
      # https://stackoverflow.com/questions/41399795/savewidget-from-htmlwidget-in-r-cannot-save-html-file-in-another-folder
      tmp_fp <- file
      tmp_fp <- file.path(normalizePath(dirname(tmp_fp)), basename(tmp_fp))
      hmly %>% htmlwidgets::saveWidget(file = tmp_fp, selfcontained = TRUE)
    }
    if (file_extension %in% c("pdf", "png", "jpeg")) {
      if (is.null(width)) {
        width <- size_default(file_extension, "width")
      }
      if (is.null(height)) {
        height <- size_default(file_extension, "height")
      }
      tryCatch(
        export_orca(hmly, file, width, height),
        error = function(e) {
          warning <- paste("plotly::orca failed:\n", e)
          warning(warning)
          export(hmly, file, width, height)
        }
      )
    }
  }
  invisible(hmly)
}

export_orca <- function(x, file, width, height) {
  plotly::orca(
    x,
    file = file,
    width = width,
    height = height
  )
}

export <- function(x, file, width, height) {
  plotly::export(
    x,
    file = file,
    vwidth = width,
    vheight = height,
    cliprect = "viewport"
  )
}

size_default <- function(file_extension, direction=c("width", "height")) {
  direction <- match.arg(direction)
  ## webshot uses viewport size in pixels to control file size, so
  ## all sizes in pixels
  # switch(direction,
  #   "width" = if (file_extension %in% bitmap_types) 800 else 8,
  #   "height" = if (file_extension %in% bitmap_types) 500 else 5
  # )
  switch(direction,
    "width" = 800,
    "height" = 500
  )
}


bitmap_types <- c("png", "jpeg")

hmly_to_file <- Vectorize(hmly_to_file_1file, vectorize.args = "file")

## Copied from gplots
#' @importFrom grDevices col2rgb
col2hex <- function(col) {
    colMat <- col2rgb(col)
    rgb(red = colMat[1, ] / 255,
        green = colMat[2, ] / 255,
        blue = colMat[3, ] / 255)
}

