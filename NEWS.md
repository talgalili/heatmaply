heatmaply 0.9.0 (2017-04-09)
==============================

This release adds unit testing and code coverage to the heatmaply package.
Users should not be affected, but this will hopefully accelerate development
and reduce the occurrence of bugs.

* heatmaply - 
    * plot_method argument ("plotly" or "ggplot") which controls the underlying
      plot method used for dendrograms and the main heatmap.
      Currently there is some disparity between the features 
      available in both methods but it is hoped this can be addressed.
      plotly will likely give higher performance for large matrices.
    * Add colorbar_len argument, which controls the fraction of the total height
      which the colorbar will take up.
    * Allowed for colorbar and side color legends to be displayed simultaneously
      when plot_method is "plotly"
    * Added "long_data" argumnet, which allows the user to use data in "long" 
      format (eg, http://www.theanalysisfactor.com/wide-and-long-data/).
    * Added "label_names" argument, which allows the user to specify names to
      replace "row" and "column" as the names of the mouseover co-ordinates.
    * Added "cellnote" functionality, allowing the display of text overlaid
      on the cells of the heatmap. Controlled by the "draw_cellnote" argument.
    * Added "fontsize_row" and "fontsize_col" arguments to heatmaply,
      which control font size for row and column labels. cexRow and cexCol are 
      aliases.

* heatmapr
    * Removed some unused arguments and code.

### BUG FIXES
* heatmaply
  * Fixed the cexRow and cexCol arguments, which were previously non-functional.

heatmaply 0.8.0 (2017-03-18)
==============================

### NEW FEATURES
* heatmaply -
      * file parameter - allows to save a heatmaply plot as an HTML file.
      * Allow Rowv/Colv to also work for hclust (by turning them into a dendrogram).
      * Add examples to heatmaply's Rd and the vignette on using Rowv/Colv.
      * Added the parameters hclust_method, dist_method to allow a more refined control over dist and hclust functions which are creating the dendrograms. A user wanting an even more refined control should just supply dendrograms directly to Rowv and Colv.
      * Added the parameters: distfun_row, hclustfun_row, distfun_col, hclustfun_col. They allow a more refine control over the dendrograms of the rows/columns (without the user needing to create the dendrograms from scratch)
      * clean the modeBarButtons from irrelevent icons
  * RColorBrewer_colors - added colors to be available for the heatmap.
      The sequential palettes names are (less important since we have viridis):  Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
      The diverging palettes are: BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral




### BUG FIXES
* heatmaply -
  * make row_side_colors/col_side_colors work for vectors as well (by turning them to a data.frame). Fixes:  Error in if (dim(row_side_colors)[1] != dim(x)[1]) stop("row_side_colors and x have different numbers of rows") : argument is of length zero



heatmaply 0.7.0 (2017-01-21)
==============================

### NEW FEATURES
* heatmaply -
      * Non numeric columns are now added to `row_side_colors`, making heatmaply reasonably robust to heterogeneous data.frames.
      * main - a new parameter for setting the title of the plot.
      * the margins parameter now accepts 4 elements (previously it was only 2), for bottom, left, top (relevant for the plot's title) and right margins.
* heatmapr -
      * Allow k_row and k_col to accept NA. This will pick the number of clusters using dendextend::find_k

### NEW FUNCTIONS
* percentize - a pre-processing function for performing the Empirical Percentile Transformation on a vector or data.frame.
* normalize - a pre-processing function for adjusing the range of the variables to be from 0 to 1.
* is.na10 - Indicates which elements are missing (either 1 and 0)



heatmaply 0.6.1 (2016-11-11)
==============================

### BUG FIXES
* Allow dendrogram parameter to work again.


heatmaply 0.6.0 (2016-11-05)
==============================

### NEW FEATURES
* heatmaply -
      * added `row_side_colors` to allow non-numeric values in the sidebar of the
        heatmap, similar to RowSideColors in gplots::heatmap.2 (`RowSideColors` also
        works in heatmaply). Props goes to Alanocallaghan for his work on this.
      * added a proper `margins` parameter.
      * grid_color now works (thanks to a hack of using geom_hline and
        geom_vline instead of heom_tile(..., color) in ggplot_heatmap )
      * added branches_lwd - to control the width of the dendrograms' width.


heatmaply 0.5.0 (2016-08-04)
==============================

### NEW FEATURES
* is.heatmapr
* ggheatmap - first try. (not working well enough yet. The proportions of the elements are not good)
* heatmaply
  * key.title - a parameter to control the main title of the color key. (feature request by John Rizk)



heatmaply 0.4.0 (2016-07-15)
==============================

### NEW FEATURES
* heatmaply
  * grid_color - control the color of the heatmap grid. This is currently not working until this feature will be added by plotly.
  * hover over dendrogram now returns the height.
  * colors can now also accept a function (and will use it to produce 256 colors)
* heatmapr
  * seriate - character indicating the method of matrix sorting (default: "OLO"). This uses the seriation package.
  * srtRow, srtCol - add legacy support for these parameters (they are passed to row_text_angle and column_text_angle)
  * hide_colorbar - controls if the color bar should be hidden.
  * xlab, ylab - add titles to the x and y axis.


heatmaply 0.3.2 (2016-05-26)
==============================

### ANNOUNCMENTS
* heatmaply 0.3.2 - first CRAN release!

### BUG FIXES
* http -> https

heatmaply 0.3.1 (2016-05-26)
==============================
### BUG FIXES
* fix minor typos.

heatmaply 0.3.0 (2016-05-25)
==============================


### NEW FEATURES
* heatmaply
  * Now works with Rowv=F and Colv=F (by introducing a new un-exported function: heatmap_subplot_from_ggplotly)
  * Remove space between the heatmap and dendrograms (via: coord_cartesian(expand = FALSE)  and coord_flip(expand = FALSE))
  * Added the margin parameter (to control the distance between the heatmap and the dendrograms.)
  * Added row_text_angle and column_text_angle (with srtRow and srtCol for backward compatibility with gplots::heatmap.2). Fix #3

### BUG FIXES
* fix #2 : Error: Don't know how to add scale_fill_gradient_fun to a plot
  by moving "scale_fill_gradient_fun" after "..." (I may change this parameter's name later)


### VIGNETTE
* heatmaply now has a basic vignette.


heatmaply 0.2.1 (2016-05-23)
==============================

### BUG FIXES
* fix various import issues that caused warnings with devtools::check()


heatmaply 0.2.0 (2016-05-23)
==============================
### NEW FEATURES
* More control over colors in heatmaply via the new parameters: colours, limits, na.value, and scale_colour_gradient_fun.
* first attempts at row_dend_left (although this is not yet working.)

### BUG FIXES
* dendrograms are now presented without axes text.
* passing scale (= "row" or "column") works again.


heatmaply 0.1.0 (2016-05-14)
==============================

* First (very rough) version. It has a minimal working example, as well as MANY things to fix/tweak/adjust.




TODO:
==============================
* add bars for extra categories
* remove unneeded code from d3heatmap
* add many options for controlling the heatmap "as it should be"
* implement all relevant options streight to heatmaply.
* ggheatmap?
* Expose widths and heights from heatmap_subplot_from_ggplotly to heatmaply

* create colors from RcolorBrewer
* write example for using seriation+dendextend for heatmaps.

* add seriation to imports
* Show the following example for using seriation:



require(seriation)
require(dendextend)
# "GW", "OLO"
d <- dist(USArrests[1:15,])
dend <- as.dendrogram(hclust(d, method = "ave"))
par(mfrow = c(1,2))
plot(dend, main = "default")
# seriate(cophenetic(dend), method = "OLO", control = list(hclust = as.hclust(dend)))
# the downside in using cophenetic is that seriate has to go through running hclust all over again
# but we'll just have to accept it...
o <- seriate(d, method = "GW", control = list(hclust = as.hclust(dend)) )
get_order(o)
labels(cophenetic(dend))[get_order(o)]
d2 <- rotate(dend, order = rev(labels(d)[get_order(o)]))
plot(d2, main = "GW")

o <- seriate(d, method = "OLO", control = list(hclust = as.hclust(dend)) )
d3 <- rotate(dend, order = rev(labels(d)[get_order(o)]))

require(heatmaply)
heatmaply(USArrests[1:15,], Rowv = d2)
heatmaply(USArrests[1:15,], Rowv = d3)
heatmaply(USArrests[1:15,], Rowv = dend)

hmap(USArrests[1:15,])



identical(seriate(d, method = "OLO"),
		seriate(d, method = "OLO", control = list(hclust = as.hclust(dend)) ))
get_order(seriate(d, method = "OLO"))
get_order(seriate(d, method = "OLO", control = list(hclust = hclust(d, method = "sing")) )) # this works :)









