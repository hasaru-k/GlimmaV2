MA_details <- function()
{
  c(
    "@details",
    "The summary plot on the left represents gene-wise log-fold-change (logFC) on the y-axis versus average gene
    expression calculated as log-counts-per-million (logCPM) values.
    We call our summary plot an MA plot because this type of plot was originally referred to as an MA plot in
    the \\code{limma} package, with the M-value representing logFC and A-value representing average expression -
    it has since been renamed to MD plot in the \\code{limma} package.
    The expression plot on the right displays sample expression values for a single gene.
    Interactions with the htmlwidget include clicking on genes (points) in the summary plot to bring up
    associated sample expression values in the expression plot, as well as the summary statistics in the table below.
    Alternatively, users can interact with the table by clicking on genes (rows) to highlight genes in the summary plot,
    as well as bring up associated sample expression values in the expression plot.
    Briefly, other interactive features include a search box for the table, buttons to save plots and data
    (summary statistics and expression values), additional pop-up information when hovering on points in plots,
    and rescaling of the y-axis in the expression plot.",
    "@return htmlwidget object or \\code{NULL} if \\code{html} argument is specified."
  )
}

XY_details <- function()
{
  c(
    "@details",
    "The summary plot on the left displays the x and y values specified.
    The expression plot on the right displays sample expression values for a single gene.
    Interactions with the htmlwidget include clicking on genes (points) in the summary plot to bring up
    associated sample expression values in the expression plot, as well as the summary statistics in the table below.
    Alternatively, users can interact with the table by clicking on genes (rows) to highlight genes in the summary plot,
    as well as bring up associated sample expression values in the expression plot.
    Briefly, other interactive features include a search box for the table, buttons to save plots and data
    (summary statistics and expression values), additional pop-up information when hovering on points in plots,
    and rescaling of the y-axis in the expression plot.",
    "@return htmlwidget object or \\code{NULL} if \\code{html} argument is specified."
  )
}

volcano_details <- function()
{
  c(
    "@details",
    "The summary plot on the left represents gene-wise log-fold-change (logFC) on the x-axis versus
    \\code{-log10(pvalue)}. The expression plot on the right displays sample expression values for a single gene.
    Interactions with the htmlwidget include clicking on genes (points) in the summary plot to bring up
    associated sample expression values in the expression plot, as well as the summary statistics in the table below.
    Alternatively, users can interact with the table by clicking on genes (rows) to highlight genes in the summary plot,
    as well as bring up associated sample expression values in the expression plot.
    Briefly, other interactive features include a search box for the table, buttons to save plots and data
    (summary statistics and expression values), additional pop-up information when hovering on points in plots,
    and rescaling of the y-axis in the expression plot.",
    "@return htmlwidget object or \\code{NULL} if \\code{html} argument is specified."
  )
}

MDS_details <- function()
{
  c(
    "@details",
    "The left plot shows two MDS dimensions, with sample annotations displayed on hover.
     The right panel contains a bar plot of the eigenvalues of each dimension.
     The controls beneath the plots can be used to change the dimensions being displayed, and the scale, colour and shape of points.
     The interactive MDS plot allows users to adjust sample points by scale, colour and shape for multiple vectors associated with
     sample information. This is carried out most effectively when \\code{x$samples} includes an abundance of sample information, or
     when a data frame object is supplied to \\code{groups}. If a simple character or factor vector is given to \\code{groups}
     (with the default of \\code{continous.colour=FALSE}), then sample points will have no scaling options, but can only be adjusted
     in colour and shape by \\code{groups} and \\code{labels}. Instead, if \\code{groups} is a numeric vector (e.g. library size or
     expression level of a specific gene), then the plot can be scaled and coloured by the numeric values with
     \\code{continous.colour=TRUE}.",
    "@return htmlwidget object or \\code{NULL} if \\code{html} argument is specified."
  )
}

#' extractGroups
#'
#' Extracts the column named \code{group} from column data matrix
#' of a SummarizedExperiment object if it is present. Otherwise return a
#' vector of 1s.
#'
#' @param cdata SummarizedExperiment column data matrix
#'
#' @return groups column of data if present, otherwise 1
#'
#' @keywords internal
extractGroups <- function(cdata)
{
  if ("group" %in% colnames(cdata))
  {
    return(cdata[, "group"])
  } else
  {
    return(1)
  }
}
