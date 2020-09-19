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
    "The left plot shows the log-fold-change vs -log10(pvalue).",
    "The right plot shows the expression levels of a particular gene of each sample.",
    "Clicking on genes in the plot brings up the corresponding genes in the table.",
    "Selecting rows in the table will highlight the corresponding genes in the volcano plot.",
    "Expression values for a gene can be found by hovering over a sample in the right plot.",
    "@return htmlwidget object or \\code{NULL} if \\code{html} argument is specified."
  )
}

MDS_details <- function() 
{
  c(
    "@details",
    "The left plot shows two MDS dimensions, with annotations displayed on hover.",
    "The right panel contains a bar plot of the eigenvalues of each dimension.",
    "The controls beneath the plots can be used to change the dimensions being displayed.",
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