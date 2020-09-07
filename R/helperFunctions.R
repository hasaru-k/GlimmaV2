MA_details <- function()
{
  c(
    "@details",
    "The left plot shows the log-fold-change vs average expression.",
    "The right plot shows the expression levels of a particular gene of each sample.",
    "Clicking on genes in the plot brings up the corresponding genes in the table.",
    "Selecting rows in the table will highlight the corresponding genes in the MA plot.",
    "Expression values for a gene can be found by hovering over a sample in the right plot.",
    "@return htmlwidget object or \\code{NULL} if \\code{html} argument is specified."
  )
}

XY_details <- function()
{
  c(
    "@details",
    "The left plot shows the x and y values specified.",
    "The right plot shows the expression levels of a particular gene of each sample.",
    "Clicking on genes in the plot brings up the corresponding genes in the table.",
    "Selecting rows in the table will highlight the corresponding genes in the XY plot.",
    "Expression values for a gene can be found by hovering over a sample in the right plot.",
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
