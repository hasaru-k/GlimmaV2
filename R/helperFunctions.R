MA_details <- function() {
  c(
    "@details",
    "The left plot shows the log-fold-change vs average expression.",
    "The right plot shows the expression levels of a particular gene of each sample.",
    "Clicking on genes in the plot brings up the corresponding genes in the table.",
    "Selecting rows in the table will highlight the corresponding genes in the MA plot.",
    "Expression values for a gene can be found by hovering over a sample in the right plot.",
    "@return htmlwidget object."
  )
}

XY_details <- function() {
  c(
    "@details",
    "The left plot shows the x and y values specified.",
    "The right plot shows the expression levels of a particular gene of each sample.",
    "Clicking on genes in the plot brings up the corresponding genes in the table.",
    "Selecting rows in the table will highlight the corresponding genes in the XY plot.",
    "Expression values for a gene can be found by hovering over a sample in the right plot.",
    "@return htmlwidget object."
  )
}

volcano_details <- function() {
  c(
    "@details",
    "The left plot shows the log-fold-change vs -log10(p-value).",
    "The right plot shows the expression levels of a particular gene of each sample.",
    "Clicking on genes in the plot brings up the corresponding genes in the table.",
    "Selecting rows in the table will highlight the corresponding genes in the MA plot.",
    "Expression values for a gene can be found by hovering over a sample in the right plot.",
    "@return htmlwidget object."
  )
}

#' extractGroups
#'
#' extract groups vector from colData(x) if it is present; otherwise return
#' vector of 1s.
#'
#' @param x SummarizedExperiment object
#' @importFrom SummarizedExperiment colData
extractGroups <- function(x)
{
  colData <- SummarizedExperiment::colData(x)
  if ("group" %in% colnames(colData))
  {
    groups <- colData[, "group"]
  } else
  {
    groups <- 1
  }
  return(groups)
}