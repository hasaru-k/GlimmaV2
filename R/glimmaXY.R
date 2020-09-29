#' Glimma XY Plot
#'
#' Draws a two-panel interactive XY scatter plot.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @param x numeric vector of values to plot on the x-axis of the summary plot.
#' @param y numeric vector of values to plot on the y-axis of the summary plot.
#' @param status vector of length \code{length(x)} indicating the status of each gene.
#' A value of -1 marks a down-regulated gene, 0 marks a gene with no expression difference, and
#' 1 marks an up-regulated gene.
#' @param anno dataframe with \code{length(x)} rows containing gene annotations.
#' @param groups vector of length \code{ncol(counts)} representing categorisation of samples in expression plot.
#' @param counts numeric matrix with \code{length(x)} rows containing gene expression values. This can be used to replace
#' raw gene counts from dge$counts with transformed counts e.g. logCPM or logRPKM values.
#' @eval XY_details()
#' @export
glimmaXY <- function(
  x,
  y,
  xlab="x",
  ylab="y",
  counts=NULL,
  groups=NULL,
  status=rep(0, length(x)),
  anno=NULL,
  display.columns = NULL,
  status.cols=c("dodgerblue", "silver", "firebrick"),
  sample.cols=NULL,
  transform.counts=FALSE,
  main="XY Plot",
  html=NULL,
  width = 920,
  height = 920)
{
  if (length(x)!=length(y)) stop("Error: x and y args must have the same length.")
  table <- data.frame(signif(x, digits=4), signif(y, digits=4))
  colnames(table) <- c(xlab, ylab)
  # add rownames to LHS of table
  if (!is.null(counts)) {
    table <- cbind(gene=rownames(counts), table)
  } else if (!is.null(rownames(x))) {
    table <- cbind(gene=rownames(x), table)
  } else if (!is.null(rownames(y))) {
    table <- cbind(gene=rownames(y), table)
  } else {
    table <- cbind(gene=1:length(x), table)
  }

  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.cols, sample.cols, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height, html))
}

#' XY Data Object Builder
#'
#' Common processing steps for both MA, XY and volcano plots.
#' Expects a dataframe, \code{table}, which contains two columns labelled \code{xlab} and \code{ylab}
#' as well as a unique identifier column labelled \code{gene}.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @param table dataframe containing xlab and ylab columns for plotting.
#' @importFrom edgeR cpm
#' @keywords internal
buildXYData <- function(
  table,
  status,
  main,
  display.columns,
  anno,
  counts,
  xlab,
  ylab,
  status.cols,
  sample.cols,
  groups,
  transform.counts)
{
  if (is.null(counts)) {
    counts <- -1
    level <- NULL
  } else {
    # df format for serialisation
    if (transform.counts) {
      if (!all.equal(counts, as.integer(counts))) {
        warning("count transform requested but not all count values are integers.")
      }
      counts <- edgeR::cpm(counts, log=TRUE)
    }
    counts <- data.frame(counts)
    #if (is.null(groups)) stop("If counts arg is supplied, groups arg must be non-null.")
    if (is.null(groups)) {
      groups <- factor("group")
    } else {
      if (ncol(counts) != length(groups)) stop("Length of groups must be equal to the number of columns in counts.\n")
    }

    level <- levels(groups)
    groups <- data.frame(group=groups)
    groups <- cbind(groups, sample=colnames(counts))
  }

  if (length(status)!=nrow(table)) stop("Status vector
     must have the same number of genes as the main arguments.")

  table <- cbind(table, status=as.vector(status))
  if (!is.null(anno)) table <- cbind(table, anno)

  if (is.null(display.columns)) {
    display.columns <- colnames(table)
  } else {
    # if it's specified, make sure at least x, y, gene are displayed in the table and tooltips
    if (!(xlab %in% display.columns)) display.columns <- c(display.columns, xlab)
    if (!(ylab %in% display.columns)) display.columns <- c(display.columns, ylab)
    if (!("gene" %in% display.columns)) display.columns <- c("gene", display.columns)
  }

  table <- data.frame(index=0:(nrow(table)-1), table)

  if (length(status.cols) != 3) stop("status.cols
          arg must have exactly 3 elements for [downreg, notDE, upreg]")

  xData <- list(data=list(x=xlab,
                          y=ylab,
                          table=table,
                          cols=display.columns,
                          counts=counts,
                          groups=groups,
                          levels=level,
                          expCols=colnames(groups),
                          statusColours=status.cols,
                          sampleColours= if (is.null(sample.cols)) {-1} else {sample.cols},
                          title=main))
  return(xData)
}

#' GlimmaXY HTMLWidget Wrapper
#'
#' Passes packaged data to JS interface for rendering.
#'
#' @param xData packaged data object returned from buildXYData()
#' @param width htmlwidget element width in pixels
#' @param height htmlwidget element height in pixels
#' @param html name of HTML file (including extension) to export widget into rather than displaying the widget; \code{NULL} by default.
#' @import htmlwidgets
#' @keywords internal
glimmaXYWidget <- function(xData, width, height, html)
{
  widget <- htmlwidgets::createWidget(
    name = 'glimmaXY',
    xData,
    width = width,
    height = height,
    package = 'GlimmaV2',
    elementId = NULL,
    sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth=width, defaultHeight=height, browser.fill=TRUE, viewer.suppress=TRUE)
  )
  if (is.null(html))
  {
    return(widget)
  }
  else
  {
    message("Saving widget...")
    htmlwidgets::saveWidget(widget, file=html)
    message(html, " generated.")
  }
}
