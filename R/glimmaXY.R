#' Glimma XY Plot
#'
#' Draws a two-panel interactive XY scatter plot.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @param x numeric vector of values to plot on the x-axis of the summary plot.
#' @param y numeric vector of values to plot on the y-axis of the summary plot.
#' @eval XY_details()
#' @export
glimmaXY <- function(
  x,
  y,
  xlab="x",
  ylab="y",
  status=rep(0, length(x)),
  main="XY Plot",
  display.columns = NULL,
  anno=NULL,
  groups=NULL,
  counts=NULL,
  status.colours=c("dodgerblue", "silver", "firebrick"),
  transform.counts=FALSE,
  save=FALSE,
  filename="glimmaXY.html",
  width = 920,
  height = 920)
{
  if (length(x)!=length(y)) stop("Error: x and y args must have the same length.")
  table <- data.frame(round(x, digits=4),  round(y, digits=4))
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
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height))
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
buildXYData <- function(
  table,
  status,
  main,
  display.columns,
  anno,
  counts,
  xlab,
  ylab,
  status.colours,
  groups,
  transform.counts)
{

  if (is.null(counts)) {
    counts <- -1
  } else {
    # df format for serialisation
    if (transform.counts) counts <- edgeR::cpm(counts, log=TRUE)
    counts <- data.frame(counts)
    if (is.null(groups)) stop("If counts arg is supplied, groups arg must be non-null.")
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

  if (length(status.colours) != 3) stop("status_colours
          arg must have exactly 3 elements for [downreg, notDE, upreg]")

  xData <- list(data=list(x=xlab,
                          y=ylab,
                          table=table,
                          cols=display.columns,
                          counts=counts,
                          groups=groups,
                          expCols=colnames(groups),
                          statusColours=status.colours,
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
#' @param save if \code{TRUE}, widget will be exported to standalone HTML file rather than being displayed.
#' @param filename name of the standalone HTML file created if \code{save} is \code{TRUE}.
#' @import htmlwidgets
glimmaXYWidget <- function(xData, width, height, save, filename)
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
  if (!save)
  {
    return(widget)
  }
  else
  {
    message("Saving widget...")
    htmlwidgets::saveWidget(widget, file=filename)
    message(filename, " generated.")
  }
}