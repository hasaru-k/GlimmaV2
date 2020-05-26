glimmaMDS <- function(x, ...)
{
  xData <- prepareMDSData(x, ...)
  return(GlimmaV2(xData))
}

glimmaMA <- function(x, ...)
{
  xData <- prepareXYData(x, plotType="MA",...)
  return(GlimmaV2(xData))
}

# required: x and y
glimmaXY <- function(x, y, xlab="x", ylab="y", ...)
{
  # might be a bit confusing that there are two x variables?
  # maybe rename second x to fit
  xData <- prepareXYData(x=NULL, xvals=x, yvals=y, xlab=xlab, ylab=ylab, plotType="XY", ...)
  return(GlimmaV2(xData))
}

#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
GlimmaV2 <- function(
  xData,
  width = NULL,
  height = NULL,
  elementId = NULL,
  ...)
{

  # create widget
  htmlwidgets::createWidget(
    name = 'GlimmaV2',
    xData,
    width = width,
    height = height,
    package = 'GlimmaV2',
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth=750, defaultHeight=750, browser.fill=TRUE, viewer.suppress=TRUE)
  )

}

prepareMDSData <- function(x, ...)
{
  UseMethod("prepareMDSData")
}

prepareMDSData.default <- function(
  x,
  top = 500,
  labels = as.character(seq_len(ncol(x))),
  groups = as.character(rep(1, ncol(x))),
  gene.selection = c("pairwise", "common"),
  continuous.colour=FALSE)
{

  # helper function
  getCols <- function(x, inds) {
      x[, inds, drop=FALSE]
  }

  # CODE TAKEN FROM GLIMMA

  #   Multi-dimensional scaling with top-distance
  #   Di Wu and Gordon Smyth
  #   19 March 2009.  Last modified 14 Jan 2015
  #   Modified by Shian Su on 25 Jan 2016

  ##
  # Check Inputs

  x <- as.matrix(x)
  nsamples <- ncol(x)
  ndim <- nsamples - 1

  if (nsamples < 3) {
      stop(paste("Only", nsamples, "columns of data: need at least 3"))
  }

  cn <- colnames(x)
  bad <- rowSums(is.finite(x)) < nsamples

  if (any(bad)) {
      warning("Rows containing infinite values have been removed")
      x <- x[!bad, , drop=FALSE]
  }

  nprobes <- nrow(x)
  top <- min(top, nprobes)

  #
  ##

  gene.selection <- match.arg(gene.selection, c("pairwise", "common"))

  # Distance matrix from pairwise leading fold changes
  dd <- matrix(0, nrow=nsamples, ncol=nsamples, dimnames=list(cn, cn))
  if (gene.selection == "pairwise") {
  # Distance measure is mean of top squared deviations for each pair of arrays
      topindex <- nprobes - top + 1L
      for (i in 2L:(nsamples)) {
          for (j in 1L:(i - 1L)) {
              dists <- (getCols(x, i) - getCols(x, j))^2
              dists <- sort.int(dists, partial = topindex )
              topdist <- dists[topindex:nprobes]
              dd[i, j] <- sqrt(mean(topdist))
          }
      }
  } else 
  {
  # Same genes used for all comparisons
      if (nprobes > top) {
          o <- order(rowMeans( (x-rowMeans(x))^2 ), decreasing=TRUE)
          x <- getRows(x, o[1L:top])
      }
      for (i in 2L:(nsamples)) {
          dists <- (x[, i] - x[, 1:(i-1), drop=FALSE]) ^ 2
          dd[i, 1L:(i-1L)] <- sqrt(colMeans(dists))
      }
  }

  # Multi-dimensional scaling
  a1 <- suppressWarnings(cmdscale(as.dist(dd), k=min(ndim, 8), eig=TRUE))

  # Method for MDS objects
  points <- a1$points

  if (!is.data.frame(groups) && class(groups) != "DataFrame") groups <- data.frame(groups)



  # add labels to groups
  groups <- data.frame(labels, groups)
  # ensure there are no periods in groups colnames (Vega doesn't handle these well)
  colnames(groups) <- gsub("\\.", "_", colnames(groups))

  points <- data.frame(points)
  dimlist <- paste0("dim", seq_len(ncol(points)))
  names(points) <- paste0("dim", seq_len(ncol(points)))
  points <- data.frame(points, groups)

  eigen <- data.frame(
      name = 1:min(ndim, 8),
      eigen = round(a1$eig[1:min(ndim, 8)]/sum(a1$eig), 2)
  )

  # add this column for no dimensionality in Vega
  points <- cbind(points, "-" = "0")
  points <- cbind(points, "- " = 0)
  is_numeric <- sapply(groups, is.numeric)
  numeric <- c(colnames(groups[, is_numeric]), "- ")
  discrete <- c(colnames(groups[, !is_numeric]), "-")
  features <- list(numeric=numeric, discrete=discrete, all=c(numeric,discrete))

  # forward data to the widget using xData
  xData = list(plotType = "MDS",
               data = list(mdsData=points,
                           eigenData=eigen,
                           features=features,
                           continuous_colour=continuous.colour,
                           dimlist=dimlist))

  return(xData)

}

prepareMDSData.DGEList <- function(
  x,
  top = 500,
  labels = NULL,
  groups = as.character(rep(1, ncol(x))),
  gene.selection = c("pairwise", "common"),
  prior.count = 2,
  continuous.colour = FALSE)
{

  # extract sample groups based on DGEList class, if we need to
  if (is.null(labels))
  {
    if (!is.null(x$samples$groups))
    {
      labels <- rownames(x$samples)
    } else
    {
      labels <- as.character(seq_len(ncol(x)))
    }
  }

  transformed_counts <- edgeR::cpm(x, log=TRUE, prior.count = prior.count)

  # call main processing function
  prepareMDSData.default(
    transformed_counts,
    top=top,
    labels=labels,
    groups=groups,
    gene.selection=gene.selection,
    continuous.colour=continuous.colour)

}

prepareMDSData.DESeqDataSet <- function(
  x,
  top = 500,
  labels = NULL,
  groups = NULL,
  gene.selection = c("pairwise", "common"),
  prior.count = 0.25,
  continuous.colour = FALSE)
{

  # extract sample groups based on DESeqDataSet class, if we need to
  if (is.null(labels)) 
  {
    if (!is.null(SummarizedExperiment::colData(x))) 
    {
      labels <- rownames(SummarizedExperiment::colData(x))
    } else 
    {
      labels <- as.character(seq_len(ncol(x)))
    }
  }

  transformed_counts <- edgeR::cpm(
      DESeq2::counts(x),
      log = TRUE,
      prior.count = prior.count
  )

  if (is.null(groups)) 
  {
    if (!is.null(SummarizedExperiment::colData(x))) 
    {
        groups <- S4Vectors::as.data.frame.DataTable(SummarizedExperiment::colData(x))
    } else 
    {
        groups <- as.character(rep(1, ncol(x)))
    }
  }

  prepareMDSData.default(
    transformed_counts,
    top=top,
    labels=labels,
    groups=groups,
    gene.selection=gene.selection,
    continuous.colour=continuous.colour)

}


prepareXYData <- function(x, ...)
{
  UseMethod("prepareXYData")
}

# anno - extra columns to add to the gene table
# p.adj.method - method to adjust p-value in table
# coef - column in MArrayLM object to use
prepareXYData.default <- function(
  x,
  plotType,
  coef=ncol(x$coefficients),
  p.adj.method = "BH",
  display.columns = NULL,
  anno=NULL,
  xvals=NULL,
  yvals=NULL,
  xlab=NULL,
  ylab=NULL,
  status=NULL)
{
  
  if (plotType=="MA")
  {
    # for now assume fit object.
    # create initial table with logCPM and logFC features
    xvals <- unname(x$Amean)
    xlab <- "logCPM"
    yvals <- unname(x$coefficients[, coef])
    ylab <- "logFC"
    stopifnot(all(names(x$Amean) == names(x$coefficients[, coef])))
    table <- data.frame(xvals, yvals)
    names(table) <- c(xlab, ylab)

    # add pvalue/adjusted pvalue info to table from fit object
    AdjPValue <- stats::p.adjust(x$p.value[, coef], method=p.adj.method)
    table <- cbind(table, PValue=x$p.value[, coef], AdjPValue=AdjPValue)

    # add gene info from MArrayLM object
    table <- cbind(x$genes, table)

  } else 
  {
    table <- data.frame(xvals, yvals) 
    names(table) <- c(xlab, ylab)
  }
  
  # add colour info
  if (is.null(status)) status <- rep(0, nrow(table))
  table <- cbind(table, status=as.vector(status))

  # add anno columns
  if (!is.null(anno)) table <- cbind(table, anno)

  # add index for linking table and plot (independent of object type)
  table <- data.frame(index=1:nrow(table), table)

  # set display.columns (columns to show in tooltips and in the table)
  if (is.null(display.columns)) 
  {
    display.columns <- colnames(table)
  } else
  {
    # if it's specified, make sure at least x, y and index are shown
    if (!(xlab %in% display.columns)) display.columns <- c(display.columns, xlab)
    if (!(ylab %in% display.columns)) display.columns <- c(display.columns, ylab)
    if (!("index" %in% display.columns)) display.columns <- c("index", display.columns)
  }

  data <- list(x=xlab, y=ylab, table=table, cols=display.columns, tooltipFields=display.columns)
  return(list(plotType="XY", data=data))
}

#' Shiny bindings for GlimmaV2
#'
#' Output and render functions for using GlimmaV2 within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a GlimmaV2
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name GlimmaV2-shiny
#'
#' @export
GlimmaV2Output <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'GlimmaV2', width, height, package = 'GlimmaV2')
}

#' @rdname GlimmaV2-shiny
#' @export
renderGlimmaV2 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, GlimmaV2Output, env, quoted = TRUE)
}
