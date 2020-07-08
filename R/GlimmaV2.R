glimmaMA <- function(
  x, 
  status=rep(0, nrow(x)),
  main="MA Plot", 
  width = 900, 
  height = 570,
  ...)
{
  xData <- prepareXYData(x, parameter.type="MA", status, main, ...)
  return(GlimmaV2(xData, width, height))
}

# required: x and y
glimmaXY <- function(
  x, 
  y, 
  xlab="x", 
  ylab="y", 
  status=rep(0, length(x)),
  main="XY Plot", 
  width = 900, 
  height = 570,
  ...)
{
  # might be a bit confusing that there are two x variables?
  # maybe rename second x to fit
  xData <- prepareXYData(x=NULL, xvals=x, yvals=y, xlab=xlab, ylab=ylab, parameter.type="XY", status, main, ...)
  return(GlimmaV2(xData, width, height))
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
  width = 900,
  height = 570,
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

#' Glimma MDS Plot
#'
#' Draws a two-panel interactive MDS plot in an html page. The left panel contains the plot between
#' two MDS dimensions, with annotations displayed on hover. The right panel contains a bar plot of
#' the eigenvalues of each dimension. The controls below can be used to change the dimensions being
#' displayed.
#'
#' @seealso \code{\link{glimmaMDS.default}}, \code{\link{glimmaMDS.DGEList}}, \code{\link{glimmaMDS.DESeqDataSet}}
#'
#' @param x the matrix containing the gene expressions.
#' @param ... the additional arguments.
#' @export
glimmaMDS <- function(x, ...)
{
  UseMethod("glimmaMDS")
}

#' Glimma MDS Plot
#'
#' Draws a two-panel interactive MDS plot. The left panel contains the plot between
#' two MDS dimensions, with annotations displayed on hover. The right panel contains a bar plot of
#' the eigenvalues of each dimension. The controls below can be used to change the dimensions being
#' displayed.
#'
#' @seealso \code{\link{glimmaMDS.DGEList}}, \code{\link{glimmaMDS.DESeqDataSet}}
#'
#' @param x the matrix containing the gene expressions.
#' @param top the number of top most variable genes to use.
#' @param labels the labels for each sample.
#' @param groups the experimental group to which samples belong.
#' @param gene.selection 	"pairwise" if most variable genes are to be chosen for each pair of samples or
#' "common" to select the same genes for all comparisons.
#' @param continuous.colour if TRUE, colour is displayed using continuous columns in groups; if false,
#' colour is displayed using discrete columns.
#' @param width custom widget width in pixels
#' @param height custom widget height in pixels
#' @export
glimmaMDS.default <- function(
  x,
  top = 500,
  labels = as.character(seq_len(ncol(x))),
  groups = as.character(rep(1, ncol(x))),
  gene.selection = c("pairwise", "common"),
  continuous.colour=FALSE,
  width = 900, 
  height = 570)
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
  return(GlimmaV2(xData, width, height))
}

#' Glimma MDS Plot
#'
#' Draws a two-panel interactive MDS plot using a DGEList x.
#' By default, extracts \code{labels} as \code{rownames(x$samples)}.
#' @seealso \code{\link{glimmaMDS.default}}, \code{\link{glimmaMDS.DESeqDataSet}}
#'
#' @inheritParams glimmaMDS.default
#' @param prior.count average count to be added to each observation to avoid taking log of zero.
#' @export
glimmaMDS.DGEList <- function(
  x,
  top = 500,
  labels = NULL,
  groups = as.character(rep(1, ncol(x))),
  gene.selection = c("pairwise", "common"),
  prior.count = 2,
  continuous.colour = FALSE,
  width = 900, 
  height = 570)
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
  return(glimmaMDS.default(
    transformed_counts,
    top=top,
    labels=labels,
    groups=groups,
    gene.selection=gene.selection,
    continuous.colour=continuous.colour))

}

#' Glimma MDS Plot
#'
#' Draws a two-panel interactive MDS plot using a DESeqDataset x. 
#' By default, extracts groups as \code{colData(x)} and extracts labels as \code{rownames(colData(x))}.
#' @seealso \code{\link{glimmaMDS.default}}, \code{\link{glimmaMDS.DGEList}}
#'
#' @inheritParams glimmaMDS.DGEList
#' @export
glimmaMDS.DESeqDataSet <- function(
  x,
  top = 500,
  labels = NULL,
  groups = NULL,
  gene.selection = c("pairwise", "common"),
  prior.count = 0.25,
  continuous.colour = FALSE,
  width = 900, 
  height = 570)
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

  return(glimmaMDS.default(
    transformed_counts,
    top=top,
    labels=labels,
    groups=groups,
    gene.selection=gene.selection,
    continuous.colour=continuous.colour))

}


prepareXYData <- function(x, ...)
{
  UseMethod("prepareXYData")
}

# display.columns - character vector containing names of columns to display in mouseover tooltips and table.
# anno - the data.frame containing gene annotations.
# p.adj.method - character vector indicating multiple testing correction method. See p.adjust for available methods. (defaults to "BH")
# coef - integer or character index vector indicating which column of object to plot.
prepareXYData.default <- function(
  x,
  parameter.type,
  status,
  main,
  coef=ncol(x$coefficients),
  p.adj.method = "BH",
  display.columns = NULL,
  anno=NULL,
  xvals=NULL,
  yvals=NULL,
  xlab=NULL,
  ylab=NULL,
  status.colours=c("dodgerblue", "lightslategray", "firebrick"))
{
  
  if (parameter.type=="MA")
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
    if (length(xvals)!=length(yvals)) stop("Error: x and y args must have the same length.")
    table <- data.frame(xvals, yvals) 
    names(table) <- c(xlab, ylab)
  }
  
  # add colour info
  if (is.matrix(status)) status <- status[, coef]
  if (length(status)!=nrow(table))
  {
    if (parameter.type=="MA") stop("Status vector must have the same number of genes as x arg.")
    else stop("Status vector must have the same number of genes as x/y args.")
  }
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

  # error checking on status_colours
  if (length(status.colours) != 3) stop("status_colours 
          arg must have exactly 3 elements for [downreg, notDE, upreg]")

  data <- list(x=xlab, 
               y=ylab, 
               table=table, 
               cols=display.columns, 
               tooltipFields=display.columns,
               status_colours=status.colours,
               title=main)
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
