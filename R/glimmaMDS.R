#' Glimma MDS Plot
#'
#' Generic function for drawing a two-panel interactive multidimensional scaling (MDS) plot.
#' The function invokes the following methods which depend on the class of the first argument:
#' \itemize{
#'   \item \code{\link{glimmaMDS.DGEList}} for edgeR analysis
#'   \item \code{\link{glimmaMDS.DESeqDataSet}} for DESeq2 analysis
#'   \item \code{\link{glimmaMDS.default}} for all other object types }
#'
#' @param x the matrix containing the gene expressions.
#' @param ... the additional arguments affecting the plot produced. See specific methods for detailed arguments.
#'
#' @eval MDS_details()
#'
#' @examples
#'
#' dge <- readRDS(system.file("RNAseq123/dge.rds", package = "Glimma"))
#' glimmaMDS(dge)
#'
#' # using DESeqDataSet
#' dds <- DESeq2::DESeqDataSetFromMatrix(
#'  countData = dge$counts,
#'  colData = dge$samples,
#'  rowData = dge$genes,
#'  design = ~group
#' )
#' glimmaMDS(dds)
#'
#' # using matrix object
#' expr <- edgeR::cpm(dge, log = TRUE)
#' glimmaMDS(expr)
#'
#'
#' @export
glimmaMDS <- function(x, ...)
{
  UseMethod("glimmaMDS")
}


#' Glimma MDS Plot
#'
#' Draws a two-panel interactive MDS plot.
#'
#' @seealso \code{\link{glimmaMDS}}, \code{\link{glimmaMDS.DGEList}}, \code{\link{glimmaMDS.DESeqDataSet}}
#'
#' @param x the matrix containing the gene expressions.
#'
#' @param groups vector or data frame object with associated sample information such as experimental groups.
#' The information is displayed in mouseover tooltips, and appropriate vector(s) can be used to adjust the plot using \code{scale_by}, \code{colour_by}
#' and \code{shape_by} drop-down boxes of the widget.
#'
#' @param labels character vector of sample names or labels.
#'
#' @param continuous.colour \code{TRUE} if continuous colour schemes should be used. Defaults to \code{FALSE} where distinct colour schemes are used.
#'
#' @param top integer indiating number of top genes used to calculate pairwise distances.
#'
#' @param gene.selection character string specifying how genes are selected from the plot - "pairwise" if most variable genes are to be chosen for each pair of
#' samples,  or "common" to select the same genes for all comparisons.
#'
#' @param html character string for naming HTML file or exportation of widget. The extension should be included in the file name e.g. "file.hml".
#'
#' @param width numeric value indicating width of widget in pixels.
#'
#' @param height numeric value indicating width of widget in pixels.
#' @param ... additional unused arguments.
#'
#' @eval MDS_details()
#'
#' @examples
#' dge <- readRDS(system.file("RNAseq123/dge.rds", package = "Glimma"))
#' expr <- edgeR::cpm(dge, log = TRUE)
#' glimmaMDS(expr)
#'
#' @importFrom stats cmdscale as.dist
#' @export
glimmaMDS.default <- function(
  x,
  groups = as.character(rep(1, ncol(x))),
  labels = as.character(seq_len(ncol(x))),
  continuous.colour=FALSE,
  top = 500,
  gene.selection = c("pairwise", "common"),
  html=NULL,
  width = 900,
  height = 500,
  ...)
{

  getCols <- function(x, inds) {
      x[, inds, drop=FALSE]
  }
  getRows <- function(x, inds) {
      x[inds, , drop=FALSE]
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
      for (i in 2:nsamples) {
          dists <- (x[, i] - x[, seq_len(i-1), drop=FALSE]) ^ 2
          dd[i, seq_len(i-1L)] <- sqrt(colMeans(dists))
      }
  }

  # Multi-dimensional scaling
  a1 <- suppressWarnings(cmdscale(as.dist(dd), k=min(ndim, 8), eig=TRUE))

  # Method for MDS objects
  points <- a1$points

  if (!is.data.frame(groups) && !is(groups, "DataFrame")) groups <- data.frame(groups)

  # add labels to groups
  groups <- data.frame(labels, groups)
  # ensure there are no periods in groups colnames (Vega doesn't handle these well)
  colnames(groups) <- gsub("\\.", "_", colnames(groups))

  points <- data.frame(points)
  dimlist <- paste0("dim", seq_len(ncol(points)))
  names(points) <- paste0("dim", seq_len(ncol(points)))
  points <- data.frame(points, groups)

  eigen <- data.frame(
      name = seq_len(min(ndim, 8)),
      eigen = round(a1$eig[seq_len(min(ndim, 8))]/sum(a1$eig), 2)
  )

  # add this column for no dimensionality in Vega
  points <- cbind(points, "-" = "0")
  points <- cbind(points, "- " = 0)
  is_numeric <- vapply(groups, is.numeric, logical(1))
  numeric <- c(colnames(groups)[is_numeric], "- ")
  discrete <- c(colnames(groups)[!is_numeric], "-")
  features <- list(numeric=numeric, discrete=discrete, all=c(numeric,discrete))

  # forward data to the widget using xData
  xData = list(data = list(mdsData=points,
                           eigenData=eigen,
                           features=features,
                           continuousColour=continuous.colour,
                           dimlist=dimlist))

  # create widget
  widget <- htmlwidgets::createWidget(
    name = 'glimmaMDS',
    xData,
    width = width,
    height = height,
    package = 'Glimma',
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

#' Glimma MDS Plot
#'
#' Draws a two-panel interactive MDS plot using a DGEList x.
#' Transforms counts using \code{edgeR::cpm(x, log=TRUE, prior.count = prior.count)}.
#' @seealso \code{\link{glimmaMDS}}, \code{\link{glimmaMDS.default}}, \code{\link{glimmaMDS.DESeqDataSet}}
#'
#' @inheritParams glimmaMDS.default
#' @param x \code{DGEList} object containing gene counts in \code{x$counts}.
#' @param prior.count integer indicating the average count to be added to each observation to avoid taking log of zero when
#' raw counts are transformed to log-counts-per-million values (using \code{edgeR::cpm} function).
#'
#' @eval MDS_details()
#'
#' @examples
#' dge <- readRDS(system.file("RNAseq123/dge.rds", package = "Glimma"))
#' glimmaMDS(dge)
#'
#' @importFrom edgeR cpm
#' @export
glimmaMDS.DGEList <- function(
  x,
  groups = x$samples,
  labels = rownames(x$samples),
  continuous.colour = FALSE,
  top = 500,
  gene.selection = c("pairwise", "common"),
  prior.count = 2,
  html=NULL,
  width = 900,
  height = 500,
  ...)
{

  if (is.vector(groups) && (length(groups) != ncol(x))) {
    stop("length of groups argument must equal the number of columns in the DGE object")
  }
  if (is.data.frame(groups) && nrow(groups) != ncol(x)) {
    stop("number of rows in groups argument must equal the number of columns in the DGE object")
  }

  if (is.null(labels)) labels <- as.character(seq_len(ncol(x)))
  
  transformed_counts <- edgeR::cpm(x, log=TRUE, prior.count = prior.count)
  # call main processing function
  return(glimmaMDS.default(
    transformed_counts,
    top=top,
    labels=labels,
    groups=groups,
    gene.selection=gene.selection,
    continuous.colour=continuous.colour,
    html=html,
    width=width,
    height=height))
}

#' Glimma MDS Plot
#'
#' Draws a two-panel interactive MDS plot using a DESeqDataset x.
#' Transforms counts using \code{edgeR::cpm(DESeq2::counts(x), log = TRUE, prior.count = prior.count)}.
#'
#' @seealso \code{\link{glimmaMDS}}, \code{\link{glimmaMDS.default}}, \code{\link{glimmaMDS.DGEList}}
#'
#' @inheritParams glimmaMDS.DGEList
#' @param x \code{DESeqDataSet} object containing gene counts.
#'
#' @eval MDS_details()
#'
#' @examples
#' dge <- readRDS(system.file("RNAseq123/dge.rds", package = "Glimma"))
#' dds <- DESeq2::DESeqDataSetFromMatrix(
#'  countData = dge$counts,
#'  colData = dge$samples,
#'  rowData = dge$genes,
#'  design = ~group
#' )
#' glimmaMDS(dds)
#'
#' @importFrom SummarizedExperiment colData
#' @importFrom edgeR cpm
#' @export
glimmaMDS.DESeqDataSet <- function(
  x,
  groups = as.data.frame(SummarizedExperiment::colData(x)),
  labels = rownames(SummarizedExperiment::colData(x)),
  continuous.colour = FALSE,
  top = 500,
  gene.selection = c("pairwise", "common"),
  prior.count = 2,
  html=NULL,
  width = 900,
  height = 500,
  ...)
{
  
  if (is.null(labels)) labels <- as.character(seq_len(ncol(x)))

  if (is.null(groups)) groups <- as.character(rep(1, ncol(x)))

  transformed_counts <- edgeR::cpm(
      DESeq2::counts(x),
      log = TRUE,
      prior.count = prior.count
  )
  return(glimmaMDS.default(
    transformed_counts,
    top=top,
    labels=labels,
    groups=groups,
    gene.selection=gene.selection,
    continuous.colour=continuous.colour,
    html=html,
    width=width,
    height=height))
}
