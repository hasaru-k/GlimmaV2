#' Glimma MDS Plot
#'
#' Generic function for drawing a two-panel interactive MDS plot.
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
#' \dontrun{
#' # using DGEList object
#' dge <- readRDS(system.file("RNAseq123/dge.rds", package = "GlimmaV2"))
#' glimmaMDS(dge)
#'
#' # using DESeqDataSet
#' dds <- DESeqDataSetFromMatrix(
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
#' }
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
#' @param top the number of top most variable genes to use.
#' @param labels the labels for each sample.
#' @param groups the experimental group to which samples belong.
#' @param gene.selection 	"pairwise" if most variable genes are to be chosen for each pair of samples or
#' "common" to select the same genes for all comparisons.
#' @param continuous.colour if \code{TRUE}, colour is displayed using continuous columns in groups; otherwise,
#' colour is displayed using discrete factors.
#' @param html name of HTML file (including extension) to export widget into rather than displaying the widget; \code{NULL} by default.
#' @param width custom widget width in pixels
#' @param height custom widget height in pixels
#'
#' @eval MDS_details()
#'
#' @examples
#' \dontrun{
#' dge <- readRDS(system.file("RNAseq123/dge.rds", package = "GlimmaV2"))
#' expr <- edgeR::cpm(dge, log = TRUE)
#' glimmaMDS(expr)
#' }
#'
#' @importFrom stats cmdscale as.dist
#' @export
glimmaMDS.default <- function(
  x,
  top = 500,
  labels = as.character(seq_len(ncol(x))),
  groups = as.character(rep(1, ncol(x))),
  gene.selection = c("pairwise", "common"),
  continuous.colour=FALSE,
  save=FALSE,
  html=NULL,
  width = 900,
  height = 500)
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

#' Glimma MDS Plot
#'
#' Draws a two-panel interactive MDS plot using a DGEList x.
#' @seealso \code{\link{glimmaMDS}}, \code{\link{glimmaMDS.default}}, \code{\link{glimmaMDS.DESeqDataSet}}
#'
#' @inheritParams glimmaMDS.default
#' @param prior.count average count to be added to each observation to avoid taking log of zero.
#'
#' @eval MDS_details()
#'
#' @examples
#' \dontrun{
#' dge <- readRDS(system.file("RNAseq123/dge.rds", package = "GlimmaV2"))
#' glimmaMDS(dge)
#' }
#'
#' @importFrom edgeR cpm
#' @export
glimmaMDS.DGEList <- function(
  x,
  top = 500,
  labels = rownames(x$samples),
  groups = x$samples,
  gene.selection = c("pairwise", "common"),
  prior.count = 2,
  continuous.colour = FALSE,
  html=NULL,
  width = 900,
  height = 500)
{
  if (is.null(labels))
  {
    labels <- as.character(seq_len(ncol(x)))
  }
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
#'
#' @seealso \code{\link{glimmaMDS}}, \code{\link{glimmaMDS.default}}, \code{\link{glimmaMDS.DGEList}}
#'
#' @inheritParams glimmaMDS.default
#' @param prior.count average count to be added to each observation to avoid taking log of zero.
#'
#' @eval MDS_details()
#'
#' @examples
#' \dontrun{
#' dge <- readRDS(system.file("RNAseq123/dge.rds", package = "GlimmaV2"))
#' dds <- DESeqDataSetFromMatrix(
#'  countData = dge$counts,
#'  colData = dge$samples,
#'  rowData = dge$genes,
#'  design = ~group
#' )
#' glimmaMDS(dds)
#' }
#'
#' @importFrom SummarizedExperiment colData
#' @importFrom edgeR cpm
#' @export
glimmaMDS.DESeqDataSet <- function(
  x,
  top = 500,
  labels = rownames(SummarizedExperiment::colData(x)),
  groups = as.data.frame(SummarizedExperiment::colData(x)),
  gene.selection = c("pairwise", "common"),
  prior.count = 0.25,
  continuous.colour = FALSE,
  html=NULL,
  width = 900,
  height = 500)
{
  if (is.null(labels))
  {
    labels <- as.character(seq_len(ncol(x)))
  }
  if (is.null(groups))
  {
    groups <- as.character(rep(1, ncol(x)))
  }
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
