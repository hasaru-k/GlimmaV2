#' Glimma MA Plot
#'
#' Draws a two-panel interactive MA plot.
#'
#' @seealso \code{\link{glimmaMA.MArrayLM}}, \code{\link{glimmaMA.DGEExact}}, \code{\link{glimmaMA.DGELRT}}, \code{\link{glimmaMA.DESeqDataSet}}
#' @param x the DE object to plot.
#' @param ... additional arguments affecting the plots produced. See specific methods for detailed arguments.
#' @eval MA_details()
#' @export
glimmaMA <- function(x, ...)
{
  UseMethod("glimmaMA")
}

#' Glimma MA Plot
#'
#' Draws a two-panel interactive MA plot from an MArrayLM object.
#'
#' @param x the MArrayLM object to plot.
#' @param dge DGEList object used to generate the expression plot. Gene counts are extracted from \code{dge$counts} and
#' sample groups from \code{dge$samples$group}.
#' @param status vector giving the control status of each row in the object.
#' @param coef integer or logical indexing vector indicating which column of object to plot.
#' @param main Left plot title.
#' @param p.adj.method character vector indicating multiple testing correction method.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips and table.
#' @param anno a dataframe containing gene annotations
#' @param groups vector/factor representing the experimental group for each sample.
#' @param counts the matrix of expression values, with samples in columns.
#' @param xlab x axis label.
#' @param ylab y axis label.
#' @param status.colours vector of three valid CSS strings representing colours for genes with status [-1, 0 and 1] respectively.
#' @param transform.counts TRUE if counts should be log-cpm transformed, defaults to FALSE.
#' @param width width of the weidget in pixels.
#' @param height height of the widget in pixels.
#' @seealso \code{\link{glimmaMA.DGEExact}}, \code{\link{glimmaMA.DGELRT}}, \code{\link{glimmaMA.DESeqDataSet}}
#' @eval MA_details()
#' @importFrom limma decideTests
#' @export
glimmaMA.MArrayLM <- function(
  x,
  dge = NULL,
  status=limma::decideTests(x),
  coef=ncol(x$coefficients),
  main=colnames(x)[coef],
  p.adj.method = "BH",
  display.columns = NULL,
  anno=x$genes,
  groups=dge$samples$group,
  counts=dge$counts,
  xlab="logCPM",
  ylab="logFC",
  status.colours=c("dodgerblue", "silver", "firebrick"),
  transform.counts=FALSE,
  width = 920,
  height = 920)
{
  # create initial table with logCPM and logFC features
  table <- data.frame(round(unname(x$Amean), digits=4), 
                      round(unname(x$coefficients[, coef]), digits=4))
  names(table) <- c(xlab, ylab)
  AdjPValue <- round(stats::p.adjust(x$p.value[, coef], method=p.adj.method), digits=4)
  table <- cbind(table, PValue=round(x$p.value[, coef], digits=4), AdjPValue=AdjPValue)
  table <- cbind(gene=rownames(x), table)
  if (is.matrix(status)) status <- status[, coef]
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height))
}

#' Glimma MA Plot
#'
#' Draws a two-panel interactive MA plot from an DGEExact object.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @seealso \code{\link{glimmaMA.MArrayLM}}, \code{\link{glimmaMA.DGELRT}}, \code{\link{glimmaMA.DESeqDataSet}}
#' @eval MA_details()
#' @importFrom edgeR decideTestsDGE
#' @importFrom stats p.adjust
#' @export
glimmaMA.DGEExact <- function(
  x,
  dge=NULL,
  status=edgeR::decideTestsDGE(x),
  main=paste(x$comparison[2],"vs",x$comparison[1]),
  p.adj.method = "BH",
  display.columns = NULL,
  anno=x$genes,
  groups=dge$samples$group,
  counts=dge$counts,
  xlab="logCPM",
  ylab="logFC",
  status.colours=c("dodgerblue", "silver", "firebrick"),
  transform.counts=FALSE,
  width = 920,
  height = 920)
{
  table <- data.frame(round(x$table$logCPM, digits=4),
                      round(x$table$logFC, digits=4))
  names(table) <- c(xlab, ylab)
  AdjPValue <- round(stats::p.adjust(x$table$PValue, method=p.adj.method), digits=4)
  table <- cbind(table, PValue=round(x$table$PValue, digits=4), AdjPValue=AdjPValue)
  table <- cbind(gene=rownames(x), table)
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height))
}

#' Glimma MA Plot
#'
#' Draws a two-panel interactive MA plot from an DGELRT object.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @seealso \code{\link{glimmaMA.MArrayLM}}, \code{\link{glimmaMA.DGEExact}}, \code{\link{glimmaMA.DESeqDataSet}}
#' @eval MA_details()
#' @importFrom edgeR decideTestsDGE
#' @importFrom stats p.adjust
#' @export
glimmaMA.DGELRT <- glimmaMA.DGEExact

#' Glimma MA Plot
#'
#' Draws a two-panel interactive MA plot from an DESeqDataSet object.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @param groups vector/factor representing the experimental group for each sample; defaults to the first column of colData(x).
#' @seealso \code{\link{glimmaMA.MArrayLM}}, \code{\link{glimmaMA.DGEExact}}, \code{\link{glimmaMA.DGELRT}}
#' @eval MA_details()
#' @importFrom DESeq2 results counts
#' @importFrom stats complete.cases
#' @export
glimmaMA.DESeqDataSet  <- function(
  x,
  status=NULL,
  main="MA Plot",
  display.columns = NULL,
  anno=NULL,
  groups=NULL,
  counts=DESeq2::counts(x),
  xlab="logCPM",
  ylab="logFC",
  status.colours=c("dodgerblue", "silver", "firebrick"),
  transform.counts=FALSE,
  width = 920,
  height = 920)
{
  # extract logCPM, logFC from DESeqDataSet
  res.df <- as.data.frame(DESeq2::results(x))

  # filter out genes that have missing data
  complete_genes <- complete.cases(res.df)
  res.df <- res.df[complete_genes, ]
  x <- x[complete_genes, ]

  # extract status if it is not given
  if (is.null(status))
  {
    status <- ifelse(
      res.df$padj < 0.05,
      ifelse(res.df$log2FoldChange < 0, -1, 1),
      0
    )
  }

  # create initial table with logCPM and logFC features
  xvals <- round(log(res.df$baseMean + 0.5), digits=4)
  yvals <- round(res.df$log2FoldChange, digits=4)
  table <- data.frame(xvals, yvals)
  colnames(table) <- c(xlab, ylab)

  # add pvalue/adjusted pvalue info from fit object to table
  table <- cbind(table, PValue=round(res.df$pvalue, digits=4), AdjPValue=round(res.df$padj, digits=4))

  # process groups if counts is non-null, and if groups isn't already given
  if (!is.null(counts) && is.null(groups))
  {
    groups <- extractGroups(x)
  }

  # add rownames to LHS of table
  table <- cbind(gene=rownames(x), table)

  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height))
}