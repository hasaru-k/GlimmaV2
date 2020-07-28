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


#' Glimma Volcano Plot
#'
#' Draws a two-panel interactive MA plot.
#'
#' @param x the DE object to plot.
#' @param ... additional arguments affecting the plots produced. See specific methods for detailed arguments.
#' @eval volcano_details()
#'
#' @export
glimmaVolcano <- function(x, ...)
{
  UseMethod("glimmaVolcano")
}

#' Glimma Volcano Plot
#'
#' Draws a two-panel interactive volcano plot from an MArrayLM object.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @seealso \code{\link{glimmaVolcano.DGEExact}}, \code{\link{glimmaVolcano.DGELRT}}, \code{\link{glimmaVolcano.DESeqDataSet}}
#' @eval volcano_details()
#'
#' @importFrom limma decideTests
#' @export
glimmaVolcano.MArrayLM <- function(
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
  xlab="logFC",
  ylab="negLogPValue",
  status.colours=NULL,
  transform.counts=FALSE,
  width = 920,
  height = 920)
{

  # create initial table with logFC and -log10(pvalue) features
  xvals <- round(unname(x$coefficients[, coef]), digits=4)
  yvals <- round( -log10(x$p.value[, coef]), digits=4)
  table <- data.frame(xvals, yvals)
  names(table) <- c(xlab, ylab)

  # add logCPM and adjusted pvalue to table
  logCPM <- round(unname(x$Amean), digits=4)
  AdjPValue <- round(stats::p.adjust(x$p.value[, coef], method=p.adj.method), digits=4)
  table <- cbind(table, logCPM=logCPM, AdjPValue=AdjPValue)

  # add rownames to LHS of table
  table <- cbind(gene=rownames(x), table)

  # make status single-dimensional
  if (is.matrix(status)) status <- status[, coef]
  if (length(status)!=nrow(table)) stop("Status vector must have the same number of genes as x arg.")
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height))
}


#' Glimma Volcano Plot
#'
#' Draws a two-panel interactive volcano plot from an DGEExact object.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @seealso \code{\link{glimmaVolcano.MArrayLM}}, \code{\link{glimmaVolcano.DGELRT}}, \code{\link{glimmaVolcano.DESeqDataSet}}
#' @eval MA_details()
#'
#' @importFrom edgeR decideTestsDGE
#' @importFrom stats p.adjust
#' @export
glimmaVolcano.DGEExact <- function(
  x,
  dge=NULL,
  status=edgeR::decideTestsDGE(x),
  main=paste(x$comparison[2],"vs",x$comparison[1]),
  p.adj.method = "BH",
  display.columns = NULL,
  anno=NULL,
  groups=dge$samples$group,
  counts=dge$counts,
  xlab="logFC",
  ylab="negLogPValue",
  status.colours=NULL,
  transform.counts=FALSE,
  width = 920,
  height = 920)
{

  # create initial table with -log10(pvalue) and logFC features
  table <- data.frame(round(x$table$logFC, digits=4),
                      round(-log10(x$table$PValue), digits=4))
  names(table) <- c(xlab, ylab)

  # add logCPM/adjusted pvalue info to table
  AdjPValue <- round(stats::p.adjust(x$table$PValue, method=p.adj.method), digits=4)
  logCPM <- round(x$table$logCPM, digits=4)
  table <- cbind(table, logCPM=logCPM, AdjPValue=AdjPValue)

  # add gene info from DGEExact object to table, if non-null
  if (!is.null(x$genes)) table <- cbind(x$genes, table)

  # add rownames to LHS of table
  table <- cbind(gene=rownames(x), table)

  # error-check status
  if (length(status)!=nrow(table)) stop("Status vector must have the same number of genes as x arg.")

  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height))
}

#' Glimma Volcano Plot
#'
#' Draws a two-panel interactive volcano plot from an DGELRT object.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @seealso \code{\link{glimmaVolcano.MArrayLM}}, \code{\link{glimmaVolcano.DGEExact}}, \code{\link{glimmaVolcano.DESeqDataSet}}
#' @eval MA_details()
#'
#' @importFrom edgeR decideTestsDGE
#' @importFrom stats p.adjust
#' @export
glimmaVolcano.DGELRT <- glimmaVolcano.DGEExact

#' Glimma Volcano Plot
#'
#' Draws a two-panel interactive volcano plot from an DESeqDataSet object.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @param groups vector/factor representing the experimental group for each sample; defaults to the first column of colData(x).
#' @seealso \code{\link{glimmaVolcano.MArrayLM}}, \code{\link{glimmaVolcano.DGEExact}}, \code{\link{glimmaVolcano.DGELRT}}
#' @eval MA_details()
#'
#' @importFrom DESeq2 results counts
#' @importFrom SummarizedExperiment colData
#' @export
glimmaVolcano.DESeqDataSet  <- function(
  x,
  status=NULL,
  main="Volcano Plot",
  display.columns = NULL,
  anno=NULL,
  groups=NULL,
  counts=DESeq2::counts(x),
  xlab="logFC",
  ylab="negLogPValue",
  status.colours=NULL,
  transform.counts=FALSE,
  width = 920,
  height = 920)
{

  # extract logCPM, logFC from DESeqDataSet
  res <- DESeq2::results(x)
  res.df <- as.data.frame(res)

  # extract status if it is not given
  if (is.null(status))
  {
    status <- ifelse(
      res$padj < 0.05,
      ifelse(res$log2FoldChange < 0, -1, 1),
      0
    )
  }

  # create initial table with logCPM and logFC features
  xvals <- round(res.df$log2FoldChange, digits=4)
  yvals <- round(-log10(res.df$pvalue), digits=4)
  table <- data.frame(xvals, yvals)
  names(table) <- c(xlab, ylab)

  # add pvalue/adjusted pvalue info from fit object to table
  table <- cbind(table, logCPM=round(log(res.df$baseMean + 0.5), digits=4), AdjPValue=round(res.df$padj, digits=4))

  # process groups if counts is non-null, and if groups isn't already given
  if (!is.null(counts) && is.null(groups))
  {
    colData <- SummarizedExperiment::colData(x)
    if ("group" %in% colnames(colData))
    {
      groups <- colData[, "group"]
    } else
    {
      groups <- 1
    }
  }

  # add rownames to LHS of table
  table <- cbind(gene=rownames(x), table)

  if (length(status)!=nrow(table)) stop("Status vector must have the same number of genes as x arg.")
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height))
}