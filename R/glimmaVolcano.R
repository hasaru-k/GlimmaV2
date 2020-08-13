#' Glimma Volcano Plot
#'
#' Generic function for drawing a two-panel interactive volcano plot. 
#' The function invokes the following methods which depend on the class of the first argument:
#' \itemize{
#'   \item \code{\link{glimmaVolcano.MArrayLM}} for limma analysis
#'   \item \code{\link{glimmaVolcano.DGEExact}} for edgeR analysis, produced from \code{\link{exactTest}}
#'   \item \code{\link{glimmaVolcano.DGELRT}} for edgeR analysis, produced from \code{\link{glmLRT}}
#'   \item \code{\link{glimmaVolcano.DESeqDataSet}} for DESeq2 analysis }
#'
#' @param x the DE object to plot.
#' @param ... additional arguments affecting the plots produced. See specific methods for detailed arguments.
#' @eval volcano_details()
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
#' @seealso \code{\link{glimmaVolcano}}, \code{\link{glimmaVolcano.DGEExact}}, \code{\link{glimmaVolcano.DGELRT}}, \code{\link{glimmaVolcano.DESeqDataSet}}
#' @eval volcano_details()
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
  ylab="negLog10PValue",
  status.colours=c("dodgerblue", "silver", "firebrick"),
  transform.counts=FALSE,
  html=NULL,
  width = 920,
  height = 920)
{
  table <- data.frame(round(unname(x$coefficients[, coef]), digits=4), 
                      round( -log10(x$p.value[, coef]), digits=4))
  colnames(table) <- c(xlab, ylab)
  logCPM <- round(unname(x$Amean), digits=4)
  AdjPValue <- round(stats::p.adjust(x$p.value[, coef], method=p.adj.method), digits=4)
  table <- cbind(table, logCPM=logCPM, AdjPValue=AdjPValue)
  table <- cbind(gene=rownames(x), table)
  if (is.matrix(status)) status <- status[, coef]
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height, html))
}

#' Glimma Volcano Plot
#'
#' Draws a two-panel interactive volcano plot from an DGEExact object.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @seealso \code{\link{glimmaVolcano}}, \code{\link{glimmaVolcano.MArrayLM}}, \code{\link{glimmaVolcano.DGELRT}}, \code{\link{glimmaVolcano.DESeqDataSet}}
#' @eval MA_details()
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
  anno=x$genes,
  groups=dge$samples$group,
  counts=dge$counts,
  xlab="logFC",
  ylab="negLog10PValue",
  status.colours=c("dodgerblue", "silver", "firebrick"),
  transform.counts=FALSE,
  html=NULL,
  width = 920,
  height = 920)
{
  # create initial table with -log10(pvalue) and logFC features
  table <- data.frame(round(x$table$logFC, digits=4),
                      round(-log10(x$table$PValue), digits=4))
  colnames(table) <- c(xlab, ylab)
  AdjPValue <- round(stats::p.adjust(x$table$PValue, method=p.adj.method), digits=4)
  logCPM <- round(x$table$logCPM, digits=4)
  table <- cbind(table, logCPM=logCPM, AdjPValue=AdjPValue)
  table <- cbind(gene=rownames(x), table)
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height, html))
}

#' Glimma Volcano Plot
#'
#' Draws a two-panel interactive volcano plot from an DGELRT object.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @seealso \code{\link{glimmaVolcano}}, \code{\link{glimmaVolcano.MArrayLM}}, \code{\link{glimmaVolcano.DGEExact}}, \code{\link{glimmaVolcano.DESeqDataSet}}
#' @eval MA_details()
#' @importFrom edgeR decideTestsDGE
#' @importFrom stats p.adjust
#' @export
glimmaVolcano.DGELRT <- glimmaVolcano.DGEExact

#' Glimma Volcano Plot
#'
#' Draws a two-panel interactive volcano plot from an DESeqDataSet object.
#'
#' @inheritParams glimmaMA.MArrayLM
#' @param groups vector/factor representing the experimental group for each sample; see \code{\link{extractGroups}} for default value.
#' @seealso \code{\link{glimmaVolcano}}, \code{\link{glimmaVolcano.MArrayLM}}, \code{\link{glimmaVolcano.DGEExact}}, \code{\link{glimmaVolcano.DGELRT}}
#' @eval MA_details()
#' @importFrom DESeq2 results counts
#' @importFrom SummarizedExperiment colData
#' @export
glimmaVolcano.DESeqDataSet  <- function(
  x,
  status=NULL,
  main="Volcano Plot",
  display.columns = NULL,
  anno=NULL,
  groups=extractGroups(colData(x)),
  counts=DESeq2::counts(x),
  xlab="logFC",
  ylab="negLog10PValue",
  status.colours=c("dodgerblue", "silver", "firebrick"),
  transform.counts=FALSE,
  html=NULL,
  width = 920,
  height = 920)
{
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

  # create initial table with logFC and -log10(pvalue) features
  table <- data.frame(round(res.df$log2FoldChange, digits=4),
                      round(-log10(res.df$pvalue), digits=4))
  colnames(table) <- c(xlab, ylab)
  table <- cbind(table, logCPM=round(log(res.df$baseMean + 0.5), digits=4), 
                        AdjPValue=round(res.df$padj, digits=4))
  table <- cbind(gene=rownames(x), table)
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height, html))
}