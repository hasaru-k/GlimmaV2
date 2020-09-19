#' Glimma MA Plot
#'

#' Generic function for drawing a two-panel interactive MA plot, a special case of the
#' glimmaXY plot. 

#' The function invokes the following methods which depend on the class of the first argument:
#' \itemize{
#'   \item \code{\link{glimmaMA.MArrayLM}} for limma analysis
#'   \item \code{\link{glimmaMA.DGEExact}} for edgeR analysis, produced from \code{\link{exactTest}}
#'   \item \code{\link{glimmaMA.DGELRT}} for edgeR analysis, produced from \code{\link{glmLRT}}
#'   \item \code{\link{glimmaMA.DESeqDataSet}} for DESeq2 analysis }
#'
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
#' Draws a two-panel interactive MA plot from an MArrayLM object. This is a special case of the
#' \code{glimmaXY} plot. 
#'
#' @param x \code{MArrayLM} object from which summary statistics are extracted from to create 
#' summary (left) plot.
#'
#' @param dge \code{DGEList} object with \code{nrow(x)} rows from which expression values are 
#' extracted from to create expression (right) plot. Gene counts are taken from \code{dge$counts} 
#' and sample groups from \code{dge$samples$group}.
#'
#' @param status vector of length \code{nrow(x)} indicating the status of each gene. 
#' By default genes in the summary plot are coloured based on its differential expression status 
#' using an adjusted p-value cutoff of 5\% by calling the \code{limma::decideTests} function, where
#' the value of -1 marks down-regulated genes, 0 marks genes with no expression difference, and 
#' 1 marks up-regulated genes.
#'
#' @param coef integer indicating the column in \code{x} from the summary plot is created.
#' @param main character string for the main title of summary plot.
#' @param p.adj.method character string specifying p-value adjustment method.
#' @param display.columns character vector containing names of columns from \code{anno} from
#' which to display in mouseover tooltips and table.
#'
#' @param anno dataframe with \code{nrow(x)} rows containing gene annotations.
#' @param groups vector of length \code{ncol(dge)} representing categorisation of samples in 
#' expression plot.
#'
#' @param counts numeric matrix with \code{nrow(x)} rows containing gene expression values. 
#' This can be used to replace raw gene counts from \code{dge$counts} with transformed counts 
#' e.g. logCPM or logRPKM values.
#' @param xlab character string for the x-axis label of summary plot.
#' @param ylab character string for the y-axis label of summary plot.
#' @param status.colours vector of length 3 containing valid CSS strings for colours associated
#' with \code{status}  in the order of -1, 0 and 1.
#'
#' @param transform.counts TRUE if counts should be log-cpm transformed, defaults to FALSE.
#' @param html character string for naming HTML file for exportation of widget. The extension 
#' should be included in the file name e.g. "file.html".
#' @param width numeric value indicating width of widget in pixels.
#' @param height numeric value indicating width of height in pixels.
#' @seealso \code{\link{glimmaMA}}, \code{\link{glimmaMA.DGEExact}}, \code{\link{glimmaMA.DGELRT}}, \code{\link{glimmaMA.DESeqDataSet}}
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
  html=NULL,
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
  return(glimmaXYWidget(xData, width, height, html))
}

#' Glimma MA Plot
#'
#' Draws a two-panel interactive MA plot from an DGEExact object. This is a special case of the
#' \code{glimmaXY} plot. 
#'
#' @inheritParams glimmaMA.MArrayLM
#' @param x DGEExact object from which summary statistics are extracted from to create summary (left) plot.
#' @param status vector of length nrow(x) indicating the status of each gene. By default genes in the summary plot are 
#' coloured based on its differential expression status using an adjusted p-value cutoff of 0.05
#' by calling the \code{edgeR::decideTestsDGE()} function, where the value of -1 marks down-regulated genes, 0 marks genes with no 
#' expression difference, and 1 marks up-regulated genes.
#'
#' @seealso \code{\link{glimmaMA}}, \code{\link{glimmaMA.MArrayLM}}, \code{\link{glimmaMA.DGELRT}}, \code{\link{glimmaMA.DESeqDataSet}}
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
  html=NULL,
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
  return(glimmaXYWidget(xData, width, height, html))
}

#' Glimma MA Plot
#'
#' Draws a two-panel interactive MA plot from an DGELRT object. This is a special case of the
#' \code{glimmaXY} plot. 
#'
#' @inheritParams glimmaMA.DGEExact
#' @param x DGELRT object from which summary statistics are extracted from to create summary (left) plot.
#' @seealso \code{\link{glimmaMA}}, \code{\link{glimmaMA.MArrayLM}}, \code{\link{glimmaMA.DGEExact}}, \code{\link{glimmaMA.DESeqDataSet}}
#' @eval MA_details()
#' @importFrom edgeR decideTestsDGE
#' @importFrom stats p.adjust
#' @export
glimmaMA.DGELRT <- glimmaMA.DGEExact

#' Glimma MA Plot
#'
#' Draws a two-panel interactive MA plot from an DESeqDataSet object. This is a special case of the
#' \code{glimmaXY} plot. 
#'
#' @inheritParams glimmaMA.MArrayLM
#' @param x DESeqDataSet object from which summary statistics are extracted from to create summary (left) plot.
#' @param status vector of length nrow(x) indicating the status of each gene.
#' @param counts numeric matrix with nrow(x) rows containing gene expression values.
#' @param groups vector/factor representing the experimental group for each sample; see \code{\link{extractGroups}} for default value.
#' @seealso \code{\link{glimmaMA}}, \code{\link{glimmaMA.MArrayLM}}, \code{\link{glimmaMA.DGEExact}}, \code{\link{glimmaMA.DGELRT}}
#' @eval MA_details()
#' @importFrom DESeq2 results counts
#' @importFrom stats complete.cases
#' @importFrom SummarizedExperiment colData
#' @export
glimmaMA.DESeqDataSet  <- function(
  x,
  status=NULL,
  main="MA Plot",
  display.columns = NULL,
  anno=NULL,
  groups=extractGroups(colData(x)),
  counts=DESeq2::counts(x),
  xlab="logCPM",
  ylab="logFC",
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
  else
  {
    if (length(status)!=length(complete_genes)) stop("Status vector
      must have the same number of genes as the main arguments.")
    status <- status[complete_genes]
  }

  # create initial table with logCPM and logFC features
  table <- data.frame(round(log(res.df$baseMean + 0.5), digits=4),
                      round(res.df$log2FoldChange, digits=4))
  colnames(table) <- c(xlab, ylab)
  table <- cbind(table, PValue=round(res.df$pvalue, digits=4), AdjPValue=round(res.df$padj, digits=4))
  table <- cbind(gene=rownames(x), table)
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups, transform.counts)
  return(glimmaXYWidget(xData, width, height, html))
}
