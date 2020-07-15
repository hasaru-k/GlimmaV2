#' Glimma MA Plot
#'
#' @export
glimmaMA <- function(x, ...)
{
  UseMethod("glimmaMA")
}

#' Glimma MA Plot
#'
#' @importFrom limma decideTests
#' @export
glimmaMA.MArrayLM <- function(
  x,
  status=limma::decideTests(x),
  coef=ncol(x$coefficients),
  main=colnames(x)[coef],
  p.adj.method = "BH",
  display.columns = NULL,
  anno=NULL,
  groups=NULL,
  counts=NULL,
  xlab=NULL,
  ylab=NULL,
  status.colours=NULL,
  width = 920, 
  height = 920)
{
  # create initial table with logCPM and logFC features
  xvals <- round(unname(x$Amean), digits=4)
  yvals <- round(unname(x$coefficients[, coef]), digits=4)
  stopifnot(all(names(x$Amean) == names(x$coefficients[, coef])))
  if (is.null(xlab)) xlab <- "logCPM"
  if (is.null(ylab)) ylab <- "logFC"
  table <- data.frame(xvals, yvals)
  names(table) <- c(xlab, ylab)

  # add pvalue/adjusted pvalue info from fit object to table
  AdjPValue <- round(stats::p.adjust(x$p.value[, coef], method=p.adj.method), digits=4)
  table <- cbind(table, PValue=round(x$p.value[, coef], digits=4), AdjPValue=AdjPValue)

  # add gene info from MArrayLM object to table
  table <- cbind(x$genes, table)
    
  # make status single-dimensional
  if (is.matrix(status)) status <- status[, coef]
  if (length(status)!=nrow(table)) stop("Status vector must have the same number of genes as x arg.")
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups)
  return(glimmaXYWidget(xData, width, height))
}

#' Glimma MA Plot
#'
#' @importFrom edgeR decideTestsDGE
#' @export
glimmaMA.DGEExact <- function(
  x,
  status=edgeR::decideTestsDGE(x),
  main=paste(x$comparison[2],"vs",x$comparison[1]),
  p.adj.method = "BH",
  display.columns = NULL,
  anno=NULL,
  groups=NULL,
  counts=NULL,
  xlab=NULL,
  ylab=NULL,
  status.colours=NULL,
  width = 920, 
  height = 920)
{

  # create initial table with logCPM and logFC features
  if (is.null(xlab)) xlab <- "logCPM"
  if (is.null(ylab)) ylab <- "logFC"
  table <- data.frame(round(x$table$logCPM, digits=4), 
                      round(x$table$logFC, digits=4))
  names(table) <- c(xlab, ylab)

  # add pvalue/adjusted pvalue info to table
  AdjPValue <- round(stats::p.adjust(x$table$PValue, method=p.adj.method), digits=4)
  table <- cbind(table, PValue=round(x$table$PValue, digits=4), AdjPValue=AdjPValue)

  # add gene info from DGEExact object to table, if non-null
  if (!is.null(x$genes)) table <- cbind(x$genes, table)

  # generate, error-check status
  if (length(status)!=nrow(table)) stop("Status vector must have the same number of genes as x arg.")
  
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups)
  return(glimmaXYWidget(xData, width, height))
}

#' Glimma MA Plot
#'
#' @importFrom edgeR decideTestsDGE
#' @export
glimmaMA.DGELRT <- glimmaMA.DGEExact

#' Glimma MA Plot
#' @importFrom DESeq2 results counts
#' @importFrom SummarizedExperiment colData
#' @export
glimmaMA.DESeqDataSet  <- function(
  x,
  status=NULL,
  main="MA Plot",
  display.columns = NULL,
  anno=NULL,
  groups=NULL,
  counts=DESeq2::counts(x),
  xlab=NULL,
  ylab=NULL,
  status.colours=NULL,
  width = 920, 
  height = 920)
{

  # extract logCPM, logFC from DESeqDataSet
  res <- DESeq2::results(x)
  res.df <- as.data.frame(res)
  delRows <- naRowInds(res.df, "log2FoldChange", "padj")
  res.df <- res.df[!delRows, , drop=FALSE]
  anno <- anno[!delRows, , drop=FALSE]

  # extract status if it is not given
  if (is.null(status)) status <- as.numeric(res$padj<0.05)
  status <- status[!delRows]

  # create initial table with logCPM and logFC features
  xvals <- round(log(res.df$baseMean + 0.5), digits=4)
  yvals <- round(res.df$log2FoldChange, digits=4)
  if (is.null(xlab)) xlab <- "logCPM"
  if (is.null(ylab)) ylab <- "logFC"
  table <- data.frame(xvals, yvals)
  names(table) <- c(xlab, ylab)

  # add pvalue/adjusted pvalue info from fit object to table
  table <- cbind(table, PValue=round(res.df$pvalue, digits=4), AdjPValue=round(res.df$padj, digits=4))

  # process groups if counts is non-null, and if groups isn't already given
  if (!is.null(counts) && is.null(groups))
  {
    colData <- SummarizedExperiment::colData(x)
    groups <- colData[, ncol(colData)]
  }

  if (length(status)!=nrow(table)) stop("Status vector must have the same number of genes as x arg.")
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups)
  return(glimmaXYWidget(xData, width, height))
}

# returns indices of NA rows
naRowInds <- function(res.df, ...) 
{
  res.df <- data.frame(res.df)
  filterCols <- unlist(list(...))

  delRows <- rep(FALSE, nrow(res.df))

  for (cols in filterCols) 
  {
      delRows <- delRows | is.na(res.df[, cols])
  }
  return(delRows)
}

#' Glimma XY Plot
#'
#' @export
glimmaXY <- function(x, ...)
{
  UseMethod("glimmaXY")
}

#' Glimma XY Plot
#'
#' @export
glimmaXY.default <- function(
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
  status.colours=NULL,
  width = 920, 
  height = 920)
{
  x <- round(x, digits=4)
  y <- round(y, digits=4)
  if (length(x)!=length(y)) stop("Error: x and y args must have the same length.")
  table <- data.frame(x, y) 
  names(table) <- c(xlab, ylab)
  if (length(status)!=nrow(table)) stop("Status vector must have the same number of genes as x/y args.")
  xData <- buildXYData(table, status, main, display.columns, anno, counts, xlab, ylab, status.colours, groups)
  return(glimmaXYWidget(xData, width, height))
}

# common processing for both MA and XY plots.
# expects a table with xlab and ylab columns
# and a 1D status vector.
buildXYData <- function(
  table,
  status,
  main,
  display.columns = NULL,
  anno=NULL,
  counts=NULL,
  xlab=NULL,
  ylab=NULL,
  status.colours=NULL,
  groups=NULL)
{

  # process counts and groups
  if (is.null(counts)) {
    counts <- -1
  } else {
    # df format for serialisation
    counts <- data.frame(counts)
    if (is.null(groups)) stop("If counts arg is supplied, groups arg must be non-null.")
    groups <- data.frame(group=groups)
    groups <- cbind(groups, sample=colnames(counts))
    # add gene col to counts for ease of display
    counts <- cbind(counts, gene=rownames(counts))
  }
  
  # add colour and anno info to table
  table <- cbind(table, status=as.vector(status))
  if (!is.null(anno)) table <- cbind(table, anno)

  # add index for linking table and plot (independent of object type) to table
  table <- data.frame(index=0:(nrow(table)-1), table)

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
  if (is.null(status.colours)) status.colours <- c("dodgerblue", "silver", "firebrick")
  if (length(status.colours) != 3) stop("status_colours 
          arg must have exactly 3 elements for [downreg, notDE, upreg]")
  
  xData <- list(data=list(x=xlab, 
                          y=ylab, 
                          table=table, 
                          cols=display.columns,
                          counts=counts,
                          groups=groups, 
                          status_colours=status.colours,
                          title=main))
  return(xData)
}

#' GlimmaXY HTMLWidget Wrapper
#'
#' Passes packaged data to JS interface for rendering.
#'
#' @import htmlwidgets
glimmaXYWidget <- function(
  xData,
  width = 920,
  height = 920,
  elementId = NULL,
  ...)
{

  # create widget
  htmlwidgets::createWidget(
    name = 'glimmaXY',
    xData,
    width = width,
    height = height,
    package = 'GlimmaV2',
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth=750, defaultHeight=750, browser.fill=TRUE, viewer.suppress=TRUE)
  )

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