#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
GlimmaV2 <- function(
  plotType = "MDS", 
  x, 
  groups = rep(1, ncol(x)),
  top = 500, 
  labels = seq_len(ncol(x)),
  gene.selection = c("pairwise", "common"),
  width = NULL, 
  height = NULL, 
  elementId = NULL)
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
  } else {
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

  if (!is.data.frame(groups) && class(groups) != "DataFrame") {
  # Rename for the column name in dataframe
      groups <- data.frame(groups)
  }

  all_col_names <- colnames(groups)
  first_col_name <- all_col_names[1]

  points <- data.frame(points)
  names(points) <- paste0("dim", seq_len(ncol(points)))
  points <- data.frame(points, label=labels, groups)

  eigen <- data.frame(
      name = 1:min(ndim, 8),
      eigen = round(a1$eig[1:min(ndim, 8)]/sum(a1$eig), 2)
  )

  is_factor <- sapply(groups, is.factor)
  numericFeatures <- colnames(groups[, !is_factor])
  discreteFeatures <- colnames(groups[, is_factor])

  # forward data to the widget using xData
  xData = list(
    plotType = plotType,
    data = list(mdsData=points, 
                eigenData=eigen,
                numericFeatures=numericFeatures,
                discreteFeatures=discreteFeatures)
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'GlimmaV2',
    xData,
    width = width,
    height = height,
    package = 'GlimmaV2',
    elementId = elementId
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
