# plotDailyRainHeightTable -----------------------------------------------------
#' Print Tables of Daily Rain Heights (as Plots!)
#'
#' @param rd data frame with time stamps in first column and rain heights per
#'   time interval for different rain gauges in all the other columns
#' @param landscape passed to \code{kwb.utils::preparePdf}
#' @param cex passed to \code{\link[kwb.misc]{hsPrintToPlot}}
#' @param ppp number of plots per page
#' @param to.pdf if \code{TRUE} the output goes into a temporary PDF file
#'
#' @export
#' @importFrom kwb.datetime hsDateStr
#' @importFrom stats aggregate
#' @importFrom kwb.utils finishAndShowPdfIf preparePdfIf
#' @importFrom graphics par
#' @importFrom kwb.misc hsPrintToPlot
plotDailyRainHeightTable <- function
(
  rd,
  landscape = FALSE,
  cex = ifelse(landscape, 0.8, 0.6),
  ppp = ifelse(landscape, 1, 2),
  to.pdf = TRUE
)
{
  ## Calculate sum per day, once with and once without removing NAs
  aggargs <- list(
    rd[, -1], by = list(day = kwb.datetime::hsDateStr(rd[, 1])), FUN = sum
  )

  rpd <- do.call(stats::aggregate, c(aggargs, list(na.rm = TRUE)))
  rna <- do.call(stats::aggregate, aggargs)

  ## round and convert values to text matrix
  rpdm <- matrix(sprintf("%.2f", mtx <- as.matrix(rpd[, -1])), nrow = nrow(mtx))

  ## Append "*" to values that include NAs that have been treated as zero
  rpdm[sel] <- paste0(rpdm[sel <- is.na(rna[, -1])], "*")
  rpdf <- cbind(rpd[[1]], as.data.frame(rpdm))
  names(rpdf) <- names(rpd)

  file.pdf <- kwb.utils::preparePdfIf(to.pdf, landscape = landscape)

  ## Set graphical parameters and reset on exit
  opar <- graphics::par(mfrow = c(ppp, 1), mar = c(1, 2, 2, 1))
  on.exit(graphics::par(opar))

  ## loop through different months of which data is in rpd
  for (month in unique(substr(rpd[[1]], 1, 7))) {

    ## Filter for data of month
    kwb.misc::hsPrintToPlot(
      data = rpdf[substr(rpdf[[1]], 1, 7) == month, ],
      sprintf("total rain depth per day in %s", month),
      addLines = c("", "* = includes NA-values treated as zero"),
      cex = cex
    )
  }

  kwb.utils::finishAndShowPdfIf(to.pdf, file.pdf)
}

# plotRainEventOverview --------------------------------------------------------
#' Plot all rain events contained in rain data
#'
#' @param rd data frame with time stamps in first column and rain heights per
#'   time interval for different rain gauges in all the other columns
#' @param timeColumn name of the column containing the timestamps.
#'   Default: "tEnd_BWB"
#' @param to.pdf if \code{TRUE} the output goes into a temporary PDF file
#'
#' @export
#'
plotRainEventOverview <- function(rd, timeColumn = "tEnd_BWB", to.pdf = TRUE)
{
  file.pdf <- kwb.utils::preparePdfIf(to.pdf, landscape = FALSE)

  kwb.misc::plot_rain_events(
    rd, strTimestamp = timeColumn, myMinN = 0, events = FALSE
  )

  kwb.utils::finishAndShowPdfIf(to.pdf, file.pdf)
}

# plotCumulativeRain -----------------------------------------------------------
#' plot cumulative rain
#'
#' @param rd.long data frame in 'long' format as returned by
#'   \code{\link{getDailyCumulativeRain}}
#' @param to.pdf If \code{TRUE} (default) the plot goes into a temporary PDF
#'   file, otherwise to the current plot device
#'
#' @export
#' @importFrom lattice xyplot
#' @importFrom kwb.datetime intervalKey
#' @importFrom kwb.utils finishAndShowPdfIf preparePdfIf
plotCumulativeRain <- function(rd.long, to.pdf = TRUE)
{
  cat("Plotting cumulative rain heights... ")

  pdf.file <- kwb.utils::preparePdfIf(to.pdf, landscape = FALSE)

  print(lattice::xyplot(
    cum_mm ~ tBeg_BWB | kwb.datetime::intervalKey(tBeg_BWB, "d"),
    data = rd.long,
    groups = get("gauge"),
    subset = TRUE, # rd.list$day %in% days[1:7],
    #parName %in% c("ZhlIe", "BlnXI", "Wil", "Wila", "NknI",
    #"NknII", "BlnV", "Hlg", "SpaII", "KoepIf",
    #"Lbg")
    as.table = TRUE,
    xlab = "",
    ylab = "cumulative rain in mm",
    type = "l",
    auto.key = list(columns = 5, points = FALSE, lines = TRUE),
    layout = c(1, 3),
    scale = "free"
  ))

  kwb.utils::finishAndShowPdfIf(to.pdf, pdf.file)

  cat("ok\n")
}
