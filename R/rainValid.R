# getCorrectionCases -----------------------------------------------------------
#' get the cases of required rain data correction
#'
#' get a data frame with each row representing a gauge and a day at which
#' rain data needs to be corrected
#'
#' @param corrData correction data as returned by
#'   \code{kwb.read::readBwbRainCorrection}
#' @param rainData rain data as returned by \code{kwb.read::readBwbRainData}
#' @param ... additional arguments passed to the final \code{merge}, such as
#'   \code{all.x} (keep correction data even for days for which no rain data
#'   is available), \code{all.y} (keep rain data even for days for which no
#'   correction data is available)
#'
#' @export
#' @importFrom kwb.utils callWith defaultIfNA hsMatrixToListForm hsRenameColumns
#' moveColumnsToFront selectColumns hsRenameColumns resetRowNames
getCorrectionCases <- function(corrData, rainData, ...)
{
  # Prepare "left" table x: (non-zero) correction value per gauge and day
  corrData.long <- corrToLongFormat(corrData)
  x <- corrData.long[kwb.utils::selectColumns(corrData.long, "corr_mm") != 0, ]

  # Prepare "right" table y: rain heights in mm and number of NA values per
  # gauge and day

  num.NA <- aggregateByDay(rainData, FUN = function(x) sum(is.na(x)))
  rainData <- kwb.utils::defaultIfNA(rainData, 0.0)
  height <- aggregateByDay(rainData, FUN = sum)
  highest <- aggregateByDay(rainData, FUN = max)

  args <- list(keyFields = names(height)[1], colNamePar = "gauge")

  y <- merge(
    merge(
      kwb.utils::callWith(
        kwb.utils::hsMatrixToListForm, args, df = height, colNameVal = "rain_mm"
      ),
      kwb.utils::callWith(
        kwb.utils::hsMatrixToListForm, args, df = highest, colNameVal = "highest"
      )
    ),
    kwb.utils::callWith(
      kwb.utils::hsMatrixToListForm, args, df = num.NA, colNameVal = "n.NA"
    )
  )

  cases <- merge(kwb.utils::hsRenameColumns(x, list(tDate_BWB = "day")), y, ...)

  # Order by gauge (as they appear in rainData!) and day
  gaugeNames <- names(rainData)[-(1:2)]
  gaugeFactor <- factor(cases$gauge, levels = gaugeNames, ordered = TRUE)
  cases <- cases[order(gaugeFactor, cases$day), ]

  # Reorder columns and reset row numbers
  kwb.utils::resetRowNames(kwb.utils::moveColumnsToFront(
    cases, c("gauge", "day")
  ))
}

# aggregateByDay ---------------------------------------------------------------
#' aggregate rain data by day
#'
#' @param x data frame containing at least one date and time column
#' @param FUN aggregation function
#' @param ... arguments passed to FUN
#' @param timeColumn number of the date and time column
#' @param excludeColumns vector of column numbers to be excluded before aggregation
#'
#' @export
#' @importFrom kwb.datetime hsDateStr
#' @importFrom stats aggregate
aggregateByDay <- function(x, FUN, ..., timeColumn = 1, excludeColumns = 1:2)
{
  by <- list(day = as.Date(kwb.datetime::hsDateStr(x[, timeColumn])))

  stats::aggregate(x[, - excludeColumns], by = by, FUN = FUN, ...)
}
