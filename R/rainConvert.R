# reformat_rain_data -----------------------------------------------------------

#' Reformat Rain Data
#'
#' Reformat rain data as returned by \code{\link{correctRainDiffs}} to the
#' format required by \code{\link{plotRainEventOverview}}.
#'
#' @param rain data frame with begin and end timestamps of five minute time
#'   intervals in the first two (character) columns, rain heights of five minute
#'   intervals in the remaining columns. Such a data frame is returned by
#'   \code{\link{plotRainEventOverview}}
#'
reformat_rain_data <- function(rain)
{
  stopifnot(is.data.frame(rain), ncol(rain) > 2)

  if (! all(names(rain)[1:2] == c("From", "To"))) {

    stop("The first columns are expected to be named 'From' and 'To'!")
  }

  timestamps <- kwb.utils::selectColumns(rain, "From")
  
  # Add seconds as required by "iso_to_localtime"
  timestamps <- gsub("\\+", ":00+", timestamps)

  rain$LocalDateTime <- kwb.datetime::iso_to_localtime(timestamps)

  kwb.utils::moveColumnsToFront(rain[, -(1:2)], "LocalDateTime")
}

# rainToLongFormat -------------------------------------------------------------
#' rain data from 'wide' format to 'long' format
#'
#' @param rainData rain data as returned by \code{kwb.read::readBwbRainData}
#'
#' @export
#'
rainToLongFormat <- function(rainData)
{
  kwb.utils::hsMatrixToListForm(
    rainData,
    keyFields = c("tBeg_DST", "tBeg_BWB", "tEnd_BWB", "tDate_BWB", "tBeg_UTCp1"),
    colNamePar = "gauge",
    colNameVal = "raw_mm"
  )
}

# corrToLongFormat -------------------------------------------------------------
#' rain correction data from 'wide' format to 'long' format
#'
#' @param corrData correction data as returned by
#'   \code{kwb.read::readBwbRainCorrection}
#'
#' @export
#'
corrToLongFormat <- function(corrData)
{
  dateColumn <- "tDate_BWB"

  kwb.utils::hsMatrixToListForm(
    corrData,
    keyFields = dateColumn,
    parFields = setdiff(names(corrData), c(dateColumn, "sum", "abssum")),
    colNamePar = "gauge",
    colNameVal = "corr_mm"
  )
}
