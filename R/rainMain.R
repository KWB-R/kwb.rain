# getDailyCumulativeRain -------------------------------------------------------
#' cumulate rain data within each day
#'
#' @param rd data frame containing rain data as returned by
#'   \code{kwb.read::readBwbRainData}
#' @param to.long if \code{TRUE} (default) the result data frame is converted to
#'   'long' format.
#'
#' @export
#' @importFrom kwb.datetime intervalKey
#' @importFrom kwb.utils selectColumns hsMatrixToListForm
getDailyCumulativeRain <- function
(
  rd,
  ### data frame with the first two columns (must be named "tBeg_BWB" and
  ### "tEnd_BWB", respectively) representing the start and end, respectively of
  ### a time interval and all other columns representing rain heights
  ### corresponding to these time intervals
  to.long = TRUE
)
{
  columns <- names(rd)

  timeColumns <- columns[1:2]

  daystrings <- kwb.datetime::intervalKey(
    kwb.utils::selectColumns(rd, timeColumns[1]), "d"
  )

  gaugeColumns <- setdiff(columns, timeColumns)

  for (loopday in unique(daystrings)) {

    cat("Calculating cumulative rain heights for:", loopday, "...\n")

    selected <- (daystrings == loopday)

    rd[selected, gaugeColumns] <- sapply(rd[selected, gaugeColumns], cumsum)
  }

  if (isTRUE(to.long)) {
    cat("Converting matrix to list form... ")
    rd <- kwb.utils::hsMatrixToListForm(
      df = rd,
      keyFields = timeColumns,
      colNamePar = "gauge",
      colNameVal = "cum_mm"
    )
    cat("ok.\n")
  }

  rd
}
