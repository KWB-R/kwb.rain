# correctRainDiffs -------------------------------------------------------------

#' Correct Rain Height Differences
#'
#' Add one day to the data in the "Date" column if the time in column "To" is
#' before the time in column "From"
#'
#' @param rainDiffs rain height differences as provided by
#'   \code{kwb.read:::.toDifferences}
#' @param userCorrections user correction data as returned by
#'   \code{\link{readUserCorrection}}
#' @param gauges names of rain gauges (as they appear as column names in
#'   \code{rainDiffs}) to be considered
#' @param dbg if \code{TRUE} (default) debug messages are shown
#'
#' @export
#'
correctRainDiffs <- function(
  rainDiffs, userCorrections, gauges = names(rainDiffs)[-(1:2)], dbg = TRUE
)
{
  get <- kwb.utils::selectColumns

  # Add the date string (+ 1 day if "To" < "From") to columns "From" and "To"
  userCorrections <- prepareTimeColumns(userCorrections)

  validSeries <- lapply(gauges, function(gauge) {

    cat("Gauge:", gauge, "...\n")

    x <- get(rainDiffs, c("From", "To", gauge))

    selected <- (get(userCorrections, "Gauge") == gauge)

    columns <- c("From", "To", "Valid")

    y <- kwb.utils::selectColumns(userCorrections[selected, ], columns)

    x <- merge(x, y, all.x = TRUE)

    useValid <- ! is.na(x$Valid)

    x[useValid, gauge] <- x[useValid, "Valid"]

    kwb.utils::removeColumns(x, "Valid")
  })

  kwb.utils::catIf(dbg, "Merging...\n")
  out <- kwb.utils::mergeAll(validSeries, by = c("From", "To"))
  kwb.utils::catIf(dbg, "Merging ok.\n")

  structure(out, names = c("From", "To", gauges))
}

# prepareTimeColumns -----------------------------------------------------------
prepareTimeColumns <- function(userCorrections)
{
  get <- kwb.utils::selectColumns

  dates <- get(userCorrections, "Date")

  toIsBeforeFrom <- get(userCorrections, "To") < get(userCorrections, "From")

  userCorrections$From <- paste(dates, get(userCorrections, "From"))

  dates[toIsBeforeFrom] <- as.character(as.Date(dates[toIsBeforeFrom]) + 1)

  userCorrections$To <- paste(dates, userCorrections$To)

  kwb.utils::removeColumns(userCorrections, "Date")
}

# writeDiffFiles ---------------------------------------------------------------

#' Write Files containing the "Diff" between raw and valid rain signal
#'
#' @param gaugeSignals list of data frames with each element representing one
#'   rain gauge
#' @param dir.out full path to output directory
#' @param gauges vector of character containing the names of the gauges for
#'   which files are to be written
#' @param years vector of integer representing the years for which diff files
#'   are to be generated
#' @param suffix to be used in the names of the created files
#' @param sep column separator in created CSV files
#' @param dec decimal character in created CSV files
#' @param dbg if \code{TRUE} (default) debug messages are shown
#'
#' @export
#'
writeDiffFiles <- function
(
  gaugeSignals, dir.out, gauges = names(gaugeSignals), years = NULL,
  suffix = NULL, sep = ";", dec = ",", dbg = TRUE
)
{
  kwb.utils::catIf(dbg, "Writing diff files to", dir.out, ":\n")

  # Provide a default file name suffix
  if (is.null(suffix)) {
    suffix <- if (is.null(years)) {
      ""
    } else {
      paste0("_", do.call(paste, c(as.list(range(years)), sep = "_")))
    }
  }

  for (gauge in gauges) {

    x <- kwb.utils::selectElements(gaugeSignals, gauge)

    # If years are given, filter for the given years
    if (! is.null(years)) {
      x <- x[substr(x$Date, 1, 4) %in% as.character(years), ]
    }

    if (nrow(x) > 0) {

      # Keep the Date only at the start of each block belonging to the same day
      columns <- .name(c("date", "sum.val", "sum.raw", "todo"))

      dateInfo <- kwb.utils::selectColumns(x, columns)
      i_keep <- kwb.event::hsEventsOnChange(dateInfo[, 1])$iBeg

      x[, columns] <- NA
      x[i_keep, columns] <- dateInfo[i_keep, ]

      x <- kwb.utils::removeColumns(x, .name(c("max.raw", "nmax.raw")))

      file <- .toDiffFilename(dir.out, gauge, suffix = suffix)

      kwb.utils::catIf(dbg, "Writing", basename(file), "... ")

      utils::write.table(
        x, file, sep = sep, dec = dec, row.names = FALSE, na = ""
      )

      kwb.utils::catIf(dbg, "ok.\n")

    } else {

      kwb.utils::catIf(dbg, sprintf(
        "Gauge %s: No data available for years %s\n",
        gauge, kwb.utils::collapsed(years, ", ")
      ))
    }
  }
}

# .toDiffFilename --------------------------------------------------------------
.toDiffFilename <- function(dir.out, gauge, suffix = "")
{
  file.path(dir.out, sprintf("autodiff_%s%s.csv", gauge, suffix))
}

# .name ------------------------------------------------------------------------
.name <- function(id)
{
  mapping <- c(
    sum.val = "Sum.val", sum.raw = "Sum.raw", max.raw = "Max.raw",
    nmax.raw = "N.max.raw", check = "Check", date = "Date", from = "From",
    to = "To", gauge = "Gauge", raw. = "Raw.", val. = "Val.", todo = "Todo"
  )

  columnNames <- mapping[id]

  isNA <- is.na(columnNames)

  if (any(isNA)) {

    stop("No column name defined for id(s): ", kwb.utils::stringList(id[isNA]),
         ". Please specify within .name()", call. = FALSE)
  }

  columnNames
}

# toValidationTable ------------------------------------------------------------

#' Create Validation Table from Signal and Correction Data
#'
#' @param signals data frame as returned by
#'   \code{kwb.read:::.toLongFormat(signals, type = "signal")}
#' @param corrections data frame as returned by
#'   \code{kwb.read:::.toLongFormat(corrections, type = "correction")}
#'
#' @export
#'
toValidationTable <- function(signals, corrections)
{
  # Add daily sums (round to two digits)
  n.max <- function(x) sum(x == max(x))

  daysums <- stats::aggregate(Value ~ Date + Gauge, signals, sum, na.rm = TRUE)
  daymaxs <- stats::aggregate(Value ~ Date + Gauge, signals, max, na.rm = TRUE)
  nummaxs <- stats::aggregate(Value ~ Date + Gauge, signals, n.max)

  daysums <- kwb.utils::roundColumns(daysums, "Value", 2)
  daymaxs <- kwb.utils::roundColumns(daymaxs, "Value", 2)

  # Merge the correction data (where it is not NA) with the daily sums and
  # with the daily maximum values

  # Prepare "left" table x for merging
  x <- corrections[! is.na(corrections$Value), ]
  x <- kwb.utils::hsRenameColumns(x, list(Value = .name("sum.val")))

  # Prepare "right" table y for merging
  y.sum <- kwb.utils::hsRenameColumns(daysums, list(Value = .name("sum.raw")))
  y.max <- kwb.utils::hsRenameColumns(daymaxs, list(Value = .name("max.raw")))
  y.nmax <- kwb.utils::hsRenameColumns(nummaxs, list(Value = .name("nmax.raw")))

  # Merge x and y
  validation <- merge(x, y.sum, all.x = TRUE)
  validation <- merge(validation, y.max, all.x = TRUE)
  validation <- merge(validation, y.nmax, all.x = TRUE)

  # Which records do we want to skip because they seem to be valid?
  raws <- kwb.utils::selectColumns(validation, .name("sum.raw"))
  valids <- kwb.utils::selectColumns(validation, .name("sum.val"))

  # Raw sum and valid sum are available and (almost) equal
  skip <- ! is.na(raws) & ! is.na(valids) & kwb.utils::almostEqual(raws, valids)

  # or raw sum is not available and valid sum is also not available
  skip <- skip | (is.na(raws) & is.na(valids))

  # Keep the records that are not to be skipped
  validation[! skip, ]
}

# defaultArgumentAssignment ----------------------------------------------------

#' Output Template for Argument Assignments
#'
#' @param x vector of character
#' @param type one of \code{"R", "csv"}
#'
defaultArgumentAssignment <- function(x, type = c("R", "csv")[1])
{
  if (type == "R") {

    cat(sprintf(
      "args <- list(\n    %s\n)\n",
      kwb.utils::collapsed(
        paste(kwb.utils::hsQuoteChr(x, '"'), "= list()"), "\n  , "
      )
    ))

  } else if (type == "csv") {

    kwb.utils::catLines(c(
      "config,additional.args,type,date.format,country,sep",
      paste0(kwb.utils::hsQuoteChr(x), kwb.utils::collapsed(rep(",", 5), ""))
    ))
  }
}

# toGaugeSignals ---------------------------------------------------------------

#' Convert raw Signals and Validation Data to Valid Signals
#'
#' @param signals data frame as returned by
#'   \code{kwb.read:::.toLongFormat(signals, type = "signal")}
#' @param validation data frame as returned by \code{\link{toValidationTable}}
#' @param neighbours vector of names of neighbour gauges
#' @param digits number of digits to which the signals are rounded
#' @param \dots arguments passed to \code{\link{compactForGauges}}
#'
#' @export
#'
toGaugeSignals <- function(signals, validation, neighbours, digits = 3, ...)
{
  #digits=3
  signals <- kwb.utils::roundColumns(signals, "Value", digits)

  gaugeCodes <- kwb.utils::toNamedList(names(neighbours))

  #gaugeCode <- gaugeCodes[[2]]

  lapply(gaugeCodes, function(gaugeCode) {

    gauges <- c(gaugeCode, kwb.utils::selectElements(neighbours, gaugeCode))

    compactForGauges(signals, gauges, validation, ...)
  })
}

# getNeighbours ----------------------------------------------------------------

#' Get the Names of Neighbouring Rain Gauges
#'
#' @param signals data frame as returned by
#'   \code{kwb.read:::.toLongFormat(signals, type = "signal")}
#' @param gaugeInfo data frame as returned by
#'   \code{\link[kwb.read]{readAllBwbSignals}} in attribute \code{gaugeInfo}
#' @param n number of neighbours
#' @param gaugePos data frame containing the gauge positions as required by
#'   \code{\link[kwb.read]{getGaugeDistances}}
#'
#' @export
#'
getNeighbours <- function(signals, gaugeInfo, n = 2, gaugePos = NULL)
{
  get <- kwb.utils::selectColumns

  gaugeCodes <- sort(unique(get(signals, "Gauge")))

  if (! is.null(gaugePos) || file.exists(kwb.read::mdb_rain_meta())) {

    gaugeInfo$Name <- gsub("\\s+", "", get(gaugeInfo, "Name"))

    gauge_names <- get(gaugeInfo, "Name")

    distances <- if (is.null(gaugePos)) {
      kwb.read::getGaugeDistances(gauges = gauge_names)
    } else {
      kwb.read::getGaugeDistances(gaugePos, gauges = gauge_names)
    }

    neighbourMatrix <- kwb.read::distanceToNeighbour(distances)

    lapply(kwb.utils::toNamedList(gaugeCodes), function(gaugeCode) {
      gaugeName <- gaugeInfo$Name[gaugeInfo$Code == gaugeCode]

      found <- gaugeName %in% row.names(neighbourMatrix)

      if (any(found)) {

        gaugeName <- gaugeName[which(found)[1]]

        neighbours <- neighbourMatrix[gaugeName, seq_len(n)]

        gaugeInfo$Code[gaugeInfo$Name %in% neighbours]

      } else {

        character()
      }
    })

  } else {

    warning("Returning random neighbours since I do not have access to the ",
            "meta database...")

    lapply(kwb.utils::toNamedList(gaugeCodes), function(gaugeCode) {

      sample(setdiff(gaugeInfo$Code, gaugeCode), n)
    })
  }
}

# compactForGauges -------------------------------------------------------------

#' Compact Signal Data
#'
#' @param signals data frame as returned by
#'   \code{kwb.read:::.toLongFormat(signals, type = "signal")}
#' @param gauges character vector of rain gauge names
#' @param validation data frame
#' @param level level of compression
#' @param stars if \code{TRUE} (default) stars are used to indicate differences
#' @param dbg if \code{TRUE} (default) debug messages are shown
#'
compactForGauges <- function(
  signals, gauges, validation, level = 2, stars = TRUE, dbg = TRUE
)
{
  #level=2;stars=TRUE;dbg=TRUE
  gauge <- gauges[1]

  kwb.utils::catIf(dbg, "Generating validation table for:", gauge, "... ")

  gaugeSignals <- signals[signals$Gauge %in% gauges, ]

  keys <- c(.name("date"), .name("from"), .name("to"))

  gaugeSignals <- stats::reshape(
    gaugeSignals, direction = "wide", timevar = .name("gauge"), idvar = keys
  )

  # -> Date | From | To | Value.03.01 | Value.03.08 | Value.04.01

  attr(gaugeSignals, "reshapeWide") <- NULL

  checkForDuplicates(gaugeSignals, keys)

  # If level is greater than 1, keep only rows where the sum of signals at the
  # given gauges is greater than zero
  if (level > 1) {

    sums <- rowSums(kwb.utils::removeColumns(gaugeSignals, keys), na.rm = TRUE)
    gaugeSignals <- gaugeSignals[sums > 0, ]
  }

  names(gaugeSignals) <- gsub("^Value\\.", .name("raw."), names(gaugeSignals))

  # -> Date | From | To | Raw.03.01 | Raw.03.08 | Raw.04.01

  # Filter for validation information of the current gauge, remove column "Gauge"
  gaugeValidation <- validation[validation$Gauge == gauge, -2]

  checkForDuplicates(gaugeValidation, "Date")

  gaugeSignals <- merge(gaugeSignals, gaugeValidation, by = .name("date"))

  # -> Date | From | To | Raw.03.01 | Raw.03.08 | Raw.04.01 | Sum.val | Sum.raw

  row.order <- do.call(order, kwb.utils::selectColumns(gaugeSignals, keys))

  gaugeSignals <- kwb.utils::resetRowNames(gaugeSignals[row.order, ])

  column.todo <- .name("todo")

  columns <- c(.name("date"), .name("sum.val"), .name("sum.raw"), column.todo)

  # Add an empty column where to put the valid values
  column.val <- paste0(.name("val."), gauge)
  column.raw <- paste0(.name("raw."), gauge)

  gaugeSignals[, column.val] <- NA

  #Date|From|To|Raw.03.01|Raw.03.08|Raw.04.01|Sum.val|Sum.raw|Val.03.01

  n.rows <- nrow(gaugeSignals)

  if (n.rows > 0) {

    # Provide column vectors
    sum.raw <- gaugeSignals[, .name("sum.raw")]
    sum.val <- gaugeSignals[, .name("sum.val")]
    max.raw <- gaugeSignals[, .name("max.raw")]
    nmax.raw <- gaugeSignals[, .name("nmax.raw")]

    x <- gaugeSignals[, column.raw]
    delta <- (sum.raw - sum.val)

    # Put a zero where the daily sum is zero and the raw value is not zero
    isZeroSum <- (sum.val == 0)
    deltaIsMax <- (delta == max.raw) & (nmax.raw == 1)

    isZero <- (isZeroSum & (is.na(x) | x > 0))
    isZero <- (isZero | (deltaIsMax & ! is.na(x) & delta == x))

    gaugeSignals[which(isZero), column.val] <- 0.0

    # Put a zero where the difference between raw sum and valid sum is equal
    # to the maximum value and there is only one maximum value

    # Add a column "Todo" containing "x" or "" if the case has been handled
    gaugeSignals[, column.todo] <- "x"
    gaugeSignals[which(isZeroSum | deltaIsMax), column.todo] <- ""
  }

  # Add columns "ge0.1" (greater than or equal to 0.1), "ge1" (greater than or
  # equal to 1) and "ge10" (greater than or equal to 10) containing a star "*"
  # if the signal is greater than or equal to the corresponding value
  if (stars) {

    gaugeValues <- kwb.utils::selectColumns(gaugeSignals, column.raw)
    gaugeValues <- kwb.utils::defaultIfNA(gaugeValues, 0)

    starIfReached <- function(x, limit) ifelse(x >= limit, "*", "")
    columns.gt <- c("ge0.1", "ge1", "ge10")

    for (i in seq_along(columns.gt)) {

      gaugeSignals[[columns.gt[i]]] <- starIfReached(gaugeValues, 10^(i-2))
    }

  } else {

    columns.gt <- NULL
  }

  columns <- c(columns, keys[-1], column.val, #column.check,
               columns.gt, column.raw)

  gaugeSignals <- kwb.utils::moveColumnsToFront(gaugeSignals, columns)

  kwb.utils::catIf(dbg, "ok.\n")

  gaugeSignals
}

# checkForDuplicates -----------------------------------------------------------

#' Stop if there are Duplicates and show them
#'
#' @param x data frame
#' @param keys columns that are expected to be key columns and that should not
#'   contain duplicated value combinations
#'
checkForDuplicates <- function(x, keys)
{
  isDuplicate <- duplicated(kwb.utils::selectColumns(x, keys))

  if (any(isDuplicate)) {

    kwb.utils::printIf(TRUE, utils::head(x[isDuplicate, ]), "Duplicate Signals")

    stop("There are duplicates in ", deparse(substitute(x)),
         " (see first duplicates above)!")
  }
}

# readUserCorrection -----------------------------------------------------------

#' Read User Correction Data from CSV File
#'
#' @param file full path to CSV file
#' @param sep column separator
#' @param country country code ("de" or "en")
#' @param date.format date format string
#' @param dbg if \code{TRUE} (default) debug messages are shown
#'
#' @export
#'
readUserCorrection <- function(
  file, sep = ";", country = "de", date.format = "%d.%m.%Y", dbg = TRUE
)
{
  #sep=";";dbg=TRUE;date.format = "%d.%m.%Y"; country = "en"; file = files
  if (length(file) > 1) {

    result <- kwb.utils::rbindAll(
      lapply(file, readUserCorrection, sep, country, date.format, dbg)
    )

    columns.key <- c("Gauge", "Date", "From", "To")

    return(kwb.read:::.removeDuplicates(x = result, columns.key = columns.key))
  }

  #sep=";";dec=","

  kwb.utils::catIf(dbg, "Reading", basename(file), "... ")

  x <- utils::read.table(file, TRUE, sep, stringsAsFactors = FALSE)

  kwb.utils::catIf(dbg, "ok.\n")

  dates <- kwb.utils::selectColumns(x, "Date")
  dates <- kwb.utils::.defaultIf(function(x) x == "", dates, NA)
  x$Date <- kwb.utils::naToLastNonNa(dates)

  x$Date <- kwb.datetime::reformatTimestamp(x$Date, date.format, "%Y-%m-%d")

  pattern <- "^Val."

  column.val <- grep(pattern, names(x), value = TRUE)

  gauge <- gsub(pattern, "", column.val)

  valids <- kwb.utils::selectColumns(x, column.val)

  selected <- ! is.na(valids)

  x <- x[selected, ]

  keys <- c("Date", "From", "To")

  x <- cbind(
    kwb.utils::selectColumns(x, keys),
    Gauge = gauge,
    Valid = kwb.utils::hsChrToNum(valids[selected], country),
    stringsAsFactors = FALSE
  )

  kwb.utils::resetRowNames(x)
}
