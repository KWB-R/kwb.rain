library("kwb.utils")
library("kwb.rain")

# Reload data when sourcing this script?
reload <- TRUE

# Read and resolve a dictionary of paths from a configuration file
config_dir <- safePath(desktop(), "R_Development/RScripts/Flusshygiene/config")
paths <- readDictionary(safePath(config_dir, "pathDictionary.txt"))

paths <- if (user() == "hauke") {
  resolve(paths, desktop = desktop(), dir.rain = "<desktop>/tmp/Regen_BWB")
} else {
  resolve(paths, desktop = desktop(), initials = "fd")
}

# Load rain data and correction data from an RData file ------------------------
if (! file.exists(paths$rdata)) {

  stop("Please run 'readBwbRainFiles.R' first to create an RData file!")

} else if (reload) {

  clearConsole()

  dbg <- TRUE

  corrections <- loadObject(paths$rdata, "corrections_long", dbg)
  signals <- loadObject(paths$rdata, "signals_long", dbg)
  gaugeInfo <- loadObject(paths$rdata, "gaugeInfo", dbg)

  #file <- "/home/hauke/Desktop/tmp/RTest/rain.RData"
  #corrections2 <- loadObject(file, "corrData")
}

# Load special rain data (Vorstadtspree) ---------------------------------------
if (FALSE)
{
  file <- safePath(paths$dir.rdata, "data_vorstadtspree.RData")
  signals <- loadObject(file, "signals_long", dbg)
  gaugeInfo <- loadObject(file, "gaugeInfo", dbg)
}

# M A I N: Prepare Correction Files --------------------------------------------
if (FALSE)
{
  # Calculate daily sum of signals and merge with correction data
  validation <- kwb.rain::toValidationTable(signals, corrections)

  #file <- file.path(paths$dir.bwb, "gaugeInfo.csv")
  #gaugePos <- read.table(file, sep = ",", stringsAsFactors = FALSE, header = TRUE)
  #neighbours <- getNeighbours(signals, gaugeInfo, gaugePos = gaugePos)

  # Adapt gauge names
  gaugeInfo_tmp <- gaugeInfo
  gaugeInfo_tmp$Name <- kwb.utils::multiSubstitute(gaugeInfo_tmp$Name, list(
    "K\xf6p I" = "KoepIf"
  ))

  neighbours <- kwb.rain::getNeighbours(signals, gaugeInfo_tmp, n = 3)

  gaugeSignals <- kwb.rain::toGaugeSignals(signals, validation, neighbours)

  x <- gaugeSignals[[1]]
  head(x)

  kwb.rain::writeDiffFiles(
    gaugeSignals, paths$dir.autodiff, years = 2016:2017, sep = ";", dec = "."
  )

  # Modify diff-files and save as userdiff....csv
  hsOpenWindowsExplorer(paths$dir.out)
}

# M A I N: Apply the Corrections to the Raw Data -------------------------------
if (FALSE)
{
  # Read user corrections (either from .RData file or from CSV files)
  file_user <- file.path(safePath(paths$dir.rdata), "userCorrections.RData")

  if (file.exists(file_user)) {

    userCorrections <- loadObject(file_user, "userCorrections")

  } else {

    # Get paths to files processed by Julia Schmidt / Francesco
    (files_js <- dir(paths$dir.user, "^userdiff.*_js.*", full.names = TRUE))
    (files_fd <- dir(paths$dir.user, "^userdiff.*_fd.*", full.names = TRUE))

    # Have a look into the file format used by Julia Schmidt / Francesco
    readLines(files_js[1], 5)
    readLines(files_fd[1], 5)

    # Start a result list
    userCorrections <- list()

    # Read the files processed by Julia Schmidt
    userCorrections[["js"]] <- kwb.rain::readUserCorrection(
      file = files_js, country = "en", date.format = "%d.%m.%Y"
    )

    # Read the files processed by Francesco
    userCorrections[["fd"]] <- kwb.rain::readUserCorrection(
      file = files_fd, country = "en", date.format = "%d/%m/%Y"
    )

    # Combine the data frames keeping the information on who did the validation
    userCorrections <- rbindAll(userCorrections, nameColumn = "validator")

    # Check for duplicates
    columns <- c("Date", "From", "To", "Gauge")
    is_duplicate <- duplicated(kwb.utils::selectColumns(userCorrections, columns))
    stopifnot(sum(is_duplicate) == 0)

    # Order by gauge and time and move column "Gauge" to the front
    columns <- c("Gauge", "Date", "From")

    rank <- do.call(order, kwb.utils::selectColumns(userCorrections, columns))

    userCorrections <- kwb.utils::moveColumnsToFront(
      userCorrections[rank, ], "Gauge"
    )

    # Save the user corrections
    save(userCorrections, file = file_user)
  }

  # Read the raw data (differences per 5 minute time interval)
  file <- safePath(paths$dir.rdata, "rainDiffs.RData")

  rainDiffs <- loadObject(file, "rainDiffs")

  # Apply the corrections to the raw data but only for gauges for which
  # raw data are available
  validDiffs <- kwb.rain::correctRainDiffs(
    rainDiffs = rainDiffs,
    userCorrections = userCorrections,
    gauges = unique(userCorrections$Gauge)
  )

  str(validDiffs)

  # Check the validated data for duplicates
  stopifnot(sum(duplicated(kwb.utils::selectColumns(validDiffs, "From"))) == 0)

  # Save the validated rain data
  file <- file.path(kwb.utils::safePath(paths$dir.rdata), "validDiffs.RData")

  save(validDiffs, file = file)
}
