#' Evaluate and log changes in data records
#'
#' A function to evaluate and log changes in data records between two
#' dataframes and optionally create a markdown changelog.
#'
#' @param x \code{data.frame}. A \code{data.frame} to compare against
#'   \code{y}. Treated as the original version of the data records.
#' @param y \code{data.frame}. A \code{data.frame} to compare against
#'   \code{x}. Treated as the replacement version of the data records.
#' @param by \code{character}. How should \code{x} and \code{y} be compared?
#'   Options include by "column" (the default) or "row".
#' @param report \code{logical}. If \code{TRUE}, a verbose report will be
#'   written to \code{file}.
#' @param file \code{character}. Path, name, and extension of the file to be
#'   saved. Defaults to current working directory and "CHANGELOG.md".
#' @param author \code{character}. The author of to report in the changelog
#'   report, if requested.
#' @param ver \code{character}. The version number to report in the
#'   changelog report, if requested.
#' @param date \code{character}. The data to report in the changelog report,
#'   if requested. By default, the current date is used.
#'
#' @return A \code{data.frame} containing data record comparison results. Only
#'   changes are reported. If \code{report} is \code{TRUE}, a changelog report
#'   will also be saved to \code{file}.
#'
#' @details This function assumes that input \code{data.frames} maintain the
#'   same structure and aims to evaluate whether existing record values have
#'   been changed (e.g. through data cleaning).
#'
#' @export
#' @examples
#' # Dataset
#' x <- mtcars
#' # Shuffled dataset
#' y <- x[sample(nrow(x)), ]
#' # Evaluate differences
#' changelog(x = x, y = y)
#' # Create markdown report
#' changelog(x = x, y = y, report = TRUE, author = "Bob", ver = "0.0.1")
changelog <- function(x, y, by = "column",
                      report = FALSE, file = "CHANGELOG.md",
                      author = NULL, date = NULL, ver = NULL) {
  # Error handling
  if (!is.data.frame(x) | !is.data.frame(y)) {
    stop("Both `x` and `y` must be of class 'data.frame'.")
  }
  if (by != "column" && by != "row") {
    stop("`by` must be either 'column' or 'row'.")
  }
  if (!is.logical(report)) {
    stop("`report` must be of class 'logical'.")
  }
  if (!is.character(file)) {
    stop("`file` must be of class 'character'.")
  }
  if (!is.character(author) && !is.null(author)) {
    stop("`author` must be of class 'character' or 'NULL.")
  }
  if (!is.character(date) && !is.null(date)) {
    stop("`date` must be of class 'character' or 'NULL'.")
  }
  if (!is.character(ver) && !is.null(ver)) {
    stop("`ver` must be of class 'character' or 'NULL'.")
  }
  # Changes in n cols/rows?
  nc <- abs(ncol(x) - ncol(y))
  nr <- abs(nrow(x) - nrow(y))

  if (nr != 0 | nc != 0) {
    warning(paste0("There is a difference in the number of columns (",
                   nc, ") or/and rows (", nr, ")."))
    warning("Comparing common column/row indices.")
    x <- x[1:min(nrow(x), nrow(y)), ]
    y <- y[1:min(nrow(x), nrow(y)), ]
    x <- x[, 1:min(ncol(x), ncol(y))]
    y <- y[, 1:min(ncol(x), ncol(y))]
  } else {
    nr <- NULL
    nc <- NULL
  }

  # Get column indices
  col_ind <- 1:ncol(x)
  row_ind <- 1:nrow(x)
  # Get reference for subsetting
  if (by == "column") {
    ref <- col_ind
  } else if (by == "row") {
    ref <- row_ind
  }
  changes <- lapply(ref, function(j) {
    all_match <- FALSE
    if (by == "column") {
      col_ind <- j
    } else if (by == "row") {
      row_ind <- j
    }
    state <- x[row_ind, col_ind] != y[row_ind, col_ind]
    # Get names/indices
    if (by == "column") {
      row <- rownames(x)[row_ind[state]]
      column <- colnames(x)[j]
      if (length(row) == 0) all_match <- TRUE
    } else if (by == "row") {
      column <- colnames(x)[col_ind[state]]
      row <- rownames(x)[j]
      if (length(column) == 0) all_match <- TRUE
    }
    # Select original/replacement values
    original <- x[row_ind, col_ind][state]
    replacement <- y[row_ind, col_ind][state]
    # Create dataframe
    if (all_match) {
      data.frame()
    } else if (by == "column") {
      data.frame(column, row, original, replacement)
    } else if (by == "row") {
      data.frame(row, column, original, replacement)
    }
  })
  # Bind data
  x <- do.call(rbind, changes)
  # Generate text report?
  if (report) {
    generate_report(x = x, cols = nc, rows = nr,
                    file = file, author = author,
                    date = date, ver = ver)
  }
  # Return data
  x
}
