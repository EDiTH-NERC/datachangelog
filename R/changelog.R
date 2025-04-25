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
#' @param report \code{logical}. If \code{TRUE}, a markdown report
#'   ("CHANGELOG.md") will be generated in the current working directory
#'   (default: \code{FALSE}).
#' @param author \code{character}. The author of to report in the changelog
#'   report, if requested.
#' @param version \code{character}. The version number to report in the
#'   changelog report, if requested.
#' @param date \code{character}. The data to report in the changelog report,
#'   if requested. By default, the current date is used.
#'
#' @return A \code{data.frame} containing data record comparison results. Only
#'   changes are reported. If \code{report} is \code{TRUE}, a changelog report
#'   ("CHANGELOG.md") will also be generated in the current working directory.
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
changelog <- function(x, y, by = "column", report = FALSE,
                       author = NULL, date = Sys.Date(), ver = NULL) {
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
    # Bind data
    if (all_match) {
      data.frame()
    } else if (by == "column") {
      data.frame(column, row, original, replacement)
    } else if (by == "row") {
      data.frame(row, column, original, replacement)
    }
  })
  x <- do.call(rbind, changes)
  if (report) {
    title <- c("# Changelog  \n\n")
    author <- paste0("Author: ", author,  "\n")
    date <- paste0("Date: ", date,  "\n")
    version <- paste0("Version: ", ver, "  \n\n")
    subheadings <- unique(x[, 1])
    content <- lapply(subheadings, function(j) {
      ind <- which(x[, 1] == j)
      paste0("## ", j, " \n\n",
             paste(paste0(x[ind, 2], ": ", x[ind, 3], " -> ", x[ind, 4]),
                   collapse = "  \n"),
             "\n")
    })
    content <- paste(unlist(content), collapse = "\n")
    content <- paste0(title, author, date, version, content)
    fn <- paste0("CHANGELOG.md")
    writeLines(content, fn)
  }
  x
}
