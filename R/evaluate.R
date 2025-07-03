#' Evaluate and log changes in data records
#'
#' A function to evaluate and log changes between two datasets and
#' optionally create a markdown changelog report.
#'
#' @param x \code{data.frame}. The original dataset to compare against
#'   \code{y}.
#' @param y \code{data.frame}. The replacement dataset to compare against
#'   \code{x}.
#' @param by \code{character}. The name of the column to treat as unique
#'   record identifiers for comparing \code{x} and \code{y}.
#' @param report \code{logical}. If \code{TRUE}, a report of the evaluation
#'   of the datasets will be written to \code{file}.
#' @param file \code{character}. Path, name, and extension of the file to be
#'   saved. Defaults to current working directory and "CHANGELOG.md".
#' @param metadata \code{list}. A named list of metadata values to include in
#'   the header of the changelog report (e.g. author, version), if requested.
#'
#' @return A \code{data.frame} containing evaluation results. Only changes in
#'   data records are reported. If \code{report} is \code{TRUE}, a changelog
#'   report will also be saved to \code{file}.
#'
#' @details This function evaluates whether records and fields have been added
#'   or removed from original (`x`) and replacement (`y`) dataset, as
#'   well as whether any record values have changed.
#'
#' @export
#' @examples
#' # Original data
#' x <- mtcars
#' x$car <- row.names(x)
#' # Replacement dataset
#' y <- x
#' # Change value
#' y$disp[10] <- 150
#' # Evaluate differences
#' evaluate(x = x, y = y, by = "car")
#' # Create markdown report
#' evaluate(x = x, y = y, by = "car", report = TRUE,
#'          metadata = list(title = "A really cool dataset",
#'                          author = "Bob",
#'                          date = Sys.Date(),
#'                          version = 1))
evaluate <- function(x, y, by,
                     report = FALSE, file = "CHANGELOG.md",
                     metadata = NULL) {
  # Error handling
  if (!is.data.frame(x) | !is.data.frame(y)) {
    stop("Both `x` and `y` must be of class data.frame.")
  }
  if (!by %in% colnames(x) | !by %in% colnames(y)) {
    stop("`by` must be a named column in `x` and `y`.")
  }
  if (!is.logical(report)) {
    stop("`report` must be of class 'logical'.")
  }
  if (!is.character(file)) {
    stop("`file` must be of class 'character'.")
  }
  if (!is.null(metadata) & !is.list(metadata)) {
    stop("`metadata` must be NULL or a named list.")
  }
  # Check for non-unique values
  x_ids <- table(x[, by])
  y_ids <- table(y[, by])
  if (any(x_ids > 1) | any(y_ids > 1)) {
    stop("Non-unique `id` values detected in `x` and/or `y`.")
  }
  # Evaluate if records have been added or removed ----
  # Get IDs
  x_ids <- x[, by]
  y_ids <- y[, by]
  # Which IDs have been added / removed?
  xy_ids <- x_ids[which(!x_ids %in% y_ids)]
  yx_ids <- y_ids[which(!y_ids %in% x_ids)]
  # Warning
  if (length(xy_ids) != 0) {
    warning(paste0("Records present in `x` but not `y`: ",
                   toString(xy_ids)))
  } else {
    xy_ids <- NULL
  }
  if (length(yx_ids) != 0) {
    warning(paste0("Records present in `y` but not `x`: ",
                   toString(yx_ids)))
  } else {
    yx_ids <- NULL
  }
  if (length(yx_ids) != 0 | length(xy_ids) != 0) {
    message("Comparing common records between `x` and `y`.")
  }
  # Homogenise for IDs
  y <- y[which(y[, by] %in% x[, by]), ]
  x <- x[which(x[, by] %in% y[, by]), ]
  # Order data
  x <- x[order(x[, by]), ]
  y <- y[order(y[, by]), ]

  # Evaluate if fields have been added or removed ----
  # Get column names
  x_cols <- colnames(x)
  y_cols <- colnames(y)
  # Which columns have been added / removed?
  xy_cols <- x_cols[which(!x_cols %in% y_cols)]
  yx_cols <- y_cols[which(!y_cols %in% x_cols)]
  # Warning
  if (length(xy_cols) != 0) {
    warning(paste0("Fields present in `x` but not `y`: ", toString(xy_cols)))
  } else {
    xy_cols <- NULL
  }
  if (length(yx_cols) != 0) {
    warning(paste0("Fields present in `y` but not `x`: ", toString(yx_cols)))
  } else {
    yx_cols <- NULL
  }
  if (length(xy_cols) != 0 | length(yx_cols) != 0) {
    message("Comparing common fields between `x` and `y`.")
  }
  # Homogenise for cols
  y <- y[, which(y_cols %in% x_cols)]
  x <- x[ ,which(x_cols %in% y_cols)]
  # Order data
  y <- y[, colnames(x)]

  # Evaluate if record values have been changed ----
  # Get reference IDs
  ref <- x[, by]
  # Evaluate changes
  changes <- lapply(ref, function(j) {
    all_match <- FALSE
    state <- x[which(x[, by] == j), ] != y[which(y[, by] == j), ]
    # Get names/indices
    column <- colnames(x)[state]
    row <- j
    if (length(column) == 0) all_match <- TRUE
    # Select original/replacement values
    original <- x[which(x[, by] == j), ][state]
    replacement <- y[which(y[, by] == j), ][state]
    # Create dataframe
    if (all_match) {
      data.frame()
    } else {
      data.frame(id = j, column, original, replacement)
    }
  })
  # Bind data
  x <- do.call(rbind, changes)
  # Warn
  if (nrow(x) != 0) {
    # Set colname as `by`
    colnames(x)[1] <- by
    warning("Record values have changed between `x` and `y`.")
  }
  # Create a report of changes ----
  # Generate text report?
  if (report) {
    generate_report(x = x,
                    xy_cols = xy_cols, yx_cols = yx_cols,
                    xy_ids = xy_ids, yx_ids = yx_ids,
                    file = file,
                    metadata = metadata)
  }
  # Return data
  return(x)
}
