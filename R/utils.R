# Create report
generate_report <- function(x,
                            by,
                            cols = NULL,
                            rows = NULL,
                            idxy = NULL,
                            idyx = NULL,
                            file = "CHANGELOG.md",
                            author = NULL,
                            date = NULL,
                            ver = NULL) {
  # Use system date if not provided
  if (is.null(date)) { date <- Sys.Date()}
  title <- c("# Changelog  \n\n")
  author <- paste0("Author: ", author,  "\n")
  date <- paste0("Date: ", date,  "\n")
  version <- paste0("Version: ", ver, "  \n\n")
  cols <- paste0("The number of columns has changed by: ", cols, "  \n")
  rows <- paste0("The number of rows has changed by: ", rows, "  \n\n")
  if (length(idxy) > 0) {
    idxy <- paste0("The following IDs have been removed: ",
                  toString(idxy), "  \n")
  } else {
    idxy <- paste0("No IDs have been removed.",  "  \n")
  }
  if (length(idyx) > 0) {
    idyx <- paste0("The following IDs have been added: ",
                   toString(idyx), "  \n\n")
  } else {
    idyx <- paste0("No IDs have been added.",  "  \n\n")
  }

  if (nrow(x) > 0) {
    subheadings <- unique(x[, 1])
    content <- lapply(subheadings, function(j) {
      ind <- which(x[, 1] == j)
      paste0("## ", j, " \n\n",
             paste(paste0(x[ind, 2], ": ", x[ind, 3], " -> ", x[ind, 4]),
                   collapse = "  \n"),
             "\n")
    })
    content <- paste(unlist(content), collapse = "\n")
  } else {
    content <- "No changes in inspected records observed."
  }
  if (by == "id") {
    content <- paste0(title, author, date, version, idxy, idyx, content)
  } else {
    content <- paste0(title, author, date, version, cols, rows, content)
  }
  writeLines(content, file)
}
