# Create report
generate_report <- function(x,
                            cols = NULL,
                            rows = NULL,
                            file = "CHANGELOG.md",
                            author = NULL,
                            date = NULL,
                            ver = NULL) {
  if (is.null(date)) { date <- Sys.Date()}
  title <- c("# Changelog  \n\n")
  author <- paste0("Author: ", author,  "\n")
  date <- paste0("Date: ", date,  "\n")
  version <- paste0("Version: ", ver, "  \n\n")
  cols <- paste0("The number of columns has changed by: ", cols, "  \n")
  rows <- paste0("The number of rows has changed by: ", rows, "  \n\n")
  subheadings <- unique(x[, 1])
  content <- lapply(subheadings, function(j) {
    ind <- which(x[, 1] == j)
    paste0("## ", j, " \n\n",
           paste(paste0(x[ind, 2], ": ", x[ind, 3], " -> ", x[ind, 4]),
                 collapse = "  \n"),
           "\n")
  })
  content <- paste(unlist(content), collapse = "\n")
  content <- paste0(title, author, date, version, cols, rows, content)
  writeLines(content, file)
}
