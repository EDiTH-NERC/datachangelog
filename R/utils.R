# Create report
generate_report <- function(x, xy_cols = NULL, yx_cols = NULL,
                            xy_ids = NULL, yx_ids = NULL,
                            file = NULL, metadata) {

  # Title
  changelog <- c("# Changelog  \n\n")

  # Extract metadata
  if (!is.null(metadata)) {
    metadata <- paste0(names(metadata), ": ",unlist(metadata))
    metadata <- paste(unlist(metadata), collapse = "\n")
  }

  # Which records have been added/removed?
  if (!is.null(xy_ids)) {
    xy_ids <- paste0("## The following records have been removed: \n\n",
                      paste("- ", unlist(xy_ids), collapse = "\n"),
                     " \n\n")
  }
  if (!is.null(yx_ids)) {
    yx_ids <- paste0("## The following records have been added: \n\n",
                     paste("- ", unlist(yx_ids), collapse = "\n"),
                     " \n\n")
  }

  # Which fields have been added/removed?
  if (!is.null(xy_cols)) {
    xy_cols <- paste0("## The following fields have been removed: \n\n",
                      paste("- ", unlist(xy_cols), collapse = "\n"),
                      " \n\n")
  }
  if (!is.null(yx_cols)) {
    yx_cols <- paste0("## The following fields have been added: \n\n",
                      paste("- ", unlist(yx_cols), collapse = "\n"),
                      " \n\n")
  }

  # Extract record changes
  if (nrow(x) > 0) {
    subheadings <- unique(x[, 1])
    content <- lapply(subheadings, function(j) {
      ind <- which(x[, 1] == j)
      paste0("", j, " \n\n",
             paste(paste0("- ", x[ind, 2], ": ", x[ind, 3], " -> ", x[ind, 4]),
                   collapse = "  \n"),
             "\n")
    })
    content <- paste(unlist(content), collapse = "\n")
    content <- paste0("## The following record values have been changed:\n\n",
                     content)
  } else {
    content <- "No changes in inspected records observed."
  }

  # Compile content
  content <- paste0(changelog, metadata,
                    " \n\n",
                    xy_cols, yx_cols, xy_ids, yx_ids,
                    content)
  writeLines(content, file)
}
