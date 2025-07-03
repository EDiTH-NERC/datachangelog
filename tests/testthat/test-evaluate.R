test_that("changelog works", {
  # Original data
  x <- mtcars
  x$car <- row.names(x)
  # Replacement dataset
  y <- x
  # Change value
  y$disp[10] <- 150
  y <- y[2:nrow(y), 2:ncol(y)]

  # Test output
  expect_true(is.data.frame(evaluate(x = x, y = x, by = "car")))
  expect_snapshot(evaluate(x = x, y = y, by = "car", report = TRUE,
                           file = "_snaps/changelog.md",
                           metadata = list(author = "Mario")))

  # Test error
  expect_error(evaluate(x = 1, y = y, by = "car"))
  expect_error(evaluate(x = x, y = y, by = "test"))
  expect_error(evaluate(x = x, y = y, by = "car", report = "test"))
  expect_error(evaluate(x = x, y = y, by = "car", file = NULL))
  expect_error(evaluate(x = x, y = y, by = "car", metadata = 4))
  y <- rbind.data.frame(y, y)
  expect_error(evaluate(x = x, y = y, by = "car"))

  # Test warnings
  y <- x[2:nrow(x), 2:ncol(x)]
  expect_warning(evaluate(x = x, y = y, by = "car", report = TRUE))
  x <- y[2:nrow(y), 2:ncol(y),]
  expect_warning(evaluate(x = x, y = y, by = "car", report = TRUE))

  x <- mtcars
  x$car <- row.names(x)
  y <- x
  y$car[2] <- "test"
  expect_message(evaluate(x = x, y = y, by = "car"))
})
