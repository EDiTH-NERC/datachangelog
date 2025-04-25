test_that("changelog works", {
  # Get dataset
  x <- mtcars

  # Shuffled dataset
  y <- x[c(1:15, 19, 18, 17, 16, 20:32), ]

  # Test output
  expect_true(is.data.frame(changelog(x = x, y = x, by = "column")))
  expect_true(is.data.frame(changelog(x = x, y = y, by = "column")))
  expect_true(is.data.frame(changelog(x = x, y = y, by = "row")))
  expect_true(is.data.frame(changelog(x = x, y = x, by = "row")))
  expect_snapshot(changelog(x = x, y = y, by = "column", report = TRUE,
                            file = "_snaps/changelog.md"))

  # Test error
  expect_error(changelog(x = 1, y = y))
  expect_error(changelog(x = x, y = y, by = "test"))
  expect_error(changelog(x = x, y = y, report = "test"))
  expect_error(changelog(x = x, y = y, file = NULL))
  expect_error(changelog(x = x, y = y, author = 1))
  expect_error(changelog(x = x, y = y, date = 1))
  expect_error(changelog(x = x, y = y, ver = 1))

  # Test warning
  y <- rbind.data.frame(y, y)
  expect_warning(changelog(x = x, y = y))
})
