library(mockery)

silence_error <- function(e) invisible(NULL)
mock_error <- function(x) stop("Mock error")

## inst ########################################################################

test_that("inst() returns the expected path or an empty string", {
  expect_identical(
    inst("foo", "bar"),
    ""
  )

  expect_identical(
    inst("foo", "bar"),
    system.file("foo", "bar", package = "watercolouR")
  )

  expect_identical(
    inst("WORDLIST"),
    system.file("WORDLIST", package = "watercolouR")
  )
})

## dev #########################################################################

## dev() - TT
test_that("dev() returns TRUE if both `usethis` calls are successful", {
  mock_use_r <- mock(TRUE)
  stub(dev, "usethis::use_r", mock_use_r)
  mock_use_test <- mock(TRUE)
  stub(dev, "usethis::use_test", mock_use_test)
  expect_true(dev("foo"))
  expect_called(mock_use_r, 1)
  expect_called(mock_use_test, 1)
})

## dev() - F-
test_that("dev() returns FALSE if the first `usethis` call fails", {
  mock_use_r <- mock(FALSE)
  stub(dev, "usethis::use_r", mock_use_r)
  mock_use_test <- mock(NA)
  stub(dev, "usethis::use_test", mock_use_test)
  expect_false(dev("foo"))
  expect_called(mock_use_r, 1)
  expect_called(mock_use_test, 0)
})

## dev() - TF
test_that("dev() returns FALSE if the second `usethis` call fails", {
  mock_use_r <- mock(TRUE)
  stub(dev, "usethis::use_r", mock_use_r)
  mock_use_test <- mock(FALSE)
  stub(dev, "usethis::use_test", mock_use_test)
  expect_false(dev("foo"))
  expect_called(mock_use_r, 1)
  expect_called(mock_use_test, 1)
})

## dev_lint ################################################################

test_that("dev_lint() calls `style_pkg`", {
  mock_style_pkg <- mock()
  stub(dev_lint, "styler::style_pkg", mock_style_pkg)
  expect_true(dev_lint())
  expect_called(mock_style_pkg, 1)
})

## dev_document ################################################################

test_that("dev_document() calls `document` and `use_tidy_description`", {
  mock_document <- mock()
  stub(dev_document, "devtools::document", mock_document)
  mock_use_tidy_description <- mock()
  stub(dev_document, "usethis::use_tidy_description", mock_use_tidy_description)
  expect_true(dev_document())
  expect_called(mock_document, 1)
  expect_called(mock_use_tidy_description, 1)
})

## dev_spell_check #############################################################

## dev_spell_check() - no spelling errors
test_that("dev_spell_check() silently updates wordlist and returns TRUE if no spelling errors", {
  stub(dev_spell_check, "spelling::spell_check_package", structure(
    list(word = character(0), found = list()),
    row.names = integer(0),
    class = c("summary_spellcheck", "data.frame")
  ))

  mock_readline <- mock()
  stub(dev_spell_check, "readline", mock_readline)

  mock_update_wordlist <- mock(cat("No changes required..."))
  stub(dev_spell_check, "spelling::update_wordlist", mock_update_wordlist)

  expect_silent(y <- dev_spell_check())
  expect_true(y)
  expect_called(mock_readline, 0)
  expect_args(mock_update_wordlist, 1, confirm = FALSE)
})

## dev_spell_check() - spelling errors; user declines WORDLIST update
test_that("dev_spell_check() throws warning, prints misspellings, and throws error if user declines WORDLIST update", {
  stub(dev_spell_check, "spelling::spell_check_package", structure(
    list(word = c("missspelled"), found = list("foo.Rd:42")),
    row.names = c(NA, 1L),
    class = c("summary_spellcheck", "data.frame")
  ))

  mock_readline <- mock("not yes", cycle = TRUE)
  stub(dev_spell_check, "readline", mock_readline)

  mock_update_wordlist <- mock()
  stub(dev_spell_check, "spelling::update_wordlist", mock_update_wordlist)

  o <- capture.output(
    expect_warning(
      tryCatch(dev_spell_check(), error = silence_error),
      "Spelling errors found:"
    )
  )
  expect_true(any(grepl("missspelled", o)))
  expect_called(mock_readline, 1)
  expect_called(mock_update_wordlist, 0)

  o <- capture.output(expect_error(
    suppressWarnings(dev_spell_check()),
    "Please resolve spelling errors."
  ))
  expect_true(any(grepl("missspelled", o)))
  expect_called(mock_readline, 2)
  expect_called(mock_update_wordlist, 0)
})

## dev_spell_check() - spelling errors; user accepts WORDLIST update
test_that("dev_spell_check() throws warning, prints misspellings, updates wordlist, and returns TRUE if user accepts", {
  stub(dev_spell_check, "spelling::spell_check_package", structure(
    list(word = c("missspelled"), found = list("foo.Rd:42")),
    row.names = c(NA, 1L),
    class = c("summary_spellcheck", "data.frame")
  ))

  mock_readline <- mock("yes")
  stub(dev_spell_check, "readline", mock_readline)

  mock_update_wordlist <- mock()
  stub(dev_spell_check, "spelling::update_wordlist", mock_update_wordlist)

  o <- capture.output(
    expect_warning(y <- dev_spell_check(), "Spelling errors found:")
  )
  expect_true(y)
  expect_true(any(grepl("missspelled", o)))
  expect_called(mock_readline, 1)
  expect_args(mock_update_wordlist, 1, confirm = FALSE)
})

## dev_coverage ################################################################

## dev_coverage() - 100% coverage
test_that("dev_coverage() calls `package_coverage`", {
  mock_package_coverage <- mock("mock package coverage")
  stub(dev_coverage, "covr::package_coverage", mock_package_coverage)

  mock_coverage_to_list <- mock(list(filecoverage = c("a" = 100, "b" = 100)))
  stub(dev_coverage, "covr::coverage_to_list", mock_coverage_to_list)

  mock_file_report <- mock("mock file coverage")
  stub(dev_coverage, "covr::file_report", mock_file_report)

  o <- utils::capture.output(expect_true(dev_coverage()))
  expect_true(grepl("mock package coverage", o))
  expect_called(mock_package_coverage, 1)
  expect_args(mock_coverage_to_list, 1, "mock package coverage")
  expect_called(mock_file_report, 0)
})

## dev_coverage() - < 100% coverage
test_that("dev_coverage() calls `package_coverage` then `file_report` on file with least coverage", {
  mock_package_coverage <- mock("mock package coverage")
  stub(dev_coverage, "covr::package_coverage", mock_package_coverage)

  mock_coverage_to_list <- mock(list(filecoverage = c("a" = 100, "b" = 99)))
  stub(dev_coverage, "covr::coverage_to_list", mock_coverage_to_list)

  mock_file_report <- mock("mock file coverage")
  stub(dev_coverage, "covr::file_report", mock_file_report)

  o <- utils::capture.output(expect_true(dev_coverage()))
  expect_true(grepl("mock package coverage", o))
  expect_called(mock_package_coverage, 1)
  expect_args(mock_coverage_to_list, 1, "mock package coverage")
  expect_args(mock_file_report, 1, "mock package coverage", "b")
})

## dev_coverage() - user supplied file
test_that("dev_coverage() calls `package_coverage` then `file_report` on requested file if supplied", {
  mock_package_coverage <- mock("mock package coverage")
  stub(dev_coverage, "covr::package_coverage", mock_package_coverage)

  mock_coverage_to_list <- mock(list(filecoverage = c("a" = 100, "b" = 100)))
  stub(dev_coverage, "covr::coverage_to_list", mock_coverage_to_list)

  mock_file_report <- mock("mock file coverage")
  stub(dev_coverage, "covr::file_report", mock_file_report)

  o <- utils::capture.output(expect_true(dev_coverage("mock file")))
  expect_true(grepl("mock package coverage", o))
  expect_called(mock_package_coverage, 1)
  expect_args(mock_coverage_to_list, 1, "mock package coverage")
  expect_args(mock_file_report, 1, "mock package coverage", "mock file")
})

## dev_publish_vignettes #######################################################

## dev_publish_vignettes() - no dir
test_that("dev_publish_vignettes() returns TRUE if vignettes-raw directory does not exist", {
  wd <- dir_create(file_temp())
  on.exit(unlink(wd))
  wd_v <- dir_create(wd, "vignettes")
  wd_vs <- path(wd, "vignettes-raw")
  withr::with_dir(wd, expect_true(dev_publish_vignettes()))
  expect_true(length(dir_ls(wd_v, type = "file", recurse = TRUE)) == 0L)
})

## dev_publish_vignettes() - empty dir
test_that("dev_publish_vignettes() returns TRUE if vignettes-raw directory is empty", {
  wd <- dir_create(file_temp())
  on.exit(unlink(wd))
  wd_v <- dir_create(wd, "vignettes")
  wd_vs <- dir_create(wd, "vignettes-raw")
  withr::with_dir(wd, expect_true(dev_publish_vignettes()))
  expect_true(length(dir_ls(wd_v, type = "file", recurse = TRUE)) == 0L)
})

## dev_publish_vignettes() - dir with files
test_that("dev_publish_vignettes() returns TRUE and copies existing files", {
  wd <- dir_create(file_temp())
  on.exit(unlink(wd))
  wd_v <- dir_create(path(wd, "vignettes"))
  wd_vs <- dir_create(path(wd, "vignettes-raw"))
  wd_vs_a <- dir_create(path(wd_vs, "assets"))

  qmd <- uuid::UUIDgenerate()
  md <- uuid::UUIDgenerate()
  img <- uuid::UUIDgenerate()

  write(qmd, path(wd_vs, "foo.qmd"))
  write(md, path(wd_vs, "foo.md"))
  write(img, path(wd_vs_a, "img"))

  expect_true(withr::with_dir(wd, dev_publish_vignettes()))
  expect_true(length(dir_ls(wd_v, type = "file", recurse = TRUE)) == 2L)
  expect_identical(readLines(path(wd_v, "vignettes-raw", "foo.qmd")), md) # sic
})

## dev_build_site ##############################################################

## dev_build_site() - with _pkgdown.yml
test_that("dev_build_site() calls `build_site` without previewing if non-interactive", {
  stub(dev_build_site, "interactive", FALSE)

  mock_dev_publish_vignettes <- mock(TRUE)
  stub(dev_build_site, "dev_publish_vignettes", mock_dev_publish_vignettes)

  mock_unlink <- mock(TRUE)
  stub(dev_build_site, "unlink", mock_unlink)

  mock_build_site <- mock(NULL)
  stub(dev_build_site, "pkgdown::build_site", mock_build_site)

  mock_read_yaml <- mock(list(destination = "mock destination"))
  stub(dev_build_site, "yaml::read_yaml", mock_read_yaml)

  stub(dev_build_site, "file_temp", "temp_path")

  mock_dir_copy <- mock("new_path")
  stub(dev_build_site, "dir_copy", mock_dir_copy)

  mock_viewer <- mock(TRUE)
  stub(dev_build_site, "rstudioapi::viewer", mock_viewer)

  expect_true(dev_build_site())
  expect_called(mock_dev_publish_vignettes, 1)
  expect_args(mock_build_site, 1, preview = FALSE)
  expect_called(mock_read_yaml, 0)
  expect_called(mock_dir_copy, 0)
  expect_called(mock_viewer, 0)
  expect_args(mock_unlink, 1, "vignettes/vignettes-raw", recursive = TRUE)
})

## dev_build_site() - with _pkgdown.yml
test_that("dev_build_site() calls `build_site` and previews site in viewer using custom destination if specified in _pkgdown.yml", {
  stub(dev_build_site, "interactive", TRUE)

  mock_dev_publish_vignettes <- mock(TRUE)
  stub(dev_build_site, "dev_publish_vignettes", mock_dev_publish_vignettes)

  mock_unlink <- mock(TRUE)
  stub(dev_build_site, "unlink", mock_unlink)

  mock_build_site <- mock(NULL)
  stub(dev_build_site, "pkgdown::build_site", mock_build_site)

  mock_read_yaml <- mock(list(destination = "mock destination"))
  stub(dev_build_site, "yaml::read_yaml", mock_read_yaml)

  stub(dev_build_site, "file_temp", "temp_path")

  mock_dir_copy <- mock("new_path")
  stub(dev_build_site, "dir_copy", mock_dir_copy)

  mock_viewer <- mock(TRUE)
  stub(dev_build_site, "rstudioapi::viewer", mock_viewer)

  expect_true(dev_build_site())
  expect_called(mock_dev_publish_vignettes, 1)
  expect_args(mock_build_site, 1, preview = FALSE)
  expect_called(mock_read_yaml, 1)
  expect_args(mock_dir_copy, 1, "mock destination", "temp_path")
  expect_args(mock_viewer, 1, fs::path("new_path/index.html"))
  expect_args(mock_unlink, 1, "vignettes/vignettes-raw", recursive = TRUE)
})

## dev_build_site() - with NULL destination
test_that("dev_build_site() calls `build_site` and previews site in viewer using default destination if NULL destination", {
  stub(dev_build_site, "interactive", TRUE)

  mock_dev_publish_vignettes <- mock(TRUE)
  stub(dev_build_site, "dev_publish_vignettes", mock_dev_publish_vignettes)

  mock_unlink <- mock(TRUE)
  stub(dev_build_site, "unlink", mock_unlink)

  mock_build_site <- mock(NULL)
  stub(dev_build_site, "pkgdown::build_site", mock_build_site)

  mock_read_yaml <- mock(NULL)
  stub(dev_build_site, "yaml::read_yaml", mock_read_yaml)

  stub(dev_build_site, "file_temp", "temp_path")

  mock_dir_copy <- mock("new_path")
  stub(dev_build_site, "dir_copy", mock_dir_copy)

  mock_viewer <- mock(TRUE)
  stub(dev_build_site, "rstudioapi::viewer", mock_viewer)

  expect_true(dev_build_site())
  expect_called(mock_dev_publish_vignettes, 1)
  expect_args(mock_build_site, 1, preview = FALSE)
  expect_called(mock_read_yaml, 1)
  expect_args(mock_dir_copy, 1, "docs", "temp_path")
  expect_args(mock_viewer, 1, fs::path("new_path/index.html"))
  expect_args(mock_unlink, 1, "vignettes/vignettes-raw", recursive = TRUE)
})

## dev_build_site() - with read_yaml error
test_that("dev_build_site() calls `build_site` and previews site in viewer using default destination if read_yaml error", {
  stub(dev_build_site, "interactive", TRUE)

  mock_dev_publish_vignettes <- mock(TRUE)
  stub(dev_build_site, "dev_publish_vignettes", mock_dev_publish_vignettes)

  mock_unlink <- mock(TRUE)
  stub(dev_build_site, "unlink", mock_unlink)

  mock_build_site <- mock(NULL)
  stub(dev_build_site, "pkgdown::build_site", mock_build_site)

  mock_read_yaml <- mock(mock_error)
  stub(dev_build_site, "yaml::read_yaml", mock_read_yaml)

  stub(dev_build_site, "file_temp", "temp_path")

  mock_dir_copy <- mock("new_path")
  stub(dev_build_site, "dir_copy", mock_dir_copy)

  mock_viewer <- mock(TRUE)
  stub(dev_build_site, "rstudioapi::viewer", mock_viewer)

  expect_true(dev_build_site())
  expect_called(mock_dev_publish_vignettes, 1)
  expect_args(mock_build_site, 1, preview = FALSE)
  expect_called(mock_read_yaml, 1)
  expect_args(mock_dir_copy, 1, "docs", "temp_path")
  expect_args(mock_viewer, 1, fs::path("new_path/index.html"))
  expect_args(mock_unlink, 1, "vignettes/vignettes-raw", recursive = TRUE)
})

## dev_check ###################################################################

test_that("dev_check() calls lower level utilities", {
  mock_dev_document <- mock(TRUE)
  stub(dev_check, "dev_document", mock_dev_document)

  mock_dev_lint <- mock(TRUE)
  stub(dev_check, "dev_lint", mock_dev_lint)

  mock_dev_spell_check <- mock(TRUE)
  stub(dev_check, "dev_spell_check", mock_dev_spell_check)

  mock_check <- mock(list(status = 0))
  stub(dev_check, "devtools::check", mock_check)

  mock_dev_coverage <- mock(TRUE)
  stub(dev_check, "dev_coverage", mock_dev_coverage)

  mock_build_site <- mock(TRUE)
  stub(dev_check, "dev_build_site", mock_build_site)

  o <- capture.output(dev_check())

  expect_called(mock_dev_lint, 1)
  expect_called(mock_dev_document, 1)
  expect_called(mock_dev_spell_check, 1)
  expect_called(mock_check, 1)
  expect_called(mock_dev_coverage, 1)
  expect_called(mock_build_site, 1)
})
