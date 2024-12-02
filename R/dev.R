#' Find paths to package files (internal function)
#'
#' R packages may include arbitrary files such as drivers and templates that are
#' installed alongside the code itself. This function is a simple wrapper to
#' [base::system.file()] to find paths for this package.
#'
#' @param ... character vectors, specifying subdirectory and file(s) within this
#'   package. The default, none, returns the root of the package. Wildcards are
#'   not supported.
#'
#' @return Returns a character vector of file paths that matched `...` or an
#'   empty string if none matched.
#'
#' @examples
#' # Where is the custom dictionary for this installation?
#' watercolouR:::inst("WORDLIST")
inst <- function(...) {
  system.file(..., package = "watercolouR")
}

#' Open an R file and its corresponding test file (internal function)
#'
#' An opinionated wrapper function that calls [usethis::use_r()] and
#' [usethis::use_test()].
#'
#' @param name file name, without extension or "test-" prefix
#'
#' @return Returns `TRUE` invisibly upon success.
dev <- function(name) {
  invisible(usethis::use_r(name) && usethis::use_test(name))
}

#' Run code quality checks (internal function)
#'
#' This function runs a sequence of code quality checks. You are encouraged to
#' run this function as you develop, but at a minimum you should run it before
#' pushing code changes.
#'
#' @return Returns `TRUE` invisibly upon success.
#' @seealso [dev_check_helpers]
dev_check <- function() {
  y <- c()

  message("\nTidying code...")
  y %<>% c(dev_lint())

  message("\nRebuilding docs...")
  y %<>% c(dev_document())

  message("\nChecking spelling...")
  y %<>% c(dev_spell_check())

  message("\nRebuilding package site...")
  y %<>% c(dev_build_site())

  message("\nChecking package...")
  y %<>% c(devtools::check(
    document = FALSE,
    cran = TRUE,
    remote = FALSE,
    force_suggests = TRUE,
    args = "--force-multiarch"
  )$status == 0)

  message("\nRunning code coverage check...")
  y %<>% c(dev_coverage())

  return(invisible(all(y)))
}

#' Helper functions called by `dev_check()`
#'
#' Learn more about what [dev_check()] does.
#'
#' @param file passed to [covr::file_report()], optional. See Details.
#'
#' @details
#'
#' [dev_check()] is a convenience wrapper that runs the following checks in
#' sequence:
#'
#'  * [dev_lint()] tidies source code with [styler::style_pkg()].
#'  * [dev_document()] updates package documentation and vignettes with calls
#'    to [devtools::document()] and [usethis::use_tidy_description()].
#'  * [dev_spell_check()] checks spelling of package documentation and vignettes
#'    with calls to [spelling::spell_check_package()] and
#'    [spelling::update_wordlist()].
#'  * [dev_build_site()] builds the documentation site with calls to
#'    [dev_publish_vignettes()] and [pkgdown::build_site()], then loads a
#'    preview in the RStudio Viewer pane.
#'  * [dev_coverage()] runs code coverage checks with calls to
#'    [covr::package_coverage()] and [covr::file_report()].
#'
#' ## Spell checking
#'
#' [dev_spell_check()] first checks package documentation and vignettes for
#' spelling errors. If no spelling errors are found, the custom dictionary is
#' re-sorted, deduplicated, and purged of unused words. If spelling errors are
#' found, a user confirmation is required before updating the custom dictionary;
#' otherwise an error is thrown.
#'
#' ## About *vignettes-raw*
#'
#' [pkgdown::build_site()] generates a documentation site for your package,
#' including an "Articles" section that contains rendered package vignettes.
#' This works great for simple demos, but [pkgdown::build_site()] re-renders
#' every R Markdown vignette when building the site, making it impractical for
#' long-running vignettes or when code requires specific resources. This
#' function introduces the concept of a *vignettes-raw* subdirectory as a place for
#' notebooks and R Markdown documents that are only run manually.
#'
#' To enable this feature, simply render R Markdown documents in *vignettes-raw*
#' with `keep_md: true`. When you are ready to "publish", simply check in the
#' rendered `*.md` file (plus any supporting files) into source control.
#'
#' [dev_publish_vignettes()] employs a simple hack to add these documents to the
#' package site without re-rendering. Briefly, the *vignettes-raw* directory is
#' copied into the `vignettes` directory, and some name changes are applied so
#' [pkgdown::build_site()] uses the (pre-rendered) *Markdown* file, rather than
#' the source *R Markdown* file.
#'
#' ## Coverage reports
#'
#' If a `file` is specified, a coverage report will be generated for that file.
#' If a `file` is not specified, and package coverage is less than 100%, then
#' the a report will be generated for the file with the least coverage. A
#' package coverage summary is printed to the console, and if a file report is
#' generated, it is previewed in the RStudio Viewer pane.
#'
#' @seealso [spelling::wordlist]
#'
#' @references <https://pkgdown.r-lib.org>
#'
#' @return Each function returns `TRUE` invisibly upon success.
#'
#' @name dev_check_helpers
NULL

#' @rdname dev_check_helpers
dev_lint <- function() {
  styler::style_pkg() # defaults to tidyverse_style

  return(invisible(TRUE))
}

#' @rdname dev_check_helpers
dev_document <- function() {
  o <- capture.output(suppressMessages(
    devtools::document(
      roclets = c("rd", "collate", "namespace", "vignette"),
      quiet = TRUE
    )
  ))

  o <- capture.output(suppressMessages(
    usethis::use_tidy_description()
  ))

  return(invisible(TRUE))
}

#' @rdname dev_check_helpers
dev_spell_check <- function() {
  y <- spelling::spell_check_package()

  if (nrow(y) == 0L) {
    o <- capture.output(spelling::update_wordlist(confirm = FALSE))
    rm(o)
    return(invisible(TRUE))
  } else {
    warning(
      "Spelling errors found:",
      call. = FALSE, immediate. = TRUE, noBreaks. = TRUE
    )
    print(y)

    if (readline("Type 'yes' to update wordlist: ") == "yes") {
      spelling::update_wordlist(confirm = FALSE)
      return(invisible(TRUE))
    } else {
      stop("Please resolve spelling errors.")
    }
  }
}

#' @rdname dev_check_helpers
dev_build_site <- function() {
  dev_publish_vignettes()
  on.exit(unlink("vignettes/vignettes-raw", recursive = TRUE))

  pkgdown::build_site(preview = FALSE)

  if (interactive()) {
    d <- tryCatch(
      yaml::read_yaml("_pkgdown.yml")$destination,
      error = function(e) "docs"
    )

    if (is.null(d)) d <- "docs"

    as.character(d) |>
      dir_copy(file_temp()) |>
      as.character() |>
      path("index", ext = "html") |>
      rstudioapi::viewer()
  }

  return(invisible(TRUE))
}

#' @rdname dev_check_helpers
dev_publish_vignettes <- function() {
  md_paths <- tryCatch(
    dir_ls("vignettes-raw", glob = "*.md", recurse = TRUE),
    error = function(e) c()
  )

  if (length(md_paths) > 0) {
    dir_create("vignettes")
    dir_copy("vignettes-raw", "vignettes")
    unlink(dir_ls("vignettes/vignettes-raw", glob = "*.qmd", recurse = TRUE))
    file_move(
      dir_ls("vignettes/vignettes-raw", glob = "*.md", recurse = TRUE),
      dir_ls("vignettes/vignettes-raw", glob = "*.md", recurse = TRUE) |>
        fs::path_ext_set("qmd")
    )
  }

  return(invisible(TRUE))
}

#' @rdname dev_check_helpers
dev_coverage <- function(file = NULL) {
  coverage <- covr::package_coverage()
  print(coverage)

  file_coverage <- covr::coverage_to_list(coverage)$filecoverage

  if (!is.null(file) || min(file_coverage) < 100) {
    if (is.null(file)) file <- head(names(sort(file_coverage)), 1)
    covr::file_report(coverage, file)
  }

  return(invisible(TRUE))
}
