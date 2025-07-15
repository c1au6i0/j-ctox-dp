source("renv/activate.R")



options(
  renv.config.auto.snapshot = TRUE,
  renv.config.pak.enabled = TRUE,
  renv.config.ppm.enabled = TRUE
)


if (isTRUE(Sys.getenv("TERM_PROGRAM") == "vscode") && interactive()) {
  View_decimals <- function(x, title, ...) {
    if (isTRUE(is.data.frame(x) || is.matrix(x))) {
      if (missing(title)) {
        title <- paste("Data:", deparse(substitute(x))[1])
      }
      utils::View(
        x = as.data.frame(
          lapply(x, function(col) {
            if (is.numeric(col)) {
              format(col, nsmall = 12, scientific = FALSE, drop0trailing = TRUE, trim = FALSE)
            } else {
              col
            }
          })
        ),
        title = title,
        ...
      )
    } else {
      utils::View(x, title, ...)
    }
  }
}