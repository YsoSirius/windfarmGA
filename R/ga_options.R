ga_option_names <- function() {
  sort(grep("^windfarmGA\\.", names(options()), value = TRUE))
}

format_ga_option <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.data.frame(x)) {
    return(sprintf("data.frame [%d x %d]", nrow(x), ncol(x)))
  }
  if (length(x) > 3L) {
    return(paste0(paste(utils::head(x, 3L), collapse = ", "), ", ..."))
  }
  paste(x, collapse = ", ")
}

#' @title Get or set windfarmGA options
#' @name ga_options
#' @description Print every `windfarmGA.*` session option, or set some of
#'   them. Names may be given with or without the `windfarmGA.` prefix.
#' @export
#'
#' @param ... Named options to set, or a single named list. With no arguments,
#'   print the current values and return them invisibly.
#'
#' @return A named list of `windfarmGA.*` options, invisibly.
#'
#' @examples
#' ga_options()
#' ga_options(immigrants = 3, local_search_tries = 6)
#'
#' @seealso [options()]
ga_options <- function(...) {
  dots <- list(...)
  if (length(dots) == 1L && is.null(names(dots)) && is.list(dots[[1]])) {
    dots <- dots[[1]]
  }
  if (length(dots)) {
    nms <- names(dots)
    if (is.null(nms) || any(!nzchar(nms))) {
      stop("ga_options() needs named arguments, e.g. ga_options(immigrants = 3).")
    }
    nms <- ifelse(
      startsWith(nms, "windfarmGA."), nms, paste0("windfarmGA.", nms)
    )
    names(dots) <- nms
    do.call(options, dots)
  }
  keys <- ga_option_names()
  vals <- lapply(keys, function(k) getOption(k))
  names(vals) <- keys
  if (!length(dots) || interactive()) {
    print(data.frame(
      option = sub("^windfarmGA\\.", "", keys),
      value = vapply(vals, format_ga_option, character(1)),
      stringsAsFactors = FALSE
    ), row.names = FALSE, right = FALSE)
  }
  invisible(vals)
}
