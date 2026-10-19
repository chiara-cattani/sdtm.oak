#' Convert ISO 8601 values to base date-time classes
#'
#' These methods allow values returned by [create_iso8601()] to be converted
#' with the usual base R date-time functions. The original `iso8601` value
#' retains its parsing diagnostics, which can be inspected with [problems()].
#'
#' @param x A value returned by [create_iso8601()].
#' @param tz Time zone passed to [as.POSIXct()] or [as.POSIXlt()].
#' @param format Optional date-time format passed to [as.POSIXct()] or
#'   [as.POSIXlt()].
#' @param ... Additional arguments passed to the corresponding base method.
#' @return A `Date`, `POSIXct`, or `POSIXlt` vector, respectively.
#'
#' @examples
#' x <- create_iso8601("20200102", .format = "ymd")
#' as.Date(x)
#' as.POSIXct(x, tz = "UTC")
#' as.POSIXlt(x, tz = "UTC")
#'
#' @name iso8601_coercion
NULL

#' @rdname iso8601_coercion
#' @export
as.Date.iso8601 <- function(x, ...) {
  as.Date(unclass(x), ...)
}

#' @rdname iso8601_coercion
#' @export
as.POSIXct.iso8601 <- function(x, tz = "", format, ...) {
  if (missing(format)) {
    as.POSIXct(sub("T", " ", unclass(x), fixed = TRUE), tz = tz, ...)
  } else {
    as.POSIXct(unclass(x), tz = tz, format = format, ...)
  }
}

#' @rdname iso8601_coercion
#' @export
as.POSIXlt.iso8601 <- function(x, tz = "", format, ...) {
  if (missing(format)) {
    as.POSIXlt(sub("T", " ", unclass(x), fixed = TRUE), tz = tz, ...)
  } else {
    as.POSIXlt(unclass(x), tz = tz, format = format, ...)
  }
}
