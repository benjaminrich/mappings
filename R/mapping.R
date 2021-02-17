#' Generate a Mapping Function
#'
#' This function returns a function that does a simple mapping from one set of value to another.
#' It is a function-generating function.
#'
#' @param from A vector.  This is the domain of the function.
#' @param to A vector of the same length as \code{from}. If omitted, then the
#' \code{names} of \code{from} are taken as the domain, and the values as the
#' values to map to.
#' @param na An alternative way to specify the value that \code{NA} maps to.
#' Ignored if from contains \code{NA}.
#' @param ch.as.fact A logical.  Should the mapping return a \code{factor}
#' instead of \code{character}?
#'
#' @details
#' 
#' This function returns a function.  When called with a vector
#' argument \code{x}, this function will return a vector \code{y} of
#' the same length as \code{x} and such that each element \code{y[i]}
#' is equal to \code{to[j]} where \code{j} is the smallest integer such
#' that \code{from[j] == x[i]}, and \code{NA} if no such \code{j}
#' exists.
#'
#' Note: \code{from} will always be matched as a string, even if it is numeric.
#' So, \code{mapping(1, "A")} and \code{mapping("1", "A")} are the same, and
#' both functions will return \code{"A"} when called with either \code{1} or
#' \code{"1"}.
#' 
#' @return
#' 
#' A function that translates from \code{from} to \code{to}.  The
#' function also has a attribute \code{"inverse"} which is a function
#' that performs the inverse mapping.
#' 
#' @examples
#' 
#' sex.mapping <- mapping(c("Female", "F", "Male", "M"), c(0, 0, 1, 1))
#' sex.mapping(c("Female", "Female", "Male", "F"))
#' 
#' sex.mapping <- mapping(0:1, c("Female", "Male"), na="Unknown")
#' sex.mapping(c(0, 1, NA, 0, 1, 1, 0))
#' inverse(sex.mapping)(c("Female", "Male", "Unknown"))
#' 
#' from <- c(0, 1, NA)
#' to <- c(NA, "Male", "Female")
#' x <- c(0, 1, NA, 0, 1, 1, 0)
#' sex.mapping <- mapping(c(0, 1, NA), c(NA, "Male", "Female"))
#' sex.mapping
#' sex.mapping(c(0, 1, NA, 0, 1, 1, 0))
#' inverse(sex.mapping)
#' inverse(sex.mapping)(c("Female", "Male", NA))
#' 
#' race.mapping <- mapping(c(1, 2, 5), c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE"))
#' race.mapping(1:5)
#' 
#' @keyword utilities
#' @export
mapping <- function(from, to, na=NA, ch.as.fact=TRUE) {
  if (missing(to)) {
    x <- from
    from <- names(x)
    to <- setNames(x, NULL)
    if (!is.na(na)) {
      from <- c(from, NA)
      to <- c(to, na)
    }
  }
  if (length(from) != length(to)) {
    stop("Lengths of from and to should be the same")
  }
  from.dup <- duplicated(from)
  to.dup <- duplicated(to)

  from.unique.from <- from[!from.dup]
  from.unique.to <- from[!to.dup]
  to.unique.from <- to[!from.dup]
  to.unique.to <- to[!to.dup]

  if (any(is.na(from.unique.from))) {
    i <- which(is.na(from.unique.from)) 
    na.mapsto <- to.unique.from[i]
    from.unique.from <- from.unique.from[-i]
    to.unique.from <- to.unique.from[-i]
  } else {
    na.mapsto <- na
  }

  if (any(is.na(to.unique.to))) {
    i <- which(is.na(to.unique.to)) 
    na.mapsfrom <- from.unique.to[i]
    to.unique.to <- to.unique.to[-i]
    from.unique.to <- from.unique.to[-i]
  } else {
    na.mapsfrom <- NA
  }

  if (ch.as.fact) {
    if (is.character(from)) {
      from.unique.to <- factor(from.unique.to, levels=from.unique.from)
    }
    if (is.character(to)) {
      to.unique.from <- factor(to.unique.from, levels=unique(c(to.unique.to, na.mapsto)))
    }
  }

  fn <- function(x) {
    na.x <- is.na(x)
    x <- factor(x, levels=from.unique.from)
    x <- to.unique.from[x]
    x[na.x] <- na.mapsto
    x
  }

  attr(fn, "inverse") <- function(z) {
    na.z <- is.na(z)
    z <- factor(z, levels=to.unique.to)
    z <- from.unique.to[z]
    z[na.z] <- na.mapsfrom
    z
  }
  structure(fn, class="mapping", domain=from, codomain=to)
}

#' Domain and codomain of a mapping.
#' @param x A \code{\link{mapping}}
#' @export
domain <- function(x) { attr(x, "domain") }

#' @rdname domain
#' @export
codomain <- function(x) { attr(x, "codomain") }

#' Inverse of a mapping
#'
#' Given a \code{\link{mapping}} \code{x}, return the inverse mapping.
#'
#' @param x A \code{\link{mapping}}
#' @return The inverse \code{\link{mapping}}.
#' @examples
#' sex.mapping <- mapping(c("Female", "F", "Male", "M"), c(0, 0, 1, 1))
#' sex.inverse.mapping <- inverse(sex.mapping)
#' sex.inverse.mapping(c(0, 0, 1, 0))
#' @export
inverse <- function(x) { 
  structure(attr(x, "inverse"), class="mapping", domain=codomain(x), codomain=domain(x))
}

#' Convenient shorthand for specifying mappings with text strings
#'
#' @param text A multi-line string specifying a mapping with 2 columns (see examples).
#' @param file If `text` is missing, read from this file instead.
#' @param sep Character used as column separator.
#' @param flip If \code{TRUE}, flip the column order to To, From (default \code{FALSE}).
#' @param convert.na
#' @param numericWherePossible
#' @param ... Further arguments passed to \code{\link{mapping}}.
#' @examples
#' f <- text2mapping("
#' L | Low
#' M | Medium
#' H | High
#' ")
#' f(warpbreaks$tension)
#' @export
text2mapping <- function(text, file=NULL, sep="|", flip=FALSE, convert.na=TRUE, numericWherePossible=T, ...) {
  if (missing(text)) {
    x <- read.table(sep=sep, file=file, colClasses="character", header=FALSE)
  } else {
    x <- read.table(sep=sep, text=trimws(text), colClasses="character", header=FALSE)
  }
  x <- x[, sapply(x, function(y) !all(is.na(y) | trimws(y) == "")), drop=FALSE] # Remove empty
  if (ncol(x) == 1) {
    x <- cbind(x, x)
  }
  if (flip) {
    x[, 1:2] <- x[, 2:1]
  }
  x <- lapply(x, trimws)
  if (convert.na) {
    x[[1]][x[[1]]=="NA"] <- NA
    x[[2]][x[[2]]=="NA"] <- NA
    if (any(is.na(x[[1]]))) {
      warning("Domain contains missing values")
    }
  }
  if (numericWherePossible && sum(is.na(suppressWarnings(as.numeric(as.character(x[[2]]))))) == 0) {
    x[[2]] <- as.numeric(as.character(x[[2]]))
  }
  mapping(from=x[[1]], to=x[[2]], ...)
}

#' Convert a mapping to \code{data.frame}
#'
#' The resulting \code{data.frame} has 2 columns: \code{mapsfrom}, and \code{mapsto}.
#'
#' @param x A \code{\link{mapping}}
#' @param ... Ignored
#' @return A \code{data.frame}.
#' @export
as.data.frame.mapping <- function(x, ...) { data.frame(mapsfrom=domain(x), mapsto=codomain(x)) }

#' Print a mapping
#'
#' @param x A \code{\link{mapping}}
#' @param ... Ignored
#' @return Returns \code{x} invisibly.
#' @export
print.mapping <- function(x, ...) {
  cat("Mapping\n")
  print(as.data.frame(x), row.names=FALSE)
  invisible(x)
}

#' Apply a Mapping
#'
#' @param x The values to apply the \code{\link{mapping}} to.
#' @param ... Passed to \code{\link{mapping}}.
#' @return The values returned by \code{\link{mapping}}.
#' @export
amapping <- function(x, ...) {
  mapping(...)(x)
}

#' Construct a Factor From One or More Vectors
#'
#' A \code{factor} is constructed from one or more atomic vectors.  If more than
#' one atomic vector is supplied, then a compound value is constructed by
#' concatenating the values together. The order of the levels is the natural
#' order in which the values appear.
#
#' @param x An atomic vector.
#' @param ... Additional atomic vectors (optional).
#' @param sep A character to use as a separator when forming a compound value
#' (default ';').
#'
#' @return A \code{factor}.
#'
#' @examples
#' @export
cf <- function(x, ..., sep=";") {
  args <- list(...)
  if (length(args) > 0) {
    if (!all(sapply(args, length) == length(x))) {
      stop("Elements of a compound value must have the same length")
    }
    if (any(is.na(x)) || !all(sapply(args, nmissing) == 0)) {
      warning("One or more arguments contains missing values. 'NA' will be treated as a distict value when forming the compound value.")
    }
    x2 <- do.call(function(...) paste(..., sep=sep), args)
    x <- paste(x, x2, sep=sep)
  } else {
    if (any(is.na(x))) {
      warning("Argument contains missing values.")
    }
  }
  x <- factor(x, levels=unique(x))
  x
}


# vim: tw=80 ts=2 sw=2 et

