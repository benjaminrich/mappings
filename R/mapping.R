#' Generate a Mapping Function
#'
#' This function returns a function that does a simple mapping from one set of value to another.
#' It is a function-generating function.
#'
#' @param from A vector.  This is the domain of the function.
#' @param to A vector of the same length as `from`. If omitted, then the
#' `names` of `from` are taken as the domain, and the values as the
#' values to map to. If `from` has no `names`, then `to` is equal to
#' `from` (useful for re-ordering `factor` levels).
#' @param na An alternative way to specify the value that `NA` maps to.
#' Ignored if from contains `NA`.
#' @param ch.as.fact A logical.  Should the mapping return a `factor`
#' instead of `character`?
#'
#' @details
#' 
#' This function returns a function.  When called with a vector
#' argument `x`, this function will return a vector `y` of
#' the same length as `x` and such that each element `y[i]`
#' is equal to `to[j]` where `j` is the smallest integer such
#' that `from[j] == x[i]`, and `NA` if no such `j`
#' exists.
#'
#' Note: `from` will always be matched as a string, even if it is numeric.
#' So, `mapping(1, "A")` and `mapping("1", "A")` are the same, and
#' both functions will return `"A"` when called with either `1` or
#' `"1"`.
#' 
#' @return
#' 
#' A function that translates from `from` to `to`.  The function also
#' has an [`inverse`] which is a function that performs the inverse mapping.
#' 
#' @seealso
#' [inverse()],
#' [codomain()],
#' [domain()],
#' [remap()],
#' [text2mapping()],
#' [cut_mapping()]
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
#' race.mapping <- mapping(c(
#'       "1"="WHITE",
#'       "2"="BLACK OR AFRICAN AMERICAN",
#'       "5"="AMERICAN INDIAN OR ALASKA NATIVE"))
#' race.mapping(1:5)
#' 
#' @importFrom stats setNames
#' @export
mapping <- function(from, to, na=NA, ch.as.fact=TRUE) {
  if (missing(to)) {
    x <- from
    if (!is.null(names(x))) {
      from <- names(x)
    }
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
#'
#' @param x A [`mapping`].
#' @return x A vector of the same type as we supplied when the
#' [`mapping`] was created.
#' @note
#' These aren't the true domain and codomain in the mathematical sense; both
#' can contain duplicates.
#' @examples
#' sex.mapping <- mapping(c("Female", "F", "Male", "M"), c(0, 0, 1, 1))
#' domain(sex.mapping)
#' codomain(sex.mapping)
#' @export
domain <- function(x) { attr(x, "domain") }

#' @rdname domain
#' @export
codomain <- function(x) { attr(x, "codomain") }

#' Inverse of a mapping
#'
#' Given a [`mapping`] `x`, return the inverse mapping.
#'
#' @param x A [`mapping`].
#' @return The inverse [`mapping`].
#' @examples
#' sex.mapping <- mapping(c("Female", "F", "Male", "M"), c(0, 0, 1, 1))
#' sex.inverse.mapping <- inverse(sex.mapping)
#' sex.inverse.mapping(c(0, 0, 1, 0))
#' @export
inverse <- function(x) { 
  if (is.null(attr(x, "inverse"))) {
    stop("This mapping is not invertable")
  }
  structure(attr(x, "inverse"), class="mapping", domain=codomain(x), codomain=domain(x))
}

#' Convenient shorthand for specifying mappings with text strings
#'
#' @param text A multi-line string specifying a mapping with 2 columns (see examples).
#' @param file If `text` is missing, read from this file instead.
#' @param sep Character used as column separator.
#' @param flip If `TRUE`, flip the column order to To, From (default `FALSE`).
#' @param convert.na If `TRUE`, the string `"NA"` will be converted to
#' `NA`.
#' @param numericWherePossible If `TRUE`, the mapping will return a
#' `numeric` vector if the codomain contains only numbers. 
#' @param ... Further arguments passed to [mapping()].
#' @return A [`mapping`].
#' @examples
#' f <- text2mapping("
#' L | Low
#' M | Medium
#' H | High
#' ")
#' f(warpbreaks$tension)
#' @importFrom utils read.table
#' @export
text2mapping <- function(text, file=NULL, sep="|", flip=FALSE, convert.na=TRUE, numericWherePossible=TRUE, ...) {
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

#' Convert a mapping to `data.frame`
#'
#' The resulting `data.frame` has 2 columns: `mapsfrom`, and `mapsto`.
#'
#' @param x A [`mapping`].
#' @param ... Ignored.
#' @return A `data.frame`.
#' @export
as.data.frame.mapping <- function(x, ...) { data.frame(mapsfrom=domain(x), mapsto=codomain(x)) }

#' Print a mapping
#'
#' @param x [`mapping`].
#' @param ... Ignored.
#' @return Returns `x` invisibly.
#' @export
print.mapping <- function(x, ...) {
  cat("Mapping\n")
  print(as.data.frame(x), row.names=FALSE)
  invisible(x)
}

#' Mapping from continuous to categorical
#'
#' @param ... Passed to [`cut()`][base::cut()].
#' @param to Passed to [mapping()].
#' @param na Passed to [mapping()].
#' @param ch.as.fact Passed to [mapping()].
#' @return A function that cuts a `numeric` vector and maps the result.
#' @examples
#' x <- c(0, 10, 20, 30, Inf)
#' m <- cut_mapping(x, right=FALSE,
#'     to=c("0 to <10", "10 to <20", "20 to <30", ">= 30"))
#' print(m)
#' m(c(5, 27, 3, 10, 99))
#' @export
cut_mapping <- function(..., to=NULL, na=NA, ch.as.fact=TRUE) {
  l <- levels(cut(numeric(0), ...))
  if (is.null(to)) to <- l
  m <- mapping(levels(cut(numeric(0), ...)), to=to, na=na, ch.as.fact=ch.as.fact)
  fn <- function(x) m(cut(x, ...))
  structure(fn, class="mapping", domain=domain(m), codomain=codomain(m))
}

#' A mapping that adds a prefix and/or suffix
#'
#' @param prefix,suffix Character strings.
#' @return A `mapping` function.
#' @examples
#'
#' # The objective is to turn a numeric vector into a factor such that
#' # the levels preserve the numeric order but contain the suffix "mg"
#' # (i.e., so that 2 becomes "2 mg" for instance)
#' x <- c(1, 2, 1, 10, 3, 2, 2, 1)
#'
#' # The following does not produce the levels in the desired numeric order
#' # (because alphabetical ordering places "10" before "2")
#' factor(paste(x, "mg"))
#'
#' # The following works, but takes 2 lines of code and requires a variable
#' # assignment
#' y <- factor(x)
#' levels(y) <- paste(levels(y), "mg")
#' y
#'
#' # This does the same thing with one line of code and no assignment
#' paste_mapping(, " mg")(x)
#'
#' # -----
#'
#' # In this example, you start with a factor, and want to preserve its ordering
#' x <- factor(c("Treatment", "Placebo"), levels=c("Treatment", "Placebo"))
#'
#' # Again, this won't work as desired
#' factor(paste("Randomized to", x, "Group"))
#'
#' # But this will
#' paste_mapping("Randomized to ", " Group")(x)
#' @export
paste_mapping <- function(prefix=NULL, suffix=NULL) {
  fn <- paste0
  m <- function(x) {
    x <- as.factor(x)
    if (!is.null(prefix) & !is.null(suffix)) {
      levels(x) <- fn(prefix, levels(x), suffix)
    } else if (!is.null(prefix)) {
      levels(x) <- fn(prefix, levels(x))
    } else if (!is.null(suffix)) {
      levels(x) <- fn(levels(x), suffix)
    }
    x
  }
  structure(m, class="mapping", domain="x", codomain=m("x"))
}

#levels_paste <- function(x, prefix="", suffix="", fn=paste0) {
#  x <- as.factor(x)
#  levels(x) <- fn(prefix, levels(x), suffix)
#  x
#}

#' Re-map a variable
#'
#' Apply a mapping to a vector directly. The mapping is temporary and not saved.
#'
#' @param x The values to apply the [`mapping`] to.
#' @param ... Passed to [mapping()].
#' @return The values returned by calling the [`mapping`] function.
#' @examples
#' x <- c("A", "B", "A")
#' remap(x, c(A=0, B=1))
#' @export
remap <- function(x, ...) {
  mapping(...)(x)
}

#' Construct a `factor` from one or more vectors
#'
#' A `factor` is constructed from one or more atomic vectors.  If more than
#' one atomic vector is supplied, then a compound value is constructed by
#' concatenating the values together. The order of the levels is the natural
#' order in which the values appear.
#
#' @param x An atomic vector.
#' @param ... Additional atomic vectors (optional).
#' @param sep A `character` to use as a separator when forming a compound value
#' (default ';').
#'
#' @return A `factor`.
#'
#' @examples
#' x <- c("A", "B", "A")
#' y <- c(2, 5, 7)
#' cf(x, y)
#' mapping(cf(x, y), c("X", "Y", "Z"))
#' @export
cf <- function(x, ..., sep=";") {
  args <- list(...)
  if (length(args) > 0) {
    if (!all(sapply(args, length) == length(x))) {
      stop("Elements of a compound value must have the same length")
    }
    nmissing <- function(x) { sum(is.na(x)) }
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

