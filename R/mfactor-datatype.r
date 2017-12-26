###############################################################################
# factor with multiple descriptions
###############################################################################

#' "mfactor" class
#'
#' @name mfactor-class
#' @aliases mfactor
#' @family mfactor
#'
#' @exportClass mfactor
setOldClass("mfactor")

#' As("ANY", "mfactor")
#'
#' @name as
#' @family ANY
setAs("ANY", "mfactor", function(from) as.mfactor(from))

#' Keeps class and attributes when accessing subsets of the value
#'
#' @param x mfactor
#' @inheritDotParams `[`
#' @export
`[.mfactor` <- function(x, ...) {
  r <- x
  class(r) <- "vector"
  r <- NextMethod("[", r, ...)
  mostattributes(r) <- attributes(x)
  r
}

#' Keeps class and attributes when accessing subsets of the value
#'
#' @param x mfactor
#' @inheritDotParams `[[`
#' @export
`[[.mfactor` <- function(x, ...) {
  r <- NextMethod("[", x, ...)
  mostattributes(r) <- attributes(x)
  r
}

#' Generic function to overload for casting to mfactor
#'
#' @param x values to transform
#' @inheritDotParams as
#' @export
as.mfactor <- function(x, ...) UseMethod("as.mfactor")

#' mfactor generation
#'
#' @param x values to transform
#' @param levels vector defining the values x can take
#' @param labels dataframe representing labels for this mfactor
#' @param exclude values to remove from x
#' @param ordered defines if levels have to be ordered
#' @param nmax max number of levels
#' @param representation name of the initial representation of this mfactor
#' @export
mfactor <- function(x = character(), levels, labels,
  exclude = NA, ordered = is.ordered(x), nmax = NA,
  representation = NULL) {
  if (is.null(x))
      x <- character()
  # Keeps the names
  nx <- names(x)
  # if levels are not defined, find them
  if (missing(levels)) {
    y <- unique(x, nmax = nmax)
    ind <- sort.list(y)
    levels <- unique(as.character(y)[ind])
  }
  # if labels are not defined, create them
  if (missing(labels)) {
    labels <- stats::setNames(as.data.frame(levels), c("default"))
  }

  force(ordered)

  if (!is.character(x)) x <- as.character(x)
  # Remove the elements to exclude
  levels <- levels[is.na(match(levels, exclude))]

  # Associates each element to its value
  f <- match(x, levels)
  if (!is.null(nx))
    names(f) <- nx

  nl <- nrow(labels)
  nL <- length(levels)

  if (!(nl == nL))
    stop(gettextf("invalid 'labels'; length is %d ... should be %d",
            nl, nL), domain = NA)

  mlevels(f) <- labels

  class(f) <- c(if (ordered) "ordered", "mfactor")

  # If representation is set to null or the representation is not in
  # the labels, the representation will be the first label
  attr(f, "representation") <- {
    if (is.null(representation) | sum(colnames(labels) == representation) == 0)
      colnames(labels)[1]
    else
      representation
  }
  f
}

#' Overload of the <- operator in order to define mfactor levels
#'
#' @param x mfactor
#' @param value labels
#' @export
`mlevels<-` <- function(x, value) {
  for (i in 1:ncol(value)) {
    attr(x, paste0("label-", colnames(value)[[i]])) <- vapply(value[, i], format, "")
  }
  x
}

#' Overload of the <- operator in order to define mfactor representation
#'
#' @param x mfactor
#' @param value representation name
#' @export
`representation<-` <- function(x, value) {
  if (is.null(attributes(x)[[paste0("label-", value)]])) {
    warning(paste0("Representation ", value, " is not defined as label"))
  } else {
    attr(x, "representation") <- value
  }
  x
}

#' Transformation to mfactor
#'
#' @param x value
#' @inheritDotParams as.mfactor
#' @export
as.mfactor.default <- function(x, ...) {
	if (is.null(class(x))) {
		x
	} else if (is.list(x)) {
		lapply(x, as.mfactor, ...)
	} else if (is.numeric(x)) {
    mfactor(as.character(x))
	} else if (is.character(x)) {
    mfactor(x)
  } else {
		stop(paste0(class(x), " not coercible to adaptativeFormat"))
	}
}

#' Transforms an mfactor to data frame
#'
#' @param x mfactor
#' @inheritDotParams as.data.frame.vector
#' @export
as.data.frame.mfactor <- function(x, ...) {
  funArgs <- list(...)
	funArgs <- funArgs[!(names(funArgs) == "x")]
  r <- do.call(as.data.frame.vector, c(list("x" = x), funArgs))
  mostattributes(r[[1]]) <- attributes(x)
  r
}

#' Transforms an mfactor to character depending to the underlying representation needed
#'
#' @param x mfactor
#' @export
as.character.mfactor <- function(x, ...) {
  # value is null, return null
  if (is.null(x)) x
  # representation is unknown, show underlying representation
  else if (is.null(attr(x, "representation"))) as.numeric(x)
  # representation is not defined, show underlying representation
  else if (is.null(attr(x, paste0("label-", attr(x, "representation"))))) as.numeric(x)
  else {
    # Use factor as.character to speed up
    r <- x
    attributes(r) <- NULL
    class(r) <- "factor"
    levels(r) <- attr(x, paste0("label-", attr(x, "representation")))
    as.character(r)
  }
}

#' Overloads the unique capacity for mfactor
#'
#' @param x mfactor
#' @inheritDotParams unique.default
#' @export
unique.mfactor <- function(x, ...) {
  funArgs <- list(...)
	funArgs <- funArgs[!(names(funArgs) == "x")]
  r <- do.call(unique.default, c(list("x" = x), funArgs))
  mostattributes(r) <- attributes(x)
  r
}

#' Unlist for mfactor elements
#'
#' @param x mfactor
#' @inheritDotParams unlist
#' @export
unlistMfactor <- function(x, ...) {
  if (!is.list(x)) {
    warning("Not a list")
    x
  }
  funArgs <- list(...)
	funArgs <- funArgs[!(names(funArgs) == "x")]
  r <- do.call(unlist, c(list("x" = x), funArgs))
  mostattributes(r) <- attributes(x[[1]])
  r
}

#' Formats en element of an mfactor
#'
#' @param x mfactor element
#' @export
format.mfactor <- function(x, ...) {
  structure(as.character.mfactor(x), names = names(x), dim = dim(x),
    dimnames = dimnames(x))
}
