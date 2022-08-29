
utils::globalVariables(c(".id", "variable", "name"))

# Utils -------------------------------------------------------------------


#' Test if an object is a crosstable
#'
#' @param x An object
#'
#' @return TRUE if the object inherits from the `crosstable` class or other subclasses.
#' @export
is.crosstable = function(x) {
  inherits(x, "crosstable")
}

#' @rdname is.crosstable
#' @export
is.transposed_crosstable = function(x) {
  inherits(x, "transposed_crosstable")
}

#' @rdname is.crosstable
#' @export
is.compacted_crosstable = function(x) {
  inherits(x, "compacted_crosstable")
}

#' @rdname is.crosstable
#' @export
is.multiby_crosstable = function(x) {
  inherits(x, "crosstable_multiby")
}



