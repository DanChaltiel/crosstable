% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funs.R
\name{remove_labels}
\alias{remove_labels}
\alias{remove_label}
\title{Remove all label attributes.}
\usage{
remove_labels(x)
}
\arguments{
\item{x}{object to unlabel}
}
\description{
Use \code{remove_labels()} to remove the label from an object or to recursively remove all the labels from a collection of objects (such as a list or a data.frame). \cr This can be useful with functions reacting badly to labelled objects.
}
\examples{
crosstable(remove_label(mtcars2))
crosstable(mtcars2)
mtcars2 \%>\% remove_label \%>\% crosstable(1:2) #no labels
mtcars2$hp \%>\% remove_label \%>\% get_label #numeric
}
\seealso{
\link{get_label}, \link{set_label}, \link{import_labels}, \link[expss:unlab]{expss::unlab}
}