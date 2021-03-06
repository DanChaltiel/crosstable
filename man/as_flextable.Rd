% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flextable.R
\docType{import}
\name{as_flextable.crosstable}
\alias{as_flextable.crosstable}
\alias{ctf}
\alias{cross_to_flextable}
\alias{to_flextable}
\alias{as_flextable}
\title{Turns a \code{crosstable} object into a formatted \code{flextable}}
\usage{
\method{as_flextable}{crosstable}(
  x,
  keep_id = FALSE,
  by_header = NULL,
  autofit = getOption("crosstable_autofit", TRUE),
  compact = getOption("crosstable_compact", FALSE),
  show_test_name = getOption("crosstable_show_test_name", TRUE),
  fontsizes = list(body = getOption("crosstable_fontsize_body", 11), subheaders =
    getOption("crosstable_fontsize_subheaders", 11), header =
    getOption("crosstable_fontsize_header", 11)),
  generic_labels = list(id = ".id", variable = "variable", value = "value", total =
    "Total", label = "label", test = "test", effect = "effect"),
  ...
)

to_flextable(
  x,
  keep_id = FALSE,
  by_header = NULL,
  autofit = getOption("crosstable_autofit", TRUE),
  compact = getOption("crosstable_compact", FALSE),
  show_test_name = getOption("crosstable_show_test_name", TRUE),
  fontsizes = list(body = getOption("crosstable_fontsize_body", 11), subheaders =
    getOption("crosstable_fontsize_subheaders", 11), header =
    getOption("crosstable_fontsize_header", 11)),
  generic_labels = list(id = ".id", variable = "variable", value = "value", total =
    "Total", label = "label", test = "test", effect = "effect"),
  ...
)
}
\arguments{
\item{x}{the result of \code{\link[=crosstable]{crosstable()}}}

\item{keep_id}{whether to keep the \code{.id} column}

\item{by_header}{a string to override the \code{by} header}

\item{autofit}{whether to use \code{\link[flextable:autofit]{flextable::autofit()}} on the table}

\item{compact}{whether to compact the table}

\item{show_test_name}{in the \code{test} column, show the test name}

\item{fontsizes}{font sizes as a list of keys [body, subheaders, header]. If set through arguments, all needed names should be mentioned.}

\item{generic_labels}{names of the crosstable default columns}

\item{...}{unused}
}
\value{
a flextable
}
\section{Methods (by class)}{
\itemize{
\item \code{crosstable}: Turns a \code{crosstable} object into a formatted \code{flextable}.
}}

\examples{
#Crosstables
library(crosstable)
library(dplyr)
options(crosstable_fontsize_header=14)
options(crosstable_fontsize_subheaders=10)
options(crosstable_fontsize_body=8)
crosstable(mtcars2) \%>\% as_flextable()
crosstable(mtcars2, by=vs, test = TRUE) \%>\% as_flextable()
crosstable(esoph, by=tobgp, test = TRUE) \%>\% as_flextable()
crosstable(iris, by=Species, test = TRUE) \%>\% as_flextable(autofit=FALSE)
crosstable(iris, by=Species, test = TRUE) \%>\% as_flextable(compact=TRUE)
crosstable(iris) \%>\% as_flextable(compact=TRUE, autofit=TRUE)

#Renaming (because why not?)
crosstable(iris, by=Species, total="both", test=TRUE, effect=TRUE) \%>\%
   rename(ID=.id, math=variable, Tot=Total, lab=label, pval=test, fx=effect) \%>\%
   as_flextable(by_header = "The specie", 
                generic_labels=list(id = "ID", variable = "math", total="Tot", 
                                    label = "lab", test = "pval", effect="fx"))
}
\seealso{
\code{\link[=crosstable]{crosstable()}}, \code{\link[flextable:flextable]{flextable::flextable()}}, \code{\link[=as_gt.crosstable]{as_gt.crosstable()}}
}
\author{
Dan Chaltiel
}
\keyword{internal}
\description{
These objects are imported from other packages. Follow the links
below to see their documentation.

\describe{
  \item{flextable}{\code{\link[flextable]{as_flextable}}}
}}

