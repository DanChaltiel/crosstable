#' Modified `mtcars` dataset
#'
#' @description
#' Modified `mtcars` dataset so:
#'  + every column is labelled (using `label` attribute)
#'  + rownames are a character column named `model`
#'  + `gear` and `cyl` columns are considered as numerical factors
#'  + `vs` and `am` columns are considered as character vector
#'
#' See \code{\link{mtcars}} for more informations on the original "Motor Trend Car Road Tests" dataset.
#'
#' @format A data frame with 32 observations on 11 variables with labels.
#'
#' @source \preformatted{
#' library(dplyr)
#' mtcars2 = mtcars \%>\%
#'     mutate(
#'        model=rownames(mtcars),
#'        vs=ifelse(vs==0, "vshaped", "straight"),
#'        am=ifelse(am==0, "auto", "manual"),
#'        across(c("cyl", "gear"), factor),
#'        .before=1
#'     ) \%>\%
#'     expss::apply_labels( #I also could have used [import_labels] or even `labelled::set_variable_labels()`
#'         mpg="Miles/(US) gallon",
#'         cyl="Number of cylinders",
#'         disp="Displacement (cu.in.)",
#'         hp="Gross horsepower",
#'         drat="Rear axle ratio",
#'         wt="Weight (1000 lbs)",
#'         qsec="1/4 mile time",
#'         vs="Engine",
#'         am="Transmission",
#'         gear="Number of forward gears",
#'        carb="Number of carburetors"
#'     )
#' }
#'
#' @examples
#' library(crosstable)
#' ct=crosstable(mtcars2, by=vs)
#' ct
#' as_flextable(ct)
"mtcars2"


#' Modified `iris` dataset
#'
#' @description
#' Modified `iris` dataset so:
#'  + every column is labelled (using `label` attribute)
#'  + `Species` column is considered as factor
#'
#' See \code{\link{iris}} for more informations on the original "Edgar Anderson's Iris Data" dataset.
#'
#' @format A data frame with 150 observations on 5 variables with labels.
#'
#' @source \preformatted{
#' library(dplyr)
#' iris2 = iris \%>\%
#'     expss::apply_labels( #I also could have used [import_labels] or even `labelled::set_variable_labels()`
#'         Species = "Specie",
#'         Sepal.Length = "Length of Sepal",
#'         Sepal.Width = "Width of Sepal",
#'         Petal.Length = "Length of Petal",
#'         Petal.Width = "Width of Petal"
#'     ) \%>\%
#'     as_tibble()
#' }
#'
#' @examples
#' library(crosstable)
#' ct=crosstable(iris2, by=Species)
#' ct
#' as_flextable(ct)
"iris2"
