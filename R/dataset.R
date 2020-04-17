#' Modified `mtcars` dataset 
#'
#' @description 
#' Modified `mtcars` dataset so: 
#'  + every column is labelled (using `label` attribute from `expss` package, compatible with `Hmisc` package) 
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
#'     mutate(vs=ifelse(vs==0, "vshaped", "straight"),
#'        am=ifelse(am==0, "auto", "manual")) \%>\% 
#'     mutate_at(c("cyl", "gear"), factor) \%>\% 
#'     expss::apply_labels( #I also could have used `Hmisc::label`
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
#'  + every column is labelled (using `label` attribute from `expss` package, compatible with `Hmisc` package) 
#'  + `Species` column is considered as factor
#' 
#' See \code{\link{iris}} for more informations on the original "Edgar Anderson's Iris Data" dataset.
#'
#' @format A data frame with 150 observations on 5 variables with labels.
#' 
#' @source \preformatted{
#' library(dplyr)
#' iris2 = iris \%>\% 
#'     mutate_at("Species", factor) \%>\% 
#'     expss::apply_labels(
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