# Cleans names of a dataframe while retaining old names as labels

Cleans names of a dataframe while retaining old names as labels

## Usage

``` r
clean_names_with_labels(
  df,
  except = NULL,
  .fun = getOption("crosstable_clean_names_fun")
)
```

## Arguments

- df:

  a data.frame

- except:

  \<[`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)\>
  columns that should not be renamed.

- .fun:

  the function used to clean the names. Default function is limited; if
  the cleaning is not good enough you could use
  janitor::make_clean_names()

## Value

A dataframe with clean names and label attributes

## Author

Dan Chaltiel

## Examples

``` r
#options(crosstable_clean_names_fun=janitor::make_clean_names)
x = data.frame("name with space"=1, TwoWords=1, "total $ (2009)"=1, àccénts=1,
               check.names=FALSE)
cleaned = clean_names_with_labels(x, except=TwoWords)
cleaned %>% names()
#> [1] "name_with_space" "TwoWords"        "total_2009"      "accents"        
cleaned %>% get_label()
#>   name_with_space          TwoWords        total_2009           accents 
#> "name with space"        "TwoWords"  "total $ (2009)"         "àccénts" 
```
