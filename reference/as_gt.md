# Converts a `crosstable` object into a formatted `gt` table.

Converts a `crosstable` object into a formatted `gt` table.

Method to convert an object to a `gt` table

Default method to convert an object to a `gt` table

## Usage

``` r
# S3 method for class 'crosstable'
as_gt(
  x,
  show_test_name = TRUE,
  by_header = NULL,
  keep_id = FALSE,
  generic_labels = list(id = ".id", variable = "variable", value = "value", total =
    "Total", label = "label", test = "test", effect = "effect"),
  ...
)

as_gt(x, ...)

# Default S3 method
as_gt(x, ...)
```

## Arguments

- x:

  object to be converted

- show_test_name:

  in the `test` column, show the test name

- by_header:

  a string to override the `by` header

- keep_id:

  whether to keep the `.id` column

- generic_labels:

  names of the crosstable default columns

- ...:

  arguments for custom methods

## Value

a formatted `gt` table

## Methods (by class)

- `as_gt(crosstable)`: For crosstables

- `as_gt(default)`: default function

## See also

[`as_flextable.crosstable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)

[`gt::gt()`](https://gt.rstudio.com/reference/gt.html)

## Author

Dan Chaltiel

## Examples

``` r
xx = mtcars2 %>% dplyr::select(2:10)
crosstable(xx) %>% as_gt


  
```

value

Miles/(US) gallon

Min / Max

10.4 / 33.9

Med \[IQR\]

19.2 \[15.4;22.8\]

Mean (std)

20.1 (6.0)

N (NA)

32 (0)

Number of cylinders

4

11 (34.38%)

6

7 (21.88%)

8

14 (43.75%)

Displacement (cu.in.)

Min / Max

71.1 / 472.0

Med \[IQR\]

196.3 \[120.8;326.0\]

Mean (std)

230.7 (123.9)

N (NA)

32 (0)

Gross horsepower

Min / Max

52.0 / 335.0

Med \[IQR\]

123.0 \[96.5;180.0\]

Mean (std)

146.7 (68.6)

N (NA)

32 (0)

Rear axle ratio

Min / Max

2.8 / 4.9

Med \[IQR\]

3.7 \[3.1;3.9\]

Mean (std)

3.6 (0.5)

N (NA)

32 (0)

Weight (1000 lbs)

Min / Max

1.5 / 5.4

Med \[IQR\]

3.3 \[2.6;3.6\]

Mean (std)

3.2 (1.0)

N (NA)

32 (0)

1/4 mile time

Min / Max

14.5 / 22.9

Med \[IQR\]

17.7 \[16.9;18.9\]

Mean (std)

17.8 (1.8)

N (NA)

32 (0)

Engine

straight

14 (43.75%)

vshaped

18 (56.25%)

Transmission

auto

19 (59.38%)

manual

13 (40.62%)

[crosstable](https://danchaltiel.github.io/crosstable/reference/crosstable.md)(xx,
by=am) [%\>%](https://magrittr.tidyverse.org/reference/pipe.html) as_gt

[TABLE]

[crosstable](https://danchaltiel.github.io/crosstable/reference/crosstable.md)(xx,
by=cyl, test=TRUE, total=TRUE)
[%\>%](https://magrittr.tidyverse.org/reference/pipe.html)
as_gt(keep_id=TRUE, show_test_name=FALSE, by_header="Cylinders")

[TABLE]
