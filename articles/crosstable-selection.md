# Select variables

This vignette will review the different ways of selecting variables to
describe with `crosstable`. For more general informations about
`crosstable`, see
[`vignette("crosstable")`](https://danchaltiel.github.io/crosstable/articles/crosstable.md)
([link](https://danchaltiel.github.io/crosstable/articles/crosstable.md)).
For more information and tips on `tidyselect`, see [tidyselect
syntax](https://tidyselect.r-lib.org/articles/syntax.html)

## Whole table

The simplest case is when you want to describe the whole table, as you
need no further argument. If you really want to be more explicit, you
also can use
[`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html).
All `tidyselect` helpers are re-exported by `dplyr` so we only want to
load this latter package.

Here are the 10 first lines of the `iris2` dataset:

``` r
library(crosstable)
ct = crosstable(iris2, everything()) #or simply `crosstable(iris2)`
ct %>% 
  as_flextable(keep_id=TRUE)
```

| .id          | label           | variable    | value           |
|--------------|-----------------|-------------|-----------------|
| Sepal.Length | Length of Sepal | Min / Max   | 4.3 / 7.9       |
|              |                 | Med \[IQR\] | 5.8 \[5.1;6.4\] |
|              |                 | Mean (std)  | 5.8 (0.8)       |
|              |                 | N (NA)      | 150 (0)         |
| Sepal.Width  | Width of Sepal  | Min / Max   | 2.0 / 4.4       |
|              |                 | Med \[IQR\] | 3.0 \[2.8;3.3\] |
|              |                 | Mean (std)  | 3.1 (0.4)       |
|              |                 | N (NA)      | 150 (0)         |
| Petal.Length | Length of Petal | Min / Max   | 1.0 / 6.9       |
|              |                 | Med \[IQR\] | 4.3 \[1.6;5.1\] |
|              |                 | Mean (std)  | 3.8 (1.8)       |
|              |                 | N (NA)      | 150 (0)         |
| Petal.Width  | Width of Petal  | Min / Max   | 0.1 / 2.5       |
|              |                 | Med \[IQR\] | 1.3 \[0.3;1.8\] |
|              |                 | Mean (std)  | 1.2 (0.8)       |
|              |                 | N (NA)      | 150 (0)         |
| Species      | Specie          | setosa      | 50 (33.33%)     |
|              |                 | versicolor  | 50 (33.33%)     |
|              |                 | virginica   | 50 (33.33%)     |

## Select by column name

### Name

Just like with
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
you can use names with or without quotes to select variables you want to
describe. Use [`c()`](https://rdrr.io/r/base/c.html) to select several
columns:

``` r
crosstable(mtcars2, c(mpg, "qsec"), by=vs) %>% 
  as_flextable(keep_id=TRUE)
```

| .id  | label             | variable    | Engine             |                    |
|------|-------------------|-------------|--------------------|--------------------|
|      |                   |             | straight           | vshaped            |
| mpg  | Miles/(US) gallon | Min / Max   | 17.8 / 33.9        | 10.4 / 26.0        |
|      |                   | Med \[IQR\] | 22.8 \[21.4;29.6\] | 15.7 \[14.8;19.1\] |
|      |                   | Mean (std)  | 24.6 (5.4)         | 16.6 (3.9)         |
|      |                   | N (NA)      | 14 (0)             | 18 (0)             |
| qsec | 1/4 mile time     | Min / Max   | 16.9 / 22.9        | 14.5 / 18.0        |
|      |                   | Med \[IQR\] | 19.2 \[18.6;20.0\] | 17.0 \[16.0;17.4\] |
|      |                   | Mean (std)  | 19.3 (1.4)         | 16.7 (1.1)         |
|      |                   | N (NA)      | 14 (0)             | 18 (0)             |

### External vector

However, it is better to use `all_of` or `any_of` when you take your
column names from an external vector. Otherwise, there would be an
ambiguity as you might have wanted to select a column named like this
vector.

``` r
qsec = c("mpg", "cyl") #wouldn't that be the most evil variable name ever?
crosstable(mtcars2, any_of(qsec), by=vs) %>% 
  as_flextable(keep_id=TRUE)
```

| .id | label               | variable    | Engine             |                    |
|-----|---------------------|-------------|--------------------|--------------------|
|     |                     |             | straight           | vshaped            |
| mpg | Miles/(US) gallon   | Min / Max   | 17.8 / 33.9        | 10.4 / 26.0        |
|     |                     | Med \[IQR\] | 22.8 \[21.4;29.6\] | 15.7 \[14.8;19.1\] |
|     |                     | Mean (std)  | 24.6 (5.4)         | 16.6 (3.9)         |
|     |                     | N (NA)      | 14 (0)             | 18 (0)             |
| cyl | Number of cylinders | 4           | 10 (90.91%)        | 1 (9.09%)          |
|     |                     | 6           | 4 (57.14%)         | 3 (42.86%)         |
|     |                     | 8           | 0 (0%)             | 14 (100.00%)       |

### Negation

You can use negation to keep all but some columns:

``` r
crosstable(mtcars2, c(-mpg, -cyl, -1), by=vs) %>% head(8) %>% #-c(mpg, cyl, 1) would also work
  as_flextable(keep_id=TRUE)
```

| .id  | label                 | variable    | Engine               |                       |
|------|-----------------------|-------------|----------------------|-----------------------|
|      |                       |             | straight             | vshaped               |
| disp | Displacement (cu.in.) | Min / Max   | 71.1 / 258.0         | 120.3 / 472.0         |
|      |                       | Med \[IQR\] | 120.5 \[83.0;162.4\] | 311.0 \[275.8;360.0\] |
|      |                       | Mean (std)  | 132.5 (56.9)         | 307.1 (106.8)         |
|      |                       | N (NA)      | 14 (0)               | 18 (0)                |
| hp   | Gross horsepower      | Min / Max   | 52.0 / 123.0         | 91.0 / 335.0          |
|      |                       | Med \[IQR\] | 96.0 \[66.0;109.8\]  | 180.0 \[156.2;226.2\] |
|      |                       | Mean (std)  | 91.4 (24.4)          | 189.7 (60.3)          |
|      |                       | N (NA)      | 14 (0)               | 18 (0)                |

### Indice

This can be useful sometimes, for instance when you want to quickly
describe the 3 first columns.

``` r
crosstable(mtcars2, 2:4, by=vs) %>% 
  as_flextable(keep_id=TRUE)
```

| .id  | label                 | variable    | Engine               |                       |
|------|-----------------------|-------------|----------------------|-----------------------|
|      |                       |             | straight             | vshaped               |
| mpg  | Miles/(US) gallon     | Min / Max   | 17.8 / 33.9          | 10.4 / 26.0           |
|      |                       | Med \[IQR\] | 22.8 \[21.4;29.6\]   | 15.7 \[14.8;19.1\]    |
|      |                       | Mean (std)  | 24.6 (5.4)           | 16.6 (3.9)            |
|      |                       | N (NA)      | 14 (0)               | 18 (0)                |
| cyl  | Number of cylinders   | 4           | 10 (90.91%)          | 1 (9.09%)             |
|      |                       | 6           | 4 (57.14%)           | 3 (42.86%)            |
|      |                       | 8           | 0 (0%)               | 14 (100.00%)          |
| disp | Displacement (cu.in.) | Min / Max   | 71.1 / 258.0         | 120.3 / 472.0         |
|      |                       | Med \[IQR\] | 120.5 \[83.0;162.4\] | 311.0 \[275.8;360.0\] |
|      |                       | Mean (std)  | 132.5 (56.9)         | 307.1 (106.8)         |
|      |                       | N (NA)      | 14 (0)               | 18 (0)                |

You can also use negation (`-(1:3)`), concatenation (`c(1,2,3)`), or
both (`crosstable(mtcars2, 1:4, -2, by=vs)`).

## Select with `tidyselect` helpers

Along with
[`everything()`](https://tidyselect.r-lib.org/reference/everything.html),
`tidyselect` provides a large choice of helpers. You can browse
[`?tidyselect::select_helpers`](https://tidyselect.r-lib.org/reference/language.html)
for a complete list.

Note that all have the useful `ignore.case` argument which is often very
convenient.

The main ones are re-exported by crosstable:
[`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
[`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html)
and
[`matches()`](https://tidyselect.r-lib.org/reference/starts_with.html).
Here are some examples:

``` r
crosstable(mtcars2, starts_with("d")) %>% 
  as_flextable(keep_id=TRUE)
```

| .id  | label                 | variable    | value                 |
|------|-----------------------|-------------|-----------------------|
| disp | Displacement (cu.in.) | Min / Max   | 71.1 / 472.0          |
|      |                       | Med \[IQR\] | 196.3 \[120.8;326.0\] |
|      |                       | Mean (std)  | 230.7 (123.9)         |
|      |                       | N (NA)      | 32 (0)                |
| drat | Rear axle ratio       | Min / Max   | 2.8 / 4.9             |
|      |                       | Med \[IQR\] | 3.7 \[3.1;3.9\]       |
|      |                       | Mean (std)  | 3.6 (0.5)             |
|      |                       | N (NA)      | 32 (0)                |

``` r
crosstable(mtcars2, c(ends_with("g"), contains("yl"))) %>% 
  as_flextable(keep_id=TRUE)
```

| .id | label               | variable    | value              |
|-----|---------------------|-------------|--------------------|
| mpg | Miles/(US) gallon   | Min / Max   | 10.4 / 33.9        |
|     |                     | Med \[IQR\] | 19.2 \[15.4;22.8\] |
|     |                     | Mean (std)  | 20.1 (6.0)         |
|     |                     | N (NA)      | 32 (0)             |
| cyl | Number of cylinders | 4           | 11 (34.38%)        |
|     |                     | 6           | 7 (21.88%)         |
|     |                     | 8           | 14 (43.75%)        |

``` r
#to all regex haters: the following call selects all columns which name 
#starts with "d" or "g", followed by exactly 3 characters
crosstable(mtcars2, matches("^d|g.{3}$")) %>% 
  as_flextable(keep_id=TRUE)
```

| .id  | label                   | variable    | value                 |
|------|-------------------------|-------------|-----------------------|
| disp | Displacement (cu.in.)   | Min / Max   | 71.1 / 472.0          |
|      |                         | Med \[IQR\] | 196.3 \[120.8;326.0\] |
|      |                         | Mean (std)  | 230.7 (123.9)         |
|      |                         | N (NA)      | 32 (0)                |
| drat | Rear axle ratio         | Min / Max   | 2.8 / 4.9             |
|      |                         | Med \[IQR\] | 3.7 \[3.1;3.9\]       |
|      |                         | Mean (std)  | 3.6 (0.5)             |
|      |                         | N (NA)      | 32 (0)                |
| gear | Number of forward gears | 3           | 15 (46.88%)           |
|      |                         | 4           | 12 (37.50%)           |
|      |                         | 5           | 5 (15.62%)            |

## Select with predicate functions

Sometimes, you want to select columns if they meet a set of
specifications, for instance of type or of value. You can then use
predicate functions: functions that return a single logical value. If
the function is named, it is a good practice to wrap it in
[`where()`](https://tidyselect.r-lib.org/reference/where.html).

For instance, you might want to keep only `character` variables:

``` r
crosstable(mtcars2, c(where(is.character), where(is.factor), -model)) %>% 
  as_flextable(keep_id=TRUE)
```

| .id  | label                   | variable | value       |
|------|-------------------------|----------|-------------|
| vs   | Engine                  | straight | 14 (43.75%) |
|      |                         | vshaped  | 18 (56.25%) |
| am   | Transmission            | auto     | 19 (59.38%) |
|      |                         | manual   | 13 (40.62%) |
| cyl  | Number of cylinders     | 4        | 11 (34.38%) |
|      |                         | 6        | 7 (21.88%)  |
|      |                         | 8        | 14 (43.75%) |
| gear | Number of forward gears | 3        | 15 (46.88%) |
|      |                         | 4        | 12 (37.50%) |
|      |                         | 5        | 5 (15.62%)  |

Using anonymous functions, you can even use more complicated patterns.
For instance, you might want only numeric variables which mean is higher
than 100:

``` r
crosstable(mtcars2, where(function(x) is.numeric(x) && mean(x)>100)) %>% 
  as_flextable(keep_id=TRUE)
```

| .id  | label                 | variable    | value                 |
|------|-----------------------|-------------|-----------------------|
| disp | Displacement (cu.in.) | Min / Max   | 71.1 / 472.0          |
|      |                       | Med \[IQR\] | 196.3 \[120.8;326.0\] |
|      |                       | Mean (std)  | 230.7 (123.9)         |
|      |                       | N (NA)      | 32 (0)                |
| hp   | Gross horsepower      | Min / Max   | 52.0 / 335.0          |
|      |                       | Med \[IQR\] | 123.0 \[96.5;180.0\]  |
|      |                       | Mean (std)  | 146.7 (68.6)          |
|      |                       | N (NA)      | 32 (0)                |

Of note, crosstable support lambda-functions, so you could instead write
`crosstable(mtcars2, where(~is.numeric(.x) && mean(.x)>100))` for the
exact same result but a tidier code.

The only logical constraint is that the function in
[`where()`](https://tidyselect.r-lib.org/reference/where.html) should
return a single logical value. Use `&&`, `||`, and parenthesis to
combine functions in complex patterns.

## Select with a formula

If you want to mutate some variables in real-time, you can use the
formula interface. The left-hand-side are the variables to describe,
while the right-hand-side is the `by` variable (which can be set to
`NULL`, `0` or `1` for “no variable”).

``` r
crosstable(mtcars2, mpg+cyl ~ vs) %>% 
  as_flextable(keep_id=TRUE)
```

| .id | label               | variable    | Engine             |                    |
|-----|---------------------|-------------|--------------------|--------------------|
|     |                     |             | straight           | vshaped            |
| mpg | Miles/(US) gallon   | Min / Max   | 17.8 / 33.9        | 10.4 / 26.0        |
|     |                     | Med \[IQR\] | 22.8 \[21.4;29.6\] | 15.7 \[14.8;19.1\] |
|     |                     | Mean (std)  | 24.6 (5.4)         | 16.6 (3.9)         |
|     |                     | N (NA)      | 14 (0)             | 18 (0)             |
| cyl | Number of cylinders | 4           | 10 (90.91%)        | 1 (9.09%)          |
|     |                     | 6           | 4 (57.14%)         | 3 (42.86%)         |
|     |                     | 8           | 0 (0%)             | 14 (100.00%)       |

This permits very complex and interesting patterns, using functions *in
situ* and operations using with the `I` function. Labels are inherited
and make little sense though.

``` r
crosstable(mtcars2, sqrt(mpg) + I(qsec^2) ~ ifelse(mpg>20,"mpg>20","mpg<20"),
           label=FALSE) %>% 
  as_flextable()
```

| label     | variable    | ifelse(mpg \> 20, "mpg\>20", "mpg\<20") |                       |
|-----------|-------------|-----------------------------------------|-----------------------|
|           |             | mpg\<20                                 | mpg\>20               |
| sqrt(mpg) | Min / Max   | 3.2 / 4.4                               | 4.6 / 5.8             |
|           | Med \[IQR\] | 4.0 \[3.8;4.2\]                         | 4.9 \[4.6;5.4\]       |
|           | Mean (std)  | 4.0 (0.4)                               | 5.0 (0.4)             |
|           | N (NA)      | 18 (0)                                  | 14 (0)                |
| I(qsec^2) | Min / Max   | 210.2 / 408.8                           | 270.9 / 524.4         |
|           | Med \[IQR\] | 301.0 \[259.3;321.8\]                   | 351.8 \[303.0;391.8\] |
|           | Mean (std)  | 294.3 (50.4)                            | 356.8 (66.7)          |
|           | N (NA)      | 18 (0)                                  | 14 (0)                |

Note that you cannot use `tidyselect` helpers in formulas and that you
cannot use formula declared in an object.

## Ultimate example

Lets play a little with all of this :-)

I want all numeric variables that do not start by “d” or “w”, but I
still want `drat` in the end.

``` r
crosstable(mtcars2, c(where(is.numeric), -matches("^d|w"), drat), label=FALSE) %>% 
  as_flextable()
```

| label | variable    | value                |
|-------|-------------|----------------------|
| mpg   | Min / Max   | 10.4 / 33.9          |
|       | Med \[IQR\] | 19.2 \[15.4;22.8\]   |
|       | Mean (std)  | 20.1 (6.0)           |
|       | N (NA)      | 32 (0)               |
| hp    | Min / Max   | 52.0 / 335.0         |
|       | Med \[IQR\] | 123.0 \[96.5;180.0\] |
|       | Mean (std)  | 146.7 (68.6)         |
|       | N (NA)      | 32 (0)               |
| qsec  | Min / Max   | 14.5 / 22.9          |
|       | Med \[IQR\] | 17.7 \[16.9;18.9\]   |
|       | Mean (std)  | 17.8 (1.8)           |
|       | N (NA)      | 32 (0)               |
| carb  | Min / Max   | 1.0 / 8.0            |
|       | Med \[IQR\] | 2.0 \[2.0;4.0\]      |
|       | Mean (std)  | 2.8 (1.6)            |
|       | N (NA)      | 32 (0)               |
| drat  | Min / Max   | 2.8 / 4.9            |
|       | Med \[IQR\] | 3.7 \[3.1;3.9\]      |
|       | Mean (std)  | 3.6 (0.5)            |
|       | N (NA)      | 32 (0)               |
