# Alternative to [`officer::body_add_gg()`](https://davidgohel.github.io/officer/reference/body_add_gg.html) which uses `ggplot` syntax

Alternative to
[`officer::body_add_gg()`](https://davidgohel.github.io/officer/reference/body_add_gg.html)
which uses `ggplot` syntax

## Usage

``` r
body_add_gg2(
  doc,
  value,
  width = getOption("crosstable_gg_width", 6),
  height = getOption("crosstable_gg_height", 5),
  units = getOption("crosstable_units", "in"),
  style = getOption("crosstable_style_image", doc$default_styles$paragraph),
  add_legend = TRUE,
  bookmark = NULL,
  res = 300,
  ...
)
```

## Arguments

- doc:

  An `rdocx` object from **officer**.

- value:

  ggplot object

- width, height:

  width and height. Can be abbreviated to w and h.

- units:

  units for width and height

- style:

  paragraph style

- add_legend:

  add a legend if the ggplot has a legend attribute (see example)

- bookmark:

  the bookmark of the legend, if applicable

- res:

  resolution of the png image in ppi (passed to the argument `dpi` of
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html))

- ...:

  other arguments to be passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## Value

The docx object `doc`

## Author

Dan Chaltiel

## Examples

``` r
library(officer)
library(ggplot2)
p = ggplot(data=iris, aes(Sepal.Length, Petal.Length)) + geom_point()
attr(p, "legend") = "Sepal length by Petal length"
crosstable_options(
  units="cm",
  style_image="centered"
)
doc = read_docx() %>%
 body_add_normal("Text before") %>%
 body_add_gg2(p, w=14, h=10, scale=1.5) %>% #or units="cm" instead of using options
 body_add_normal("Text after")
write_and_open(doc)
```
