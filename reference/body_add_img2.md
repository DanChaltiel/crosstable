# Alternative to [`officer::body_add_img()`](https://davidgohel.github.io/officer/reference/body_add_img.html) which adds a `units` choice

Alternative to
[`officer::body_add_img()`](https://davidgohel.github.io/officer/reference/body_add_img.html)
which adds a `units` choice

## Usage

``` r
body_add_img2(
  doc,
  src,
  width,
  height,
  units = getOption("crosstable_units", "in"),
  style = getOption("crosstable_style_image", doc$default_styles$paragraph),
  ...
)
```

## Arguments

- doc:

  an `rdocx` object

- src:

  image filename, the basename of the file must not contain any blank.

- width, height:

  width and height. Can be abbreviated to w and h.

- units:

  units for width and height

- style:

  paragraph style

- ...:

  other arguments to be passed to
  [`officer::body_add_img()`](https://davidgohel.github.io/officer/reference/body_add_img.html)

## Value

The docx object `doc`

## See also

[`body_add_gg2()`](https://danchaltiel.github.io/crosstable/reference/body_add_gg2.md)

## Author

Dan Chaltiel

## Examples

``` r
img.file = file.path( R.home("doc"), "html", "logo.jpg" )
if(file.exists(img.file)){
    library(officer)
    options(crosstable_units="cm")
    doc = read_docx() %>%
        body_add_normal("This is the R logo.") %>%
        body_add_img2(img.file, h=7.6, w=10, style="centered") #or units="cm" without options
    #write_and_open(doc)
}
```
