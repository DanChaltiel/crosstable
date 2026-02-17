# Alternative to default `officer` print() function. Write the file and try to open it right away.

As it tests if the file is writable, this function also prevents
`officer:::print.rdocx()` to abort the RStudio session.

## Usage

``` r
write_and_open(doc, docx.file, add_date = FALSE)
```

## Arguments

- doc:

  the docx object

- docx.file:

  the name of the target file. If missing or NULL, the doc will open in
  a temporary file.

- add_date:

  whether to add the current date before `docx.file` extension.

## Value

Nothing, called for its side effects

## Author

Dan Chaltiel

## Examples

``` r
library(officer)
library(crosstable)
mytable = crosstable(mtcars2)
doc = read_docx() %>%
    body_add_crosstable(mytable)

write_and_open(doc)
if (FALSE) { # \dontrun{
write_and_open(doc, "example.docx")
write_and_open(doc, "example.docx", add_date=TRUE)
write_and_open(doc, "example.docx", add_date=Sys.Date()-1)
} # }
```
