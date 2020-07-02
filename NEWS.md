# crosstable 0.1.3 (2020-06-29)

* Added support for Dates description
* Removed some dependencies to ease installation

# crosstable 0.1.2 (2020-06-10)

* Effect refactoring: better error/warning handling
* Name sanitazion: replacing "." by "_" in function names
* Better error messages
* Bug fixes

# crosstable 0.1.1 (2020-06-07)

* Added `format_fixed`, rounding with the right number of decimals (including zeros)
* Method `cross_to_flextable` was deprecated and renamed `as_flextable` (cf https://github.com/davidgohel/flextable/issues/207)
* Added function `import_labels`, which apply labels taken from a source dataframe (name, label) to another dataframe
* Added `margin="none"` option, to remove percentages and keep only counts
* Columns of unsupported class are dropped with a warning instead of failing with an error
* Reexporting pipes and tidyselect helpers so that user does not have to load these libraries
* Computing time optimization (speed x2.6!)
* Fixed bug in normality testing
* Fixed bug in `compact`

# crosstable 0.1.0 (2020-04-09)

* First release, big changes from the `biostat2` package.bgb

