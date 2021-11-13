<!-- https://style.tidyverse.org/news.html -->

# crosstable 0.3.0 <sub><sup>(xxx)</sup></sub>

#### New features

* New parameter `percent_pattern` in replacement of `crosstable(margin=x)` for better control over proportion format. Introduces the possibility of displaying confidence intervals (using Wilson score method) along with proportions.
* New function `body_add_crosstable_list()` to add a list of crosstables all at once, separated by some customizable titles. Also works with flextables and plain old dataframes.
* New argument `crosstable_padding_v` for `as_flextable()` to manage vertical padding. Also available as the global option `crosstable_padding_v`.
* New global options `crosstable_{arg}` for almost all arguments. See `?crosstable_options` for more details.


#### Bug fixes

* fixed a bug in `as_flextable()` occurring when `showNA=TRUE` (header row was disappearing)
* fixed a bug occurring very randomly, when rounding caused `format_fixed()` to return a numeric value ("Error: Can't combine `..1$value` <character> and `..2$value` <double>.")
* crosstable now shows all unused levels in `by` when it is a factor 
* removed extra bold columns in compacted crosstables displayed as flextables


#### Internal

* renamed branch `master` to `main`
* use a lot more snapshots in tests

# crosstable 0.2.2 <sub><sup>(2021-10-18)</sup></sub>

#### New features

* Added support for multiple `by`! You can now write `crosstable(mtcars, c(mpg, gear), by=c(am, vs)) %>% as_flextable()`.
* Added a macro that can autofit every table in the document at once. This macro can be generated using the function `generate_autofit_macro()` which creates a file that should then be imported into MS Word.
* `body_add_crosstable()` gains a `padding_v` argument to control the vertical padding of all rows.
* `body_add_title()` and `body_add_xxx_legend)` gain a glue functionality. You can now write `body_add_title("The iris dataset (nrow={nrow(iris)})", 1)`.
* `as_workbook()` can now take a named list of crosstables, that will be considered as sheets. 
* New parameter `percent` in `format_fixed(percent=TRUE/FALSE)` to easily format percentages.


#### Minor changes

* `style` is deprecated in `body_add_table_legend()` and `body_add_image_legend()` in favor of `name_format`.
* Changed the behaviour of some `effect` calculations that were done by column instead of rows. That might change some outputs but not their meaning.
* `body_add_normal()` now removes duplicated spaces (squish) in its input by default. Use `squish=FALSE` to override.
* `docx_bookmarks2()` gains a `target` parameter.

#### Bug fixes

* `effect` calculation now takes into account the reference level (first level of a factor).
* `body_add_crosstable()` rightly takes `body_fontsize` and `header_fontsize` into account.
* Added few more warnings, so that you know what went wrong.


#### Internal
* burgled 2 functions using `burglr::burgle()` to avoid dependency: `nortest::ad.test()` and `DescTools::CochranArmitageTest()`.
* fixes the bug from the breaking change in `testthat` (https://github.com/DanChaltiel/crosstable/pull/3).


# crosstable 0.2.1 <sub><sup>(2021-02-07)</sup></sub>

* First version on CRAN
* Improved functions naming in `funs`, especially with multiple combinations of named and unnamed functions, including lambda or anonymous
* Use `simplify=FALSE` in `get_label()` to get a list instead of a vector


# crosstable 0.2.0 <sub><sup>(2021-02-02)</sup></sub>

#### Misc

* added lots of global options for easier implementation. See `?crosstable_options` for the comprehensive list.
* added label helpers: `apply_labels()` (inspired by `expss`'s), `copy_label_from()` and `rename_dataframe_with_labels()`
* added `as_workbook()` to export a crosstable as a formatted `openxlsx` Excel workbook, for copypasting purpose.
* added `peek()` to open a crosstable in a temporary Word document, as copy-pasting in RStudio's viewer is very limited.
* fixed the bug when a columns contained both "NA" (string) and `<NA>` (missing).
* fixed the bug where function in `funs` was not found if declared in another environment.
* numerous other minor bugfixes and internal improvements.

#### Officer

* added **cross-reference functionality** to `body_add_figure_legend()` and `body_add_table_legend()`.   
Use the `bookmark` argument to set a reference, then write `"\\@ref(my_bkm)"` inside `body_add_normal()` to call it.
* added docx helpers to add lists: `body_add_list()` and `body_add_list_item()`. These will unfortunately not work with the default `officer` template.
* added some alternatives for some `officer` functions: 
  * `docx_bookmarks2()`, which list bookmarks found in the header and footer as well 
  * `body_add_img2()`, and `body_add_gg2()`, which win a `units=c("in", "cm", "mm")` argument
  * `write_and_open()`, an alternative to `print()` for documents, which tries to open it right away.

#### Deprecations

* Ellipsis (`...`) use in `crosstable()` has been deprecated for a more "tidy" syntax. Write `crosstable(mtcars2, c(disp, vs))` instead of `crosstable(mtcars2, disp, vs)`. Ellipsis will be defunct in future v1.0.
* `crosstable(.vars=)` has been renamed to `crosstable(cols=)`.
* `moystd()` has been renamed to `meansd()`.
* `body_add_glued()` has been superseded by `body_add_normal()`, which inherits all functionalities and more.

# crosstable 0.1.5 <sub><sup>(2020-08-02)</sup></sub>

* added minimal support for `gt` tables (with `as_gt()`) for those who like them better than `flextable`s
* improved working with `officer`: added `body_add_figure_legend()` and `fontsize` options for `body_add_crosstable()`

# crosstable 0.1.4 <sub><sup>(2020-07-16)</sup></sub>

* added `save_labels()` to ease working with `dplyr`
* added `meanCI()` an additional summary function to use in `crosstable()`'s `funs` argument
* improved support for `Date` variables
* multiple, numerous bug fixes
* renamed `moystd()` to `meansd()`

# crosstable 0.1.3 <sub><sup>(2020-06-29)</sup></sub>

* Added support for description of `Date` variables. Format can be specified in `funs_arg` with the `date_format` key. 
* Removed some dependencies to ease installation

# crosstable 0.1.2 <sub><sup>(2020-06-10)</sup></sub>

* Effect refactoring: better error/warning handling
* Name sanitation: replacing "." by "_" in function names
* Better error messages
* Bug fixes

# crosstable 0.1.1 <sub><sup>(2020-06-07)</sup></sub>

### New features and behaviors

* Added `format_fixed()`, rounding with the right number of decimals (including zeros)
* Added `import_labels()`, which apply labels taken from a source dataframe (name, label) to another dataframe
* Added `margin="none"` option, to remove percentages and keep only counts
* Columns of unsupported class are dropped with a warning instead of failing with an error

### Misc

* Method `cross_to_flextable()` (`ctf()`) was deprecated and renamed `as_flextable()` ([#207](https://github.com/davidgohel/flextable/issues/207))
* Reexporting pipes and tidyselect helpers so that user does not have to load these libraries
* Computing time optimization (speed x2.6!)
* Fixed bug in normality testing
* Fixed bug in `compact()`

# crosstable 0.1.0 <small>(2020-04-09)</small>

* First release, big changes from the [`biostat2`](https://github.com/eusebe/biostat2) package.

