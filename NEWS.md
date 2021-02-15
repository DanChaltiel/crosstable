<!-- https://style.tidyverse.org/news.html -->

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

