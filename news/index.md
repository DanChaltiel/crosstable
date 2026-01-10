# Changelog

## crosstable 0.9.0 (dev)

##### New features

- New function
  [`body_add_flextable2()`](https://danchaltiel.github.io/crosstable/reference/body_add_flextable2.md):
  automatically adds a legend if the `flextable` object has a `"legend"`
  attribute. Equivalent functionality is now available in
  [`body_add_gg2()`](https://danchaltiel.github.io/crosstable/reference/body_add_gg2.md).
  ([\#100](https://github.com/DanChaltiel/crosstable/issues/100))
- New argument `rename_with_labels(cols)`, more intuitive than `except`.
  ([\#99](https://github.com/DanChaltiel/crosstable/issues/99))
- New function
  [`ct_bind_cols()`](https://danchaltiel.github.io/crosstable/reference/ct_bind_cols.md)
  helper to combine (column-bind) two multi-`by` crosstables into a
  wider crosstable, enabling multi-column outputs with intra-group
  margins (**experimental**).
  ([\#97](https://github.com/DanChaltiel/crosstable/issues/97))
- New argument `as_flextable(allow_breaks=FALSE)`, to avoid breaking a
  table group in 2 pages.
  ([\#92](https://github.com/DanChaltiel/crosstable/issues/92))
- New argument `as_flextable(compact=TRUE, collapse)`, to collapse
  levels in compact format.
  ([\#98](https://github.com/DanChaltiel/crosstable/issues/98)) For
  example, `collapse="Yes"` will yield a single row for the “Yes” level,
  instead of a redundant Yes/No that sums up to 100%. This will only
  apply if there are 2 levels only, `NA` included. See examples for
  more.

##### Bug fixes and improvements

- Fixed
  [`ct_compact()`](https://danchaltiel.github.io/crosstable/reference/ct_compact.md)
  so that `.id` is kept in the output.
- Fixed bug in
  [`rename_with_labels()`](https://danchaltiel.github.io/crosstable/reference/rename_with_labels.md)
  when `except` contains
  [`c()`](https://rdrr.io/r/base/c.html).([\#99](https://github.com/DanChaltiel/crosstable/issues/99))
- Fixed bug in
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  when a factor variable had `NA` as a level (like after
  `forcats::fct_value_to_na_level()`).
  ([\#94](https://github.com/DanChaltiel/crosstable/issues/94))

##### Lifecycle

- `compact()`, conflicting with
  [`purrr::compact()`](https://purrr.tidyverse.org/reference/keep.html)
  and deprecated if favor of
  [`ct_compact()`](https://danchaltiel.github.io/crosstable/reference/ct_compact.md)
  since 2022, has been removed from the package.

## crosstable 0.8.2

CRAN release: 2025-09-07

##### New features

- New argument `apply_labels(fn)` to modify existing labels
  ([\#80](https://github.com/DanChaltiel/crosstable/issues/80)).

##### Bug fixes and improvements

- fixed
  [`crosstable_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  sometimes not being taken into account
  ([\#90](https://github.com/DanChaltiel/crosstable/issues/90)).
- fixed error when using a named list of functions in `crosstable(funs)`
  ([\#89](https://github.com/DanChaltiel/crosstable/issues/89)).
- fixed rare infinite loop with
  [`get_label()`](https://danchaltiel.github.io/crosstable/reference/get_label.md)
  on very specific objects like `POSIXlt`
  ([\#86](https://github.com/DanChaltiel/crosstable/issues/86)).

## crosstable 0.8.1

CRAN release: 2024-11-01

##### Bug fixes and improvements

- fixed `as_flextable(compact=TRUE)` with duplicated labels
  ([\#87](https://github.com/DanChaltiel/crosstable/issues/87)).

## crosstable 0.8.0

CRAN release: 2024-09-30

##### New features

- Remove percents when N=0 with `crosstable(remove_zero_percent=TRUE)`
  ([\#78](https://github.com/DanChaltiel/crosstable/issues/78)).
- Better header for multi-by flextables
  `as_flextable(ct, remove_header_keys=TRUE)`
  ([\#74](https://github.com/DanChaltiel/crosstable/issues/74)).
- Control font size with `body_add_normal(font_size)` and
  `crosstable_options(normal_font_size)`
  ([\#65](https://github.com/DanChaltiel/crosstable/issues/65)).
- Option control on height & width in
  [`body_add_gg2()`](https://danchaltiel.github.io/crosstable/reference/body_add_gg2.md)
  ([\#68](https://github.com/DanChaltiel/crosstable/issues/68)).

##### Bug fixes and improvements

- Add envir argument to functions that call `glue()`
  ([\#83](https://github.com/DanChaltiel/crosstable/issues/83)).
- Better error when `x` is `NULL` in
  [`body_add_table_section()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_section.md)
  ([\#82](https://github.com/DanChaltiel/crosstable/issues/82)).
- Warn if `by` is “”
  ([\#77](https://github.com/DanChaltiel/crosstable/issues/77)).
- Default to quiet warnings if columns are missing
  ([\#76](https://github.com/DanChaltiel/crosstable/issues/76)).
- Moved the legend above the table in
  [`body_add_table_section()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_section.md)
  ([\#73](https://github.com/DanChaltiel/crosstable/issues/73)).
- Fixed
  [`get_label()`](https://danchaltiel.github.io/crosstable/reference/get_label.md)
  on named vector columns
  ([\#72](https://github.com/DanChaltiel/crosstable/issues/72)).
- Fixed `crosstable(drop_levels)`
  ([\#69](https://github.com/DanChaltiel/crosstable/issues/69),
  [\#70](https://github.com/DanChaltiel/crosstable/issues/70),
  [\#71](https://github.com/DanChaltiel/crosstable/issues/71)).
- Flextable autofit now uses the recommended method
  ([\#62](https://github.com/DanChaltiel/crosstable/issues/62)).
- `transpose()` will work on multi-by crosstables
  ([\#60](https://github.com/DanChaltiel/crosstable/issues/60)).
- Fixed a bug happening when NA is already a level
  ([\#59](https://github.com/DanChaltiel/crosstable/issues/59)).
- Fixed error where `by` contains “label”
  ([\#56](https://github.com/DanChaltiel/crosstable/issues/56)).
- Fixed `as_flextable(header_show_n)` not working in single `by`
  ([\#54](https://github.com/DanChaltiel/crosstable/issues/54)).
- Fixed some missing options in
  [`crosstable_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  and added tests
  ([\#53](https://github.com/DanChaltiel/crosstable/issues/53)).

## crosstable 0.7.0 _(²⁰²³⁻¹¹⁻¹²)

CRAN release: 2023-11-12

##### New features

- New officer helper
  [`body_add_table_section()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_section.md)
  that adds a table (crosstable or flextable) which can be surrounded by
  a title (level 3 by default), a legend, and/or a short sentence.
- [`set_label()`](https://danchaltiel.github.io/crosstable/reference/set_label.md)
  now accepts a function as a value
  (e.g. `set_label(mtcars2, toupper)`).
- Argument `format_fixed(scientific)` can now be `FALSE` to force
  standard format. Can be set through
  `options(crosstable_scientific_log=FALSE)`
  ([\#49](https://github.com/DanChaltiel/crosstable/issues/49)).
- New argument `format_fixed(epsilon)` to format values as `"<epsilon"`.
  Can be set through `options(crosstable_format_epsilon=0.001)`.
- Markdown implementation has improved, with support of crossing
  patterns
  ([\#26](https://github.com/DanChaltiel/crosstable/issues/26)), newline
  tokens ([\#30](https://github.com/DanChaltiel/crosstable/issues/30)),
  and many new tags
  ([\#51](https://github.com/DanChaltiel/crosstable/issues/51)).  
  You can now use
  `` body_add_normal("Here is **`some code` in bold & *italic* ** <br> And here is <color:red>red text</color>.") ``  
  See
  [`?body_add_normal`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
  for more insight.

##### Bug fixes and improvements

- Reimplement description of columns containing both `NA` (missing
  values) and “NA” (characters)
  ([\#42](https://github.com/DanChaltiel/crosstable/issues/42)).
- Argument `as_flextable(by_header)` can now be set through
  `options(crosstable_by_header=FALSE)` to remove all headers.
- Argument `body_add_crosstable(header_fontsize)` now defaults to
  `1.2*body_fontsize`.
- Adds an error message in
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  when the crosstable is empty
  ([\#41](https://github.com/DanChaltiel/crosstable/issues/41)).
- Fixes a regression where references were not added anymore.

## crosstable 0.6.2 _(²⁰²³⁻⁰⁵⁻²⁶)

CRAN release: 2023-05-28

##### Bug fixes and improvements

- Fixes for CRAN checks.
- Fixes
  [`write_and_open()`](https://danchaltiel.github.io/crosstable/reference/write_and_open.md)
  not opening sometimes.
- Trailing commas will not make
  [`crosstable_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  fail anymore.

## crosstable 0.6.1 _(²⁰²³⁻⁰⁴⁻⁰³)

CRAN release: 2023-04-04

Fixes for CRAN checks.

## crosstable 0.6.0 _(²⁰²³⁻⁰³⁻²⁶)

CRAN release: 2023-03-26

##### New features

- New function
  [`transpose_crosstable()`](https://danchaltiel.github.io/crosstable/reference/transpose_crosstable.md)
  (or simply [`t()`](https://rdrr.io/r/base/t.html)), which transposes a
  crosstable so that `by` is in rows instead of columns.  
  `r ct = crosstable(mtcars2, c(mpg, drat), by=am) t_ct = t(ct) as_flextable(t_ct, by_header="Variables")`

- New function
  [`pivot_crosstable()`](https://danchaltiel.github.io/crosstable/reference/pivot_crosstable.md),
  which pivots a crosstable so that `variable` is spread as several
  columns.  
  `r ct = crosstable(mtcars2, c(mpg, drat)) p_ct = pivot_crosstable(ct) as_flextable(p_ct)`

- [`body_add_table_list()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_list.md)
  now replaces
  [`body_add_crosstable_list()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_list.md)
  and
  [`body_add_flextable_list()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_list.md).
  It allows both crosstables and flextable (and even dataframes, which
  are turned to flextables first) and has a new argument `fun_after` to
  control what comes after the table. See examples in
  [`?body_add_table_list`](https://danchaltiel.github.io/crosstable/reference/body_add_table_list.md).

- New function
  [`get_percent_pattern()`](https://danchaltiel.github.io/crosstable/reference/get_percent_pattern.md).
  See also the new vignette
  [`vignette("percent_pattern")`](https://danchaltiel.github.io/crosstable/articles/percent_pattern.md).

- New argument `crosstable(drop_levels=TRUE)` to drop unused levels in
  factors.

- [`copy_label_from()`](https://danchaltiel.github.io/crosstable/reference/set_label.md)
  now works on dataframes as well.

- `as_flextable(header_show_n_pattern)` can now be a list of names
  `cell` and `total`, so that the “Total” column can be labelled too.

##### Breaking changes (minor)

- `showNA="no"` is now consistent with
  [`stats::addmargins()`](https://rdrr.io/r/stats/addmargins.html),
  `gtsummary::tbl_cross(missing="no")`, and
  `janitor::tabyl(show_na=FALSE)`. It now actually removes all `NA` from
  the equation, instead of not doing much
  ([\#24](https://github.com/DanChaltiel/crosstable/issues/24)).

- In `percent_patern`, the proportion relative to the total sample
  `p_cell` has been renamed to `p_tot` for clarity.

##### Bug fixes and improvements

- Removed unexpected warning “NaNs produced” thrown when calculating
  percentages in totals while the number of NA is higher than the other
  classes ([\#20](https://github.com/DanChaltiel/crosstable/issues/20)).
- The output is now correct when `header_show_n=TRUE` and
  `remove_header_keys=TRUE` while using multiple by variables
  ([\#21](https://github.com/DanChaltiel/crosstable/issues/21)).
- Flextables will not have missing columns when using multiple by
  variables that has same key levels.
- String interpolation works as intended in `body_add_xxx_legend()`.
- [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  will not fail if
  [`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) fails
  \[#28\]
- [`forcats::fct_explicit_na()`](https://forcats.tidyverse.org/reference/fct_explicit_na.html)
  is not used anymore \[#29\]
- [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
  now removes \` symbols when showing code \[#31\]
- Trailing commas will not make
  [`apply_labels()`](https://danchaltiel.github.io/crosstable/reference/apply_labels.md)
  fail anymore \[#32\]
- Performance improvement (around 30% for small tables) as confidence
  intervals are not calculated anymore when not needed \[#34\]

Many thanks to Stephan Daus ([@sda030](https://github.com/sda030)) for
his bug reports and feedback on this release.

## crosstable 0.5.0 _(²⁰²²⁻⁰⁸⁻¹⁶)

CRAN release: 2022-08-16

##### New features

- New
  [`clean_names_with_labels()`](https://danchaltiel.github.io/crosstable/reference/clean_names_with_labels.md)
  which cleans the names of the dataframe but keeps the old names as
  labels. Obviously inspired by
  [janitor](https://github.com/sfirke/janitor).
- New variables `n_col`, `n_row`, and `n_tot` available for
  `percent_pattern`. Also, every variable has now its counterpart with
  the `_na` suffix which accounts for missing values.  
  For instance, one can now write:

``` r
    crosstable(mtcars2, cyl, percent_pattern="{p_col} ({n}/{n_col}) [95%CI: {p_col_inf}; {p_col_sup}]")
    crosstable(mtcars2, cyl, percent_pattern="{p_col_na} ({n}/{n_col_na}) [95%CI: {p_col_inf}; {p_col_sup}]")
```

- `percent_pattern` can now be a list of characters with names `body`,
  `total_row`, `total_col`, and `total_all` to also control the pattern
  in other parts of the crosstable than the body.

##### Improvements

- [`crosstable_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  does not need the “crosstable\_” prefix anymore, so the autocomplete
  is less clumsy. Options with the prefix will still work though.
  `crosstable_options(reset=TRUE)` is deprecated in favor of the new
  [`crosstable_reset_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_reset_options.md).
- [`crosstable_test_args()`](https://danchaltiel.github.io/crosstable/reference/crosstable_test_args.md)
  and
  [`crosstable_effect_args()`](https://danchaltiel.github.io/crosstable/reference/crosstable_effect_args.md)
  now have arguments to easily control the non-default parameters.
- Allow scientific notation for big numbers. Default to numbers for
  `which abs(log10(x))>4`. This can be controlled using options,
  e.g. `crosstable_options(scientific_log=5)`.
- In MS Word, crosstables will now break across pages by default. You
  can revert this by using `body_add_crosstable(allow_break=FALSE)` or
  using
  [`crosstable_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md).
  This is the pendant of `keepnext` in officer/flextable.
- New argument `body_add_crosstable(max_cols=25)`, which limits the size
  of crosstables in Word documents. This prevents very large tables to
  be wrongly included.
- [`peek()`](https://danchaltiel.github.io/crosstable/reference/peek.md)
  is now usable on non-crosstable objects as well.
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  method will be applied on the object if available, otherwise
  [`flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
  will be applied.
- Better error messages in
  [`import_labels()`](https://danchaltiel.github.io/crosstable/reference/import_labels.md)
  when `data_label` doesn’t have the right columns.

##### Deprecations

- [`rename_dataframe_with_labels()`](https://danchaltiel.github.io/crosstable/reference/rename_with_labels.md)
  is now deprecated in favor of the better named
  [`rename_with_labels()`](https://danchaltiel.github.io/crosstable/reference/rename_with_labels.md).
- `compact()` is now deprecated in favor of
  [`ct_compact()`](https://danchaltiel.github.io/crosstable/reference/ct_compact.md)
  to avoid the conflict with
  [`purrr::compact()`](https://purrr.tidyverse.org/reference/keep.html).
  It will be removed in the next version.

##### Bug fixes

- `body_add_crosstable(..., parse=NULL)` will now work as intended .
- `by_header` can replace the “value” column if `by` has only one
  stratum, in both
  [`as_gt()`](https://danchaltiel.github.io/crosstable/reference/as_gt.md)
  and
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  (<https://github.com/DanChaltiel/crosstable/issues/9>).
- `percent_pattern` can now contain functions that start by “p”
  (<https://github.com/DanChaltiel/crosstable/issues/10>).
- `percent_pattern` is actually taken into account when `by` has one
  single level (<https://github.com/DanChaltiel/crosstable/issues/13>).
- [`import_labels()`](https://danchaltiel.github.io/crosstable/reference/import_labels.md)
  now works even if there is a missing value in the dataframe.
- Minor other bugfixes

## crosstable 0.4.1 _(⁽²⁰²²⁻⁰²⁻¹⁹⁾)

CRAN release: 2022-02-25

- Maintenance release to fix CRAN errors.

## crosstable 0.4.0 _(⁽²⁰²²⁻⁰²⁻¹⁴⁾)

CRAN release: 2022-02-14

##### New features

- You can now use basic markdown formats in
  [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md):
  \*\***bold**\*\*, \**italic*\*, \_*underlined*\_, and \``code`\`.
- New `keep_id` argument in `compact.crosstable()`, which enables
  `as_flextable(compact=TRUE, keep_id=TRUE)`
- You can use `as_flextable(header_show_n_pattern)` to change the glue
  pattern of these group sizes. The default is `{.col} (N={.n})`; you
  can use `{.col_key}` and `{.col_val}` when `by` has multiple strata.
- Functions
  [`body_add_table_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md)
  and
  [`body_add_figure_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md)
  earn an argument `legend_prefix`. Useful when set through global
  options, for instance to make every table/figure start with the name
  of the study.
- [`crosstable_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  can handle new options for all those new features.

##### Improvements

- You can now set normal style directly in
  [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
  (it was only doable through options before).
- Removed the “variable” header in compacted crosstables.
- Dataset `mtcars2` is now a tibble, with its rownames as a column named
  “model”.
- `as_flextable(by_header=FALSE)` now removes the group header (if `by`
  has only one stratum).
- `as_flextable(header_show_n)` now also works if `by` has multiple
  strata.
- `as_flextable(header_show_n=TRUE)` adds group size for all groups.

##### Bug fixes

- Fixed a bug when numeric variables are treated as categorical (failed
  if one had a `NA` value).
- Fixed a bug that caused
  [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
  to add an extra empty paragraph if there was a reference in the text.
- Fixed a bug so that
  [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
  can be used without argument.
- Fixed a bug in `effect=TRUE` when some groups were present in
  [`table()`](https://rdrr.io/r/base/table.html) but not in
  [`glm()`](https://rdrr.io/r/stats/glm.html) due to missing values.

## crosstable 0.3.2 _(⁽²⁰²¹⁻¹¹⁻²⁷⁾)

CRAN release: 2021-11-27

##### New features

- Global options management is now easier thanks to
  [`crosstable_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  and autocompletion. Almost every argument can now be set using
  options. See
  [`?crosstable_options`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  for further details.
- You can also use
  [`crosstable_peek_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_peek_options.md)
  to see which crosstable option is currently set.
- New argument `num_digits` in
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md).
  It was about time!
- New argument `header_show_n` for
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md),
  which adds the group size (`N=xx`) to the header of the flextable.
- New arguments (and global options) `par_before` and `par_after` in
  respectively
  [`body_add_table_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md)
  and
  [`body_add_figure_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md),
  which add an empty paragraph before/after the legend (for
  readability).
- New function for {officer}:
  [`body_replace_text_at_bkms()`](https://danchaltiel.github.io/crosstable/reference/body_replace_text_at_bkms.md),
  to replace several bookmarks at once.
- New global option `crosstable_options(crosstable_zero_percent=FALSE)`,
  which removes percentages whenever `n==0` (as it would always be 0%).
  I should add it as an argument of
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  one day…

##### Improvements

- Better sorting when numeric variables are treated as categorical
  (before, 10 was sorted before 2).
- Removed the labelled class which caused too many problems for little
  to no improvement. See
  <https://github.com/larmarange/labelled/issues/111>.

##### Bug fixes

- fixed a bug in
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  occurring when one of `funs` does not have ellipsis (…) and `funs_arg`
  contains an unused argument.

## crosstable 0.3.1 _(⁽²⁰²¹⁻¹¹⁻¹⁴⁾)

CRAN release: 2021-11-14

##### New features

- New parameter `percent_pattern` in replacement of
  `crosstable(margin=x)` for better control over proportion format.
  Introduces the possibility of displaying confidence intervals (using
  Wilson score method) along with proportions.
- New function
  [`body_add_crosstable_list()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_list.md)
  to add a list of crosstables all at once, separated by some
  customizable titles. Also works with flextables and plain old
  dataframes.
- New argument `crosstable_padding_v` for
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  to manage vertical padding. Also available as the global option
  `crosstable_padding_v`.
- New global options `crosstable_{arg}` for almost all arguments. See
  [`?crosstable_options`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  for more details.

##### Bug fixes

- fixed a bug in
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  occurring when `showNA=TRUE` (header row was disappearing)
- fixed a bug occurring very randomly, when rounding caused
  [`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md)
  to return a numeric value (“Error: Can’t combine `..1$value` and
  `..2$value` .”)
- crosstable now shows all unused levels in `by` when it is a factor
- removed extra bold columns in compacted crosstables displayed as
  flextables

##### Internal

- renamed branch `master` to `main`
- use a lot more snapshots in tests

## crosstable 0.2.2 _(⁽²⁰²¹⁻¹⁰⁻¹⁸⁾)

CRAN release: 2021-10-19

##### New features

- Added support for multiple `by`! You can now write
  `crosstable(mtcars, c(mpg, gear), by=c(am, vs)) %>% as_flextable()`.
- Added a macro that can autofit every table in the document at once.
  This macro can be generated using the function
  [`generate_autofit_macro()`](https://danchaltiel.github.io/crosstable/reference/generate_autofit_macro.md)
  which creates a file that should then be imported into MS Word.
- [`body_add_crosstable()`](https://danchaltiel.github.io/crosstable/reference/body_add_crosstable.md)
  gains a `padding_v` argument to control the vertical padding of all
  rows.
- [`body_add_title()`](https://danchaltiel.github.io/crosstable/reference/body_add_title.md)
  and `body_add_xxx_legend)` gain a glue functionality. You can now
  write `body_add_title("The iris dataset (nrow={nrow(iris)})", 1)`.
- [`as_workbook()`](https://danchaltiel.github.io/crosstable/reference/as_workbook.md)
  can now take a named list of crosstables, that will be considered as
  sheets.
- New parameter `percent` in `format_fixed(percent=TRUE/FALSE)` to
  easily format percentages.

##### Minor changes

- `style` is deprecated in
  [`body_add_table_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md)
  and `body_add_image_legend()` in favor of `name_format`.
- Changed the behaviour of some `effect` calculations that were done by
  column instead of rows. That might change some outputs but not their
  meaning.
- [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
  now removes duplicated spaces (squish) in its input by default. Use
  `squish=FALSE` to override.
- [`docx_bookmarks2()`](https://danchaltiel.github.io/crosstable/reference/docx_bookmarks2.md)
  gains a `target` parameter.

##### Bug fixes

- `effect` calculation now takes into account the reference level (first
  level of a factor).
- [`body_add_crosstable()`](https://danchaltiel.github.io/crosstable/reference/body_add_crosstable.md)
  rightly takes `body_fontsize` and `header_fontsize` into account.
- Added few more warnings, so that you know what went wrong.

##### Internal

- burgled 2 functions using `burglr::burgle()` to avoid dependency:
  `nortest::ad.test()` and `DescTools::CochranArmitageTest()`.
- fixes the bug from the breaking change in `testthat`
  (<https://github.com/DanChaltiel/crosstable/pull/3>).

## crosstable 0.2.1 _(⁽²⁰²¹⁻⁰²⁻⁰⁷⁾)

CRAN release: 2021-03-08

- First version on CRAN
- Improved functions naming in `funs`, especially with multiple
  combinations of named and unnamed functions, including lambda or
  anonymous
- Use `simplify=FALSE` in
  [`get_label()`](https://danchaltiel.github.io/crosstable/reference/get_label.md)
  to get a list instead of a vector

## crosstable 0.2.0 _(⁽²⁰²¹⁻⁰²⁻⁰²⁾)

##### Misc

- added lots of global options for easier implementation. See
  [`?crosstable_options`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  for the comprehensive list.
- added label helpers:
  [`apply_labels()`](https://danchaltiel.github.io/crosstable/reference/apply_labels.md)
  (inspired by `expss`’s),
  [`copy_label_from()`](https://danchaltiel.github.io/crosstable/reference/set_label.md)
  and
  [`rename_dataframe_with_labels()`](https://danchaltiel.github.io/crosstable/reference/rename_with_labels.md)
- added
  [`as_workbook()`](https://danchaltiel.github.io/crosstable/reference/as_workbook.md)
  to export a crosstable as a formatted `openxlsx` Excel workbook, for
  copypasting purpose.
- added
  [`peek()`](https://danchaltiel.github.io/crosstable/reference/peek.md)
  to open a crosstable in a temporary Word document, as copy-pasting in
  RStudio’s viewer is very limited.
- fixed the bug when a columns contained both “NA” (string) and `<NA>`
  (missing).
- fixed the bug where function in `funs` was not found if declared in
  another environment.
- numerous other minor bugfixes and internal improvements.

##### Officer

- added **cross-reference functionality** to
  [`body_add_figure_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md)
  and
  [`body_add_table_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md).  
  Use the `bookmark` argument to set a reference, then write
  `"\\@ref(my_bkm)"` inside
  [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
  to call it.
- added docx helpers to add lists:
  [`body_add_list()`](https://danchaltiel.github.io/crosstable/reference/body_add_list.md)
  and
  [`body_add_list_item()`](https://danchaltiel.github.io/crosstable/reference/body_add_list.md).
  These will unfortunately not work with the default `officer` template.
- added some alternatives for some `officer` functions:
- [`docx_bookmarks2()`](https://danchaltiel.github.io/crosstable/reference/docx_bookmarks2.md),
  which list bookmarks found in the header and footer as well
- [`body_add_img2()`](https://danchaltiel.github.io/crosstable/reference/body_add_img2.md),
  and
  [`body_add_gg2()`](https://danchaltiel.github.io/crosstable/reference/body_add_gg2.md),
  which win a `units=c("in", "cm", "mm")` argument
- [`write_and_open()`](https://danchaltiel.github.io/crosstable/reference/write_and_open.md),
  an alternative to [`print()`](https://rdrr.io/r/base/print.html) for
  documents, which tries to open it right away.

##### Deprecations

- Ellipsis (`...`) use in
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  has been deprecated for a more “tidy” syntax. Write
  `crosstable(mtcars2, c(disp, vs))` instead of
  `crosstable(mtcars2, disp, vs)`. Ellipsis will be defunct in future
  v1.0.
- `crosstable(.vars=)` has been renamed to `crosstable(cols=)`.
- [`moystd()`](https://danchaltiel.github.io/crosstable/reference/summaryFunctions.md)
  has been renamed to
  [`meansd()`](https://danchaltiel.github.io/crosstable/reference/summaryFunctions.md).
- [`body_add_glued()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
  has been superseded by
  [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md),
  which inherits all functionalities and more.

## crosstable 0.1.5 _(⁽²⁰²⁰⁻⁰⁸⁻⁰²⁾)

- added minimal support for `gt` tables (with
  [`as_gt()`](https://danchaltiel.github.io/crosstable/reference/as_gt.md))
  for those who like them better than `flextable`s
- improved working with `officer`: added
  [`body_add_figure_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md)
  and `fontsize` options for
  [`body_add_crosstable()`](https://danchaltiel.github.io/crosstable/reference/body_add_crosstable.md)

## crosstable 0.1.4 _(⁽²⁰²⁰⁻⁰⁷⁻¹⁶⁾)

- added
  [`save_labels()`](https://danchaltiel.github.io/crosstable/reference/import_labels.md)
  to ease working with `dplyr`
- added
  [`meanCI()`](https://danchaltiel.github.io/crosstable/reference/summaryFunctions.md)
  an additional summary function to use in
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)’s
  `funs` argument
- improved support for `Date` variables
- multiple, numerous bug fixes
- renamed
  [`moystd()`](https://danchaltiel.github.io/crosstable/reference/summaryFunctions.md)
  to
  [`meansd()`](https://danchaltiel.github.io/crosstable/reference/summaryFunctions.md)

## crosstable 0.1.3 _(⁽²⁰²⁰⁻⁰⁶⁻²⁹⁾)

- Added support for description of `Date` variables. Format can be
  specified in `funs_arg` with the `date_format` key.
- Removed some dependencies to ease installation

## crosstable 0.1.2 _(⁽²⁰²⁰⁻⁰⁶⁻¹⁰⁾)

- Effect refactoring: better error/warning handling
- Name sanitation: replacing “.” by “\_” in function names
- Better error messages
- Bug fixes

## crosstable 0.1.1 _(⁽²⁰²⁰⁻⁰⁶⁻⁰⁷⁾)

##### New features and behaviors

- Added
  [`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md),
  rounding with the right number of decimals (including zeros)
- Added
  [`import_labels()`](https://danchaltiel.github.io/crosstable/reference/import_labels.md),
  which apply labels taken from a source dataframe (name, label) to
  another dataframe
- Added `margin="none"` option, to remove percentages and keep only
  counts
- Columns of unsupported class are dropped with a warning instead of
  failing with an error

##### Misc

- Method
  [`cross_to_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  ([`ctf()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md))
  was deprecated and renamed
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  ([\#207](https://github.com/davidgohel/flextable/issues/207))
- Reexporting pipes and tidyselect helpers so that user does not have to
  load these libraries
- Computing time optimization (speed x2.6!)
- Fixed bug in normality testing
- Fixed bug in `compact()`

## crosstable 0.1.0 (2020-04-09)

- First release, big changes from the
  [`biostat2`](https://github.com/eusebe/biostat2) package.
