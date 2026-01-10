# Package index

## Main function

- [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  : Easily describe datasets

- [`crosstable_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_options.md)
  :

  Options for the package `crosstable`

- [`crosstable_peek_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_peek_options.md)
  :

  See which `crosstable` option is currently set.

- [`crosstable_reset_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_reset_options.md)
  :

  Reset all `crosstable` options.

## Datasets

Standard datasets, modified to have labels, characters, and factors

- [`iris2`](https://danchaltiel.github.io/crosstable/reference/iris2.md)
  :

  Modified `iris` dataset

- [`mtcars2`](https://danchaltiel.github.io/crosstable/reference/mtcars2.md)
  :

  Modified `mtcars` dataset

## Labelling

Handle label attributes (compatible with Hmisc, expss, haven…)

- [`import_labels()`](https://danchaltiel.github.io/crosstable/reference/import_labels.md)
  [`save_labels()`](https://danchaltiel.github.io/crosstable/reference/import_labels.md)
  : Import labels
- [`set_label()`](https://danchaltiel.github.io/crosstable/reference/set_label.md)
  [`copy_label_from()`](https://danchaltiel.github.io/crosstable/reference/set_label.md)
  : Set the "label" attribute of an object
- [`get_label()`](https://danchaltiel.github.io/crosstable/reference/get_label.md)
  : Get label if wanted and available, or default (name) otherwise
- [`remove_labels()`](https://danchaltiel.github.io/crosstable/reference/remove_labels.md)
  : Remove all label attributes.
- [`apply_labels()`](https://danchaltiel.github.io/crosstable/reference/apply_labels.md)
  : Batch set variable labels
- [`rename_with_labels()`](https://danchaltiel.github.io/crosstable/reference/rename_with_labels.md)
  : Rename every column of a dataframe with its label
- [`clean_names_with_labels()`](https://danchaltiel.github.io/crosstable/reference/clean_names_with_labels.md)
  : Cleans names of a dataframe while retaining old names as labels

## Post-processing

Modify an already built crosstable

- [`transpose_crosstable()`](https://danchaltiel.github.io/crosstable/reference/transpose_crosstable.md)
  [`t(`*`<crosstable>`*`)`](https://danchaltiel.github.io/crosstable/reference/transpose_crosstable.md)
  : Transpose a crosstable
- [`pivot_crosstable()`](https://danchaltiel.github.io/crosstable/reference/pivot_crosstable.md)
  : Pivot a crosstable

## Summary functions

Summarise numeric variables

- [`meansd()`](https://danchaltiel.github.io/crosstable/reference/summaryFunctions.md)
  [`meanCI()`](https://danchaltiel.github.io/crosstable/reference/summaryFunctions.md)
  [`mediqr()`](https://danchaltiel.github.io/crosstable/reference/summaryFunctions.md)
  [`minmax()`](https://danchaltiel.github.io/crosstable/reference/summaryFunctions.md)
  [`nna()`](https://danchaltiel.github.io/crosstable/reference/summaryFunctions.md)
  : Summary functions
- [`cross_summary()`](https://danchaltiel.github.io/crosstable/reference/cross_summary.md)
  : Summarize a numeric vector
- [`na()`](https://danchaltiel.github.io/crosstable/reference/na.md) :
  Return the number of NA observations
- [`N()`](https://danchaltiel.github.io/crosstable/reference/n.md) :
  Return the number of non NA observations

## Visualization

Visualize a crosstable in various formats

- [`as_gt()`](https://danchaltiel.github.io/crosstable/reference/as_gt.md)
  :

  Converts a `crosstable` object into a formatted `gt` table.

- [`as_workbook()`](https://danchaltiel.github.io/crosstable/reference/as_workbook.md)
  :

  Converts a `crosstable` object into a formatted, savable `openxlsx`
  workbook.

- [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  :

  Turns a `crosstable` object into a formatted `flextable`

## Officer helpers

Used in cooperation with the `officer` package

- [`body_add_crosstable()`](https://danchaltiel.github.io/crosstable/reference/body_add_crosstable.md)
  :

  Add a crosstable to an `officer` document

- [`body_add_crosstable_footnote()`](https://danchaltiel.github.io/crosstable/reference/body_add_crosstable_footnote.md)
  : Adds a standard footnote explaining the abbreviations used in a
  crosstable

- [`body_add_flextable2()`](https://danchaltiel.github.io/crosstable/reference/body_add_flextable2.md)
  :

  Alternative to
  [`flextable::body_add_flextable()`](https://davidgohel.github.io/flextable/reference/body_add_flextable.html)

- [`body_add_gg2()`](https://danchaltiel.github.io/crosstable/reference/body_add_gg2.md)
  :

  Alternative to
  [`officer::body_add_gg()`](https://davidgohel.github.io/officer/reference/body_add_gg.html)
  which uses `ggplot` syntax

- [`body_add_img2()`](https://danchaltiel.github.io/crosstable/reference/body_add_img2.md)
  :

  Alternative to
  [`officer::body_add_img()`](https://davidgohel.github.io/officer/reference/body_add_img.html)
  which adds a `units` choice

- [`body_add_table_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md)
  [`body_add_figure_legend()`](https://danchaltiel.github.io/crosstable/reference/body_add_legend.md)
  : Add a legend to a table or a figure

- [`body_add_list()`](https://danchaltiel.github.io/crosstable/reference/body_add_list.md)
  [`body_add_list_item()`](https://danchaltiel.github.io/crosstable/reference/body_add_list.md)
  :

  Add a list to an `officer` document

- [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)
  : Add a new paragraph with default style

- [`body_add_table_list()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_list.md)
  [`body_add_flextable_list()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_list.md)
  [`body_add_crosstable_list()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_list.md)
  : Add a list of tables

- [`body_add_table_section()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_section.md)
  : Add a section with a table and its legend

- [`body_add_title()`](https://danchaltiel.github.io/crosstable/reference/body_add_title.md)
  :

  Add a title to an `officer` document

- [`body_replace_text_at_bkms()`](https://danchaltiel.github.io/crosstable/reference/body_replace_text_at_bkms.md)
  : Replace text on several bookmarks at once

- [`docx_bookmarks2()`](https://danchaltiel.github.io/crosstable/reference/docx_bookmarks2.md)
  : List Word bookmarks, including the ones in header and footer

- [`write_and_open()`](https://danchaltiel.github.io/crosstable/reference/write_and_open.md)
  :

  Alternative to default `officer` print() function. Write the file and
  try to open it right away.

- [`generate_autofit_macro()`](https://danchaltiel.github.io/crosstable/reference/generate_autofit_macro.md)
  : Generate a macro file for autofitting

## Utils

Practical functions for various purposes

- [`get_percent_pattern()`](https://danchaltiel.github.io/crosstable/reference/get_percent_pattern.md)
  : Percent pattern helper

- [`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md)
  : Format numbers with the exact same number of decimals, including
  trailing zeros

- [`ct_compact(`*`<data.frame>`*`)`](https://danchaltiel.github.io/crosstable/reference/ct_compact.md)
  [`ct_compact(`*`<crosstable>`*`)`](https://danchaltiel.github.io/crosstable/reference/ct_compact.md)
  : Generic function to compact a table (publication formatting)

- [`ct_bind_cols()`](https://danchaltiel.github.io/crosstable/reference/ct_bind_cols.md)
  **\[experimental\]** : Combine crosstables

- [`plim()`](https://danchaltiel.github.io/crosstable/reference/plim.md)
  :

  Format p values (alternative to
  [`format.pval()`](https://rdrr.io/r/base/format.pval.html))

- [`confint_numeric()`](https://danchaltiel.github.io/crosstable/reference/confint_numeric.md)
  : Confidence interval of a numeric vector

- [`narm()`](https://danchaltiel.github.io/crosstable/reference/narm.md)
  : Remove missing values

- [`is.crosstable()`](https://danchaltiel.github.io/crosstable/reference/is.crosstable.md)
  [`is.transposed_crosstable()`](https://danchaltiel.github.io/crosstable/reference/is.crosstable.md)
  [`is.compacted_crosstable()`](https://danchaltiel.github.io/crosstable/reference/is.crosstable.md)
  [`is.multiby_crosstable()`](https://danchaltiel.github.io/crosstable/reference/is.crosstable.md)
  : Test if an object is a crosstable

- [`peek()`](https://danchaltiel.github.io/crosstable/reference/peek.md)
  :

  Open a `crosstable` in a temporary document

## Effects and tests

### Default parameters

- [`crosstable_effect_args()`](https://danchaltiel.github.io/crosstable/reference/crosstable_effect_args.md)
  :

  Default arguments for calculating and displaying effects in
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)

- [`crosstable_test_args()`](https://danchaltiel.github.io/crosstable/reference/crosstable_test_args.md)
  :

  Default arguments for calculating and displaying tests in
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)

### Effects

- [`display_effect()`](https://danchaltiel.github.io/crosstable/reference/display_effect.md)
  : Default function to display the effect
- [`diff_mean_auto()`](https://danchaltiel.github.io/crosstable/reference/effect_summary.md)
  [`diff_mean_boot()`](https://danchaltiel.github.io/crosstable/reference/effect_summary.md)
  [`diff_median_boot()`](https://danchaltiel.github.io/crosstable/reference/effect_summary.md)
  [`diff_mean_student()`](https://danchaltiel.github.io/crosstable/reference/effect_summary.md)
  : Effect measure for association between one continuous and one
  categorical variable
- [`effect_survival_coxph()`](https://danchaltiel.github.io/crosstable/reference/effect_survival.md)
  : Effect measure for association between one censored variable and one
  categorical variable
- [`effect_odds_ratio()`](https://danchaltiel.github.io/crosstable/reference/effect_tabular.md)
  [`effect_relative_risk()`](https://danchaltiel.github.io/crosstable/reference/effect_tabular.md)
  [`effect_risk_difference()`](https://danchaltiel.github.io/crosstable/reference/effect_tabular.md)
  : Effect measure for association between two categorical variables

### Tests

- [`display_test()`](https://danchaltiel.github.io/crosstable/reference/display_test.md)
  : Default function to display a test result

- [`crosstable_test_args()`](https://danchaltiel.github.io/crosstable/reference/crosstable_test_args.md)
  :

  Default arguments for calculating and displaying tests in
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)

- [`test_correlation_auto()`](https://danchaltiel.github.io/crosstable/reference/test_correlation_auto.md)
  : test for correlation coefficients

- [`test_summarize_auto()`](https://danchaltiel.github.io/crosstable/reference/test_summarize_auto.md)
  : test for mean comparison

- [`test_summarize_linear_contrasts()`](https://danchaltiel.github.io/crosstable/reference/test_summarize_linear_contrasts.md)
  : Test for linear trend across ordered factor with contrasts

- [`test_survival_logrank()`](https://danchaltiel.github.io/crosstable/reference/test_survival_logrank.md)
  : test for survival comparison

- [`test_tabular_auto()`](https://danchaltiel.github.io/crosstable/reference/test_tabular_auto.md)
  : test for contingency table
