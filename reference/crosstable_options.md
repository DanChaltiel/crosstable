# Options for the package `crosstable`

Use this function to manage your `crosstable` parameters globally while
taking advantage of autocompletion. Use
[`crosstable_peek_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_peek_options.md)
to see which option is currently set and
[`crosstable_reset_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_reset_options.md)
to set all options back to default.

## Usage

``` r
crosstable_options(
  ...,
  remove_zero_percent = FALSE,
  only_round = FALSE,
  verbosity_autotesting = "default",
  verbosity_duplicate_cols = "default",
  fishertest_B = 1e+05,
  total,
  percent_pattern,
  margin,
  percent_digits,
  num_digits,
  showNA,
  label,
  funs,
  funs_arg,
  cor_method,
  drop_levels,
  unique_numeric,
  date_format,
  times,
  followup,
  test_args,
  effect_args,
  wrap_id = 70,
  compact_padding = 25,
  header_show_n_pattern = "{.col} (N={.n})",
  keep_id,
  by_header,
  autofit,
  compact,
  remove_header_keys,
  show_test_name,
  padding_v,
  header_show_n,
  fontsize_body,
  fontsize_subheaders,
  fontsize_header,
  generic_labels,
  units = "in",
  peek_docx = TRUE,
  font_code = "Consolas",
  add_max_cols = 25,
  gg_width,
  gg_height,
  format_legend_name,
  table_legend_par_before,
  table_legend_prefix,
  figure_legend_par_after,
  figure_legend_prefix,
  normal_squish,
  normal_font_size,
  title_squish,
  allow_break,
  section_title,
  section_title_level,
  section_sentence,
  style_normal,
  style_image,
  style_legend,
  style_heading,
  style_list_ordered,
  style_list_unordered,
  scientific_log,
  clean_names_fun,
  verbosity_na_cols,
  format_epsilon,
  .local = FALSE,
  reset = deprecated()
)
```

## Arguments

- ...:

  unused

- remove_zero_percent:

  set to TRUE so that proportions are not displayed if `n==0`

- only_round:

  default argument for
  [`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md)

- verbosity_autotesting:

  one of `default`, `quiet`, or `verbose`

- verbosity_duplicate_cols:

  one of `default`, `quiet`, or `verbose`.

- fishertest_B:

  number of simulations to perform when
  [`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) is failing
  (FEXACT error 7).

- total:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- percent_pattern:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- margin:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- percent_digits:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- num_digits:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- showNA:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- label:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- funs:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- funs_arg:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- cor_method:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- drop_levels:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- unique_numeric:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- date_format:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- times:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- followup:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- test_args:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- effect_args:

  For setting
  [`crosstable()`](https://danchaltiel.github.io/crosstable/reference/crosstable.md)
  arguments globally.

- wrap_id:

  if `id` contains no spaces, wrap it with this maximum number of
  characters.

- compact_padding:

  in flextables, left-padding for non-headers rows when `compact=TRUE`.

- header_show_n_pattern:

  glue pattern used when showing N in the header of flextables. `.col`
  is the name of the column and `.n` the size of the group. Default to
  `{.col} (N={.n})`.

- keep_id:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- by_header:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- autofit:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- compact:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- remove_header_keys:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- show_test_name:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- padding_v:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- header_show_n:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- fontsize_body:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- fontsize_subheaders:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally. Subheaders are only considered when
  `compact=TRUE`.

- fontsize_header:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- generic_labels:

  For setting
  [`as_flextable()`](https://danchaltiel.github.io/crosstable/reference/as_flextable.md)
  arguments globally.

- units:

  default units in
  [`body_add_gg2()`](https://danchaltiel.github.io/crosstable/reference/body_add_gg2.md)
  and
  [`body_add_img2()`](https://danchaltiel.github.io/crosstable/reference/body_add_img2.md)

- peek_docx:

  behavior of
  [`peek()`](https://danchaltiel.github.io/crosstable/reference/peek.md),
  which will open a `docx` if `TRUE` (default) and an `xlsx` if `FALSE`

- font_code:

  font family used to show code, most likely a monospaced typeface such
  as Consolas (default)

- add_max_cols:

  max number of columns a crosstable can have to be added to a Word
  document

- gg_width, gg_height:

  cf.
  [`body_add_gg2()`](https://danchaltiel.github.io/crosstable/reference/body_add_gg2.md)

- format_legend_name:

  how the legend name ("Table", "Figure") is formatted. Default to
  `officer::fp_text_lite(bold=TRUE)`

- table_legend_par_before:

  whether to add an empty paragraph before all table legends

- table_legend_prefix, figure_legend_prefix:

  a prefix before each legend, after the numbering

- figure_legend_par_after:

  whether to add an empty paragraph after all figure legends

- normal_squish:

  Should you squish text in normal paragraphs?

- normal_font_size:

  Font size in normal paragraph, cf.
  [`body_add_normal()`](https://danchaltiel.github.io/crosstable/reference/body_add_normal.md)

- title_squish:

  Should you squish text in headers paragraphs?

- allow_break:

  allow crosstable rows to break across pages

- section_title, section_title_level, section_sentence:

  cf.
  [`body_add_table_section()`](https://danchaltiel.github.io/crosstable/reference/body_add_table_section.md)

- style_normal:

  For specifying styles used in your `{officer}` template.

- style_image:

  For specifying styles used in your `{officer}` template.

- style_legend:

  For specifying styles used in your `{officer}` template.

- style_heading:

  For specifying styles used by headings on different levels. Levels
  will be pasted in the end (e.g. use `"title"` if your level 2 heading
  style is `"title2"`).

- style_list_ordered, style_list_unordered:

  For specifying styles used by lists in the `rdocx` template. Needed
  for
  [`body_add_list()`](https://danchaltiel.github.io/crosstable/reference/body_add_list.md)
  to work.

- scientific_log:

  the maximum power a number can have before being formatted as
  scientific. Default to 4 so applies on numbers \<1e-4 or \>1e4.

- clean_names_fun:

  cf.
  [`clean_names_with_labels()`](https://danchaltiel.github.io/crosstable/reference/clean_names_with_labels.md)

- verbosity_na_cols:

  verbosity of a warning

- format_epsilon:

  cf.
  [`format_fixed()`](https://danchaltiel.github.io/crosstable/reference/format_fixed.md)

- .local:

  if TRUE, the effect will only apply to the local frame (thanks to
  [`rlang::local_options()`](https://rlang.r-lib.org/reference/local_options.html))

- reset:

  if `TRUE`, set all these options back to default

## Value

Nothing, called for its side effects

## See also

[`crosstable_peek_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_peek_options.md)
and
[`crosstable_reset_options()`](https://danchaltiel.github.io/crosstable/reference/crosstable_reset_options.md)
