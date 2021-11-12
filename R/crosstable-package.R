#' @details
#' The `crosstable`package is centered on the [crosstable()]` function.
#' See vignettes for more information.
#' @keywords internal
#' @name crosstable-package
#' @aliases Test-package
"_PACKAGE"





#' Options for the package `crosstable` 
#' @name crosstable_options
#'
#' @description 
#' Here is a comprehensive list of all options that you can set globally.
#' 
#' For setting `crosstable()` arguments globally:
#'  + `crosstable_total`, `crosstable_percent_pattern`, `crosstable_percent_digits`, `crosstable_showNA`, `crosstable_label`, `crosstable_funs`, `crosstable_funs_arg`, `crosstable_cor_method`, `crosstable_unique_numeric`, `crosstable_date_format`, `crosstable_times`, `crosstable_followup`, `crosstable_test_arg`, and `crosstable_effect_args`
#' 
#' For flextables:
#'  + `crosstable_autofit`, `crosstable_compact`, and `crosstable_show_test_name`: default arguments for [as_flextable()].
#'  + `crosstable_compact_padding`: left-padding for non-headers rows when `compact=TRUE`.
#'  + `crosstable_padding_v`: vertical padding (body).
#'  + `crosstable_fontsize_body`, `crosstable_fontsize_header`, and `crosstable_fontsize_subheaders`: font sizes for, respectively for normal, header and subheader rows. Subheaders are only considered when `compact=TRUE`.
#'  + `crosstable_wrap_id`: if `id` contains no spaces, wrap it with this maximum number of characters.
#'  
#' For specifying styles in officer docx output: 
#'  + `crosstable_style_normal`
#'  + `crosstable_style_character`: used in cross-references
#'  + `crosstable_style_heading`
#'  + `crosstable_style_strong`
#'  + `crosstable_style_image`
#'  + `crosstable_style_legend`
#'  + `crosstable_style_list_ordered` and `crosstable_style_list_unordered`, mandatory for [body_add_list()] to work.
#'  
#' Verbosity: 
#'  + `crosstable_verbosity_autotesting`: one of `default`, `quiet`, or `verbose`
#'  
#' Misc: 
#'  + `crosstable_only_round`: default argument for [format_fixed()]
#'  + `crosstable_units`: default units in [body_add_gg2()] and [body_add_img2()]
#'  + `crosstable_peek_docx`: behavior of [peek()], which will open a `docx` if `TRUE` and an `xlsx` if `FALSE`
#' 
NULL
